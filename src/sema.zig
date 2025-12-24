pub const SemaAnalyzer = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    scope: Scope,

    pub fn init(allocator: std.mem.Allocator) !Self {
        return Self{ .allocator = allocator, .scope = try Scope.init(allocator, .void) };
    }

    pub fn deinit(self: *Self) void {
        self.scope.deinit();
    }

    pub fn analyze(self: *Self, root: *const ast.Root) !void {
        try self.visitBlock(root);
    }

    pub fn visitBlock(self: *Self, block: *const ast.Block) !void {
        try self.scope.enter(self.scope.return_type);
        defer self.scope.exit(self.scope.return_type);

        for (block.statements.items) |stmt| {
            switch (stmt) {
                .let_stmt => try self.visitLetStmt(&stmt.let_stmt),
                .if_stmt => try self.visitIfStmt(&stmt.if_stmt),
                .for_stmt => try self.visitForStmt(&stmt.for_stmt),
                .assign_stmt => try self.visitAssignStmt(&stmt.assign_stmt),
                .fn_call_stmt => _ = try self.analyzeFnCall(&stmt.fn_call_stmt),
                .return_stmt => try self.visitReturnStmt(&stmt.return_stmt),
            }
        }
    }

    fn analyzeExpression(self: *Self, exp: *const ast.Exp) SemaError!Type {
        switch (exp.*) {
            .number_literal => {
                return .number;
            },
            .string_literal => {
                return .string;
            },
            .bool_literal => {
                return .boolean;
            },
            .identifier => {
                const id_symbol = try self.visitIdentifier(exp.identifier);
                return id_symbol.type;
            },
            .fn_call => {
                return self.analyzeFnCall(&exp.fn_call);
            },
            .fn_def => {
                return .function;
            },
            .unary_exp => {
                return self.analyzeUnaryExp(&exp.unary_exp);
            },
            .binary_exp => {
                return self.analyzeBinaryExp(&exp.binary_exp);
            },
        }
    }

    fn visitLetStmt(self: *Self, letStmt: *const ast.LetStmt) !void {
        const expression_type = try self.analyzeExpression(letStmt.value);
        const var_type = try Type.fromString(letStmt.type_annotation.name);

        if (expression_type != var_type) {
            return SemaError.InvalidExpressionType;
        }

        try self.scope.symbol_table.put(.{ .id = letStmt.identifier, .is_mut = letStmt.is_mut, .type = var_type, .metadata = null });

        if (expression_type == .function) {
            try self.visitFnDef(&letStmt.value.fn_def, letStmt.identifier);
        }
    }

    fn visitIfStmt(self: *Self, ifStmt: *const ast.IfStmt) SemaError!void {
        const exp_type = try self.analyzeExpression(ifStmt.condition);
        if (exp_type != .boolean) {
            return SemaError.InvalidExpressionType;
        }

        try self.visitBlock(&ifStmt.then_block);

        if (ifStmt.else_block) |else_blc| {
            try self.visitBlock(&else_blc);
        }
    }

    fn visitAssignStmt(self: *Self, assignStmt: *const ast.AssignStmt) !void {
        const identifier_symbol = try self.visitIdentifier(assignStmt.identifier);
        const exp_type = try self.analyzeExpression(assignStmt.exp);

        if (!identifier_symbol.is_mut) {
            return SemaError.InvalidAssignment;
        }

        if (identifier_symbol.type != exp_type) {
            return SemaError.InvalidAssignment;
        }
    }

    fn analyzeFnCall(self: *Self, fnCall: *const ast.FnCall) !Type {
        const func_symbol = try self.visitIdentifier(fnCall.identifier);
        if (func_symbol.type != .function) {
            return SemaError.InvalidFunction;
        }

        if (func_symbol.metadata) |fn_data| {
            if (fn_data.params_types.items.len != fnCall.arguments.items.len) {
                return SemaError.InvalidFunctionParameter;
            }

            for (fn_data.params_types.items, 0..) |param_type, i| {
                const fn_call_param_type = try self.analyzeExpression(fnCall.arguments.items[i]);
                if (fn_call_param_type != param_type) {
                    return SemaError.InvalidFunctionParameter;
                }
            }

            return fn_data.return_type;
        }

        unreachable;
    }

    fn visitForStmt(self: *Self, forStmt: *const ast.ForStmt) SemaError!void {
        if (forStmt.condition) |condition| {
            const exp_type = try self.analyzeExpression(condition);
            if (exp_type != .boolean) {
                return SemaError.InvalidExpressionType;
            }
        }

        try self.visitBlock(&forStmt.do_block);
    }

    fn visitFnDef(self: *Self, fnDef: *const ast.FnDef, identifier: []const u8) SemaError!void {
        const old_return_type = self.scope.return_type;
        var argument_types: std.ArrayList(Type) = .empty;
        const return_type = try Type.fromString(fnDef.return_type.name);

        for (fnDef.arguments.items) |arg| {
            const arg_type = try Type.fromString(arg.type_annotation.name);
            try argument_types.append(self.allocator, arg_type);
        }

        try self.scope.symbol_table.replace(.{ .id = identifier, .is_mut = false, .type = .function, .metadata = FnMetadata.init(self.allocator, argument_types, return_type) });

        try self.scope.enter(return_type);
        defer self.scope.exit(old_return_type);

        for (fnDef.arguments.items) |arg| {
            const arg_type = try Type.fromString(arg.type_annotation.name);
            try self.scope.symbol_table.put(.{
                .id = arg.identifier,
                //todo: later this can also be passed as argument
                .is_mut = false,
                .type = arg_type,
                .metadata = null,
            });
        }

        try self.visitBlock(&fnDef.body_block);
    }

    fn visitReturnStmt(self: *Self, returnStmt: *const ast.ReturnStmt) !void {
        if (returnStmt.exp) |exp| {
            const exp_type = try self.analyzeExpression(exp);
            if (exp_type != self.scope.return_type) {
                return SemaError.InvalidReturnType;
            }
            return;
        }

        if (self.scope.return_type != .void) {
            return SemaError.InvalidReturnType;
        }
    }

    fn visitIdentifier(self: *Self, id: []const u8) !Symbol {
        return self.scope.symbol_table.getOrThrow(id);
    }

    fn analyzeUnaryExp(self: *Self, unary_exp: *const ast.UnaryExp) !Type {
        const exp_type = try self.analyzeExpression(unary_exp.right);

        switch (unary_exp.op) {
            .neg => {
                if (exp_type != .number) {
                    return SemaError.InvalidExpressionType;
                }
            },
            .not => {
                if (exp_type != .boolean) {
                    return SemaError.InvalidExpressionType;
                }
            },
        }

        return exp_type;
    }

    fn analyzeBinaryExp(self: *Self, bin_exp: *const ast.BinaryExp) !Type {
        const left_type = try self.analyzeExpression(bin_exp.left);
        const right_type = try self.analyzeExpression(bin_exp.right);

        if (left_type != right_type) {
            return SemaError.InvalidExpressionType;
        }

        switch (bin_exp.op) {
            .add, .sub, .mult, .div, .mod => {
                if (left_type != .number) {
                    return SemaError.InvalidOperation;
                }
                return .number;
            },

            .eq, .not_eq, .lt, .lt_or_eq, .gt, .gt_or_eq => {
                if (left_type == .boolean) {
                    return SemaError.InvalidOperation;
                }
                return .boolean;
            },

            .bool_or, .bool_and => {
                if (left_type != .boolean) {
                    return SemaError.InvalidOperation;
                }
                return .boolean;
            },
        }
    }
};

const Scope = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    symbol_table: *SymbolTable,
    return_type: Type,

    pub fn init(allocator: std.mem.Allocator, return_type: Type) !Self {
        return Self{ .allocator = allocator, .symbol_table = try SymbolTable.init(allocator, null), .return_type = return_type };
    }

    pub fn deinit(self: *Self) void {
        var current_ptr: ?*SymbolTable = self.symbol_table;
        while (current_ptr) |table| {
            const parent = table.parent;
            table.deinit();
            current_ptr = parent;
        }
    }

    pub fn enter(self: *Self, return_type: Type) !void {
        self.return_type = return_type;
        self.symbol_table = try SymbolTable.init(self.allocator, self.symbol_table);
    }

    pub fn exit(self: *Self, return_type: Type) void {
        const scope_to_destroy = self.symbol_table;
        defer scope_to_destroy.deinit();

        self.return_type = return_type;
        if (self.symbol_table.parent) |parent_scope| self.symbol_table = parent_scope;
    }
};

const SymbolTable = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    parent: ?*SymbolTable,
    symbols: std.StringHashMap(Symbol),

    pub fn init(allocator: std.mem.Allocator, parent: ?*SymbolTable) !*Self {
        const self = try allocator.create(Self);
        self.* = Self{ .allocator = allocator, .parent = parent, .symbols = std.StringHashMap(Symbol).init(allocator) };
        return self;
    }

    fn deinit(self: *Self) void {
        defer self.allocator.destroy(self);

        var symbol_it = self.symbols.iterator();
        while (symbol_it.next()) |symbol| {
            symbol.value_ptr.deinit();
        }

        self.symbols.deinit();
    }

    fn put(self: *Self, symbol: Symbol) !void {
        if (self.symbols.get(symbol.id)) |_| {
            return SemaError.AlreadyDefined;
        }

        try self.symbols.put(symbol.id, symbol);
    }

    fn replace(self: *Self, symbol: Symbol) !void {
        var old_symbol = try self.getOrThrow(symbol.id);
        old_symbol.deinit();
        try self.symbols.put(symbol.id, symbol);
    }

    fn get(self: *Self, id: []const u8) ?Symbol {
        if (self.symbols.get(id)) |s| return s;
        if (self.parent) |parent| {
            return parent.get(id);
        }
        return null;
    }

    fn getOrThrow(self: *Self, id: []const u8) !Symbol {
        if (self.get(id)) |s| return s;
        return SemaError.NotDefined;
    }
};

const Symbol = struct {
    const Self = @This();
    id: []const u8,
    type: Type,
    is_mut: bool,
    metadata: ?FnMetadata,

    fn deinit(self: *Self) void {
        if (self.metadata) |*metadata| {
            metadata.deinit();
        }
    }
};

const FnMetadata = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    params_types: std.ArrayList(Type),
    return_type: Type,

    fn init(allocator: std.mem.Allocator, params_types: std.ArrayList(Type), return_type: Type) Self {
        return Self{
            .allocator = allocator,
            .params_types = params_types,
            .return_type = return_type,
        };
    }

    fn deinit(self: *Self) void {
        self.params_types.deinit(self.allocator);
    }
};

const Type = enum {
    number,
    string,
    boolean,
    function,
    void,
    unknown,

    pub fn fromString(name: []const u8) !Type {
        if (std.mem.eql(u8, name, "number")) return .number;
        if (std.mem.eql(u8, name, "bool")) return .boolean;
        if (std.mem.eql(u8, name, "string")) return .string;
        if (std.mem.eql(u8, name, "fn")) return .function;
        if (std.mem.eql(u8, name, "void")) return .void;
        return SemaError.InvalidType;
    }
};

pub const SemaError = error{
    AlreadyDefined,
    NotDefined,
    InvalidExpressionType,
    InvalidOperation,
    InvalidReturnType,
    InvalidAssignment,
    InvalidFunction,
    InvalidFunctionParameter,
    InvalidType,
    OutOfMemory,
};

const std = @import("std");
const ast = @import("ast.zig");
