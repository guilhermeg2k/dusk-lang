pub const SemaAnalyzer = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    scope: Scope,

    pub fn init(allocator: std.mem.Allocator) !Self {
        return Self{ .allocator = allocator, .scope = try Scope.init(allocator, .void) };
    }

    pub fn deinit(self: *Self) void {
        var next = self.current_scope;
        while (next != null) {
            next = self.current_scope.deinit();
        }
    }

    pub fn analyze(self: *Self, root: *const ast.Root) !void {
        try self.visitBlock(root);
    }

    pub fn visitBlock(self: *Self, block: *const ast.Block) !void {
        for (block.statements.items) |stmt| {
            switch (stmt) {
                .let_stmt => {
                    return self.visitLetStmt(&stmt.let_stmt);
                },
                .if_stmt => {
                    return self.visitIfStmt(&stmt.if_stmt);
                },
                .for_stmt => {
                    return self.visitForStmt(&stmt.for_stmt);
                },
                .assign_stmt => {
                    return self.visitAssignStmt(&stmt.assign_stmt);
                },
                .fn_call_stmt => {
                    _ = try self.analyzeFnCall(&stmt.fn_call_stmt);
                    return;
                },
                .return_stmt => {
                    return self.visitReturnStmt(&stmt.return_stmt);
                },
            }
        }
    }

    fn anaylizeExpression(self: *Self, exp: *const ast.Exp) SemaError!Type {
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
        const expression_type = try self.anaylizeExpression(letStmt.value);
        const var_type = try Type.fromString(letStmt.type_annotation.name);

        if (expression_type != var_type) {
            return SemaError.InvalidExpressionType;
        }

        var metadata: ?FnMetadata = null;
        if (expression_type == .function) {
            const fn_metadata = try self.visitFnDef(&letStmt.value.fn_def);
            metadata = fn_metadata;
        }

        try self.scope.symbol_table.put(.{ .id = letStmt.identifier, .is_mut = letStmt.is_mut, .type = var_type, .metadata = metadata });
    }

    fn visitIfStmt(self: *Self, ifStmt: *const ast.IfStmt) SemaError!void {
        const exp_type = try self.anaylizeExpression(ifStmt.condition);
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
        const exp_type = try self.anaylizeExpression(assignStmt.exp);

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
            if (fn_data.param_types.items.len != fnCall.arguments.items.len) {
                return SemaError.InvalidFunctionParameter;
            }

            for (fn_data.param_types.items, 0..) |param_type, i| {
                const fn_call_param_type = try self.anaylizeExpression(fnCall.arguments.items[i]);
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
            const exp_type = try self.anaylizeExpression(condition);
            if (exp_type != .boolean) {
                return SemaError.InvalidExpressionType;
            }
        }

        try self.visitBlock(&forStmt.do_block);
    }

    fn visitFnDef(self: *Self, fnDef: *const ast.FnDef) SemaError!FnMetadata {
        var argument_types: std.ArrayList(Type) = .empty;
        const return_type = try Type.fromString(fnDef.return_type.name);

        try self.scope.enter(fnDef.return_type.name);
        defer self.scope.exit();

        for (fnDef.arguments.items) |arg| {
            const arg_type = try Type.fromString(arg.type_annotation.name);
            try argument_types.append(self.allocator, arg_type);
            try self.scope.symbol_table.put(.{
                .id = arg.identifier,
                //todo: later this can also be passed as argument
                .is_mut = false,
                .type = arg_type,
                .metadata = null,
            });
        }

        try self.visitBlock(&fnDef.body_block);

        return FnMetadata{
            .param_types = argument_types,
            .return_type = return_type,
        };
    }

    fn visitReturnStmt(self: *Self, returnStmt: *const ast.ReturnStmt) !void {
        if (returnStmt.exp) |exp| {
            const exp_type = try self.anaylizeExpression(exp);
            if (exp_type != self.scope.return_type.current) {
                return SemaError.InvalidReturnType;
            }
            return;
        }

        if (self.scope.return_type.current != .void) {
            return SemaError.InvalidReturnType;
        }
    }

    fn visitIdentifier(self: *Self, id: []const u8) !Symbol {
        return self.scope.symbol_table.getOrThrow(id);
    }

    fn analyzeUnaryExp(self: *Self, unary_exp: *const ast.UnaryExp) !Type {
        const exp_type = try self.anaylizeExpression(unary_exp.right);

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
        const left_type = try self.anaylizeExpression(bin_exp.left);
        const right_type = try self.anaylizeExpression(bin_exp.right);

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
                return left_type;
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
    return_type: struct {
        current: Type,
        previous: ?Type,
    },

    pub fn init(allocator: std.mem.Allocator, return_type: Type) !Self {
        return Self{ .allocator = allocator, .symbol_table = try SymbolTable.init(allocator, null), .return_type = .{
            .current = return_type,
            .previous = null,
        } };
    }

    pub fn enter(self: *Self, return_type: []const u8) !void {
        self.return_type.previous = self.return_type.current;
        self.return_type.current = try Type.fromString(return_type);
        self.symbol_table = try SymbolTable.init(self.allocator, self.symbol_table);
    }

    pub fn exit(self: *Self) void {
        defer self.symbol_table.deinit();
        if (self.return_type.previous) |return_type| self.return_type.current = return_type;
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

    pub fn deinit(self: *Self) void {
        self.symbols.deinit();
        self.allocator.destroy(self);
    }

    pub fn put(self: *Self, symbol: Symbol) !void {
        if (self.symbols.get(symbol.id)) |_| {
            return SemaError.AlreadyDefined;
        }

        try self.symbols.put(symbol.id, symbol);
    }

    pub fn get(self: *Self, id: []const u8) ?Symbol {
        if (self.symbols.get(id)) |s| return s;
        if (self.parent) |parent| {
            return parent.get(id);
        }
        return null;
    }

    pub fn getOrThrow(self: *Self, id: []const u8) !Symbol {
        if (self.get(id)) |s| return s;
        return SemaError.NotDefined;
    }
};

const Symbol = struct {
    id: []const u8,
    type: Type,
    is_mut: bool,

    metadata: ?FnMetadata,
};

const FnMetadata = struct {
    param_types: std.ArrayList(Type),
    return_type: Type,
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
// const lexer = @import("lexer.zig");
// const parser = @import("parser.zig");
// const Token = lexer.Token;
// const Tag = lexer.Tag;
