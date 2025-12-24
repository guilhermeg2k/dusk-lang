pub const SemaAnalyzer = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    scope: Scope,

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{ .allocator = allocator, .scope = try Scope.init(allocator) };
    }

    pub fn deinit(self: *Self) void {
        var next = self.current_scope;
        while (next != null) {
            next = self.current_scope.deinit();
        }
    }

    pub fn analyze(self: *Self, root: *ast.Root) !void {
        self.visitBlock(&root);
    }

    pub fn visitBlock(self: *Self, block: *ast.Block) !void {
        for (block.statements.items) |stmt| {
            switch (stmt) {
                .let_stmt => {
                    return self.visitLetStmt(&stmt);
                },
                .if_stmt => {
                    return self.visitIfStmt(&stmt);
                },
                .for_stmt => {
                    return self.visitForStmt(&stmt);
                },
                .assign_stmt => {
                    return self.visitAssignStmt(&stmt);
                },
                .fn_call_stmt => {
                    try self.analyzeFnCall(&stmt);
                    return;
                },
                .return_stmt => {
                    return self.visitReturnStmt(&stmt);
                },
                else => unreachable,
            }
        }
    }

    pub fn anaylizeExpression(_: *Self, exp: *ast.Exp) !Type {
        switch (exp) {
            .number_literal => {
                return .number;
            },
            .string_literal => {
                return .string;
            },
            .bool_literal => {
                return .boolean;
            },
            else => unreachable,
        }
    }

    pub fn visitLetStmt(self: *Self, letStmt: *ast.LetStmt) !void {
        const expression_type = try self.anaylizeExpression(letStmt.value);
        const var_type = Type.fromString(letStmt.type_annotation.name);

        if (expression_type != var_type) {
            return SemaError.InvalidExpressionType;
        }

        self.scope.symbol_table.put(.{ .id = letStmt.identifier, .is_mut = letStmt.is_mut, .type = var_type });
    }

    pub fn visitIfStmt(self: *Self, ifStmt: *ast.IfStmt) !void {
        const exp_type = try self.anaylizeExpression(ifStmt.condition);
        if (exp_type != .boolean) {
            return SemaError.InvalidExpressionType;
        }

        try self.visitBlock(ifStmt.then_block);

        if (ifStmt.else_block) |else_blc| {
            try self.visitBlock(else_blc);
        }
    }

    fn visitAssignStmt(self: *Self, assignStmt: *ast.AssignStmt) !void {
        const identifier_symbol = try self.visitIdentifier(assignStmt.identifier);
        const exp_type = try self.anaylizeExpression(assignStmt.exp);

        if (!identifier_symbol.is_mut) {
            return SemaError.InvalidAssignment;
        }

        if (identifier_symbol.type != exp_type) {
            return SemaError.InvalidAssignment;
        }
    }

    fn analyzeFnCall(self: *Self, fnCall: *ast.FnCall) !Type {
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

    pub fn visitForStmt(self: *Self, forStmt: *ast.ForStmt) !void {
        const exp_type = try self.anaylizeExpression(forStmt.condition);
        if (exp_type != .boolean) {
            return SemaError.InvalidExpressionType;
        }

        try self.visitBlock(forStmt.do_block);
    }

    pub fn visitFnDef(self: *Self, fnDef: *ast.FnDef) !void {
        const argument_types: std.ArrayList(Type) = .empty;
        const return_type = Type.fromString(fnDef.return_type.name);
        self.scope.enter(return_type);
        defer self.scope.exit();

        for (fnDef.arguments.items) |arg| {
            const arg_type = Type.fromString(arg.type_annotation);
            argument_types.append(self.allocator, arg_type);
            self.current_scope.put(.{
                .id = arg.identifier,
                //todo: later this can also be passed as argument
                .is_mut = false,
                .type = arg_type,
            });
        }

        try self.visitBlock(&fnDef.body_block);
    }

    pub fn visitReturnStmt(self: *Self, returnStmt: *ast.ReturnStmt) !void {
        if (returnStmt.exp) |exp| {
            const exp_type = self.anaylizeExpression(exp);
            if (exp_type != self.scope.return_type.current) {
                return SemaError.InvalidReturnType;
            }
            return;
        }

        if (self.scope.return_type.current != .void) {
            return SemaError.InvalidReturnType;
        }
    }

    pub fn visitIdentifier(self: *Self, id: []const u8) !Symbol {
        return self.current_scope.getOrThrow(id);
    }

    pub fn analyzeBinaryExp(self: *Self, bin_exp: *ast.BinaryExp) !Type {
        const left_type = self.anaylizeExpression(bin_exp.left);
        const right_type = self.anaylizeExpression(bin_exp.right);

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

pub const Scope = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    symbol_table: *SymbolTable,
    return_type: struct {
        current: Type = void,
        previous: ?Type,
    },

    pub fn init(allocator: std.mem.Allocator, return_type: Type) Self {
        return Self{ .allocator = allocator, .symbol_table = try SymbolTable.init(allocator), .return_type = .{
            .current = return_type,
            .previous = null,
        } };
    }

    pub fn enter(self: *Self) !void {
        self.return_type.previous = self.return_type.current;
        self.symbol_table = try SymbolTable.init(self.allocator, self.symbol_table);
    }

    pub fn exit(self: *Self) !void {
        if (self.return_type.previous) |return_type| self.return_type.previous = return_type;
        if (self.current_scope.parent) |parent_scope| self.current_scope = parent_scope;
    }
};

pub const SymbolTable = struct {
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

pub const Symbol = struct {
    id: []const u8,
    type: Type,
    is_mut: bool,

    metadata: ?FnMetadata,
};

pub const FnMetadata = struct {
    param_types: std.ArrayList(Type),
    return_type: Type,
};

pub const Type = enum {
    number,
    string,
    boolean,
    function,
    void,
    unknown,

    pub fn fromString(name: []const u8) Type {
        if (std.mem.eql(u8, name, "number")) return .number;
        if (std.mem.eql(u8, name, "bool")) return .boolean;
        if (std.mem.eql(u8, name, "string")) return .string;
        if (std.mem.eql(u8, name, "fn")) return .function;
        if (std.mem.eql(u8, name, "void")) return .void;
        return .unknown;
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
    OutOfMemory,
};

const std = @import("std");
const ast = @import("ast.zig");
// const lexer = @import("lexer.zig");
// const parser = @import("parser.zig");
// const Token = lexer.Token;
// const Tag = lexer.Tag;
