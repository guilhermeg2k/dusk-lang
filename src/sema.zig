pub const SemaAnalyzer = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    scope: Scope,
    program: ir.Program,

    pub fn init(allocator: std.mem.Allocator) !Self {
        return Self{ .allocator = allocator, .scope = try Scope.init(allocator, .void), .program = .{
            .instructions = .empty,
            .functions = .empty,
        } };
    }

    pub fn deinit(self: *Self) void {
        self.scope.deinit();
    }

    pub fn analyze(self: *Self, root: *const ast.Root) !ir.Program {
        self.program.instructions = try self.visitBlock(root);
        return self.program;
    }

    pub fn visitBlock(self: *Self, block: *const ast.Block) !std.ArrayList(ir.Instruction) {
        var instructions: std.ArrayList(ir.Instruction) = .empty;
        try self.scope.enter(self.scope.return_type);
        defer self.scope.exit(self.scope.return_type);

        for (block.statements.items) |stmt| {
            const instruction = switch (stmt) {
                .let_stmt => try self.visitLetStmt(&stmt.let_stmt),
                .assign_stmt => try self.visitAssignStmt(&stmt.assign_stmt),
                .if_stmt => try self.visitIfStmt(&stmt.if_stmt),
                .for_stmt => try self.visitForStmt(&stmt.for_stmt),
                .fn_call_stmt => ir.Instruction{ .expression_stmt = .{ .value = try self.analyzeFnCall(&stmt.fn_call_stmt) } },
                .return_stmt => try self.visitReturnStmt(&stmt.return_stmt),
            };

            if (instruction) |i| {
                try instructions.append(self.allocator, i);
            }
        }

        return instructions;
    }

    fn evaluateExpression(self: *Self, exp: *const ast.Exp) SemaError!*ir.Value {
        switch (exp.*) {
            .number_literal => {
                return ir.Value.init(self.allocator, .{ .i_float = exp.number_literal });
            },
            .string_literal => {
                return ir.Value.init(self.allocator, .{ .i_string = exp.string_literal });
            },
            .bool_literal => {
                return ir.Value.init(self.allocator, .{ .i_bool = exp.bool_literal });
            },
            .fn_def => {
                return ir.Value.init(self.allocator, .{ .fn_def = {} });
            },
            .identifier => {
                return self.visitIdentifier(exp.identifier);
            },
            .fn_call => {
                return self.analyzeFnCall(&exp.fn_call);
            },
            .unary_exp => {
                return self.evaluateUnaryExp(&exp.unary_exp);
            },
            .binary_exp => {
                return self.evaluateBinaryExp(&exp.binary_exp);
            },
        }
    }

    fn visitLetStmt(self: *Self, letStmt: *const ast.LetStmt) !?ir.Instruction {
        const expression_value = try self.evaluateExpression(letStmt.value);
        const expression_type = expression_value.toType();
        const var_type = try Type.fromString(letStmt.type_annotation.name);
        const uid = self.scope.genUid();

        if (expression_type != var_type) {
            return SemaError.InvalidExpressionType;
        }

        try self.scope.symbol_table.put(.{ .identifier = letStmt.identifier, .uid = uid, .is_mut = letStmt.is_mut, .type = var_type, .metadata = null });

        if (expression_type == .function) {
            try self.visitFnDef(&letStmt.value.fn_def, letStmt.identifier);
            return null;
        }

        return ir.Instruction{ .store_var = .{
            .uid = uid,
            .identifier = letStmt.identifier,
            .type = var_type,
            .value = expression_value,
        } };
    }

    fn visitIfStmt(self: *Self, ifStmt: *const ast.IfStmt) SemaError!ir.Instruction {
        const condition_value = try self.evaluateExpression(ifStmt.condition);
        if (condition_value.toType() != .boolean) {
            return SemaError.InvalidExpressionType;
        }

        const then_block = try self.visitBlock(&ifStmt.then_block);

        var else_block: std.ArrayList(ir.Instruction) = .empty;
        if (ifStmt.else_block) |else_blc| {
            else_block = try self.visitBlock(&else_blc);
        }

        return ir.Instruction{ .branch_if = .{
            .condition = condition_value,
            .then_block = then_block,
            .else_block = else_block,
        } };
    }

    fn visitAssignStmt(self: *Self, assignStmt: *const ast.AssignStmt) !ir.Instruction {
        const identifier_symbol = try self.scope.symbol_table.getOrThrow(assignStmt.identifier);
        const exp_value = try self.evaluateExpression(assignStmt.exp);

        if (!identifier_symbol.is_mut) {
            return SemaError.InvalidAssignment;
        }

        if (identifier_symbol.type != exp_value.toType()) {
            return SemaError.InvalidAssignment;
        }

        return ir.Instruction{ .update_var = .{
            .identifier = identifier_symbol.identifier,
            .var_uid = identifier_symbol.uid,
            .value = exp_value,
        } };
    }

    fn analyzeFnCall(self: *Self, fnCall: *const ast.FnCall) !*ir.Value {
        const func_symbol = try self.scope.symbol_table.getOrThrow(fnCall.identifier);
        if (func_symbol.type != .function) {
            return SemaError.InvalidFunction;
        }

        var fn_call_arguments_values: std.ArrayList(*ir.Value) = .empty;

        if (func_symbol.metadata) |fn_data| {
            const is_arg_unknown = fn_data.params_types.items.len == 1 and fn_data.params_types.items[0] == .unknown;

            if (!is_arg_unknown and fn_data.params_types.items.len != fnCall.arguments.items.len) {
                return SemaError.InvalidFunctionParameter;
            }

            for (fn_data.params_types.items, 0..) |param_type, i| {
                const fn_call_arg = try self.evaluateExpression(fnCall.arguments.items[i]);
                if (!is_arg_unknown and fn_call_arg.toType() != param_type) {
                    return SemaError.InvalidFunctionParameter;
                }
                try fn_call_arguments_values.append(self.allocator, fn_call_arg);
            }

            return ir.Value.init(self.allocator, .{ .fn_call = .{ .fn_uid = func_symbol.uid, .identifier = func_symbol.identifier, .return_type = fn_data.return_type, .args = fn_call_arguments_values } });
        }

        unreachable;
    }

    fn visitForStmt(self: *Self, forStmt: *const ast.ForStmt) SemaError!ir.Instruction {
        var condition_value: ?*ir.Value = null;
        if (forStmt.condition) |condition| {
            const exp_value = try self.evaluateExpression(condition);
            condition_value = exp_value;
            if (exp_value.toType() != .boolean) {
                return SemaError.InvalidExpressionType;
            }
        }

        const block = try self.visitBlock(&forStmt.do_block);

        return ir.Instruction{ .loop = .{
            .condition = condition_value,
            .do_block = block,
        } };
    }

    fn visitFnDef(self: *Self, fnDef: *const ast.FnDef, identifier: []const u8) SemaError!void {
        const old_return_type = self.scope.return_type;
        const return_type = try Type.fromString(fnDef.return_type.name);

        var arguments: std.ArrayList(ir.FuncArg) = .empty;
        var argument_types: std.ArrayList(Type) = .empty;

        const fn_uid = self.scope.genUid();

        for (fnDef.arguments.items) |arg| {
            const arg_type = try Type.fromString(arg.type_annotation.name);
            try argument_types.append(self.allocator, arg_type);
        }

        try self.scope.symbol_table.replace(.{ .identifier = identifier, .uid = fn_uid, .is_mut = false, .type = .function, .metadata = FnMetadata.init(self.allocator, argument_types, return_type) });

        try self.scope.enter(return_type);
        defer self.scope.exit(old_return_type);

        for (fnDef.arguments.items) |arg| {
            const arg_type = try Type.fromString(arg.type_annotation.name);
            const uid = self.scope.genUid();
            try self.scope.symbol_table.put(.{
                .uid = uid,
                .identifier = arg.identifier,
                //todo: later this can also be passed as argument
                .is_mut = false,
                .type = arg_type,
                .metadata = null,
            });
            try arguments.append(self.allocator, .{
                .uid = uid,
                .identifier = arg.identifier,
                .type = arg_type,
            });
        }

        const body = try self.visitBlock(&fnDef.body_block);
        try self.program.functions.append(self.allocator, ir.Func{
            .uid = fn_uid,
            .identifier = identifier,
            .args = arguments,
            .return_type = return_type,
            .body = body,
        });
    }

    fn visitReturnStmt(self: *Self, returnStmt: *const ast.ReturnStmt) !ir.Instruction {
        if (returnStmt.exp) |exp| {
            const exp_value = try self.evaluateExpression(exp);
            if (exp_value.toType() != self.scope.return_type) {
                return SemaError.InvalidReturnType;
            }
            return ir.Instruction{ .return_stmt = .{
                .value = exp_value,
            } };
        }

        if (self.scope.return_type != .void) {
            return SemaError.InvalidReturnType;
        }

        return ir.Instruction{ .return_stmt = .{
            .value = null,
        } };
    }

    fn visitIdentifier(self: *Self, id: []const u8) !*ir.Value {
        const id_symbol = try self.scope.symbol_table.getOrThrow(id);

        return ir.Value.init(self.allocator, .{ .identifier = .{
            .uid = id_symbol.uid,
            .identifier = id_symbol.identifier,
            .type = id_symbol.type,
        } });
    }

    fn evaluateUnaryExp(self: *Self, unary_exp: *const ast.UnaryExp) !*ir.Value {
        const exp_value = try self.evaluateExpression(unary_exp.right);
        const exp_type = exp_value.toType();

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

        return ir.Value.init(self.allocator, .{ .unary_op = .{ .kind = self.astUnaryOpToIrUnaryOpKind(unary_exp.op), .type = exp_type, .right = exp_value } });
    }

    fn evaluateBinaryExp(self: *Self, bin_exp: *const ast.BinaryExp) !*ir.Value {
        const left_value = try self.evaluateExpression(bin_exp.left);
        const right_value = try self.evaluateExpression(bin_exp.right);
        const left_type = left_value.toType();
        const right_type = right_value.toType();
        var op_type: Type = .void;

        if (left_type != right_type) {
            return SemaError.InvalidExpressionType;
        }

        switch (bin_exp.op) {
            .add, .sub, .mult, .div, .mod => {
                if (left_type != .number) {
                    return SemaError.InvalidOperation;
                }
                op_type = .number;
            },

            .eq, .not_eq, .lt, .lt_or_eq, .gt, .gt_or_eq => {
                if (left_type == .boolean) {
                    return SemaError.InvalidOperation;
                }
                op_type = .boolean;
            },

            .bool_or, .bool_and => {
                if (left_type != .boolean) {
                    return SemaError.InvalidOperation;
                }
                op_type = .boolean;
            },
        }

        return ir.Value.init(self.allocator, .{ .binary_op = .{
            .kind = self.astBinOpToIrBinOpKind(bin_exp.op),
            .type = op_type,
            .left = left_value,
            .right = right_value,
        } });
    }

    fn astBinOpToIrBinOpKind(_: *Self, bin_op: ast.BinaryOp) ir.BinaryOpKind {
        return switch (bin_op) {
            .add => .add,
            .sub => .sub,
            .mult => .mult,
            .div => .div,
            .mod => .mod,
            .eq => .cmp_eq,
            .not_eq => .cmp_neq,
            .lt => .cmp_lt,
            .lt_or_eq => .cmp_le,
            .gt => .cmp_gt,
            .gt_or_eq => .cmp_ge,
            .bool_or => .b_and,
            .bool_and => .b_or,
        };
    }

    fn astUnaryOpToIrUnaryOpKind(_: *Self, bin_op: ast.UnaryOp) ir.UnaryOpKind {
        return switch (bin_op) {
            .neg => .neg,
            .not => .not,
        };
    }
};

const Scope = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    symbol_table: *SymbolTable,
    return_type: Type,
    next_uid: usize,

    pub fn init(allocator: std.mem.Allocator, return_type: Type) !Self {
        return Self{ .allocator = allocator, .symbol_table = try SymbolTable.init(allocator, null), .return_type = return_type, .next_uid = 10 };
    }

    pub fn deinit(self: *Self) void {
        var current_ptr: ?*SymbolTable = self.symbol_table;
        while (current_ptr) |table| {
            const parent = table.parent;
            table.deinit();
            current_ptr = parent;
        }
    }

    pub fn genUid(self: *Self) usize {
        defer self.next_uid += 1;
        return self.next_uid;
    }

    pub fn enter(self: *Self, return_type: Type) !void {
        self.return_type = return_type;
        self.symbol_table = try SymbolTable.init(self.allocator, self.symbol_table);
    }

    pub fn exit(self: *Self, return_type: Type) void {
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
        const ptr = try allocator.create(Self);

        var symbols = std.StringHashMap(Symbol).init(allocator);
        var echo_params: std.ArrayList(Type) = .empty;
        try echo_params.append(allocator, .unknown);

        try symbols.put("echo", .{ .uid = 0, .identifier = "echo", .type = .function, .is_mut = false, .metadata = FnMetadata.init(ptr.allocator, echo_params, .void) });

        ptr.* = Self{ .allocator = allocator, .parent = parent, .symbols = symbols };
        return ptr;
    }

    fn put(self: *Self, symbol: Symbol) !void {
        if (self.symbols.get(symbol.identifier)) |_| {
            return SemaError.AlreadyDefined;
        }

        try self.symbols.put(symbol.identifier, symbol);
    }

    fn replace(self: *Self, symbol: Symbol) !void {
        var old_symbol = try self.getOrThrow(symbol.identifier);
        old_symbol.deinit();
        try self.symbols.put(symbol.identifier, symbol);
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
    uid: usize,
    identifier: []const u8,
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

pub const Type = enum {
    number,
    string,
    boolean,
    function,
    //currently only used for internals
    unknown,
    void,

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
const ir = @import("ir.zig");
