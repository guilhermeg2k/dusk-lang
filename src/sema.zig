pub const SemaAnalyzer = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    src: []const u8,
    program: ir.Program,
    scope: Scope,
    err_dispatcher: err.ErrorDispatcher,

    pub fn init(allocator: std.mem.Allocator, src: []const u8) !Self {
        return Self{
            .allocator = allocator,
            .src = src,
            .program = .{
                .instructions = .empty,
                .functions = .empty,
            },
            .scope = try Scope.init(allocator, .void),
            .err_dispatcher = .{ .src = src },
        };
    }

    pub fn deinit(self: *Self) void {
        self.scope.deinit();
    }

    pub fn analyze(self: *Self, src: []const u8, root: *const ast.Root) !ir.Program {
        self.src = src;
        self.program.instructions = try self.visitBlock(root);
        return self.program;
    }

    pub fn visitBlock(self: *Self, block: *const ast.Block) !std.ArrayList(ir.Instruction) {
        var instructions: std.ArrayList(ir.Instruction) = .empty;
        try self.scope.enter(self.scope.return_type);
        defer self.scope.exit(self.scope.return_type);

        for (block.statements.items) |stmt| {
            const instruction = switch (stmt.data) {
                .let_stmt => try self.visitLetStmt(&stmt),
                .assign_stmt => try self.visitAssignStmt(&stmt),
                .if_stmt => try self.visitIfStmt(&stmt),
                .for_stmt => try self.visitForStmt(&stmt),
                .fn_call_stmt => ir.Instruction{
                    .expression_stmt = .{ .value = try self.evalFnCall(&stmt.data.fn_call_stmt, stmt.loc_start) },
                },
                .return_stmt => try self.visitReturnStmt(&stmt),
            };

            if (instruction) |i| {
                try instructions.append(self.allocator, i);
            }
        }

        return instructions;
    }

    fn visitLetStmt(self: *Self, stmt: *const ast.StatementNode) !?ir.Instruction {
        const let_stmt = stmt.data.let_stmt;
        const expression_value = try self.evalExpression(let_stmt.value);
        const expression_type = expression_value.toType();

        const var_type = Type.fromString(let_stmt.type_annotation.name);
        if (var_type == null) {
            return self.err_dispatcher.typeNotDefined(var_type, stmt.loc_start);
        }

        const uid = self.scope.genUid();

        if (expression_type != var_type) {
            return self.err_dispatcher.invalidType(var_type, expression_type, "Invalid expression type expected {s} found {s}", .{
                var_type,
            }, stmt.data, self.src);
        }

        try self.scope.symbol_table.put(.{ .identifier = let_stmt.identifier, .uid = uid, .is_mut = let_stmt.is_mut, .type = var_type, .metadata = null });

        if (expression_type == .function) {
            try self.visitFnDef(&let_stmt.value.data.fn_def, let_stmt.identifier);
            return null;
        }

        return ir.Instruction{ .store_var = .{
            .uid = uid,
            .identifier = let_stmt.identifier,
            .type = var_type,
            .value = expression_value,
        } };
    }

    fn visitIfStmt(self: *Self, stmt: *const ast.StatementNode) Errors!ir.Instruction {
        const if_stmt = stmt.data.if_stmt;
        const condition_value = try self.evalExpression(if_stmt.condition);
        const condition_value_type = condition_value.toType();
        if (condition_value_type != .boolean) {
            return self.err_dispatcher.invalidType("boolean", condition_value_type, stmt.loc_start);
        }

        const then_block = try self.visitBlock(&if_stmt.then_block);

        var else_block: std.ArrayList(ir.Instruction) = .empty;
        if (if_stmt.else_block) |else_blc| {
            else_block = try self.visitBlock(&else_blc);
        }

        return ir.Instruction{ .branch_if = .{
            .condition = condition_value,
            .then_block = then_block,
            .else_block = else_block,
        } };
    }

    fn visitAssignStmt(self: *Self, stmt: *const ast.StatementNode) !ir.Instruction {
        const assign_stmt = stmt.data.assign_stmt;
        const identifier_symbol = self.scope.symbol_table.get(assign_stmt.identifier);
        if (identifier_symbol == null) {
            return self.err_dispatcher.notDefined(assign_stmt.identifier, stmt.loc_start);
        }

        const exp_value = try self.evalExpression(assign_stmt.exp);
        const exp_value_type = exp_value.toType();

        if (!identifier_symbol.is_mut) {
            return self.err_dispatcher.notMutable(assign_stmt.identifier, stmt.loc_start);
        }

        if (identifier_symbol.type != exp_value_type) {
            return self.err_dispatcher.invalidType(identifier_symbol.type, exp_value, stmt.loc_start);
        }

        return ir.Instruction{ .update_var = .{
            .identifier = identifier_symbol.identifier,
            .var_uid = identifier_symbol.uid,
            .value = exp_value,
        } };
    }

    fn visitForStmt(self: *Self, stmt: *const ast.StatementNode) Errors!ir.Instruction {
        const for_stmt = stmt.data.for_stmt;
        var condition_value: ?*ir.Value = null;

        if (for_stmt.condition) |condition| {
            condition_value = try self.evalExpression(condition);
            const condition_value_type = condition_value.toType();
            if (condition_value_type != .boolean) {
                return self.err_dispatcher.invalidType("boolean", condition_value_type, stmt.loc_start);
            }
        }

        const block = try self.visitBlock(&for_stmt.do_block);

        return ir.Instruction{ .loop = .{
            .condition = condition_value,
            .do_block = block,
        } };
    }

    fn visitFnDef(self: *Self, exp: *const ast.ExpNode, identifier: []const u8) Errors!void {
        const fn_def = exp.data.fn_def;
        const old_return_type = self.scope.return_type;
        const return_type = Type.fromString(fn_def.return_type.name);
        if (return_type == null) {
            return self.err_dispatcher.typeNotDefined(return_type, exp.loc_start);
        }

        var arguments: std.ArrayList(ir.FuncArg) = .empty;
        var argument_types: std.ArrayList(Type) = .empty;

        const fn_uid = self.scope.genUid();

        for (fn_def.arguments.items) |arg| {
            const arg_type = Type.fromString(arg.type_annotation.name);
            if (arg_type == null) {
                return self.err_dispatcher.typeNotDefined(arg_type, exp.loc_start);
            }

            try argument_types.append(self.allocator, arg_type.?);
        }

        try self.scope.symbol_table.replace(.{ .identifier = identifier, .uid = fn_uid, .is_mut = false, .type = .function, .metadata = FuncMetadata.init(self.allocator, argument_types, return_type) });

        try self.scope.enter(return_type);
        defer self.scope.exit(old_return_type);

        for (fn_def.arguments.items) |arg| {
            const arg_type = Type.fromString(arg.type_annotation.name).?;
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

        const body = try self.visitBlock(&fn_def.body_block);
        try self.program.functions.append(self.allocator, ir.Func{
            .uid = fn_uid,
            .identifier = identifier,
            .args = arguments,
            .return_type = return_type,
            .body = body,
        });
    }

    fn visitReturnStmt(self: *Self, stmt: *const ast.StatementNode) !ir.Instruction {
        const return_stmt = stmt.data.return_stmt;

        if (return_stmt.exp) |exp| {
            const exp_value = try self.evalExpression(exp);
            const exp_value_type = exp_value.toType();

            if (exp_value_type != self.scope.return_type) {
                return self.err_dispatcher.invalidType(
                    @tagName(self.scope.return_type),
                    @tagName(exp_value_type),
                    stmt.loc_start,
                );
            }

            return ir.Instruction{ .return_stmt = .{
                .value = exp_value,
            } };
        }

        if (self.scope.return_type != .void) {
            return self.err_dispatcher.invalidType(
                @tagName(self.scope.return_type),
                "void",
                stmt.loc_start,
            );
        }

        return ir.Instruction{ .return_stmt = .{
            .value = null,
        } };
    }

    fn evalExpression(self: *Self, exp: *const ast.ExpNode) Errors!*ir.Value {
        switch (exp.*.data) {
            .number_literal => {
                return ir.Value.init(self.allocator, .{ .i_float = exp.data.number_literal });
            },
            .string_literal => {
                return ir.Value.init(self.allocator, .{ .i_string = exp.data.string_literal });
            },
            .bool_literal => {
                return ir.Value.init(self.allocator, .{ .i_bool = exp.data.bool_literal });
            },
            .fn_def => {
                return ir.Value.init(self.allocator, .{ .fn_def = {} });
            },
            .identifier => {
                return self.evalIdentifier(exp);
            },
            .fn_call => {
                return self.evalFnCall(&exp.data.fn_call, exp.loc_start);
            },
            .unary_exp => {
                return self.evalUnaryExp(exp);
            },
            .binary_exp => {
                return self.evalBinaryExp(exp);
            },
        }
    }

    fn evalFnCall(self: *Self, fn_call: *const ast.FnCall, loc_start: usize) !*ir.Value {
        const func_symbol = try self.scope.symbol_table.getOrThrow(fn_call.identifier);
        if (func_symbol.type != .function) {
            return self.err_dispatcher.invalidType("function", @tagName(func_symbol.type), loc_start, self.src);
        }

        var fn_call_arguments_values: std.ArrayList(*ir.Value) = .empty;

        if (func_symbol.metadata) |fn_data| {
            //if arg[0] is unknown it disables any check for the fn_call
            //currently this is only useful for allowing anything as arg for the echo function
            //when echo function be propertly implemented this should be modified
            const is_arg_unknown = fn_data.params_types.items.len == 1 and fn_data.params_types.items[0] == .unknown;
            const args_len = fn_call.arguments.items.len;
            const fn_params_len = fn_data.params_types.items.len;

            if (!is_arg_unknown and fn_params_len != args_len) {
                return self.err_dispatcher.invalidNumberOfArgs(args_len, fn_params_len, loc_start);
            }

            for (fn_call.arguments.items, 0..) |arg, i| {
                const fn_call_arg_value = try self.evalExpression(arg);

                const param_type = fn_data.params_types.items[i];
                const arg_type = fn_call_arg_value.toType();
                if (!is_arg_unknown and arg_type != param_type) {
                    return self.err_dispatcher.invalidType(param_type, arg_type, loc_start);
                }

                try fn_call_arguments_values.append(self.allocator, fn_call_arg_value);
            }

            return ir.Value.init(self.allocator, .{ .fn_call = .{ .fn_uid = func_symbol.uid, .identifier = func_symbol.identifier, .return_type = fn_data.return_type, .args = fn_call_arguments_values } });
        }

        unreachable;
    }

    fn evalIdentifier(self: *Self, exp: *ast.ExpNode) !*ir.Value {
        const id = exp.data.identifier;
        const id_symbol = self.scope.symbol_table.get(id);
        if (id_symbol == null) {
            return self.err_dispatcher.notDefined(id_symbol, exp.loc_start);
        }

        return ir.Value.init(self.allocator, .{ .identifier = .{
            .uid = id_symbol.uid,
            .identifier = id_symbol.identifier,
            .type = id_symbol.type,
        } });
    }

    fn evalUnaryExp(self: *Self, exp: *const ast.ExpNode) !*ir.Value {
        const unary_exp = exp.data.unary_exp;
        const exp_value = try self.evalExpression(unary_exp.right);
        const exp_type = exp_value.toType();

        switch (unary_exp.op) {
            .neg => {
                if (exp_type != .number) {
                    return self.err_dispatcher.invalidType("number", @tagName(exp_type), exp.loc_start);
                }
            },
            .not => {
                if (exp_type != .boolean) {
                    return self.err_dispatcher.invalidType("boolean", @tagName(exp_type), exp.loc_start);
                }
            },
        }

        return ir.Value.init(self.allocator, .{ .unary_op = .{ .kind = self.astUnaryOpToIrUnaryOpKind(unary_exp.op), .type = exp_type, .right = exp_value } });
    }

    fn evalBinaryExp(self: *Self, exp: *const ast.ExpNode) !*ir.Value {
        const bin_exp = exp.data.binary_exp;
        const left_value = try self.evalExpression(bin_exp.left);
        const right_value = try self.evalExpression(bin_exp.right);
        const left_type = left_value.toType();
        const right_type = right_value.toType();
        var op_type: Type = .void;

        if (left_type != right_type) {
            return self.err_dispatcher.invalidType(left_type, right_type, exp.loc_start);
        }

        switch (bin_exp.op) {
            .add, .sub, .mult, .div, .mod => {
                if (left_type != .number) {
                    return self.err_dispatcher.invalidType("number", left_type, exp.loc_start);
                }
                op_type = .number;
            },

            //warn: this makes support string comparison by operators
            .eq, .not_eq, .lt, .lt_or_eq, .gt, .gt_or_eq => {
                if (left_type == .boolean) {
                    return self.err_dispatcher.invalidType("string, number", left_type, exp.loc_start);
                }
                op_type = .boolean;
            },

            .bool_or, .bool_and => {
                if (left_type != .boolean) {
                    return self.err_dispatcher.invalidType("boolean", left_type, exp.loc_start);
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
            .bool_or => .b_or,
            .bool_and => .b_and,
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

        try symbols.put("echo", .{ .uid = 0, .identifier = "echo", .type = .function, .is_mut = false, .metadata = FuncMetadata.init(ptr.allocator, echo_params, .void) });

        ptr.* = Self{ .allocator = allocator, .parent = parent, .symbols = symbols };
        return ptr;
    }

    fn put(self: *Self, symbol: Symbol) !void {
        if (self.symbols.get(symbol.identifier)) |_| {
            return Errors.AlreadyDefined;
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
        return Errors.SemaError;
    }
};

const Symbol = struct {
    const Self = @This();
    uid: usize,
    identifier: []const u8,
    type: Type,
    is_mut: bool,
    metadata: ?FuncMetadata,

    fn deinit(self: *Self) void {
        if (self.metadata) |*metadata| {
            metadata.deinit();
        }
    }
};

const FuncMetadata = struct {
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

    pub fn fromString(name: []const u8) Type {
        if (std.mem.eql(u8, name, "number")) return .number;
        if (std.mem.eql(u8, name, "bool")) return .boolean;
        if (std.mem.eql(u8, name, "string")) return .string;
        if (std.mem.eql(u8, name, "fn")) return .function;
        if (std.mem.eql(u8, name, "void")) return .void;
        return null;
    }
};

pub const Errors = err.Errors;

const err = @import("error.zig");
const ir = @import("ir.zig");
const ast = @import("ast.zig");
const std = @import("std");
const allocPrint = std.fmt.allocPrint;
