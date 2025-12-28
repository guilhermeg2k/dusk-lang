pub const Program = struct {
    instructions: std.ArrayList(Instruction),
    functions: std.ArrayList(Func),

    pub fn deinit(self: *Program) void {
        self.instructions.deinit();
        self.functions.deinit();
    }
};

pub const Func = struct {
    uid: usize,
    identifier: []const u8,
    args: std.ArrayList(FuncArg),
    return_type: Type,
    body: std.ArrayList(Instruction),
};

pub const FuncArg = struct {
    uid: usize,
    identifier: []const u8,
    type: Type,
};

pub const Instruction = union(enum) {
    store_var: StoreVar,
    update_var: UpdateVar,
    branch_if: BranchIf,
    loop: Loop,
    return_stmt: ReturnStmt,
    expression_stmt: ExpressionStmt,
};

pub const StoreVar = struct {
    uid: usize,
    identifier: []const u8,
    type: Type,
    value: *Value,
};

pub const UpdateVar = struct {
    var_uid: usize,
    identifier: []const u8,
    value: *Value,
};

pub const BranchIf = struct {
    condition: *Value,
    then_block: std.ArrayList(Instruction),
    else_block: std.ArrayList(Instruction),
};

pub const Loop = struct {
    condition: ?*Value,
    do_block: std.ArrayList(Instruction),
};

pub const ReturnStmt = struct {
    value: ?*Value,
};

pub const ExpressionStmt = struct {
    value: *Value,
};

pub const Value = union(enum) {
    const Self = @This();

    i_float: f64,
    i_bool: bool,
    i_string: []const u8,
    i_void: void,

    identifier: struct { uid: usize, identifier: []const u8, type: Type },

    binary_op: BinaryOp,

    unary_op: UnaryOp,

    fn_def: void,

    fn_call: FnCall,

    pub fn init(allocator: std.mem.Allocator, exp: Self) !*Self {
        const ptr = try allocator.create(Self);
        ptr.* = exp;
        return ptr;
    }

    pub fn toType(self: *Self) Type {
        return switch (self.*) {
            .i_float => .number,
            .i_bool => .boolean,
            .i_string => .string,
            .i_void => .void,
            .fn_def => .function,
            .identifier => self.identifier.type,
            .binary_op => self.binary_op.type,
            .unary_op => self.unary_op.type,
            .fn_call => self.fn_call.return_type,
        };
    }
};

pub const BinaryOp = struct {
    kind: BinaryOpKind,
    type: Type,
    left: *Value,
    right: *Value,
};

pub const UnaryOp = struct {
    kind: UnaryOpKind,
    type: Type,
    right: *Value,
};

pub const FnCall = struct {
    fn_uid: usize,
    identifier: []const u8,
    args: std.ArrayList(*Value),
    return_type: Type,
};

pub const UnaryOpKind = enum {
    neg,
    not,
};

pub const BinaryOpKind = enum {
    add,
    sub,
    mult,
    div,
    mod,

    cmp_eq,
    cmp_neq,

    cmp_lt,
    cmp_le,
    cmp_ge,
    cmp_gt,

    b_and,
    b_or,
};

const std = @import("std");
const sema = @import("sema.zig");
const SemaError = sema.Errors;
const Type = sema.Type;
