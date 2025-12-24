pub const Program = struct {
    instructions: InstructionBlock,
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
    body: InstructionBlock,
};

pub const FuncArg = struct {
    uid: usize,
    type: Type,
};

pub const Instruction = union(enum) {
    store_var: struct {
        uid: usize,
        type: Type,
        value: *Value,
    },

    update_var: struct {
        var_uid: usize,
        value: *Value,
    },

    branch_if: struct {
        condition: *Value,
        then_block: InstructionBlock,
        else_block: InstructionBlock,
    },

    loop: struct {
        condition: ?*Value,
        do_block: InstructionBlock,
    },

    return_stmt: struct {
        value: ?*Value,
    },

    expression_stmt: struct {
        value: *Value,
    },
};

const InstructionBlock = std.ArrayList(Instruction);

pub const Value = union(enum) {
    const Self = @This();

    i_float: f64,
    i_bool: bool,
    i_string: []const u8,
    i_void: void,

    identifier: struct { uid: usize, type: Type },

    binary_op: struct {
        kind: BinaryOpKind,
        type: Type,
        left: *Value,
        right: *Value,
    },

    unary_op: struct {
        kind: UnaryOpKind,
        type: Type,
        right: *Value,
    },

    fn_def: void,

    fn_call: struct {
        fn_uid: usize,
        args: std.ArrayList(*Value),
        return_type: Type,
    },

    pub fn init(allocator: std.mem.Allocator, exp: Self) !*Self {
        const ptr = try allocator.create(Self);
        ptr.* = exp;
        return ptr;
    }

    pub fn toType(self: *Self) Type {
        return switch (self) {
            .i_float => .number,
            .i_bool => .bool,
            .i_string => .string,
            .i_void => .void,
            .fn_def => .function,
            .identifier => self.identifier.type,
            .binary_op => self.binary_op.type,
            .unary_op => self.unary_op.type,
            .fn_call => self.fn_call.return_type,
            else => SemaError.InvalidOperation,
        };
    }
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
    cmp_let,
    cmp_get,
    cmp_gt,

    b_and,
    b_or,
};

const std = @import("std");
const sema = @import("sema.zig");
const SemaError = sema.SemaError;
const Type = sema.Type;
