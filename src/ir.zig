pub const Program = struct {
    instructions: InstructionBlock,
    functions: std.ArrayList(Func),

    pub fn deinit(self: *Program) void {
        self.instructions.deinit();
        self.functions.deinit();
    }
};

const Func = struct {
    id: usize,
    name: []const u8,
    args: std.ArrayList(FuncArg),
    return_type: Type,
    body: InstructionBlock,
};

const FuncArg = struct {
    id: usize,
    type: Type,
};

const Instruction = union(enum) {
    store_var: struct {
        id: usize,
        type: Type,
        value: Value,
    },

    update_var: struct {
        id: usize,
        value: Value,
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
        value: *Value,
    },

    expression_stmt: struct {
        value: *Value,
    },
};

const InstructionBlock = std.ArrayList(Instruction);

const Value = union(enum) {
    i_float: f64,
    i_bool: bool,
    i_string: []const u8,
    i_void: void,

    load: struct { id: usize, type: Type },

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

    fn_call: struct {
        id: usize,
        args: std.ArrayList(*Value),
        return_type: Type,
    },
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

    b_and,
    b_or,

    cmp_eq,
    cmp_neq,
    cmp_lt,
    cmp_gt,
};

const std = @import("std");
const sema = @import("sema.zig");
const Type = sema.Type;
