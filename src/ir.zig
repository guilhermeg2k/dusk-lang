const std = @import("std");
const TypeId = @import("type_table.zig").TypeId;

pub const Program = struct {
    instructions: []const Instruction,
    functions: []const Func,
    defined_types: []const DefinedType,
};

pub const Block = struct {
    return_type: TypeId,
    instructions: []const Instruction,
};

pub const Instruction = union(enum) {
    store_var: StoreVar,
    branch_if: BranchIf,
    loop: Loop,
    return_stmt: ReturnStmt,
    expression_stmt: ExpressionStmt,
    update_indexed: UpdateIndexed,
    break_stmt: void,
    continue_stmt: void,
};

pub const DefinedType = struct {
    kind: union(enum) {
        @"struct": Struct,
        @"enum": Enum,
        @"union": Union,
    },
};

pub const Struct = struct {
    uid: usize,
    type_id: TypeId,
    identifier: []const u8,
    fields: []const Field,
    static_fields: []const Field,
    funcs: []const Func,
};

pub const Enum = struct {
    uid: usize,
    type_id: TypeId,
    identifier: []const u8,

    variants: std.StringHashMap(i64),
    static_fields: []const Field,
    funcs: []const Func,
};

pub const Union = struct {
    uid: usize,
    type_id: TypeId,
    identifier: []const u8,

    variants: std.StringHashMap(TypeId),
    static_fields: []const Field,
    funcs: []const Func,
};

pub const Field = struct {
    identifier: []const u8,
    type_id: TypeId,
    default_value: ?*Value,
};

pub const Func = struct {
    uid: usize,
    identifier: []const u8,
    params: []const FuncParam,
    body: []const Instruction,
    return_type: TypeId,
};

pub const FuncParam = struct {
    uid: usize,
    identifier: []const u8,
    default_value: ?*Value,
    type_id: TypeId,
};

pub const UpdateIndexed = struct {
    target: *Value,
    value: *Value,
};

pub const StructInit = struct {
    keys: [][]const u8,
    values: []*Value,
};

pub const UnionInit = struct {
    tag: ?[]const u8,
    value: ?*Value,
};

pub const StoreVar = struct {
    uid: usize,
    identifier: []const u8,
    type_id: TypeId,
    value: *Value,
};

pub const UpdateVar = struct {
    var_uid: usize,
    identifier: []const u8,
    value: *Value,
};

pub const BranchIf = struct {
    condition: *Value,
    then_block: []const Instruction,
    else_block: []const Instruction,
};

pub const Loop = struct {
    condition: ?*Value,
    do_block: []const Instruction,
};

pub const ReturnStmt = struct {
    value: ?*Value,
};

pub const ExpressionStmt = struct {
    value: *Value,
};

pub const Value = struct {
    const Self = @This();
    data: union(enum) {
        i_float: f64,
        i_int: i64,
        i_bool: bool,
        i_string: []const u8,
        i_void: void,
        i_null: void,
        i_array: Array,

        identifier: struct { uid: usize, value: []const u8 },
        indexed: IndexedValue,

        binary_op: BinaryOp,
        unary_op: UnaryOp,

        fn_call: FnCall,
        struct_init: StructInit,
        union_init: UnionInit,
    },

    type_id: TypeId,

    pub fn init(allocator: std.mem.Allocator, value: Value) !*Value {
        const ptr = try allocator.create(Value);
        ptr.* = value;
        return ptr;
    }
};

pub const Array = struct { values: []const *Value };

pub const BinaryOp = struct {
    kind: BinaryOpKind,
    left: *Value,
    right: *Value,
};

pub const IndexedValue = struct {
    target: *Value,
    index: *Value,
};

pub const UnaryOp = struct {
    kind: UnaryOpKind,
    right: *Value,
};

pub const FnCall = struct {
    fn_uid: usize,
    identifier: []const u8,
    args: []const *Value,
};

pub const UnaryOpKind = enum {
    i_neg,
    f_neg,
    not,
};

pub const BinaryOpKind = enum {
    i_add,
    i_sub,
    i_mult,
    trunc_div,
    i_mod,

    i_cmp_eq,
    i_cmp_neq,

    i_cmp_lt,
    i_cmp_le,
    i_cmp_ge,
    i_cmp_gt,

    f_add,
    f_sub,
    f_mult,
    f_div,
    f_mod,

    f_cmp_eq,
    f_cmp_neq,

    f_cmp_lt,
    f_cmp_le,
    f_cmp_ge,
    f_cmp_gt,

    b_and,
    b_or,
    b_cmp_eq,
    b_cmp_neq,
};
