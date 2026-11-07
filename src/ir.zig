const std = @import("std");
const TypeId = @import("type_table.zig").TypeId;
const TypedIdentifier = @import("type_table.zig").TypedIdentifier;

pub const Program = struct {
    instructions: Block,
    externs: []const Extern,
    functions: []const Func,
    defined_types: []const DefinedType,
    error_names: []const []const u8,
};

pub const Block = []const Instruction;

pub const Instruction = union(enum) {
    store_var: StoreVar,
    branch_if: BranchIf,
    match: Match,
    block: Block,
    loop: Loop,
    return_stmt: ReturnStmt,
    return_error: ReturnError,
    expression_stmt: ExpressionStmt,
    update_indexed: UpdateIndexed,
    unwrap_null_box: UnwrapNullBox,
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
    funcs: []const Func,
};

pub const Enum = struct {
    uid: usize,
    type_id: TypeId,
    identifier: []const u8,

    variants: std.StringHashMap(i64),
    funcs: []const Func,
};

pub const Union = struct {
    uid: usize,
    type_id: TypeId,
    identifier: []const u8,

    variants: std.StringHashMap(TypeId),
    funcs: []const Func,
};

pub const Field = struct {
    identifier: []const u8,
    type_id: TypeId,
    default_value: ?*Value,
};

pub const Extern = struct {
    uid: usize,
    identifier: []const u8,
    params: []const TypedIdentifier,
};

pub const Func = struct {
    uid: usize,
    identifier: []const u8,
    params: []const FuncParam,
    body: Block,
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

pub const NullBoxInit = struct {
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
    then_block: Block,
    else_block: Block,
};

pub const Match = struct {
    null_check: ?*Value = null,
    null_body: ?Block = null,
    cases: []const MatchCase,
    table: ?JumpTable = null,
    default_block: Block,
};

pub const MatchCase = struct {
    condition: *Value,
    body: Block,
};

pub const JumpTable = struct {
    subject: *Value,
    min: i64,
    case_by_offset: []const ?usize,
};

pub const Loop = struct {
    advancement: Block = &.{},
    condition: ?*Value,
    do_block: Block,
};

pub const ReturnStmt = struct {
    value: ?*Value,
};

pub const ReturnError = struct {
    value: *Value,
};

pub const TryCall = struct {
    call: *Value,
    is_top_level: bool,
};

pub const CatchCall = struct {
    call: *Value,
    fallback: union(enum) {
        value: *Value,
        block: CatchBlock,
    },
};

pub const CatchBlock = struct {
    capture_uid: usize,
    capture_identifier: []const u8,
    capture_type_id: TypeId,
    instructions: Block,
};

pub const BlockValue = struct {
    instructions: Block,
    result_uid: usize,
    can_complete: bool,
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
        string_interpolation: []const *Value,
        i_void: void,
        i_null: void,
        i_array: Array,

        identifier: struct { uid: usize, name: []const u8 },
        indexed: IndexedValue,

        binary_op: BinaryOp,
        unary_op: UnaryOp,

        fn_call: FnCall,
        struct_init: StructInit,
        union_init: UnionInit,
        null_box_init: NullBoxInit,
        union_tag: *Value,
        union_payload: *Value,
        try_call: TryCall,
        catch_call: CatchCall,
        block_value: BlockValue,
        int_to_float: *Value,
        array_length: *Value,

        bytes_length: *Value,
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

pub const NullBoxNNullOp = struct {
    value: *Value,
};

pub const UnwrapNullBox = struct {
    unwrapped: struct { uid: usize, identifier: []const u8, type_id: TypeId },
    null_box: *Value,
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
    box_not_null,
    i_neg,
    f_neg,
    not,
    to_byte,
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
