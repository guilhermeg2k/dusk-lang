pub const Root = Block;

//note: arraylist
pub const Block = struct {
    statements: std.ArrayList(StatementNode),
};

pub const StatementNode = struct {
    data: Statement,
    loc: err.Loc,
};

pub const Statement = union(enum) {
    import: ImportStmt,
    let: LetStmt,
    func: FuncStmt,
    @"struct": StructStmt,
    @"enum": EnumStmt,
    @"union": UnionStmt,
    error_set: ErrorSetStmt,
    trait: TraitStmt,
    @"for": ForStmt,
    for_capture: ForCaptureStmt,
    assign: AssignStmt,
    @"return": ReturnStmt,
    expression: *ExpNode,
    @"break": void,
    @"continue": void,
};

pub const ImportStmt = struct {
    source: ImportSource,
    path: []const u8,
    path_loc: err.Loc,
    name: []const u8,
    selected: []const ImportedName,
};

pub const ImportSource = enum {
    std,
    library,
    project,
};

pub const ImportedName = struct {
    identifier: []const u8,
    name: []const u8,
    loc: err.Loc,
};

pub const FuncStmt = struct {
    identifier: []const u8,
    def: FnDef,
    is_extern: bool = false,
    is_file_only: bool = false,
    slots: []const Slot = &.{},
};

pub const Slot = struct {
    identifier: []const u8,
    constraint: []const []const *TypeAnnotation = &.{},
    is_error_set: bool = false,
    loc: err.Loc,
};

pub const StructStmt = struct {
    identifier: []const u8,
    def: Struct,
    is_file_only: bool = false,
    slots: []const Slot = &.{},
};

pub const EnumStmt = struct {
    identifier: []const u8,
    def: EnumDef,
    is_file_only: bool = false,
};

pub const UnionStmt = struct {
    identifier: []const u8,
    def: UnionDef,
    is_file_only: bool = false,
    slots: []const Slot = &.{},
};

pub const ErrorSetStmt = struct {
    identifier: []const u8,
    variants: []const ErrorVariant,
    is_file_only: bool = false,
};

pub const TraitStmt = struct {
    identifier: []const u8,
    is_file_only: bool = false,
    type_list: []const *TypeAnnotation = &.{},
    parents: []const *TypeAnnotation = &.{},
    methods: []const StatementNode = &.{},
};

pub const ErrorVariant = struct {
    identifier: []const u8,
    loc: err.Loc,
};

pub const LetStmt = struct {
    identifier: []const u8,
    is_mut: bool,
    is_file_only: bool = false,
    type_annotation: ?*TypeAnnotation,
    value: *ExpNode,
};

pub const IfStmt = struct {
    condition: *ExpNode,
    then_block: Block,
    else_block: ?Block,
};

pub const IfCaptureStmt = struct {
    exp: *ExpNode,
    identifier: struct { name: []const u8, is_mut: bool },
    body: Block,
    else_block: ?Block,
};

pub const WhenStmt = struct {
    subject: *ExpNode,
    prongs: []const WhenProng,
    else_block: ?Block,
    else_loc: ?err.Loc,
};

pub const WhenProng = struct {
    pattern: union(enum) {
        values: []const *ExpNode,
        variant: struct { name: []const u8, capture: []const u8, is_mut: bool },
        null_case: void,
    },
    body: Block,
    loc: err.Loc,
};

pub const ForStmt = struct {
    condition: *ExpNode,
    do_block: Block,
};

pub const ForCaptureStmt = struct {
    source: *ExpNode,
    item: []const u8,
    is_item_mut: bool,
    index: ?[]const u8,
    do_block: Block,
};

pub const AssignStmt = struct {
    target: *ExpNode,
    exp: *ExpNode,
};

pub const TypeAnnotation = struct {
    const Self = @This();

    type: union(enum) {
        primitive: []const u8,
        @"struct": []const u8,
        anonymous_struct: Struct,
        array: *TypeAnnotation,
        type_self: void,
        function: FuncTypeAnnotation,
        resolved: u64,
    },

    is_nullable: bool,
    args: []const *TypeAnnotation = &.{},
    loc: err.Loc = .{ .start = 0, .end = 0 },

    pub fn init(allocator: std.mem.Allocator, exp: Self) !*Self {
        const ptr = try allocator.create(Self);
        ptr.* = exp;
        return ptr;
    }

    pub fn value(self: *Self, alloc: std.mem.Allocator) ![]const u8 {
        return switch (self.type) {
            .primitive => |primitive_name| primitive_name,
            .@"struct" => |struct_name| struct_name,
            .anonymous_struct => |anom_struct| {
                var field_list: std.ArrayList(u8) = .empty;
                for (anom_struct.fields, 0..) |field, i| {
                    if (i > 0) {
                        try field_list.appendSlice(alloc, ", ");
                    }
                    try field_list.appendSlice(alloc, field.identifier);
                }
                return std.fmt.allocPrint(alloc, "anonymous struct {{ {s} }}", .{field_list.items});
            },
            .array => {
                return std.fmt.allocPrint(alloc, "[]{s}", .{try self.type.array.value(alloc)});
            },
            .type_self => {
                return "@";
            },
            .resolved => {
                return "inferred type";
            },
            .function => |func| {
                var text: std.ArrayList(u8) = .empty;
                try text.appendSlice(alloc, "func(");
                for (func.params, 0..) |param, i| {
                    if (i > 0) {
                        try text.appendSlice(alloc, ", ");
                    }
                    if (param.is_mut) {
                        try text.appendSlice(alloc, "mut ");
                    }
                    try text.appendSlice(alloc, try param.type.value(alloc));
                }
                try text.appendSlice(alloc, ") -> ");
                if (func.error_set) |set| {
                    try text.print(alloc, "{s}!", .{set});
                }
                try text.appendSlice(alloc, try func.return_type.value(alloc));
                return text.toOwnedSlice(alloc);
            },
        };
    }
};

pub const FuncTypeAnnotation = struct {
    params: []const FuncTypeParam,
    return_type: *TypeAnnotation,
    error_set: ?[]const u8,
};

pub const FuncTypeParam = struct {
    type: *TypeAnnotation,
    is_mut: bool,
};

pub const ExpNode = struct {
    const Self = @This();

    data: Exp,
    loc: err.Loc,

    pub fn init(allocator: std.mem.Allocator, exp: Self) !*Self {
        const ptr = try allocator.create(Self);
        ptr.* = exp;
        return ptr;
    }
};

pub const Exp = union(enum) {
    const Self = @This();

    float_literal: f64,
    int_literal: i64,
    byte_literal: u8,

    string_literal: []const u8,
    string_interpolation: []const *ExpNode,
    bool_literal: bool,
    identifier: []const u8,
    anonymous_init: AnonymousInit,
    array_literal: ArrayLiteral,
    null_literal: void,
    fn_call: FnCall,
    indexed: IndexedExp,
    unary_exp: UnaryExp,
    binary_exp: BinaryExp,
    try_exp: TryExp,
    catch_exp: CatchExp,
    orelse_exp: OrelseExp,
    if_exp: IfStmt,
    if_capture_exp: IfCaptureStmt,
    when_exp: WhenStmt,
    range: Range,
    lambda: FnDef,
};

pub const Range = struct {
    start: *ExpNode,
    end: *ExpNode,
};

pub const TryExp = struct {
    operand: *ExpNode,
};

pub const OrelseExp = struct {
    operand: *ExpNode,
    fallback: *ExpNode,
};

pub const CatchExp = struct {
    operand: *ExpNode,
    fallback: union(enum) {
        value: *ExpNode,
        block: struct { capture: []const u8, body: Block },
    },
};

pub const ArrayLiteral = struct {
    exps: []const *ExpNode,
};

pub const UnaryExp = struct {
    op: UnaryOp,
    right: *ExpNode,
};

pub const BinaryExp = struct {
    left: *ExpNode,
    op: BinaryOp,
    right: *ExpNode,
};

pub const FnDef = struct {
    params: []const FnParam,
    return_type: ?*TypeAnnotation,
    error_set: ?FnErrorSet = null,
    body_block: Block,
};

pub const FnErrorSet = union(enum) {
    inferred: err.Loc,
    named: struct { identifier: []const u8, loc: err.Loc },
};

pub const FnParam = struct {
    identifier: []const u8,
    type_annotation: ?*TypeAnnotation,
    default_value: ?*ExpNode,
    is_mut: bool,
};

pub const FnCall = struct {
    target: *ExpNode,
    are_arguments_named: bool,
    arguments: []const FnCallArg,
};

pub const AnonymousInit = struct {
    are_arguments_named: bool,
    arguments: []const FnCallArg,
};

pub const FnCallArg = struct {
    identifier: ?[]const u8,
    exp: *ExpNode,
};

pub const ReturnStmt = struct {
    exp: ?*ExpNode,
};

pub const IndexedExp = struct {
    target: *ExpNode,
    index: *ExpNode,
    is_nullable: bool,
};

pub const Struct = struct {
    fields: []const Field,
    static_fields: []const Field,
    funcs: []const StatementNode,
};

pub const Field = struct {
    identifier: []const u8,
    is_mut: bool,
    is_file_only: bool = false,
    type: ?*TypeAnnotation,
    default_value: ?*ExpNode,
};

pub const EnumDef = struct {
    variants: []const EnumVariant,
    static_fields: []const Field,
    funcs: []const StatementNode,
};

pub const EnumVariant = struct {
    identifier: []const u8,
    value: ?i64,
};

pub const UnionDef = struct {
    variants: []const UnionVariant,
    static_fields: []const Field,
    funcs: []const StatementNode,
};

pub const UnionVariant = struct {
    identifier: []const u8,
    type: *TypeAnnotation,
};

pub const UnaryOp = enum {
    neg,
    not,

    pub fn fromTag(tag: Tag) !UnaryOp {
        return switch (tag) {
            .minus => .neg,
            .not => .not,
            else => err.CompileError.ParserError,
        };
    }
};

pub const BinaryOp = enum {
    add,
    sub,
    mult,
    div,
    trunc_div,
    mod,

    eq,
    not_eq,
    lt,
    lt_or_eq,
    gt,
    gt_or_eq,

    bool_or,
    bool_and,

    pub fn fromTag(tag: Tag) !BinaryOp {
        return switch (tag) {
            .plus => .add,
            .minus => .sub,
            .star => .mult,
            .slash => .div,
            .double_slash => .trunc_div,
            .percent => .mod,
            .double_eq => .eq,
            .not_eq => .not_eq,
            .lt => .lt,
            .le => .lt_or_eq,
            .gt => .gt,
            .ge => .gt_or_eq,
            .or_kw => .bool_or,
            .and_kw => .bool_and,
            else => err.CompileError.ParserError,
        };
    }
};

const Tag = lex.Tag;

const lex = @import("lexer.zig");
const err = @import("compile_error.zig");
const std = @import("std");
