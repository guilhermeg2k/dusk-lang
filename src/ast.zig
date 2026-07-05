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
    let: LetStmt,
    func: FuncStmt,
    @"struct": StructStmt,
    @"enum": EnumStmt,
    @"if": IfStmt,
    if_capture: IfCaptureStmt,
    @"for": ForStmt,
    assign: AssignStmt,
    @"return": ReturnStmt,
    expression: *ExpNode,
    @"break": void,
    @"continue": void,
};

pub const FuncStmt = struct {
    identifier: []const u8,
    def: FnDef,
};

pub const StructStmt = struct {
    identifier: []const u8,
    def: Struct,
};

pub const EnumStmt = struct {
    identifier: []const u8,
    def: EnumDef,
};

pub const LetStmt = struct {
    identifier: []const u8,
    is_mut: bool,
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

pub const ForStmt = struct {
    condition: *ExpNode,
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
    },

    nullable: bool,

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
                    if (i > 0) try field_list.appendSlice(alloc, ", ");
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
        };
    }
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
    string_literal: []const u8,
    bool_literal: bool,
    identifier: []const u8,
    anonymous_struct_identifier: void,
    array_literal: ArrayLiteral,
    null_literal: void,
    fn_call: FnCall,
    indexed: IndexedExp,
    unary_exp: UnaryExp,
    binary_exp: BinaryExp,
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
    body_block: Block,
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
    nullable: bool,
};

pub const Struct = struct {
    fields: []const StructField,
    static_fields: []const StructField,
    funcs: []const StatementNode,
};

pub const StructField = struct {
    identifier: []const u8,
    is_mut: bool,
    type: ?*TypeAnnotation,
    default_value: ?*ExpNode,
};

pub const EnumDef = struct {
    variants: []const EnumVariant,
    funcs: []const StatementNode,
};

pub const EnumVariant = struct {
    identifier: []const u8,
    value: ?i64,
};

pub const UnaryOp = enum {
    neg,
    not,

    pub fn fromTag(tag: Tag) !UnaryOp {
        return switch (tag) {
            .minus => .neg,
            .not => .not,
            else => err.Errors.ParserError,
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
            else => err.Errors.ParserError,
        };
    }
};

const Tag = lex.Tag;
const Token = lex.Token;

const lex = @import("lexer.zig");
const parser = @import("parser.zig");
const err = @import("error.zig");
const std = @import("std");
