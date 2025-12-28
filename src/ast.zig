pub const Root = Block;

pub const Block = struct {
    statements: std.ArrayList(StatementNode),
};

pub const StatementNode = struct {
    data: Statement,
    loc_start: usize,
};

pub const Statement = union(enum) {
    let_stmt: LetStmt,
    if_stmt: IfStmt,
    for_stmt: ForStmt,
    assign_stmt: AssignStmt,
    fn_call_stmt: FnCall,
    return_stmt: ReturnStmt,
};

pub const LetStmt = struct {
    identifier: []const u8,
    is_mut: bool,
    type_annotation: TypeAnnotation,
    value: *ExpNode,
};

pub const IfStmt = struct {
    condition: *ExpNode,
    then_block: Block,
    else_block: ?Block,
};

pub const ForStmt = struct {
    condition: ?*ExpNode,
    do_block: Block,
};

pub const AssignStmt = struct {
    identifier: []const u8,
    exp: *ExpNode,
};

pub const TypeAnnotation = struct {
    name: []const u8,
};

pub const ExpNode = struct {
    const Self = @This();

    data: Exp,
    loc_start: usize,

    pub fn init(allocator: std.mem.Allocator, exp: Self) !*Self {
        const expPtr = try allocator.create(Self);
        expPtr.* = exp;
        return expPtr;
    }
};

pub const Exp = union(enum) {
    const Self = @This();

    number_literal: f64,
    string_literal: []const u8,
    bool_literal: bool,
    identifier: []const u8,
    array_literal: ArrayLiteral,
    fn_def: FnDef,
    fn_call: FnCall,

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
    arguments: std.ArrayList(FnArg),
    return_type: TypeAnnotation,
    body_block: Block,
};

pub const FnArg = struct {
    identifier: []const u8,
    type_annotation: TypeAnnotation,
    default_value: ?*ExpNode,
};

pub const FnCall = struct {
    identifier: []const u8,
    arguments: std.ArrayList(*ExpNode),
};

pub const ReturnStmt = struct {
    exp: ?*ExpNode,
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
            .percent => .mod,
            .equals => .eq,
            .not_equals => .not_eq,
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
