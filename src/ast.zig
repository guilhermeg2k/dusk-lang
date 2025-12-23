pub const Root = Block;

pub const Block = struct {
    statements: std.ArrayList(Statement),
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
    value: *Exp,
};

pub const IfStmt = struct {
    condition: *Exp,
    then_block: Block,
    else_block: ?Block,
};

pub const ForStmt = struct {
    condition: ?*Exp,
    do_block: Block,
};

pub const AssignStmt = struct {
    identifier: []const u8,
    exp: *Exp,
};

pub const TypeAnnotation = struct {
    name: []const u8,
};

pub const Exp = union(enum) {
    const Self = @This();

    number_literal: f64,
    string_literal: []const u8,
    bool_literal: bool,
    identifier: []const u8,
    fn_def: FnDef,
    fn_call: FnCall,

    unary_exp: UnaryExp,
    binary_exp: BinaryExp,

    pub fn init(allocator: std.mem.Allocator, exp: Self) !*Self {
        const expPtr = try allocator.create(Self);
        expPtr.* = exp;
        return expPtr;
    }
};

pub const UnaryExp = struct {
    op: UnaryOp,
    right: *Exp,
};

pub const BinaryExp = struct {
    left: *Exp,
    op: BinaryOp,
    right: *Exp,
};

pub const FnDef = struct {
    arguments: std.ArrayList(FnArg),
    return_type: TypeAnnotation,
    body_block: Block,
};

pub const FnArg = struct {
    identifier: []const u8,
    type_annotation: TypeAnnotation,
    default_value: ?*Exp,
};

pub const FnCall = struct {
    identifier: []const u8,
    arguments: std.ArrayList(*Exp),
};

pub const ReturnStmt = struct {
    exp: ?*Exp,
};

pub const UnaryOp = enum {
    neg,
    not,

    pub fn fromTag(tag: Tag) !UnaryOp {
        return switch (tag) {
            .minus => .neg,
            .not => .not,
            else => ParserError.UnexpectedToken,
        };
    }
};

pub const BinaryOp = enum {
    add,
    sub,
    mult,
    div,
    eq,
    not_eq,
    lt,
    lt_or_eq,
    gt,
    gt_or_eq,
    mod,
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
            .less_than => .lt,
            .less_than_or_equal => .lt_or_eq,
            .greater_than => .gt,
            .greater_than_or_equal => .gt_or_eq,
            .or_kw => .bool_or,
            .and_kw => .bool_and,
            else => ParserError.UnexpectedToken,
        };
    }
};

const AstError = error{ InvalidTag, InvalidToken };

const std = @import("std");
const lex = @import("lexer.zig");
const parser = @import("parser.zig");
const Tag = lex.Tag;
const Token = lex.Token;
const ParserError = parser.ParserError;
