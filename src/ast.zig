pub const Root = std.ArrayList(Statement);

pub const Statement = union(enum) {
    let_stmt: LetStmt,
};

pub const LetStmt = struct {
    identifier: []const u8,
    is_mut: bool,
    type_annotation: TypeAnnotation,
    value: *Exp,
};

pub const TypeAnnotation = struct {
    name: []const u8,
};

pub const Exp = union(enum) {
    number_literal: f64,
    string_literal: []const u8,
    bool_literal: bool,
    identifier: []const u8,

    unary: UnaryExp,
    binary: BinaryExp,
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

pub const UnaryOp = enum {
    neg,
    not,

    pub fn fromTag(tag: Tag) !UnaryOp {
        return switch (tag) {
            .minus => .neg,
            .not => .not,
            else => AstError.InvalidTag,
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
            else => AstError.InvalidTag,
        };
    }
};

const AstError = error{InvalidTag};

const std = @import("std");
const lex = @import("lexer.zig");
const Tag = lex.Tag;
