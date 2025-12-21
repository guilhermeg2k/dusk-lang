pub const Node = union {
    identifier: []const u8,
    number_literal: f64,
    string_literal: []const u8,
    bool_literal: bool,

    root: std.ArrayList(Node),
    let_stmt: LetStmt,
};

pub const LetStmt = struct {
    identifier: []const u8,
    is_mut: bool,
    type_annotation: []const u8,
    value: ?*Node,
};

const std = @import("std");
