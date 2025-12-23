pub const Lexer = struct {
    const Self = @This();

    const INDENTATION_WIDTH: usize = 4;

    const keywords = std.StaticStringMap(Tag).initComptime(.{
        .{ "let", Tag.let_kw },
        .{ "mut", Tag.mut_kw },
        .{ "number", Tag.number_kw },
        .{ "string", Tag.string_kw },
        .{ "bool", Tag.bool_kw },
        .{ "void", Tag.void_kw },
        .{ "if", Tag.if_kw },
        .{ "else", Tag.else_kw },
        .{ "and", Tag.and_kw },
        .{ "or", Tag.or_kw },
        .{ "for", Tag.for_kw },
        .{ "true", Tag.true_literal },
        .{ "false", Tag.false_literal },
        .{ "fn", Tag.fn_kw },
        .{ "return", Tag.return_kw },
    });

    source: []const u8,
    cur_index: usize = 0,
    last_indentation_level: usize = 0,
    pending_dedents: usize = 0,
    is_first_line: bool = true,

    pub fn init(source: []const u8) Self {
        return Self{ .source = source };
    }

    pub fn next(self: *Self) Token {
        while (true) {
            if (self.is_first_line) {
                const cur_char = self.peekCurrent();
                const is_first_char_white_space = cur_char == ' ' or cur_char == '\t';
                if (is_first_char_white_space) {
                    return Token.init(Tag.err, self.cur_index, self.cur_index);
                }
                self.is_first_line = false;
            }

            const has_pending_dedent = self.pending_dedents != 0;
            if (has_pending_dedent) {
                return self.popPendingDedent();
            }

            const is_eof = self.cur_index == self.source.len;
            if (is_eof and self.last_indentation_level > 0) {
                self.flushIndentation();
                continue;
            }

            if (is_eof) {
                return Token.init(Tag.eof, self.cur_index, self.cur_index);
            }

            defer self.walk();
            self.readWhiteSpaces();
            const cur_char = self.peekCurrent();
            switch (cur_char) {
                '\n' => {
                    const token = self.readNewLine();
                    if (token) |tk| {
                        return tk;
                    }
                    continue;
                },
                '(' => {
                    return Token.init(Tag.l_paren, self.cur_index, self.cur_index);
                },
                ')' => {
                    return Token.init(Tag.r_paren, self.cur_index, self.cur_index);
                },
                ':' => {
                    return Token.init(Tag.colon, self.cur_index, self.cur_index);
                },
                ',' => {
                    return Token.init(Tag.comma, self.cur_index, self.cur_index);
                },
                '+' => {
                    return Token.init(Tag.plus, self.cur_index, self.cur_index);
                },
                '-' => {
                    return self.readComplexSymbol('>', Tag.arrow, Tag.minus);
                },
                '*' => {
                    return Token.init(Tag.star, self.cur_index, self.cur_index);
                },
                '/' => {
                    return Token.init(Tag.slash, self.cur_index, self.cur_index);
                },
                '%' => {
                    return Token.init(Tag.percent, self.cur_index, self.cur_index);
                },
                '=' => {
                    return self.readComplexSymbol('=', Tag.equals, Tag.assign);
                },
                '>' => {
                    return self.readComplexSymbol('=', Tag.greater_than_or_equal, Tag.greater_than);
                },
                '<' => {
                    return self.readComplexSymbol('=', Tag.less_than_or_equal, Tag.less_than);
                },
                '!' => {
                    return self.readComplexSymbol('=', Tag.not_equals, Tag.not);
                },
                'a'...'z', 'A'...'Z', '_' => {
                    return self.readWord();
                },
                '0'...'9' => {
                    return self.readNumberLiteral();
                },
                '\'' => {
                    return self.readStringLiteral();
                },
                else => {
                    return Token.init(Tag.err, self.cur_index, self.cur_index);
                },
            }
        }
    }

    fn readWhiteSpaces(self: *Self) void {
        var cur_char = self.peekCurrent();
        while (cur_char == ' ') : (cur_char = self.peekCurrent()) {
            self.walk();
        }
    }

    fn readNewLine(self: *Self) ?Token {
        const start = self.cur_index;
        var spaces: usize = 0;

        while (self.peekNext() == ' ') {
            spaces += 1;
            self.walk();
        }

        if (spaces % INDENTATION_WIDTH != 0) {
            return Token.init(Tag.err, start, self.cur_index);
        }

        if (self.peekNext() == '\n') {
            if (spaces > 0) {
                return Token.init(Tag.err, start, self.cur_index);
            }
            return null;
        }

        const indentation_level = spaces / INDENTATION_WIDTH;
        const is_isvalid_indentation = indentation_level > 1 and (indentation_level - self.last_indentation_level) > 1;

        if (is_isvalid_indentation) {
            return Token.init(Tag.err, start, self.cur_index);
        }

        if (indentation_level > self.last_indentation_level) {
            self.last_indentation_level = indentation_level;
            return Token.init(Tag.indent, start, self.cur_index);
        }

        if (indentation_level < self.last_indentation_level) {
            self.pending_dedents = self.last_indentation_level - indentation_level;
            self.last_indentation_level = indentation_level;
            return null;
        }

        return Token.init(Tag.new_line, start, self.cur_index);
    }

    fn readExclamationMarkSymbol(self: *Self) Token {
        return self.readComplexSymbol('=', Tag.not_equals, Tag.err);
    }

    fn readMinusSymbol(self: *Self) Token {
        return self.readComplexSymbol('>', Tag.arrow, Tag.minus);
    }

    fn readGreaterSymbol(self: *Self) Token {
        return self.readComplexSymbol('=', Tag.greater_than_or_equal, Tag.greater_than);
    }

    fn readLessThanSymbol(self: *Self) Token {
        const next_char = self.peekNext();

        if (next_char == '=') {
            const start = self.cur_index;
            self.walk();
            return Token.init(Tag.less_than_or_equal, start, self.cur_index);
        }

        return Token.init(Tag.less_than, self.cur_index, self.cur_index);
    }

    fn readEqualsSymbol(self: *Self) Token {
        const next_char = self.peekNext();

        if (next_char == '=') {
            const start = self.cur_index;
            self.walk();
            return Token.init(Tag.equals, start, self.cur_index);
        }

        return Token.init(Tag.assign, self.cur_index, self.cur_index);
    }

    fn readNumberLiteral(self: *Self) Token {
        const start = self.cur_index;
        var next_char = self.peekNext();

        while (std.ascii.isDigit(next_char) or next_char == '.') {
            self.walk();
            next_char = self.peekNext();
        }

        return Token.init(Tag.number_literal, start, self.cur_index);
    }

    fn readStringLiteral(self: *Self) Token {
        const start = self.cur_index;

        while (self.peekNext() != '\'') {
            self.walk();
        }

        self.walk();
        return Token.init(Tag.string_literal, start, self.cur_index);
    }

    fn readWord(self: *Self) Token {
        const start = self.cur_index;

        var next_char = self.peekNext();
        while (std.ascii.isAlphanumeric(next_char) or next_char == '_') {
            self.walk();
            next_char = self.peekNext();
        }

        const word = self.source[start .. self.cur_index + 1];

        if (keywords.get(word)) |tag| {
            return Token.init(tag, start, self.cur_index);
        }

        return Token.init(Tag.identifier, start, self.cur_index);
    }

    fn readComplexSymbol(self: *Self, expected_next: u8, return_tag: Tag, fallback_tag: Tag) Token {
        if (self.peekNext() == expected_next) {
            const start = self.cur_index;
            self.walk();
            return Token.init(return_tag, start, self.cur_index);
        }
        return Token.init(fallback_tag, self.cur_index, self.cur_index);
    }

    fn popPendingDedent(self: *Self) Token {
        self.pending_dedents -= 1;
        return Token.init(Tag.dedent, self.cur_index, self.cur_index);
    }

    fn flushIndentation(self: *Self) void {
        self.pending_dedents = self.last_indentation_level;
        self.last_indentation_level = 0;
    }

    fn walk(self: *Self) void {
        self.cur_index += 1;
    }

    fn peekCurrent(self: *Self) u8 {
        if (self.cur_index >= self.source.len) {
            return 0;
        }
        return self.source[self.cur_index];
    }

    fn peekNext(self: *Self) u8 {
        if (self.cur_index + 1 >= self.source.len) {
            return 0;
        }
        return self.source[self.cur_index + 1];
    }
};

pub const Token = struct {
    const Self = @This();
    tag: Tag,
    loc: Loc,

    pub fn value(self: Token, source: []const u8) []const u8 {
        if (self.tag == Tag.eof) {
            return "EOF";
        }

        if (self.tag == Tag.new_line) {
            return "NL";
        }

        if (self.tag == Tag.dedent) {
            return "DEDENT";
        }

        if (self.tag == Tag.indent) {
            return "INDENT";
        }

        return source[self.loc.start .. self.loc.end + 1];
    }

    pub fn init(tag: Tag, start: usize, end: usize) Self {
        return Self{
            .tag = tag,
            .loc = .{ .start = start, .end = end },
        };
    }
};

pub const Tag = enum {
    identifier,
    let_kw,
    mut_kw,
    string_kw,
    number_kw,
    void_kw,
    bool_kw,
    if_kw,
    else_kw,
    for_kw,
    and_kw,
    or_kw,
    fn_kw,
    return_kw,
    string_literal,
    number_literal,
    true_literal,
    false_literal,
    equals,
    not,
    not_equals,
    colon,
    comma,
    assign,
    less_than,
    less_than_or_equal,
    greater_than,
    greater_than_or_equal,
    l_paren,
    r_paren,
    arrow,
    plus,
    minus,
    star,
    slash,
    percent,
    indent,
    dedent,
    new_line,
    eof,
    err,
};

const Loc = struct {
    start: usize,
    end: usize,
};

pub fn main() void {
    const src =
        \\if x:
        \\    return
    ;
    var lexer = Lexer.init(src);

    var tk = lexer.next();

    while (tk.tag != Tag.eof and tk.tag != Tag.err) {
        std.debug.print("tk {any} = {s}\n", .{ tk.tag, tk.value(src) });
        tk = lexer.next();
    }

    std.debug.print("tk {any} = {s}\n", .{ tk.tag, tk.value(src) });
}

fn expectTags(source: []const u8, expected: []const Tag) !void {
    var lex = Lexer.init(source);
    for (expected) |expected_tag| {
        const token = lex.next();
        try testing.expectEqual(expected_tag, token.tag);
    }
}

test "Keywords" {
    const src = "let mut if else number string bool return void and or for";
    try expectTags(src, &.{ Tag.let_kw, Tag.mut_kw, Tag.if_kw, Tag.else_kw, Tag.number_kw, Tag.string_kw, Tag.bool_kw, Tag.return_kw, Tag.void_kw, Tag.and_kw, Tag.or_kw, Tag.for_kw, Tag.eof });
}

test "Symbols " {
    const src = "():,+-*/ = > < >= <= != -> % !";
    try expectTags(src, &.{ Tag.l_paren, Tag.r_paren, Tag.colon, Tag.comma, Tag.plus, Tag.minus, Tag.star, Tag.slash, Tag.assign, Tag.greater_than, Tag.less_than, Tag.greater_than_or_equal, Tag.less_than_or_equal, Tag.not_equals, Tag.arrow, Tag.percent, Tag.not, Tag.eof });
}

test "String Literals" {
    const src = "'hello world' 'foo'";
    try expectTags(src, &.{ Tag.string_literal, Tag.string_literal, Tag.eof });
}

test "Number Literals" {
    const src = "3333.33 33333";
    try expectTags(src, &.{ Tag.number_literal, Tag.number_literal, Tag.eof });
}

test "Bool Literals" {
    const src = "true false";
    try expectTags(src, &.{ Tag.true_literal, Tag.false_literal, Tag.eof });
}

test "Basic Indentation" {
    const src =
        \\if x:
        \\    return
    ;

    try expectTags(src, &.{
        Tag.if_kw,  Tag.identifier, Tag.colon,
        Tag.indent, Tag.return_kw,
    });
}

test "Double Dedent" {
    const src =
        \\if x:
        \\    if y:
        \\        z
        \\x
    ;

    try expectTags(src, &.{
        Tag.if_kw,  Tag.identifier, Tag.colon,
        Tag.indent, Tag.if_kw,      Tag.identifier,
        Tag.colon,  Tag.indent,     Tag.identifier,
        Tag.dedent, Tag.dedent,     Tag.identifier,
        Tag.eof,
    });
}

test "Flush dedent before eof" {
    const src =
        \\if x:
        \\    if y:
        \\        z
    ;

    try expectTags(src, &.{
        Tag.if_kw,  Tag.identifier, Tag.colon,
        Tag.indent, Tag.if_kw,      Tag.identifier,
        Tag.colon,  Tag.indent,     Tag.identifier,
        Tag.dedent, Tag.dedent,     Tag.eof,
    });
}

test "Not allow invalid indent" {
    const src =
        \\if x:
        \\        if y:
        \\        z
    ;

    try expectTags(src, &.{
        Tag.if_kw, Tag.identifier, Tag.colon,
        Tag.err,
    });
}

test "Empty Lines Should Be Ignored" {
    const src =
        \\let x = 1
        \\
        \\let y = 2
    ;

    try expectTags(src, &.{
        Tag.let_kw, Tag.identifier, Tag.assign, Tag.number_literal, Tag.new_line,
        Tag.let_kw, Tag.identifier, Tag.assign, Tag.number_literal, Tag.eof,
    });
}

const std = @import("std");
const testing = std.testing;
