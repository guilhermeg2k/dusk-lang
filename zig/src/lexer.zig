const Tag = enum {
    let,
    mut,
    kw_string,
    kw_number,
    kw_void,
    kw_bool,
    identifier,
    string_literal,
    number_literal,
    true_lit,
    false_lit,
    if_kw,
    else_kw,
    for_kw,
    and_kw,
    or_kw,
    return_kw,
    equals,
    not_equals,
    colon,
    comma,
    assign,
    less_than,
    greater_than,
    greater_or_equal,
    less_or_equal,
    l_paren,
    r_paren,
    arrow,
    plus,
    minus,
    star,
    slash,
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

const Token = struct {
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

const keywords = std.StaticStringMap(Tag).initComptime(.{
    .{ "let", Tag.let },
    .{ "mut", Tag.mut },
    .{ "if", Tag.if_kw },
    .{ "else", Tag.else_kw },
    .{ "return", Tag.return_kw },
});

const INDENTATION_WIDTH = 4;

pub const Lexer = struct {
    const Self = @This();

    source: []const u8,
    cur_index: usize = 0,
    last_indentation_level: usize = 0,
    pending_dedents: usize = 0,

    pub fn init(source: []const u8) Self {
        return Self{ .source = source };
    }

    pub fn popPendingDedent(self: *Self) Token {
        self.pending_dedents -= 1;
        return Token.init(Tag.dedent, self.cur_index, self.cur_index);
    }

    pub fn flushIndentation(self: *Self) void {
        self.pending_dedents = self.last_indentation_level;
        self.last_indentation_level = 0;
    }

    pub fn next(self: *Self) Token {
        while (true) {
            const should_pop_dedent = self.pending_dedents != 0;
            if (should_pop_dedent) {
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
            const cur_char = self.source[self.cur_index];
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
                    return self.readMinusSymbol();
                },
                '*' => {
                    return Token.init(Tag.star, self.cur_index, self.cur_index);
                },
                '/' => {
                    return Token.init(Tag.slash, self.cur_index, self.cur_index);
                },
                '=' => {
                    return self.readEqualsSymbol();
                },
                '>' => {
                    return self.readGreaterSymbol();
                },
                '<' => {
                    return self.readLessThanSymbol();
                },
                '!' => {
                    return self.readExclamationMarkSymbol();
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

    fn readExclamationMarkSymbol(self: *Self) Token {
        if (self.peek_next() == '=') {
            const start = self.cur_index;
            self.walk();
            return Token.init(Tag.not_equals, start, self.cur_index);
        }

        return Token.init(Tag.err, self.cur_index, self.cur_index);
    }

    fn readMinusSymbol(self: *Self) Token {
        const next_char = self.peek_next();

        if (next_char == '>') {
            const start = self.cur_index;
            self.walk();
            return Token.init(Tag.arrow, start, self.cur_index);
        }

        return Token.init(Tag.minus, self.cur_index, self.cur_index);
    }

    fn readGreaterSymbol(self: *Self) Token {
        const next_char = self.peek_next();

        if (next_char == '=') {
            const start = self.cur_index;
            self.walk();
            return Token.init(Tag.greater_or_equal, start, self.cur_index);
        }

        return Token.init(Tag.greater_than, self.cur_index, self.cur_index);
    }

    fn readLessThanSymbol(self: *Self) Token {
        const next_char = self.peek_next();

        if (next_char == '=') {
            const start = self.cur_index;
            self.walk();
            return Token.init(Tag.less_or_equal, start, self.cur_index);
        }

        return Token.init(Tag.less_than, self.cur_index, self.cur_index);
    }

    fn readEqualsSymbol(self: *Self) Token {
        const next_char = self.peek_next();

        if (next_char == '=') {
            const start = self.cur_index;
            self.walk();
            return Token.init(Tag.equals, start, self.cur_index);
        }

        return Token.init(Tag.assign, self.cur_index, self.cur_index);
    }

    fn readWhiteSpaces(self: *Self) void {
        while (self.cur_index < self.source.len and self.source[self.cur_index] == ' ') {
            self.walk();
        }
    }

    fn readNumberLiteral(self: *Self) Token {
        const start = self.cur_index;

        while (self.cur_index < self.source.len) {
            const next_char = self.peek_next();
            if (std.ascii.isDigit(next_char) or next_char == '.') {
                self.walk();
            } else {
                break;
            }
        }

        return Token.init(Tag.number_literal, start, self.cur_index);
    }

    fn readStringLiteral(self: *Self) Token {
        const start = self.cur_index;

        while (self.peek_next() != '\'') {
            self.walk();
        }

        self.walk();
        return Token.init(Tag.string_literal, start, self.cur_index);
    }

    fn readWord(self: *Self) Token {
        const start = self.cur_index;

        while (self.cur_index < self.source.len) {
            const next_char = self.peek_next();
            if (std.ascii.isAlphanumeric(next_char) or next_char == '_') {
                self.walk();
            } else {
                break;
            }
        }

        const word = self.source[start .. self.cur_index + 1];
        if (keywords.get(word)) |tag| {
            return Token.init(tag, start, self.cur_index);
        }

        return Token.init(Tag.identifier, start, self.cur_index);
    }

    fn readNewLine(self: *Self) ?Token {
        defer self.walk();
        const start = self.cur_index;
        var spaces: usize = 0;

        while (self.peek_next() == ' ') {
            spaces += 1;
            self.walk();
        }

        if (spaces % INDENTATION_WIDTH != 0) {
            return Token.init(Tag.err, start, self.cur_index);
        }

        if (self.peek_next() == '\n') {
            if (spaces > 0) {
                return Token.init(Tag.err, start, self.cur_index);
            } else {
                return null;
            }
        }

        const indentation_level = spaces / INDENTATION_WIDTH;

        if (indentation_level > self.last_indentation_level) {
            if (indentation_level - 1 > self.last_indentation_level) {
                return Token.init(Tag.err, start, self.cur_index);
            }
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

    fn walk(self: *Self) void {
        self.cur_index += 1;
    }

    fn peek_next(self: *Self) u8 {
        if (self.cur_index + 1 == self.source.len) {
            return 0;
        }

        return self.source[self.cur_index + 1];
    }
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
    const src = "let mut if else return";
    try expectTags(src, &.{ .let, .mut, .if_kw, .else_kw, .return_kw, .eof });
}

test "Symbols " {
    const src = "():,+-*/=>< >= <= != ->";
    try expectTags(src, &.{ .l_paren, .r_paren, .colon, .comma, .plus, .minus, .star, .slash, .assign, .greater_than, .less_than, .greater_or_equal, .less_or_equal, .not_equals, .arrow, .eof });
}

test "String Literals" {
    const src = "'hello world' 'foo'";
    try expectTags(src, &.{ .string_literal, .string_literal, .eof });
}

test "Number Literals" {
    const src = "3333.33 33333";
    try expectTags(src, &.{ .number_literal, .number_literal, .eof });
}

test "Basic Indentation" {
    const src =
        \\if x:
        \\    return
    ;

    try expectTags(src, &.{
        .if_kw,  .identifier, .colon,
        .indent, .return_kw,
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
        .if_kw,  .identifier, .colon,
        .indent, .if_kw,      .identifier,
        .colon,  .indent,     .identifier,
        .dedent, .dedent,     .identifier,
        .eof,
    });
}

test "Flush dedent before eof" {
    const src =
        \\if x:
        \\    if y:
        \\        z
    ;

    try expectTags(src, &.{
        .if_kw,  .identifier, .colon,
        .indent, .if_kw,      .identifier,
        .colon,  .indent,     .identifier,
        .dedent, .dedent,     .eof,
    });
}

test "Empty Lines Should Be Ignored" {
    const src =
        \\let x = 1
        \\
        \\let y = 2
    ;

    try expectTags(src, &.{
        .let, .identifier, .assign, .number_literal, .new_line,
        .let, .identifier, .assign, .number_literal, .eof,
    });
}

const std = @import("std");
const testing = std.testing;
