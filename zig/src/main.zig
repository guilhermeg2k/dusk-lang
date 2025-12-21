const std = @import("std");
const zig = @import("zig");

pub const Tag = enum {
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

const LexerError = error{
    InvalidIndentation,
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

const Lexer = struct {
    const Self = @This();

    source: []const u8,
    cur_index: usize = 0,
    last_indentation_level: usize = 0,
    pending_dedents: usize = 0,

    pub fn init(source: []const u8) Self {
        return Self{ .source = source };
    }

    pub fn next(self: *Self) Token {
        if (self.pending_dedents != 0) {
            self.pending_dedents -= 1;
            return Token.init(Tag.dedent, self.cur_index, self.cur_index);
        }

        if (self.cur_index == self.source.len) {
            return Token.init(Tag.eof, self.cur_index, self.cur_index);
        }

        const start = self.cur_index;
        self.readWhiteSpaces();

        const char = self.source[self.cur_index];
        if (char == '\n') {
            const token = self.readNewLine() catch {
                return Token.init(Tag.err, start, self.cur_index);
            };

            if (token) |tk| {
                return tk;
            }

            return self.next();
        }

        switch (char) {
            '(' => {
                defer self.walk();
                return Token.init(Tag.l_paren, self.cur_index, self.cur_index);
            },
            ')' => {
                defer self.walk();
                return Token.init(Tag.r_paren, self.cur_index, self.cur_index);
            },
            ':' => {
                defer self.walk();
                return Token.init(Tag.colon, self.cur_index, self.cur_index);
            },
            ',' => {
                defer self.walk();
                return Token.init(Tag.comma, self.cur_index, self.cur_index);
            },
            '+' => {
                defer self.walk();
                return Token.init(Tag.plus, self.cur_index, self.cur_index);
            },
            '-' => {
                return self.readMinusSymbol();
            },
            '*' => {
                defer self.walk();
                return Token.init(Tag.star, self.cur_index, self.cur_index);
            },
            '/' => {
                defer self.walk();
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
                std.debug.print("else {c}", .{char});
                return Token.init(Tag.err, self.cur_index, self.cur_index);
            },
        }

        return Token.init(Tag.eof, start, self.cur_index);
    }

    fn readExclamationMarkSymbol(self: *Self) Token {
        defer self.walk();
        const next_char = self.peek();

        if (next_char == '=') {
            const start = self.cur_index;
            self.walk();
            return Token.init(Tag.not_equals, start, self.cur_index);
        }

        return Token.init(Tag.err, self.cur_index, self.cur_index);
    }

    fn readMinusSymbol(self: *Self) Token {
        defer self.walk();
        const next_char = self.peek();

        if (next_char == '>') {
            const start = self.cur_index;
            self.walk();
            return Token.init(Tag.arrow, start, self.cur_index);
        }

        return Token.init(Tag.minus, self.cur_index, self.cur_index);
    }

    fn readGreaterSymbol(self: *Self) Token {
        defer self.walk();
        const next_char = self.peek();

        if (next_char == '=') {
            const start = self.cur_index;
            self.walk();
            return Token.init(Tag.greater_or_equal, start, self.cur_index);
        }

        return Token.init(Tag.greater_than, self.cur_index, self.cur_index);
    }

    fn readLessThanSymbol(self: *Self) Token {
        defer self.walk();
        const next_char = self.peek();

        if (next_char == '=') {
            const start = self.cur_index;
            self.walk();
            return Token.init(Tag.less_or_equal, start, self.cur_index);
        }

        return Token.init(Tag.less_than, self.cur_index, self.cur_index);
    }

    fn readEqualsSymbol(self: *Self) Token {
        defer self.walk();
        const next_char = self.peek();

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
            const curr = self.source[self.cur_index];
            if (std.ascii.isDigit(curr) or curr == '.') {
                self.walk();
            } else {
                break;
            }
        }

        return Token.init(Tag.number_literal, start, self.cur_index);
    }

    fn readStringLiteral(self: *Self) Token {
        const start = self.cur_index;
        self.walk();

        while (self.cur_index < self.source.len) {
            const curr = self.source[self.cur_index];
            self.walk();
            if (curr == '\'') {
                break;
            }
        }

        return Token.init(Tag.string_literal, start, self.cur_index);
    }

    fn readWord(self: *Self) Token {
        const start = self.cur_index;
        while (self.cur_index < self.source.len) {
            const curr = self.source[self.cur_index];
            if (std.ascii.isAlphanumeric(curr) or curr == '_') {
                self.walk();
            } else {
                break;
            }
        }

        const word = self.source[start..self.cur_index];
        if (keywords.get(word)) |tag| {
            return Token.init(tag, start, self.cur_index);
        }

        return Token.init(Tag.identifier, start, self.cur_index);
    }

    fn readNewLine(self: *Self) !?Token {
        self.walk();
        const start = self.cur_index;
        var spaces: usize = 0;
        while (self.cur_index < self.source.len and self.source[self.cur_index] == ' ') {
            spaces += 1;
            self.walk();
        }

        if (spaces % INDENTATION_WIDTH != 0) {
            return LexerError.InvalidIndentation;
        }

        const indentation_level = spaces / INDENTATION_WIDTH;

        if (indentation_level > self.last_indentation_level) {
            if (indentation_level - 1 > self.last_indentation_level) {
                return LexerError.InvalidIndentation;
            }
            self.last_indentation_level = indentation_level;
            return Token.init(Tag.indent, start, self.cur_index - 1);
        }

        if (indentation_level < self.last_indentation_level) {
            self.pending_dedents = self.last_indentation_level - indentation_level;
            self.last_indentation_level = indentation_level;
            return null;
        }

        return Token.init(Tag.new_line, start, self.cur_index - 1);
    }

    fn walk(self: *Self) void {
        self.cur_index += 1;
    }

    fn peek(self: *Self) u8 {
        if (self.cur_index + 1 == self.source.len) {
            return 0;
        }

        return self.source[self.cur_index + 1];
    }
};

pub fn main() void {
    const src =
        \\let main = ()
        \\    let mut x = 444.44
        \\    let mut x = 444.44
        \\    let mut y = 'salve coronel 213121\1'
        \\    if x > 2 <
        \\    >= <= != ==
        \\():
    ;
    var lexer = Lexer.init(src);

    var tk = lexer.next();

    while (tk.tag != Tag.eof and tk.tag != Tag.err) {
        std.debug.print("tk {any} = {s}\n", .{ tk.tag, tk.value(src) });
        tk = lexer.next();
    }

    std.debug.print("tk {any} = {s}\n", .{ tk.tag, tk.value(src) });
}
