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

        return source[self.loc.start..self.loc.end];
    }

    pub fn make(tag: Tag, start: usize, end: usize) Self {
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
    index: usize = 0,
    indent_stack: [64]u8 = [_]u8{0} ** 64,
    stack_top: usize = 0,
    last_indentation_level: usize = 0,
    pending_dedents: usize = 0,

    pub fn init(source: []const u8) Self {
        return Self{ .source = source };
    }

    pub fn next(self: *Self) Token {
        if (self.pending_dedents != 0) {
            self.pending_dedents -= 1;
            return Token.make(Tag.dedent, self.index, self.index);
        }

        if (self.index == self.source.len) {
            return Token.make(Tag.eof, self.index, self.index + 1);
        }

        const start = self.index;
        self.skipWhiteSpace();

        const char = self.source[self.index];
        if (char == '\n') {
            const token = self.readNewLine() catch {
                return Token.make(Tag.err, start, self.index);
            };

            if (token) |tk| {
                return tk;
            }

            return self.next();
        }

        switch (char) {
            '(' => {
                defer self.walk();
                return Token.make(Tag.l_paren, self.index, self.index + 1);
            },
            ')' => {
                defer self.walk();
                return Token.make(Tag.r_paren, self.index, self.index + 1);
            },
            ':' => {
                defer self.walk();
                return Token.make(Tag.colon, self.index, self.index + 1);
            },
            ',' => {
                defer self.walk();
                return Token.make(Tag.comma, self.index, self.index + 1);
            },
            '+' => {
                defer self.walk();
                return Token.make(Tag.plus, self.index, self.index + 1);
            },
            '-' => {
                defer self.walk();
                return Token.make(Tag.minus, self.index, self.index + 1);
            },
            '*' => {
                defer self.walk();
                return Token.make(Tag.star, self.index, self.index + 1);
            },
            '/' => {
                defer self.walk();
                return Token.make(Tag.slash, self.index, self.index + 1);
            },
            '=' => {
                defer self.walk();
                return Token.make(Tag.equals, self.index, self.index + 1);
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
                return Token.make(Tag.err, self.index, self.index + 1);
            },
        }

        return Token.make(Tag.eof, start, self.index);
    }

    fn skipWhiteSpace(self: *Self) void {
        while (self.index < self.source.len and self.source[self.index] == ' ') {
            self.walk();
        }
    }

    fn readNumberLiteral(self: *Self) Token {
        const start = self.index;
        while (self.index < self.source.len) {
            const curr = self.source[self.index];
            if (std.ascii.isDigit(curr) or curr == '.') {
                self.walk();
            } else {
                break;
            }
        }

        return Token.make(Tag.number_literal, start, self.index);
    }

    fn readStringLiteral(self: *Self) Token {
        const start = self.index;
        self.walk();

        while (self.index < self.source.len) {
            const curr = self.source[self.index];
            self.walk();
            if (curr == '\'') {
                break;
            }
        }

        return Token.make(Tag.string_literal, start, self.index);
    }

    fn readWord(self: *Self) Token {
        const start = self.index;
        while (self.index < self.source.len) {
            const curr = self.source[self.index];
            if (std.ascii.isAlphanumeric(curr) or curr == '_') {
                self.walk();
            } else {
                break;
            }
        }

        const word = self.source[start..self.index];
        if (keywords.get(word)) |tag| {
            return Token.make(tag, start, self.index);
        }

        return Token.make(Tag.identifier, start, self.index);
    }

    fn readNewLine(self: *Self) !?Token {
        self.walk();
        const start = self.index;
        var spaces: usize = 0;
        while (self.index < self.source.len and self.source[self.index] == ' ') {
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
            return Token.make(Tag.indent, start, self.index);
        }

        if (indentation_level < self.last_indentation_level) {
            self.pending_dedents = self.last_indentation_level - indentation_level;
            self.last_indentation_level = indentation_level;
            return null;
        }

        return Token.make(Tag.new_line, start, self.index);
    }

    fn walk(self: *Self) void {
        self.index += 1;
    }

    fn peek(self: *Self) u8 {
        if (self.index + 1 == self.source.len) {
            return 0;
        }

        return self.source[self.index + 1];
    }
};

pub fn main() void {
    const src =
        \\let main = ():
        \\    let mut x = 444.44
        \\    let mut x = 444.44
        \\    let mut y = 'salve coronel 213121\1'
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
