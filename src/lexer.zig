const std = @import("std");
const err = @import("error.zig");

const LexerError = err.Errors;

pub const Lexer = struct {
    const Self = @This();

    const keywords = std.StaticStringMap(Tag).initComptime(.{
        .{ "let", Tag.let_kw },
        .{ "mut", Tag.mut_kw },
        .{ "int", Tag.int_kw },
        .{ "float", Tag.float_kw },
        .{ "string", Tag.string_kw },
        .{ "bool", Tag.bool_kw },
        .{ "void", Tag.void_kw },
        .{ "struct", Tag.struct_kw },
        .{ "enum", Tag.enum_kw },
        .{ "if", Tag.if_kw },
        .{ "else", Tag.else_kw },
        .{ "elif", Tag.elif_kw },
        .{ "and", Tag.and_kw },
        .{ "or", Tag.or_kw },
        .{ "for", Tag.for_kw },
        .{ "continue", Tag.continue_kw },
        .{ "break", Tag.break_kw },
        .{ "true", Tag.true_literal },
        .{ "false", Tag.false_literal },
        .{ "fn", Tag.fn_kw },
        .{ "return", Tag.return_kw },
        .{ "null", Tag.null_literal },
        .{ "then", Tag.then_kw },
        .{ "do", Tag.do_kw },
        .{ "end", Tag.end_kw },
    });

    allocator: std.mem.Allocator,
    src: []const u8 = "",
    err_dispatcher: err.ErrorDispatcher,
    cur_index: usize = 0,

    pub fn init(allocator: std.mem.Allocator, src: []const u8) Self {
        return Self{
            .allocator = allocator,
            .src = src,
            .err_dispatcher = .{ .allocator = allocator, .src = src },
        };
    }

    pub fn list(self: *Self) err.Errors!std.ArrayList(Token) {
        var tokens: std.ArrayList(Token) = .empty;

        while (true) {
            const tkn = self.next();

            if (tkn.tag == Tag.err) {
                return self.err_dispatcher.unexpectedToken(tkn);
            }

            try tokens.append(self.allocator, tkn);
            if (tkn.tag == Tag.eof) break;
        }

        return tokens;
    }

    fn next(self: *Self) Token {
        while (true) {
            const is_eof = self.cur_index >= self.src.len;
            if (is_eof) {
                return Token.init(Tag.eof, self.cur_index, self.cur_index);
            }

            defer self.walk();
            self.readWhiteSpaces();
            const cur_char = self.peekCurrent();
            switch (cur_char) {
                '#' => {
                    while (self.peekCurrent() != '\n') {
                        self.walk();
                    }
                },
                '\n' => {
                    continue;
                },
                '(' => {
                    return Token.init(Tag.l_paren, self.cur_index, self.cur_index);
                },
                ')' => {
                    return Token.init(Tag.r_paren, self.cur_index, self.cur_index);
                },
                '[' => {
                    return Token.init(Tag.l_bracket, self.cur_index, self.cur_index);
                },
                ']' => {
                    return Token.init(Tag.r_bracket, self.cur_index, self.cur_index);
                },
                ':' => {
                    return Token.init(Tag.colon, self.cur_index, self.cur_index);
                },
                ',' => {
                    return Token.init(Tag.comma, self.cur_index, self.cur_index);
                },
                '+' => {
                    return self.readComplexSymbol('=', Tag.plus_eq, Tag.plus);
                },
                '-' => {
                    if (self.peekNext() == '>') {
                        return self.readComplexSymbol('>', Tag.arrow, Tag.minus);
                    }
                    return self.readComplexSymbol('=', Tag.minus_eq, Tag.minus);
                },
                '@' => {
                    return Token.init(Tag.at, self.cur_index, self.cur_index);
                },
                '?' => {
                    return self.readComplexSymbol('.', Tag.question_mark_dot, Tag.question_mark);
                },
                '.' => {
                    return Token.init(Tag.dot, self.cur_index, self.cur_index);
                },
                '*' => {
                    return Token.init(Tag.star, self.cur_index, self.cur_index);
                },
                '/' => {
                    return self.readComplexSymbol('/', Tag.double_slash, Tag.slash);
                },
                '%' => {
                    return Token.init(Tag.percent, self.cur_index, self.cur_index);
                },
                '=' => {
                    return self.readComplexSymbol('=', Tag.double_eq, Tag.eq);
                },
                '>' => {
                    return self.readComplexSymbol('=', Tag.ge, Tag.gt);
                },
                '<' => {
                    return self.readComplexSymbol('=', Tag.le, Tag.lt);
                },
                '!' => {
                    return self.readComplexSymbol('=', Tag.not_eq, Tag.not);
                },
                '|' => {
                    return self.readComplexSymbol('>', Tag.pipe, Tag.err);
                },
                'a'...'z', 'A'...'Z', '_' => {
                    return self.readWord();
                },
                '0'...'9' => {
                    return self.readNumberLiteral();
                },
                '"' => {
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

    fn readNumberLiteral(self: *Self) Token {
        const start = self.cur_index;
        var next_char = self.peekNext();
        var is_float = false;

        while (std.ascii.isDigit(next_char) or next_char == '.') {
            if (next_char == '.') {
                is_float = true;
            }
            self.walk();
            next_char = self.peekNext();
        }

        const token_type = if (is_float) Tag.float_literal else Tag.int_literal;
        return Token.init(token_type, start, self.cur_index);
    }

    fn readStringLiteral(self: *Self) Token {
        const start = self.cur_index;

        while (self.peekNext() != '"') {
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

        const word = self.src[start .. self.cur_index + 1];

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

    fn walk(self: *Self) void {
        self.cur_index += 1;
    }

    fn peekCurrent(self: *Self) u8 {
        if (self.cur_index >= self.src.len) {
            return 0;
        }
        return self.src[self.cur_index];
    }

    fn peekNext(self: *Self) u8 {
        if (self.cur_index + 1 >= self.src.len) {
            return 0;
        }
        return self.src[self.cur_index + 1];
    }
};

pub const Token = struct {
    const Self = @This();
    tag: Tag,
    loc: Loc,

    pub fn value(self: Token, src: []const u8) []const u8 {
        if (self.tag == Tag.eof) {
            return "EOF";
        }

        if (self.tag == Tag.string_literal) {
            return src[self.loc.start + 1 .. self.loc.end];
        }

        return src[self.loc.start .. self.loc.end + 1];
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
    float_kw,
    int_kw,
    void_kw,
    bool_kw,
    struct_kw,
    enum_kw,
    if_kw,
    else_kw,
    elif_kw,
    for_kw,
    continue_kw,
    break_kw,
    and_kw,
    or_kw,
    fn_kw,
    return_kw,
    then_kw,
    do_kw,
    end_kw,
    string_literal,
    int_literal,
    float_literal,
    true_literal,
    false_literal,
    null_literal,
    eq,
    double_eq,
    plus_eq,
    minus_eq,
    not,
    not_eq,
    colon,
    comma,
    lt,
    le,
    gt,
    ge,
    at,
    dot,
    question_mark,
    question_mark_dot,
    l_bracket,
    r_bracket,
    l_paren,
    r_paren,
    arrow,
    plus,
    minus,
    star,
    slash,
    double_slash,
    percent,
    pipe,
    eof,
    err,
};

pub const Loc = struct {
    start: usize,
    end: usize,
};
