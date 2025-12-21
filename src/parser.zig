pub const Parser = struct {
    const Self = @This();

    tokens: std.ArrayList(Token),
    alloc: std.mem.Allocator,
    src: []const u8,
    cur_index: usize = 0,

    pub fn init(alloc: std.mem.Allocator, src: []const u8, tokens: std.ArrayList(Token)) Self {
        return Self{ .alloc = alloc, .src = src, .tokens = tokens, .cur_index = 0 };
    }

    pub fn parse(self: *Self) !*ast.Node {
        var statements: std.ArrayList(ast.Node) = .empty;

        while (true) {
            const stmt = try self.parseStmt();
            statements.append(self.alloc, stmt);
        }

        return .Node{ .root = {
            statements = statements;
        } };
    }

    fn parseStmt(self: *Self) !*ast.Node {
        const token = self.tokens.items[self.cur_index];
        switch (token.tag) {
            .let_kw => try self.parserLetStmt(),
            else => ParserError.UnexpectedToken,
        }
    }

    fn parserLetStmt(self: *Self) !*ast.LetStmt {
        self.walk();
        const token = try self.expect(.identifier);
        const id = token.value(self.src);

        self.walk();
        try self.expect(.colon);
        try self.expect(.assign);
        //todo: stop here

    }

    fn expect(self: *Self, tag: Tag) !Token {
        const token = self.tokens.items[self.cur_index];
        if (token.tag == tag) {
            return token;
        }

        return ParserError.UnexpectedToken;
    }

    fn walk(self: *Self) void {
        self.cur_index += 1;
    }

    fn peek(self: *Self) Token {
        if (self.cur_index == self.source.len) {
            return Token.init(Tag.eof, 0, 0);
        }

        return self.tokens[self.cur_index];
    }

    fn peek_next(self: *Self) Token {
        if (self.cur_index + 1 == self.source.len) {
            return Token.init(Tag.eof, 0, 0);
        }

        return self.tokens[self.cur_index + 1];
    }
};

pub const ParserError = error{
    UnexpectedToken,
    OutOfMemory,
};

const std = @import("std");
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");
const Token = lexer.Token;
const Tag = lexer.Tag;
