pub const Parser = struct {
    const Self = @This();

    tokens: std.ArrayList(Token),
    allocator: std.mem.Allocator,
    src: []const u8,
    cur_index: usize = 0,

    pub fn init(allococator: std.mem.Allocator, src: []const u8, tokens: std.ArrayList(Token)) Self {
        return Self{ .allocator = allococator, .src = src, .tokens = tokens, .cur_index = 0 };
    }

    pub fn parse(self: *Self) !ast.Root {
        var statements: std.ArrayList(ast.Statement) = .empty;

        while (self.peekCurrent().tag != .eof) {
            const stmt = try self.parseStmt();
            try statements.append(self.allocator, stmt);
        }

        return .{ .statements = statements };
    }

    fn parseStmt(self: *Self) !ast.Statement {
        const token = self.tokens.items[self.cur_index];

        switch (token.tag) {
            .let_kw => {
                return ast.Statement{ .let_stmt = try self.parserLetStmt() };
            },
            else => {
                return ParserError.UnexpectedToken;
            },
        }
    }

    fn parserLetStmt(self: *Self) !ast.LetStmt {
        self.walk();

        const is_mutable = self.match(.mut_kw);
        const identifier_token = try self.expect(.identifier);
        _ = try self.expect(.colon);
        const type_annotation = try self.parseTypeAnnotation();
        _ = try self.expect(.assign);
        const value = try self.parseExpression();

        return ast.LetStmt{ .identifier = identifier_token.value(self.src), .is_mut = is_mutable, .type_annotation = type_annotation, .value = value };
    }

    fn parseExpression(self: *Self) !*ast.Exp {
        defer self.walk();
        const tk = self.peekCurrent();

        switch (tk.tag) {
            .number_literal, .string_literal, .true_literal, .false_literal, .identifier => {
                const next_tk = self.peekNext();
                switch (next_tk.tag) {
                    .minus, .plus, .star, .slash, .percent, .equals, .not_equals, .greater_than, .greater_than_or_equal, .less_than, .less_than_or_equal => {
                        const left = try self.parseLiteral();
                        self.walk();

                        const op = try ast.BinaryOp.fromTag(self.peekCurrent().tag);
                        self.walk();

                        const right = try self.parseLiteral();
                        return ast.Exp.init(self.allocator, .{ .binary_exp = .{ .left = left, .op = op, .right = right } });
                    },
                    else => {
                        return self.parseLiteral();
                    },
                }
            },
            .not, .minus => {
                const op = try ast.UnaryOp.fromTag(self.peekCurrent().tag);
                self.walk();
                const right = try self.parseLiteral();
                return ast.Exp.init(self.allocator, .{ .unary_exp = .{ .op = op, .right = right } });
            },
            else => {
                return ParserError.UnexpectedToken;
            },
        }
    }

    //temporary while pratt's not impl
    fn parseLiteral(self: *Self) !*ast.Exp {
        const token = self.peekCurrent();
        return switch (token.tag) {
            .identifier => {
                return ast.Exp.init(self.allocator, .{ .identifier = token.value(self.src) });
            },
            .number_literal => {
                const value = try std.fmt.parseFloat(f64, token.value(self.src));
                return ast.Exp.init(self.allocator, .{ .number_literal = value });
            },
            .string_literal => {
                return ast.Exp.init(self.allocator, .{ .string_literal = token.value(self.src) });
            },
            .true_literal => {
                return ast.Exp.init(self.allocator, .{ .bool_literal = true });
            },
            .false_literal => {
                return ast.Exp.init(self.allocator, .{ .bool_literal = false });
            },
            else => {
                return ParserError.UnexpectedToken;
            },
        };
    }

    fn parseTypeAnnotation(self: *Self) !ast.TypeAnnotation {
        defer self.walk();
        const token = self.peekCurrent();

        return switch (token.tag) {
            .string_kw, .number_kw, .bool_kw, .fn_kw => ast.TypeAnnotation{
                .name = token.value(self.src),
            },
            else => ParserError.UnexpectedToken,
        };
    }

    fn match(self: *Self, tag: Tag) bool {
        const token = self.peekCurrent();
        if (token.tag == tag) {
            self.walk();
            return true;
        }

        return false;
    }

    fn expect(self: *Self, tag: Tag) !Token {
        defer self.walk();
        const token = self.peekCurrent();
        if (token.tag == tag) {
            return token;
        }

        return ParserError.UnexpectedToken;
    }

    fn walk(self: *Self) void {
        self.cur_index += 1;
    }

    fn peekCurrent(self: *Self) Token {
        if (self.cur_index == self.src.len) {
            return Token.init(Tag.eof, 0, 0);
        }

        return self.tokens.items[self.cur_index];
    }

    fn peekNext(self: *Self) Token {
        if (self.cur_index + 1 == self.src.len) {
            return Token.init(Tag.eof, 0, 0);
        }

        return self.tokens.items[self.cur_index + 1];
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
