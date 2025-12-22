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

        return statements;
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
        const id_token = try self.expect(.identifier);

        self.walk();

        //todo: support mut

        _ = try self.expect(.colon);
        self.walk();
        const type_annotation = try self.parseType();
        self.walk();
        _ = try self.expect(.assign);
        self.walk();
        const value = try self.parseExpression();

        return ast.LetStmt{ .identifier = id_token.value(self.src), .is_mut = false, .type_annotation = type_annotation, .value = value };
    }

    fn parseExpression(self: *Self) !*ast.Exp {
        const tk = self.peekCurrent();
        defer self.walk();

        switch (tk.tag) {
            .number_literal, .string_literal, .true_literal, .false_literal, .identifier => {
                const next_tk = self.peekNext();

                switch (next_tk.tag) {
                    .minus, .plus, .star, .slash, .percent, .equals, .not_equals, .greater_than, .greater_than_or_equal, .less_than, .less_than_or_equal => {
                        const left = switch (tk.tag) {
                            .identifier => {
                                const exp = try self.allocator.create(ast.Exp);
                                const value = tk.value(self.src);
                                exp.* = ast.Exp{ .identifier = value };
                                return exp;
                            },
                            .number_literal => {
                                const exp = try self.allocator.create(ast.Exp);
                                const value = try std.fmt.parseFloat(f64, tk.value(self.src));
                                exp.* = ast.Exp{ .number_literal = value };
                                return exp;
                            },
                            .string_literal => {
                                const exp = try self.allocator.create(ast.Exp);
                                const value = tk.value(self.src);
                                exp.* = ast.Exp{ .string_literal = value };
                                return exp;
                            },
                            .true_literal => {
                                const exp = try self.allocator.create(ast.Exp);
                                exp.* = ast.Exp{ .bool_literal = true };
                                return exp;
                            },
                            .false_literal => {
                                const exp = try self.allocator.create(ast.Exp);
                                exp.* = ast.Exp{ .bool_literal = false };
                                return exp;
                            },
                            else => {
                                return ParserError.UnexpectedToken;
                            },
                        };

                        self.walk();
                        const op = ast.BinaryOp.fromTag(self.peekCurrent().tag);
                        self.walk();

                        const right = switch (self.peekCurrent().tag) {
                            .identifier => {
                                const exp = try self.allocator.create(ast.Exp);
                                const value = tk.value(self.src);
                                exp.* = ast.Exp{ .identifier = value };
                                return exp;
                            },
                            .number_literal => {
                                const exp = try self.allocator.create(ast.Exp);
                                const value = try std.fmt.parseFloat(f64, tk.value(self.src));
                                exp.* = ast.Exp{ .number_literal = value };
                                return exp;
                            },
                            .string_literal => {
                                const exp = try self.allocator.create(ast.Exp);
                                const value = tk.value(self.src);
                                exp.* = ast.Exp{ .string_literal = value };
                                return exp;
                            },
                            .true_literal => {
                                const exp = try self.allocator.create(ast.Exp);
                                exp.* = ast.Exp{ .bool_literal = true };
                                return exp;
                            },
                            .false_literal => {
                                const exp = try self.allocator.create(ast.Exp);
                                exp.* = ast.Exp{ .bool_literal = false };
                                return exp;
                            },
                            else => {
                                return ParserError.UnexpectedToken;
                            },
                        };

                        const exp = try self.allocator.create(ast.Exp);
                        exp.* = ast.Exp{ .binary = .{ .left = left, .op = op, .right = right } };
                        return exp;
                    },
                    else => {
                        switch (tk.tag) {
                            .identifier => {
                                const exp = try self.allocator.create(ast.Exp);
                                const value = tk.value(self.src);
                                exp.* = ast.Exp{ .identifier = value };
                                return exp;
                            },
                            .number_literal => {
                                const exp = try self.allocator.create(ast.Exp);
                                const value = try std.fmt.parseFloat(f64, tk.value(self.src));
                                exp.* = ast.Exp{ .number_literal = value };
                                return exp;
                            },
                            .string_literal => {
                                const exp = try self.allocator.create(ast.Exp);
                                const value = tk.value(self.src);
                                exp.* = ast.Exp{ .string_literal = value };
                                return exp;
                            },
                            .true_literal => {
                                const exp = try self.allocator.create(ast.Exp);
                                exp.* = ast.Exp{ .bool_literal = true };
                                return exp;
                            },
                            .false_literal => {
                                const exp = try self.allocator.create(ast.Exp);
                                exp.* = ast.Exp{ .bool_literal = false };
                                return exp;
                            },
                            else => {
                                return ParserError.UnexpectedToken;
                            },
                        }
                    },
                }
            },
            .not, .minus => {
                const op = ast.UnaryOp.fromTag(self.peekCurrent().tag);
                self.walk();
                const current = self.peekCurrent();

                const right = switch (current.tag) {
                    .identifier => {
                        const exp = try self.allocator.create(ast.Exp);
                        const value = tk.value(self.src);
                        exp.* = ast.Exp{ .identifier = value };
                        return exp;
                    },
                    .number_literal => {
                        const exp = try self.allocator.create(ast.Exp);
                        const value = try std.fmt.parseFloat(f64, tk.value(self.src));
                        exp.* = ast.Exp{ .number_literal = value };
                        return exp;
                    },
                    .string_literal => {
                        const exp = try self.allocator.create(ast.Exp);
                        const value = tk.value(self.src);
                        exp.* = ast.Exp{ .string_literal = value };
                        return exp;
                    },
                    .true_literal => {
                        const exp = try self.allocator.create(ast.Exp);
                        exp.* = ast.Exp{ .bool_literal = true };
                        return exp;
                    },
                    .false_literal => {
                        const exp = try self.allocator.create(ast.Exp);
                        exp.* = ast.Exp{ .bool_literal = false };
                        return exp;
                    },
                    else => {
                        return ParserError.UnexpectedToken;
                    },
                };

                const exp = try self.allocator.create(ast.Exp);
                exp.* = ast.Exp{ .unary = .{ .op = op, .right = right } };
                return exp;
            },
            else => {
                return ParserError.UnexpectedToken;
            },
        }
    }

    fn parseType(self: *Self) !ast.TypeAnnotation {
        const token = self.peekCurrent();

        return switch (token.tag) {
            .string_kw, .number_kw, .bool_kw, .fn_kw => ast.TypeAnnotation{
                .name = token.value(self.src),
            },
            else => ParserError.UnexpectedToken,
        };
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
