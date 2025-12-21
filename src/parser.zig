pub const Parser = struct {
    const Self = @This();

    tokens: std.ArrayList(Token),
    alloc: std.mem.Allocator,
    src: []const u8,
    cur_index: usize = 0,

    pub fn init(alloc: std.mem.Allocator, src: []const u8, tokens: std.ArrayList(Token)) Self {
        return Self{ .alloc = alloc, .src = src, .tokens = tokens, .cur_index = 0 };
    }

    pub fn parse(self: *Self) !*ast.Root {
        var statements: std.ArrayList(ast.Statement) = .empty;

        while (self.peekCurrent().tag != .eof) {
            const stmt = try self.parseStmt();
            statements.append(self.alloc, stmt);
        }

        return .Node{ .root = {
            statements = statements;
        } };
    }

    fn parseStmt(self: *Self) !*ast.Statement {
        const token = self.tokens.items[self.cur_index];
        switch (token.tag) {
            .let_kw => try self.parserLetStmt(),
            else => ParserError.UnexpectedToken,
        }
    }

    fn parserLetStmt(self: *Self) !*ast.LetStmt {
        self.walk();
        const id_token = try self.expect(.identifier);

        self.walk();

        //todo: support mut

        _ = try self.expect(.colon);
        self.walk();
        const type_annotation = try self.parseType();
        self.walk();
        _ = try self.expect(.assign);
        const value = try self.parseExpression();

        return ast.LetStmt{ .identifier = id_token.value(self.src), .is_mut = false, .type_annotation = type_annotation, .value = value };
    }

    fn parseExpression(self: *Self) !*ast.Exp {
        const tk = self.peekCurrent();

        switch (tk.tag) {
            .number_literal, .string_literal, .true_literal, .false_literal => {
                const next_tk = self.peekNext();
                switch (next_tk.tag) {
                    .minus, .plus, .star, .slash, .percent, .equals, .not_equals, .greater_than, .greater_than_or_equal, .less_than, .less_than_or_equal => {
                        const left = switch (tk.tag) {
                            .number_literal => try std.fmt.parseFloat(f64, tk.value()),
                            .string_literal => tk.value(),
                            .true_literal => true,
                            .false_literal => false,
                            else => ParserError.UnexpectedToken,
                        };

                        self.walk();
                        const op = ast.BinaryOp.fromTag(self.peekCurrent().tag);
                        self.walk();

                        const right = switch (self.peekCurrent()) {
                            .number_literal => try std.fmt.parseFloat(f64, tk.value()),
                            .string_literal => tk.value(),
                            .true_literal => true,
                            .false_literal => false,
                            else => ParserError.UnexpectedToken,
                        };

                        return ast.BinaryExp{ .left = left, .op = op, .right = right };
                    },
                    else => {
                        switch (tk.tag) {
                            .number_literal => try std.fmt.parseFloat(f64, tk.value()),
                            .string_literal => tk.value(),
                            .true_literal => true,
                            .false_literal => false,
                            else => ParserError.UnexpectedToken,
                        }
                    },
                }
            },
            .not, .minus => {
                const op = ast.UnaryOp.fromTag(self.peekCurrent().tag);
                self.walk();
                const right = switch (self.peekCurrent()) {
                    .number_literal => try std.fmt.parseFloat(f64, tk.value()),
                    .string_literal => tk.value(),
                    .true_literal => true,
                    .false_literal => false,
                    else => ParserError.UnexpectedToken,
                };
                return ast.UnaryExp{
                    .op = op,
                    .right = right,
                };
            },
            else => ParserError.UnexpectedToken,
        }
    }

    fn parseType(self: *Self) !*ast.TypeAnnotation {
        //todo: stoped here
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
        if (self.cur_index == self.source.len) {
            return Token.init(Tag.eof, 0, 0);
        }

        return self.tokens[self.cur_index];
    }

    fn peekNext(self: *Self) Token {
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
