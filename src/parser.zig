pub const Parser = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    tokens: std.ArrayList(Token) = .empty,
    src: []const u8 = "",
    cur_index: usize = 0,

    pub fn parse(self: *Self, src: []const u8, tokens: std.ArrayList(Token)) !ast.Root {
        self.src = src;
        self.tokens = tokens;
        return self.parseBlock();
    }

    pub fn parseBlock(self: *Self) !ast.Block {
        var statements: std.ArrayList(ast.Statement) = .empty;

        var current_tk = self.peekCurrent();
        while (current_tk.tag != .eof and current_tk.tag != .dedent) : (current_tk = self.peekCurrent()) {
            if (self.peekCurrent().tag == .new_line) {
                self.walk();
            }

            if (self.peekCurrent().tag == .eof) {
                break;
            }

            const stmt = try self.parseStmt();
            try statements.append(self.allocator, stmt);
        }

        return .{ .statements = statements };
    }

    fn parseStmt(self: *Self) !ast.Statement {
        const token = self.tokens.items[self.cur_index];

        switch (token.tag) {
            .let_kw => {
                return ast.Statement{ .let_stmt = try self.parseLetStmt() };
            },
            .if_kw => {
                return ast.Statement{ .if_stmt = try self.parseIfStmt() };
            },
            .for_kw => {
                return ast.Statement{ .for_stmt = try self.parseForStmt() };
            },
            .identifier => {
                if (self.peekNext().tag == .assign) {
                    return ast.Statement{ .assign_stmt = try self.parseAssignStmt() };
                }
                defer self.walk();
                return ast.Statement{ .fn_call_stmt = try self.parseFnCall() };
            },
            .return_kw => {
                return ast.Statement{ .return_stmt = try self.parseReturnStmt() };
            },
            else => {
                std.debug.print("{any}\n", .{self.peekCurrent().tag});
                return ParserError.UnexpectedToken;
            },
        }
    }

    fn parseLetStmt(self: *Self) !ast.LetStmt {
        self.walk();

        const is_mutable = self.match(.mut_kw);
        const identifier_token = try self.expect(.identifier);
        _ = try self.expect(.colon);
        const type_annotation = try self.parseTypeAnnotation();
        _ = try self.expect(.assign);
        const value = try self.parseExpression();

        return ast.LetStmt{ .identifier = identifier_token.value(self.src), .is_mut = is_mutable, .type_annotation = type_annotation, .value = value };
    }

    fn parseIfStmt(self: *Self) ParserError!ast.IfStmt {
        self.walk();
        const exp = try self.parseExpression();
        var else_block: ?ast.Block = null;

        _ = try self.expect(.indent);
        const then_block = try self.parseBlock();
        _ = try self.expect(.dedent);

        const has_else = self.match(.else_kw);
        if (has_else) {
            _ = try self.expect(.indent);
            else_block = try self.parseBlock();
            _ = try self.expect(.dedent);
        }

        return ast.IfStmt{ .condition = exp, .then_block = then_block, .else_block = else_block };
    }

    fn parseForStmt(self: *Self) ParserError!ast.ForStmt {
        self.walk();
        var condition: ?*ast.Exp = null;
        if (self.peekCurrent().tag != .indent) {
            condition = try self.parseExpression();
        }

        _ = try self.expect(.indent);
        const do_block = try self.parseBlock();
        _ = try self.expect(.dedent);
        return ast.ForStmt{ .condition = condition, .do_block = do_block };
    }

    fn parseAssignStmt(self: *Self) ParserError!ast.AssignStmt {
        const tk = self.peekCurrent();
        const id = tk.value(self.src);
        self.walk();
        _ = try self.expect(.assign);
        const exp = try self.parseExpression();
        return ast.AssignStmt{ .identifier = id, .exp = exp };
    }

    fn parseFnDef(self: *Self) ParserError!ast.FnDef {
        var arguments: std.ArrayList(ast.FnArg) = .empty;
        self.walk();

        _ = try self.expect(.l_paren);
        if (self.peekCurrent().tag != .r_paren) {
            arguments = try self.parseFnArgs();
        }
        _ = try self.expect(.r_paren);

        _ = try self.expect(.arrow);
        const return_type = try self.parseTypeAnnotation();

        _ = try self.expect(.indent);
        const body_block = try self.parseBlock();

        return ast.FnDef{ .arguments = arguments, .body_block = body_block, .return_type = return_type };
    }

    fn parseFnArgs(self: *Self) ParserError!std.ArrayList(ast.FnArg) {
        var arguments: std.ArrayList(ast.FnArg) = .empty;
        while (true) {
            const arg = try self.parseFnArg();
            try arguments.append(self.allocator, arg);
            if (self.peekCurrent().tag != .comma) {
                break;
            }
            self.walk();
        }
        return arguments;
    }

    fn parseFnArg(self: *Self) ParserError!ast.FnArg {
        const tk = try self.expect(.identifier);
        const id = tk.value(self.src);

        _ = try self.expect(.colon);
        const type_annotation = try self.parseTypeAnnotation();

        var default_value: ?*ast.Exp = null;
        if (self.match(.assign)) {
            default_value = try self.parseExpression();
        }

        return ast.FnArg{ .identifier = id, .type_annotation = type_annotation, .default_value = default_value };
    }

    fn parseFnCall(self: *Self) ParserError!ast.FnCall {
        const tk = try self.expect(.identifier);
        const id = tk.value(self.src);
        var arguments: std.ArrayList(*ast.Exp) = .empty;

        _ = try self.expect(.l_paren);

        if (self.peekCurrent().tag != .r_paren) {
            arguments = try self.parseFnCallArgs();
        }

        if (self.peekCurrent().tag != .r_paren) {
            return ParserError.UnexpectedToken;
        }

        return ast.FnCall{ .identifier = id, .arguments = arguments };
    }

    fn parseFnCallArgs(self: *Self) ParserError!std.ArrayList(*ast.Exp) {
        var arguments: std.ArrayList(*ast.Exp) = .empty;

        while (true) {
            const arg = try self.parseExpression();
            try arguments.append(self.allocator, arg);

            if (!self.match(.comma)) {
                break;
            }
        }

        return arguments;
    }

    fn parseReturnStmt(self: *Self) ParserError!ast.ReturnStmt {
        self.walk();
        const cur_tk = self.peekCurrent();
        if (cur_tk.tag == .new_line or cur_tk.tag == .dedent or cur_tk.tag == .eof) {
            return ast.ReturnStmt{ .exp = null };
        }

        return ast.ReturnStmt{ .exp = try self.parseExpression() };
    }

    fn parseExpression(self: *Self) ParserError!*ast.Exp {
        defer self.walk();
        const tk = self.peekCurrent();

        switch (tk.tag) {
            .identifier, .number_literal, .string_literal, .true_literal, .false_literal => {
                const next_tk = self.peekNext();
                switch (next_tk.tag) {
                    .minus, .plus, .star, .slash, .percent, .equals, .not_equals, .greater_than, .greater_than_or_equal, .less_than, .less_than_or_equal => {
                        const bin_exp = try self.parseBinaryExp();
                        return ast.Exp.init(self.allocator, .{ .binary_exp = bin_exp });
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
            .fn_kw => {
                return ast.Exp.init(self.allocator, .{ .fn_def = try self.parseFnDef() });
            },
            else => {
                return ParserError.UnexpectedToken;
            },
        }
    }

    fn parseBinaryExp(self: *Self) ParserError!ast.BinaryExp {
        const left = try self.parseLiteral();
        self.walk();

        const op = try ast.BinaryOp.fromTag(self.peekCurrent().tag);
        self.walk();

        const right = try self.parseLiteral();
        return ast.BinaryExp{ .left = left, .op = op, .right = right };
    }

    //temporary while pratt's not impl
    fn parseLiteral(self: *Self) ParserError!*ast.Exp {
        const token = self.peekCurrent();

        return switch (token.tag) {
            .identifier => {
                const next_tk = self.peekNext();
                if (next_tk.tag == .l_paren) {
                    return ast.Exp.init(self.allocator, .{ .fn_call = try self.parseFnCall() });
                }
                return ast.Exp.init(self.allocator, .{ .identifier = token.value(self.src) });
            },
            .number_literal => {
                const value = std.fmt.parseFloat(f64, token.value(self.src)) catch {
                    return ParserError.UnexpectedToken;
                };
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
            .string_kw, .number_kw, .bool_kw, .fn_kw, .void_kw => ast.TypeAnnotation{
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

        std.debug.print("{any}\n", .{self.peekCurrent().tag});
        return ParserError.UnexpectedToken;
    }

    fn walk(self: *Self) void {
        self.cur_index += 1;
    }

    fn peekCurrent(self: *Self) Token {
        if (self.cur_index >= self.tokens.items.len) {
            return Token.init(Tag.eof, 0, 0);
        }

        return self.tokens.items[self.cur_index];
    }

    fn peekNext(self: *Self) Token {
        return self.peekNextN(1);
    }

    fn peekNextN(self: *Self, n: usize) Token {
        if (self.cur_index + n >= self.tokens.items.len) {
            return Token.init(Tag.eof, 0, 0);
        }

        return self.tokens.items[self.cur_index + n];
    }
};

pub const ParserError = error{
    UnexpectedToken,
    OutOfMemory,
};

const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const Token = lexer.Token;
const Tag = lexer.Tag;
