pub const Parser = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    err_dispatcher: err.ErrorDispatcher,
    tokens: []const Token = &.{},
    src: []const u8 = "",
    cur_index: usize = 0,
    loop_depth: usize = 0,

    pub fn init(
        allocator: std.mem.Allocator,
        src: []const u8,
        tokens: []const Token,
    ) Self {
        return Self{
            .allocator = allocator,
            .src = src,
            .tokens = tokens,
            .err_dispatcher = .{
                .allocator = allocator,
                .src = src,
            },
        };
    }

    pub fn parse(self: *Self) !ast.Root {
        return self.parseBlock();
    }

    pub fn parseBlock(self: *Self) ParserError!ast.Block {
        var statements: std.ArrayList(ast.StatementNode) = .empty;

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

    fn parseStmt(self: *Self) !ast.StatementNode {
        const tk = self.peekCurrent();

        switch (tk.tag) {
            .let_kw => {
                return ast.StatementNode{ .data = .{ .let_stmt = try self.parseLetStmt() }, .loc_start = tk.loc.start };
            },
            .if_kw => {
                return ast.StatementNode{ .data = .{ .if_stmt = try self.parseIfStmt() }, .loc_start = tk.loc.start };
            },
            .for_kw => {
                return ast.StatementNode{ .data = .{ .for_stmt = try self.parseForStmt() }, .loc_start = tk.loc.start };
            },
            .identifier => {
                return self.parseIdentifierStmt();
            },
            .break_kw => {
                if (self.loop_depth == 0) {
                    return self.err_dispatcher.invalidSyntax("break called outside a loop", tk);
                }
                self.walk();
                return ast.StatementNode{ .data = .{ .break_stmt = {} }, .loc_start = tk.loc.start };
            },
            .continue_kw => {
                if (self.loop_depth == 0) {
                    return self.err_dispatcher.invalidSyntax("continue called outside a loop", tk);
                }
                self.walk();
                return ast.StatementNode{ .data = .{ .continue_stmt = {} }, .loc_start = tk.loc.start };
            },
            .return_kw => {
                return ast.StatementNode{ .data = .{ .return_stmt = try self.parseReturnStmt() }, .loc_start = tk.loc.start };
            },
            else => {
                return self.err_dispatcher.invalidSyntax("let, if, for, return ...", tk);
            },
        }
    }

    fn parseLetStmt(self: *Self) !ast.LetStmt {
        self.walk();
        const is_mutable = self.match(.mut_kw);
        const identifier_token = try self.expect(.identifier);
        var type_annotation: ?*ast.TypeAnnotation = null;

        const has_type_annotation = self.match(.colon);
        if (has_type_annotation) {
            type_annotation = try self.parseTypeAnnotation();
        }

        _ = try self.expect(.eq);
        const value = try self.parseExp(0);

        return ast.LetStmt{
            .identifier = identifier_token.value(self.src),
            .is_mut = is_mutable,
            .type_annotation = type_annotation,
            .value = value,
        };
    }

    fn parseIdentifierStmt(self: *Self) !ast.StatementNode {
        const expr = try self.parseExp(0);
        const cur_tk = self.peekCurrent();

        switch (cur_tk.tag) {
            .eq => {
                self.walk();
                const assign_exp = try self.parseExp(0);
                return ast.StatementNode{
                    .data = .{ .assign_stmt = .{
                        .target = expr,
                        .exp = assign_exp,
                    } },
                    .loc_start = expr.loc_start,
                };
            },

            .plus_eq => {
                self.walk();
                const right_exp = try self.parseExp(0);
                const bin_exp = try ast.ExpNode.init(self.allocator, .{
                    .data = .{ .binary_exp = .{
                        .left = expr,
                        .op = .add,
                        .right = right_exp,
                    } },
                    .loc_start = expr.loc_start,
                });

                return ast.StatementNode{
                    .data = .{ .assign_stmt = .{
                        .target = expr,
                        .exp = bin_exp,
                    } },
                    .loc_start = expr.loc_start,
                };
            },

            .minus_eq => {
                self.walk();
                const right_exp = try self.parseExp(0);
                const bin_exp = try ast.ExpNode.init(self.allocator, .{
                    .data = .{ .binary_exp = .{
                        .left = expr,
                        .op = .sub,
                        .right = right_exp,
                    } },
                    .loc_start = expr.loc_start,
                });

                return ast.StatementNode{
                    .data = .{ .assign_stmt = .{
                        .target = expr,
                        .exp = bin_exp,
                    } },
                    .loc_start = expr.loc_start,
                };
            },

            else => {
                return ast.StatementNode{
                    .data = .{ .expression_stmt = expr },
                    .loc_start = expr.loc_start,
                };
            },
        }
    }

    fn parseIfStmt(self: *Self) ParserError!ast.IfStmt {
        self.walk();
        const exp = try self.parseExp(0);
        var else_block: ?ast.Block = null;

        _ = try self.expect(.indent);
        const then_block = try self.parseBlock();
        _ = try self.expect(.dedent);

        const has_else = self.match(.else_kw);
        if (has_else) {
            const is_else_if = self.peekCurrent().tag == .if_kw;
            if (is_else_if) {
                else_block = try self.parseBlock();
            } else {
                _ = try self.expect(.indent);
                else_block = try self.parseBlock();
                _ = try self.expect(.dedent);
            }
        }

        return ast.IfStmt{ .condition = exp, .then_block = then_block, .else_block = else_block };
    }

    fn parseForStmt(self: *Self) ParserError!ast.ForStmt {
        self.walk();

        self.loop_depth += 1;
        defer self.loop_depth -= 1;

        const condition = try self.parseExp(0);
        _ = try self.expect(.indent);
        const do_block = try self.parseBlock();
        _ = try self.expect(.dedent);

        return ast.ForStmt{ .condition = condition, .do_block = do_block };
    }

    fn parseStructDef(self: *Self) ParserError!ast.StructDef {
        var tk = try self.expect(.indent);
        var fields: std.ArrayList(ast.StructField) = .empty;
        var funcs: std.ArrayList(ast.StructFn) = .empty;

        while (tk.tag != .dedent and tk.tag != .eof) : (tk = self.peekCurrent()) {
            const identifier = try self.expect(.identifier);
            _ = try self.expect(.colon);
            const is_fn = self.match(.l_paren);

            if (is_fn) {
                const fn_def = try self.parseFnDef();
                try funcs.append(self.allocator, .{
                    .identifier = identifier.value(self.src),
                    .def = try ast.ExpNode.init(self.allocator, .{
                        .loc_start = tk.loc.start,
                        .data = .{
                            .fn_def = fn_def,
                        },
                    }),
                });
                continue;
            }

            const field_type = try self.parseTypeAnnotation();
            try fields.append(self.allocator, .{
                .identifier = identifier.value(self.src),
                .type = field_type,
            });
            self.walk();
        }

        _ = try self.expect(.dedent);

        return ast.StructDef{
            .funcs = try funcs.toOwnedSlice(self.allocator),
            .fields = try fields.toOwnedSlice(self.allocator),
        };
    }

    fn parseFnDef(self: *Self) ParserError!ast.FnDef {
        var arguments: std.ArrayList(ast.FnParam) = .empty;
        var body_block: ast.Block = undefined;
        var return_type: ?*ast.TypeAnnotation = null;

        if (self.peekCurrent().tag != .r_paren) {
            arguments = try self.parseFnArgs();
        }

        _ = try self.expect(.r_paren);
        _ = try self.expect(.arrow);

        const tk = self.peekCurrent();
        if (tk.tag == .return_kw) {
            var statements: std.ArrayList(ast.StatementNode) = .empty;
            const return_stmt = ast.StatementNode{
                .data = .{ .return_stmt = try self.parseReturnStmt() },
                .loc_start = tk.loc.start,
            };
            try statements.append(self.allocator, return_stmt);
            body_block = .{ .statements = statements };
        } else {
            return_type = try self.parseTypeAnnotation();
            _ = try self.expect(.indent);
            body_block = try self.parseBlock();
            _ = try self.expect(.dedent);
        }

        return ast.FnDef{
            .params = try arguments.toOwnedSlice(self.allocator),
            .body_block = body_block,
            .return_type = return_type,
        };
    }

    fn parseFnArgs(self: *Self) ParserError!std.ArrayList(ast.FnParam) {
        var arguments: std.ArrayList(ast.FnParam) = .empty;
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

    fn parseFnArg(self: *Self) ParserError!ast.FnParam {
        var default_value: ?*ast.ExpNode = null;
        const is_mut = self.match(.mut_kw);

        const id_tk = try self.expect(.identifier);
        const id = id_tk.value(self.src);

        _ = try self.expect(.colon);
        const type_annotation = try self.parseTypeAnnotation();

        if (self.match(.eq)) {
            default_value = try self.parseExp(0);
        }

        return ast.FnParam{
            .identifier = id,
            .is_mut = is_mut,
            .type_annotation = type_annotation,
            .default_value = default_value,
        };
    }

    fn parseArrayLiteral(self: *Self) ParserError!ast.ArrayLiteral {
        var exps: []const *ast.ExpNode = &.{};
        if (self.peekCurrent().tag != .r_bracket) {
            exps = try self.parseExpList();
        }

        _ = try self.expect(.r_bracket);

        return ast.ArrayLiteral{ .exps = exps };
    }

    fn parseExpList(self: *Self) ParserError![]const *ast.ExpNode {
        var exps: std.ArrayList(*ast.ExpNode) = .empty;

        while (true) {
            const exp = try self.parseExp(0);
            try exps.append(self.allocator, exp);

            if (!self.match(.comma)) {
                break;
            }
        }

        return exps.toOwnedSlice(self.allocator);
    }

    fn parseFnCallArgs(self: *Self) ParserError![]const ast.FnCallArg {
        if (self.peekCurrent().tag == .r_paren) {
            return &.{};
        }

        var are_arguments_named = false;
        var args: std.ArrayList(ast.FnCallArg) = .empty;

        var i: usize = 0;
        while (true) : (i += 1) {
            const prev_tk = self.peekCurrent();
            const exp = try self.parseExp(0);
            const tk = self.peekCurrent();

            if (i == 0 and tk.tag == .eq) {
                are_arguments_named = true;
            }

            if (are_arguments_named) {
                if (tk.tag != .eq) {
                    return self.err_dispatcher.invalidSyntax("=", tk);
                }

                if (exp.data != .identifier) {
                    return self.err_dispatcher.invalidSyntax("identifier", prev_tk);
                }

                self.walk();
                const value = try self.parseExp(0);
                try args.append(self.allocator, .{
                    .identifier = exp.data.identifier,
                    .exp = value,
                });
            } else {
                try args.append(self.allocator, .{
                    .identifier = null,
                    .exp = exp,
                });
            }

            if (self.peekCurrent().tag != .comma) {
                break;
            }

            self.walk();
        }

        return args.toOwnedSlice(self.allocator);
    }

    fn parseReturnStmt(self: *Self) ParserError!ast.ReturnStmt {
        self.walk();
        const cur_tk = self.peekCurrent();

        if (cur_tk.tag == .new_line or cur_tk.tag == .dedent or cur_tk.tag == .eof) {
            return ast.ReturnStmt{ .exp = null };
        }

        return ast.ReturnStmt{ .exp = try self.parseExp(0) };
    }

    fn parseExp(self: *Self, min_bp: u8) ParserError!*ast.ExpNode {
        var exp = try self.parsePrefix();
        exp = try self.parsePostfix(exp);

        while (true) {
            const tk = self.peekCurrent();
            const op_bp = self.getBindingPower(tk.tag);
            if (op_bp <= min_bp) break;

            //if dont break it means it's a binary op
            const op_token = self.peekCurrent();
            const op = try ast.BinaryOp.fromTag(op_token.tag);

            self.walk();
            const right = try self.parseExp(op_bp);

            exp = try ast.ExpNode.init(self.allocator, .{
                .data = .{ .binary_exp = .{
                    .left = exp,
                    .op = op,
                    .right = right,
                } },
                .loc_start = tk.loc.start,
            });
        }

        return exp;
    }

    fn parsePrefix(self: *Self) ParserError!*ast.ExpNode {
        const tk = self.peekCurrent();
        self.walk();

        return switch (tk.tag) {
            .identifier => {
                return ast.ExpNode.init(self.allocator, .{ .data = .{ .identifier = tk.value(self.src) }, .loc_start = tk.loc.start });
            },
            .number_literal => {
                const value = std.fmt.parseFloat(f64, tk.value(self.src)) catch {
                    return self.err_dispatcher.invalidSyntax("number literal", tk);
                };
                return ast.ExpNode.init(self.allocator, .{ .data = .{ .number_literal = value }, .loc_start = tk.loc.start });
            },
            .string_literal => {
                return ast.ExpNode.init(self.allocator, .{ .data = .{ .string_literal = tk.value(self.src) }, .loc_start = tk.loc.start });
            },
            .true_literal => {
                return ast.ExpNode.init(self.allocator, .{ .data = .{ .bool_literal = true }, .loc_start = tk.loc.start });
            },
            .false_literal => {
                return ast.ExpNode.init(self.allocator, .{ .data = .{ .bool_literal = false }, .loc_start = tk.loc.start });
            },
            .l_bracket => {
                return ast.ExpNode.init(
                    self.allocator,
                    .{
                        .data = .{ .array_literal = try self.parseArrayLiteral() },
                        .loc_start = tk.loc.start,
                    },
                );
            },
            .l_paren => {
                const is_function_def = self.isFuncDef();

                if (is_function_def) {
                    return ast.ExpNode.init(self.allocator, .{ .data = .{ .fn_def = try self.parseFnDef() }, .loc_start = tk.loc.start });
                }

                const exp = try self.parseExp(0);
                _ = try self.expect(.r_paren);
                return exp;
            },
            .not, .minus => {
                const op = try ast.UnaryOp.fromTag(tk.tag);
                const right = try self.parseExp(100);
                return ast.ExpNode.init(self.allocator, .{ .data = .{ .unary_exp = .{ .op = op, .right = right } }, .loc_start = tk.loc.start });
            },
            .fn_kw => {
                return ast.ExpNode.init(self.allocator, .{ .data = .{ .fn_def = try self.parseFnDef() }, .loc_start = tk.loc.start });
            },
            .struct_kw => {
                return ast.ExpNode.init(self.allocator, .{ .data = .{ .struct_def = try self.parseStructDef() }, .loc_start = tk.loc.start });
            },
            else => {
                return self.err_dispatcher.invalidSyntax("valid expression", tk);
            },
        };
    }

    fn isFuncDef(self: *Self) bool {
        const start_id = self.cur_index;
        defer self.cur_index = start_id;

        var paren_depth: usize = 1;
        while (paren_depth != 0) {
            switch (self.peekCurrent().tag) {
                .l_paren => {
                    paren_depth += 1;
                },
                .r_paren => {
                    paren_depth -= 1;
                },
                .new_line => {
                    break;
                },
                else => {},
            }
            self.walk();
        }

        const tk = self.peekCurrent();
        return tk.tag == .arrow or tk.tag == .return_kw;
    }

    fn parsePostfix(self: *Self, left: *ast.ExpNode) ParserError!*ast.ExpNode {
        var node = left;

        while (true) {
            const tk = self.peekCurrent();

            if (tk.tag == .l_paren) {
                self.walk();
                const args = try self.parseFnCallArgs();
                _ = try self.expect(.r_paren);

                const call_node = try ast.ExpNode.init(self.allocator, .{
                    .data = .{
                        .fn_call = .{
                            .are_arguments_named = args.len > 0 and args[0].identifier != null,
                            .identifier = node.data.identifier,
                            .arguments = args,
                        },
                    },
                    .loc_start = node.loc_start,
                });

                node = call_node;
                //warn: this is supporting only simple fn calls
                continue;
            }

            if (tk.tag == .l_bracket) {
                self.walk();

                const index = try self.parseExp(0);
                _ = try self.expect(.r_bracket);

                const index_node = try ast.ExpNode.init(self.allocator, .{
                    .data = .{ .index_exp = .{
                        .target = node,
                        .index = index,
                    } },
                    .loc_start = node.loc_start,
                });

                node = index_node;
                continue;
            }

            if (tk.tag == .dot) {
                self.walk();
                const index = try self.parseExp(0);

                const index_node = try ast.ExpNode.init(self.allocator, .{ .data = .{
                    .index_exp = .{
                        .target = node,
                        .index = index,
                    },
                }, .loc_start = node.loc_start });

                node = index_node;
                continue;
            }

            break;
        }

        return node;
    }

    fn getBindingPower(_: *const Self, tag: Tag) u8 {
        return switch (tag) {
            .or_kw => 10,
            .and_kw => 20,
            .double_eq, .not_eq, .gt, .ge, .lt, .le => 30,
            .plus, .minus => 40,
            .star, .slash, .percent => 50,
            .l_paren => 60,
            else => 0,
        };
    }

    fn parseTypeAnnotation(self: *Self) !*ast.TypeAnnotation {
        const tk = self.peekCurrent();
        self.walk();

        return switch (tk.tag) {
            .string_kw, .number_kw, .bool_kw, .fn_kw, .void_kw => ast.TypeAnnotation.init(self.allocator, .{ .name = tk.value(self.src) }),
            .at => ast.TypeAnnotation.init(self.allocator, .{ .struct_self = {} }),
            .l_bracket => {
                _ = try self.expect(.r_bracket);
                const arr_type = try self.parseTypeAnnotation();
                return ast.TypeAnnotation.init(self.allocator, .{
                    .array = arr_type,
                });
            },
            else => {
                return self.err_dispatcher.invalidSyntax("type string, number, bool, fn, void, ...", tk);
            },
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

        return self.err_dispatcher.invalidSyntax(@tagName(tag), token);
    }

    fn walk(self: *Self) void {
        self.cur_index += 1;
    }

    fn peekCurrent(self: *Self) Token {
        if (self.cur_index >= self.tokens.len) {
            return Token.init(Tag.eof, 0, 0);
        }

        return self.tokens[self.cur_index];
    }

    fn peekNext(self: *Self) Token {
        return self.peekNextN(1);
    }

    fn peekNextN(self: *Self, n: usize) Token {
        if (self.cur_index + n >= self.tokens.len) {
            return Token.init(Tag.eof, 0, 0);
        }

        return self.tokens[self.cur_index + n];
    }
};

const Token = lexer.Token;
const Tag = lexer.Tag;
const Error = err.ErrorDispatcher;
const ParserError = err.Errors;

const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const err = @import("error.zig");
const std = @import("std");
