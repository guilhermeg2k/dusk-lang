const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const err = @import("error.zig");
const std = @import("std");
const Token = lexer.Token;
const Tag = lexer.Tag;
const Errors = err.Errors;

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
        return self.parseBlock(&.{});
    }

    pub fn parseBlock(self: *Self, extra_stop_tags: []const Tag) Errors!ast.Block {
        var statements: std.ArrayList(ast.StatementNode) = .empty;

        var current_tk = self.peekCurrent();
        while (!isStopToken(current_tk.tag, extra_stop_tags)) : (current_tk = self.peekCurrent()) {
            const stmt = try self.parseStmt();
            try statements.append(self.allocator, stmt);
        }

        return .{ .statements = statements };
    }

    fn isStopToken(tag: Tag, extrap_stop_tags: []const Tag) bool {
        if (tag == .eof or tag == .end_kw) return true;
        for (extrap_stop_tags) |t| {
            if (tag == t) return true;
        }
        return false;
    }

    fn parseStmt(self: *Self) !ast.StatementNode {
        const tk = self.peekCurrent();

        switch (tk.tag) {
            .let_kw => {
                const let_stmt_value = try self.parseLetStmt();
                return ast.StatementNode{
                    .data = .{ .let_stmt = let_stmt_value },
                    .loc = .{ .start = tk.loc.start, .end = let_stmt_value.value.loc.end },
                };
            },
            .func_kw => {
                const func_stmt_value = try self.parseFuncStmt();
                return ast.StatementNode{
                    .data = .{ .func_stmt = func_stmt_value },
                    .loc = .{ .start = tk.loc.start, .end = self.tokens[self.cur_index - 1].loc.end },
                };
            },
            .struct_kw => {
                const struct_stmt_value = try self.parseStructStmt();
                return ast.StatementNode{
                    .data = .{ .struct_stmt = struct_stmt_value },
                    .loc = .{ .start = tk.loc.start, .end = self.tokens[self.cur_index - 1].loc.end },
                };
            },
            .enum_kw => {
                const enum_stmt_value = try self.parseEnumStmt();
                return ast.StatementNode{
                    .data = .{ .enum_stmt = enum_stmt_value },
                    .loc = .{ .start = tk.loc.start, .end = self.tokens[self.cur_index - 1].loc.end },
                };
            },
            .if_kw => {
                if (self.isIfCapture()) {
                    const if_capture = try self.parseIfCapture();
                    const end_pos = self.tokens[self.cur_index - 1].loc.end;
                    return ast.StatementNode{
                        .data = .{ .if_capture_stmt = if_capture },
                        .loc = .{ .start = tk.loc.start, .end = end_pos },
                    };
                }
                const if_stmt = try self.parseIfStmt();
                const end_pos = self.tokens[self.cur_index - 1].loc.end;
                return ast.StatementNode{ .data = .{ .if_stmt = if_stmt }, .loc = .{ .start = tk.loc.start, .end = end_pos } };
            },
            .for_kw => {
                const for_stmt = try self.parseForStmt();
                const end_pos = self.tokens[self.cur_index - 1].loc.end;
                return ast.StatementNode{ .data = .{ .for_stmt = for_stmt }, .loc = .{ .start = tk.loc.start, .end = end_pos } };
            },
            .identifier => {
                return self.parseIdentifierStmt();
            },
            .break_kw => {
                if (self.loop_depth == 0) {
                    return self.err_dispatcher.invalidSyntax("break called outside a loop", tk);
                }
                self.walk();
                return ast.StatementNode{ .data = .{ .break_stmt = {} }, .loc = tk.loc };
            },
            .continue_kw => {
                if (self.loop_depth == 0) {
                    return self.err_dispatcher.invalidSyntax("continue called outside a loop", tk);
                }
                self.walk();
                return ast.StatementNode{ .data = .{ .continue_stmt = {} }, .loc = tk.loc };
            },
            .return_kw => {
                const return_stmt = try self.parseReturnStmt();
                const end_pos = if (return_stmt.exp) |exp| exp.loc.end else tk.loc.end;
                return ast.StatementNode{ .data = .{ .return_stmt = return_stmt }, .loc = .{ .start = tk.loc.start, .end = end_pos } };
            },
            .then_kw, .do_kw, .end_kw, .elif_kw, .else_kw => {
                return self.err_dispatcher.invalidSyntax("unexpected keyword", tk);
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
            type_annotation = try self.parseTypeAnnotation(false);
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

    fn parseFuncStmt(self: *Self) !ast.FuncStmt {
        self.walk();
        const identifier_token = try self.expect(.identifier);
        _ = try self.expect(.l_paren);
        const fn_def = try self.parseFnDef();

        return ast.FuncStmt{
            .identifier = identifier_token.value(self.src),
            .def = fn_def,
        };
    }

    fn parseStructStmt(self: *Self) !ast.StructStmt {
        self.walk();
        const identifier_token = try self.expect(.identifier);
        const struct_def = try self.parseStructDef();

        return ast.StructStmt{
            .identifier = identifier_token.value(self.src),
            .def = struct_def,
        };
    }

    fn parseEnumStmt(self: *Self) !ast.EnumStmt {
        self.walk();
        const identifier_token = try self.expect(.identifier);
        const enum_def = try self.parseEnumDef();

        return ast.EnumStmt{
            .identifier = identifier_token.value(self.src),
            .def = enum_def,
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
                    .loc = .{ .start = expr.loc.start, .end = assign_exp.loc.end },
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
                    .loc = .{ .start = expr.loc.start, .end = right_exp.loc.end },
                });

                return ast.StatementNode{
                    .data = .{ .assign_stmt = .{
                        .target = expr,
                        .exp = bin_exp,
                    } },
                    .loc = .{ .start = expr.loc.start, .end = bin_exp.loc.end },
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
                    .loc = .{ .start = expr.loc.start, .end = right_exp.loc.end },
                });

                return ast.StatementNode{
                    .data = .{ .assign_stmt = .{
                        .target = expr,
                        .exp = bin_exp,
                    } },
                    .loc = .{ .start = expr.loc.start, .end = bin_exp.loc.end },
                };
            },

            else => {
                return ast.StatementNode{
                    .data = .{ .expression_stmt = expr },
                    .loc = expr.loc,
                };
            },
        }
    }

    fn parseIfCapture(self: *Self) !ast.IfCaptureStmt {
        self.walk();

        const exp = try self.parseExp(0);
        _ = try self.expect(.colon);

        const is_mut = self.match(.mut_kw);
        const identififer = try self.expect(.identifier);

        _ = try self.expect(.then_kw);
        const body = try self.parseBlock(&.{.else_kw});

        const has_else = self.match(.else_kw);
        var else_block: ?ast.Block = null;

        if (has_else) {
            else_block = try self.parseBlock(&.{});
        }

        _ = try self.expect(.end_kw);

        return ast.IfCaptureStmt{
            .exp = exp,
            .identifier = .{
                .name = identififer.value(self.src),
                .is_mut = is_mut,
            },
            .body = body,
            .else_block = else_block,
        };
    }

    fn parseIfStmt(self: *Self) Errors!ast.IfStmt {
        self.walk();
        const first_cond = try self.parseExp(0);

        _ = try self.expect(.then_kw);
        const then_block = try self.parseBlock(&.{ .else_kw, .elif_kw });

        var elif_chain: std.ArrayList(ast.StatementNode) = .empty;

        while (self.match(.elif_kw)) {
            const elif_start = self.tokens[self.cur_index - 1].loc.start;

            const cond = try self.parseExp(0);
            _ = try self.expect(.then_kw);
            const body = try self.parseBlock(&.{ .else_kw, .elif_kw });

            var elif_end = self.tokens[self.cur_index - 1].loc.end;
            if (body.statements.items.len > 0) {
                elif_end = body.statements.items[body.statements.items.len - 1].loc.end;
            }

            try elif_chain.append(self.allocator, .{
                .data = .{
                    .if_stmt = .{ .condition = cond, .then_block = body, .else_block = null },
                },
                .loc = .{
                    .start = elif_start,
                    .end = elif_end,
                },
            });
        }

        var else_block: ?ast.Block = null;
        if (self.match(.else_kw)) {
            else_block = try self.parseBlock(&.{});
        }

        _ = try self.expect(.end_kw);

        while (elif_chain.items.len > 0) {
            var stmt_node = elif_chain.pop().?;
            stmt_node.data.if_stmt.else_block = else_block;
            var stmts: std.ArrayList(ast.StatementNode) = .empty;
            try stmts.append(self.allocator, stmt_node);
            else_block = .{ .statements = stmts };
        }

        return .{ .condition = first_cond, .then_block = then_block, .else_block = else_block };
    }

    fn parseForStmt(self: *Self) Errors!ast.ForStmt {
        self.walk();

        self.loop_depth += 1;
        defer self.loop_depth -= 1;

        const condition = try self.parseExp(0);
        _ = try self.expect(.do_kw);
        const do_block = try self.parseBlock(&.{});
        _ = try self.expect(.end_kw);

        return ast.ForStmt{ .condition = condition, .do_block = do_block };
    }

    fn parseStructField(self: *Self, identifier: []const u8, has_type_annotation: bool) !ast.StructField {
        var type_annotation: ?*ast.TypeAnnotation = null;
        if (has_type_annotation) {
            type_annotation = try self.parseTypeAnnotation(true);
        }

        const has_initial_value = self.match(.eq);
        var value: ?*ast.ExpNode = null;
        if (has_initial_value) {
            value = try self.parseExp(0);
        }

        return .{
            .identifier = identifier,
            .is_mut = true,
            .type = type_annotation,
            .default_value = value,
        };
    }

    fn parseStructDef(self: *Self) Errors!ast.Struct {
        var static_fields: std.ArrayList(ast.StructField) = .empty;
        var fields: std.ArrayList(ast.StructField) = .empty;
        var funcs: std.ArrayList(ast.StatementNode) = .empty;

        while (true) {
            const tk = self.peekCurrent();
            switch (tk.tag) {
                .let_kw => {
                    self.walk();
                    const is_mut = self.match(.mut_kw);
                    const identifier_token = try self.expect(.identifier);
                    if (self.match(.colon)) {
                        const type_annotation = try self.parseTypeAnnotation(false);
                        _ = try self.expect(.eq);
                        const value = try self.parseExp(0);
                        try static_fields.append(self.allocator, .{
                            .identifier = identifier_token.value(self.src),
                            .is_mut = is_mut,
                            .type = type_annotation,
                            .default_value = value,
                        });
                    } else if (self.match(.eq)) {
                        const value = try self.parseExp(0);
                        try static_fields.append(self.allocator, .{
                            .identifier = identifier_token.value(self.src),
                            .is_mut = is_mut,
                            .type = null,
                            .default_value = value,
                        });
                    } else {
                        return self.err_dispatcher.invalidSyntax("':' or '='", tk);
                    }
                },
                .func_kw => {
                    self.walk();
                    const identifier_token = try self.expect(.identifier);
                    _ = try self.expect(.l_paren);
                    const fn_def = try self.parseFnDef();
                    try funcs.append(self.allocator, .{
                        .data = .{ .func_stmt = .{
                            .identifier = identifier_token.value(self.src),
                            .def = fn_def,
                        } },
                        .loc = .{ .start = tk.loc.start, .end = self.tokens[self.cur_index - 1].loc.end },
                    });
                },
                .identifier => {
                    self.walk();
                    const identifier = tk;
                    const has_type_annotation = self.match(.colon);
                    const strc_field = try self.parseStructField(identifier.value(self.src), has_type_annotation);
                    try fields.append(self.allocator, strc_field);
                },
                .end_kw, .eof => {
                    self.walk();
                    break;
                },
                else => {
                    return self.err_dispatcher.invalidSyntax("let or an identifier", tk);
                },
            }
        }

        return ast.Struct{
            .funcs = try funcs.toOwnedSlice(self.allocator),
            .fields = try fields.toOwnedSlice(self.allocator),
            .static_fields = try static_fields.toOwnedSlice(self.allocator),
        };
    }

    fn parseAnonymousStructDef(self: *Self) Errors!ast.Struct {
        var fields: std.ArrayList(ast.StructField) = .empty;

        while (true) {
            const tk = self.peekCurrent();
            switch (tk.tag) {
                .identifier => {
                    self.walk();
                    const identifier = tk;
                    const has_type_annotation = self.match(.colon);

                    const strc_field = try self.parseStructField(identifier.value(self.src), has_type_annotation);
                    try fields.append(self.allocator, strc_field);
                },
                .end_kw, .eof => {
                    self.walk();
                    break;
                },
                else => {
                    return self.err_dispatcher.invalidSyntax("identifier", tk);
                },
            }
        }

        return ast.Struct{
            .fields = try fields.toOwnedSlice(self.allocator),
            .static_fields = &.{},
            .funcs = &.{},
        };
    }

    fn parseEnumDef(self: *Self) Errors!ast.EnumDef {
        var variants: std.ArrayList(ast.EnumVariant) = .empty;
        var funcs: std.ArrayList(ast.StatementNode) = .empty;
        var mode: enum { none, implicit, explicit } = .none;

        while (true) {
            const tk = self.peekCurrent();
            switch (tk.tag) {
                .identifier => {
                    self.walk();
                    const identifier = tk.value(self.src);

                    if (self.match(.eq)) {
                        switch (mode) {
                            .none => mode = .explicit,
                            .implicit => return self.err_dispatcher.invalidSyntax(
                                "all enum variants to either have explicit values or none at all",
                                tk,
                            ),
                            .explicit => {},
                        }

                        const value_tk = try self.expect(.int_literal);
                        const value = std.fmt.parseInt(i64, value_tk.value(self.src), 10) catch {
                            return self.err_dispatcher.invalidSyntax("integer literal", value_tk);
                        };

                        try variants.append(self.allocator, .{
                            .identifier = identifier,
                            .value = value,
                        });
                    } else {
                        switch (mode) {
                            .none => mode = .implicit,
                            .explicit => return self.err_dispatcher.invalidSyntax(
                                "all enum variants to either have explicit values or none at all",
                                tk,
                            ),
                            .implicit => {},
                        }

                        try variants.append(self.allocator, .{
                            .identifier = identifier,
                            .value = null,
                        });
                    }
                },
                .func_kw => {
                    self.walk();
                    const identifier_token = try self.expect(.identifier);
                    _ = try self.expect(.l_paren);
                    const fn_def = try self.parseFnDef();
                    try funcs.append(self.allocator, .{
                        .data = .{
                            .func_stmt = .{
                                .identifier = identifier_token.value(self.src),
                                .def = fn_def,
                            },
                        },
                        .loc = .{
                            .start = tk.loc.start,
                            .end = self.tokens[self.cur_index - 1].loc.end,
                        },
                    });
                },
                .end_kw, .eof => {
                    self.walk();
                    break;
                },
                else => {
                    return self.err_dispatcher.invalidSyntax("identifier or 'end'", tk);
                },
            }
        }

        return ast.EnumDef{
            .variants = try variants.toOwnedSlice(self.allocator),
            .funcs = try funcs.toOwnedSlice(self.allocator),
        };
    }

    fn parseFnDef(self: *Self) Errors!ast.FnDef {
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
            const ret_stmt = try self.parseReturnStmt();
            const return_stmt = ast.StatementNode{
                .data = .{ .return_stmt = ret_stmt },
                .loc = .{ .start = tk.loc.start, .end = if (ret_stmt.exp) |exp| exp.loc.end else tk.loc.end },
            };
            try statements.append(self.allocator, return_stmt);
            body_block = .{ .statements = statements };
        } else {
            return_type = try self.parseTypeAnnotation(false);
            body_block = try self.parseBlock(&.{});
            _ = try self.expect(.end_kw);
        }

        return ast.FnDef{
            .params = try arguments.toOwnedSlice(self.allocator),
            .body_block = body_block,
            .return_type = return_type,
        };
    }

    fn parseFnArgs(self: *Self) Errors!std.ArrayList(ast.FnParam) {
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

    fn parseFnArg(self: *Self) Errors!ast.FnParam {
        var default_value: ?*ast.ExpNode = null;
        var type_annotation: ?*ast.TypeAnnotation = null;

        const is_mut = self.match(.mut_kw);

        const id_tk = try self.expect(.identifier);
        const id = id_tk.value(self.src);

        const has_type_annotation = self.match(.colon);
        if (has_type_annotation) {
            type_annotation = try self.parseTypeAnnotation(false);
        }

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

    fn parseArrayLiteral(self: *Self) Errors!ast.ArrayLiteral {
        var exps: []const *ast.ExpNode = &.{};
        if (self.peekCurrent().tag != .r_bracket) {
            exps = try self.parseExpList();
        }

        _ = try self.expect(.r_bracket);

        return ast.ArrayLiteral{ .exps = exps };
    }

    fn parseExpList(self: *Self) Errors![]const *ast.ExpNode {
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

    fn parseFnCallArgs(self: *Self) Errors![]const ast.FnCallArg {
        if (self.peekCurrent().tag == .r_paren) {
            return &.{};
        }

        var are_arguments_named: ?bool = null;
        var args: std.ArrayList(ast.FnCallArg) = .empty;

        while (true) {
            if (self.peekCurrent().tag == .r_paren) {
                break;
            }

            const exp = try self.parseExp(0);
            const tk = self.peekCurrent();

            if (are_arguments_named == null) {
                are_arguments_named = tk.tag == .eq;
            }

            if (are_arguments_named.?) {
                if (tk.tag != .eq) {
                    return self.err_dispatcher.invalidSyntax("=", tk);
                }

                if (exp.data != .identifier) {
                    return self.err_dispatcher.invalidSyntax("identifier", .{ .tag = .identifier, .loc = exp.loc });
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

            const cur = self.peekCurrent().tag;
            if (cur != .comma) {
                break;
            }

            self.walk();
        }

        return args.toOwnedSlice(self.allocator);
    }

    fn canStartExpression(_: *Self, tag: Tag) bool {
        return switch (tag) {
            .identifier,
            .int_literal,
            .float_literal,
            .string_literal,
            .true_literal,
            .false_literal,
            .null_literal,
            .l_paren,
            .l_bracket,
            .not,
            .minus,
            .at,
            => true,
            else => false,
        };
    }

    fn parseReturnStmt(self: *Self) Errors!ast.ReturnStmt {
        self.walk();
        const cur_tk = self.peekCurrent();

        if (cur_tk.tag == .eof or !self.canStartExpression(cur_tk.tag)) {
            return ast.ReturnStmt{ .exp = null };
        }

        return ast.ReturnStmt{ .exp = try self.parseExp(0) };
    }

    fn parsePipeOperator(self: *Self, exp: *ast.ExpNode) !*ast.ExpNode {
        self.walk();
        const right = try self.parseExp(self.getBindingPower(.l_paren));

        if (right.data != .fn_call) {
            return self.err_dispatcher.invalidSyntax("function call after '->'", .{ .tag = .identifier, .loc = right.loc });
        }

        const old_args = right.data.fn_call.arguments;
        var new_args = try std.ArrayList(ast.FnCallArg).initCapacity(self.allocator, old_args.len + 1);

        new_args.appendAssumeCapacity(
            .{
                .identifier = null,
                .exp = exp,
            },
        );

        new_args.appendSliceAssumeCapacity(old_args);

        right.data.fn_call.arguments = try new_args.toOwnedSlice(self.allocator);
        return right;
    }

    fn parseExp(self: *Self, min_bp: u8) Errors!*ast.ExpNode {
        var exp = try self.parsePrefix();
        exp = try self.parsePostfix(exp);

        while (true) {
            const tk = self.peekCurrent();
            const op_bp = self.getBindingPower(tk.tag);
            if (op_bp <= min_bp) {
                break;
            }

            if (tk.tag == .pipe) {
                exp = try self.parsePipeOperator(exp);
                continue;
            }

            //if dont break or continue it means it's a binary op
            const op_token = self.peekCurrent();
            const op = try ast.BinaryOp.fromTag(op_token.tag);

            self.walk();
            const right = try self.parseExp(op_bp);

            exp = try ast.ExpNode.init(self.allocator, .{
                .data = .{
                    .binary_exp = .{
                        .left = exp,
                        .op = op,
                        .right = right,
                    },
                },
                .loc = .{ .start = exp.loc.start, .end = right.loc.end },
            });
        }

        return exp;
    }

    fn parsePrefix(self: *Self) Errors!*ast.ExpNode {
        const tk = self.peekCurrent();
        self.walk();

        return switch (tk.tag) {
            .identifier => {
                return ast.ExpNode.init(self.allocator, .{
                    .data = .{ .identifier = tk.value(self.src) },
                    .loc = tk.loc,
                });
            },
            .int_literal => {
                const value = std.fmt.parseInt(i64, tk.value(self.src), 10) catch {
                    return self.err_dispatcher.invalidSyntax("int literal", tk);
                };
                return ast.ExpNode.init(self.allocator, .{
                    .data = .{ .int_literal = value },
                    .loc = tk.loc,
                });
            },
            .float_literal => {
                const value = std.fmt.parseFloat(f64, tk.value(self.src)) catch {
                    return self.err_dispatcher.invalidSyntax("float literal", tk);
                };
                return ast.ExpNode.init(self.allocator, .{ .data = .{ .float_literal = value }, .loc = tk.loc });
            },
            .string_literal => {
                return ast.ExpNode.init(self.allocator, .{ .data = .{ .string_literal = tk.value(self.src) }, .loc = tk.loc });
            },
            .true_literal => {
                return ast.ExpNode.init(self.allocator, .{ .data = .{ .bool_literal = true }, .loc = tk.loc });
            },
            .false_literal => {
                return ast.ExpNode.init(self.allocator, .{ .data = .{ .bool_literal = false }, .loc = tk.loc });
            },
            .null_literal => {
                return ast.ExpNode.init(self.allocator, .{ .data = .{ .null_literal = {} }, .loc = tk.loc });
            },
            .l_bracket => {
                const arr = try self.parseArrayLiteral();
                const arr_end = self.tokens[self.cur_index - 1].loc.end;
                return ast.ExpNode.init(
                    self.allocator,
                    .{
                        .data = .{ .array_literal = arr },
                        .loc = .{ .start = tk.loc.start, .end = arr_end },
                    },
                );
            },
            .l_paren => {
                const exp = try self.parseExp(0);
                _ = try self.expect(.r_paren);
                return exp;
            },
            .not, .minus => {
                const op = try ast.UnaryOp.fromTag(tk.tag);
                const right = try self.parseExp(100);
                return ast.ExpNode.init(self.allocator, .{ .data = .{ .unary_exp = .{ .op = op, .right = right } }, .loc = .{ .start = tk.loc.start, .end = right.loc.end } });
            },
            .at => {
                return ast.ExpNode.init(self.allocator, .{
                    .data = .{
                        .anonymous_struct_identifier = {},
                    },
                    .loc = tk.loc,
                });
            },
            else => {
                return self.err_dispatcher.invalidSyntax("valid expression", tk);
            },
        };
    }

    fn isIfCapture(self: *Self) bool {
        const start_id = self.cur_index;
        const tokens_scan_limit = 500;
        var tokens_scanned: usize = 0;
        defer self.cur_index = start_id;

        while (tokens_scanned < tokens_scan_limit) {
            const tk = self.peekCurrent();
            if (tk.tag == .colon) {
                return true;
            }
            if (tk.tag == .then_kw) {
                return false;
            }
            self.walk();
            tokens_scanned += 1;
        }

        return false;
    }

    fn parsePostfix(self: *Self, left: *ast.ExpNode) Errors!*ast.ExpNode {
        var node = left;

        var done = false;
        while (!done) {
            const tk = self.peekCurrent();

            switch (tk.tag) {
                //fnCall
                .l_paren => {
                    self.walk();
                    const args = try self.parseFnCallArgs();
                    const r_paren = try self.expect(.r_paren);

                    //note: this is supporting only simple fn calls
                    node = try ast.ExpNode.init(self.allocator, .{
                        .data = .{
                            .fn_call = .{
                                .are_arguments_named = args.len > 0 and args[0].identifier != null,
                                .target = node,
                                .arguments = args,
                            },
                        },
                        .loc = .{ .start = node.loc.start, .end = r_paren.loc.end },
                    });
                },

                //arrays
                .l_bracket => {
                    self.walk();

                    const index = try self.parseExp(0);
                    const r_bracket = try self.expect(.r_bracket);

                    node = try ast.ExpNode.init(self.allocator, .{
                        .data = .{ .indexed = .{
                            .target = node,
                            .index = index,
                            .nullable = false,
                        } },
                        .loc = .{ .start = node.loc.start, .end = r_bracket.loc.end },
                    });
                },

                //struct field member access
                .dot, .question_mark_dot => {
                    const nullable = tk.tag == .question_mark_dot;
                    self.walk();

                    const id_token = try self.expect(.identifier);
                    const id_node = try ast.ExpNode.init(self.allocator, .{
                        .data = .{ .identifier = id_token.value(self.src) },
                        .loc = id_token.loc,
                    });

                    node = try ast.ExpNode.init(self.allocator, .{
                        .data = .{ .indexed = .{
                            .target = node,
                            .index = id_node,
                            .nullable = nullable,
                        } },
                        .loc = .{ .start = node.loc.start, .end = id_token.loc.end },
                    });
                },

                else => done = true,
            }
        }

        return node;
    }

    fn getBindingPower(_: *const Self, tag: Tag) u8 {
        return switch (tag) {
            .pipe => 5,
            .or_kw => 10,
            .and_kw => 20,
            .double_eq, .not_eq, .gt, .ge, .lt, .le => 30,
            .plus, .minus => 40,
            .star, .slash, .double_slash, .percent => 50,
            .l_paren => 60,
            else => 0,
        };
    }

    fn parseTypeAnnotation(self: *Self, allow_anonymous_struct: bool) !*ast.TypeAnnotation {
        const is_nullable = self.match(.question_mark);
        const tk = self.peekCurrent();
        self.walk();

        if (!allow_anonymous_struct and tk.tag == .struct_kw) {
            return self.err_dispatcher.invalidSyntax("type string, number, bool, void, ...", tk);
        }

        return switch (tk.tag) {
            .string_kw, .int_kw, .float_kw, .bool_kw, .void_kw => ast.TypeAnnotation.init(self.allocator, .{
                .type = .{ .primitive = tk.value(self.src) },
                .nullable = is_nullable,
            }),
            .identifier => ast.TypeAnnotation.init(self.allocator, .{
                .type = .{ .@"struct" = tk.value(self.src) },
                .nullable = is_nullable,
            }),
            .struct_kw => ast.TypeAnnotation.init(self.allocator, .{
                .type = .{ .anonymous_struct = try self.parseAnonymousStructDef() },
                .nullable = is_nullable,
            }),
            .at => ast.TypeAnnotation.init(self.allocator, .{
                .type = .{ .struct_self = {} },
                .nullable = is_nullable,
            }),
            .l_bracket => {
                _ = try self.expect(.r_bracket);
                const arr_type = try self.parseTypeAnnotation(false);
                return ast.TypeAnnotation.init(self.allocator, .{
                    .type = .{ .array = arr_type },
                    .nullable = is_nullable,
                });
            },
            else => {
                return self.err_dispatcher.invalidSyntax("type string, number, bool, void, ...", tk);
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

    fn walkBack(self: *Self) void {
        self.cur_index = @max(0, self.cur_index - 1);
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

    fn peekBack(self: *Self) Token {
        if (self.cur_index - 1 >= self.tokens.len or self.cur_index - 1 == 0) {
            return Token.init(Tag.eof, 0, 0);
        }

        return self.tokens[self.cur_index - 1];
    }
};
