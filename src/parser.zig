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
                const letStmt = ast.StatementNode{
                    .data = .{ .let_stmt = try self.parseLetStmt() },
                    .loc_start = tk.loc.start,
                };

                if (self.peekCurrent().tag == .dedent) self.walk();
                return letStmt;
            },
            .if_kw => {
                if (self.isIfCapture()) {
                    return ast.StatementNode{
                        .data = .{ .if_capture_stmt = try self.parseIfCapture() },
                        .loc_start = tk.loc.start,
                    };
                }
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

    fn parseIfCapture(self: *Self) !ast.IfCaptureStmt {
        self.walk();

        const exp = try self.parseExp(0);
        _ = try self.expect(.colon);

        const is_mut = self.match(.mut_kw);
        const identififer = try self.expect(.identifier);

        _ = try self.expect(.indent);
        const body = try self.parseBlock();
        _ = try self.expect(.dedent);

        const has_else = self.match(.else_kw);
        var else_block: ?ast.Block = null;

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

    fn parseStructDef(self: *Self) ParserError!ast.StructDef {
        _ = try self.expect(.indent);
        var static_fields: std.ArrayList(ast.StructField) = .empty;
        var fields: std.ArrayList(ast.StructField) = .empty;
        var funcs: std.ArrayList(ast.StructFn) = .empty;

        var section: enum { static, field, func } = .static;

        while (true) {
            const tk = self.peekCurrent();
            switch (tk.tag) {
                //note: this is allowing define fn with let and it should not
                .let_kw => {
                    if (section != .static) {
                        return self.err_dispatcher.invalidSyntax("static fields to be at the beginning of the struct", tk);
                    }
                    const let_stmt = try self.parseLetStmt();
                    try static_fields.append(self.allocator, .{
                        .identifier = let_stmt.identifier,
                        .is_mut = let_stmt.is_mut,
                        .type = let_stmt.type_annotation,
                        .default_value = let_stmt.value,
                    });
                },
                .identifier => {
                    self.walk();
                    const identifier = tk;
                    const has_type_annotation = self.match(.colon);
                    const is_fn = self.match(.l_paren);
                    const has_default_value = self.match(.eq);
                    if (has_default_value) {
                        if (self.match(.l_paren)) {
                            return self.err_dispatcher.invalidSyntax("'colon'", self.peekBack());
                        }
                        self.walkBack();
                    }

                    if (is_fn) {
                        if (has_type_annotation == false) {
                            return self.err_dispatcher.invalidSyntax("'colon'", self.peekBack());
                        }
                        section = .func;
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
                    } else {
                        if (section == .func) {
                            return self.err_dispatcher.invalidSyntax("instance fields to be before methods", tk);
                        }
                        section = .field;
                        const strc_field = try self.parseStructField(identifier.value(self.src), has_type_annotation);
                        try fields.append(self.allocator, strc_field);
                    }
                },
                .new_line => {
                    self.walk();
                },
                .dedent, .eof => {
                    self.walk();
                    break;
                },
                else => {
                    return self.err_dispatcher.invalidSyntax("let or an identifier", tk);
                },
            }
        }

        return ast.StructDef{
            .funcs = try funcs.toOwnedSlice(self.allocator),
            .fields = try fields.toOwnedSlice(self.allocator),
            .static_fields = try static_fields.toOwnedSlice(self.allocator),
        };
    }

    fn parseAnonymousStructDef(self: *Self) ParserError!ast.AnonymousStructDef {
        _ = try self.expect(.indent);
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
                .new_line => {
                    self.walk();
                },
                .dedent, .eof => {
                    self.walk();
                    break;
                },
                else => {
                    return self.err_dispatcher.invalidSyntax("identifier", tk);
                },
            }
        }

        return ast.AnonymousStructDef{
            .fields = try fields.toOwnedSlice(self.allocator),
        };
    }

    fn parseFnDef(self: *Self) ParserError!ast.FnDef {
        var arguments: std.ArrayList(ast.FnParam) = .empty;
        var body_block: ast.Block = undefined;
        var return_type: ?*ast.TypeAnnotation = null;

        _ = self.match(.indent);
        if (self.peekCurrent().tag != .r_paren) {
            arguments = try self.parseFnArgs();
        }
        _ = self.match(.dedent);

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
            return_type = try self.parseTypeAnnotation(false);
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

    //note: return std.ArrayList
    fn parseFnArgs(self: *Self) ParserError!std.ArrayList(ast.FnParam) {
        var arguments: std.ArrayList(ast.FnParam) = .empty;
        while (true) {
            _ = self.match(.new_line);
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

        //note: this first_run seems to be dumb
        //i think we can just take the tk as var out but i'm not changing this now
        var first_run: bool = true;
        while (true) {
            _ = self.match(.new_line);
            const first_tk = self.peekCurrent();
            if (first_tk.tag == .dedent) {
                break;
            }

            const exp = try self.parseExp(0);
            const tk = self.peekCurrent();

            if (first_run and tk.tag == .eq) {
                are_arguments_named = true;
            }
            first_run = false;

            if (are_arguments_named) {
                if (tk.tag != .eq) {
                    return self.err_dispatcher.invalidSyntax("=", tk);
                }

                if (exp.data != .identifier) {
                    return self.err_dispatcher.invalidSyntax("identifier", first_tk);
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

    fn parseReturnStmt(self: *Self) ParserError!ast.ReturnStmt {
        self.walk();
        const cur_tk = self.peekCurrent();

        if (cur_tk.tag == .new_line or cur_tk.tag == .dedent or cur_tk.tag == .eof) {
            return ast.ReturnStmt{ .exp = null };
        }

        return ast.ReturnStmt{ .exp = try self.parseExp(0) };
    }

    fn parsePipeOperator(self: *Self, exp: *ast.ExpNode) !*ast.ExpNode {
        self.walk();
        const right = try self.parseExp(self.getBindingPower(.l_paren));

        if (right.data != .fn_call) {
            //note: is not actually a "type" error is it?
            return self.err_dispatcher.invalidType("function call", @tagName(right.data), right.loc_start);
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

    fn parseExp(self: *Self, min_bp: u8) ParserError!*ast.ExpNode {
        var exp = try self.parsePrefix();
        exp = try self.parsePostfix(exp);

        while (true) {
            var tk = self.peekCurrent();
            if (tk.tag == .indent or tk.tag == .new_line) {
                self.walk();
            }

            tk = self.peekCurrent();
            const op_bp = self.getBindingPower(tk.tag);
            if (op_bp <= min_bp) {
                const previous_tk = self.peekBack();
                if (previous_tk.tag == .indent or previous_tk.tag == .new_line) {
                    self.walkBack();
                }
                break;
            }

            tk = self.peekCurrent();
            if (tk.tag == .pipe) {
                exp = try self.parsePipeOperator(exp);
                continue;
            }

            //if dont break or continue it means it's a binary op
            const op_token = self.peekCurrent();
            const op = try ast.BinaryOp.fromTag(op_token.tag);

            self.walk();
            const right = try self.parseExp(op_bp);

            tk = self.peekCurrent();
            exp = try ast.ExpNode.init(self.allocator, .{
                .data = .{
                    .binary_exp = .{
                        .left = exp,
                        .op = op,
                        .right = right,
                    },
                },
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
            .float_literal => {
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
            .null_literal => {
                return ast.ExpNode.init(self.allocator, .{ .data = .{ .null_literal = {} }, .loc_start = tk.loc.start });
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
                return ast.ExpNode.init(self.allocator, .{
                    .data = .{ .fn_def = try self.parseFnDef() },
                    .loc_start = tk.loc.start,
                });
            },
            .at => {
                return ast.ExpNode.init(self.allocator, .{
                    .data = .{
                        .anonymous_struct_inicialization = {},
                    },
                    .loc_start = tk.loc.start,
                });
            },
            .struct_kw => {
                return ast.ExpNode.init(self.allocator, .{ .data = .{ .struct_def = try self.parseStructDef() }, .loc_start = tk.loc.start });
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
            if (tk.tag == .new_line or tk.tag == .indent) {
                return false;
            }
            self.walk();
            tokens_scanned += 1;
        }

        return false;
    }

    fn isFuncDef(self: *Self) bool {
        const start_id = self.cur_index;
        const tokens_scan_limit = 500;
        var tokens_scanned: usize = 0;
        defer self.cur_index = start_id;

        var paren_depth: usize = 1;
        while (paren_depth != 0 and tokens_scanned < tokens_scan_limit) {
            switch (self.peekCurrent().tag) {
                .l_paren => {
                    paren_depth += 1;
                },
                .r_paren => {
                    paren_depth -= 1;
                },
                else => {},
            }
            self.walk();
            tokens_scanned += 1;
        }

        const tk = self.peekCurrent();
        return tk.tag == .arrow or tk.tag == .return_kw;
    }

    fn parsePostfix(self: *Self, left: *ast.ExpNode) ParserError!*ast.ExpNode {
        var node = left;

        while (true) {
            const tk = self.peekCurrent();

            //todo: should be a switch no?
            //fnCall
            if (tk.tag == .l_paren) {
                self.walk();
                _ = self.match(.indent);
                const args = try self.parseFnCallArgs();
                _ = self.match(.dedent);
                _ = try self.expect(.r_paren);

                const call_node = try ast.ExpNode.init(self.allocator, .{
                    .data = .{
                        .fn_call = .{
                            .are_arguments_named = args.len > 0 and args[0].identifier != null,
                            .target = node,
                            .arguments = args,
                        },
                    },
                    .loc_start = node.loc_start,
                });

                node = call_node;
                //note: this is supporting only simple fn calls
                continue;
            }

            //arrays
            if (tk.tag == .l_bracket) {
                self.walk();

                const index = try self.parseExp(0);
                _ = try self.expect(.r_bracket);

                const index_node = try ast.ExpNode.init(self.allocator, .{
                    .data = .{ .indexed = .{
                        .target = node,
                        .index = index,
                    } },
                    .loc_start = node.loc_start,
                });

                node = index_node;
                continue;
            }

            //struct field member access
            if (tk.tag == .dot) {
                self.walk();

                const id_token = try self.expect(.identifier);
                const id_node = try ast.ExpNode.init(self.allocator, .{
                    .data = .{ .identifier = id_token.value(self.src) },
                    .loc_start = id_token.loc.start,
                });

                const index_node = try ast.ExpNode.init(self.allocator, .{ .data = .{
                    .indexed = .{
                        .target = node,
                        .index = id_node,
                    },
                }, .loc_start = node.loc_start });

                node = index_node;
                continue;
            }

            //nullable struct field member access
            if (tk.tag == .question_mark_dot) {
                self.walk();

                const id_token = try self.expect(.identifier);
                const id_node = try ast.ExpNode.init(self.allocator, .{
                    .data = .{ .identifier = id_token.value(self.src) },
                    .loc_start = id_token.loc.start,
                });

                const index_node = try ast.ExpNode.init(self.allocator, .{
                    .data = .{
                        .nullable_indexed = .{
                            .target = node,
                            .index = id_node,
                        },
                    },
                    .loc_start = node.loc_start,
                });

                node = index_node;
                continue;
            }

            break;
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
            .star, .slash, .percent => 50,
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
            .string_kw, .int_kw, .float_kw, .bool_kw, .fn_kw, .void_kw => ast.TypeAnnotation.init(self.allocator, .{
                .type = .{ .primitive = tk.value(self.src) },
                .nullable = is_nullable,
            }),
            .identifier => ast.TypeAnnotation.init(self.allocator, .{
                .type = .{ .struct_ = tk.value(self.src) },
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

const Token = lexer.Token;
const Tag = lexer.Tag;
const Error = err.ErrorDispatcher;
const ParserError = err.Errors;

const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const err = @import("error.zig");
const std = @import("std");
