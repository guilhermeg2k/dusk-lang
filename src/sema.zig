pub const SemaAnalyzer = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    src: []const u8 = "",

    functions: std.ArrayList(ir.Func),
    structs: std.ArrayList(ir.Struct),

    scope: Scope,
    err_dispatcher: err.ErrorDispatcher,

    type_number: *Type,
    type_str: *Type,
    type_bool: *Type,
    type_func_def: *Type,
    type_struct_def: *Type,
    type_void: *Type,
    type_dynamic: *Type,

    pub fn init(allocator: std.mem.Allocator) !Self {
        const void_type = try Type.init(allocator, .void);
        return Self{
            .allocator = allocator,
            .scope = try Scope.init(allocator, void_type),
            .err_dispatcher = .{ .allocator = allocator, .src = "" },
            .functions = .empty,
            .structs = .empty,

            //warn: change this to static outside the struct
            .type_number = try Type.init(allocator, .number),
            .type_str = try Type.init(allocator, .string),
            .type_bool = try Type.init(allocator, .boolean),
            .type_func_def = try Type.init(allocator, .function_def),
            .type_struct_def = try Type.init(allocator, .struct_def),
            .type_void = void_type,
            .type_dynamic = try Type.init(allocator, .dynamic),
        };
    }

    pub fn analyze(self: *Self, root: *const ast.Root, src: []const u8) !ir.Program {
        self.src = src;
        self.err_dispatcher.src = src;

        try self.hoistFunctionsAndStructs(root);

        return .{
            .instructions = try self.visitBlock(root),
            .functions = try self.functions.toOwnedSlice(self.allocator),
            .structs = try self.structs.toOwnedSlice(self.allocator),
        };
    }

    pub fn hoistFunctionsAndStructs(self: *Self, root: *const ast.Root) !void {
        for (root.statements.items) |stmt| {
            if (stmt.data != .let_stmt) {
                continue;
            }

            const let_stmt = stmt.data.let_stmt;
            const expression_value = try switch (let_stmt.value.*.data) {
                .fn_def => ir.Value.init(self.allocator, .{ .fn_def = {} }),
                .struct_def => ir.Value.init(self.allocator, .{ .struct_def = {} }),
                else => {
                    continue;
                },
            };

            const expression_type = self.resolveValueType(expression_value);

            var var_type: *Type = expression_type;
            if (let_stmt.type_annotation) |type_annotation| {
                var_type = self.resolveTypeAnnotation(type_annotation) catch {
                    return self.err_dispatcher.typeNotDefined(type_annotation.name, stmt.loc_start);
                };
            }

            if (!expression_type.eql(var_type)) {
                return self.err_dispatcher.invalidType(
                    try var_type.name(self.allocator),
                    try expression_type.name(self.allocator),
                    stmt.loc_start,
                );
            }

            if (self.type_struct_def.eql(var_type)) {
                const struct_symbol = try self.visitStructSymbol(
                    let_stmt.identifier,
                    let_stmt.value,
                );
                try self.scope.symbol_table.put(struct_symbol);
            }

            if (self.type_func_def.eql(var_type)) {
                const fn_symbol = try self.visitFnSymbol(let_stmt.identifier, let_stmt.value);
                try self.scope.symbol_table.put(fn_symbol);
            }
        }
    }

    fn visitStructSymbol(self: *Self, identifier: []const u8, exp: *const ast.ExpNode) !*Symbol {
        const struct_def = exp.data.struct_def;
        var fields_in_order: std.ArrayList(TypedIdentifier) = .empty;
        var fields: std.StringHashMap(*Type) = .init(self.allocator);
        var funcs: std.StringHashMap(FuncMetadata) = .init(self.allocator);

        for (struct_def.fields) |field| {
            const field_type = try self.resolveTypeAnnotation(field.type);
            try fields_in_order.append(
                self.allocator,
                .{
                    .identifier = field.identifier,
                    .type = field_type,
                },
            );
            try fields.put(field.identifier, field_type);
        }

        for (struct_def.funcs) |func| {
            const fn_symbol = try self.visitFnSymbol(func.identifier, func.def);
            try funcs.put(func.identifier, fn_symbol.type.function);
        }

        return Symbol.init(
            self.allocator,
            .{
                .identifier = identifier,
                .uid = self.scope.genUid(),
                //warn: mut setting by hand to false
                .is_mut = false,
                .type = try Type.init(self.allocator, .{ .struct_ = .{
                    .identifier = identifier,
                    .fields_in_order = try fields_in_order.toOwnedSlice(self.allocator),
                    .fields = fields,
                    .functions = funcs,
                } }),
            },
        );
    }

    fn visitFnSymbol(self: *Self, identifier: []const u8, exp: *ast.ExpNode) !*Symbol {
        const fn_def = exp.data.fn_def;
        var return_type: *Type = undefined;
        var params_metadata: std.ArrayList(TypedIdentifier) = .empty;

        //create a temporary scope just for inline return type inference
        try self.scope.enter(self.type_void);

        for (fn_def.params) |arg| {
            const arg_type = self.resolveTypeAnnotation(arg.type_annotation) catch {
                return self.err_dispatcher.typeNotDefined(
                    arg.type_annotation.name,
                    exp.loc_start,
                );
            };

            self.scope.symbol_table.put(
                try Symbol.init(self.allocator, .{
                    .uid = self.scope.genUid(),
                    .identifier = arg.identifier,
                    .is_mut = arg.is_mut,
                    .type = arg_type,
                }),
            ) catch {
                return self.err_dispatcher.alreadyDefined(arg.identifier, exp.loc_start);
            };

            try params_metadata.append(self.allocator, .{ .identifier = arg.identifier, .type = arg_type });
        }

        //warn:  need to catch the else case ?
        if (fn_def.return_type) |r_type| {
            return_type = self.resolveTypeAnnotation(r_type) catch {
                return self.err_dispatcher.typeNotDefined(r_type.name, exp.loc_start);
            };
        } else if (fn_def.body_block.statements.items[0].data.return_stmt.exp) |return_exp| {
            const return_value = try self.evalExp(return_exp);
            return_type = self.resolveValueType(return_value);
        }

        //restore main scope
        self.scope.exit(self.type_void);

        return Symbol.init(self.allocator, .{
            .identifier = identifier,
            .uid = self.scope.genUid(),
            //warn: mut setting by hand to false
            .is_mut = false,
            .type = try Type.init(self.allocator, .{
                .function = .{
                    .identifier = identifier,
                    .params = try params_metadata.toOwnedSlice(self.allocator),
                    .return_type = return_type,
                },
            }),
        });
    }

    pub fn visitBlock(self: *Self, block: *const ast.Block) ![]const ir.Instruction {
        var instructions: std.ArrayList(ir.Instruction) = .empty;
        try self.scope.enter(self.scope.return_type);
        defer self.scope.exit(self.scope.return_type);

        for (block.statements.items) |stmt| {
            const instruction = switch (stmt.data) {
                .let_stmt => try self.visitLetStmt(&stmt),
                .assign_stmt => try self.visitAssignStmt(&stmt),
                .if_stmt => try self.visitIfStmt(&stmt),
                .for_stmt => try self.visitForStmt(&stmt),
                .expression_stmt => try self.visitExpressionStmt(&stmt),
                .break_stmt => ir.Instruction{ .break_stmt = {} },
                .continue_stmt => ir.Instruction{ .continue_stmt = {} },
                .return_stmt => try self.visitReturnStmt(&stmt),
            };

            if (instruction) |i| {
                try instructions.append(self.allocator, i);
            }
        }

        return instructions.toOwnedSlice(self.allocator);
    }

    fn visitExpressionStmt(self: *Self, stmt: *const ast.StatementNode) !ir.Instruction {
        const expression_stmt = stmt.data.expression_stmt;

        if (expression_stmt.data != .fn_call) {
            return self.err_dispatcher.invalidType("function", @tagName(expression_stmt.data), stmt.loc_start);
        }

        return ir.Instruction{
            .expression_stmt = .{ .value = try self.evalFnCall(&expression_stmt.data.fn_call, stmt.loc_start) },
        };
    }

    fn visitLetStmt(self: *Self, stmt: *const ast.StatementNode) !?ir.Instruction {
        var var_type: *Type = undefined;
        const let_stmt = stmt.data.let_stmt;
        const expression_value = try self.evalExp(let_stmt.value);
        const expression_type = self.resolveValueType(expression_value);

        if (let_stmt.type_annotation) |type_annotation| {
            var_type = try self.resolveTypeAnnotation(type_annotation);
        } else {
            var_type = expression_type;
        }

        if (self.type_func_def.eql(var_type)) {
            const fn_symbol = self.scope.symbol_table.getOrThrow(let_stmt.identifier) catch {
                return self.err_dispatcher.notDefined(let_stmt.identifier, stmt.loc_start);
            };
            const func = try self.visitFnDef(let_stmt.value, fn_symbol.uid, fn_symbol.type.function, null);
            try self.functions.append(self.allocator, func);
            return null;
        }

        if (self.type_struct_def.eql(var_type)) {
            const struct_def = try self.visitStructDef(let_stmt.value, let_stmt.identifier);
            try self.structs.append(self.allocator, struct_def);
            return null;
        }

        const uid = self.scope.genUid();

        try self.scope.symbol_table.put(try Symbol.init(
            self.allocator,
            .{
                .identifier = let_stmt.identifier,
                .uid = uid,
                .is_mut = let_stmt.is_mut,
                .type = var_type,
            },
        ));

        return ir.Instruction{ .store_var = .{
            .uid = uid,
            .identifier = let_stmt.identifier,
            .type = var_type,
            .value = expression_value,
        } };
    }

    fn visitIfStmt(self: *Self, stmt: *const ast.StatementNode) Errors!ir.Instruction {
        const if_stmt = stmt.data.if_stmt;
        const condition_value = try self.evalExp(if_stmt.condition);
        const condition_value_type = self.resolveValueType(condition_value);
        if (!condition_value_type.eql(self.type_bool)) {
            return self.err_dispatcher.invalidType("boolean", try condition_value_type.name(self.allocator), stmt.loc_start);
        }

        const then_block = try self.visitBlock(&if_stmt.then_block);

        var else_block: std.ArrayList(ir.Instruction) = .empty;
        if (if_stmt.else_block) |else_blc| {
            try else_block.appendSlice(self.allocator, try self.visitBlock(&else_blc));
        }

        return ir.Instruction{ .branch_if = .{
            .condition = condition_value,
            .then_block = then_block,
            .else_block = try else_block.toOwnedSlice(self.allocator),
        } };
    }

    fn visitAssignStmt(self: *Self, stmt: *const ast.StatementNode) !ir.Instruction {
        const assign_stmt = stmt.data.assign_stmt;

        switch (assign_stmt.target.data) {
            .identifier => {
                const id = assign_stmt.target.data.identifier;
                const id_symbol = self.scope.symbol_table.getOrThrow(id) catch {
                    return self.err_dispatcher.notDefined(id, stmt.loc_start);
                };

                const assignment_value = try self.evalExp(assign_stmt.exp);
                const assignment_value_type = self.resolveValueType(assignment_value);

                if (!id_symbol.is_mut) {
                    return self.err_dispatcher.notMutable(id, stmt.loc_start);
                }

                if (!id_symbol.type.eql(assignment_value_type)) {
                    return self.err_dispatcher.invalidType(
                        try id_symbol.type.name(self.allocator),
                        try assignment_value_type.name(self.allocator),
                        stmt.loc_start,
                    );
                }

                return ir.Instruction{ .update_var = .{
                    .identifier = id_symbol.identifier,
                    .var_uid = id_symbol.uid,
                    .value = assignment_value,
                } };
            },
            .indexed => {
                //warn: don't supporting struct indexing
                const index_exp = assign_stmt.target.data.indexed;
                const target = try self.evalExp(index_exp.target);
                const target_type = self.resolveValueType(target);

                if (target.* != .identifier) {
                    return self.err_dispatcher.invalidExpression(
                        "identifier",
                        @tagName(target.*),
                        assign_stmt.exp.loc_start,
                    );
                }

                if (target_type.* != .array) {
                    return self.err_dispatcher.invalidType(
                        "array",
                        try target_type.name(self.allocator),
                        assign_stmt.exp.loc_start,
                    );
                }

                const id = index_exp.target.data.identifier;
                const target_symbol = self.scope.symbol_table.getOrThrow(id) catch {
                    return self.err_dispatcher.notDefined(id, stmt.loc_start);
                };

                if (!target_symbol.is_mut) {
                    return self.err_dispatcher.notMutable(id, stmt.loc_start);
                }

                const assignment_value = try self.evalExp(assign_stmt.exp);
                const assignment_value_type = self.resolveValueType(assignment_value);

                if (!target_symbol.type.array.eql(assignment_value_type)) {
                    return self.err_dispatcher.invalidType(
                        try target_symbol.type.name(self.allocator),
                        try assignment_value_type.name(self.allocator),
                        stmt.loc_start,
                    );
                }

                const target_exp = try self.evalExp(index_exp.target);
                const index = try self.evalExp(index_exp.index);

                return ir.Instruction{
                    .update_indexed = .{
                        .target = target_exp,
                        .index = index,
                        .value = assignment_value,
                    },
                };
            },
            else => {
                return self.err_dispatcher.invalidAssignment(
                    "identifier or identifier[n]",
                    @tagName(assign_stmt.target.data),
                    stmt.loc_start,
                );
            },
        }
    }

    fn visitForStmt(self: *Self, stmt: *const ast.StatementNode) Errors!ir.Instruction {
        const for_stmt = stmt.data.for_stmt;

        const condition_value = try self.evalExp(for_stmt.condition);
        const condition_value_type = self.resolveValueType(condition_value);

        if (!condition_value_type.eql(self.type_bool)) {
            return self.err_dispatcher.invalidType(
                "boolean",
                try condition_value_type.name(self.allocator),
                stmt.loc_start,
            );
        }

        const block = try self.visitBlock(&for_stmt.do_block);

        return ir.Instruction{ .loop = .{
            .condition = condition_value,
            .do_block = block,
        } };
    }

    fn visitStructDef(self: *Self, exp: *const ast.ExpNode, identifier: []const u8) Errors!ir.Struct {
        const struct_symbol = try self.scope.symbol_table.getOrThrow(identifier);
        const prev_return_type = self.scope.return_type;
        try self.scope.enter(self.type_void);
        defer self.scope.exit(prev_return_type);

        const struct_def = exp.data.struct_def;
        var fields: std.ArrayList(ir.StructField) = .empty;
        var funcs: std.ArrayList(ir.Func) = .empty;

        for (struct_def.fields) |field| {
            const field_type = try self.resolveTypeAnnotation(field.type);
            try fields.append(
                self.allocator,
                .{
                    .identifier = field.identifier,
                    .type = field_type,
                },
            );
        }

        for (struct_def.funcs) |func| {
            const fn_metadata = if (struct_symbol.type.struct_.functions.get(func.identifier)) |metadata| metadata else unreachable;
            const fn_name = try self.allocPrintStructFnIdentifier(struct_symbol.identifier, fn_metadata.identifier);
            const fn_def = try self.visitFnDef(func.def, struct_symbol.uid, fn_metadata, fn_name);
            try funcs.append(self.allocator, fn_def);
        }

        return ir.Struct{
            .uid = struct_symbol.uid,
            .identifier = identifier,
            .fields = try fields.toOwnedSlice(self.allocator),
            .funcs = try funcs.toOwnedSlice(self.allocator),
        };
    }

    fn allocPrintStructFnIdentifier(self: *Self, struct_name: []const u8, fn_name: []const u8) ![]const u8 {
        return std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ struct_name, fn_name });
    }

    fn visitFnDef(
        self: *Self,
        exp: *const ast.ExpNode,
        uid: usize,
        metadata: FuncMetadata,
        override_name: ?[]const u8,
    ) Errors!ir.Func {
        const fn_def = exp.data.fn_def;
        var params: std.ArrayList(ir.FuncParam) = .empty;

        const old_return_type = self.scope.return_type;
        try self.scope.enter(metadata.return_type);
        defer self.scope.exit(old_return_type);

        for (metadata.params, 0..) |param, i| {
            const param_default_value: ?*ir.Value = null;

            //warn: not evaluating deafult values
            // if (param.default_value) |default_value| {
            //     param_default_value = try self.evalExp(default_value);
            //     const default_value_type = self.resolveValueType(param_default_value.?);
            //
            //     if (!param.type.eql(default_value_type)) {
            //         return self.err_dispatcher.invalidType(
            //             try param.type.name(self.allocator),
            //             try default_value_type.name(self.allocator),
            //             default_value.loc_start,
            //         );
            //     }
            // }

            const arg_uid = self.scope.genUid();

            try self.scope.symbol_table.put(try Symbol.init(
                self.allocator,
                .{
                    .uid = arg_uid,
                    .identifier = param.identifier,
                    .is_mut = fn_def.params[i].is_mut,
                    .type = param.type,
                },
            ));

            try params.append(self.allocator, .{
                .uid = arg_uid,
                .identifier = param.identifier,
                .type = param.type,
                .default_value = param_default_value,
            });
        }

        const body = try self.visitBlock(&fn_def.body_block);

        return ir.Func{
            .uid = uid,
            .identifier = if (override_name) |name| name else metadata.identifier,
            .params = try params.toOwnedSlice(self.allocator),
            .return_type = metadata.return_type,
            .body = body,
        };
    }

    fn visitReturnStmt(self: *Self, stmt: *const ast.StatementNode) !ir.Instruction {
        const return_stmt = stmt.data.return_stmt;

        if (return_stmt.exp) |exp| {
            const exp_value = try self.evalExp(exp);
            const exp_value_type = self.resolveValueType(exp_value);

            if (!exp_value_type.eql(self.scope.return_type)) {
                return self.err_dispatcher.invalidType(
                    try self.scope.return_type.name(self.allocator),
                    try exp_value_type.name(self.allocator),
                    stmt.loc_start,
                );
            }

            return ir.Instruction{ .return_stmt = .{
                .value = exp_value,
            } };
        }

        if (!self.scope.return_type.eql(self.type_void)) {
            return self.err_dispatcher.invalidType(
                try self.scope.return_type.name(self.allocator),
                "void",
                stmt.loc_start,
            );
        }

        return ir.Instruction{ .return_stmt = .{
            .value = null,
        } };
    }

    fn evalExp(self: *Self, exp: *const ast.ExpNode) Errors!*ir.Value {
        switch (exp.*.data) {
            .number_literal => {
                return ir.Value.init(self.allocator, .{ .i_float = exp.data.number_literal });
            },
            .string_literal => {
                return ir.Value.init(self.allocator, .{ .i_string = exp.data.string_literal });
            },
            .bool_literal => {
                return ir.Value.init(self.allocator, .{ .i_bool = exp.data.bool_literal });
            },
            .fn_def => {
                return ir.Value.init(self.allocator, .{ .fn_def = {} });
            },
            .array_literal => {
                return self.evalArrayLiteral(exp);
            },
            .identifier => {
                return self.evalIdentifier(exp);
            },
            .fn_call => {
                return self.evalFnCall(&exp.data.fn_call, exp.loc_start);
            },
            .struct_def => {
                return ir.Value.init(self.allocator, .{ .struct_def = {} });
            },
            .indexed => {
                return self.evalIndexedExp(exp);
            },
            .unary_exp => {
                return self.evalUnaryExp(exp);
            },
            .binary_exp => {
                return self.evalBinaryExp(exp);
            },
        }
    }

    fn evalFnCall(self: *Self, fn_call: *const ast.FnCall, loc_start: usize) !*ir.Value {
        var fn_identifier: []const u8 = undefined;
        var uid: usize = undefined;
        var params: []const TypedIdentifier = undefined;
        var return_type: *Type = undefined;

        switch (fn_call.target.data) {
            .identifier => |id| {
                const symbol = self.scope.symbol_table.getOrThrow(id) catch {
                    return self.err_dispatcher.notDefined(id, loc_start);
                };

                const is_fn = symbol.type.* == .function;
                const is_struct = symbol.type.* == .struct_;

                if (!is_fn and !is_struct) {
                    return self.err_dispatcher.invalidType(
                        "function or struct",
                        try symbol.type.name(self.allocator),
                        loc_start,
                    );
                }

                //when identifier is a struct when turn into its a struct inicialization
                fn_identifier = symbol.identifier;
                uid = symbol.uid;
                params = if (is_fn) symbol.type.function.params else symbol.type.struct_.fields_in_order;
                return_type = if (is_fn) symbol.type.function.return_type else try Type.init(self.allocator, .{
                    .struct_ = symbol.type.struct_,
                });
            },
            .indexed => |indexed| {
                const target = try self.evalExp(indexed.target);
                const target_type = self.resolveValueType(target);

                if (target_type.* != .struct_) {
                    return self.err_dispatcher.invalidType(
                        "a struct",
                        try target_type.name(self.allocator),
                        loc_start,
                    );
                }

                const fn_name = indexed.index.data.identifier;

                const fn_metadata = target_type.struct_.functions.get(fn_name) orelse {
                    return self.err_dispatcher.invalidStructFunction(
                        target_type.struct_.identifier,
                        fn_name,
                        loc_start,
                    );
                };

                const target_symbol = try self.scope.symbol_table.getOrThrow(target_type.struct_.identifier);

                params = fn_metadata.params;
                return_type = fn_metadata.return_type;
                uid = target_symbol.uid;
                fn_identifier = try self.allocPrintStructFnIdentifier(target_symbol.identifier, fn_metadata.identifier);
            },
            else => unreachable,
        }

        var fn_call_arguments_values: std.ArrayList(*ir.Value) = .empty;

        var arg_exp_by_param_name = std.StringHashMap(*ast.ExpNode).init(self.allocator);
        defer arg_exp_by_param_name.deinit();

        const call_args_len = fn_call.arguments.len;
        const fn_params_len = params.len;

        if (fn_params_len != call_args_len) {
            return self.err_dispatcher.invalidNumberOfArgs(fn_params_len, call_args_len, loc_start);
        }

        if (fn_call.are_arguments_named) {
            for (fn_call.arguments) |arg| {
                if (arg.identifier) |identifier| {
                    try arg_exp_by_param_name.put(identifier, arg.exp);
                } else unreachable;
            }
        }

        for (params, 0..) |param, i| {
            var arg_exp = fn_call.arguments[i].exp;

            if (fn_call.are_arguments_named) {
                const named_arg_exp = arg_exp_by_param_name.get(param.identifier);
                if (named_arg_exp) |exp| {
                    arg_exp = exp;
                } else {
                    return self.err_dispatcher.missingArgument(param.identifier, loc_start);
                }
            }

            const fn_call_arg_value = try self.evalExp(arg_exp);
            const arg_type = self.resolveValueType(fn_call_arg_value);

            if (!arg_type.eql(param.type)) {
                return self.err_dispatcher.invalidType(try param.type.name(self.allocator), try arg_type.name(self.allocator), loc_start);
            }

            try fn_call_arguments_values.append(self.allocator, fn_call_arg_value);
        }

        return ir.Value.init(
            self.allocator,
            .{
                .fn_call = .{
                    .fn_uid = uid,
                    .identifier = fn_identifier,
                    .return_type = return_type,
                    .args = try fn_call_arguments_values.toOwnedSlice(self.allocator),
                },
            },
        );
    }

    fn evalIndexedExp(self: *Self, exp: *const ast.ExpNode) !*ir.Value {
        const index_exp = exp.data.indexed;
        const target = try self.evalExp(index_exp.target);
        const target_type = self.resolveValueType(target);

        if (target.* != .identifier) {
            return self.err_dispatcher.invalidExpression("identifier", @tagName(target.*), exp.loc_start);
        }

        switch (target_type.*) {
            .array => {
                const index = try self.evalExp(index_exp.index);
                const index_type = self.resolveValueType(index);

                if (!index_type.eql(self.type_number)) {
                    return self.err_dispatcher.invalidIndexing("number", @tagName(index.*), exp.loc_start);
                }

                return ir.Value.init(self.allocator, .{
                    .indexed = .{
                        .target = target,
                        .index = index,
                    },
                });
            },
            .struct_ => {
                return switch (index_exp.index.*.data) {
                    .identifier => |struct_member| {
                        const struct_symbol = try self.scope.symbol_table.getOrThrow(target.identifier.identifier);
                        const field_member = struct_symbol.type.struct_.fields.get(struct_member);
                        const function_member = struct_symbol.type.struct_.functions.get(struct_member);

                        if (field_member == null and function_member == null) {
                            return self.err_dispatcher.invalidStructMember(target.identifier.identifier, struct_member, exp.loc_start);
                        }

                        return ir.Value.init(self.allocator, .{
                            .indexed = .{
                                .target = target,
                                .index = try ir.Value.init(self.allocator, .{ .i_string = struct_member }),
                            },
                        });
                    },
                    else => unreachable,
                };
            },

            else => {
                return self.err_dispatcher.invalidType("array or struct", try target_type.name(self.allocator), exp.loc_start);
            },
        }
    }

    fn evalIdentifier(self: *Self, exp: *const ast.ExpNode) !*ir.Value {
        const id = exp.data.identifier;
        const id_symbol = self.scope.symbol_table.get(id);

        if (id_symbol) |symbol| {
            return ir.Value.init(self.allocator, .{ .identifier = .{
                .uid = symbol.uid,
                .identifier = symbol.identifier,
                .type = symbol.type,
            } });
        }

        return self.err_dispatcher.notDefined(id, exp.loc_start);
    }

    fn evalArrayLiteral(self: *Self, exp: *const ast.ExpNode) !*ir.Value {
        var values: std.ArrayList(*ir.Value) = .empty;
        const array_literal = exp.data.array_literal;
        var array_literal_type: *Type = self.type_dynamic;

        for (array_literal.exps) |e| {
            const exp_value = try self.evalExp(e);
            const exp_value_type = self.resolveValueType(exp_value);

            if (array_literal_type.eql(self.type_void)) {
                array_literal_type = exp_value_type;
            }

            if (!exp_value_type.eql(array_literal_type)) {
                return self.err_dispatcher.invalidType(try array_literal_type.name(self.allocator), try exp_value_type.name(self.allocator), exp.loc_start);
            }

            try values.append(self.allocator, exp_value);
        }

        return ir.Value.init(
            self.allocator,
            .{
                .i_array = .{
                    .type = try Type.init(self.allocator, .{ .array = array_literal_type }),
                    .values = try values.toOwnedSlice(self.allocator),
                },
            },
        );
    }

    fn evalUnaryExp(self: *Self, exp: *const ast.ExpNode) !*ir.Value {
        const unary_exp = exp.data.unary_exp;
        const exp_value = try self.evalExp(unary_exp.right);
        const exp_type = self.resolveValueType(exp_value);

        switch (unary_exp.op) {
            .neg => {
                if (!exp_type.eql(self.type_number)) {
                    return self.err_dispatcher.invalidType(
                        "number",
                        try exp_type.name(self.allocator),
                        exp.loc_start,
                    );
                }
            },
            .not => {
                if (!exp_type.eql(self.type_bool)) {
                    return self.err_dispatcher.invalidType(
                        "boolean",
                        try exp_type.name(self.allocator),
                        exp.loc_start,
                    );
                }
            },
        }

        return ir.Value.init(self.allocator, .{ .unary_op = .{ .kind = self.astUnaryOpToIrUnaryOpKind(unary_exp.op), .type = exp_type, .right = exp_value } });
    }

    fn evalBinaryExp(self: *Self, exp: *const ast.ExpNode) !*ir.Value {
        const bin_exp = exp.data.binary_exp;
        const left_value = try self.evalExp(bin_exp.left);
        const right_value = try self.evalExp(bin_exp.right);
        const left_type = self.resolveValueType(left_value);
        const right_type = self.resolveValueType(right_value);

        var op_type: *Type = self.type_void;

        if (!left_type.eql(right_type)) {
            return self.err_dispatcher.invalidType(
                try left_type.name(self.allocator),
                try right_type.name(self.allocator),
                exp.loc_start,
            );
        }

        switch (bin_exp.op) {
            .add, .sub, .mult, .div, .mod => {
                if (!left_type.eql(self.type_number)) {
                    return self.err_dispatcher.invalidType(
                        "number",
                        try left_type.name(self.allocator),
                        exp.loc_start,
                    );
                }
                op_type = self.type_number;
            },

            //warn: this makes support string comparison by operators
            .eq, .not_eq, .lt, .lt_or_eq, .gt, .gt_or_eq => {
                if (left_type.eql(self.type_bool)) {
                    return self.err_dispatcher.invalidType(
                        "string, number",
                        try left_type.name(self.allocator),
                        exp.loc_start,
                    );
                }
                op_type = self.type_bool;
            },

            .bool_or, .bool_and => {
                if (!left_type.eql(self.type_bool)) {
                    return self.err_dispatcher.invalidType(
                        "boolean",
                        try left_type.name(self.allocator),
                        exp.loc_start,
                    );
                }
                op_type = self.type_bool;
            },
        }

        return ir.Value.init(self.allocator, .{ .binary_op = .{
            .kind = self.astBinOpToIrBinOpKind(bin_exp.op),
            .type = op_type,
            .left = left_value,
            .right = right_value,
        } });
    }

    //warn: not supporting structs
    pub fn resolveTypeAnnotation(self: *Self, type_annotation: *ast.TypeAnnotation) !*Type {
        switch (type_annotation.*) {
            .name => {
                if (std.mem.eql(u8, type_annotation.name, "number")) return self.type_number;
                if (std.mem.eql(u8, type_annotation.name, "string")) return self.type_str;
                if (std.mem.eql(u8, type_annotation.name, "bool")) return self.type_bool;
                if (std.mem.eql(u8, type_annotation.name, "fn")) return self.type_func_def;
                if (std.mem.eql(u8, type_annotation.name, "void")) return Type.init(self.allocator, .void);
                if (std.mem.eql(u8, type_annotation.name, "dynamic")) return self.type_dynamic;

                return Errors.SemaError;
            },
            .array => {
                const inner_type = try self.resolveTypeAnnotation(type_annotation.array);
                return Type.init(self.allocator, .{
                    .array = inner_type,
                });
            },
            .struct_self => {
                return Type.init(self.allocator, .struct_self);
            },
        }
    }

    pub fn resolveValueType(self: *Self, value: *ir.Value) *Type {
        //warn: func_def needs a better solving probably doing this when we have generics?
        //also missing struct
        return switch (value.*) {
            .i_float => self.type_number,
            .i_bool => self.type_bool,
            .i_string => self.type_str,
            .i_void => self.type_bool,
            .fn_def => self.type_func_def,
            .struct_def => self.type_struct_def,
            .identifier => value.identifier.type,
            .binary_op => value.binary_op.type,
            .unary_op => value.unary_op.type,
            .indexed => |indexed| {
                return switch (indexed.target.identifier.type.*) {
                    .array => |array| array,
                    .struct_ => {
                        if (indexed.target.identifier.type.struct_.fields.get(indexed.index.i_string)) |field_type| return field_type;
                        //warn: func_def?
                        if (indexed.target.identifier.type.struct_.functions.get(indexed.index.i_string) != null) return self.type_func_def;
                        unreachable;
                    },
                    else => unreachable,
                };
            },
            .i_array => value.i_array.type,
            .fn_call => value.fn_call.return_type,
        };
    }

    fn astBinOpToIrBinOpKind(_: *Self, bin_op: ast.BinaryOp) ir.BinaryOpKind {
        return switch (bin_op) {
            .add => .add,
            .sub => .sub,
            .mult => .mult,
            .div => .div,
            .mod => .mod,
            .eq => .cmp_eq,
            .not_eq => .cmp_neq,
            .lt => .cmp_lt,
            .lt_or_eq => .cmp_le,
            .gt => .cmp_gt,
            .gt_or_eq => .cmp_ge,
            .bool_or => .b_or,
            .bool_and => .b_and,
        };
    }

    fn astUnaryOpToIrUnaryOpKind(_: *Self, bin_op: ast.UnaryOp) ir.UnaryOpKind {
        return switch (bin_op) {
            .neg => .neg,
            .not => .not,
        };
    }
};

const Scope = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    symbol_table: *SymbolTable,
    return_type: *Type,
    next_uid: usize,

    pub fn init(alloc: std.mem.Allocator, return_type: *Type) !Self {
        const builtins = buildin.BuiltIn{
            .alloc = alloc,
        };

        var symbols = std.StringHashMap(*const Symbol).init(alloc);

        for (try builtins.generate()) |func| {
            try symbols.put(func.symbol.identifier, func.symbol);
        }

        return Self{
            .allocator = alloc,
            .symbol_table = try SymbolTable.init(alloc, null, symbols),
            .return_type = return_type,
            .next_uid = 10,
        };
    }

    pub fn genUid(self: *Self) usize {
        defer self.next_uid += 1;
        return self.next_uid;
    }

    pub fn enter(self: *Self, return_type: *Type) !void {
        self.return_type = return_type;
        self.symbol_table = try SymbolTable.init(self.allocator, self.symbol_table, null);
    }

    pub fn exit(self: *Self, return_type: *Type) void {
        self.return_type = return_type;
        if (self.symbol_table.parent) |parent_scope| self.symbol_table = parent_scope;
    }
};

const SymbolTable = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    parent: ?*SymbolTable,
    symbols: std.StringHashMap(*const Symbol),

    pub fn init(allocator: std.mem.Allocator, parent: ?*SymbolTable, symbols: ?std.StringHashMap(*const Symbol)) !*Self {
        const ptr = try allocator.create(Self);
        ptr.* = Self{
            .allocator = allocator,
            .parent = parent,
            .symbols = if (symbols) |s| s else std.StringHashMap(*const Symbol).init(allocator),
        };
        return ptr;
    }

    fn put(self: *Self, symbol: *Symbol) !void {
        if (self.symbols.get(symbol.identifier)) |_| {
            return Errors.SemaError;
        }

        try self.symbols.put(symbol.identifier, symbol);
    }

    fn remove(self: *Self, symbol: *Symbol) !void {
        if (self.symbols.get(symbol.identifier)) |_| {
            return Errors.SemaError;
        }

        try self.symbols.remove(symbol.identifier);
    }

    fn get(self: *Self, id: []const u8) ?*const Symbol {
        if (self.symbols.get(id)) |s| return s;
        if (self.parent) |parent| {
            return parent.get(id);
        }
        return null;
    }

    //warn: getOrError
    fn getOrThrow(self: *Self, id: []const u8) !*const Symbol {
        if (self.get(id)) |s| return s;
        return Errors.SemaError;
    }
};

pub const Symbol = struct {
    const Self = @This();

    uid: usize,
    identifier: []const u8,
    type: *Type,
    is_mut: bool,

    pub fn init(allocator: std.mem.Allocator, exp: Self) !*Self {
        const ptr = try allocator.create(Self);
        ptr.* = exp;
        return ptr;
    }
};

const StructMetadata = struct {
    identifier: []const u8,
    fields: std.StringHashMap(*Type),
    fields_in_order: []const TypedIdentifier,
    functions: std.StringHashMap(FuncMetadata),
};

const FuncMetadata = struct {
    identifier: []const u8,
    params: []const TypedIdentifier,
    return_type: *Type,
};

pub const TypedIdentifier = struct {
    identifier: []const u8,
    type: *Type,
};

pub const Type = union(enum) {
    const Self = @This();

    number,
    string,
    boolean,
    void,
    //currently only used for built-in functions
    dynamic,

    //warn: later both os this type should take the metadata
    function_def,
    struct_def,

    struct_: StructMetadata,
    function: FuncMetadata,

    struct_self,
    array: *Type,

    pub fn init(allocator: std.mem.Allocator, exp: Self) !*Self {
        const ptr = try allocator.create(Self);
        ptr.* = exp;
        return ptr;
    }

    pub fn eql(self: *Self, other: *Type) bool {
        if (other.* == .dynamic) return true;
        switch (self.*) {
            .number => return other.* == .number,
            .string => return other.* == .string,
            .boolean => return other.* == .boolean,
            .void => return other.* == .void,
            .dynamic => return true,

            //warn: this is wrong
            .function_def => return other.* == .function_def,
            .struct_def => return other.* == .struct_def,
            .struct_ => return true,
            .function => return true,

            .struct_self => unreachable,
            .array => |inner| {
                if (other.* != .array) return false;
                return inner.eql(other.array);
            },
        }
    }

    pub fn name(self: *Self, allocator: std.mem.Allocator) ![]const u8 {
        return switch (self.*) {
            .number => "number",
            .string => "string",
            .boolean => "boolean",
            .void => "void",
            .dynamic => "dynamic",
            .struct_def => "struct def",
            .function_def => "function def",
            .function => |metadata| {
                return std.fmt.allocPrint(allocator, "function {s}", .{
                    metadata.identifier,
                });
            },
            .struct_ => |metadata| {
                return std.fmt.allocPrint(allocator, "struct {s}", .{
                    metadata.identifier,
                });
            },
            .struct_self => "@",
            .array => |inner_type| {
                return std.fmt.allocPrint(allocator, "[]{s}", .{try inner_type.name(allocator)});
            },
        };
    }
};

pub const Errors = err.Errors;

const buildin = @import("built-in.zig");
const err = @import("error.zig");
const ir = @import("ir.zig");
const ast = @import("ast.zig");
const allocPrint = std.fmt.allocPrint;
const std = @import("std");
