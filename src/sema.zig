pub const SemaAnalyzer = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    src: []const u8 = "",

    functions: std.ArrayList(ir.Func),
    structs: std.ArrayList(ir.Struct),

    scope: Scope,
    err_dispatcher: err.ErrorDispatcher,

    type_float: *Type,
    type_int: *Type,
    type_str: *Type,
    type_bool: *Type,
    type_null: *Type,
    type_func_def: *Type,
    type_struct_def: *Type,
    type_void: *Type,
    type_dynamic: *Type,
    type_float_nullable: *Type,
    type_int_nullable: *Type,
    type_str_nullable: *Type,
    type_bool_nullable: *Type,
    type_null_nullable: *Type,
    type_func_def_nullable: *Type,
    type_struct_def_nullable: *Type,
    type_dynamic_nullable: *Type,

    pub fn init(allocator: std.mem.Allocator) !Self {
        const void_type = try Type.init(allocator, .{ .kind = .void, .nullable = false });

        return Self{
            .allocator = allocator,
            .scope = try Scope.init(allocator, void_type),
            .err_dispatcher = .{ .allocator = allocator, .src = "" },
            .functions = .empty,
            .structs = .empty,

            //note: change this to static outside the struct
            .type_float = try Type.init(allocator, .{ .kind = .float, .nullable = false }),
            .type_int = try Type.init(allocator, .{ .kind = .int, .nullable = false }),
            .type_str = try Type.init(allocator, .{ .kind = .string, .nullable = false }),
            .type_bool = try Type.init(allocator, .{ .kind = .boolean, .nullable = false }),
            .type_func_def = try Type.init(allocator, .{ .kind = .function_def, .nullable = false }),
            .type_struct_def = try Type.init(allocator, .{ .kind = .struct_def, .nullable = false }),
            .type_void = void_type,
            .type_dynamic = try Type.init(allocator, .{ .kind = .dynamic, .nullable = false }),
            .type_null = try Type.init(allocator, .{ .kind = .null, .nullable = false }),

            .type_int_nullable = try Type.init(allocator, .{ .kind = .int, .nullable = true }),
            .type_float_nullable = try Type.init(allocator, .{ .kind = .float, .nullable = true }),
            .type_str_nullable = try Type.init(allocator, .{ .kind = .string, .nullable = true }),
            .type_bool_nullable = try Type.init(allocator, .{ .kind = .boolean, .nullable = true }),
            .type_func_def_nullable = try Type.init(allocator, .{ .kind = .function_def, .nullable = true }),
            .type_struct_def_nullable = try Type.init(allocator, .{ .kind = .struct_def, .nullable = true }),
            .type_dynamic_nullable = try Type.init(allocator, .{ .kind = .dynamic, .nullable = true }),
            .type_null_nullable = try Type.init(allocator, .{ .kind = .null, .nullable = true }),
        };
    }

    pub fn analyze(self: *Self, root: *const ast.Root, src: []const u8) !ir.Program {
        self.src = src;
        self.err_dispatcher.src = src;

        try self.hoistFunctionsAndStructs(root);

        return .{
            .instructions = (try self.visitBlock(root)).instructions,
            .functions = try self.functions.toOwnedSlice(self.allocator),
            .structs = try self.structs.toOwnedSlice(self.allocator),
        };
    }

    pub fn hoistFunctionsAndStructs(self: *Self, root: *const ast.Root) !void {
        //first hoist only names
        for (root.statements.items) |stmt| {
            if (stmt.data != .let_stmt) {
                continue;
            }

            const let_stmt = stmt.data.let_stmt;
            if (let_stmt.value.data != .fn_def and let_stmt.value.data != .struct_def) {
                continue;
            }

            switch (let_stmt.value.data) {
                .fn_def => {
                    const symbol = try self.createIncompleteFuncSymbol(let_stmt.identifier);
                    //note: wrong loc_start
                    self.scope.symbol_table.put(symbol) catch return self.err_dispatcher.alreadyDefined(let_stmt.identifier, let_stmt.value.loc_start);
                },
                .struct_def => {
                    const symbol = try self.createIncompleteStructSymbol(let_stmt.identifier);
                    self.scope.symbol_table.put(symbol) catch return self.err_dispatcher.alreadyDefined(let_stmt.identifier, let_stmt.value.loc_start);
                },
                else => unreachable,
            }
        }

        //later complete the hoist fullfilling the types
        for (root.statements.items) |stmt| {
            if (stmt.data != .let_stmt) {
                continue;
            }

            const let_stmt = stmt.data.let_stmt;
            if (let_stmt.value.data != .fn_def and let_stmt.value.data != .struct_def) {
                continue;
            }

            switch (let_stmt.value.data) {
                .fn_def => {
                    const symbol = try self.scope.symbol_table.getOrThrow(let_stmt.identifier);
                    try self.fulfillFuncType(symbol, let_stmt.value, null);
                },
                .struct_def => {
                    const symbol = try self.scope.symbol_table.getOrThrow(let_stmt.identifier);
                    try self.fulfillStructType(
                        symbol,
                        let_stmt.value,
                    );
                },
                else => unreachable,
            }
        }
    }

    fn createIncompleteFuncSymbol(self: *Self, identifier: []const u8) !*Symbol {
        const symbol = try Symbol.init(self.allocator, .{
            .identifier = identifier,
            .uid = self.scope.genUid(),
            //note: mut setting by hand to false
            .is_mut = false,
            .type = undefined,
        });

        symbol.type = try Type.init(self.allocator, .{
            .kind = .{
                .function = .{
                    .symbol = symbol,
                    .params = &.{},
                    .return_type = undefined,
                },
            },
            .nullable = false,
        });

        return symbol;
    }

    fn createIncompleteStructSymbol(self: *Self, identifier: []const u8) !*Symbol {
        const symbol = try Symbol.init(
            self.allocator,
            .{
                .identifier = identifier,
                .uid = self.scope.genUid(),
                //note: mut setting by hand to false
                .is_mut = false,
                .type = undefined,
            },
        );

        var fields_in_order: std.ArrayList(TypedIdentifier) = .empty;

        symbol.type = try Type.init(self.allocator, .{
            .kind = .{ .struct_ = .{
                .identifier = identifier,
                .fields_in_order = try fields_in_order.toOwnedSlice(self.allocator),
                .fields = .init(self.allocator),
                .static_fields = .init(self.allocator),
                .functions = .init(self.allocator),
            } },
            .nullable = false,
        });

        return symbol;
    }

    fn createAnonymousStructFromFnCall(self: *Self, exp: *const ast.ExpNode, is_mut: bool) !AnonymousStruct {
        const fn_call = exp.data.fn_call;
        var fields_in_order: std.ArrayList(TypedIdentifier) = .empty;
        var fields: std.StringHashMap(TypedIdentifier) = .init(self.allocator);

        if (fn_call.are_arguments_named == false) {
            return self.err_dispatcher.cantInferAnonymousStruct(exp.loc_start);
        }

        for (fn_call.arguments) |arg| {
            const exp_value = try self.evalExp(arg.exp);
            const field_type = self.resolveValueType(exp_value);
            const field: TypedIdentifier = .{
                .identifier = arg.identifier.?,
                .is_mut = is_mut,
                .type = field_type,
                .default_value = exp_value,
            };
            try fields_in_order.append(
                self.allocator,
                field,
            );
            try fields.put(arg.identifier.?, field);
        }

        return AnonymousStruct{ .fields = fields, .fields_in_order = try fields_in_order.toOwnedSlice(self.allocator) };
    }

    fn fulfillStructType(self: *Self, struct_symbol: *Symbol, exp: *const ast.ExpNode) !void {
        const struct_def = exp.data.struct_def;
        var fields_in_order: std.ArrayList(TypedIdentifier) = .empty;
        var fields: std.StringHashMap(TypedIdentifier) = .init(self.allocator);
        var static_fields: std.StringHashMap(TypedIdentifier) = .init(self.allocator);
        var funcs: std.StringHashMap(FuncMetadata) = .init(self.allocator);

        for (struct_def.static_fields) |field| {
            const struct_field = try self.createStructField(field);
            try static_fields.put(field.identifier, struct_field);
        }

        for (struct_def.fields) |field| {
            const struct_field = try self.createStructField(field);
            try fields_in_order.append(
                self.allocator,
                struct_field,
            );
            try fields.put(field.identifier, struct_field);
        }

        for (struct_def.funcs) |func| {
            //note: maybe this is not optimal because we only need the fn metadata
            const fn_symbol = try self.createIncompleteFuncSymbol(func.identifier);
            try self.fulfillFuncType(fn_symbol, func.def, struct_symbol);
            try funcs.put(func.identifier, fn_symbol.type.kind.function);
        }

        struct_symbol.type.kind.struct_.fields_in_order = try fields_in_order.toOwnedSlice(self.allocator);
        struct_symbol.type.kind.struct_.fields = fields;
        struct_symbol.type.kind.struct_.static_fields = static_fields;
        struct_symbol.type.kind.struct_.functions = funcs;
    }

    fn createStructField(self: *Self, field: ast.StructField) Errors!TypedIdentifier {
        var field_type: *Type = undefined;
        var field_default_value: ?*ir.Value = null;

        if (field.type) |_type| {
            field_type = try self.resolveTypeAnnotation(_type);
        }

        if (field.default_value) |initial_value| {
            const value = try self.evalExp(initial_value);
            const value_type = self.resolveValueType(value);
            field_default_value = value;

            if (field.type) |_type| {
                if (!value_type.eql(field_type)) {
                    return self.err_dispatcher.invalidType(
                        try _type.value(self.allocator),
                        @tagName(value_type.kind),
                        field.default_value.?.loc_start,
                    );
                }
            }

            field_type = self.resolveValueType(value);
        }

        return TypedIdentifier{
            .identifier = field.identifier,
            .is_mut = field.is_mut,
            .type = field_type,
            .default_value = field_default_value,
        };
    }

    fn fulfillFuncType(self: *Self, fn_symbol: *Symbol, exp: *ast.ExpNode, struct_symbol: ?*Symbol) !void {
        const fn_def = exp.data.fn_def;
        var return_type: *Type = undefined;
        var params_metadata: std.ArrayList(TypedIdentifier) = .empty;

        //create a temporary scope just for inline return type inference
        try self.scope.enter(self.type_void);

        for (fn_def.params) |param| {
            var param_default_value: ?*ir.Value = null;
            var param_type: *Type = undefined;

            if (param.type_annotation) |annotation| {
                param_type = self.resolveTypeAnnotation(annotation) catch {
                    return self.err_dispatcher.typeNotDefined(
                        try param.type_annotation.?.value(self.allocator),
                        exp.loc_start,
                    );
                };

                if (param.default_value) |default_value| {
                    const value = try self.evalExp(default_value);
                    param_default_value = value;
                    const value_type = self.resolveValueType(value);
                    if (!value_type.eql(param_type)) {
                        return self.err_dispatcher.invalidType(
                            try param_type.name(self.allocator),
                            try value_type.name(self.allocator),
                            param.default_value.?.loc_start,
                        );
                    }
                }
            } else {
                if (param.default_value) |default_value| {
                    const value = try self.evalExp(default_value);
                    param_default_value = value;
                    param_type = self.resolveValueType(value);
                } else {
                    return self.err_dispatcher.invalidParameterType(param.identifier, exp.loc_start);
                }
            }

            if (param_type.kind == .struct_self and struct_symbol == null) {
                return self.err_dispatcher.selfCantBeUsedOutsideOfAstruct(exp.loc_start);
            }

            if (param_type.kind == .struct_self) {
                param_type = try Type.init(self.allocator, .{
                    .kind = .{
                        .struct_instance = &struct_symbol.?.type.kind.struct_,
                    },
                    .nullable = param.type_annotation.?.nullable,
                });
            }

            if (param_type.kind == .struct_) {
                param_type = try Type.init(self.allocator, .{
                    .kind = .{ .struct_instance = &param_type.kind.struct_ },
                    .nullable = param.type_annotation.?.nullable,
                });
            }

            self.scope.symbol_table.put(
                try Symbol.init(self.allocator, .{
                    .uid = self.scope.genUid(),
                    .identifier = param.identifier,
                    .is_mut = param.is_mut,
                    .type = param_type,
                }),
            ) catch {
                return self.err_dispatcher.alreadyDefined(param.identifier, exp.loc_start);
            };

            try params_metadata.append(self.allocator, .{
                .identifier = param.identifier,
                .type = param_type,
                .is_mut = param.is_mut,
                .default_value = param_default_value,
            });
        }

        //note:  need to catch the else case ?
        if (fn_def.return_type) |r_type| {
            return_type = self.resolveTypeAnnotation(r_type) catch {
                return self.err_dispatcher.typeNotDefined(r_type.type.primitive, exp.loc_start);
            };
        } else if (fn_def.body_block.statements.items[0].data.return_stmt.exp) |return_exp| {
            const return_value = try self.evalExp(return_exp);
            return_type = self.resolveValueType(return_value);
        }

        //restore main scope
        self.scope.exit(self.type_void);

        fn_symbol.type.kind.function.return_type = return_type;
        fn_symbol.type.kind.function.params = try params_metadata.toOwnedSlice(self.allocator);
    }

    pub fn visitBlock(self: *Self, block: *const ast.Block) !ir.Block {
        var instructions: std.ArrayList(ir.Instruction) = .empty;
        try self.scope.enter(self.scope.return_type);
        defer self.scope.exit(self.scope.return_type);

        var return_type: *Type = self.type_void;

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
                .if_capture_stmt => null,
            };

            if (stmt.data == .if_capture_stmt) {
                const if_capture_insts = try self.visitIfCaptureStmt(&stmt);
                try instructions.appendSlice(self.allocator, if_capture_insts);
                continue;
            }

            if (instruction) |i| {
                if (i == .return_stmt) {
                    return_type = self.scope.return_type;
                }
                try instructions.append(self.allocator, i);
            }
        }

        return ir.Block{
            .return_type = return_type,
            .instructions = try instructions.toOwnedSlice(self.allocator),
        };
    }

    fn visitExpressionStmt(self: *Self, stmt: *const ast.StatementNode) !ir.Instruction {
        const expression_stmt = stmt.data.expression_stmt;

        if (expression_stmt.data != .fn_call) {
            return self.err_dispatcher.invalidType("function", @tagName(expression_stmt.data), stmt.loc_start);
        }

        return ir.Instruction{
            .expression_stmt = .{ .value = try self.evalFnCall(expression_stmt) },
        };
    }

    fn visitLetStmt(self: *Self, stmt: *const ast.StatementNode) !?ir.Instruction {
        var var_type: *Type = undefined;
        const let_stmt = stmt.data.let_stmt;

        // const is_anonymous_struct =
        //     let_stmt.value.data == .fn_call and
        //     let_stmt.value.data.fn_call.target.data == .anonymous_struct_inicialization;
        //
        // if (is_anonymous_struct) {
        //     const anonymous_struct_symbol = if (let_stmt.type_annotation == null) try self.createAnonymousStruct(let_stmt.value, let_stmt.is_mut) else null;
        //     const struct_identifier = if (anonymous_struct_symbol) |sym| sym.identifier else let_stmt.type_annotation.?.type.struct_;
        //     let_stmt.value.data.fn_call.target.data = .{ .identifier = struct_identifier };
        // }

        const expression_value = try self.evalExp(let_stmt.value);
        const expression_type = self.resolveValueType(expression_value);

        if (let_stmt.type_annotation) |type_annotation| {
            var_type = try self.resolveTypeAnnotation(type_annotation);
        } else {
            var_type = expression_type;
            if (var_type.kind == .array and var_type.kind.array.kind == .dynamic) {
                return self.err_dispatcher.cantInferArrayLiteralType(let_stmt.value.loc_start);
            }
        }

        if (var_type.kind == .function_def) {
            const fn_symbol = self.scope.symbol_table.getOrThrow(let_stmt.identifier) catch {
                return self.err_dispatcher.notDefined(let_stmt.identifier, stmt.loc_start);
            };
            const func = try self.visitFnDef(let_stmt.value, fn_symbol.uid, fn_symbol.type.kind.function, null);
            try self.functions.append(self.allocator, func);
            return null;
        }

        if (var_type.kind == .struct_def) {
            const struct_def = try self.visitStructDef(let_stmt.value, let_stmt.identifier);
            try self.structs.append(self.allocator, struct_def);
            return null;
        }

        if (!var_type.eql(expression_type)) {
            return self.err_dispatcher.invalidType(
                try var_type.name(self.allocator),
                try expression_type.name(self.allocator),
                stmt.data.let_stmt.value.loc_start,
            );
        }

        const uid = self.scope.genUid();

        self.scope.symbol_table.put(try Symbol.init(
            self.allocator,
            .{
                .identifier = let_stmt.identifier,
                .uid = uid,
                .is_mut = let_stmt.is_mut,
                .type = var_type,
            },
        )) catch {
            return self.err_dispatcher.alreadyDefined(let_stmt.identifier, stmt.loc_start);
        };

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

        return ir.Instruction{ .branch_if = .{
            .condition = condition_value,
            .then_block = (try self.visitBlock(&if_stmt.then_block)).instructions,
            .else_block = if (if_stmt.else_block) |else_blc| (try self.visitBlock(&else_blc)).instructions else &.{},
        } };
    }

    fn visitIfCaptureStmt(self: *Self, stmt: *const ast.StatementNode) Errors![]ir.Instruction {
        const if_capture_stmt = stmt.data.if_capture_stmt;
        const exp_value = try self.evalExp(if_capture_stmt.exp);
        const exp_type = self.resolveValueType(exp_value);
        var instructions: std.ArrayList(ir.Instruction) = .empty;

        if (exp_type.nullable == false) {
            return self.err_dispatcher.invalidType("nullable", try exp_type.name(self.allocator), stmt.loc_start);
        }

        if (if_capture_stmt.identifier.is_mut and !self.isExpressionMutable(if_capture_stmt.exp)) {
            return self.err_dispatcher.unwrappedValueCantBeMutable(if_capture_stmt.identifier.name, stmt.loc_start);
        }

        const captured_type = try Type.init(self.allocator, .{
            .nullable = false,
            .kind = exp_type.kind,
        });

        const aux_var_id = self.scope.genUid();
        const aux_var_name = try std.fmt.allocPrint(self.allocator, "$captured_${d}", .{aux_var_id});

        const store_aux_var_inst = ir.Instruction{ .store_var = .{
            .uid = aux_var_id,
            .identifier = aux_var_name,
            .type = captured_type,
            .value = exp_value,
        } };

        const aux_var_value = try ir.Value.init(self.allocator, .{
            .identifier = .{
                .uid = aux_var_id,
                .identifier = aux_var_name,
                .type = captured_type,
            },
        });

        const captured_symbol = try Symbol.init(self.allocator, .{
            .uid = self.scope.genUid(),
            .identifier = if_capture_stmt.identifier.name,
            .type = captured_type,
            .is_mut = if_capture_stmt.identifier.is_mut,
        });

        try self.scope.symbol_table.put(captured_symbol);
        defer _ = self.scope.symbol_table.remove(captured_symbol);

        const store_captured_var_inst = ir.Instruction{ .store_var = .{
            .uid = captured_symbol.uid,
            .identifier = captured_symbol.identifier,
            .type = captured_type,
            .value = aux_var_value,
        } };

        const check_null_condition = try ir.Value.init(
            self.allocator,
            .{
                .binary_op = .{
                    .kind = .b_cmp_neq,
                    .type = self.type_bool,
                    .left = aux_var_value,
                    .right = try ir.Value.init(self.allocator, .{
                        .i_null = {},
                    }),
                },
            },
        );

        const then_block = (try self.visitBlock(&if_capture_stmt.body)).instructions;
        const else_block = if (if_capture_stmt.else_block) |else_blc| (try self.visitBlock(&else_blc)).instructions else &.{};

        const check_null_if_inst = ir.Instruction{ .branch_if = .{
            .condition = check_null_condition,
            .then_block = then_block,
            .else_block = else_block,
        } };

        try instructions.appendSlice(self.allocator, &.{ store_aux_var_inst, store_captured_var_inst, check_null_if_inst });
        return instructions.toOwnedSlice(self.allocator);
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
                const index_exp = assign_stmt.target.data.indexed;
                const target = try self.evalExp(index_exp.target);
                const target_type = self.resolveValueType(target);
                const assignment_value = try self.evalExp(assign_stmt.exp);
                const assignment_value_type = self.resolveValueType(assignment_value);

                var target_root = index_exp.target;
                while (target_root.data == .indexed) {
                    target_root = target_root.data.indexed.target;
                }

                const target_symbol = self.scope.symbol_table.getOrThrow(target_root.data.identifier) catch {
                    return self.err_dispatcher.notDefined(target_root.data.identifier, stmt.loc_start);
                };

                if (!target_symbol.is_mut and target_type.kind != .struct_) {
                    return self.err_dispatcher.notMutable(target_root.data.identifier, stmt.loc_start);
                }

                switch (target_type.kind) {
                    .array => {
                        if (!target_symbol.type.kind.array.eql(assignment_value_type)) {
                            return self.err_dispatcher.invalidType(
                                try target_symbol.type.name(self.allocator),
                                try assignment_value_type.name(self.allocator),
                                stmt.loc_start,
                            );
                        }
                    },
                    .struct_instance => |struct_instance| {
                        const field = struct_instance.fields.get(index_exp.index.data.identifier) orelse return self.err_dispatcher.invalidStructField(
                            struct_instance.identifier,
                            index_exp.index.data.identifier,
                            stmt.loc_start,
                        );

                        if (!field.type.eql(assignment_value_type)) {
                            return self.err_dispatcher.invalidType(
                                try field.type.name(self.allocator),
                                try assignment_value_type.name(self.allocator),
                                stmt.loc_start,
                            );
                        }
                    },
                    .struct_ => |struct_| {
                        const field = struct_.static_fields.get(index_exp.index.data.identifier) orelse return self.err_dispatcher.invalidStaticStructField(
                            struct_.identifier,
                            index_exp.index.data.identifier,
                            stmt.loc_start,
                        );
                        if (!field.is_mut) {
                            return self.err_dispatcher.notMutable(field.identifier, stmt.loc_start);
                        }

                        if (!field.type.eql(assignment_value_type)) {
                            return self.err_dispatcher.invalidType(
                                try field.type.name(self.allocator),
                                try assignment_value_type.name(self.allocator),
                                stmt.loc_start,
                            );
                        }
                    },
                    else => unreachable,
                }

                return ir.Instruction{
                    .update_indexed = .{
                        .target = try self.evalExp(assign_stmt.target),
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
            .do_block = block.instructions,
        } };
    }

    fn visitAnonymousStructDef(self: *Self, struct_def: *const ast.AnonymousStructDef) !AnonymousStruct {
        var fields_in_order: std.ArrayList(TypedIdentifier) = .empty;
        var fields: std.StringHashMap(TypedIdentifier) = .init(self.allocator);

        for (struct_def.fields) |field| {
            const struct_field = try self.createStructField(field);
            try fields_in_order.append(
                self.allocator,
                struct_field,
            );
            try fields.put(field.identifier, struct_field);
        }

        return .{
            .fields = fields,
            .fields_in_order = try fields_in_order.toOwnedSlice(self.allocator),
        };
    }

    fn visitStructDef(self: *Self, exp: *const ast.ExpNode, identifier: []const u8) Errors!ir.Struct {
        const struct_symbol = try self.scope.symbol_table.getOrThrow(identifier);
        const prev_return_type = self.scope.return_type;
        try self.scope.enter(self.type_void);
        defer self.scope.exit(prev_return_type);

        const struct_def = exp.data.struct_def;
        var fields: std.ArrayList(ir.StructField) = .empty;
        var static_fields: std.ArrayList(ir.StructField) = .empty;
        var funcs: std.ArrayList(ir.Func) = .empty;

        for (struct_def.fields) |field| {
            const default_value: ?*ir.Value = if (field.default_value) |_value| try self.evalExp(_value) else null;
            const field_type = if (field.type) |_type| try self.resolveTypeAnnotation(_type) else self.resolveValueType(default_value.?);

            try fields.append(
                self.allocator,
                .{
                    .identifier = field.identifier,
                    .type = field_type,
                    .default_value = default_value,
                },
            );
        }

        for (struct_def.static_fields) |field| {
            const default_value: ?*ir.Value = if (field.default_value) |_value| try self.evalExp(_value) else null;
            const field_type = if (field.type) |_type| try self.resolveTypeAnnotation(_type) else self.resolveValueType(default_value.?);

            try static_fields.append(
                self.allocator,
                .{
                    .identifier = field.identifier,
                    .type = field_type,
                    .default_value = default_value,
                },
            );
        }

        for (struct_def.funcs) |func| {
            const fn_metadata = if (struct_symbol.type.kind.struct_.functions.get(func.identifier)) |metadata| metadata else unreachable;
            const fn_name = fn_metadata.symbol.identifier;
            const fn_def = try self.visitFnDef(func.def, struct_symbol.uid, fn_metadata, fn_name);
            try funcs.append(self.allocator, fn_def);
        }

        return ir.Struct{
            .uid = struct_symbol.uid,
            .identifier = identifier,
            .fields = try fields.toOwnedSlice(self.allocator),
            .static_fields = try static_fields.toOwnedSlice(self.allocator),
            .funcs = try funcs.toOwnedSlice(self.allocator),
        };
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

        for (metadata.params) |param| {
            const arg_uid = self.scope.genUid();

            if (param.is_mut and param.type.kind != .array and param.type.kind != .struct_instance) {
                return self.err_dispatcher.primitiveParamsCantBeMutable(exp.loc_start);
            }

            try self.scope.symbol_table.put(try Symbol.init(
                self.allocator,
                .{
                    .uid = arg_uid,
                    .identifier = param.identifier,
                    .is_mut = param.is_mut,
                    .type = param.type,
                },
            ));

            try params.append(self.allocator, .{
                .uid = arg_uid,
                .identifier = param.identifier,
                .type = param.type,
                .default_value = param.default_value,
            });
        }

        const body = try self.visitBlock(&fn_def.body_block);

        if (!body.return_type.eql(self.scope.return_type)) {
            return self.err_dispatcher.invalidFunctionReturnType(
                try self.scope.return_type.name(self.allocator),
                try body.return_type.name(self.allocator),
                exp.loc_start,
            );
        }

        return ir.Func{
            .uid = uid,
            .identifier = if (override_name) |name| name else metadata.symbol.identifier,
            .params = try params.toOwnedSlice(self.allocator),
            .return_type = metadata.return_type,
            .body = body.instructions,
        };
    }

    fn visitReturnStmt(self: *Self, stmt: *const ast.StatementNode) !ir.Instruction {
        const return_stmt = stmt.data.return_stmt;

        if (return_stmt.exp) |exp| {
            const exp_value = try self.evalExp(exp);
            const exp_value_type = self.resolveValueType(exp_value);

            if (!exp_value_type.eql(self.scope.return_type)) {
                return self.err_dispatcher.invalidFunctionReturnType(
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
            return self.err_dispatcher.invalidFunctionReturnType(
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
            .float_literal => {
                return ir.Value.init(self.allocator, .{ .i_float = exp.data.float_literal });
            },
            .int_literal => {
                return ir.Value.init(self.allocator, .{ .i_int = exp.data.int_literal });
            },
            .string_literal => {
                return ir.Value.init(self.allocator, .{ .i_string = exp.data.string_literal });
            },
            .bool_literal => {
                return ir.Value.init(self.allocator, .{ .i_bool = exp.data.bool_literal });
            },
            .null_literal => {
                return ir.Value.init(self.allocator, .{ .i_null = {} });
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
                return self.evalFnCall(exp);
            },
            .struct_def => {
                return ir.Value.init(self.allocator, .{ .struct_def = {} });
            },
            .indexed => {
                return self.evalIndexedExp(exp, false);
            },
            .nullable_indexed => {
                return self.evalIndexedExp(exp, true);
            },
            .unary_exp => {
                return self.evalUnaryExp(exp);
            },
            .binary_exp => {
                return self.evalBinaryExp(exp);
            },
            else => unreachable,
        }
    }

    //note: too big needs refactor
    fn evalFnCall(self: *Self, exp: *const ast.ExpNode) !*ir.Value {
        const fn_call = exp.data.fn_call;
        var fn_identifier: []const u8 = undefined;
        var uid: usize = undefined;
        var params: []const TypedIdentifier = undefined;
        var return_type: *Type = undefined;
        var is_struct_fn_call = false;
        var fn_call_target: *ir.Value = undefined;

        var arg_exp_by_param_name = std.StringHashMap(*ast.ExpNode).init(self.allocator);
        defer arg_exp_by_param_name.deinit();

        var fn_call_arguments_values: std.ArrayList(*ir.Value) = .empty;

        switch (fn_call.target.data) {
            .identifier => |id| {
                const symbol = self.scope.symbol_table.getOrThrow(id) catch {
                    return self.err_dispatcher.notDefined(id, exp.loc_start);
                };

                fn_identifier = symbol.identifier;
                uid = symbol.uid;

                switch (symbol.type.kind) {
                    .function => |func| {
                        params = func.params;
                        return_type = func.return_type;
                    },
                    //when identifier is a struct we turn it into a struct inicialization
                    .struct_ => |struct_| {
                        params = struct_.fields_in_order;
                        return_type = try Type.init(self.allocator, .{
                            .kind = .{ .struct_instance = &symbol.type.kind.struct_ },
                            .nullable = false,
                        });
                    },
                    else => {
                        return self.err_dispatcher.invalidType(
                            "function or struct",
                            try symbol.type.name(self.allocator),
                            exp.loc_start,
                        );
                    },
                }
            },
            //note: not supporting functions stored in arrays
            .indexed => |indexed| {
                const target = try self.evalExp(indexed.target);
                const target_type = self.resolveValueType(target);

                if (target_type.kind != .struct_ and target_type.kind != .struct_instance) {
                    return self.err_dispatcher.invalidType(
                        "a struct",
                        try target_type.name(self.allocator),
                        exp.loc_start,
                    );
                }

                is_struct_fn_call = true;
                fn_call_target = target;
                const fn_name = indexed.index.data.identifier;

                const fn_metadata = switch (target_type.kind) {
                    .struct_ => |_struct| _struct.functions.get(fn_name) orelse {
                        return self.err_dispatcher.invalidStructFunction(
                            _struct.identifier,
                            fn_name,
                            exp.loc_start,
                        );
                    },
                    .struct_instance => |struct_instance| struct_instance.functions.get(fn_name) orelse {
                        return self.err_dispatcher.invalidStructFunction(
                            struct_instance.identifier,
                            fn_name,
                            exp.loc_start,
                        );
                    },
                    else => unreachable,
                };

                const struct_symbol_identifier = if (target_type.kind == .struct_instance) target_type.kind.struct_instance.identifier else target_type.kind.struct_.identifier;
                const target_type_symbol = try self.scope.symbol_table.getOrThrow(struct_symbol_identifier);

                //auto append struct to fn call if first argument of function is itself
                if (fn_metadata.params.len > 0 and target_type.kind == .struct_instance) {
                    const first_argument_uid = switch (fn_metadata.params[0].type.kind) {
                        .struct_instance => |struct_instance| (try self.scope.symbol_table.getOrThrow(struct_instance.identifier)).uid,
                        else => 0,
                    };

                    const target_uid = target_type_symbol.uid;

                    if (first_argument_uid == target_uid) {
                        if (fn_metadata.params[0].is_mut) {
                            const symbol = try self.scope.symbol_table.getOrThrow(target.identifier.identifier);
                            if (!symbol.is_mut) {
                                return self.err_dispatcher.notMutable(indexed.target.data.identifier, indexed.target.loc_start);
                            }
                        }
                        try fn_call_arguments_values.append(self.allocator, target);
                    }
                }

                params = fn_metadata.params;
                return_type = fn_metadata.return_type;
                uid = target_type_symbol.uid;
                fn_identifier = fn_metadata.symbol.identifier;
            },
            .anonymous_struct_inicialization => {
                const anonymous_struct = try self.createAnonymousStructFromFnCall(exp, false);
                params = anonymous_struct.fields_in_order;
                return_type = try Type.init(self.allocator, .{
                    .kind = .{ .anonymous_struct_instance = anonymous_struct },
                    .nullable = false,
                });
            },
            else => unreachable,
        }

        const call_args_len = fn_call.arguments.len;

        var required_params_len: usize = 0;
        for (params) |param| {
            if (param.default_value == null) {
                required_params_len += 1;
            }
        }

        //call args + auto binded struct
        const total_args_len = call_args_len + fn_call_arguments_values.items.len;

        if (required_params_len > total_args_len) {
            return self.err_dispatcher.invalidNumberOfArgs(required_params_len, total_args_len, exp.loc_start);
        }

        if (fn_call.are_arguments_named) {
            for (fn_call.arguments) |arg| {
                if (arg.identifier) |identifier| {
                    try arg_exp_by_param_name.put(identifier, arg.exp);
                } else unreachable;
            }
        }

        const has_first_argument_being_binded = fn_call_arguments_values.items.len == 1;
        for (params, 0..) |param, i| {
            if (i == 0 and has_first_argument_being_binded) continue;

            var arg_exp: ?*ast.ExpNode = null;

            if (fn_call.are_arguments_named) {
                const named_arg_exp = arg_exp_by_param_name.get(param.identifier);
                if (named_arg_exp) |named_arg| {
                    arg_exp = named_arg;
                }
            } else {
                const index = if (has_first_argument_being_binded) i - 1 else i;
                arg_exp = if (fn_call.arguments.len > index) fn_call.arguments[index].exp else null;
            }

            if (arg_exp) |_exp| {
                const fn_call_arg_value = try self.evalExp(_exp);
                const arg_type = self.resolveValueType(fn_call_arg_value);

                if (!param.type.eql(arg_type)) {
                    return self.err_dispatcher.invalidType(try param.type.name(self.allocator), try arg_type.name(self.allocator), _exp.loc_start);
                }

                if (param.type.kind == .struct_ and param.is_mut) {
                    const symbol = try self.scope.symbol_table.getOrThrow(fn_call_arg_value.identifier.identifier);
                    if (!symbol.is_mut) {
                        return self.err_dispatcher.notMutable(symbol.identifier, _exp.loc_start);
                    }
                }

                if (param.type.kind == .array and param.is_mut and _exp.data != .array_literal) {
                    const symbol = try self.scope.symbol_table.getOrThrow(fn_call_arg_value.identifier.identifier);
                    if (!symbol.is_mut) {
                        return self.err_dispatcher.notMutable(fn_call_arg_value.identifier.identifier, _exp.loc_start);
                    }
                }
                try fn_call_arguments_values.append(self.allocator, fn_call_arg_value);
            } else if (param.default_value) |value| {
                try fn_call_arguments_values.append(self.allocator, value);
            } else {
                return self.err_dispatcher.missingArgument(param.identifier, exp.loc_start);
            }
        }

        const is_anonymous_struct_inicialization = fn_call.target.data == .anonymous_struct_inicialization;

        if (is_anonymous_struct_inicialization) {
            var keys: std.ArrayList([]const u8) = .empty;
            for (params) |param| {
                try keys.append(self.allocator, param.identifier);
            }

            return ir.Value.init(
                self.allocator,
                .{ .struct_init = .{
                    .keys = try keys.toOwnedSlice(self.allocator),
                    .values = try fn_call_arguments_values.toOwnedSlice(self.allocator),
                    .type = return_type,
                } },
            );
        }

        if (is_struct_fn_call) {
            return ir.Value.init(
                self.allocator,
                .{
                    .struct_fn_call = .{
                        .target = fn_call_target,
                        .identifier = fn_identifier,
                        .return_type = return_type,
                        .args = try fn_call_arguments_values.toOwnedSlice(self.allocator),
                    },
                },
            );
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

    fn createIndexedIrValue(self: *Self, target: *ir.Value, index: *ir.Value, is_nullable: bool) !*ir.Value {
        if (is_nullable) {
            return ir.Value.init(self.allocator, .{
                .nullable_indexed = .{ .target = target, .index = index },
            });
        } else {
            return ir.Value.init(self.allocator, .{
                .indexed = .{ .target = target, .index = index },
            });
        }
    }

    fn evalIndexedExp(self: *Self, exp: *const ast.ExpNode, is_nullable: bool) !*ir.Value {
        const indexed_exp_index = if (is_nullable) exp.data.nullable_indexed.index else exp.data.indexed.index;
        const index_exp_target = if (is_nullable) exp.data.nullable_indexed.target else exp.data.indexed.target;

        const target = try self.evalExp(index_exp_target);
        const target_type = self.resolveValueType(target);

        if (!is_nullable and target_type.nullable) {
            return self.err_dispatcher.nullableMustBeUnwraped(try target_type.name(self.allocator), exp.loc_start);
        }

        if (is_nullable and !target_type.nullable) {
            return self.err_dispatcher.unnecessaryOptionalChain(try target_type.name(self.allocator), exp.loc_start);
        }

        switch (target_type.kind) {
            .array => {
                const index = try self.evalExp(indexed_exp_index);
                const index_type = self.resolveValueType(index);

                if (!index_type.eql(self.type_int)) {
                    return self.err_dispatcher.invalidIndexing("int", try index_type.name(self.allocator), exp.loc_start);
                }

                return self.createIndexedIrValue(target, index, is_nullable);
            },
            .struct_instance => {
                const struct_metadata = target_type.kind.struct_instance;

                return switch (indexed_exp_index.data) {
                    .identifier => |member_name| {
                        const field_member = struct_metadata.fields.get(member_name);
                        const function_member = struct_metadata.functions.get(member_name);

                        if (field_member == null and function_member == null) {
                            return self.err_dispatcher.invalidStructField(struct_metadata.identifier, member_name, exp.loc_start);
                        }

                        return self.createIndexedIrValue(
                            target,
                            try ir.Value.init(self.allocator, .{ .i_string = member_name }),
                            is_nullable,
                        );
                    },
                    else => unreachable,
                };
            },
            .struct_ => {
                const struct_metadata = target_type.kind.struct_;

                return switch (indexed_exp_index.data) {
                    .identifier => |member_name| {
                        const field_member = struct_metadata.static_fields.get(member_name);
                        const function_member = struct_metadata.functions.get(member_name);

                        if (field_member == null and function_member == null) {
                            return self.err_dispatcher.invalidStaticStructField(struct_metadata.identifier, member_name, exp.loc_start);
                        }

                        return self.createIndexedIrValue(
                            target,
                            try ir.Value.init(self.allocator, .{ .i_string = member_name }),
                            is_nullable,
                        );
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
                    .type = try Type.init(self.allocator, .{ .kind = .{ .array = array_literal_type }, .nullable = false }),
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
                if (!exp_type.eql(self.type_float) and !exp_type.eql(self.type_int)) {
                    return self.err_dispatcher.invalidType(
                        "int or float",
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

        return ir.Value.init(self.allocator, .{ .unary_op = .{
            .kind = self.astUnaryOpToIrUnaryOpKind(unary_exp.op, exp_type),
            .type = exp_type,
            .right = exp_value,
        } });
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
                if (!left_type.eql(self.type_float) and !left_type.eql(self.type_int)) {
                    return self.err_dispatcher.invalidType(
                        "int or float",
                        try left_type.name(self.allocator),
                        exp.loc_start,
                    );
                }
                op_type = left_type;
            },

            //note: this makes support string comparison by operators
            .lt, .lt_or_eq, .gt, .gt_or_eq => {
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
            .eq, .not_eq => {
                op_type = self.type_bool;
            },
        }

        return ir.Value.init(self.allocator, .{ .binary_op = .{
            .kind = self.astBinOpToIrBinOpKind(bin_exp.op, op_type),
            .type = op_type,
            .left = left_value,
            .right = right_value,
        } });
    }

    fn isExpressionMutable(self: *Self, exp: *ast.ExpNode) bool {
        return switch (exp.data) {
            .identifier => |name| {
                const sym = self.scope.symbol_table.get(name) orelse return false;
                return sym.is_mut;
            },
            .indexed => |indexed| self.isExpressionMutable(indexed.target),
            .nullable_indexed => |indexed| self.isExpressionMutable(indexed.target),
            else => false,
        };
    }

    pub fn resolveTypeAnnotation(self: *Self, type_annotation: *ast.TypeAnnotation) !*Type {
        switch (type_annotation.type) {
            .primitive => |primitive_name| {
                if (std.mem.eql(u8, primitive_name, "float")) return if (type_annotation.nullable) self.type_float_nullable else self.type_float;
                if (std.mem.eql(u8, primitive_name, "int")) return if (type_annotation.nullable) self.type_int_nullable else self.type_int;
                if (std.mem.eql(u8, primitive_name, "string")) return if (type_annotation.nullable) self.type_str_nullable else self.type_str;
                if (std.mem.eql(u8, primitive_name, "bool")) return if (type_annotation.nullable) self.type_bool_nullable else self.type_bool;
                if (std.mem.eql(u8, primitive_name, "fn")) return if (type_annotation.nullable) self.type_func_def_nullable else self.type_func_def;
                if (std.mem.eql(u8, primitive_name, "void")) return self.type_void;
                if (std.mem.eql(u8, primitive_name, "dynamic")) return if (type_annotation.nullable) self.type_dynamic_nullable else self.type_dynamic;

                return Errors.SemaError;
            },
            .struct_ => |struct_name| {
                const struct_symbol = try self.scope.symbol_table.getOrThrow(struct_name);
                return Type.init(self.allocator, .{
                    .kind = .{ .struct_instance = &struct_symbol.type.kind.struct_ },
                    .nullable = type_annotation.nullable,
                });
            },
            .anonymous_struct => |struct_metadata| {
                return Type.init(self.allocator, .{
                    .kind = .{ .anonymous_struct_instance = try self.visitAnonymousStructDef(&struct_metadata) },
                    .nullable = type_annotation.nullable,
                });
            },
            .array => {
                const inner_type = try self.resolveTypeAnnotation(type_annotation.type.array);
                return Type.init(self.allocator, .{
                    .kind = .{
                        .array = inner_type,
                    },
                    .nullable = type_annotation.nullable,
                });
            },
            .struct_self => {
                return Type.init(self.allocator, .{ .kind = .struct_self, .nullable = type_annotation.nullable });
            },
        }
    }

    fn getIndexedType(self: *Self, target: *ir.Value, index: *ir.Value) *Type {
        const target_type = self.resolveValueType(target);

        return switch (target_type.kind) {
            .array => |inner_type| inner_type,
            .struct_instance => |struct_metadata| {
                const field_name = index.i_string;
                if (struct_metadata.fields.get(field_name)) |field_type| {
                    return field_type.type;
                }
                if (struct_metadata.functions.get(field_name)) |_| {
                    return self.type_func_def;
                }
                unreachable;
            },
            .struct_ => |struct_metadata| {
                const field_name = index.i_string;
                if (struct_metadata.static_fields.get(field_name)) |field_type| {
                    return field_type.type;
                }
                if (struct_metadata.functions.get(field_name)) |_| {
                    return self.type_func_def;
                }
                unreachable;
            },
            else => unreachable,
        };
    }

    fn makeNullable(self: *Self, base_type: *Type) !*Type {
        if (base_type.nullable) return base_type;

        const new_type = try self.allocator.create(Type);
        new_type.* = base_type.*;
        new_type.nullable = true;

        return new_type;
    }

    pub fn resolveValueType(self: *Self, value: *ir.Value) *Type {
        //note: func_def needs a better solving probably doing this when we have generics?
        return switch (value.*) {
            .i_float => self.type_float,
            .i_int => self.type_int,
            .i_bool => self.type_bool,
            .i_string => self.type_str,
            .i_void => self.type_bool,
            .i_null => self.type_null,
            .fn_def => self.type_func_def,
            .struct_def => self.type_struct_def,
            .identifier => value.identifier.type,
            .struct_init => value.struct_init.type,
            .binary_op => value.binary_op.type,
            .unary_op => value.unary_op.type,
            .indexed => |indexed| {
                return self.getIndexedType(indexed.target, indexed.index);
            },
            .nullable_indexed => |indexed| {
                const raw_type = self.getIndexedType(indexed.target, indexed.index);
                //note: this maybe dangerous but i'm not changing all the signatures of this function rn. Maybe not even be needed when we have a TypeTable
                return self.makeNullable(raw_type) catch unreachable;
            },
            .i_array => value.i_array.type,
            .fn_call => value.fn_call.return_type,
            .struct_fn_call => value.struct_fn_call.return_type,
        };
    }

    fn astBinOpToIrBinOpKind(self: *Self, bin_op: ast.BinaryOp, op_type: *Type) ir.BinaryOpKind {
        return switch (bin_op) {
            .add => if (op_type == self.type_float) .i_add else .f_add,
            .sub => if (op_type == self.type_float) .i_sub else .f_sub,
            .mult => if (op_type == self.type_float) .i_mult else .f_mult,
            .div => if (op_type == self.type_float) .i_div else .f_div,
            .mod => if (op_type == self.type_float) .i_mod else .f_mod,
            .eq => if (op_type == self.type_float) .i_cmp_eq else .f_cmp_eq,
            .not_eq => if (op_type == self.type_float) .i_cmp_neq else .f_cmp_neq,
            .lt => if (op_type == self.type_float) .i_cmp_lt else .f_cmp_lt,
            .lt_or_eq => if (op_type == self.type_float) .i_cmp_le else .f_cmp_le,
            .gt => if (op_type == self.type_float) .i_cmp_gt else .f_cmp_gt,
            .gt_or_eq => if (op_type == self.type_float) .i_cmp_ge else .f_cmp_ge,
            .bool_or => .b_or,
            .bool_and => .b_and,
        };
    }

    fn astUnaryOpToIrUnaryOpKind(self: *Self, bin_op: ast.UnaryOp, exp_type: *Type) ir.UnaryOpKind {
        return switch (bin_op) {
            .neg => if (exp_type.eql(self.type_float)) .f_neg else .i_neg,
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

        var symbols = std.StringHashMap(*Symbol).init(alloc);

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
    symbols: std.StringHashMap(*Symbol),

    pub fn init(allocator: std.mem.Allocator, parent: ?*SymbolTable, symbols: ?std.StringHashMap(*Symbol)) !*Self {
        const ptr = try allocator.create(Self);
        ptr.* = Self{
            .allocator = allocator,
            .parent = parent,
            .symbols = if (symbols) |s| s else std.StringHashMap(*Symbol).init(allocator),
        };
        return ptr;
    }

    fn put(self: *Self, symbol: *Symbol) !void {
        if (self.symbols.get(symbol.identifier)) |_| {
            return Errors.SemaError;
        }

        try self.symbols.put(symbol.identifier, symbol);
    }

    fn remove(self: *Self, symbol: *Symbol) bool {
        return self.symbols.remove(symbol.identifier);
    }

    fn get(self: *Self, identifier: []const u8) ?*Symbol {
        if (self.symbols.get(identifier)) |s| return s;
        if (self.parent) |parent| {
            return parent.get(identifier);
        }
        return null;
    }

    //note: getOrError
    fn getOrThrow(self: *Self, id: []const u8) !*Symbol {
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

const AnonymousStruct = struct {
    fields: std.StringHashMap(TypedIdentifier),
    fields_in_order: []const TypedIdentifier,
};

const StructMetadata = struct {
    identifier: []const u8,
    fields: std.StringHashMap(TypedIdentifier),
    static_fields: std.StringHashMap(TypedIdentifier),
    fields_in_order: []const TypedIdentifier,
    functions: std.StringHashMap(FuncMetadata),
};

const FuncMetadata = struct {
    symbol: *Symbol,
    params: []const TypedIdentifier,
    return_type: *Type,
};

//note: idk about this
pub const TypedIdentifier = struct {
    const Self = @This();

    identifier: []const u8,
    type: *Type,
    is_mut: bool,
    default_value: ?*ir.Value,

    pub fn init(allocator: std.mem.Allocator, exp: Self) !*Self {
        const ptr = try allocator.create(Self);
        ptr.* = exp;
        return ptr;
    }
};

pub const Type = struct {
    const Self = @This();

    kind: union(enum) {
        float,
        int,
        string,
        boolean,
        void,
        null,
        //note: currently only used for built-in functions
        dynamic,

        //note: later both of this type should take the metadata
        //i think this types are not needed specially the struct_def one not removing them now
        function_def,
        function: FuncMetadata,

        //struct_def = a struct gonna be defined
        //struct_ = is the DEFINED  struct
        struct_def,
        struct_: StructMetadata,

        anonymous_struct_instance: AnonymousStruct,
        struct_instance: *StructMetadata,

        struct_self,
        array: *Type,
    },

    nullable: bool,

    pub fn init(allocator: std.mem.Allocator, exp: Self) !*Self {
        const ptr = try allocator.create(Self);
        ptr.* = exp;
        return ptr;
    }

    pub fn eql(self: *Self, other: *Type) bool {
        if (self.kind == .dynamic or other.kind == .dynamic) return true;
        if (self.nullable == false and other.nullable == true) return false;

        switch (self.kind) {
            .float => return other.kind == .float or (self.nullable and other.kind == .null),
            .int => return other.kind == .int or (self.nullable and other.kind == .null),
            .string => return other.kind == .string or (self.nullable and other.kind == .null),
            .boolean => return other.kind == .boolean or (self.nullable and other.kind == .null),
            .void => return other.kind == .void or (self.nullable and other.kind == .null),
            .dynamic => return true,

            //note: this is wrong
            .function_def => return other.kind == .function_def or (self.nullable and other.kind == .null),
            .struct_def => return other.kind == .struct_def or (self.nullable and other.kind == .null),

            //todo: same impl of below
            .struct_instance => |struct_instance| {
                if (self.nullable == true and other.kind == .null) {
                    return true;
                }

                if (other.kind != .struct_instance and other.kind != .anonymous_struct_instance) {
                    return false;
                }

                //note: currently is structural typing
                //should change to more stricter when meta structs be implemented
                const fields = if (other.kind == .anonymous_struct_instance) other.kind.anonymous_struct_instance.fields else other.kind.struct_instance.fields;
                for (struct_instance.fields_in_order) |field| {
                    if (fields.get(field.identifier) == null) {
                        return false;
                    }
                }

                //note: i think we dont need to verify functions!
                // var fn_its = struct_instance.functions.iterator();
                // while (fn_its.next()) |func| {
                //     if (other_struct.functions.get(func.key_ptr.*) == null) {
                //         return false;
                //     }
                // }

                return true;
            },

            .anonymous_struct_instance => |struct_instance| {
                if (self.nullable == true and other.kind == .null) {
                    return true;
                }

                if (other.kind != .struct_instance and other.kind != .anonymous_struct_instance) {
                    return false;
                }

                const fields = if (other.kind == .anonymous_struct_instance) other.kind.anonymous_struct_instance.fields else other.kind.struct_instance.fields;

                for (struct_instance.fields_in_order) |field| {
                    if (fields.get(field.identifier) == null) {
                        return false;
                    }
                }

                return true;
            },

            .struct_ => |struct_| {
                if (other.kind == .struct_) {
                    return std.mem.eql(u8, other.kind.struct_.identifier, struct_.identifier);
                }
                return false;
            },

            .function => return true,

            .array => |inner| {
                if (other.kind != .array) return false;
                return inner.eql(other.kind.array);
            },

            .null => {
                return other.nullable or other.kind == .null;
            },

            .struct_self => unreachable,
        }
    }

    pub fn name(self: *Self, allocator: std.mem.Allocator) ![]const u8 {
        const prefix = if (self.nullable) "nullable " else "";

        return switch (self.kind) {
            .float => if (self.nullable) "nullable float" else "float",
            .int => if (self.nullable) "nullable int" else "int",
            .string => if (self.nullable) "nullable string" else "string",
            .boolean => if (self.nullable) "nullable boolean" else "boolean",
            .void => if (self.nullable) "nullable void" else "void",
            .null => if (self.nullable) "nullable null" else "null",
            .dynamic => if (self.nullable) "nullable dynamic" else "dynamic",
            .struct_def => if (self.nullable) "nullable struct def" else "struct def",
            .function_def => if (self.nullable) "nullable function def" else "function def",
            .struct_self => "@",
            //perf: all bellow are wasting memory
            .function => |metadata| {
                return std.fmt.allocPrint(allocator, "function {s}", .{
                    metadata.symbol.identifier,
                });
            },
            .struct_instance => |metadata| {
                return std.fmt.allocPrint(allocator, "{s}instance of struct {s}", .{
                    prefix,
                    metadata.identifier,
                });
            },
            //note: this gonna log ugly shit
            .anonymous_struct_instance => |metadata| {
                return std.fmt.allocPrint(allocator, "anonymous struct {any}", .{
                    metadata.fields,
                });
            },
            .struct_ => |metadata| {
                return std.fmt.allocPrint(allocator, "{s}struct {s}", .{
                    prefix,
                    metadata.identifier,
                });
            },
            .array => |inner_type| {
                return std.fmt.allocPrint(allocator, "{s}[]{s}", .{ prefix, try inner_type.name(allocator) });
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
