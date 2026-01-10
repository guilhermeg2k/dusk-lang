pub const SemaAnalyzer = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    src: []const u8,
    ast_root: *const ast.Root,

    functions: std.ArrayList(ir.Func),
    structs: std.ArrayList(ir.Struct),

    scope: Scope,
    err_dispatcher: err.ErrorDispatcher,

    number_type: *Type,
    str_type: *Type,
    bool_type: *Type,
    type_fn: *Type,
    type_struct: *Type,
    void_type: *Type,
    anytype_type: *Type,

    pub fn init(allocator: std.mem.Allocator, src: []const u8, ast_root: *const ast.Root) !Self {
        const void_type = try Type.init(allocator, .void);
        return Self{
            .allocator = allocator,
            .src = src,
            .ast_root = ast_root,
            .scope = try Scope.init(allocator, void_type),
            .err_dispatcher = .{ .allocator = allocator, .src = src },
            .functions = .empty,
            .structs = .empty,
            .number_type = try Type.init(allocator, .number),
            .str_type = try Type.init(allocator, .string),
            .bool_type = try Type.init(allocator, .boolean),
            .type_fn = try Type.init(allocator, .function),
            .type_struct = try Type.init(allocator, .struct_type),
            .void_type = void_type,
            .anytype_type = try Type.init(allocator, .dynamic),
        };
    }

    pub fn deinit(self: *Self) void {
        self.scope.deinit();
    }

    pub fn analyze(self: *Self) !ir.Program {
        return .{
            .instructions = try self.visitBlock(self.ast_root),
            .functions = try self.functions.toOwnedSlice(self.allocator),
            .structs = try self.structs.toOwnedSlice(self.allocator),
        };
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

        //warn: this maybe crash
        if (let_stmt.type_annotation) |type_annotation| {
            var_type = self.resolveTypeAnnotation(type_annotation) catch {
                return self.err_dispatcher.typeNotDefined(type_annotation.name, stmt.loc_start);
            };
        } else {
            var_type = expression_type;
        }

        const uid = self.scope.genUid();

        if (!expression_type.eql(var_type) and !expression_type.eql(var_type)) {
            return self.err_dispatcher.invalidType(try var_type.name(self.allocator), try expression_type.name(self.allocator), stmt.loc_start);
        }

        try self.scope.symbol_table.put(.{
            .identifier = let_stmt.identifier,
            .uid = uid,
            .is_mut = let_stmt.is_mut,
            .type = var_type,
            .metadata = null,
        });

        if (expression_type.eql(self.type_fn)) {
            const func = try self.visitFnDef(let_stmt.value, let_stmt.identifier);
            try self.functions.append(self.allocator, func);
            return null;
        }

        if (expression_type.eql(self.type_struct)) {
            const struct_def = try self.visitStructDef(let_stmt.value, let_stmt.identifier);
            try self.structs.append(self.allocator, struct_def);
            return null;
        }

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
        if (!condition_value_type.eql(self.bool_type)) {
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
            .index_exp => {
                const index_exp = assign_stmt.target.data.index_exp;
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

                return ir.Instruction{ .update_indexed = .{
                    .target = target_exp,
                    .index = index,
                    .value = assignment_value,
                } };
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

        if (!condition_value_type.eql(self.bool_type)) {
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
        const prev_return_type = self.scope.return_type;
        try self.scope.enter(self.void_type);
        defer self.scope.exit(prev_return_type);

        const struct_def = exp.data.struct_def;
        var fields: std.ArrayList(ir.StructField) = .empty;
        var funcs: std.ArrayList(ir.Func) = .empty;

        const uid = self.scope.genUid();

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
            const fn_def = try self.visitFnDef(func.def, func.identifier);
            try funcs.append(self.allocator, fn_def);
        }

        return ir.Struct{
            .uid = uid,
            .identifier = identifier,
            .fields = try fields.toOwnedSlice(self.allocator),
            .funcs = try funcs.toOwnedSlice(self.allocator),
        };
    }

    fn visitFnDef(self: *Self, exp: *const ast.ExpNode, identifier: []const u8) Errors!ir.Func {
        const fn_def = exp.data.fn_def;
        const old_return_type = self.scope.return_type;

        var return_type: *Type = undefined;
        var arguments: std.ArrayList(ir.FuncArg) = .empty;
        var params_metadata: std.ArrayList(*FuncParamMetadata) = .empty;

        const fn_uid = self.scope.genUid();

        //create a temporary scope just for inline return type inference
        try self.scope.enter(old_return_type);

        for (fn_def.arguments) |arg| {
            const arg_type = self.resolveTypeAnnotation(arg.type_annotation) catch {
                return self.err_dispatcher.typeNotDefined(
                    arg.type_annotation.name,
                    exp.loc_start,
                );
            };

            const arg_uid = self.scope.genUid();

            self.scope.symbol_table.put(.{
                .uid = arg_uid,
                .identifier = arg.identifier,
                .is_mut = arg.is_mut,
                .type = arg_type,
                .metadata = null,
            }) catch {
                return self.err_dispatcher.alreadyDefined(arg.identifier, exp.loc_start);
            };

            try params_metadata.append(self.allocator, .{ .identifier = arg.identifier, .type = arg_type });
        }

        if (fn_def.return_type) |r_type| {
            return_type = self.resolveTypeAnnotation(r_type) catch {
                return self.err_dispatcher.typeNotDefined(r_type.name, exp.loc_start);
            };
        } else if (fn_def.body_block.statements.items[0].data.return_stmt.exp) |return_exp| {
            const return_value = try self.evalExp(return_exp);
            return_type = self.resolveValueType(return_value);
        }

        //restore main scope
        self.scope.exit(old_return_type);

        try self.scope.symbol_table.replace(.{
            .identifier = identifier,
            .uid = fn_uid,
            .is_mut = false,
            .type = self.type_fn,
            .metadata = .{
                .func = .{
                    .params = try params_metadata.toOwnedSlice(self.allocator),
                    .return_type = return_type,
                },
            },
        });

        try self.scope.enter(return_type);
        defer self.scope.exit(old_return_type);

        for (fn_def.arguments) |arg| {
            const arg_type = self.resolveTypeAnnotation(arg.type_annotation) catch {
                return self.err_dispatcher.typeNotDefined(
                    arg.type_annotation.name,
                    exp.loc_start,
                );
            };

            var arg_default_value: ?*ir.Value = null;

            if (arg.default_value) |default_value| {
                arg_default_value = try self.evalExp(default_value);
                const default_value_type = self.resolveValueType(arg_default_value.?);

                if (!arg_type.eql(default_value_type)) {
                    return self.err_dispatcher.invalidType(
                        try arg_type.name(self.allocator),
                        try default_value_type.name(self.allocator),
                        default_value.loc_start,
                    );
                }
            }

            const arg_uid = self.scope.genUid();

            self.scope.symbol_table.put(.{
                .uid = arg_uid,
                .identifier = arg.identifier,
                .is_mut = arg.is_mut,
                .type = arg_type,
                .metadata = null,
            }) catch {
                return self.err_dispatcher.alreadyDefined(arg.identifier, exp.loc_start);
            };

            try arguments.append(self.allocator, .{
                .uid = arg_uid,
                .identifier = arg.identifier,
                .type = arg_type,
                .default_value = arg_default_value,
            });
        }

        const body = try self.visitBlock(&fn_def.body_block);

        return ir.Func{
            .uid = fn_uid,
            .identifier = identifier,
            .args = try arguments.toOwnedSlice(self.allocator),
            .return_type = return_type,
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

        if (!self.scope.return_type.eql(self.void_type)) {
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
            .index_exp => {
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

    //todo: named fn calls
    fn evalFnCall(self: *Self, fn_call: *const ast.FnCall, loc_start: usize) !*ir.Value {
        const func_symbol = self.scope.symbol_table.getOrThrow(fn_call.identifier) catch {
            return self.err_dispatcher.notDefined(fn_call.identifier, loc_start);
        };

        if (!func_symbol.type.eql(self.type_fn)) {
            return self.err_dispatcher.invalidType(
                "function",
                try func_symbol.type.name(self.allocator),
                loc_start,
            );
        }

        var fn_call_arguments_values: std.ArrayList(*ir.Value) = .empty;

        if (func_symbol.metadata) |fn_data| {
            const args_len = fn_call.arguments.len;
            const fn_params_len = fn_data.params.len;

            if (fn_params_len != args_len) {
                return self.err_dispatcher.invalidNumberOfArgs(args_len, fn_params_len, loc_start);
            }

            for (fn_call.arguments, 0..) |arg, i| {
                const fn_call_arg_value = try self.evalExp(arg.value);

                const param_type = fn_data.params[i];
                const arg_type = self.resolveValueType(fn_call_arg_value);
                if (!arg_type.eql(param_type)) {
                    return self.err_dispatcher.invalidType(try param_type.name(self.allocator), try arg_type.name(self.allocator), loc_start);
                }

                try fn_call_arguments_values.append(self.allocator, fn_call_arg_value);
            }

            return ir.Value.init(self.allocator, .{ .fn_call = .{ .fn_uid = func_symbol.uid, .identifier = func_symbol.identifier, .return_type = fn_data.return_type, .args = try fn_call_arguments_values.toOwnedSlice(self.allocator) } });
        }

        unreachable;
    }

    fn evalIndexedExp(self: *Self, exp: *const ast.ExpNode) !*ir.Value {
        const index_exp = exp.data.index_exp;
        const target = try self.evalExp(index_exp.target);
        const target_type = self.resolveValueType(target);

        if (target.* != .identifier) {
            return self.err_dispatcher.invalidExpression("identifier", @tagName(target.*), exp.loc_start);
        }

        if (target_type.* != .array) {
            return self.err_dispatcher.invalidType("array", try target_type.name(self.allocator), exp.loc_start);
        }

        const index = try self.evalExp(index_exp.index);
        const index_type = self.resolveValueType(index);

        if (!index_type.eql(self.number_type)) {
            return self.err_dispatcher.invalidIndexing("number", @tagName(index.*), exp.loc_start);
        }

        return ir.Value.init(self.allocator, .{
            .index_exp = .{
                .target = target,
                .index = index,
            },
        });
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
        var array_literal_type: *Type = self.anytype_type;

        for (array_literal.exps) |e| {
            const exp_value = try self.evalExp(e);
            const exp_value_type = self.resolveValueType(exp_value);

            if (array_literal_type.eql(self.void_type)) {
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
                if (!exp_type.eql(self.number_type)) {
                    return self.err_dispatcher.invalidType(
                        "number",
                        try exp_type.name(self.allocator),
                        exp.loc_start,
                    );
                }
            },
            .not => {
                if (!exp_type.eql(self.bool_type)) {
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

        var op_type: *Type = self.void_type;

        if (!left_type.eql(right_type)) {
            return self.err_dispatcher.invalidType(
                try left_type.name(self.allocator),
                try right_type.name(self.allocator),
                exp.loc_start,
            );
        }

        switch (bin_exp.op) {
            .add, .sub, .mult, .div, .mod => {
                if (!left_type.eql(self.number_type)) {
                    return self.err_dispatcher.invalidType(
                        "number",
                        try left_type.name(self.allocator),
                        exp.loc_start,
                    );
                }
                op_type = self.number_type;
            },

            //warn: this makes support string comparison by operators
            .eq, .not_eq, .lt, .lt_or_eq, .gt, .gt_or_eq => {
                if (left_type.eql(self.bool_type)) {
                    return self.err_dispatcher.invalidType(
                        "string, number",
                        try left_type.name(self.allocator),
                        exp.loc_start,
                    );
                }
                op_type = self.bool_type;
            },

            .bool_or, .bool_and => {
                if (!left_type.eql(self.bool_type)) {
                    return self.err_dispatcher.invalidType(
                        "boolean",
                        try left_type.name(self.allocator),
                        exp.loc_start,
                    );
                }
                op_type = self.bool_type;
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
                if (std.mem.eql(u8, type_annotation.name, "number")) return self.number_type;
                if (std.mem.eql(u8, type_annotation.name, "string")) return self.str_type;
                if (std.mem.eql(u8, type_annotation.name, "bool")) return self.bool_type;
                if (std.mem.eql(u8, type_annotation.name, "fn")) return self.type_fn;
                if (std.mem.eql(u8, type_annotation.name, "void")) return Type.init(self.allocator, .void);
                if (std.mem.eql(u8, type_annotation.name, "_anytype")) return self.anytype_type;

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
        return switch (value.*) {
            .i_float => self.number_type,
            .i_bool => self.bool_type,
            .i_string => self.str_type,
            .i_void => self.bool_type,
            .fn_def => self.type_fn,
            .struct_def => self.type_struct,
            .identifier => value.identifier.type,
            .binary_op => value.binary_op.type,
            .unary_op => value.unary_op.type,
            //warn: this should be a switch
            .index_exp => value.index_exp.target.identifier.type.array,
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

    pub fn init(allocator: std.mem.Allocator, return_type: *Type) !Self {
        return Self{ .allocator = allocator, .symbol_table = try SymbolTable.init(allocator, null), .return_type = return_type, .next_uid = 10 };
    }

    pub fn deinit(self: *Self) void {
        var current_ptr: ?*SymbolTable = self.symbol_table;
        while (current_ptr) |table| {
            const parent = table.parent;
            table.deinit();
            current_ptr = parent;
        }
    }

    pub fn genUid(self: *Self) usize {
        defer self.next_uid += 1;
        return self.next_uid;
    }

    pub fn enter(self: *Self, return_type: *Type) !void {
        self.return_type = return_type;
        self.symbol_table = try SymbolTable.init(self.allocator, self.symbol_table);
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
    symbols: std.StringHashMap(Symbol),

    pub fn init(allocator: std.mem.Allocator, parent: ?*SymbolTable) !*Self {
        const ptr = try allocator.create(Self);

        var symbols = std.StringHashMap(Symbol).init(allocator);

        for (built_in_functions) |func| {
            try symbols.put(func.symbol.identifier, func.symbol);
        }

        ptr.* = Self{ .allocator = allocator, .parent = parent, .symbols = symbols };
        return ptr;
    }

    fn put(self: *Self, symbol: Symbol) !void {
        if (self.symbols.get(symbol.identifier)) |_| {
            return Errors.SemaError;
        }

        try self.symbols.put(symbol.identifier, symbol);
    }

    fn remove(self: *Self, symbol: Symbol) !void {
        if (self.symbols.get(symbol.identifier)) |_| {
            return Errors.SemaError;
        }

        try self.symbols.remove(symbol.identifier);
    }

    fn replace(self: *Self, symbol: Symbol) !void {
        _ = try self.getOrThrow(symbol.identifier);
        try self.symbols.put(symbol.identifier, symbol);
    }

    fn get(self: *Self, id: []const u8) ?Symbol {
        if (self.symbols.get(id)) |s| return s;
        if (self.parent) |parent| {
            return parent.get(id);
        }
        return null;
    }

    fn getOrThrow(self: *Self, id: []const u8) !Symbol {
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
    metadata: ?SymbolMetadata,
};

pub const SymbolMetadata = union(enum) {
    func: FuncMetadata,
    struct_: StructMetadata,
};

const StructMetadata = struct {
    fields: []const StructFieldMetadata,
    functions: []const FuncMetadata,
};

const StructFieldMetadata = struct {
    identifier: []const u8,
    type: *Type,
};

const FuncMetadata = struct {
    params: []const FuncParamMetadata,
    return_type: *Type,
};

const FuncParamMetadata = struct {
    identifier: []const u8,
    type: *Type,
};

pub const Type = union(enum) {
    const Self = @This();

    number,
    string,
    boolean,
    function,
    struct_type,
    //currently only used for internals
    dynamic,
    void,
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
            .function => return other.* == .function,
            .struct_self => unreachable,
            .struct_type => unreachable,
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
            .dynamic => "anytype",
            .function => "function",
            .struct_type => "struct",
            .struct_self => "@",

            .array => |inner| {
                return std.fmt.allocPrint(allocator, "[]{s}", .{try inner.name(allocator)});
            },
        };
    }
};

pub const Errors = err.Errors;

const built_in_functions = builtin.built_in_functions;
const builtin = @import("built-in.zig");
const err = @import("error.zig");
const ir = @import("ir.zig");
const ast = @import("ast.zig");
const allocPrint = std.fmt.allocPrint;
const std = @import("std");
