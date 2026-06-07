const std = @import("std");
const util = @import("util.zig");
const buildin = @import("built-in.zig");
const err = @import("error.zig");
const ir = @import("ir.zig");
const ast = @import("ast.zig");

const allocPrint = std.fmt.allocPrint;

pub const SemaAnalyzer = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    src: []const u8 = "",

    functions: std.ArrayList(ir.Func),
    structs: std.ArrayList(ir.Struct),

    type_table: TypeTable,
    scope: Scope,
    err_dispatcher: err.ErrorDispatcher,

    pub fn init(allocator: std.mem.Allocator) !Self {
        var type_table = try TypeTable.init(allocator);

        const void_type_id = type_table.getPrimitive(.void);

        var self = Self{
            .allocator = allocator,
            .type_table = type_table,
            .scope = undefined,
            .err_dispatcher = .{ .allocator = allocator, .src = "" },
            .functions = .empty,
            .structs = .empty,
        };

        self.scope = try Scope.init(allocator, void_type_id, &self.type_table);

        return self;
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
                    self.scope.symbol_table.put(symbol) catch return self.err_dispatcher.alreadyDefined(let_stmt.identifier, let_stmt.value.loc);
                },
                .struct_def => {
                    const symbol = try self.createIncompleteStructSymbol(let_stmt.identifier);
                    self.scope.symbol_table.put(symbol) catch return self.err_dispatcher.alreadyDefined(let_stmt.identifier, let_stmt.value.loc);
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
                    const symbol = try self.scope.symbol_table.getOrError(let_stmt.identifier);
                    try self.fulfillFuncType(symbol, let_stmt.value, null);
                },
                .struct_def => {
                    const symbol = try self.scope.symbol_table.getOrError(let_stmt.identifier);
                    try self.fulfillStructType(
                        symbol,
                        let_stmt.value,
                    );
                },
                else => unreachable,
            }
        }
    }

    fn createIncompleteFuncSymbol(self: *Self, identifier: []const u8) !Symbol {
        var symbol = Symbol{
            .identifier = identifier,
            .uid = self.scope.genUid(),
            .kind = .{ .function = {} },
            .type_id = undefined,
        };

        symbol.type_id = try self.type_table.addType(.{
            .kind = .{
                .function = .{
                    .identifier = identifier,
                    .uid = self.scope.genFnUid(),
                    .params = &.{},
                    .return_type_id = undefined,
                },
            },
            .nullable = false,
        });

        return symbol;
    }

    fn createIncompleteStructSymbol(self: *Self, identifier: []const u8) !Symbol {
        const blueprint_type_id = try self.type_table.addType(.{
            .kind = .{
                .@"struct" = .{
                    .identifier = identifier,
                    .fields_in_order = &.{},
                    .fields = .init(self.allocator),
                    .static_fields = .init(self.allocator),
                    .methods = .init(self.allocator),
                },
            },
            .nullable = false,
        });

        return Symbol{
            .identifier = identifier,
            .uid = self.scope.genFnUid(),
            .type_id = self.type_table.getPrimitive(.meta),
            .kind = .{ .@"struct" = .{ .blueprint_type_id = blueprint_type_id } },
        };
    }

    fn createAnonymousStructFromFnCall(self: *Self, exp: *const ast.ExpNode, is_mut: bool) !Struct {
        const fn_call = exp.data.fn_call;
        var fields_in_order: std.ArrayList(TypedIdentifier) = .empty;
        var fields: std.StringHashMap(TypedIdentifier) = .init(self.allocator);

        if (fn_call.are_arguments_named == false) {
            return self.err_dispatcher.cantInferAnonymousStruct(exp.loc);
        }

        for (fn_call.arguments) |arg| {
            const exp_value = try self.evalExp(arg.exp);
            const field_type_id = exp_value.type_id;
            const field: TypedIdentifier = .{
                .identifier = arg.identifier.?,
                .is_mut = is_mut,
                .type_id = field_type_id,
                .default_value = exp_value,
            };
            try fields_in_order.append(
                self.allocator,
                field,
            );
            try fields.put(arg.identifier.?, field);
        }

        return Struct{
            .identifier = "@",
            .fields = fields,
            .fields_in_order = try fields_in_order.toOwnedSlice(self.allocator),
            .static_fields = .init(self.allocator),
            .methods = .init(self.allocator),
        };
    }

    fn fulfillStructType(self: *Self, struct_symbol: Symbol, exp: *const ast.ExpNode) !void {
        const struct_def = exp.data.struct_def;
        const blueprint_type_id = struct_symbol.kind.@"struct".blueprint_type_id;
        var fields_in_order: std.ArrayList(TypedIdentifier) = .empty;
        var fields: std.StringHashMap(TypedIdentifier) = .init(self.allocator);
        var static_fields: std.StringHashMap(TypedIdentifier) = .init(self.allocator);
        var methods: std.StringHashMap(TypeId) = .init(self.allocator);

        for (struct_def.static_fields) |field| {
            const struct_field = try self.createStructField(field, blueprint_type_id);
            try static_fields.put(field.identifier, struct_field);
        }

        for (struct_def.fields) |field| {
            const struct_field = try self.createStructField(field, blueprint_type_id);
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
            try methods.put(func.identifier, fn_symbol.type_id);
        }

        const struct_type_ptr = self.type_table.getTypePtrById(blueprint_type_id);
        struct_type_ptr.kind.@"struct".fields_in_order = try fields_in_order.toOwnedSlice(self.allocator);
        struct_type_ptr.kind.@"struct".fields = fields;
        struct_type_ptr.kind.@"struct".static_fields = static_fields;
        struct_type_ptr.kind.@"struct".methods = methods;
    }

    fn createStructField(self: *Self, field: ast.StructField, current_struct_type_id: ?TypeId) Errors!TypedIdentifier {
        var field_type_id: TypeId = undefined;
        var field_default_value: ?*ir.Value = null;

        if (field.type) |_type| {
            field_type_id = try self.resolveTypeAnnotation(_type, current_struct_type_id);
        }

        if (field.default_value) |initial_value| {
            const value = try self.evalExp(initial_value);
            const value_type_id = value.type_id;
            field_default_value = value;

            if (field.type) |_type| {
                if (!self.type_table.coerce(value_type_id, field_type_id)) {
                    const field_type_ptr = self.type_table.getTypePtrById(field_type_id);
                    return self.err_dispatcher.invalidType(
                        try _type.value(self.allocator),
                        @tagName(field_type_ptr.kind),
                        field.default_value.?.loc,
                    );
                }
            }

            field_type_id = value.type_id;
        }

        return TypedIdentifier{
            .identifier = field.identifier,
            .is_mut = field.is_mut,
            .type_id = field_type_id,
            .default_value = field_default_value,
        };
    }

    fn fulfillFuncType(self: *Self, fn_symbol: Symbol, exp: *ast.ExpNode, struct_symbol: ?Symbol) !void {
        const fn_def = exp.data.fn_def;
        var return_type_id: TypeId = undefined;
        var params_metadata: std.ArrayList(TypedIdentifier) = .empty;

        //create a temporary scope just for inline return type inference
        try self.scope.enter(self.type_table.getPrimitive(.void));

        for (fn_def.params) |param| {
            var param_default_value: ?*ir.Value = null;
            var param_type_id: TypeId = undefined;

            if (param.type_annotation) |annotation| {
                param_type_id = self.resolveTypeAnnotation(annotation, if (struct_symbol) |symbol| symbol.kind.@"struct".blueprint_type_id else null) catch {
                    return self.err_dispatcher.typeNotDefined(
                        try param.type_annotation.?.value(self.allocator),
                        exp.loc,
                    );
                };

                if (param.default_value) |default_value| {
                    const value = try self.evalExp(default_value);
                    param_default_value = value;
                    const value_type_id = value.type_id;
                    if (!self.type_table.coerce(value_type_id, param_type_id)) {
                        return self.err_dispatcher.invalidType(
                            try self.type_table.name(param_type_id, self.allocator),
                            try self.type_table.name(value_type_id, self.allocator),
                            param.default_value.?.loc,
                        );
                    }
                }
            } else {
                if (param.default_value) |default_value| {
                    const value = try self.evalExp(default_value);
                    param_default_value = value;
                    param_type_id = value.type_id;
                } else {
                    return self.err_dispatcher.invalidParameterType(param.identifier, exp.loc);
                }
            }

            self.scope.symbol_table.put(
                .{
                    .uid = self.scope.genUid(),
                    .identifier = param.identifier,
                    .type_id = param_type_id,
                    .kind = .{ .variable = .{ .is_mut = param.is_mut } },
                },
            ) catch {
                return self.err_dispatcher.alreadyDefined(param.identifier, exp.loc);
            };

            try params_metadata.append(self.allocator, .{
                .identifier = param.identifier,
                .type_id = param_type_id,
                .is_mut = param.is_mut,
                .default_value = param_default_value,
            });
        }

        if (fn_def.return_type) |r_type| {
            return_type_id = self.resolveTypeAnnotation(r_type, null) catch {
                return self.err_dispatcher.typeNotDefined(r_type.type.primitive, exp.loc);
            };
        } else if (fn_def.body_block.statements.items[0].data.return_stmt.exp) |return_exp| {
            const return_value = try self.evalExp(return_exp);
            return_type_id = return_value.type_id;
        } else {
            return_type_id = self.type_table.getPrimitive(.void);
        }

        //restore main scope
        self.scope.exit(self.type_table.getPrimitive(.void));

        self.type_table.getTypePtrById(fn_symbol.type_id).kind.function.return_type_id = return_type_id;
        self.type_table.getTypePtrById(fn_symbol.type_id).kind.function.params = try params_metadata.toOwnedSlice(self.allocator);
    }

    pub fn visitBlock(self: *Self, block: *const ast.Block) !ir.Block {
        var instructions: std.ArrayList(ir.Instruction) = .empty;
        try self.scope.enter(self.scope.return_type_id);
        defer self.scope.exit(self.scope.return_type_id);

        var return_type: TypeId = self.type_table.getPrimitive(.void);

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
                    return_type = self.scope.return_type_id;
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
            return self.err_dispatcher.invalidType("function", @tagName(expression_stmt.data), expression_stmt.loc);
        }

        return ir.Instruction{
            .expression_stmt = .{ .value = try self.evalFnCall(expression_stmt) },
        };
    }

    fn visitLetStmt(self: *Self, stmt: *const ast.StatementNode) !?ir.Instruction {
        var var_type_id: TypeId = undefined;
        const let_stmt = stmt.data.let_stmt;

        if (let_stmt.value.data == .fn_def) {
            const fn_symbol = self.scope.symbol_table.getOrError(let_stmt.identifier) catch {
                return self.err_dispatcher.notDefined(let_stmt.identifier, stmt.loc);
            };
            const func = try self.visitFnDef(let_stmt.value, fn_symbol.uid, self.type_table.getTypePtrById(fn_symbol.type_id).kind.function, null);
            try self.functions.append(self.allocator, func);
            return null;
        }

        if (let_stmt.value.data == .struct_def) {
            const struct_def = try self.visitStructDef(let_stmt.value, let_stmt.identifier);
            try self.structs.append(self.allocator, struct_def);
            return null;
        }

        const expression_value = try self.evalExp(let_stmt.value);
        const expression_type_id = expression_value.type_id;

        if (let_stmt.type_annotation) |type_annotation| {
            var_type_id = try self.resolveTypeAnnotation(type_annotation, null);
        } else {
            var_type_id = expression_type_id;
            const var_type_ptr = self.type_table.getTypePtrById(var_type_id);
            if (var_type_ptr.kind == .array and self.type_table.getTypePtrById(var_type_ptr.kind.array).kind == .dynamic) {
                return self.err_dispatcher.cantInferArrayLiteralType(let_stmt.value.loc);
            }
        }

        if (!self.type_table.coerce(expression_type_id, var_type_id)) {
            return self.err_dispatcher.invalidType(
                try self.type_table.name(var_type_id, self.allocator),
                try self.type_table.name(expression_type_id, self.allocator),
                stmt.data.let_stmt.value.loc,
            );
        }

        const uid = self.scope.genUid();

        self.scope.symbol_table.put(.{
            .identifier = let_stmt.identifier,
            .uid = uid,
            .type_id = var_type_id,
            .kind = .{ .variable = .{ .is_mut = let_stmt.is_mut } },
        }) catch {
            return self.err_dispatcher.alreadyDefined(let_stmt.identifier, stmt.loc);
        };

        return ir.Instruction{ .store_var = .{
            .uid = uid,
            .identifier = let_stmt.identifier,
            .type_id = var_type_id,
            .value = expression_value,
        } };
    }

    fn visitIfStmt(self: *Self, stmt: *const ast.StatementNode) Errors!ir.Instruction {
        const if_stmt = stmt.data.if_stmt;

        const condition_value = try self.evalExp(if_stmt.condition);
        const condition_value_type = condition_value.type_id;

        if (!self.type_table.eql(condition_value_type, self.type_table.getPrimitive(.boolean))) {
            return self.err_dispatcher.invalidType("boolean", try self.type_table.name(condition_value_type, self.allocator), if_stmt.condition.loc);
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
        const exp_type = exp_value.type_id;
        var instructions: std.ArrayList(ir.Instruction) = .empty;

        if (self.type_table.getTypePtrById(exp_type).nullable == false) {
            return self.err_dispatcher.invalidType("nullable", try self.type_table.name(exp_type, self.allocator), if_capture_stmt.exp.loc);
        }

        if (if_capture_stmt.identifier.is_mut and !self.isExpressionMutable(if_capture_stmt.exp)) {
            return self.err_dispatcher.unwrappedValueCantBeMutable(if_capture_stmt.identifier.name, stmt.loc);
        }

        const captured_type_id = self.type_table.typeIdByNullableTypeId.get(exp_type).?;

        const aux_var_id = self.scope.genUid();
        const aux_var_name = try std.fmt.allocPrint(self.allocator, "$captured_${d}", .{aux_var_id});

        const store_aux_var_inst = ir.Instruction{ .store_var = .{
            .uid = aux_var_id,
            .identifier = aux_var_name,
            .type_id = captured_type_id,
            .value = exp_value,
        } };

        const aux_var_value = try ir.Value.init(self.allocator, .{
            .data = .{
                .identifier = .{
                    .uid = aux_var_id,
                    .identifier = aux_var_name,
                },
            },
            .type_id = captured_type_id,
        });

        const captured_symbol = Symbol{
            .uid = self.scope.genUid(),
            .identifier = if_capture_stmt.identifier.name,
            .type_id = captured_type_id,
            .kind = .{ .variable = .{ .is_mut = if_capture_stmt.identifier.is_mut } },
        };

        try self.scope.symbol_table.put(captured_symbol);
        defer _ = self.scope.symbol_table.remove(captured_symbol.identifier);

        const store_captured_var_inst = ir.Instruction{ .store_var = .{
            .uid = captured_symbol.uid,
            .identifier = captured_symbol.identifier,
            .type_id = captured_type_id,
            .value = aux_var_value,
        } };

        const check_null_condition = try ir.Value.init(
            self.allocator,
            .{
                .data = .{
                    .binary_op = .{
                        .kind = .b_cmp_neq,
                        .left = aux_var_value,
                        .right = try ir.Value.init(self.allocator, .{
                            .data = .{ .i_null = {} },
                            .type_id = self.type_table.getPrimitive(.null),
                        }),
                    },
                },
                .type_id = self.type_table.getPrimitive(.boolean),
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
                const id_symbol = self.scope.symbol_table.getOrError(id) catch {
                    return self.err_dispatcher.notDefined(id, assign_stmt.target.loc);
                };

                const assignment_value = try self.evalExp(assign_stmt.exp);
                const assignment_value_type = assignment_value.type_id;

                if (!id_symbol.kind.variable.is_mut) {
                    return self.err_dispatcher.notMutable(id, assign_stmt.target.loc);
                }

                if (!self.type_table.coerce(assignment_value_type, id_symbol.type_id)) {
                    return self.err_dispatcher.invalidType(
                        try self.type_table.name(id_symbol.type_id, self.allocator),
                        try self.type_table.name(assignment_value_type, self.allocator),
                        assign_stmt.exp.loc,
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
                const target_type = target.type_id;
                const assignment_value = try self.evalExp(assign_stmt.exp);
                const assignment_value_type = assignment_value.type_id;

                var target_root = index_exp.target;
                while (target_root.data == .indexed) {
                    target_root = target_root.data.indexed.target;
                }

                const target_symbol = self.scope.symbol_table.getOrError(target_root.data.identifier) catch {
                    return self.err_dispatcher.notDefined(target_root.data.identifier, target_root.loc);
                };

                if (target_symbol.kind == .variable and !target_symbol.kind.variable.is_mut and self.type_table.getTypePtrById(target_type).kind != .@"struct") {
                    return self.err_dispatcher.notMutable(target_root.data.identifier, target_root.loc);
                }

                switch (self.type_table.getTypePtrById(target_type).kind) {
                    .array => {
                        if (!self.type_table.coerce(assignment_value_type, self.type_table.getTypePtrById(target_symbol.type_id).kind.array)) {
                            return self.err_dispatcher.invalidType(
                                try self.type_table.name(target_symbol.type_id, self.allocator),
                                try self.type_table.name(assignment_value_type, self.allocator),
                                assign_stmt.exp.loc,
                            );
                        }
                    },
                    .@"struct" => {
                        const struct_type_ptr = self.type_table.getTypePtrById(target_type);
                        const struct_def = &struct_type_ptr.kind.@"struct";

                        if (target_symbol.kind == .variable) {
                            const field = struct_def.fields.get(index_exp.index.data.identifier) orelse return self.err_dispatcher.invalidStructField(
                                struct_def.identifier,
                                index_exp.index.data.identifier,
                                index_exp.index.loc,
                            );

                            if (!self.type_table.coerce(assignment_value_type, field.type_id)) {
                                return self.err_dispatcher.invalidType(
                                    try self.type_table.name(field.type_id, self.allocator),
                                    try self.type_table.name(assignment_value_type, self.allocator),
                                    assign_stmt.exp.loc,
                                );
                            }
                        } else {
                            const field = struct_def.static_fields.get(index_exp.index.data.identifier) orelse return self.err_dispatcher.invalidStaticStructField(
                                struct_def.identifier,
                                index_exp.index.data.identifier,
                                index_exp.index.loc,
                            );

                            if (!field.is_mut) {
                                return self.err_dispatcher.notMutable(field.identifier, index_exp.index.loc);
                            }

                            if (!self.type_table.coerce(assignment_value_type, field.type_id)) {
                                return self.err_dispatcher.invalidType(
                                    try self.type_table.name(field.type_id, self.allocator),
                                    try self.type_table.name(assignment_value_type, self.allocator),
                                    assign_stmt.exp.loc,
                                );
                            }
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
                    assign_stmt.target.loc,
                );
            },
        }
    }

    fn visitForStmt(self: *Self, stmt: *const ast.StatementNode) Errors!ir.Instruction {
        const for_stmt = stmt.data.for_stmt;

        const condition_value = try self.evalExp(for_stmt.condition);
        const condition_value_type = condition_value.type_id;

        if (!self.type_table.eql(condition_value_type, self.type_table.getPrimitive(.boolean))) {
            return self.err_dispatcher.invalidType(
                "boolean",
                try self.type_table.name(condition_value_type, self.allocator),
                for_stmt.condition.loc,
            );
        }

        const block = try self.visitBlock(&for_stmt.do_block);

        return ir.Instruction{ .loop = .{
            .condition = condition_value,
            .do_block = block.instructions,
        } };
    }

    fn visitAnonymousStructDef(self: *Self, struct_def: *const ast.Struct) !Struct {
        var fields_in_order: std.ArrayList(TypedIdentifier) = .empty;
        var fields: std.StringHashMap(TypedIdentifier) = .init(self.allocator);

        for (struct_def.fields) |field| {
            const struct_field = try self.createStructField(field, null);
            try fields_in_order.append(
                self.allocator,
                struct_field,
            );
            try fields.put(field.identifier, struct_field);
        }

        return .{
            .identifier = "@",
            .fields = fields,
            .fields_in_order = try fields_in_order.toOwnedSlice(self.allocator),
            .static_fields = .init(self.allocator),
            .methods = .init(self.allocator),
        };
    }

    fn visitStructDef(self: *Self, exp: *const ast.ExpNode, identifier: []const u8) Errors!ir.Struct {
        const struct_symbol = try self.scope.symbol_table.getOrError(identifier);
        const blueprint_type_id = struct_symbol.kind.@"struct".blueprint_type_id;
        const prev_return_type = self.scope.return_type_id;
        try self.scope.enter(self.type_table.getPrimitive(.void));
        defer self.scope.exit(prev_return_type);

        const struct_def = exp.data.struct_def;
        var fields: std.ArrayList(ir.StructField) = .empty;
        var static_fields: std.ArrayList(ir.StructField) = .empty;
        var funcs: std.ArrayList(ir.Func) = .empty;

        for (struct_def.fields) |field| {
            const default_value: ?*ir.Value = if (field.default_value) |_value| try self.evalExp(_value) else null;
            const field_type = if (field.type) |_type| try self.resolveTypeAnnotation(_type, blueprint_type_id) else default_value.?.type_id;

            try fields.append(
                self.allocator,
                .{
                    .identifier = field.identifier,
                    .type_id = field_type,
                    .default_value = default_value,
                },
            );
        }

        for (struct_def.static_fields) |field| {
            const default_value: ?*ir.Value = if (field.default_value) |_value| try self.evalExp(_value) else null;
            const field_type = if (field.type) |_type| try self.resolveTypeAnnotation(_type, blueprint_type_id) else default_value.?.type_id;

            try static_fields.append(
                self.allocator,
                .{
                    .identifier = field.identifier,
                    .type_id = field_type,
                    .default_value = default_value,
                },
            );
        }

        for (struct_def.funcs) |func| {
            const fn_type_id = if (self.type_table.getTypePtrById(blueprint_type_id).kind.@"struct".methods.get(func.identifier)) |type_id| type_id else unreachable;
            const fn_metadata = self.type_table.getTypePtrById(fn_type_id).kind.function;
            const fn_name = func.identifier;
            const fn_def = try self.visitFnDef(func.def, fn_metadata.uid, fn_metadata, fn_name);
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

        const old_return_type = self.scope.return_type_id;
        try self.scope.enter(metadata.return_type_id);
        defer self.scope.exit(old_return_type);

        for (metadata.params) |param| {
            const arg_uid = self.scope.genUid();

            if (param.is_mut and self.type_table.getTypePtrById(param.type_id).kind != .array and self.type_table.getTypePtrById(param.type_id).kind != .@"struct") {
                return self.err_dispatcher.primitiveParamsCantBeMutable(exp.loc);
            }

            try self.scope.symbol_table.put(.{
                .uid = arg_uid,
                .identifier = param.identifier,
                .type_id = param.type_id,
                .kind = .{ .variable = .{ .is_mut = param.is_mut } },
            });

            try params.append(self.allocator, .{
                .uid = arg_uid,
                .identifier = param.identifier,
                .type_id = param.type_id,
                .default_value = param.default_value,
            });
        }

        const body = try self.visitBlock(&fn_def.body_block);

        if (!self.type_table.coerce(body.return_type, self.scope.return_type_id)) {
            return self.err_dispatcher.invalidFunctionReturnType(
                try self.type_table.name(self.scope.return_type_id, self.allocator),
                try self.type_table.name(body.return_type, self.allocator),
                exp.loc,
            );
        }

        return ir.Func{
            .uid = uid,
            .identifier = if (override_name) |name| name else metadata.identifier,
            .params = try params.toOwnedSlice(self.allocator),
            .return_type = metadata.return_type_id,
            .body = body.instructions,
        };
    }

    fn visitReturnStmt(self: *Self, stmt: *const ast.StatementNode) !ir.Instruction {
        const return_stmt = stmt.data.return_stmt;

        if (return_stmt.exp) |exp| {
            const exp_value = try self.evalExp(exp);
            const exp_value_type = exp_value.type_id;

            if (!self.type_table.coerce(exp_value_type, self.scope.return_type_id)) {
                return self.err_dispatcher.invalidFunctionReturnType(
                    try self.type_table.name(self.scope.return_type_id, self.allocator),
                    try self.type_table.name(exp_value_type, self.allocator),
                    exp.loc,
                );
            }

            return ir.Instruction{ .return_stmt = .{
                .value = exp_value,
            } };
        }

        if (!self.type_table.eql(self.scope.return_type_id, self.type_table.getPrimitive(.void))) {
            return self.err_dispatcher.invalidFunctionReturnType(
                try self.type_table.name(self.scope.return_type_id, self.allocator),
                "void",
                stmt.loc,
            );
        }

        return ir.Instruction{ .return_stmt = .{
            .value = null,
        } };
    }

    fn evalExp(self: *Self, exp: *const ast.ExpNode) Errors!*ir.Value {
        switch (exp.*.data) {
            .float_literal => {
                return ir.Value.init(self.allocator, .{ .data = .{ .i_float = exp.data.float_literal }, .type_id = self.type_table.getPrimitive(.float) });
            },
            .int_literal => {
                return ir.Value.init(self.allocator, .{ .data = .{ .i_int = exp.data.int_literal }, .type_id = self.type_table.getPrimitive(.int) });
            },
            .string_literal => {
                return ir.Value.init(self.allocator, .{ .data = .{ .i_string = exp.data.string_literal }, .type_id = self.type_table.getPrimitive(.string) });
            },
            .bool_literal => {
                return ir.Value.init(self.allocator, .{ .data = .{ .i_bool = exp.data.bool_literal }, .type_id = self.type_table.getPrimitive(.boolean) });
            },
            .null_literal => {
                return ir.Value.init(self.allocator, .{ .data = .{ .i_null = {} }, .type_id = self.type_table.getPrimitive(.null) });
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
            .indexed => {
                return self.evalIndexedExp(exp);
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
        var return_type: TypeId = undefined;

        var arg_exp_by_param_name = std.StringHashMap(*ast.ExpNode).init(self.allocator);
        defer arg_exp_by_param_name.deinit();

        var fn_call_arguments_values: std.ArrayList(*ir.Value) = .empty;

        switch (fn_call.target.data) {
            .identifier => |id| {
                const symbol = self.scope.symbol_table.getOrError(id) catch {
                    return self.err_dispatcher.notDefined(id, exp.loc);
                };

                fn_identifier = symbol.identifier;
                uid = symbol.uid;

                switch (symbol.kind) {
                    .function => {
                        const func = self.type_table.getTypePtrById(symbol.type_id).kind.function;
                        params = func.params;
                        return_type = func.return_type_id;
                    },
                    //when identifier is a struct we turn it into a struct inicialization
                    .@"struct" => |scope| {
                        const struct_def = self.type_table.getTypePtrById(scope.blueprint_type_id).kind.@"struct";
                        params = struct_def.fields_in_order;
                        return_type = scope.blueprint_type_id;
                    },
                    .variable => {
                        const type_ptr = self.type_table.getTypePtrById(symbol.type_id);
                        if (type_ptr.kind == .function) {
                            const func = type_ptr.kind.function;
                            params = func.params;
                            return_type = func.return_type_id;
                        } else if (type_ptr.kind == .@"struct") {
                            const struct_def = type_ptr.kind.@"struct";
                            params = struct_def.fields_in_order;
                            return_type = symbol.type_id;
                        } else {
                            return self.err_dispatcher.invalidType(
                                "function or struct",
                                try self.type_table.name(symbol.type_id, self.allocator),
                                exp.loc,
                            );
                        }
                    },
                }
            },
            //note: not supporting functions stored in arrays
            .indexed => |indexed| {
                const target = try self.evalExp(indexed.target);
                const target_type_id = target.type_id;
                const target_type_ptr = self.type_table.getTypePtrById(target_type_id);

                if (target_type_ptr.kind != .@"struct") {
                    return self.err_dispatcher.invalidType(
                        "a struct",
                        try self.type_table.name(target_type_id, self.allocator),
                        exp.loc,
                    );
                }

                const fn_name = indexed.index.data.identifier;

                const struct_def = target_type_ptr.kind.@"struct";

                const fn_type_id = struct_def.methods.get(fn_name) orelse {
                    return self.err_dispatcher.invalidStructFunction(
                        struct_def.identifier,
                        fn_name,
                        exp.loc,
                    );
                };

                const fn_metadata = self.type_table.getTypePtrById(fn_type_id).kind.function;

                const target_symbol = try self.scope.symbol_table.getOrError(target.data.identifier.identifier);

                //autobind self
                if (fn_metadata.params.len > 0 and target_symbol.kind == .variable and target_type_id == fn_metadata.params[0].type_id) {
                    if (fn_metadata.params[0].is_mut and !target_symbol.kind.variable.is_mut) {
                        return self.err_dispatcher.notMutable(indexed.target.data.identifier, indexed.target.loc);
                    }
                    try fn_call_arguments_values.append(self.allocator, target);
                }

                params = fn_metadata.params;
                return_type = fn_metadata.return_type_id;
                uid = fn_metadata.uid;
                fn_identifier = fn_name;
            },
            .anonymous_struct_identifier => {
                const anonymous_struct = try self.createAnonymousStructFromFnCall(exp, false);
                params = anonymous_struct.fields_in_order;
                return_type = try self.type_table.getOrAddAnonymousStruct(anonymous_struct);
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
            return self.err_dispatcher.invalidNumberOfArgs(required_params_len, total_args_len, exp.loc);
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
                const arg_type = fn_call_arg_value.type_id;

                if (!self.type_table.coerce(arg_type, param.type_id)) {
                    return self.err_dispatcher.invalidType(try self.type_table.name(param.type_id, self.allocator), try self.type_table.name(arg_type, self.allocator), _exp.loc);
                }

                if (self.type_table.getTypePtrById(param.type_id).kind == .@"struct" and param.is_mut and fn_call_arg_value.data == .identifier) {
                    const symbol = try self.scope.symbol_table.getOrError(fn_call_arg_value.data.identifier.identifier);
                    if (!symbol.kind.variable.is_mut) {
                        return self.err_dispatcher.notMutable(symbol.identifier, _exp.loc);
                    }
                }

                if (self.type_table.getTypePtrById(param.type_id).kind == .array and param.is_mut and _exp.data != .array_literal) {
                    if (fn_call_arg_value.data == .identifier) {
                        const symbol = try self.scope.symbol_table.getOrError(fn_call_arg_value.data.identifier.identifier);
                        if (!symbol.kind.variable.is_mut) {
                            return self.err_dispatcher.notMutable(fn_call_arg_value.data.identifier.identifier, _exp.loc);
                        }
                    }
                }
                try fn_call_arguments_values.append(self.allocator, fn_call_arg_value);
            } else if (param.default_value) |value| {
                try fn_call_arguments_values.append(self.allocator, value);
            } else {
                return self.err_dispatcher.missingArgument(param.identifier, exp.loc);
            }
        }

        const is_anonymous_struct_inicialization = fn_call.target.data == .anonymous_struct_identifier;

        if (is_anonymous_struct_inicialization) {
            var keys: std.ArrayList([]const u8) = .empty;
            for (params) |param| {
                try keys.append(self.allocator, param.identifier);
            }

            return ir.Value.init(
                self.allocator,
                .{
                    .data = .{ .struct_init = .{
                        .keys = try keys.toOwnedSlice(self.allocator),
                        .values = try fn_call_arguments_values.toOwnedSlice(self.allocator),
                    } },
                    .type_id = return_type,
                },
            );
        }

        return ir.Value.init(
            self.allocator,
            .{
                .data = .{
                    .fn_call = .{
                        .fn_uid = uid,
                        .identifier = fn_identifier,
                        .args = try fn_call_arguments_values.toOwnedSlice(self.allocator),
                    },
                },
                .type_id = return_type,
            },
        );
    }

    fn evalIndexedExp(self: *Self, exp: *const ast.ExpNode) !*ir.Value {
        const indexed_exp = exp.data.indexed;
        const is_nullable = indexed_exp.nullable;

        const target = try self.evalExp(indexed_exp.target);
        const target_type = target.type_id;

        if (!is_nullable and self.type_table.getTypePtrById(target_type).nullable) {
            return self.err_dispatcher.nullableMustBeUnwraped(try self.type_table.name(target_type, self.allocator), exp.loc);
        }

        if (is_nullable and !self.type_table.getTypePtrById(target_type).nullable) {
            return self.err_dispatcher.unnecessaryOptionalChain(try self.type_table.name(target_type, self.allocator), exp.loc);
        }

        switch (self.type_table.getTypePtrById(target_type).kind) {
            .array => {
                const index = try self.evalExp(indexed_exp.index);
                const index_type = index.type_id;

                if (!self.type_table.eql(index_type, self.type_table.getPrimitive(.int))) {
                    return self.err_dispatcher.invalidIndexing("int", try self.type_table.name(index_type, self.allocator), exp.loc);
                }

                const inner_type = self.type_table.getTypePtrById(target_type).kind.array;
                const result_type = if (is_nullable) try self.type_table.getOrAddNullable(inner_type) else inner_type;

                return ir.Value.init(self.allocator, .{
                    .data = .{ .indexed = .{ .target = target, .index = index } },
                    .type_id = result_type,
                });
            },
            .@"struct" => {
                const struct_def = self.type_table.getTypePtrById(target_type).kind.@"struct";
                return switch (indexed_exp.index.data) {
                    .identifier => |member_name| {
                        const field_member = struct_def.fields.get(member_name);
                        const static_field_member = struct_def.static_fields.get(member_name);
                        const method_member = struct_def.methods.get(member_name);

                        if (field_member == null and static_field_member == null and method_member == null) {
                            return self.err_dispatcher.invalidStructField(struct_def.identifier, member_name, exp.loc);
                        }

                        const field_type_id = if (field_member) |f| f.type_id else if (static_field_member) |sf| sf.type_id else method_member.?;
                        const result_type = if (is_nullable) try self.type_table.getOrAddNullable(field_type_id) else field_type_id;

                        return ir.Value.init(self.allocator, .{
                            .data = .{
                                .indexed = .{
                                    .target = target,
                                    .index = try ir.Value.init(self.allocator, .{
                                        .data = .{ .i_string = member_name },
                                        .type_id = self.type_table.getPrimitive(.string),
                                    }),
                                },
                            },
                            .type_id = result_type,
                        });
                    },
                    else => unreachable,
                };
            },

            else => {
                return self.err_dispatcher.invalidType("array or struct", try self.type_table.name(target_type, self.allocator), exp.loc);
            },
        }
    }

    fn evalIdentifier(self: *Self, exp: *const ast.ExpNode) !*ir.Value {
        const id = exp.data.identifier;
        const id_symbol = self.scope.symbol_table.get(id);

        if (id_symbol) |symbol| {
            const type_id = if (symbol.kind == .@"struct") symbol.kind.@"struct".blueprint_type_id else symbol.type_id;
            return ir.Value.init(self.allocator, .{
                .data = .{ .identifier = .{
                    .uid = symbol.uid,
                    .identifier = symbol.identifier,
                } },
                .type_id = type_id,
            });
        }

        return self.err_dispatcher.notDefined(id, exp.loc);
    }

    fn evalArrayLiteral(self: *Self, exp: *const ast.ExpNode) !*ir.Value {
        var values: std.ArrayList(*ir.Value) = .empty;
        const array_literal = exp.data.array_literal;
        var array_literal_type: TypeId = self.type_table.getPrimitive(.dynamic);

        for (array_literal.exps) |e| {
            const exp_value = try self.evalExp(e);
            const exp_value_type = exp_value.type_id;

            if (array_literal_type == self.type_table.getPrimitive(.dynamic)) {
                array_literal_type = exp_value_type;
            }

            if (!self.type_table.coerce(exp_value_type, array_literal_type)) {
                return self.err_dispatcher.invalidType(try self.type_table.name(array_literal_type, self.allocator), try self.type_table.name(exp_value_type, self.allocator), exp.loc);
            }

            try values.append(self.allocator, exp_value);
        }

        const array_type_id = try self.type_table.getOrAddArray(array_literal_type);

        return ir.Value.init(
            self.allocator,
            .{
                .data = .{
                    .i_array = .{
                        .values = try values.toOwnedSlice(self.allocator),
                    },
                },
                .type_id = array_type_id,
            },
        );
    }

    fn evalUnaryExp(self: *Self, exp: *const ast.ExpNode) !*ir.Value {
        const unary_exp = exp.data.unary_exp;
        const exp_value = try self.evalExp(unary_exp.right);
        const exp_type = exp_value.type_id;

        switch (unary_exp.op) {
            .neg => {
                if (!self.type_table.eql(exp_type, self.type_table.getPrimitive(.float)) and !self.type_table.eql(exp_type, self.type_table.getPrimitive(.int))) {
                    return self.err_dispatcher.invalidType(
                        "int or float",
                        try self.type_table.name(exp_type, self.allocator),
                        exp.loc,
                    );
                }
            },
            .not => {
                if (!self.type_table.eql(exp_type, self.type_table.getPrimitive(.boolean))) {
                    return self.err_dispatcher.invalidType(
                        "boolean",
                        try self.type_table.name(exp_type, self.allocator),
                        exp.loc,
                    );
                }
            },
        }

        return ir.Value.init(self.allocator, .{
            .data = .{ .unary_op = .{
                .kind = self.astUnaryOpToIrUnaryOpKind(unary_exp.op, exp_type),
                .right = exp_value,
            } },
            .type_id = exp_type,
        });
    }

    fn isTypeNumber(self: *Self, t: TypeId) bool {
        return self.type_table.eql(t, self.type_table.getPrimitive(.float)) or self.type_table.eql(t, self.type_table.getPrimitive(.int));
    }

    fn evalBinaryExp(self: *Self, exp: *const ast.ExpNode) !*ir.Value {
        const bin_exp = exp.data.binary_exp;
        const left_value = try self.evalExp(bin_exp.left);
        const right_value = try self.evalExp(bin_exp.right);
        const left_type = left_value.type_id;
        const right_type = right_value.type_id;

        var op_type: TypeId = self.type_table.getPrimitive(.void);

        switch (bin_exp.op) {
            .add, .sub, .mult, .mod, .div, .trunc_div => {
                if (!self.isTypeNumber(left_type) or !self.isTypeNumber(right_type)) {
                    return self.err_dispatcher.invalidType(
                        "int or float",
                        try self.type_table.name(left_type, self.allocator),
                        exp.loc,
                    );
                }

                op_type = switch (bin_exp.op) {
                    .div => self.type_table.getPrimitive(.float),
                    .trunc_div => self.type_table.getPrimitive(.int),
                    else => if (self.type_table.eql(left_type, self.type_table.getPrimitive(.float)) or self.type_table.eql(right_type, self.type_table.getPrimitive(.float))) self.type_table.getPrimitive(.float) else self.type_table.getPrimitive(.int),
                };
            },
            .lt, .lt_or_eq, .gt, .gt_or_eq => {
                if (!self.isTypeNumber(left_type) or !self.isTypeNumber(right_type)) {
                    return self.err_dispatcher.invalidType(
                        "int or float",
                        try self.type_table.name(left_type, self.allocator),
                        exp.loc,
                    );
                }
                op_type = self.type_table.getPrimitive(.boolean);
            },
            .bool_or, .bool_and => {
                if (!self.type_table.eql(left_type, self.type_table.getPrimitive(.boolean)) or !self.type_table.eql(right_type, self.type_table.getPrimitive(.boolean))) {
                    return self.err_dispatcher.invalidType(
                        "boolean",
                        try self.type_table.name(left_type, self.allocator),
                        exp.loc,
                    );
                }
                op_type = self.type_table.getPrimitive(.boolean);
            },
            //note: allowing everything
            .eq, .not_eq => {
                op_type = self.type_table.getPrimitive(.boolean);
            },
        }

        return ir.Value.init(self.allocator, .{
            .data = .{ .binary_op = .{
                .kind = self.astBinOpToIrBinOpKind(bin_exp.op, op_type, left_type, right_type),
                .left = left_value,
                .right = right_value,
            } },
            .type_id = op_type,
        });
    }

    fn isExpressionMutable(self: *Self, exp: *ast.ExpNode) bool {
        return switch (exp.data) {
            .identifier => |name| {
                const sym = self.scope.symbol_table.get(name) orelse return false;
                return sym.kind.variable.is_mut;
            },
            .indexed => |indexed| self.isExpressionMutable(indexed.target),
            else => false,
        };
    }

    pub fn resolveTypeAnnotation(self: *Self, type_annotation: *ast.TypeAnnotation, current_struct_type_id: ?TypeId) !TypeId {
        switch (type_annotation.type) {
            .primitive => |primitive_name| {
                if (std.mem.eql(u8, primitive_name, "float")) return if (type_annotation.nullable) try self.type_table.getOrAddNullable(self.type_table.getPrimitive(.float)) else self.type_table.getPrimitive(.float);
                if (std.mem.eql(u8, primitive_name, "int")) return if (type_annotation.nullable) try self.type_table.getOrAddNullable(self.type_table.getPrimitive(.int)) else self.type_table.getPrimitive(.int);
                if (std.mem.eql(u8, primitive_name, "string")) return if (type_annotation.nullable) try self.type_table.getOrAddNullable(self.type_table.getPrimitive(.string)) else self.type_table.getPrimitive(.string);
                if (std.mem.eql(u8, primitive_name, "bool")) return if (type_annotation.nullable) try self.type_table.getOrAddNullable(self.type_table.getPrimitive(.boolean)) else self.type_table.getPrimitive(.boolean);
                if (std.mem.eql(u8, primitive_name, "void")) return self.type_table.getPrimitive(.void);
                if (std.mem.eql(u8, primitive_name, "dynamic")) return if (type_annotation.nullable) try self.type_table.getOrAddNullable(self.type_table.getPrimitive(.dynamic)) else self.type_table.getPrimitive(.dynamic);

                return Errors.SemaError;
            },
            .@"struct" => |struct_name| {
                const struct_symbol = try self.scope.symbol_table.getOrError(struct_name);
                const blueprint_type_id = struct_symbol.kind.@"struct".blueprint_type_id;
                return if (type_annotation.nullable) try self.type_table.getOrAddNullable(blueprint_type_id) else blueprint_type_id;
            },
            .anonymous_struct => |struct_metadata| {
                const anon_struct = try self.visitAnonymousStructDef(&struct_metadata);
                const type_id = try self.type_table.getOrAddAnonymousStruct(anon_struct);
                return if (type_annotation.nullable) try self.type_table.getOrAddNullable(type_id) else type_id;
            },
            .array => {
                const inner_type_id = try self.resolveTypeAnnotation(type_annotation.type.array, current_struct_type_id);
                return try self.type_table.getOrAddArray(inner_type_id);
            },
            .struct_self => {
                if (current_struct_type_id) |struct_type_id| {
                    return if (type_annotation.nullable) try self.type_table.getOrAddNullable(struct_type_id) else struct_type_id;
                }
                return Errors.SemaError;
            },
        }
    }

    fn getIndexedType(self: *Self, target: *ir.Value, index: *ir.Value) TypeId {
        const target_type_id = target.type_id;
        const target_type = self.type_table.getTypePtrById(target_type_id);

        return switch (target_type.kind) {
            .array => |inner_type_id| inner_type_id,
            .@"struct" => {
                const struct_def = &target_type.kind.@"struct";
                const field_name = index.data.i_string;
                if (struct_def.fields.get(field_name)) |field| {
                    return field.type_id;
                }
                if (struct_def.static_fields.get(field_name)) |field| {
                    return field.type_id;
                }
                if (struct_def.methods.get(field_name)) |method_type_id| {
                    return method_type_id;
                }
                unreachable;
            },
            else => unreachable,
        };
    }

    fn astBinOpToIrBinOpKind(self: *Self, bin_op: ast.BinaryOp, op_type: TypeId, left_type: TypeId, right_type: TypeId) ir.BinaryOpKind {
        const float_type = self.type_table.getPrimitive(.float);
        const bool_type = self.type_table.getPrimitive(.boolean);

        const is_float_cmp = self.type_table.eql(left_type, float_type) or self.type_table.eql(right_type, float_type);
        const is_bool_cmp = self.type_table.eql(left_type, bool_type) or self.type_table.eql(right_type, bool_type);

        return switch (bin_op) {
            .add => if (self.type_table.eql(op_type, float_type)) .f_add else .i_add,
            .sub => if (self.type_table.eql(op_type, float_type)) .f_sub else .i_sub,
            .mult => if (self.type_table.eql(op_type, float_type)) .f_mult else .i_mult,
            .div => .f_div,
            .trunc_div => .i_div,
            .mod => if (self.type_table.eql(op_type, float_type)) .f_mod else .i_mod,
            .eq => if (is_float_cmp) .f_cmp_eq else if (is_bool_cmp) .b_cmp_eq else .i_cmp_eq,
            .not_eq => if (is_float_cmp) .f_cmp_neq else if (is_bool_cmp) .b_cmp_neq else .i_cmp_neq,
            .lt => if (is_float_cmp) .f_cmp_lt else .i_cmp_lt,
            .lt_or_eq => if (is_float_cmp) .f_cmp_le else .i_cmp_le,
            .gt => if (is_float_cmp) .f_cmp_gt else .i_cmp_gt,
            .gt_or_eq => if (is_float_cmp) .f_cmp_ge else .i_cmp_ge,
            .bool_or => .b_or,
            .bool_and => .b_and,
        };
    }

    fn astUnaryOpToIrUnaryOpKind(self: *Self, bin_op: ast.UnaryOp, exp_type: TypeId) ir.UnaryOpKind {
        return switch (bin_op) {
            .neg => if (self.type_table.eql(exp_type, self.type_table.getPrimitive(.float))) .f_neg else .i_neg,
            .not => .not,
        };
    }
};

const Scope = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    symbol_table: *SymbolTable,
    return_type_id: TypeId,
    next_uid: usize,
    next_fn_uid: usize,

    pub fn init(alloc: std.mem.Allocator, return_type_id: TypeId, type_table: *TypeTable) !Self {
        const builtins = try buildin.BuiltIn.init(alloc, type_table);

        var symbols = std.StringHashMap(Symbol).init(alloc);

        for (try builtins.generate()) |func| {
            try symbols.put(func.symbol.identifier, func.symbol);
        }

        return Self{
            .allocator = alloc,
            .symbol_table = try SymbolTable.init(alloc, null, symbols),
            .return_type_id = return_type_id,
            .next_uid = 10,
            .next_fn_uid = 7,
        };
    }

    pub fn genUid(self: *Self) usize {
        const uid = self.next_uid;
        self.next_uid += 1;
        return uid;
    }

    pub fn genFnUid(self: *Self) usize {
        const uid = self.next_fn_uid;
        self.next_fn_uid += 1;
        return uid;
    }

    pub fn enter(self: *Self, return_type_id: TypeId) !void {
        self.return_type_id = return_type_id;
        self.symbol_table = try SymbolTable.init(self.allocator, self.symbol_table, null);
    }

    pub fn exit(self: *Self, return_type_id: TypeId) void {
        self.return_type_id = return_type_id;
        if (self.symbol_table.parent) |parent_scope| self.symbol_table = parent_scope;
    }
};

const SymbolTable = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    parent: ?*SymbolTable,
    symbols: std.StringHashMap(Symbol),

    pub fn init(allocator: std.mem.Allocator, parent: ?*SymbolTable, symbols: ?std.StringHashMap(Symbol)) !*Self {
        const ptr = try allocator.create(Self);
        ptr.* = Self{
            .allocator = allocator,
            .parent = parent,
            .symbols = if (symbols) |s| s else std.StringHashMap(Symbol).init(allocator),
        };
        return ptr;
    }

    fn put(self: *Self, symbol: Symbol) !void {
        if (self.symbols.get(symbol.identifier)) |_| {
            return Errors.SemaError;
        }

        try self.symbols.put(symbol.identifier, symbol);
    }

    fn remove(self: *Self, identifier: []const u8) bool {
        return self.symbols.remove(identifier);
    }

    fn get(self: *Self, identifier: []const u8) ?Symbol {
        if (self.symbols.get(identifier)) |s| return s;
        if (self.parent) |parent| {
            return parent.get(identifier);
        }
        return null;
    }

    fn getOrError(self: *Self, id: []const u8) !Symbol {
        if (self.get(id)) |s| return s;
        return Errors.SemaError;
    }
};

pub const Symbol = struct {
    const Self = @This();

    uid: usize,
    identifier: []const u8,
    type_id: TypeId,

    kind: union(enum) {
        variable: struct {
            is_mut: bool,
        },
        @"struct": struct {
            blueprint_type_id: TypeId,
        },
        function: void,
    },
};

const Struct = struct {
    identifier: []const u8,
    fields: std.StringHashMap(TypedIdentifier),
    static_fields: std.StringHashMap(TypedIdentifier),
    fields_in_order: []const TypedIdentifier,
    methods: std.StringHashMap(TypeId),
};

const FuncMetadata = struct {
    identifier: []const u8,
    uid: usize,
    params: []const TypedIdentifier,
    return_type_id: TypeId,
};

//note: idk about this
pub const TypedIdentifier = struct {
    const Self = @This();

    identifier: []const u8,
    type_id: TypeId,
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
        dynamic,
        meta,

        function: FuncMetadata,
        @"struct": Struct,

        array: TypeId,
    },

    nullable: bool,

    pub fn init(allocator: std.mem.Allocator, exp: Self) !*Self {
        const ptr = try allocator.create(Self);
        ptr.* = exp;
        return ptr;
    }
};

const PrimitiveType = enum {
    float,
    int,
    string,
    boolean,
    void,
    null,
    dynamic,
    meta,
};

pub const TypeId = u64;

pub const TypeTable = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    types: std.ArrayList(Type),

    primitives: std.AutoHashMap(PrimitiveType, TypeId),
    arrays: std.AutoHashMap(TypeId, TypeId),
    nullableTypeIdByTypeId: std.AutoHashMap(TypeId, TypeId),
    typeIdByNullableTypeId: std.AutoHashMap(TypeId, TypeId),
    anom_structs: std.StringHashMap(TypeId),

    fn init(alloc: std.mem.Allocator) !Self {
        var primitives = std.AutoHashMap(PrimitiveType, TypeId).init(alloc);
        var types: std.ArrayList(Type) = .empty;

        for (std.enums.values(PrimitiveType), 0..) |primitive_type, i| {
            try types.append(alloc, Type{
                .kind = switch (primitive_type) {
                    .float => .{ .float = {} },
                    .int => .{ .int = {} },
                    .string => .{ .string = {} },
                    .boolean => .{ .boolean = {} },
                    .void => .{ .void = {} },
                    .null => .{ .null = {} },
                    .dynamic => .{ .dynamic = {} },
                    .meta => .{ .meta = {} },
                },
                .nullable = false,
            });

            try primitives.put(primitive_type, @intCast(i));
        }

        return Self{
            .allocator = alloc,
            .types = types,
            .primitives = primitives,
            .arrays = std.AutoHashMap(TypeId, TypeId).init(alloc),
            .nullableTypeIdByTypeId = std.AutoHashMap(TypeId, TypeId).init(alloc),
            .typeIdByNullableTypeId = std.AutoHashMap(TypeId, TypeId).init(alloc),
            .anom_structs = std.StringHashMap(TypeId).init(alloc),
        };
    }

    pub fn getPrimitive(self: *const Self, p: PrimitiveType) TypeId {
        return self.primitives.get(p).?;
    }

    pub fn getTypePtrById(self: *Self, id: TypeId) *Type {
        return &self.types.items[@intCast(id)];
    }

    fn eql(_: *Self, a: TypeId, b: TypeId) bool {
        return a == b;
    }

    fn coerce(self: *Self, from: TypeId, to: TypeId) bool {
        if (from == to) return true;

        const type_from = self.getTypePtrById(from);
        const type_to = self.getTypePtrById(to);

        if (type_from.kind == .dynamic or type_to.kind == .dynamic) return true;

        if (!type_from.nullable and type_to.nullable) {
            if (type_from.kind == .null) return true;
            if (self.typeIdByNullableTypeId.get(to)) |unwrapped| {
                return self.coerce(from, unwrapped);
            }
        }

        if (type_from.kind == .array and type_to.kind == .array) {
            return self.coerce(type_from.kind.array, type_to.kind.array);
        }

        if (type_from.kind == .@"struct" and type_to.kind == .@"struct") {
            const from_s = type_from.kind.@"struct";
            const to_s = type_to.kind.@"struct";
            const from_is_anon = std.mem.eql(u8, from_s.identifier, "@");
            const to_is_anon = std.mem.eql(u8, to_s.identifier, "@");

            if (from_is_anon and !to_is_anon) {
                for (to_s.fields_in_order) |field| {
                    if (from_s.fields.get(field.identifier) == null) return false;
                }
                return true;
            }
        }

        //note: wrong?
        if (type_from.kind == .function and type_to.kind == .function) return false;

        return false;
    }

    fn name(self: *Self, id: TypeId, allocator: std.mem.Allocator) ![]const u8 {
        const t = self.getTypePtrById(id);
        const prefix = if (t.nullable) "nullable " else "";

        return switch (t.kind) {
            .float => if (t.nullable) "nullable float" else "float",
            .int => if (t.nullable) "nullable int" else "int",
            .string => if (t.nullable) "nullable string" else "string",
            .boolean => if (t.nullable) "nullable boolean" else "boolean",
            .void => if (t.nullable) "nullable void" else "void",
            .null => if (t.nullable) "nullable null" else "null",
            .dynamic => if (t.nullable) "nullable dynamic" else "dynamic",
            .meta => "struct definition",
            .function => |metadata| {
                return std.fmt.allocPrint(allocator, "function {s}", .{metadata.identifier});
            },
            .@"struct" => |s| {
                if (std.mem.eql(u8, s.identifier, "@")) {
                    return std.fmt.allocPrint(allocator, "{s}anonymous struct", .{prefix});
                }
                return std.fmt.allocPrint(allocator, "{s}struct {s}", .{ prefix, s.identifier });
            },
            .array => |inner_id| {
                return std.fmt.allocPrint(allocator, "{s}[]{s}", .{ prefix, try self.name(inner_id, allocator) });
            },
        };
    }

    fn addType(self: *Self, typ: Type) !TypeId {
        try self.types.append(self.allocator, typ);
        return @intCast(self.types.items.len - 1);
    }

    fn getOrAddNullable(self: *Self, typeId: TypeId) !TypeId {
        if (self.getTypePtrById(typeId).nullable) return typeId;

        const nullable_type = self.nullableTypeIdByTypeId.get(typeId);

        if (nullable_type) |t| {
            return t;
        }

        const @"type" = self.getTypePtrById(typeId);

        try self.types.append(self.allocator, .{
            .kind = @"type".kind,
            .nullable = true,
        });

        const id: TypeId = @intCast(self.types.items.len - 1);
        try self.nullableTypeIdByTypeId.put(typeId, id);
        try self.typeIdByNullableTypeId.put(id, typeId);

        return id;
    }

    pub fn getOrAddArray(self: *Self, inner: TypeId) !TypeId {
        const cached_array = self.arrays.get(inner);
        if (cached_array) |type_id| {
            return type_id;
        }

        try self.types.append(self.allocator, .{
            .kind = .{
                .array = inner,
            },
            .nullable = false,
        });

        const new_type_id: TypeId = @intCast(self.types.items.len - 1);
        try self.arrays.put(inner, new_type_id);
        return new_type_id;
    }

    fn getAnonymoustStructSignature(self: *Self, anom_struct: *const Struct) ![]const u8 {
        var fields_in_order: std.ArrayList([]const u8) = .empty;

        var keys_it = anom_struct.fields.keyIterator();
        while (keys_it.next()) |key_ptr| {
            try fields_in_order.append(self.allocator, key_ptr.*);
        }

        std.mem.sort([]const u8, fields_in_order.items, {}, util.stringCompare);

        var buf: std.ArrayList(u8) = .empty;
        for (fields_in_order.items) |field| {
            const typed_id = anom_struct.fields.get(field);
            if (typed_id) |t_id| {
                const formatted = try std.fmt.allocPrint(self.allocator, "{s}:{d}/", .{
                    field,
                    t_id.type_id,
                });
                try buf.appendSlice(self.allocator, formatted);
            }
        }

        return buf.toOwnedSlice(self.allocator);
    }

    fn getOrAddAnonymousStruct(self: *Self, anom_struct: Struct) !TypeId {
        const anom_struct_signature = try self.getAnonymoustStructSignature(&anom_struct);
        const cached_anom_struct = self.anom_structs.get(anom_struct_signature);

        if (cached_anom_struct) |id| {
            return id;
        }

        try self.types.append(self.allocator, .{
            .kind = .{
                .@"struct" = anom_struct,
            },
            .nullable = false,
        });

        const new_id: TypeId = @intCast(self.types.items.len - 1);
        try self.anom_structs.put(anom_struct_signature, new_id);

        return new_id;
    }
};

pub const Errors = err.Errors;
