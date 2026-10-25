const util = @import("util.zig");
const buildin = @import("built-in.zig");
const err = @import("error.zig");
const ir = @import("ir.zig");
const ast = @import("ast.zig");
const tt = @import("type_table.zig");
const allocPrint = std.fmt.allocPrint;
const std = @import("std");

pub const Errors = err.Errors;

pub const SemaAnalyzer = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    src: []const u8 = "",

    functions: std.ArrayList(ir.Func),
    defined_types: std.ArrayList(ir.DefinedType),

    type_table: tt.TypeTable,
    scope: Scope,
    err_dispatcher: err.ErrorDispatcher,

    pub fn init(allocator: std.mem.Allocator) !Self {
        var type_table = try tt.TypeTable.init(allocator);

        const void_type_id = type_table.getPrimitive(.void);

        var self = Self{
            .allocator = allocator,
            .type_table = type_table,
            .scope = undefined,
            .err_dispatcher = .{ .allocator = allocator, .src = "" },
            .functions = .empty,
            .defined_types = .empty,
        };

        self.scope = try Scope.init(allocator, void_type_id, &self.type_table);

        return self;
    }

    pub fn analyze(self: *Self, root: *const ast.Root, src: []const u8) !ir.Program {
        self.src = src;
        self.err_dispatcher.src = src;

        try self.hoistDeclarations(root);

        return .{
            .instructions = (try self.visitBlock(root)).instructions,
            .functions = try self.functions.toOwnedSlice(self.allocator),
            .defined_types = try self.defined_types.toOwnedSlice(self.allocator),
        };
    }

    pub fn hoistDeclarations(self: *Self, root: *const ast.Root) !void {
        for (root.statements.items) |stmt| {
            switch (stmt.data) {
                .func => |fs| {
                    const symbol = try self.createEmptyFuncSymbol(fs.identifier);
                    self.scope.symbol_table.put(symbol) catch return self.err_dispatcher.alreadyDefined(fs.identifier, stmt.loc);
                },
                .@"struct" => |ss| {
                    const symbol = try self.createEmptyStructSymbol(ss.identifier);
                    self.scope.symbol_table.put(symbol) catch return self.err_dispatcher.alreadyDefined(ss.identifier, stmt.loc);
                },
                .@"enum" => |es| {
                    const symbol = try self.createEmptyEnumSymbol(es.identifier);
                    self.scope.symbol_table.put(symbol) catch return self.err_dispatcher.alreadyDefined(es.identifier, stmt.loc);
                },
                .@"union" => |un| {
                    const symbol = try self.createEmptyUnionSymbol(un.identifier);
                    self.scope.symbol_table.put(symbol) catch return self.err_dispatcher.alreadyDefined(un.identifier, stmt.loc);
                },
                else => {},
            }
        }

        for (root.statements.items) |stmt| {
            switch (stmt.data) {
                .func => |fs| {
                    const symbol = try self.scope.symbol_table.getOrError(fs.identifier);
                    try self.fulfillFuncType(symbol, &fs.def, null, stmt.loc);
                },
                .@"struct" => |ss| {
                    const symbol = try self.scope.symbol_table.getOrError(ss.identifier);
                    try self.fulfillStructType(symbol, &ss.def);
                },

                .@"enum" => |ss| {
                    const symbol = try self.scope.symbol_table.getOrError(ss.identifier);
                    try self.fulfillEnumType(symbol, &ss.def);
                },

                .@"union" => |us| {
                    const symbol = try self.scope.symbol_table.getOrError(us.identifier);
                    try self.fulfillUnionType(symbol, &us.def);
                },
                else => {},
            }
        }
    }

    fn createEmptyFuncSymbol(self: *Self, identifier: []const u8) !Symbol {
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
            .is_nullable = false,
        });

        return symbol;
    }

    fn createEmptyEnumSymbol(self: *Self, identifier: []const u8) !Symbol {
        const blueprint_id = try self.type_table.addType(.{
            .kind = .{
                .@"enum" = .{
                    .identifier = identifier,
                    .variants = .init(self.allocator),
                    .static_fields = .init(self.allocator),
                    .methods = .init(self.allocator),
                },
            },
            .is_nullable = false,
        });

        return Symbol{
            .identifier = identifier,
            .uid = self.scope.genUid(),
            .type_id = self.type_table.getPrimitive(.meta),
            .kind = .{
                .defined_type = .{
                    .blueprint_type_id = blueprint_id,
                },
            },
        };
    }

    fn createEmptyUnionSymbol(self: *Self, identifier: []const u8) !Symbol {
        const blueprint_id = try self.type_table.addType(.{
            .kind = .{
                .@"union" = .{
                    .identifier = identifier,
                    .variants = .init(self.allocator),
                    .static_fields = .init(self.allocator),
                    .methods = .init(self.allocator),
                },
            },
            .is_nullable = false,
        });

        return Symbol{
            .identifier = identifier,
            .uid = self.scope.genUid(),
            .type_id = self.type_table.getPrimitive(.meta),
            .kind = .{
                .defined_type = .{
                    .blueprint_type_id = blueprint_id,
                },
            },
        };
    }

    fn createEnumSymbol(self: *Self, identifier: []const u8, enum_def: *const ast.EnumDef) !Symbol {
        var variants: std.StringHashMap(i64) = .init(self.allocator);

        const variants_in_order = try self.allocator.alloc([]const u8, enum_def.variants.len);

        const is_explicit = enum_def.variants[0].value != null;
        for (enum_def.variants, 0..) |variant, i| {
            const value: i64 = if (is_explicit) variant.value.? else @intCast(i);
            try variants.put(variant.identifier, value);
            variants_in_order[i] = variant.identifier;
        }

        const blueprint_type_id = try self.type_table.addType(.{
            .kind = .{
                .@"enum" = .{
                    .identifier = identifier,
                    .variants = variants,
                    .variants_in_order = variants_in_order,
                    .static_fields = .init(self.allocator),
                    .methods = .init(self.allocator),
                },
            },
            .is_nullable = false,
        });

        return Symbol{
            .identifier = identifier,
            .uid = self.scope.genUid(),
            .type_id = self.type_table.getPrimitive(.meta),
            .kind = .{ .defined_type = .{ .blueprint_type_id = blueprint_type_id } },
        };
    }

    fn createEmptyStructSymbol(self: *Self, identifier: []const u8) !Symbol {
        const blueprint_type_id = try self.type_table.addType(.{
            .kind = .{
                .@"struct" = .{
                    .identifier = identifier,
                    .fields_in_order = &.{},
                    .field_index_by_name = .init(self.allocator),
                    .fields = .init(self.allocator),
                    .static_fields = .init(self.allocator),
                    .methods = .init(self.allocator),
                },
            },
            .is_nullable = false,
        });

        return Symbol{
            .identifier = identifier,
            .uid = self.scope.genUid(),
            .type_id = self.type_table.getPrimitive(.meta),
            .kind = .{ .defined_type = .{ .blueprint_type_id = blueprint_type_id } },
        };
    }

    fn createAnonymousStruct(self: *Self, exp: *const ast.ExpNode, is_mut: bool) !tt.Struct {
        const anonymous_init = exp.data.anonymous_init;
        var fields_in_order: std.ArrayList(tt.TypedIdentifier) = .empty;
        var fields: std.StringHashMap(tt.TypedIdentifier) = .init(self.allocator);

        if (anonymous_init.are_arguments_named == false) {
            return self.err_dispatcher.cantInferAnonymousStruct(exp.loc);
        }

        for (anonymous_init.arguments) |arg| {
            const exp_value = try self.evalExp(arg.exp);
            var field_type_id = exp_value.type_id;
            if (field_type_id == self.type_table.getPrimitive(.null)) {
                field_type_id = try self.type_table.getOrAddNullable(self.type_table.getPrimitive(.dynamic));
            } else if (self.type_table.getTypePtrById(field_type_id).kind == .@"struct") {
                field_type_id = try self.type_table.getOrAddNullable(field_type_id);
            }
            const field: tt.TypedIdentifier = .{
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

        var field_index_by_name: std.StringHashMap(u8) = .init(self.allocator);
        for (fields_in_order.items, 0..) |field, i| {
            try field_index_by_name.put(field.identifier, @intCast(i));
        }

        return tt.Struct{
            .identifier = "@",
            .fields = fields,
            .fields_in_order = try fields_in_order.toOwnedSlice(self.allocator),
            .field_index_by_name = field_index_by_name,
            .static_fields = .init(self.allocator),
            .methods = .init(self.allocator),
        };
    }

    fn fulfillEnumType(self: *Self, enum_symbol: Symbol, enum_def: *const ast.EnumDef) !void {
        const blueprint_type_id = enum_symbol.kind.defined_type.blueprint_type_id;

        var methods: std.StringHashMap(tt.TypeId) = .init(self.allocator);
        var variants: std.StringHashMap(i64) = .init(self.allocator);
        var static_fields: std.StringHashMap(tt.TypedIdentifier) = .init(self.allocator);

        const variants_in_order = try self.allocator.alloc([]const u8, enum_def.variants.len);

        const is_explicit = enum_def.variants[0].value != null;
        for (enum_def.variants, 0..) |variant, i| {
            const value: i64 = if (is_explicit) variant.value.? else @intCast(i);
            try variants.put(variant.identifier, value);
            variants_in_order[i] = variant.identifier;
        }
        self.type_table.getTypePtrById(blueprint_type_id).kind.@"enum".variants = variants;
        self.type_table.getTypePtrById(blueprint_type_id).kind.@"enum".variants_in_order = variants_in_order;

        for (enum_def.static_fields) |field| {
            const enum_field = try self.createField(field, blueprint_type_id);
            try static_fields.put(field.identifier, enum_field);
        }
        self.type_table.getTypePtrById(blueprint_type_id).kind.@"enum".static_fields = static_fields;

        for (enum_def.funcs) |func_stmt| {
            const func = func_stmt.data.func;
            const fn_symbol = try self.createEmptyFuncSymbol(func.identifier);
            try self.fulfillFuncType(fn_symbol, &func.def, enum_symbol, func_stmt.loc);
            try methods.put(func.identifier, fn_symbol.type_id);
        }
        self.type_table.getTypePtrById(blueprint_type_id).kind.@"enum".methods = methods;
    }

    fn fulfillUnionType(self: *Self, union_symbol: Symbol, union_def: *const ast.UnionDef) !void {
        const blueprint_type_id = union_symbol.kind.defined_type.blueprint_type_id;

        var methods: std.StringHashMap(tt.TypeId) = .init(self.allocator);
        var variants: std.StringHashMap(tt.TypeId) = .init(self.allocator);
        var static_fields: std.StringHashMap(tt.TypedIdentifier) = .init(self.allocator);

        const variants_in_order = try self.allocator.alloc([]const u8, union_def.variants.len);
        for (union_def.variants, 0..) |variant, i| {
            try variants.put(variant.identifier, try self.resolveTypeAnnotation(variant.type, blueprint_type_id));
            variants_in_order[i] = variant.identifier;
        }

        self.type_table.getTypePtrById(blueprint_type_id).kind.@"union".variants = variants;
        self.type_table.getTypePtrById(blueprint_type_id).kind.@"union".variants_in_order = variants_in_order;

        for (union_def.static_fields) |field| {
            const enum_field = try self.createField(field, blueprint_type_id);
            try static_fields.put(field.identifier, enum_field);
        }

        self.type_table.getTypePtrById(blueprint_type_id).kind.@"union".static_fields = static_fields;

        for (union_def.funcs) |func_stmt| {
            const func = func_stmt.data.func;
            const fn_symbol = try self.createEmptyFuncSymbol(func.identifier);
            try self.fulfillFuncType(fn_symbol, &func.def, union_symbol, func_stmt.loc);
            try methods.put(func.identifier, fn_symbol.type_id);
        }

        self.type_table.getTypePtrById(blueprint_type_id).kind.@"union".methods = methods;
    }

    fn fulfillStructType(self: *Self, struct_symbol: Symbol, struct_def: *const ast.Struct) !void {
        const blueprint_type_id = struct_symbol.kind.defined_type.blueprint_type_id;
        var fields_in_order: std.ArrayList(tt.TypedIdentifier) = .empty;
        var fields: std.StringHashMap(tt.TypedIdentifier) = .init(self.allocator);
        var static_fields: std.StringHashMap(tt.TypedIdentifier) = .init(self.allocator);
        var methods: std.StringHashMap(tt.TypeId) = .init(self.allocator);

        for (struct_def.static_fields) |field| {
            const struct_field = try self.createField(field, blueprint_type_id);
            try static_fields.put(field.identifier, struct_field);
        }

        for (struct_def.fields) |field| {
            const struct_field = try self.createField(field, blueprint_type_id);
            try fields_in_order.append(
                self.allocator,
                struct_field,
            );
            try fields.put(field.identifier, struct_field);
        }

        for (struct_def.funcs) |func_node| {
            //note: maybe this is not optimal because we only need the fn metadata
            const func = func_node.data.func;
            const fn_symbol = try self.createEmptyFuncSymbol(func.identifier);
            try self.fulfillFuncType(fn_symbol, &func.def, struct_symbol, func_node.loc);
            try methods.put(func.identifier, fn_symbol.type_id);
        }

        var field_index_by_name: std.StringHashMap(u8) = .init(self.allocator);
        for (fields_in_order.items, 0..) |field, i| {
            try field_index_by_name.put(field.identifier, @intCast(i));
        }

        const struct_type_ptr = self.type_table.getTypePtrById(blueprint_type_id);
        struct_type_ptr.kind.@"struct".fields_in_order = try fields_in_order.toOwnedSlice(self.allocator);
        struct_type_ptr.kind.@"struct".field_index_by_name = field_index_by_name;
        struct_type_ptr.kind.@"struct".fields = fields;
        struct_type_ptr.kind.@"struct".static_fields = static_fields;
        struct_type_ptr.kind.@"struct".methods = methods;
    }

    fn createField(self: *Self, field: ast.Field, current_struct_type_id: ?tt.TypeId) Errors!tt.TypedIdentifier {
        var field_type_id: tt.TypeId = undefined;
        var field_default_value: ?*ir.Value = null;

        if (field.type) |_type| {
            field_type_id = try self.resolveTypeAnnotation(_type, current_struct_type_id);
        }

        if (field.default_value) |initial_value| {
            const value = try self.evalExp(initial_value);
            const value_type_id = value.type_id;
            field_default_value = value;

            if (field.type) |_type| {
                if (!self.type_table.canCoerce(value_type_id, field_type_id)) {
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

        return tt.TypedIdentifier{
            .identifier = field.identifier,
            .is_mut = field.is_mut,
            .type_id = field_type_id,
            .default_value = field_default_value,
        };
    }

    fn fulfillFuncType(self: *Self, fn_symbol: Symbol, fn_def: *const ast.FnDef, fn_owner_symbol: ?Symbol, loc: err.Loc) !void {
        var return_type_id: tt.TypeId = undefined;
        var params_metadata: std.ArrayList(tt.TypedIdentifier) = .empty;

        //create a temporary scope just for inline return type inference
        try self.scope.enter(self.type_table.getPrimitive(.void));

        for (fn_def.params) |param| {
            var param_default_value: ?*ir.Value = null;
            var param_type_id: tt.TypeId = undefined;

            if (param.type_annotation) |annotation| {
                const owner_type_id = if (fn_owner_symbol) |symbol| symbol.kind.defined_type.blueprint_type_id else null;
                param_type_id = self.resolveTypeAnnotation(annotation, owner_type_id) catch {
                    return self.err_dispatcher.typeNotDefined(
                        try param.type_annotation.?.value(self.allocator),
                        loc,
                    );
                };

                if (param.default_value) |default_value| {
                    const value = try self.evalExp(default_value);
                    param_default_value = value;
                    const value_type_id = value.type_id;
                    if (!self.type_table.canCoerce(value_type_id, param_type_id)) {
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
                    return self.err_dispatcher.invalidParameterType(param.identifier, loc);
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
                return self.err_dispatcher.alreadyDefined(param.identifier, loc);
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
                return self.err_dispatcher.typeNotDefined(r_type.type.primitive, loc);
            };
        } else if (fn_def.body_block.statements.items[0].data.@"return".exp) |return_exp| {
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

        var return_type: tt.TypeId = self.type_table.getPrimitive(.void);

        for (block.statements.items) |stmt| {
            const instruction = switch (stmt.data) {
                .let => try self.visitLetStmt(&stmt),
                .func => try self.visitFuncStmt(&stmt),
                .@"struct" => try self.visitStructStmt(&stmt),
                .@"enum" => try self.visitEnumStmt(&stmt),
                .@"union" => try self.visitUnionStmt(&stmt),
                .assign => try self.visitAssignStmt(&stmt),
                .@"if" => try self.visitIfStmt(&stmt),
                .@"for" => try self.visitForStmt(&stmt),
                .expression => try self.visitExpressionStmt(&stmt),
                .@"break" => ir.Instruction{ .break_stmt = {} },
                .@"continue" => ir.Instruction{ .continue_stmt = {} },
                .@"return" => try self.visitReturnStmt(&stmt),
                .if_capture, .when => null,
            };

            if (stmt.data == .if_capture) {
                const if_capture_insts = try self.visitIfCaptureStmt(&stmt);
                try instructions.appendSlice(self.allocator, if_capture_insts);
                continue;
            }

            if (stmt.data == .when) {
                const when_insts = try self.visitWhenStmt(&stmt);
                try instructions.appendSlice(self.allocator, when_insts);
                continue;
            }

            if (instruction) |i| {
                try instructions.append(self.allocator, i);
            }
        }

        if (areAllPathsReturning(instructions.items)) {
            return_type = self.scope.return_type_id;
        }

        return ir.Block{
            .return_type = return_type,
            .instructions = try instructions.toOwnedSlice(self.allocator),
        };
    }

    fn areAllPathsReturning(instructions: []const ir.Instruction) bool {
        for (instructions) |inst| {
            switch (inst) {
                .return_stmt => return true,
                .branch_if => |branch| {
                    if (branch.else_block.len > 0 and areAllPathsReturning(branch.then_block) and areAllPathsReturning(branch.else_block)) return true;
                },
                else => {},
            }
        }
        return false;
    }

    fn visitExpressionStmt(self: *Self, stmt: *const ast.StatementNode) !ir.Instruction {
        const expression_stmt = stmt.data.expression;

        if (expression_stmt.data != .fn_call) {
            return self.err_dispatcher.invalidType("function", @tagName(expression_stmt.data), expression_stmt.loc);
        }

        return ir.Instruction{
            .expression_stmt = .{ .value = try self.evalFnCall(expression_stmt) },
        };
    }

    fn visitLetStmt(self: *Self, stmt: *const ast.StatementNode) !?ir.Instruction {
        var var_type_id: tt.TypeId = undefined;
        const let_stmt = stmt.data.let;

        const annotated_type_id = if (let_stmt.type_annotation) |type_annotation|
            try self.resolveTypeAnnotation(type_annotation, null)
        else
            null;

        const expression_value = try self.evalExpExpecting(let_stmt.value, annotated_type_id);
        const expression_type_id = expression_value.type_id;

        if (annotated_type_id) |type_id| {
            var_type_id = type_id;
        } else {
            var_type_id = expression_type_id;
            const var_type_ptr = self.type_table.getTypePtrById(var_type_id);
            if (var_type_ptr.kind == .array and self.type_table.getTypePtrById(var_type_ptr.kind.array).kind == .dynamic) {
                return self.err_dispatcher.cantInferArrayLiteralType(let_stmt.value.loc);
            }
        }

        if (!self.type_table.canCoerce(expression_type_id, var_type_id)) {
            return self.err_dispatcher.invalidType(
                try self.type_table.name(var_type_id, self.allocator),
                try self.type_table.name(expression_type_id, self.allocator),
                stmt.data.let.value.loc,
            );
        }

        const expr_type = self.type_table.getTypePtrById(expression_type_id);
        if (expr_type.kind == .array and self.type_table.getTypePtrById(expr_type.kind.array).kind == .dynamic) {
            expression_value.type_id = var_type_id;
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

        const boxed_value = if (self.needsNullBox(var_type_id, expression_value.type_id))
            try ir.Value.init(self.allocator, .{
                .data = .{ .null_box_init = .{ .value = expression_value } },
                .type_id = expression_value.type_id,
            })
        else
            expression_value;

        return ir.Instruction{ .store_var = .{
            .uid = uid,
            .identifier = let_stmt.identifier,
            .type_id = var_type_id,
            .value = boxed_value,
        } };
    }

    fn visitFuncStmt(self: *Self, stmt: *const ast.StatementNode) !?ir.Instruction {
        const func_stmt = stmt.data.func;
        const fn_symbol = self.scope.symbol_table.getOrError(func_stmt.identifier) catch {
            return self.err_dispatcher.notDefined(func_stmt.identifier, stmt.loc);
        };
        const fn_metadata = self.type_table.getTypePtrById(fn_symbol.type_id).kind.function;
        const func = try self.visitFnDef(&func_stmt.def, fn_metadata.uid, fn_metadata, null, stmt.loc);
        try self.functions.append(self.allocator, func);
        return null;
    }

    fn visitStructStmt(self: *Self, stmt: *const ast.StatementNode) !?ir.Instruction {
        const struct_stmt = stmt.data.@"struct";
        const struct_def = try self.visitStructDef(&struct_stmt.def, struct_stmt.identifier);
        try self.defined_types.append(self.allocator, .{ .kind = .{ .@"struct" = struct_def } });
        return null;
    }

    fn visitEnumStmt(self: *Self, stmt: *const ast.StatementNode) !?ir.Instruction {
        const enum_stmt = stmt.data.@"enum";
        const enum_symbol = try self.scope.symbol_table.getOrError(enum_stmt.identifier);
        const blueprint_type_id = enum_symbol.kind.defined_type.blueprint_type_id;
        const enum_def = self.type_table.getTypePtrById(blueprint_type_id).kind.@"enum";
        var static_fields: std.ArrayList(ir.Field) = .empty;
        var funcs: std.ArrayList(ir.Func) = .empty;

        for (enum_stmt.def.static_fields) |field| {
            const raw_default: ?*ir.Value = if (field.default_value) |_value| try self.evalExp(_value) else null;
            const field_type = if (field.type) |_type| try self.resolveTypeAnnotation(_type, blueprint_type_id) else raw_default.?.type_id;

            const default_value: ?*ir.Value = if (raw_default) |dv| blk: {
                if (self.needsNullBox(field_type, dv.type_id)) {
                    break :blk try ir.Value.init(self.allocator, .{
                        .data = .{ .null_box_init = .{ .value = dv } },
                        .type_id = dv.type_id,
                    });
                }
                break :blk dv;
            } else null;

            try static_fields.append(self.allocator, .{
                .identifier = field.identifier,
                .type_id = field_type,
                .default_value = default_value,
            });
        }

        for (enum_stmt.def.funcs) |func_node| {
            const func = func_node.data.func;
            const fn_type_id = enum_def.methods.get(func.identifier) orelse continue;
            const fn_metadata = self.type_table.getTypePtrById(fn_type_id).kind.function;
            const fn_def = try self.visitFnDef(&func.def, fn_metadata.uid, fn_metadata, func.identifier, func_node.loc);
            try funcs.append(self.allocator, fn_def);
        }

        try self.defined_types.append(self.allocator, .{
            .kind = .{
                .@"enum" = .{
                    .uid = enum_symbol.uid,
                    .type_id = blueprint_type_id,
                    .identifier = enum_stmt.identifier,
                    .variants = enum_def.variants,
                    .static_fields = try static_fields.toOwnedSlice(self.allocator),
                    .funcs = try funcs.toOwnedSlice(self.allocator),
                },
            },
        });
        return null;
    }

    fn visitUnionStmt(self: *Self, stmt: *const ast.StatementNode) !?ir.Instruction {
        const union_stmt = stmt.data.@"union";
        const union_symbol = try self.scope.symbol_table.getOrError(union_stmt.identifier);
        const blueprint_type_id = union_symbol.kind.defined_type.blueprint_type_id;
        const union_def = self.type_table.getTypePtrById(blueprint_type_id).kind.@"union";

        var static_fields: std.ArrayList(ir.Field) = .empty;
        var funcs: std.ArrayList(ir.Func) = .empty;

        for (union_stmt.def.static_fields) |field| {
            const raw_default: ?*ir.Value = if (field.default_value) |_value| try self.evalExp(_value) else null;
            const field_type = if (field.type) |_type| try self.resolveTypeAnnotation(_type, blueprint_type_id) else raw_default.?.type_id;

            const default_value: ?*ir.Value = if (raw_default) |dv| blk: {
                if (self.needsNullBox(field_type, dv.type_id)) {
                    break :blk try ir.Value.init(self.allocator, .{
                        .data = .{ .null_box_init = .{ .value = dv } },
                        .type_id = dv.type_id,
                    });
                }
                break :blk dv;
            } else null;

            try static_fields.append(self.allocator, .{
                .identifier = field.identifier,
                .type_id = field_type,
                .default_value = default_value,
            });
        }

        for (union_stmt.def.funcs) |func_node| {
            const func = func_node.data.func;
            const fn_type_id = union_def.methods.get(func.identifier) orelse continue;
            const fn_metadata = self.type_table.getTypePtrById(fn_type_id).kind.function;
            const fn_def = try self.visitFnDef(&func.def, fn_metadata.uid, fn_metadata, func.identifier, func_node.loc);
            try funcs.append(self.allocator, fn_def);
        }

        try self.defined_types.append(self.allocator, .{
            .kind = .{
                .@"union" = .{
                    .uid = union_symbol.uid,
                    .type_id = blueprint_type_id,
                    .identifier = union_stmt.identifier,
                    .variants = union_def.variants,
                    .static_fields = try static_fields.toOwnedSlice(self.allocator),
                    .funcs = try funcs.toOwnedSlice(self.allocator),
                },
            },
        });

        return null;
    }

    fn visitIfStmt(self: *Self, stmt: *const ast.StatementNode) Errors!ir.Instruction {
        const if_stmt = stmt.data.@"if";

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
        const if_capture_stmt = stmt.data.if_capture;
        const exp_value = try self.evalExp(if_capture_stmt.exp);
        const exp_type = exp_value.type_id;

        var instructions: std.ArrayList(ir.Instruction) = .empty;

        if (self.type_table.getTypePtrById(exp_type).is_nullable == false) {
            return self.err_dispatcher.invalidType("nullable", try self.type_table.name(exp_type, self.allocator), if_capture_stmt.exp.loc);
        }

        if (if_capture_stmt.identifier.is_mut and !self.isExpressionMutable(if_capture_stmt.exp)) {
            return self.err_dispatcher.unwrappedValueCantBeMutable(if_capture_stmt.identifier.name, stmt.loc);
        }

        const captured_type_id = self.type_table.typeIdByNullableTypeId.get(exp_type).?;
        const captured_symbol = Symbol{
            .uid = self.scope.genUid(),
            .identifier = if_capture_stmt.identifier.name,
            .type_id = captured_type_id,
            .kind = .{ .variable = .{ .is_mut = if_capture_stmt.identifier.is_mut } },
        };

        try self.scope.symbol_table.put(captured_symbol);
        defer _ = self.scope.symbol_table.remove(captured_symbol.identifier);

        const then_block = (try self.visitBlock(&if_capture_stmt.body)).instructions;
        const else_block = if (if_capture_stmt.else_block) |else_blc| (try self.visitBlock(&else_blc)).instructions else &.{};

        const condition = try ir.Value.init(self.allocator, .{
            .data = .{
                .unary_op = .{
                    .kind = .box_not_null,
                    .right = exp_value,
                },
            },
            .type_id = self.type_table.getPrimitive(.boolean),
        });

        const check_not_null_inst = ir.Instruction{ .branch_if = .{
            .condition = condition,
            .then_block = then_block,
            .else_block = else_block,
        } };

        const captured_base_kind = self.type_table.getTypePtrById(captured_type_id).kind;
        const captured_needs_unwrap = switch (captured_base_kind) {
            .int, .float, .boolean, .@"enum" => true,
            else => false,
        };

        const store_captured_var_inst = if (captured_needs_unwrap)
            ir.Instruction{
                .unwrap_null_box = .{
                    .unwrapped = .{
                        .uid = captured_symbol.uid,
                        .identifier = captured_symbol.identifier,
                        .type_id = captured_type_id,
                    },
                    .null_box = exp_value,
                },
            }
        else
            ir.Instruction{ .store_var = .{
                .uid = captured_symbol.uid,
                .identifier = captured_symbol.identifier,
                .value = exp_value,
                .type_id = captured_type_id,
            } };

        try instructions.appendSlice(self.allocator, &.{ store_captured_var_inst, check_not_null_inst });
        return instructions.toOwnedSlice(self.allocator);
    }

    fn visitWhenStmt(self: *Self, stmt: *const ast.StatementNode) Errors![]ir.Instruction {
        const when_stmt = stmt.data.when;
        const subject = try self.evalExp(when_stmt.subject);
        const subject_type = subject.type_id;
        const subject_type_name = try self.type_table.name(subject_type, self.allocator);

        const is_nullable = self.type_table.getTypePtrById(subject_type).is_nullable;
        const base_type = if (is_nullable) self.type_table.typeIdByNullableTypeId.get(subject_type) orelse
            return self.err_dispatcher.invalidType("int, enum or union", subject_type_name, when_stmt.subject.loc) else subject_type;

        const base_kind = self.type_table.getTypePtrById(base_type).kind;
        switch (base_kind) {
            .int, .@"enum", .@"union" => {},
            else => return self.err_dispatcher.invalidType("int, enum or union", subject_type_name, when_stmt.subject.loc),
        }

        var seen_values: std.AutoHashMap(i64, void) = .init(self.allocator);
        var seen_variants: std.StringHashMap(void) = .init(self.allocator);
        var has_null_case = false;

        const prong_values = try self.allocator.alloc([]*ir.Value, when_stmt.prongs.len);

        for (when_stmt.prongs, 0..) |prong, prong_idx| {
            prong_values[prong_idx] = &.{};

            switch (prong.pattern) {
                .null_case => {
                    if (!is_nullable) {
                        return self.err_dispatcher.whenInvalidCase(
                            try std.fmt.allocPrint(self.allocator, "'null' case on non-nullable '{s}'", .{subject_type_name}),
                            prong.loc,
                        );
                    }
                    if (has_null_case) return self.err_dispatcher.whenDuplicateCase("null", prong.loc);
                    has_null_case = true;
                },
                .variant => |variant| {
                    if (base_kind != .@"union") {
                        return self.err_dispatcher.whenInvalidCase(
                            try std.fmt.allocPrint(self.allocator, "'variant: capture' cases are only for unions, found '{s}'", .{subject_type_name}),
                            prong.loc,
                        );
                    }
                    if (!base_kind.@"union".variants.contains(variant.name)) {
                        return self.err_dispatcher.invalidUnionVariant(base_kind.@"union".identifier, variant.name, prong.loc);
                    }
                    if (seen_variants.contains(variant.name)) return self.err_dispatcher.whenDuplicateCase(variant.name, prong.loc);
                    try seen_variants.put(variant.name, {});
                },
                .values => |exps| {
                    if (base_kind == .@"union") {
                        return self.err_dispatcher.whenInvalidCase("union cases are written as 'variant: capture'", prong.loc);
                    }

                    const values = try self.allocator.alloc(*ir.Value, exps.len);
                    for (exps, 0..) |exp, i| {
                        const value = try self.evalWhenCaseValue(exp, base_type);
                        const key = value.data.i_int;
                        if (seen_values.contains(key)) {
                            return self.err_dispatcher.whenDuplicateCase(self.err_dispatcher.src[exp.loc.start .. exp.loc.end + 1], exp.loc);
                        }
                        try seen_values.put(key, {});
                        values[i] = value;
                    }
                    prong_values[prong_idx] = values;
                },
            }
        }

        var missing: std.ArrayList(u8) = .empty;
        switch (base_kind) {
            .@"enum" => |enum_def| for (enum_def.variants_in_order) |name| {
                if (!seen_values.contains(enum_def.variants.get(name).?)) try appendMissing(self.allocator, &missing, name);
            },
            .@"union" => |union_def| for (union_def.variants_in_order) |name| {
                if (!seen_variants.contains(name)) try appendMissing(self.allocator, &missing, name);
            },
            else => {},
        }
        if (is_nullable and !has_null_case) try appendMissing(self.allocator, &missing, "null");

        const when_loc: err.Loc = .{ .start = stmt.loc.start, .end = when_stmt.subject.loc.end };
        if (when_stmt.else_block == null) {
            if (base_kind == .int) return self.err_dispatcher.whenNeedsElse(subject_type_name, when_loc);
            if (missing.items.len > 0) return self.err_dispatcher.whenNotExhaustive(missing.items, when_loc);
        } else if (base_kind != .int and missing.items.len == 0) {
            return self.err_dispatcher.whenUselessElse(when_stmt.else_loc.?);
        }

        var instructions: std.ArrayList(ir.Instruction) = .empty;

        const subject_uid = self.scope.genUid();
        try instructions.append(self.allocator, .{ .store_var = .{
            .uid = subject_uid,
            .identifier = "when",
            .type_id = subject_type,
            .value = subject,
        } });
        const subject_ref = try ir.Value.init(self.allocator, .{
            .data = .{ .identifier = .{ .uid = subject_uid, .name = "when" } },
            .type_id = subject_type,
        });

        var unwrapped_ref = subject_ref;
        if (is_nullable) {
            const unwrapped_uid = self.scope.genUid();
            unwrapped_ref = try ir.Value.init(self.allocator, .{
                .data = .{ .identifier = .{ .uid = unwrapped_uid, .name = "when" } },
                .type_id = base_type,
            });

            try instructions.append(self.allocator, if (base_kind == .@"union")
                .{ .store_var = .{ .uid = unwrapped_uid, .identifier = "when", .type_id = base_type, .value = subject_ref } }
            else
                .{ .unwrap_null_box = .{
                    .unwrapped = .{ .uid = unwrapped_uid, .identifier = "when", .type_id = base_type },
                    .null_box = subject_ref,
                } });
        }

        var chain: []const ir.Instruction = if (when_stmt.else_block) |else_blc| (try self.visitBlock(&else_blc)).instructions else &.{};

        var null_insts: []const ir.Instruction = &.{};
        if (is_nullable and !has_null_case) {
            null_insts = (try self.visitBlock(&when_stmt.else_block.?)).instructions;
        }

        var unconditional_prong_idx: ?usize = null;
        if (when_stmt.else_block == null) {
            var idx = when_stmt.prongs.len;
            while (idx > 0) {
                idx -= 1;
                if (when_stmt.prongs[idx].pattern != .null_case) {
                    unconditional_prong_idx = idx;
                    break;
                }
            }
        }

        var prong_idx = when_stmt.prongs.len;
        while (prong_idx > 0) {
            prong_idx -= 1;
            const prong = when_stmt.prongs[prong_idx];
            const is_unconditional = unconditional_prong_idx == prong_idx;

            switch (prong.pattern) {
                .null_case => {
                    null_insts = (try self.visitBlock(&prong.body)).instructions;
                    continue;
                },
                .variant => |variant| {
                    const union_def = base_kind.@"union";
                    const variant_type_id = union_def.variants.get(variant.name).?;

                    const tag = try ir.Value.init(self.allocator, .{
                        .data = .{ .union_tag = unwrapped_ref },
                        .type_id = self.type_table.getPrimitive(.int),
                    });
                    const tag_index = for (union_def.variants_in_order, 0..) |name, i| {
                        if (std.mem.eql(u8, name, variant.name)) break i;
                    } else unreachable;
                    const condition = try self.cmpEq(tag, try ir.Value.init(self.allocator, .{
                        .data = .{ .i_int = @intCast(tag_index) },
                        .type_id = self.type_table.getPrimitive(.int),
                    }));

                    var body: std.ArrayList(ir.Instruction) = .empty;
                    const is_ignored = std.mem.eql(u8, variant.capture, "_");
                    const captured_symbol = Symbol{
                        .uid = self.scope.genUid(),
                        .identifier = variant.capture,
                        .type_id = variant_type_id,
                        .kind = .{ .variable = .{ .is_mut = false } },
                    };

                    if (!is_ignored) {
                        try body.append(self.allocator, .{ .store_var = .{
                            .uid = captured_symbol.uid,
                            .identifier = captured_symbol.identifier,
                            .type_id = variant_type_id,
                            .value = try ir.Value.init(self.allocator, .{
                                .data = .{ .union_payload = unwrapped_ref },
                                .type_id = variant_type_id,
                            }),
                        } });
                        try self.scope.symbol_table.put(captured_symbol);
                    }
                    defer if (!is_ignored) {
                        _ = self.scope.symbol_table.remove(captured_symbol.identifier);
                    };

                    try body.appendSlice(self.allocator, (try self.visitBlock(&prong.body)).instructions);
                    const body_insts = try body.toOwnedSlice(self.allocator);
                    chain = if (is_unconditional) body_insts else try self.wrapBranch(condition, body_insts, chain);
                },
                .values => {
                    var condition: ?*ir.Value = null;
                    for (prong_values[prong_idx]) |value| {
                        const eq = try self.cmpEq(unwrapped_ref, value);
                        condition = if (condition) |c| try ir.Value.init(self.allocator, .{
                            .data = .{ .binary_op = .{ .kind = .b_or, .left = c, .right = eq } },
                            .type_id = self.type_table.getPrimitive(.boolean),
                        }) else eq;
                    }

                    const body = (try self.visitBlock(&prong.body)).instructions;
                    chain = if (is_unconditional) body else try self.wrapBranch(condition.?, body, chain);
                },
            }
        }

        if (is_nullable) {
            const not_null = try ir.Value.init(self.allocator, .{
                .data = .{ .unary_op = .{ .kind = .box_not_null, .right = subject_ref } },
                .type_id = self.type_table.getPrimitive(.boolean),
            });
            chain = try self.wrapBranch(not_null, chain, null_insts);
        }

        try instructions.appendSlice(self.allocator, chain);
        return instructions.toOwnedSlice(self.allocator);
    }

    fn evalWhenCaseValue(self: *Self, exp: *const ast.ExpNode, expected_type: tt.TypeId) Errors!*ir.Value {
        if (self.type_table.getTypePtrById(expected_type).kind == .int) {
            const literal: ?i64 = switch (exp.data) {
                .int_literal => |i| i,
                .unary_exp => |un| if (un.op == .neg and un.right.data == .int_literal) -un.right.data.int_literal else null,
                else => null,
            };

            if (literal == null) {
                const value = try self.evalExp(exp);
                if (!self.type_table.eql(value.type_id, expected_type)) {
                    return self.err_dispatcher.invalidType("int", try self.type_table.name(value.type_id, self.allocator), exp.loc);
                }
                return self.err_dispatcher.whenInvalidCase("int cases must be int literals", exp.loc);
            }

            return ir.Value.init(self.allocator, .{ .data = .{ .i_int = literal.? }, .type_id = expected_type });
        }

        const value = try self.evalExp(exp);
        if (!self.type_table.eql(value.type_id, expected_type)) {
            return self.err_dispatcher.invalidType(
                try self.type_table.name(expected_type, self.allocator),
                try self.type_table.name(value.type_id, self.allocator),
                exp.loc,
            );
        }
        if (value.data != .i_int) {
            return self.err_dispatcher.whenInvalidCase("enum cases must be variants, like 'Color.Red'", exp.loc);
        }
        return value;
    }

    fn cmpEq(self: *Self, left: *ir.Value, right: *ir.Value) !*ir.Value {
        return ir.Value.init(self.allocator, .{
            .data = .{ .binary_op = .{ .kind = .i_cmp_eq, .left = left, .right = right } },
            .type_id = self.type_table.getPrimitive(.boolean),
        });
    }

    fn wrapBranch(self: *Self, condition: *ir.Value, then_block: []const ir.Instruction, else_block: []const ir.Instruction) ![]const ir.Instruction {
        const branch = try self.allocator.alloc(ir.Instruction, 1);
        branch[0] = .{ .branch_if = .{ .condition = condition, .then_block = then_block, .else_block = else_block } };
        return branch;
    }

    fn appendMissing(allocator: std.mem.Allocator, missing: *std.ArrayList(u8), name: []const u8) !void {
        if (missing.items.len > 0) try missing.appendSlice(allocator, ", ");
        try missing.appendSlice(allocator, name);
    }

    fn visitAssignStmt(self: *Self, stmt: *const ast.StatementNode) !ir.Instruction {
        const assign_stmt = stmt.data.assign;

        switch (assign_stmt.target.data) {
            .identifier => {
                const id = assign_stmt.target.data.identifier;
                const id_symbol = self.scope.symbol_table.getOrError(id) catch {
                    return self.err_dispatcher.notDefined(id, assign_stmt.target.loc);
                };

                const assignment_value = try self.evalExpExpecting(assign_stmt.exp, id_symbol.type_id);
                const assignment_value_type = assignment_value.type_id;

                if (!id_symbol.kind.variable.is_mut) {
                    return self.err_dispatcher.notMutable(id, assign_stmt.target.loc);
                }

                if (!self.type_table.canCoerce(assignment_value_type, id_symbol.type_id)) {
                    return self.err_dispatcher.invalidType(
                        try self.type_table.name(id_symbol.type_id, self.allocator),
                        try self.type_table.name(assignment_value_type, self.allocator),
                        assign_stmt.exp.loc,
                    );
                }

                const id_symbol_type_ptr = self.type_table.getTypePtrById(id_symbol.type_id);

                if (id_symbol_type_ptr.is_nullable) {
                    if (self.needsNullBox(id_symbol.type_id, assignment_value.type_id)) {
                        return ir.Instruction{ .update_null_box = .{
                            .identifier = id_symbol.identifier,
                            .uid = id_symbol.uid,
                            .value = assignment_value,
                            .type_id = id_symbol.type_id,
                        } };
                    }

                    return ir.Instruction{ .store_var = .{
                        .identifier = id_symbol.identifier,
                        .uid = id_symbol.uid,
                        .value = assignment_value,
                        .type_id = id_symbol.type_id,
                    } };
                }

                return ir.Instruction{ .store_var = .{
                    .identifier = id_symbol.identifier,
                    .uid = id_symbol.uid,
                    .value = assignment_value,
                    .type_id = id_symbol.type_id,
                } };
            },
            .indexed => {
                const index_exp = assign_stmt.target.data.indexed;
                const target = try self.evalExp(index_exp.target);
                const target_type = target.type_id;
                // the assigned member's type is only needed to give `@(...)` a type
                const expected_type_id = if (isAnonymousInit(assign_stmt.exp))
                    (try self.evalExp(assign_stmt.target)).type_id
                else
                    null;
                const assignment_value = try self.evalExpExpecting(assign_stmt.exp, expected_type_id);
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

                var field_maybe_nullable_type_id: tt.TypeId = undefined;

                switch (self.type_table.getTypePtrById(target_type).kind) {
                    .array => {
                        field_maybe_nullable_type_id = self.type_table.getTypePtrById(target_symbol.type_id).kind.array;
                        if (!self.type_table.canCoerce(assignment_value_type, field_maybe_nullable_type_id)) {
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

                            field_maybe_nullable_type_id = field.type_id;

                            if (!self.type_table.canCoerce(assignment_value_type, field.type_id)) {
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

                            field_maybe_nullable_type_id = field.type_id;

                            if (!field.is_mut) {
                                return self.err_dispatcher.notMutable(field.identifier, index_exp.index.loc);
                            }

                            if (!self.type_table.canCoerce(assignment_value_type, field.type_id)) {
                                return self.err_dispatcher.invalidType(
                                    try self.type_table.name(field.type_id, self.allocator),
                                    try self.type_table.name(assignment_value_type, self.allocator),
                                    assign_stmt.exp.loc,
                                );
                            }
                        }
                    },
                    .@"enum" => {
                        const enum_type_ptr = self.type_table.getTypePtrById(target_type);
                        const enum_def = &enum_type_ptr.kind.@"enum";
                        const field = enum_def.static_fields.get(index_exp.index.data.identifier) orelse return self.err_dispatcher.notMutable(
                            index_exp.index.data.identifier,
                            index_exp.index.loc,
                        );

                        field_maybe_nullable_type_id = field.type_id;

                        if (!field.is_mut) {
                            return self.err_dispatcher.notMutable(field.identifier, index_exp.index.loc);
                        }

                        if (!self.type_table.canCoerce(assignment_value_type, field.type_id)) {
                            return self.err_dispatcher.invalidType(
                                try self.type_table.name(field.type_id, self.allocator),
                                try self.type_table.name(assignment_value_type, self.allocator),
                                assign_stmt.exp.loc,
                            );
                        }
                    },
                    .@"union" => {
                        const union_def = &self.type_table.getTypePtrById(target_type).kind.@"union";
                        const member_name = index_exp.index.data.identifier;

                        // variants can't be assigned in place, a new union must be built instead
                        const field = union_def.static_fields.get(member_name) orelse {
                            if (union_def.variants.contains(member_name)) {
                                return self.err_dispatcher.notMutable(member_name, index_exp.index.loc);
                            }
                            return self.err_dispatcher.invalidUnionMember(union_def.identifier, member_name, index_exp.index.loc);
                        };

                        field_maybe_nullable_type_id = field.type_id;

                        if (!field.is_mut) {
                            return self.err_dispatcher.notMutable(field.identifier, index_exp.index.loc);
                        }

                        if (!self.type_table.canCoerce(assignment_value_type, field.type_id)) {
                            return self.err_dispatcher.invalidType(
                                try self.type_table.name(field.type_id, self.allocator),
                                try self.type_table.name(assignment_value_type, self.allocator),
                                assign_stmt.exp.loc,
                            );
                        }
                    },
                    else => unreachable,
                }

                const boxed_value = if (self.needsNullBox(field_maybe_nullable_type_id, assignment_value.type_id))
                    try ir.Value.init(self.allocator, .{
                        .data = .{ .null_box_init = .{ .value = assignment_value } },
                        .type_id = assignment_value.type_id,
                    })
                else
                    assignment_value;

                return ir.Instruction{
                    .update_indexed = .{
                        .target = try self.evalExp(assign_stmt.target),
                        .value = boxed_value,
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
        const for_stmt = stmt.data.@"for";

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

    fn visitAnonymousStructDef(self: *Self, struct_def: *const ast.Struct) !tt.Struct {
        var fields_in_order: std.ArrayList(tt.TypedIdentifier) = .empty;
        var fields: std.StringHashMap(tt.TypedIdentifier) = .init(self.allocator);

        for (struct_def.fields) |field| {
            const struct_field = try self.createField(field, null);
            try fields_in_order.append(
                self.allocator,
                struct_field,
            );
            try fields.put(field.identifier, struct_field);
        }

        var field_index_by_name: std.StringHashMap(u8) = .init(self.allocator);
        for (fields_in_order.items, 0..) |field, i| {
            try field_index_by_name.put(field.identifier, @intCast(i));
        }

        return .{
            .identifier = "@",
            .fields = fields,
            .fields_in_order = try fields_in_order.toOwnedSlice(self.allocator),
            .field_index_by_name = field_index_by_name,
            .static_fields = .init(self.allocator),
            .methods = .init(self.allocator),
        };
    }

    fn visitStructDef(self: *Self, struct_def: *const ast.Struct, identifier: []const u8) Errors!ir.Struct {
        const struct_symbol = try self.scope.symbol_table.getOrError(identifier);
        const blueprint_type_id = struct_symbol.kind.defined_type.blueprint_type_id;
        const prev_return_type = self.scope.return_type_id;
        try self.scope.enter(self.type_table.getPrimitive(.void));
        defer self.scope.exit(prev_return_type);
        var fields: std.ArrayList(ir.Field) = .empty;
        var static_fields: std.ArrayList(ir.Field) = .empty;
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

        for (struct_def.funcs) |func_node| {
            const func = func_node.data.func;
            const fn_type_id = if (self.type_table.getTypePtrById(blueprint_type_id).kind.@"struct".methods.get(func.identifier)) |type_id| type_id else unreachable;
            const fn_metadata = self.type_table.getTypePtrById(fn_type_id).kind.function;
            const fn_name = func.identifier;
            const fn_def = try self.visitFnDef(&func.def, fn_metadata.uid, fn_metadata, fn_name, func_node.loc);
            try funcs.append(self.allocator, fn_def);
        }

        return ir.Struct{
            .uid = struct_symbol.uid,
            .type_id = blueprint_type_id,
            .identifier = identifier,
            .fields = try fields.toOwnedSlice(self.allocator),
            .static_fields = try static_fields.toOwnedSlice(self.allocator),
            .funcs = try funcs.toOwnedSlice(self.allocator),
        };
    }

    fn visitFnDef(
        self: *Self,
        fn_def: *const ast.FnDef,
        uid: usize,
        metadata: tt.Func,
        override_name: ?[]const u8,
        loc: err.Loc,
    ) Errors!ir.Func {
        var params: std.ArrayList(ir.FuncParam) = .empty;

        const old_return_type = self.scope.return_type_id;
        try self.scope.enter(metadata.return_type_id);
        defer self.scope.exit(old_return_type);

        for (metadata.params) |param| {
            const arg_uid = self.scope.genUid();

            if (param.is_mut and self.type_table.getTypePtrById(param.type_id).kind == .@"union") {
                return self.err_dispatcher.unionParamsCantBeMutable(param.identifier, loc);
            }

            if (param.is_mut and self.type_table.getTypePtrById(param.type_id).kind != .array and self.type_table.getTypePtrById(param.type_id).kind != .@"struct") {
                return self.err_dispatcher.primitiveParamsCantBeMutable(loc);
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

        if (!self.type_table.canCoerce(body.return_type, self.scope.return_type_id)) {
            return self.err_dispatcher.invalidFunctionReturnType(
                try self.type_table.name(self.scope.return_type_id, self.allocator),
                try self.type_table.name(body.return_type, self.allocator),
                loc,
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
        const return_stmt = stmt.data.@"return";

        if (return_stmt.exp) |exp| {
            const exp_value = try self.evalExpExpecting(exp, self.scope.return_type_id);
            const exp_value_type = exp_value.type_id;

            if (!self.type_table.canCoerce(exp_value_type, self.scope.return_type_id)) {
                return self.err_dispatcher.invalidFunctionReturnType(
                    try self.type_table.name(self.scope.return_type_id, self.allocator),
                    try self.type_table.name(exp_value_type, self.allocator),
                    exp.loc,
                );
            }

            const boxed_value = if (self.needsNullBox(self.scope.return_type_id, exp_value_type))
                try ir.Value.init(self.allocator, .{
                    .data = .{ .null_box_init = .{ .value = exp_value } },
                    .type_id = exp_value_type,
                })
            else
                exp_value;

            return ir.Instruction{ .return_stmt = .{
                .value = boxed_value,
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

    fn isAnonymousInit(exp: *const ast.ExpNode) bool {
        return exp.data == .anonymous_init;
    }

    // `@(variant = value)` has no union type of its own: when a union is expected it is
    // built as that union, like `U(variant = value)`. Anything else is evaluated as usual.
    fn evalExpExpecting(self: *Self, exp: *const ast.ExpNode, expected_type_id: ?tt.TypeId) Errors!*ir.Value {
        if (expected_type_id) |expected| {
            const base_type_id = self.type_table.typeIdByNullableTypeId.get(expected) orelse expected;
            const base_kind = self.type_table.getTypePtrById(base_type_id).kind;

            if (isAnonymousInit(exp) and base_kind == .@"union") {
                return self.evalUnionInit(exp, exp.data.anonymous_init.arguments, base_type_id);
            }

            if (exp.data == .array_literal and base_kind == .array) {
                return self.evalArrayLiteral(exp, base_kind.array);
            }
        }

        return self.evalExp(exp);
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
                return self.evalArrayLiteral(exp, null);
            },
            .identifier => {
                return self.evalIdentifier(exp);
            },
            .fn_call => {
                return self.evalFnCall(exp);
            },
            .anonymous_init => {
                return self.evalAnonymousStructInit(exp);
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

    //note: too big needs refactor
    fn evalFnCall(self: *Self, exp: *const ast.ExpNode) !*ir.Value {
        const fn_call = exp.data.fn_call;
        var fn_identifier: []const u8 = undefined;
        var uid: usize = undefined;
        var params: []const tt.TypedIdentifier = undefined;
        var return_type: tt.TypeId = undefined;

        var is_struct_init = false;

        var arg_exp_by_param_name = std.StringHashMap(*ast.ExpNode).init(self.allocator);
        defer arg_exp_by_param_name.deinit();

        var fn_call_arguments_values: std.ArrayList(*ir.Value) = .empty;

        switch (fn_call.target.data) {
            .identifier => |id| {
                const symbol = self.scope.symbol_table.getOrError(id) catch {
                    return self.err_dispatcher.notDefined(id, exp.loc);
                };

                fn_identifier = symbol.identifier;

                switch (symbol.kind) {
                    .function => {
                        const func = self.type_table.getTypePtrById(symbol.type_id).kind.function;
                        params = func.params;
                        return_type = func.return_type_id;
                        uid = func.uid;
                    },
                    .defined_type => |dt| {
                        const bt = self.type_table.getTypePtrById(dt.blueprint_type_id);
                        switch (bt.kind) {
                            .@"struct" => {
                                const struct_def = bt.kind.@"struct";
                                params = struct_def.fields_in_order;
                                return_type = dt.blueprint_type_id;
                                uid = symbol.uid;
                                is_struct_init = true;
                            },
                            .@"union" => {
                                return self.evalUnionInit(exp, fn_call.arguments, dt.blueprint_type_id);
                            },
                            .@"enum" => {
                                return self.err_dispatcher.invalidType(
                                    "function or struct",
                                    try self.type_table.name(symbol.type_id, self.allocator),
                                    exp.loc,
                                );
                            },
                            else => unreachable,
                        }
                    },
                    .variable => {
                        const type_ptr = self.type_table.getTypePtrById(symbol.type_id);
                        if (type_ptr.kind == .function) {
                            const func = type_ptr.kind.function;
                            params = func.params;
                            return_type = func.return_type_id;
                            uid = func.uid;
                        } else if (type_ptr.kind == .@"struct") {
                            const struct_def = type_ptr.kind.@"struct";
                            params = struct_def.fields_in_order;
                            return_type = symbol.type_id;
                            uid = symbol.uid;
                            is_struct_init = true;
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
            .indexed => |indexed| {
                const target = try self.evalExp(indexed.target);
                const target_type_id = target.type_id;
                const target_type_ptr = self.type_table.getTypePtrById(target_type_id);

                if (target_type_ptr.kind != .@"struct" and target_type_ptr.kind != .@"enum" and target_type_ptr.kind != .@"union") {
                    return self.err_dispatcher.invalidType(
                        "a defined type such as struct/enums or union",
                        try self.type_table.name(target_type_id, self.allocator),
                        exp.loc,
                    );
                }

                const fn_name = indexed.index.data.identifier;

                switch (target_type_ptr.kind) {
                    .@"struct" => |s| {
                        const fn_type_id = s.methods.get(fn_name) orelse {
                            return self.err_dispatcher.invalidStructFunction(
                                s.identifier,
                                fn_name,
                                exp.loc,
                            );
                        };

                        const fn_metadata = self.type_table.getTypePtrById(fn_type_id).kind.function;

                        if (fn_metadata.params.len > 0 and target_type_id == fn_metadata.params[0].type_id) {
                            if (self.shouldAutoBindSelf(target)) {
                                const is_mutable = self.isFieldMut(target, s.static_fields);
                                if (fn_metadata.params[0].is_mut and !is_mutable) {
                                    return self.err_dispatcher.notMutable(fn_name, indexed.target.loc);
                                }
                                try fn_call_arguments_values.append(self.allocator, target);
                            }
                        }

                        params = fn_metadata.params;
                        return_type = fn_metadata.return_type_id;
                        uid = fn_metadata.uid;
                        fn_identifier = fn_name;
                    },
                    .@"enum" => |e| {
                        const fn_type_id = e.methods.get(fn_name) orelse {
                            return self.err_dispatcher.invalidEnumFunction(
                                e.identifier,
                                fn_name,
                                exp.loc,
                            );
                        };

                        const fn_metadata = self.type_table.getTypePtrById(fn_type_id).kind.function;

                        if (fn_metadata.params.len > 0 and target_type_id == fn_metadata.params[0].type_id) {
                            if (self.shouldAutoBindSelf(target)) {
                                const is_mutable = self.isFieldMut(target, e.static_fields);
                                if (fn_metadata.params[0].is_mut and !is_mutable) {
                                    return self.err_dispatcher.notMutable(fn_name, indexed.target.loc);
                                }
                                try fn_call_arguments_values.append(self.allocator, target);
                            }
                        }

                        params = fn_metadata.params;
                        return_type = fn_metadata.return_type_id;
                        uid = fn_metadata.uid;
                        fn_identifier = fn_name;
                    },
                    .@"union" => |u| {
                        const fn_type_id = u.methods.get(fn_name) orelse {
                            return self.err_dispatcher.invalidUnionFunction(
                                u.identifier,
                                fn_name,
                                exp.loc,
                            );
                        };

                        const fn_metadata = self.type_table.getTypePtrById(fn_type_id).kind.function;

                        if (fn_metadata.params.len > 0 and target_type_id == fn_metadata.params[0].type_id) {
                            if (self.shouldAutoBindSelf(target)) {
                                const is_mutable = self.isFieldMut(target, u.static_fields);
                                if (fn_metadata.params[0].is_mut and !is_mutable) {
                                    return self.err_dispatcher.notMutable(fn_name, indexed.target.loc);
                                }
                                try fn_call_arguments_values.append(self.allocator, target);
                            }
                        }

                        params = fn_metadata.params;
                        return_type = fn_metadata.return_type_id;
                        uid = fn_metadata.uid;
                        fn_identifier = fn_name;
                    },
                    else => unreachable,
                }
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

        if (total_args_len > params.len) {
            return self.err_dispatcher.invalidNumberOfArgs(params.len, total_args_len, exp.loc);
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
                const fn_call_arg_value = try self.evalExpExpecting(_exp, param.type_id);
                const arg_type = fn_call_arg_value.type_id;

                if (!self.type_table.canCoerce(arg_type, param.type_id)) {
                    return self.err_dispatcher.invalidType(try self.type_table.name(param.type_id, self.allocator), try self.type_table.name(arg_type, self.allocator), _exp.loc);
                }

                const arg_type_ptr = self.type_table.getTypePtrById(arg_type);
                if (arg_type_ptr.kind == .array and self.type_table.getTypePtrById(arg_type_ptr.kind.array).kind == .dynamic) {
                    fn_call_arg_value.type_id = param.type_id;
                }

                if (self.type_table.getTypePtrById(param.type_id).kind == .@"struct" and param.is_mut and fn_call_arg_value.data == .identifier) {
                    const symbol = try self.scope.symbol_table.getOrError(fn_call_arg_value.data.identifier.name);
                    if (!symbol.kind.variable.is_mut) {
                        return self.err_dispatcher.notMutable(symbol.identifier, _exp.loc);
                    }
                }

                if (self.type_table.getTypePtrById(param.type_id).kind == .array and param.is_mut and _exp.data != .array_literal) {
                    if (fn_call_arg_value.data == .identifier) {
                        const symbol = try self.scope.symbol_table.getOrError(fn_call_arg_value.data.identifier.name);
                        if (!symbol.kind.variable.is_mut) {
                            return self.err_dispatcher.notMutable(fn_call_arg_value.data.identifier.name, _exp.loc);
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

        for (params, fn_call_arguments_values.items, 0..) |param, _, i| {
            if (self.needsNullBox(param.type_id, fn_call_arguments_values.items[i].type_id)) {
                const current = fn_call_arguments_values.items[i];
                fn_call_arguments_values.items[i] = try ir.Value.init(self.allocator, .{
                    .data = .{ .null_box_init = .{ .value = current } },
                    .type_id = current.type_id,
                });
            }
        }

        if (is_struct_init) {
            var keys: std.ArrayList([]const u8) = .empty;
            for (params) |param| {
                try keys.append(self.allocator, param.identifier);
            }

            return ir.Value.init(
                self.allocator,
                .{
                    .data = .{
                        .struct_init = .{
                            .keys = try keys.toOwnedSlice(self.allocator),
                            .values = try fn_call_arguments_values.toOwnedSlice(self.allocator),
                        },
                    },
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

    fn evalAnonymousStructInit(self: *Self, exp: *const ast.ExpNode) !*ir.Value {
        const anonymous_struct = try self.createAnonymousStruct(exp, false);
        const keys = try self.allocator.alloc([]const u8, anonymous_struct.fields_in_order.len);
        const values = try self.allocator.alloc(*ir.Value, anonymous_struct.fields_in_order.len);

        for (anonymous_struct.fields_in_order, 0..) |field, i| {
            keys[i] = field.identifier;
            values[i] = field.default_value.?;
        }

        return ir.Value.init(self.allocator, .{
            .data = .{ .struct_init = .{ .keys = keys, .values = values } },
            .type_id = try self.type_table.getOrAddAnonymousStruct(anonymous_struct),
        });
    }

    fn evalUnionInit(self: *Self, exp: *const ast.ExpNode, arguments: []const ast.FnCallArg, blueprint_type_id: tt.TypeId) !*ir.Value {
        const union_def = self.type_table.getTypePtrById(blueprint_type_id).kind.@"union";

        if (arguments.len != 1 or arguments[0].identifier == null) {
            return self.err_dispatcher.invalidType(
                "a single named variant argument",
                try self.type_table.name(blueprint_type_id, self.allocator),
                exp.loc,
            );
        }

        const arg = arguments[0];
        const variant_name = arg.identifier.?;

        const variant_type_id = union_def.variants.get(variant_name) orelse {
            return self.err_dispatcher.invalidUnionVariant(union_def.identifier, variant_name, exp.loc);
        };

        const raw_value = try self.evalExpExpecting(arg.exp, variant_type_id);
        if (!self.type_table.canCoerce(raw_value.type_id, variant_type_id)) {
            return self.err_dispatcher.invalidType(
                try self.type_table.name(variant_type_id, self.allocator),
                try self.type_table.name(raw_value.type_id, self.allocator),
                arg.exp.loc,
            );
        }

        const value = if (self.needsNullBox(variant_type_id, raw_value.type_id))
            try ir.Value.init(self.allocator, .{
                .data = .{ .null_box_init = .{ .value = raw_value } },
                .type_id = raw_value.type_id,
            })
        else
            raw_value;

        return ir.Value.init(self.allocator, .{
            .data = .{ .union_init = .{ .tag = variant_name, .value = value } },
            .type_id = blueprint_type_id,
        });
    }

    fn evalIndexedExp(self: *Self, exp: *const ast.ExpNode) !*ir.Value {
        const indexed_exp = exp.data.indexed;
        const is_nullable = indexed_exp.is_nullable;

        const target = try self.evalExp(indexed_exp.target);
        const target_type = target.type_id;

        if (!is_nullable and self.type_table.getTypePtrById(target_type).is_nullable) {
            return self.err_dispatcher.nullableMustBeUnwraped(try self.type_table.name(target_type, self.allocator), exp.loc);
        }

        if (is_nullable and !self.type_table.getTypePtrById(target_type).is_nullable) {
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

            .@"enum" => {
                const enum_def = self.type_table.getTypePtrById(target_type).kind.@"enum";
                return switch (indexed_exp.index.data) {
                    .identifier => |member_name| {
                        if (enum_def.variants.get(member_name)) |value| {
                            return ir.Value.init(self.allocator, .{
                                .data = .{ .i_int = value },
                                .type_id = target_type,
                            });
                        }

                        if (enum_def.static_fields.get(member_name)) |field| {
                            const result_type = if (is_nullable) try self.type_table.getOrAddNullable(field.type_id) else field.type_id;

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
                        }

                        if (enum_def.methods.get(member_name)) |method_type_id| {
                            const result_type = if (is_nullable) try self.type_table.getOrAddNullable(method_type_id) else method_type_id;

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
                        }

                        return self.err_dispatcher.invalidEnumVariant(enum_def.identifier, member_name, exp.loc);
                    },
                    else => unreachable,
                };
            },

            .@"union" => {
                const union_def = self.type_table.getTypePtrById(target_type).kind.@"union";
                return switch (indexed_exp.index.data) {
                    .identifier => |member_name| {
                        // reading a variant is always ?T: null when it isn't the active one
                        const result_type = if (union_def.variants.get(member_name)) |variant_type_id|
                            try self.type_table.getOrAddNullable(variant_type_id)
                        else blk: {
                            const member_type_id = if (union_def.static_fields.get(member_name)) |field|
                                field.type_id
                            else if (union_def.methods.get(member_name)) |method_type_id|
                                method_type_id
                            else
                                return self.err_dispatcher.invalidUnionMember(union_def.identifier, member_name, exp.loc);

                            break :blk if (is_nullable) try self.type_table.getOrAddNullable(member_type_id) else member_type_id;
                        };

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
                return self.err_dispatcher.invalidType("array, struct, enum or union", try self.type_table.name(target_type, self.allocator), exp.loc);
            },
        }
    }

    fn evalIdentifier(self: *Self, exp: *const ast.ExpNode) !*ir.Value {
        const id = exp.data.identifier;
        const id_symbol = self.scope.symbol_table.get(id);

        if (id_symbol) |symbol| {
            const type_id = switch (symbol.kind) {
                .defined_type => |dt| dt.blueprint_type_id,
                else => symbol.type_id,
            };
            return ir.Value.init(self.allocator, .{
                .data = .{ .identifier = .{
                    .uid = symbol.uid,
                    .name = symbol.identifier,
                } },
                .type_id = type_id,
            });
        }

        return self.err_dispatcher.notDefined(id, exp.loc);
    }

    fn isFieldMut(self: *Self, target: *ir.Value, static_fields: std.StringHashMap(tt.TypedIdentifier)) bool {
        var current = target;
        while (true) {
            switch (current.data) {
                .identifier => {
                    const sym = self.scope.symbol_table.get(current.data.identifier.name) orelse return false;
                    return sym.kind.variable.is_mut;
                },
                .indexed => |idx| {
                    // array indexes aren't field names, mutability comes from the root
                    if (idx.index.data == .i_string) {
                        if (static_fields.get(idx.index.data.i_string)) |sf| return sf.is_mut;
                    }
                    current = idx.target;
                },
                else => return false,
            }
        }
    }

    fn shouldAutoBindSelf(self: *Self, target: *ir.Value) bool {
        if (target.data == .identifier) {
            if (self.scope.symbol_table.get(target.data.identifier.name)) |sym| {
                return sym.kind == .variable;
            }
            return false;
        }
        return true;
    }

    fn evalArrayLiteral(self: *Self, exp: *const ast.ExpNode, expected_item_type_id: ?tt.TypeId) !*ir.Value {
        var values: std.ArrayList(*ir.Value) = .empty;
        const array_literal = exp.data.array_literal;
        var array_literal_type: tt.TypeId = self.type_table.getPrimitive(.dynamic);

        for (array_literal.exps) |e| {
            // without a declared item type, later items follow the first one
            const item_type_id = expected_item_type_id orelse
                if (array_literal_type == self.type_table.getPrimitive(.dynamic)) null else array_literal_type;
            const exp_value = try self.evalExpExpecting(e, item_type_id);
            const exp_value_type = exp_value.type_id;

            if (array_literal_type == self.type_table.getPrimitive(.dynamic)) {
                array_literal_type = exp_value_type;
            }

            if (!self.type_table.canCoerce(exp_value_type, array_literal_type)) {
                return self.err_dispatcher.invalidType(try self.type_table.name(array_literal_type, self.allocator), try self.type_table.name(exp_value_type, self.allocator), exp.loc);
            }

            try values.append(self.allocator, exp_value);
        }

        // an empty literal (`list = []`) takes the expected item type when there is one
        if (array_literal.exps.len == 0) {
            if (expected_item_type_id) |item_type_id| array_literal_type = item_type_id;
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

    fn isTypeNumber(self: *Self, t: tt.TypeId) bool {
        return self.type_table.eql(t, self.type_table.getPrimitive(.float)) or self.type_table.eql(t, self.type_table.getPrimitive(.int));
    }

    fn evalBinaryExp(self: *Self, exp: *const ast.ExpNode) !*ir.Value {
        const bin_exp = exp.data.binary_exp;
        const left_value = try self.evalExp(bin_exp.left);
        const right_value = try self.evalExp(bin_exp.right);
        const left_type = left_value.type_id;
        const right_type = right_value.type_id;

        var op_type: tt.TypeId = self.type_table.getPrimitive(.void);

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
                const left_is_nullable = self.type_table.getTypePtrById(left_type).is_nullable;
                const right_is_nullable = self.type_table.getTypePtrById(right_type).is_nullable;
                const left_is_null = self.type_table.getTypePtrById(left_type).kind == .null;
                const right_is_null = self.type_table.getTypePtrById(right_type).kind == .null;

                if (left_is_nullable and right_is_null) {
                    const nnull = try ir.Value.init(self.allocator, .{
                        .data = .{ .unary_op = .{ .kind = .box_not_null, .right = left_value } },
                        .type_id = self.type_table.getPrimitive(.boolean),
                    });

                    if (bin_exp.op == .eq) {
                        return ir.Value.init(self.allocator, .{
                            .data = .{ .unary_op = .{ .kind = .not, .right = nnull } },
                            .type_id = self.type_table.getPrimitive(.boolean),
                        });
                    }

                    return nnull;
                }

                if (right_is_nullable and left_is_null) {
                    const nnull = try ir.Value.init(self.allocator, .{
                        .data = .{ .unary_op = .{ .kind = .box_not_null, .right = right_value } },
                        .type_id = self.type_table.getPrimitive(.boolean),
                    });

                    if (bin_exp.op == .eq) {
                        return ir.Value.init(self.allocator, .{
                            .data = .{ .unary_op = .{ .kind = .not, .right = nnull } },
                            .type_id = self.type_table.getPrimitive(.boolean),
                        });
                    }

                    return nnull;
                }

                op_type = self.type_table.getPrimitive(.boolean);
            },
        }

        return ir.Value.init(self.allocator, .{
            .data = .{
                .binary_op = .{
                    .kind = self.astBinOpToIrBinOpKind(bin_exp.op, left_type, right_type),
                    .left = left_value,
                    .right = right_value,
                },
            },
            .type_id = op_type,
        });
    }

    fn needsNullBox(self: *Self, target_type: tt.TypeId, value_type: tt.TypeId) bool {
        const target = self.type_table.getTypePtrById(target_type);
        if (!target.is_nullable) return false;
        if (value_type == self.type_table.getPrimitive(.null)) return false;
        if (self.type_table.getTypePtrById(value_type).is_nullable) return false;
        const base_id = self.type_table.typeIdByNullableTypeId.get(target_type).?;
        const base = self.type_table.getTypePtrById(base_id);
        return switch (base.kind) {
            .int, .float, .boolean, .@"enum" => true,
            else => false,
        };
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

    pub fn resolveTypeAnnotation(self: *Self, type_annotation: *ast.TypeAnnotation, current_defined_type_type_id: ?tt.TypeId) !tt.TypeId {
        switch (type_annotation.type) {
            .primitive => |primitive_name| {
                if (std.mem.eql(u8, primitive_name, "float")) return if (type_annotation.is_nullable) try self.type_table.getOrAddNullable(self.type_table.getPrimitive(.float)) else self.type_table.getPrimitive(.float);
                if (std.mem.eql(u8, primitive_name, "int")) return if (type_annotation.is_nullable) try self.type_table.getOrAddNullable(self.type_table.getPrimitive(.int)) else self.type_table.getPrimitive(.int);
                if (std.mem.eql(u8, primitive_name, "string")) return if (type_annotation.is_nullable) try self.type_table.getOrAddNullable(self.type_table.getPrimitive(.string)) else self.type_table.getPrimitive(.string);
                if (std.mem.eql(u8, primitive_name, "bool")) return if (type_annotation.is_nullable) try self.type_table.getOrAddNullable(self.type_table.getPrimitive(.boolean)) else self.type_table.getPrimitive(.boolean);
                if (std.mem.eql(u8, primitive_name, "void")) return self.type_table.getPrimitive(.void);
                if (std.mem.eql(u8, primitive_name, "dynamic")) return if (type_annotation.is_nullable) try self.type_table.getOrAddNullable(self.type_table.getPrimitive(.dynamic)) else self.type_table.getPrimitive(.dynamic);

                return Errors.SemaError;
            },
            .@"struct" => |struct_name| {
                const symbol = try self.scope.symbol_table.getOrError(struct_name);
                const blueprint_type_id = switch (symbol.kind) {
                    .defined_type => |dt| dt.blueprint_type_id,
                    else => return Errors.SemaError,
                };
                return if (type_annotation.is_nullable) try self.type_table.getOrAddNullable(blueprint_type_id) else blueprint_type_id;
            },
            .anonymous_struct => |struct_metadata| {
                const anon_struct = try self.visitAnonymousStructDef(&struct_metadata);
                const type_id = try self.type_table.getOrAddAnonymousStruct(anon_struct);
                return if (type_annotation.is_nullable) try self.type_table.getOrAddNullable(type_id) else type_id;
            },
            .array => {
                const inner_type_id = try self.resolveTypeAnnotation(type_annotation.type.array, current_defined_type_type_id);
                return try self.type_table.getOrAddArray(inner_type_id);
            },
            .type_self => {
                if (current_defined_type_type_id) |type_type_id| {
                    return if (type_annotation.is_nullable) try self.type_table.getOrAddNullable(type_type_id) else type_type_id;
                }
                return Errors.SemaError;
            },
        }
    }

    fn getIndexedType(self: *Self, target: *ir.Value, index: *ir.Value) tt.TypeId {
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
            .@"enum" => {
                const enum_def = &target_type.kind.@"enum";
                const field_name = index.data.i_string;
                if (enum_def.static_fields.get(field_name)) |field| {
                    return field.type_id;
                }
                if (enum_def.variants.get(field_name) != null) {
                    return target_type_id;
                }
                if (enum_def.methods.get(field_name)) |method_type_id| {
                    return method_type_id;
                }
                unreachable;
            },
            else => unreachable,
        };
    }

    fn astBinOpToIrBinOpKind(self: *Self, bin_op: ast.BinaryOp, left_type: tt.TypeId, right_type: tt.TypeId) ir.BinaryOpKind {
        const is_float = self.type_table.eql(left_type, self.type_table.getPrimitive(.float)) or self.type_table.eql(right_type, self.type_table.getPrimitive(.float));
        const is_bool = self.type_table.eql(left_type, self.type_table.getPrimitive(.boolean)) or self.type_table.eql(right_type, self.type_table.getPrimitive(.boolean));
        return switch (bin_op) {
            .add => if (is_float) .f_add else .i_add,
            .sub => if (is_float) .f_sub else .i_sub,
            .mult => if (is_float) .f_mult else .i_mult,
            .div => .f_div,
            .trunc_div => .trunc_div,
            .mod => if (is_float) .f_mod else .i_mod,
            .eq => if (is_float) .f_cmp_eq else if (is_bool) .b_cmp_eq else .i_cmp_eq,
            .not_eq => if (is_float) .f_cmp_neq else if (is_bool) .b_cmp_neq else .i_cmp_neq,
            .lt => if (is_float) .f_cmp_lt else .i_cmp_lt,
            .lt_or_eq => if (is_float) .f_cmp_le else .i_cmp_le,
            .gt => if (is_float) .f_cmp_gt else .i_cmp_gt,
            .gt_or_eq => if (is_float) .f_cmp_ge else .i_cmp_ge,
            .bool_or => .b_or,
            .bool_and => .b_and,
        };
    }

    fn astUnaryOpToIrUnaryOpKind(self: *Self, bin_op: ast.UnaryOp, exp_type: tt.TypeId) ir.UnaryOpKind {
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
    return_type_id: tt.TypeId,
    next_uid: usize,
    next_fn_uid: usize,

    pub fn init(alloc: std.mem.Allocator, return_type_id: tt.TypeId, type_table: *tt.TypeTable) !Self {
        const builtins = try buildin.BuiltIn.init(alloc, type_table);
        const generated = try builtins.generate();
        defer alloc.free(generated);

        var symbols = std.StringHashMap(Symbol).init(alloc);

        for (generated) |func| {
            try symbols.put(func.symbol.identifier, func.symbol);
        }

        return Self{
            .allocator = alloc,
            .symbol_table = try SymbolTable.init(alloc, null, symbols),
            .return_type_id = return_type_id,
            .next_uid = generated.len,
            .next_fn_uid = generated.len,
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

    pub fn enter(self: *Self, return_type_id: tt.TypeId) !void {
        self.return_type_id = return_type_id;
        self.symbol_table = try SymbolTable.init(self.allocator, self.symbol_table, null);
    }

    pub fn exit(self: *Self, return_type_id: tt.TypeId) void {
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
    type_id: tt.TypeId,

    kind: union(enum) {
        variable: struct {
            is_mut: bool,
        },
        defined_type: struct {
            blueprint_type_id: tt.TypeId,
        },
        function: void,
    },
};
