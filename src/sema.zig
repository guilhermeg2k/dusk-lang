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

    fn_depth: usize = 0,
    error_edges: std.ArrayList(ErrorEdge) = .empty,
    pending_when_checks: std.ArrayList(PendingWhenCheck) = .empty,
    fn_frames: std.ArrayList(FnFrame) = .empty,
    globals: std.AutoArrayHashMapUnmanaged(usize, []const u8) = .empty,
    global_uses: std.AutoHashMapUnmanaged(usize, std.ArrayList(usize)) = .empty,
    call_edges: std.AutoHashMapUnmanaged(usize, std.ArrayList(usize)) = .empty,
    top_level_calls: std.ArrayList(TopLevelCall) = .empty,
    is_evaluating_default: bool = false,

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

        const instructions = (try self.visitBlock(root)).instructions;
        try self.resolveErrorEdges();
        try self.checkPendingWhens();
        try self.checkGlobalsDeclaredBeforeCalls();

        return .{
            .instructions = instructions,
            .functions = try self.functions.toOwnedSlice(self.allocator),
            .defined_types = try self.defined_types.toOwnedSlice(self.allocator),
            .error_names = self.type_table.error_names.items,
            .global_uids = self.globals.keys(),
        };
    }

    fn resolveErrorEdges(self: *Self) !void {
        var is_changed = true;
        while (is_changed) {
            is_changed = false;
            for (self.error_edges.items) |edge| {
                const to_set = self.errorSetOf(edge.to);
                if (!to_set.is_inferred) continue;
                for (self.errorSetOf(edge.from).ids.keys()) |id| {
                    if (to_set.has(id)) continue;
                    try to_set.ids.put(self.allocator, id, {});
                    is_changed = true;
                }
            }
        }

        for (self.error_edges.items) |edge| {
            const to_set = self.errorSetOf(edge.to);
            if (to_set.is_inferred) continue;
            for (self.errorSetOf(edge.from).ids.keys()) |id| {
                if (to_set.has(id)) continue;
                return self.err_dispatcher.errorNotInSet(
                    self.type_table.error_names.items[@intCast(id)],
                    try self.type_table.name(edge.to, self.allocator),
                    edge.loc,
                );
            }
        }
    }

    fn checkPendingWhens(self: *Self) !void {
        for (self.pending_when_checks.items) |check| {
            const set = self.errorSetOf(check.set_type_id);
            var seen: std.AutoHashMap(i64, void) = .init(self.allocator);

            for (check.when_stmt.prongs, 0..) |prong, prong_idx| {
                for (check.values[prong_idx], 0..) |value, i| {
                    const id = value.data.i_int;
                    if (!set.has(id)) {
                        return self.err_dispatcher.errorNotInSet(
                            self.type_table.error_names.items[@intCast(id)],
                            try self.type_table.name(check.set_type_id, self.allocator),
                            prong.pattern.values[i].loc,
                        );
                    }
                    try seen.put(id, {});
                }
            }

            var missing: std.ArrayList(u8) = .empty;
            for (set.ids.keys()) |id| {
                if (!seen.contains(id)) try appendMissing(self.allocator, &missing, self.type_table.error_names.items[@intCast(id)]);
            }
            if (check.is_null_missing) try appendMissing(self.allocator, &missing, "null");

            if (check.when_stmt.else_block == null and missing.items.len > 0) {
                return self.err_dispatcher.whenNotExhaustive(missing.items, check.when_loc);
            }
            if (check.when_stmt.else_block != null and missing.items.len == 0) {
                return self.err_dispatcher.whenUselessElse(check.when_stmt.else_loc.?);
            }
        }
    }

    fn errorSetOf(self: *Self, set_type_id: tt.TypeId) *tt.ErrorSet {
        return self.type_table.getTypePtrById(set_type_id).kind.@"enum".error_set.?;
    }

    fn createErrorSetSymbol(self: *Self, stmt: *const ast.ErrorSetStmt) !Symbol {
        var variants: std.StringHashMap(i64) = .init(self.allocator);
        const variants_in_order = try self.allocator.alloc([]const u8, stmt.variants.len);
        const error_set = try self.allocator.create(tt.ErrorSet);
        error_set.* = .{ .is_inferred = false };

        for (stmt.variants, 0..) |variant, i| {
            if (variants.contains(variant.identifier)) {
                return self.err_dispatcher.alreadyDefined(variant.identifier, variant.loc);
            }
            const id = try self.type_table.addErrorName(stmt.identifier, variant.identifier);
            try variants.put(variant.identifier, id);
            try error_set.ids.put(self.allocator, id, {});
            variants_in_order[i] = variant.identifier;
        }

        const blueprint_type_id = try self.type_table.addType(.{
            .kind = .{
                .@"enum" = .{
                    .identifier = stmt.identifier,
                    .variants = variants,
                    .variants_in_order = variants_in_order,
                    .static_fields = .init(self.allocator),
                    .methods = .init(self.allocator),
                    .error_set = error_set,
                },
            },
            .is_nullable = false,
        });

        return Symbol{
            .identifier = stmt.identifier,
            .uid = self.scope.genUid(),
            .type_id = self.type_table.getPrimitive(.meta),
            .kind = .{ .defined_type = .{ .blueprint_type_id = blueprint_type_id } },
        };
    }

    fn createInferredErrorSet(self: *Self, fn_name: []const u8) !tt.TypeId {
        const error_set = try self.allocator.create(tt.ErrorSet);
        error_set.* = .{ .is_inferred = true };

        return self.type_table.addType(.{
            .kind = .{
                .@"enum" = .{
                    .identifier = fn_name,
                    .variants = .init(self.allocator),
                    .static_fields = .init(self.allocator),
                    .methods = .init(self.allocator),
                    .error_set = error_set,
                },
            },
            .is_nullable = false,
        });
    }

    fn resolveFnErrorSet(self: *Self, fn_error_set: ast.FnErrorSet, fn_name: []const u8) !tt.TypeId {
        switch (fn_error_set) {
            .inferred => return self.createInferredErrorSet(fn_name),
            .named => |named| {
                const symbol = self.scope.symbol_table.getOrError(named.identifier) catch {
                    return self.err_dispatcher.typeNotDefined(named.identifier, named.loc);
                };
                if (symbol.kind != .defined_type or !self.type_table.isErrorSet(symbol.kind.defined_type.blueprint_type_id)) {
                    return self.err_dispatcher.notAnErrorSet(named.identifier, named.loc);
                }
                return symbol.kind.defined_type.blueprint_type_id;
            },
        }
    }

    fn canCoerce(self: *Self, from: tt.TypeId, to: tt.TypeId, loc: err.Loc) !bool {
        if (self.type_table.canCoerce(from, to)) return true;

        const from_is_nullable = self.type_table.getTypePtrById(from).is_nullable;
        const to_is_nullable = self.type_table.getTypePtrById(to).is_nullable;
        if (from_is_nullable and !to_is_nullable) return false;

        const from_base = self.type_table.typeIdByNullableTypeId.get(from) orelse from;
        const to_base = self.type_table.typeIdByNullableTypeId.get(to) orelse to;
        if (!self.type_table.isErrorSet(from_base) or !self.type_table.isErrorSet(to_base)) return false;
        if (!self.errorSetOf(from_base).is_inferred or self.errorSetOf(to_base).is_inferred) return false;

        try self.addErrorEdge(from_base, to_base, loc);
        return true;
    }

    fn checkPayloadErrorOverlap(self: *Self, error_set_type_id: tt.TypeId, payload: tt.TypeId, loc: err.Loc) !void {
        const payload_base = self.type_table.typeIdByNullableTypeId.get(payload) orelse payload;
        if (!self.type_table.isErrorSet(payload_base)) return;
        const error_set = self.errorSetOf(error_set_type_id);
        if (error_set.is_inferred) return;
        for (self.errorSetOf(payload_base).ids.keys()) |id| {
            if (!error_set.has(id)) continue;
            return self.err_dispatcher.payloadSharesErrors(
                try self.type_table.name(payload, self.allocator),
                try self.type_table.name(error_set_type_id, self.allocator),
                self.type_table.error_names.items[@intCast(id)],
                loc,
            );
        }
    }

    fn appendToList(self: *Self, map: *std.AutoHashMapUnmanaged(usize, std.ArrayList(usize)), key: usize, value: usize) !void {
        const entry = try map.getOrPut(self.allocator, key);
        if (!entry.found_existing) entry.value_ptr.* = .empty;
        try entry.value_ptr.append(self.allocator, value);
    }

    fn noteVariableUse(self: *Self, symbol: Symbol) Errors!void {
        if (symbol.kind != .variable) return;
        const frames = self.fn_frames.items;
        if (frames.len == 0) return;
        if (symbol.uid >= frames[0].first_local_uid) return;
        try self.globals.put(self.allocator, symbol.uid, symbol.identifier);
        try self.appendToList(&self.global_uses, frames[frames.len - 1].fn_uid, symbol.uid);
    }

    fn noteCall(self: *Self, callee_uid: usize, callee_name: []const u8, loc: err.Loc) Errors!void {
        if (self.fn_frames.getLastOrNull()) |frame| {
            if (!self.is_evaluating_default) return self.appendToList(&self.call_edges, frame.fn_uid, callee_uid);
        }
        try self.top_level_calls.append(self.allocator, .{
            .callee_uid = callee_uid,
            .callee_name = callee_name,
            .first_undeclared_uid = if (self.is_evaluating_default) 0 else self.scope.next_uid,
            .is_default_value = self.is_evaluating_default,
            .loc = loc,
        });
    }

    fn evalDefaultValue(self: *Self, exp: *const ast.ExpNode, expected_type_id: ?tt.TypeId) Errors!*ir.Value {
        const was_evaluating_default = self.is_evaluating_default;
        self.is_evaluating_default = true;
        defer self.is_evaluating_default = was_evaluating_default;
        return self.evalExpExpecting(exp, expected_type_id);
    }

    fn checkGlobalsDeclaredBeforeCalls(self: *Self) Errors!void {
        for (self.top_level_calls.items) |call| {
            var visited: std.AutoHashMapUnmanaged(usize, void) = .empty;
            defer visited.deinit(self.allocator);
            const global_uid = try self.findUndeclaredGlobal(call.callee_uid, call.first_undeclared_uid, &visited) orelse continue;
            const global_name = self.globals.get(global_uid).?;
            if (call.is_default_value) return self.err_dispatcher.globalUsedByDefaultValue(call.callee_name, global_name, call.loc);
            return self.err_dispatcher.globalUsedBeforeDeclared(call.callee_name, global_name, call.loc);
        }
    }

    fn findUndeclaredGlobal(self: *Self, fn_uid: usize, first_undeclared_uid: usize, visited: *std.AutoHashMapUnmanaged(usize, void)) Errors!?usize {
        if ((try visited.getOrPut(self.allocator, fn_uid)).found_existing) return null;
        if (self.global_uses.get(fn_uid)) |uses| {
            for (uses.items) |global_uid| {
                if (global_uid >= first_undeclared_uid) return global_uid;
            }
        }
        if (self.call_edges.get(fn_uid)) |callees| {
            for (callees.items) |callee_uid| {
                if (try self.findUndeclaredGlobal(callee_uid, first_undeclared_uid, visited)) |global_uid| return global_uid;
            }
        }
        return null;
    }

    fn addErrorEdge(self: *Self, from: tt.TypeId, to: tt.TypeId, loc: err.Loc) !void {
        try self.error_edges.append(self.allocator, .{ .from = from, .to = to, .loc = loc });
    }

    pub fn hoistDeclarations(self: *Self, root: *const ast.Root) !void {
        for (root.statements.items) |stmt| {
            switch (stmt.data) {
                .error_set => |es| {
                    const symbol = try self.createErrorSetSymbol(&es);
                    self.scope.symbol_table.put(symbol) catch return self.err_dispatcher.alreadyDefined(es.identifier, stmt.loc);
                },
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
                    try self.fulfillUnionType(symbol, &us.def, stmt.loc);
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

    fn fulfillUnionType(self: *Self, union_symbol: Symbol, union_def: *const ast.UnionDef, loc: err.Loc) !void {
        const blueprint_type_id = union_symbol.kind.defined_type.blueprint_type_id;

        if (union_def.variants.len > tt.Union.MAX_VARIANTS) {
            return self.err_dispatcher.tooManyUnionVariants(union_symbol.identifier, tt.Union.MAX_VARIANTS, loc);
        }

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
            const value = try self.evalDefaultValue(initial_value, if (field.type != null) field_type_id else null);
            const value_type_id = value.type_id;
            field_default_value = value;

            if (field.type == null) {
                field_type_id = value_type_id;
            } else if (!try self.canCoerce(value_type_id, field_type_id, field.default_value.?.loc)) {
                return self.err_dispatcher.invalidType(
                    try field.type.?.value(self.allocator),
                    try self.type_table.name(value_type_id, self.allocator),
                    field.default_value.?.loc,
                );
            }
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
        self.fn_depth += 1;
        defer self.fn_depth -= 1;

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
                    const value = try self.evalDefaultValue(default_value, param_type_id);
                    param_default_value = value;
                    const value_type_id = value.type_id;
                    if (!try self.canCoerce(value_type_id, param_type_id, param.default_value.?.loc)) {
                        return self.err_dispatcher.invalidType(
                            try self.type_table.name(param_type_id, self.allocator),
                            try self.type_table.name(value_type_id, self.allocator),
                            param.default_value.?.loc,
                        );
                    }
                }
            } else {
                if (param.default_value) |default_value| {
                    const value = try self.evalDefaultValue(default_value, null);
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

        if (fn_def.error_set) |fn_error_set| {
            const fn_name = self.type_table.getTypePtrById(fn_symbol.type_id).kind.function.identifier;
            const error_set_type_id = try self.resolveFnErrorSet(fn_error_set, fn_name);
            try self.checkPayloadErrorOverlap(error_set_type_id, return_type_id, loc);
            return_type_id = try self.type_table.addType(.{
                .kind = .{ .error_union = .{ .error_set = error_set_type_id, .payload = return_type_id } },
                .is_nullable = false,
            });
        }

        //restore main scope
        self.scope.exit(self.type_table.getPrimitive(.void));

        self.type_table.getTypePtrById(fn_symbol.type_id).kind.function.return_type_id = return_type_id;
        self.type_table.getTypePtrById(fn_symbol.type_id).kind.function.params = try params_metadata.toOwnedSlice(self.allocator);
    }

    pub fn visitBlock(self: *Self, block: *const ast.Block) Errors!ir.Block {
        return self.visitBranchBlock(block, null);
    }

    fn visitBranchBlock(self: *Self, block: *const ast.Block, result: ?*BranchResult) Errors!ir.Block {
        var instructions: std.ArrayList(ir.Instruction) = .empty;
        try self.scope.enter(self.scope.return_type_id);
        defer self.scope.exit(self.scope.return_type_id);

        var return_type: tt.TypeId = self.type_table.getPrimitive(.void);

        for (block.statements.items, 0..) |stmt, statement_index| {
            const is_value_tail = result != null and statement_index + 1 == block.statements.items.len;
            if (is_value_tail and stmt.data == .expression) {
                try instructions.append(self.allocator, try self.visitBranchTail(&stmt, result.?));
                break;
            }
            const instruction = switch (stmt.data) {
                .let => try self.visitLetStmt(&stmt),
                .func => try self.visitFuncStmt(&stmt),
                .@"struct" => try self.visitStructStmt(&stmt),
                .@"enum" => try self.visitEnumStmt(&stmt),
                .@"union" => try self.visitUnionStmt(&stmt),
                .error_set => null,
                .assign => try self.visitAssignStmt(&stmt),
                .@"for" => try self.visitForStmt(&stmt),
                .expression => try self.visitExpressionStmt(&stmt),
                .@"break" => ir.Instruction{ .break_stmt = {} },
                .@"continue" => ir.Instruction{ .continue_stmt = {} },
                .@"return" => try self.visitReturnStmt(&stmt),
                .for_capture => ir.Instruction{ .scope = try self.visitForCaptureStmt(&stmt) },
            };

            if (instruction) |i| {
                try instructions.append(self.allocator, i);
            }
            if (result != null and isBlockLeaving(instructions.items)) break;
        }

        if (result != null) {
            const has_value_tail = block.statements.items.len > 0 and block.statements.items[block.statements.items.len - 1].data == .expression;
            if (!has_value_tail and !isBlockLeaving(instructions.items)) {
                const loc = if (block.statements.items.len > 0) block.statements.items[block.statements.items.len - 1].loc else result.?.loc;
                return self.err_dispatcher.invalidExpression("a final expression or an early exit", "a branch without a value", loc);
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

    fn visitBranchTail(self: *Self, statement: *const ast.StatementNode, result: *BranchResult) Errors!ir.Instruction {
        const value = try self.evalExpWithContext(statement.data.expression, result.expected_type_id, result.context_type_id);
        return self.storeBranchValue(value, result, statement.loc);
    }

    fn storeBranchValue(self: *Self, value: *ir.Value, result: *BranchResult, loc: err.Loc) Errors!ir.Instruction {
        if (isValueLeaving(value)) return .{ .expression_stmt = .{ .value = value } };
        if (value.type_id == self.type_table.getPrimitive(.void)) {
            return self.err_dispatcher.invalidExpression("a branch value", "void", loc);
        }

        const stored_value = try ir.Value.init(self.allocator, value.*);
        try result.values.append(self.allocator, .{ .value = stored_value, .loc = loc });
        return .{ .store_var = .{
            .uid = result.uid,
            .identifier = "branch result",
            .type_id = stored_value.type_id,
            .value = stored_value,
        } };
    }

    fn areAllPathsReturning(instructions: []const ir.Instruction) bool {
        for (instructions) |inst| {
            switch (inst) {
                .return_stmt, .return_error => return true,
                .store_var => |store| {
                    if (isValueReturning(store.value)) return true;
                },
                .expression_stmt => |expression| {
                    if (isValueReturning(expression.value)) return true;
                },
                .update_indexed => |update| {
                    if (isValueReturning(update.target) or isValueReturning(update.value)) return true;
                },
                .update_null_box => |update| {
                    if (update.value) |value| {
                        if (isValueReturning(value)) return true;
                    }
                },
                .loop => |loop| {
                    if (loop.condition) |condition| {
                        if (isValueReturning(condition)) return true;
                    }
                },
                .branch_if => |branch| {
                    if (isValueReturning(branch.condition)) return true;
                    if (branch.else_block.len > 0 and areAllPathsReturning(branch.then_block) and areAllPathsReturning(branch.else_block)) return true;
                },
                .scope => |scoped| {
                    if (areAllPathsReturning(scoped)) return true;
                },
                else => {},
            }
        }
        return false;
    }

    fn isValueReturning(value: *const ir.Value) bool {
        return doesValueExit(value, true);
    }

    fn isValueLeaving(value: *const ir.Value) bool {
        return doesValueExit(value, false);
    }

    fn doesValueExit(value: *const ir.Value, needs_return: bool) bool {
        return switch (value.data) {
            .block_value => |block| if (needs_return) areAllPathsReturning(block.instructions) else !block.can_complete,
            .int_to_float, .array_length => |operand| doesValueExit(operand, needs_return),
            .binary_op => |binary| doesValueExit(binary.left, needs_return) or doesValueExit(binary.right, needs_return),
            .unary_op => |unary| doesValueExit(unary.right, needs_return),
            .null_box_init => |box| if (box.value) |operand| doesValueExit(operand, needs_return) else false,
            .fn_call => |call| doAnyValuesExit(call.args, needs_return),
            .struct_init => |initialization| doAnyValuesExit(initialization.values, needs_return),
            .i_array => |array| doAnyValuesExit(array.values, needs_return),
            .string_interpolation => |parts| doAnyValuesExit(parts, needs_return),
            .union_init => |initialization| if (initialization.value) |operand| doesValueExit(operand, needs_return) else false,
            .indexed => |indexed| doesValueExit(indexed.target, needs_return) or doesValueExit(indexed.index, needs_return),
            .union_tag, .union_payload => |operand| doesValueExit(operand, needs_return),
            .try_call => |call| doesValueExit(call.call, needs_return),
            .catch_call => |call| doesValueExit(call.call, needs_return),
            else => false,
        };
    }

    fn doAnyValuesExit(values: []const *ir.Value, needs_return: bool) bool {
        for (values) |value| {
            if (doesValueExit(value, needs_return)) return true;
        }
        return false;
    }

    fn isBlockLeaving(instructions: []const ir.Instruction) bool {
        for (instructions) |inst| {
            switch (inst) {
                .return_stmt, .return_error, .break_stmt, .continue_stmt => return true,
                .store_var => |store| {
                    if (isValueLeaving(store.value)) return true;
                },
                .expression_stmt => |expression| {
                    if (isValueLeaving(expression.value)) return true;
                },
                .update_indexed => |update| {
                    if (isValueLeaving(update.target) or isValueLeaving(update.value)) return true;
                },
                .update_null_box => |update| {
                    if (update.value) |value| {
                        if (isValueLeaving(value)) return true;
                    }
                },
                .loop => |loop| {
                    if (loop.condition) |condition| {
                        if (isValueLeaving(condition)) return true;
                    }
                },
                .branch_if => |branch| {
                    if (isValueLeaving(branch.condition)) return true;
                    if (branch.else_block.len > 0 and isBlockLeaving(branch.then_block) and isBlockLeaving(branch.else_block)) return true;
                },
                .scope => |scoped| {
                    if (isBlockLeaving(scoped)) return true;
                },
                else => {},
            }
        }
        return false;
    }

    fn visitExpressionStmt(self: *Self, stmt: *const ast.StatementNode) Errors!ir.Instruction {
        const expression_stmt = stmt.data.expression;

        const value = switch (expression_stmt.data) {
            .if_exp => return self.lowerIf(expression_stmt, null),
            .if_capture_exp => return ir.Instruction{ .scope = try self.lowerIfCapture(expression_stmt, null) },
            .when_exp => return ir.Instruction{ .scope = try self.lowerWhen(expression_stmt, null) },
            .fn_call, .orelse_exp => try self.evalExp(expression_stmt),
            .try_exp => try self.evalTry(expression_stmt),
            .catch_exp => try self.evalCatch(expression_stmt, true),
            else => return self.err_dispatcher.invalidType("function", @tagName(expression_stmt.data), expression_stmt.loc),
        };

        return ir.Instruction{
            .expression_stmt = .{ .value = value },
        };
    }

    fn evalFailingCall(self: *Self, exp: *const ast.ExpNode, keyword: []const u8) Errors!*ir.Value {
        if (exp.data != .fn_call) {
            return self.err_dispatcher.notAFailingCall(keyword, exp.loc);
        }

        const call = try self.evalFnCall(exp);
        if (!self.type_table.isErrorUnion(call.type_id)) {
            return self.err_dispatcher.notAFailingCall(keyword, exp.loc);
        }

        return call;
    }

    fn evalTry(self: *Self, exp: *const ast.ExpNode) Errors!*ir.Value {
        const call = try self.evalFailingCall(exp.data.try_exp.operand, "try");
        const error_union = self.type_table.getTypePtrById(call.type_id).kind.error_union;
        const is_top_level = self.fn_depth == 0;

        if (!is_top_level) {
            if (!self.type_table.isErrorUnion(self.scope.return_type_id)) {
                return self.err_dispatcher.tryOutsideFailingFunction(exp.loc);
            }
            const current_set = self.type_table.getTypePtrById(self.scope.return_type_id).kind.error_union.error_set;
            try self.addErrorEdge(error_union.error_set, current_set, exp.loc);
        }

        return ir.Value.init(self.allocator, .{
            .data = .{ .try_call = .{ .call = call, .is_top_level = is_top_level } },
            .type_id = error_union.payload,
        });
    }

    fn evalCatch(self: *Self, exp: *const ast.ExpNode, is_statement: bool) Errors!*ir.Value {
        const catch_exp = exp.data.catch_exp;
        const call = try self.evalFailingCall(catch_exp.operand, "catch");
        const error_union = self.type_table.getTypePtrById(call.type_id).kind.error_union;
        const payload = error_union.payload;

        switch (catch_exp.fallback) {
            .value => |fallback_exp| {
                const fallback = try self.evalExpExpecting(fallback_exp, payload);
                if (!try self.canCoerce(fallback.type_id, payload, fallback_exp.loc)) {
                    return self.err_dispatcher.invalidType(
                        try self.type_table.name(payload, self.allocator),
                        try self.type_table.name(fallback.type_id, self.allocator),
                        fallback_exp.loc,
                    );
                }

                const boxed_fallback = if (self.needsNullBox(payload, fallback.type_id))
                    try ir.Value.init(self.allocator, .{
                        .data = .{ .null_box_init = .{ .value = fallback } },
                        .type_id = fallback.type_id,
                    })
                else
                    fallback;

                return ir.Value.init(self.allocator, .{
                    .data = .{ .catch_call = .{ .call = call, .fallback = .{ .value = boxed_fallback } } },
                    .type_id = payload,
                });
            },
            .block => |block| {
                const captured_symbol = Symbol{
                    .uid = self.scope.genUid(),
                    .identifier = block.capture,
                    .type_id = error_union.error_set,
                    .kind = .{ .variable = .{ .is_mut = false } },
                };

                try self.scope.enter(self.scope.return_type_id);
                defer self.scope.exit(self.scope.return_type_id);
                try self.scope.symbol_table.put(captured_symbol);

                const body = try self.visitBlock(&block.body);
                const is_value_used = !is_statement and payload != self.type_table.getPrimitive(.void);
                if (is_value_used and !isBlockLeaving(body.instructions)) {
                    return self.err_dispatcher.catchBlockMustLeave(exp.loc);
                }

                return ir.Value.init(self.allocator, .{
                    .data = .{ .catch_call = .{
                        .call = call,
                        .fallback = .{ .block = .{
                            .capture_uid = captured_symbol.uid,
                            .capture_identifier = captured_symbol.identifier,
                            .capture_type_id = captured_symbol.type_id,
                            .instructions = body.instructions,
                        } },
                    } },
                    .type_id = payload,
                });
            },
        }
    }

    fn callName(exp: *const ast.ExpNode) []const u8 {
        return switch (exp.data.fn_call.target.data) {
            .identifier => |id| id,
            .indexed => |indexed| if (indexed.index.data == .identifier) indexed.index.data.identifier else "function",
            else => "function",
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

        if (!try self.canCoerce(expression_type_id, var_type_id, stmt.data.let.value.loc)) {
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
            const annotated_type = if (field.type) |annotation| try self.resolveTypeAnnotation(annotation, blueprint_type_id) else null;
            const raw_default: ?*ir.Value = if (field.default_value) |value| try self.evalDefaultValue(value, annotated_type) else null;
            const field_type = annotated_type orelse raw_default.?.type_id;

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
            const annotated_type = if (field.type) |annotation| try self.resolveTypeAnnotation(annotation, blueprint_type_id) else null;
            const raw_default: ?*ir.Value = if (field.default_value) |value| try self.evalDefaultValue(value, annotated_type) else null;
            const field_type = annotated_type orelse raw_default.?.type_id;

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

    fn lowerIf(self: *Self, exp: *const ast.ExpNode, result: ?*BranchResult) Errors!ir.Instruction {
        const if_stmt = exp.data.if_exp;
        if (result != null and if_stmt.else_block == null) {
            return self.err_dispatcher.invalidExpression("else in an if expression", "if without else", exp.loc);
        }

        const condition_value = try self.evalExp(if_stmt.condition);
        const condition_value_type = condition_value.type_id;

        if (!self.type_table.eql(condition_value_type, self.type_table.getPrimitive(.boolean))) {
            return self.err_dispatcher.invalidType("boolean", try self.type_table.name(condition_value_type, self.allocator), if_stmt.condition.loc);
        }

        return ir.Instruction{ .branch_if = .{
            .condition = condition_value,
            .then_block = (try self.visitBranchBlock(&if_stmt.then_block, result)).instructions,
            .else_block = if (if_stmt.else_block) |else_blc| (try self.visitBranchBlock(&else_blc, result)).instructions else &.{},
        } };
    }

    fn lowerIfCapture(self: *Self, exp: *const ast.ExpNode, result: ?*BranchResult) Errors![]ir.Instruction {
        const capture = exp.data.if_capture_exp;
        if (result != null and capture.else_block == null) {
            return self.err_dispatcher.invalidExpression("else in an if expression", "if without else", exp.loc);
        }
        const subject = try self.evalExp(capture.exp);
        if (!self.type_table.getTypePtrById(subject.type_id).is_nullable) {
            return self.err_dispatcher.invalidType("nullable", try self.type_table.name(subject.type_id, self.allocator), capture.exp.loc);
        }
        if (capture.identifier.is_mut and !self.isExpressionMutable(capture.exp)) {
            return self.err_dispatcher.unwrappedValueCantBeMutable(capture.identifier.name, exp.loc);
        }

        var instructions: std.ArrayList(ir.Instruction) = .empty;
        const subject_uid = self.scope.genUid();
        try instructions.append(self.allocator, .{ .store_var = .{
            .uid = subject_uid,
            .identifier = "capture subject",
            .type_id = subject.type_id,
            .value = subject,
        } });
        const subject_ref = try ir.Value.init(self.allocator, .{
            .data = .{ .identifier = .{ .uid = subject_uid, .name = "capture subject" } },
            .type_id = subject.type_id,
        });
        const captured_type_id = self.type_table.typeIdByNullableTypeId.get(subject.type_id).?;
        const captured_symbol = Symbol{
            .uid = self.scope.genUid(),
            .identifier = capture.identifier.name,
            .type_id = captured_type_id,
            .kind = .{ .variable = .{ .is_mut = capture.identifier.is_mut } },
        };
        var then_instructions: std.ArrayList(ir.Instruction) = .empty;
        const captured_needs_unwrap = self.needsNullBox(subject.type_id, captured_type_id);
        try then_instructions.append(self.allocator, if (captured_needs_unwrap)
            .{ .unwrap_null_box = .{
                .unwrapped = .{ .uid = captured_symbol.uid, .identifier = captured_symbol.identifier, .type_id = captured_type_id },
                .null_box = subject_ref,
            } }
        else
            .{ .store_var = .{ .uid = captured_symbol.uid, .identifier = captured_symbol.identifier, .type_id = captured_type_id, .value = subject_ref } });
        {
            try self.scope.enter(self.scope.return_type_id);
            defer self.scope.exit(self.scope.return_type_id);
            try self.scope.symbol_table.put(captured_symbol);
            try then_instructions.appendSlice(self.allocator, (try self.visitBranchBlock(&capture.body, result)).instructions);
        }
        const else_instructions = if (capture.else_block) |else_block| (try self.visitBranchBlock(&else_block, result)).instructions else &.{};
        const condition = try ir.Value.init(self.allocator, .{
            .data = .{ .unary_op = .{ .kind = .box_not_null, .right = subject_ref } },
            .type_id = self.type_table.getPrimitive(.boolean),
        });
        try instructions.append(self.allocator, .{ .branch_if = .{
            .condition = condition,
            .then_block = try then_instructions.toOwnedSlice(self.allocator),
            .else_block = else_instructions,
        } });
        return instructions.toOwnedSlice(self.allocator);
    }

    fn lowerWhen(self: *Self, exp: *const ast.ExpNode, result: ?*BranchResult) Errors![]ir.Instruction {
        const when_stmt = exp.data.when_exp;
        const subject = try self.resolveWhenSubject(when_stmt.subject);
        const prongs = try self.checkWhenProngs(&when_stmt, subject);
        try self.checkWhenExhaustive(exp, subject, prongs);

        var instructions: std.ArrayList(ir.Instruction) = .empty;
        const refs = try self.storeWhenSubject(&instructions, subject);

        var chain: []const ir.Instruction = if (when_stmt.else_block) |else_blc| (try self.visitBranchBlock(&else_blc, result)).instructions else &.{};

        var null_insts: []const ir.Instruction = &.{};
        if (subject.is_nullable and !prongs.has_null_case) {
            null_insts = (try self.visitBranchBlock(&when_stmt.else_block.?, result)).instructions;
        }

        const unconditional_prong_idx = if (when_stmt.else_block == null) findUnconditionalProng(when_stmt.prongs) else null;

        var prong_idx = when_stmt.prongs.len;
        while (prong_idx > 0) {
            prong_idx -= 1;
            const prong = when_stmt.prongs[prong_idx];

            const lowered = switch (prong.pattern) {
                .null_case => {
                    null_insts = (try self.visitBranchBlock(&prong.body, result)).instructions;
                    continue;
                },
                .variant => try self.lowerWhenVariantProng(&prong, subject, refs.unwrapped, result),
                .values => try self.lowerWhenValuesProng(&prong, prongs.values[prong_idx], prongs.ranges[prong_idx], refs.unwrapped, result),
            };

            chain = if (unconditional_prong_idx == prong_idx) lowered.body else try self.wrapBranch(lowered.condition, lowered.body, chain);
        }

        if (subject.is_nullable) {
            const not_null = try ir.Value.init(self.allocator, .{
                .data = .{ .unary_op = .{ .kind = .box_not_null, .right = refs.subject } },
                .type_id = self.type_table.getPrimitive(.boolean),
            });
            chain = try self.wrapBranch(not_null, chain, null_insts);
        }

        try instructions.appendSlice(self.allocator, chain);
        return instructions.toOwnedSlice(self.allocator);
    }

    fn resolveWhenSubject(self: *Self, exp: *const ast.ExpNode) Errors!WhenSubject {
        const value = try self.evalExp(exp);
        const type_name = try self.type_table.name(value.type_id, self.allocator);
        const is_nullable = self.type_table.getTypePtrById(value.type_id).is_nullable;
        const base_type = self.type_table.typeIdByNullableTypeId.get(value.type_id) orelse value.type_id;
        const base_kind = self.type_table.getTypePtrById(base_type).kind;

        switch (base_kind) {
            .int, .float, .@"enum", .@"union" => {},
            else => return self.err_dispatcher.invalidType("int, float, enum or union", type_name, exp.loc),
        }

        return .{
            .value = value,
            .type_name = type_name,
            .is_nullable = is_nullable,
            .base_type = base_type,
            .base_kind = base_kind,
        };
    }

    fn checkWhenProngs(self: *Self, when_stmt: *const ast.WhenStmt, subject: WhenSubject) Errors!WhenProngs {
        var prongs: WhenProngs = .{
            .values = try self.allocator.alloc([]*ir.Value, when_stmt.prongs.len),
            .ranges = try self.allocator.alloc([]?WhenRange, when_stmt.prongs.len),
            .seen_values = .init(self.allocator),
            .seen_variants = .init(self.allocator),
            .has_null_case = false,
        };

        for (when_stmt.prongs, 0..) |prong, prong_idx| {
            prongs.values[prong_idx] = &.{};
            prongs.ranges[prong_idx] = &.{};

            switch (prong.pattern) {
                .null_case => {
                    if (!subject.is_nullable) {
                        return self.err_dispatcher.whenInvalidCase(
                            try std.fmt.allocPrint(self.allocator, "'null' case on non-nullable '{s}'", .{subject.type_name}),
                            prong.loc,
                        );
                    }
                    if (prongs.has_null_case) return self.err_dispatcher.whenDuplicateCase("null", prong.loc);
                    prongs.has_null_case = true;
                },
                .variant => |variant| {
                    if (subject.base_kind != .@"union") {
                        return self.err_dispatcher.whenInvalidCase(
                            try std.fmt.allocPrint(self.allocator, "'variant: capture' cases are only for unions, found '{s}'", .{subject.type_name}),
                            prong.loc,
                        );
                    }
                    if (!subject.base_kind.@"union".variants.contains(variant.name)) {
                        return self.err_dispatcher.invalidUnionVariant(subject.base_kind.@"union".identifier, variant.name, prong.loc);
                    }
                    if (prongs.seen_variants.contains(variant.name)) return self.err_dispatcher.whenDuplicateCase(variant.name, prong.loc);
                    try prongs.seen_variants.put(variant.name, {});
                },
                .values => |exps| {
                    if (subject.base_kind == .@"union") {
                        return self.err_dispatcher.whenInvalidCase("union cases are written as 'variant: capture'", prong.loc);
                    }

                    const values = try self.allocator.alloc(*ir.Value, exps.len);
                    const ranges = try self.allocator.alloc(?WhenRange, exps.len);

                    for (exps, 0..) |exp, i| {
                        if (exp.data == .range) {
                            const range = try self.evalWhenRange(exp, subject);
                            try self.checkWhenRangeOverlap(&prongs, range, exp.loc);
                            values[i] = range.start;
                            ranges[i] = range;
                            continue;
                        }

                        if (subject.base_kind == .float) return self.err_dispatcher.whenInvalidCase("float cases must be ranges, like '0.0..1.0'", exp.loc);
                        const value = try self.evalWhenCaseValue(exp, subject.base_type);
                        const key = value.data.i_int;

                        if (prongs.seen_values.contains(key)) {
                            return self.err_dispatcher.whenDuplicateCase(self.err_dispatcher.src[exp.loc.start .. exp.loc.end + 1], exp.loc);
                        }

                        if (subject.base_kind == .int) try self.checkWhenRangeOverlap(&prongs, .{ .start = value, .end = value }, exp.loc);
                        try prongs.seen_values.put(key, {});
                        values[i] = value;
                        ranges[i] = null;
                    }

                    prongs.values[prong_idx] = values;
                    prongs.ranges[prong_idx] = ranges;
                },
            }
        }

        return prongs;
    }

    fn isNumericWhenSubject(subject: WhenSubject) bool {
        return subject.base_kind == .int or subject.base_kind == .float;
    }

    fn evalWhenRangeBound(self: *Self, exp: *const ast.ExpNode, subject: WhenSubject) Errors!*ir.Value {
        if (subject.base_kind == .int) return self.evalWhenCaseValue(exp, subject.base_type);
        const literal: ?f64 = switch (exp.data) {
            .float_literal => |number| number,
            .int_literal => |number| @floatFromInt(number),
            .unary_exp => |unary| if (unary.op == .neg) switch (unary.right.data) {
                .float_literal => |number| -number,
                .int_literal => |number| -@as(f64, @floatFromInt(number)),
                else => null,
            } else null,
            else => null,
        };

        if (literal == null) return self.err_dispatcher.whenInvalidCase("float range bounds must be numeric literals", exp.loc);
        if (!std.math.isFinite(literal.?)) return self.err_dispatcher.whenInvalidCase("float range bounds must be finite", exp.loc);
        return ir.Value.init(self.allocator, .{ .data = .{ .i_float = literal.? }, .type_id = subject.base_type });
    }

    fn evalWhenRange(self: *Self, exp: *const ast.ExpNode, subject: WhenSubject) Errors!WhenRange {
        if (!isNumericWhenSubject(subject)) return self.err_dispatcher.whenInvalidCase("range cases require an int or float subject", exp.loc);
        const start = try self.evalWhenRangeBound(exp.data.range.start, subject);
        const end = try self.evalWhenRangeBound(exp.data.range.end, subject);
        const are_bounds_ordered = if (subject.base_kind == .int) start.data.i_int <= end.data.i_int else start.data.i_float <= end.data.i_float;
        if (!are_bounds_ordered) return self.err_dispatcher.whenInvalidCase("range start must not exceed its end", exp.loc);
        return .{ .start = start, .end = end };
    }

    fn checkWhenRangeOverlap(self: *Self, prongs: *WhenProngs, range: WhenRange, loc: err.Loc) Errors!void {
        for (prongs.seen_ranges.items) |previous| {
            const has_overlap = if (range.start.data == .i_int)
                range.start.data.i_int <= previous.end.data.i_int and previous.start.data.i_int <= range.end.data.i_int
            else
                range.start.data.i_float <= previous.end.data.i_float and previous.start.data.i_float <= range.end.data.i_float;

            if (has_overlap) return self.err_dispatcher.whenDuplicateCase(self.err_dispatcher.src[loc.start .. loc.end + 1], loc);
        }

        try prongs.seen_ranges.append(self.allocator, range);
    }

    fn createBinaryValue(self: *Self, kind: ir.BinaryOpKind, left: *ir.Value, right: *ir.Value, type_id: tt.TypeId) Errors!*ir.Value {
        return ir.Value.init(self.allocator, .{ .data = .{ .binary_op = .{ .kind = kind, .left = left, .right = right } }, .type_id = type_id });
    }

    fn lowerWhenRangeCondition(self: *Self, subject: *ir.Value, range: WhenRange) Errors!*ir.Value {
        const boolean_type = self.type_table.getPrimitive(.boolean);
        const is_float = range.start.data == .i_float;
        const is_at_least_start = try self.createBinaryValue(if (is_float) .f_cmp_ge else .i_cmp_ge, subject, range.start, boolean_type);
        const is_at_most_end = try self.createBinaryValue(if (is_float) .f_cmp_le else .i_cmp_le, subject, range.end, boolean_type);
        return self.createBinaryValue(.b_and, is_at_least_start, is_at_most_end, boolean_type);
    }

    fn checkWhenExhaustive(self: *Self, exp: *const ast.ExpNode, subject: WhenSubject, prongs: WhenProngs) Errors!void {
        const when_stmt = exp.data.when_exp;
        const when_loc: err.Loc = .{ .start = exp.loc.start, .end = when_stmt.subject.loc.end };

        if (self.type_table.isErrorSet(subject.base_type) and self.errorSetOf(subject.base_type).is_inferred) {
            try self.pending_when_checks.append(self.allocator, .{
                .when_stmt = when_stmt,
                .values = prongs.values,
                .set_type_id = subject.base_type,
                .is_null_missing = subject.is_nullable and !prongs.has_null_case,
                .when_loc = when_loc,
            });
            return;
        }

        var missing: std.ArrayList(u8) = .empty;
        switch (subject.base_kind) {
            .@"enum" => |enum_def| for (enum_def.variants_in_order) |name| {
                if (!prongs.seen_values.contains(enum_def.variants.get(name).?)) try appendMissing(self.allocator, &missing, name);
            },
            .@"union" => |union_def| for (union_def.variants_in_order) |name| {
                if (!prongs.seen_variants.contains(name)) try appendMissing(self.allocator, &missing, name);
            },
            else => {},
        }
        if (subject.is_nullable and !prongs.has_null_case) try appendMissing(self.allocator, &missing, "null");

        if (when_stmt.else_block == null) {
            if (isNumericWhenSubject(subject)) return self.err_dispatcher.whenNeedsElse(subject.type_name, when_loc);
            if (missing.items.len > 0) return self.err_dispatcher.whenNotExhaustive(missing.items, when_loc);
        } else if (!isNumericWhenSubject(subject) and missing.items.len == 0) {
            return self.err_dispatcher.whenUselessElse(when_stmt.else_loc.?);
        }
    }

    fn storeWhenSubject(self: *Self, instructions: *std.ArrayList(ir.Instruction), subject: WhenSubject) Errors!WhenSubjectRefs {
        const subject_uid = self.scope.genUid();
        try instructions.append(self.allocator, .{ .store_var = .{
            .uid = subject_uid,
            .identifier = "when",
            .type_id = subject.value.type_id,
            .value = subject.value,
        } });
        const subject_ref = try ir.Value.init(self.allocator, .{
            .data = .{ .identifier = .{ .uid = subject_uid, .name = "when" } },
            .type_id = subject.value.type_id,
        });

        if (!subject.is_nullable) return .{ .subject = subject_ref, .unwrapped = subject_ref };

        const unwrapped_uid = self.scope.genUid();
        const unwrapped_ref = try ir.Value.init(self.allocator, .{
            .data = .{ .identifier = .{ .uid = unwrapped_uid, .name = "when" } },
            .type_id = subject.base_type,
        });

        try instructions.append(self.allocator, if (subject.base_kind == .@"union")
            .{ .store_var = .{ .uid = unwrapped_uid, .identifier = "when", .type_id = subject.base_type, .value = subject_ref } }
        else
            .{ .unwrap_null_box = .{
                .unwrapped = .{ .uid = unwrapped_uid, .identifier = "when", .type_id = subject.base_type },
                .null_box = subject_ref,
            } });

        return .{ .subject = subject_ref, .unwrapped = unwrapped_ref };
    }

    fn findUnconditionalProng(prongs: []const ast.WhenProng) ?usize {
        var idx = prongs.len;
        while (idx > 0) {
            idx -= 1;
            if (prongs[idx].pattern != .null_case) return idx;
        }
        return null;
    }

    fn lowerWhenVariantProng(self: *Self, prong: *const ast.WhenProng, subject: WhenSubject, unwrapped_ref: *ir.Value, result: ?*BranchResult) Errors!LoweredProng {
        const variant = prong.pattern.variant;
        const union_def = subject.base_kind.@"union";
        const variant_type_id = union_def.variants.get(variant.name).?;

        const tag = try ir.Value.init(self.allocator, .{
            .data = .{ .union_tag = unwrapped_ref },
            .type_id = self.type_table.getPrimitive(.int),
        });
        const condition = try self.cmpEq(tag, try ir.Value.init(self.allocator, .{
            .data = .{ .i_int = @intCast(unionTagIndex(union_def, variant.name)) },
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
        }

        try self.scope.enter(self.scope.return_type_id);
        defer self.scope.exit(self.scope.return_type_id);
        if (!is_ignored) try self.scope.symbol_table.put(captured_symbol);

        try body.appendSlice(self.allocator, (try self.visitBranchBlock(&prong.body, result)).instructions);
        return .{ .condition = condition, .body = try body.toOwnedSlice(self.allocator) };
    }

    fn lowerWhenValuesProng(self: *Self, prong: *const ast.WhenProng, values: []const *ir.Value, ranges: []const ?WhenRange, unwrapped_ref: *ir.Value, result: ?*BranchResult) Errors!LoweredProng {
        var condition: ?*ir.Value = null;
        for (values, ranges) |value, range| {
            const matches_pattern = if (range) |bounds| try self.lowerWhenRangeCondition(unwrapped_ref, bounds) else try self.cmpEq(unwrapped_ref, value);
            condition = if (condition) |previous_condition| try ir.Value.init(self.allocator, .{
                .data = .{ .binary_op = .{ .kind = .b_or, .left = previous_condition, .right = matches_pattern } },
                .type_id = self.type_table.getPrimitive(.boolean),
            }) else matches_pattern;
        }

        return .{ .condition = condition.?, .body = (try self.visitBranchBlock(&prong.body, result)).instructions };
    }

    fn unionTagIndex(union_def: tt.Union, variant_name: []const u8) usize {
        for (union_def.variants_in_order, 0..) |name, i| {
            if (std.mem.eql(u8, name, variant_name)) return i;
        }
        unreachable;
    }

    fn evalWhenErrorCaseValue(self: *Self, exp: *const ast.ExpNode, expected_type: tt.TypeId) Errors!*ir.Value {
        const value = try self.evalExp(exp);
        if (!self.type_table.isErrorSet(value.type_id) or self.type_table.getTypePtrById(value.type_id).is_nullable) {
            return self.err_dispatcher.invalidType(
                try self.type_table.name(expected_type, self.allocator),
                try self.type_table.name(value.type_id, self.allocator),
                exp.loc,
            );
        }
        if (value.data != .i_int) {
            return self.err_dispatcher.whenInvalidCase("error cases must be error names, like 'ParseError.Empty'", exp.loc);
        }

        const set = self.errorSetOf(expected_type);
        if (!set.is_inferred and !set.has(value.data.i_int)) {
            return self.err_dispatcher.errorNotInSet(
                self.type_table.error_names.items[@intCast(value.data.i_int)],
                try self.type_table.name(expected_type, self.allocator),
                exp.loc,
            );
        }

        return ir.Value.init(self.allocator, .{ .data = value.data, .type_id = expected_type });
    }

    fn evalWhenCaseValue(self: *Self, exp: *const ast.ExpNode, expected_type: tt.TypeId) Errors!*ir.Value {
        if (self.type_table.isErrorSet(expected_type)) {
            return self.evalWhenErrorCaseValue(exp, expected_type);
        }

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
                try self.noteVariableUse(id_symbol);

                const assignment_value = try self.evalExpExpecting(assign_stmt.exp, id_symbol.type_id);
                const assignment_value_type = assignment_value.type_id;

                if (!id_symbol.kind.variable.is_mut) {
                    return self.err_dispatcher.notMutable(id, assign_stmt.target.loc);
                }

                if (!try self.canCoerce(assignment_value_type, id_symbol.type_id, assign_stmt.exp.loc)) {
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
                const indexed_target = try self.indexInto(assign_stmt.target, target);
                const expected_type_id = if (isAnonymousInit(assign_stmt.exp) or isConditional(assign_stmt.exp) or assign_stmt.exp.data == .array_literal)
                    indexed_target.type_id
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

                const is_immutable_target = target_symbol.kind == .variable and
                    (target_symbol.kind.variable.is_read_only or
                        (!target_symbol.kind.variable.is_mut and self.type_table.getTypePtrById(target_type).kind != .@"struct"));
                if (is_immutable_target) {
                    return self.err_dispatcher.notMutable(target_root.data.identifier, target_root.loc);
                }

                var field_maybe_nullable_type_id: tt.TypeId = undefined;

                switch (self.type_table.getTypePtrById(target_type).kind) {
                    .array => {
                        field_maybe_nullable_type_id = self.type_table.getTypePtrById(target_type).kind.array;
                        if (!try self.canCoerce(assignment_value_type, field_maybe_nullable_type_id, assign_stmt.exp.loc)) {
                            return self.err_dispatcher.invalidType(
                                try self.type_table.name(field_maybe_nullable_type_id, self.allocator),
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

                            if (!try self.canCoerce(assignment_value_type, field.type_id, assign_stmt.exp.loc)) {
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

                            if (!try self.canCoerce(assignment_value_type, field.type_id, assign_stmt.exp.loc)) {
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

                        if (!try self.canCoerce(assignment_value_type, field.type_id, assign_stmt.exp.loc)) {
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

                        if (!try self.canCoerce(assignment_value_type, field.type_id, assign_stmt.exp.loc)) {
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
                        .target = indexed_target,
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

    fn storeTemporaryValue(self: *Self, instructions: *std.ArrayList(ir.Instruction), name: []const u8, value: *ir.Value) Errors!*ir.Value {
        const uid = self.scope.genUid();
        try instructions.append(self.allocator, .{ .store_var = .{ .uid = uid, .identifier = name, .type_id = value.type_id, .value = value } });
        return ir.Value.init(self.allocator, .{ .data = .{ .identifier = .{ .uid = uid, .name = name } }, .type_id = value.type_id });
    }

    fn bindLoopCapture(self: *Self, instructions: *std.ArrayList(ir.Instruction), name: []const u8, value: *ir.Value, loc: err.Loc) Errors!void {
        if (std.mem.eql(u8, name, "_")) return;
        if (self.scope.symbol_table.symbols.contains(name)) return self.err_dispatcher.alreadyDefined(name, loc);
        const reference = try self.storeTemporaryValue(instructions, name, value);
        try self.scope.symbol_table.put(.{
            .uid = reference.data.identifier.uid,
            .identifier = name,
            .type_id = value.type_id,
            .kind = .{ .variable = .{ .is_mut = false, .is_read_only = true } },
        });
    }

    fn lowerCaptureLoopBody(self: *Self, capture: ast.ForCaptureStmt, item: *ir.Value, index: *ir.Value, loc: err.Loc) Errors![]const ir.Instruction {
        var body: std.ArrayList(ir.Instruction) = .empty;
        try self.scope.enter(self.scope.return_type_id);
        defer self.scope.exit(self.scope.return_type_id);
        try self.bindLoopCapture(&body, capture.item, item, loc);
        if (capture.index) |name| try self.bindLoopCapture(&body, name, index, loc);
        try body.appendSlice(self.allocator, (try self.visitBlock(&capture.do_block)).instructions);
        return body.toOwnedSlice(self.allocator);
    }

    fn evalIntegerRangeBound(self: *Self, exp: *const ast.ExpNode) Errors!*ir.Value {
        const value = try self.evalExp(exp);

        if (!self.type_table.eql(value.type_id, self.type_table.getPrimitive(.int))) {
            return self.err_dispatcher.invalidType("int", try self.type_table.name(value.type_id, self.allocator), exp.loc);
        }

        return value;
    }

    fn createLoopUpdate(reference: *ir.Value, value: *ir.Value) ir.Instruction {
        return .{ .store_var = .{ .uid = reference.data.identifier.uid, .identifier = reference.data.identifier.name, .type_id = reference.type_id, .value = value } };
    }

    fn appendLoopIncrement(self: *Self, instructions: *std.ArrayList(ir.Instruction), reference: *ir.Value) Errors!void {
        const one = try ir.Value.init(self.allocator, .{ .data = .{ .i_int = 1 }, .type_id = reference.type_id });
        const next = try self.createBinaryValue(.i_add, reference, one, reference.type_id);
        try instructions.append(self.allocator, createLoopUpdate(reference, next));
    }

    fn lowerRangeAdvancement(self: *Self, cursor: *ir.Value, end: *ir.Value, is_running: *ir.Value, index: ?*ir.Value) Errors![]const ir.Instruction {
        const is_last_item = try self.cmpEq(cursor, end);
        const stopped = try ir.Value.init(self.allocator, .{ .data = .{ .i_bool = false }, .type_id = is_running.type_id });
        const stop_instructions = try self.allocator.alloc(ir.Instruction, 1);
        stop_instructions[0] = createLoopUpdate(is_running, stopped);
        var advancement: std.ArrayList(ir.Instruction) = .empty;
        try self.appendLoopIncrement(&advancement, cursor);
        if (index) |reference| try self.appendLoopIncrement(&advancement, reference);
        return self.wrapBranch(is_last_item, stop_instructions, try advancement.toOwnedSlice(self.allocator));
    }

    fn visitForRangeStmt(self: *Self, stmt: *const ast.StatementNode) Errors![]ir.Instruction {
        const capture = stmt.data.for_capture;
        const range = capture.source.data.range;
        var instructions: std.ArrayList(ir.Instruction) = .empty;
        const start = try self.evalIntegerRangeBound(range.start);
        const cursor = try self.storeTemporaryValue(&instructions, "range cursor", start);
        const end = try self.evalIntegerRangeBound(range.end);
        const end_reference = try self.storeTemporaryValue(&instructions, "range end", end);
        const boolean_type = self.type_table.getPrimitive(.boolean);
        const has_items = try self.createBinaryValue(.i_cmp_le, cursor, end_reference, boolean_type);
        const is_running = try self.storeTemporaryValue(&instructions, "range is running", has_items);
        const zero = try ir.Value.init(self.allocator, .{ .data = .{ .i_int = 0 }, .type_id = self.type_table.getPrimitive(.int) });
        const needs_index = if (capture.index) |name| !std.mem.eql(u8, name, "_") else false;
        const index = if (needs_index) try self.storeTemporaryValue(&instructions, "range index", zero) else zero;
        const body = try self.lowerCaptureLoopBody(capture, cursor, index, stmt.loc);
        const advancement = try self.lowerRangeAdvancement(cursor, end_reference, is_running, if (needs_index) index else null);
        try instructions.append(self.allocator, .{ .loop = .{ .condition = is_running, .do_block = body, .advancement = advancement } });
        return instructions.toOwnedSlice(self.allocator);
    }

    fn visitForCaptureStmt(self: *Self, stmt: *const ast.StatementNode) Errors![]ir.Instruction {
        const capture = stmt.data.for_capture;
        if (capture.source.data == .range) return self.visitForRangeStmt(stmt);
        const source = try self.evalExp(capture.source);
        const source_type = self.type_table.getTypePtrById(source.type_id);
        const is_array_source = !source_type.is_nullable and source_type.kind == .array;
        if (!is_array_source) return self.err_dispatcher.invalidType("array", try self.type_table.name(source.type_id, self.allocator), capture.source.loc);
        const item_type = source_type.kind.array;
        const integer_type = self.type_table.getPrimitive(.int);
        var instructions: std.ArrayList(ir.Instruction) = .empty;
        const source_reference = try self.storeTemporaryValue(&instructions, "loop source", source);
        const length = try ir.Value.init(self.allocator, .{ .data = .{ .array_length = source_reference }, .type_id = integer_type });
        const length_reference = try self.storeTemporaryValue(&instructions, "loop length", length);
        const zero = try ir.Value.init(self.allocator, .{ .data = .{ .i_int = 0 }, .type_id = integer_type });
        const cursor = try self.storeTemporaryValue(&instructions, "loop cursor", zero);
        const condition = try ir.Value.init(self.allocator, .{
            .data = .{ .binary_op = .{ .kind = .i_cmp_lt, .left = cursor, .right = length_reference } },
            .type_id = self.type_table.getPrimitive(.boolean),
        });
        const item = try ir.Value.init(self.allocator, .{
            .data = .{ .indexed = .{ .target = source_reference, .index = cursor } },
            .type_id = item_type,
        });
        const body = try self.lowerCaptureLoopBody(capture, item, cursor, stmt.loc);
        const one = try ir.Value.init(self.allocator, .{ .data = .{ .i_int = 1 }, .type_id = integer_type });
        const next_cursor = try ir.Value.init(self.allocator, .{
            .data = .{ .binary_op = .{ .kind = .i_add, .left = cursor, .right = one } },
            .type_id = integer_type,
        });
        const advancement = try self.allocator.alloc(ir.Instruction, 1);
        advancement[0] = .{ .store_var = .{ .uid = cursor.data.identifier.uid, .identifier = "loop cursor", .type_id = integer_type, .value = next_cursor } };
        try instructions.append(self.allocator, .{ .loop = .{
            .condition = condition,
            .do_block = body,
            .advancement = advancement,
        } });
        return instructions.toOwnedSlice(self.allocator);
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
            const annotated_type = if (field.type) |annotation| try self.resolveTypeAnnotation(annotation, blueprint_type_id) else null;
            const default_value: ?*ir.Value = if (field.default_value) |value| try self.evalDefaultValue(value, annotated_type) else null;
            const field_type = annotated_type orelse default_value.?.type_id;

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
            const annotated_type = if (field.type) |annotation| try self.resolveTypeAnnotation(annotation, blueprint_type_id) else null;
            const default_value: ?*ir.Value = if (field.default_value) |value| try self.evalDefaultValue(value, annotated_type) else null;
            const field_type = annotated_type orelse default_value.?.type_id;

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
        self.fn_depth += 1;
        defer self.fn_depth -= 1;
        try self.fn_frames.append(self.allocator, .{ .fn_uid = uid, .first_local_uid = self.scope.next_uid });
        defer _ = self.fn_frames.pop();

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
        const is_failing_void = self.type_table.isErrorUnion(self.scope.return_type_id) and
            self.type_table.getTypePtrById(self.scope.return_type_id).kind.error_union.payload == self.type_table.getPrimitive(.void);

        if (!is_failing_void and !self.type_table.canCoerce(body.return_type, self.scope.return_type_id)) {
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

        if (self.type_table.isErrorUnion(self.scope.return_type_id)) {
            return self.visitFailingReturnStmt(stmt);
        }

        if (return_stmt.exp) |exp| {
            const exp_value = try self.evalExpExpecting(exp, self.scope.return_type_id);
            const exp_value_type = exp_value.type_id;

            if (!try self.canCoerce(exp_value_type, self.scope.return_type_id, exp.loc)) {
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

    fn visitFailingReturnStmt(self: *Self, stmt: *const ast.StatementNode) Errors!ir.Instruction {
        const return_stmt = stmt.data.@"return";
        const error_union = self.type_table.getTypePtrById(self.scope.return_type_id).kind.error_union;
        const payload = error_union.payload;

        const exp = return_stmt.exp orelse {
            if (payload != self.type_table.getPrimitive(.void)) {
                return self.err_dispatcher.invalidFunctionReturnType(
                    try self.type_table.name(payload, self.allocator),
                    "void",
                    stmt.loc,
                );
            }
            return ir.Instruction{ .return_stmt = .{ .value = null } };
        };

        const value = try self.evalExpExpecting(exp, payload);
        const is_error = self.type_table.isErrorSet(value.type_id) and
            !self.type_table.getTypePtrById(value.type_id).is_nullable and
            !self.type_table.canCoerce(value.type_id, payload);

        if (is_error) {
            const target_set = self.errorSetOf(error_union.error_set);

            if (value.data == .i_int) {
                if (target_set.is_inferred) {
                    try target_set.ids.put(self.allocator, value.data.i_int, {});
                } else if (!target_set.has(value.data.i_int)) {
                    return self.err_dispatcher.errorNotInSet(
                        self.type_table.error_names.items[@intCast(value.data.i_int)],
                        try self.type_table.name(error_union.error_set, self.allocator),
                        exp.loc,
                    );
                }
            } else {
                try self.addErrorEdge(value.type_id, error_union.error_set, exp.loc);
            }

            return ir.Instruction{ .return_error = .{ .value = value } };
        }

        if (!try self.canCoerce(value.type_id, payload, exp.loc)) {
            return self.err_dispatcher.invalidFunctionReturnType(
                try self.type_table.name(self.scope.return_type_id, self.allocator),
                try self.type_table.name(value.type_id, self.allocator),
                exp.loc,
            );
        }

        const boxed_value = if (self.needsNullBox(payload, value.type_id))
            try ir.Value.init(self.allocator, .{
                .data = .{ .null_box_init = .{ .value = value } },
                .type_id = value.type_id,
            })
        else
            value;

        return ir.Instruction{ .return_stmt = .{ .value = boxed_value } };
    }

    fn isConditional(exp: *const ast.ExpNode) bool {
        return switch (exp.data) {
            .if_exp, .if_capture_exp, .when_exp, .orelse_exp => true,
            else => false,
        };
    }

    fn evalConditional(self: *Self, exp: *const ast.ExpNode, expected_type_id: ?tt.TypeId) Errors!*ir.Value {
        return self.evalConditionalWithContext(exp, expected_type_id, null);
    }

    fn evalConditionalWithContext(self: *Self, exp: *const ast.ExpNode, expected_type_id: ?tt.TypeId, context_type_id: ?tt.TypeId) Errors!*ir.Value {
        if (exp.data == .orelse_exp) return self.evalOrelse(exp, expected_type_id, context_type_id);

        const expected = if (expected_type_id == self.type_table.getPrimitive(.dynamic)) null else expected_type_id;
        var result = BranchResult{ .uid = self.scope.genUid(), .expected_type_id = expected, .context_type_id = context_type_id, .loc = exp.loc };
        const instructions = switch (exp.data) {
            .if_exp => conditional: {
                const instructions = try self.allocator.alloc(ir.Instruction, 1);
                instructions[0] = try self.lowerIf(exp, &result);
                break :conditional instructions;
            },
            .if_capture_exp => try self.lowerIfCapture(exp, &result),
            .when_exp => try self.lowerWhen(exp, &result),
            else => unreachable,
        };
        return self.finishBranchResult(&result, instructions);
    }

    fn finishBranchResult(self: *Self, result: *BranchResult, instructions: []const ir.Instruction) Errors!*ir.Value {
        var result_type = result.expected_type_id orelse self.type_table.getPrimitive(.void);

        if (result.values.items.len > 0) {
            if (result.expected_type_id == null) {
                result_type = result.values.items[0].value.type_id;
                for (result.values.items[1..]) |branch_value| {
                    result_type = try self.mergeBranchTypes(result_type, branch_value.value.type_id, branch_value.loc);
                }
            }

            for (result.values.items) |branch_value| {
                const original = try ir.Value.init(self.allocator, branch_value.value.*);
                branch_value.value.* = (try self.convertBranchValue(original, result_type, branch_value.loc)).*;
            }
        }

        return ir.Value.init(self.allocator, .{
            .data = .{ .block_value = .{
                .instructions = instructions,
                .result_uid = result.uid,
                .can_complete = !isBlockLeaving(instructions),
            } },
            .type_id = result_type,
        });
    }

    fn orelseFallbackContext(self: *Self, payload_type: tt.TypeId, inherited_context: ?tt.TypeId) ?tt.TypeId {
        if (inherited_context) |context| return context;
        return switch (self.type_table.getTypePtrById(payload_type).kind) {
            .@"struct", .@"union", .array => payload_type,
            else => null,
        };
    }

    fn evalOrelse(self: *Self, exp: *const ast.ExpNode, expected_type: ?tt.TypeId, context_type: ?tt.TypeId) Errors!*ir.Value {
        const coalescing = exp.data.orelse_exp;
        const expected = if (expected_type == self.type_table.getPrimitive(.dynamic)) null else expected_type;
        const operand = try self.evalExpWithContext(coalescing.operand, null, expected orelse context_type);
        const null_type = self.type_table.getPrimitive(.null);
        const is_null_operand = operand.type_id == null_type;
        const is_nullable = self.type_table.getTypePtrById(operand.type_id).is_nullable;

        if (!is_nullable and !is_null_operand) {
            return self.err_dispatcher.invalidType("nullable or null", try self.type_table.name(operand.type_id, self.allocator), coalescing.operand.loc);
        }

        const payload_type = self.type_table.typeIdByNullableTypeId.get(operand.type_id) orelse null_type;
        const fallback_context = self.orelseFallbackContext(payload_type, context_type);
        const fallback = try self.evalExpWithContext(coalescing.fallback, expected, fallback_context);
        var result = BranchResult{ .uid = self.scope.genUid(), .expected_type_id = expected, .loc = exp.loc };
        const fallback_instruction = try self.storeBranchValue(fallback, &result, coalescing.fallback.loc);

        if (is_null_operand) {
            const instructions = try self.allocator.alloc(ir.Instruction, 2);
            instructions[0] = .{ .expression_stmt = .{ .value = operand } };
            instructions[1] = fallback_instruction;
            return self.finishBranchResult(&result, instructions);
        }

        return self.lowerOrelseBranch(operand, payload_type, fallback_instruction, &result, coalescing.operand.loc);
    }

    fn unwrapNullableValue(self: *Self, instructions: *std.ArrayList(ir.Instruction), operand: *ir.Value, payload_type: tt.TypeId, name: []const u8) Errors!*ir.Value {
        if (!self.needsNullBox(operand.type_id, payload_type)) {
            return ir.Value.init(self.allocator, .{ .data = operand.data, .type_id = payload_type });
        }

        const selected_uid = self.scope.genUid();
        try instructions.append(self.allocator, .{ .unwrap_null_box = .{
            .unwrapped = .{ .uid = selected_uid, .identifier = name, .type_id = payload_type },
            .null_box = operand,
        } });
        return ir.Value.init(self.allocator, .{
            .data = .{ .identifier = .{ .uid = selected_uid, .name = name } },
            .type_id = payload_type,
        });
    }

    fn lowerOrelseBranch(self: *Self, operand: *ir.Value, payload_type: tt.TypeId, fallback: ir.Instruction, result: *BranchResult, loc: err.Loc) Errors!*ir.Value {
        var instructions: std.ArrayList(ir.Instruction) = .empty;
        const operand_reference = try self.storeTemporaryValue(&instructions, "orelse operand", operand);
        var selected_instructions: std.ArrayList(ir.Instruction) = .empty;
        const selected = try self.unwrapNullableValue(&selected_instructions, operand_reference, payload_type, "orelse selected");
        const fallback_instructions = try self.allocator.alloc(ir.Instruction, 1);
        fallback_instructions[0] = fallback;
        try selected_instructions.append(self.allocator, try self.storeBranchValue(selected, result, loc));
        const condition = try ir.Value.init(self.allocator, .{
            .data = .{ .unary_op = .{ .kind = .box_not_null, .right = operand_reference } },
            .type_id = self.type_table.getPrimitive(.boolean),
        });
        try instructions.append(self.allocator, .{ .branch_if = .{
            .condition = condition,
            .then_block = try selected_instructions.toOwnedSlice(self.allocator),
            .else_block = fallback_instructions,
        } });
        return self.finishBranchResult(result, try instructions.toOwnedSlice(self.allocator));
    }

    fn mergeBranchTypes(self: *Self, first: tt.TypeId, second: tt.TypeId, loc: err.Loc) Errors!tt.TypeId {
        if (first == second) return first;
        const null_type = self.type_table.getPrimitive(.null);
        if (first == null_type) return self.type_table.getOrAddNullable(second);
        if (second == null_type) return self.type_table.getOrAddNullable(first);
        const first_base = self.type_table.typeIdByNullableTypeId.get(first) orelse first;
        const second_base = self.type_table.typeIdByNullableTypeId.get(second) orelse second;
        const int_type = self.type_table.getPrimitive(.int);
        const float_type = self.type_table.getPrimitive(.float);
        const are_numeric = (first_base == int_type or first_base == float_type) and (second_base == int_type or second_base == float_type);
        const is_nullable = first != first_base or second != second_base;
        if (are_numeric) {
            const numeric_type = if (first_base == float_type or second_base == float_type) float_type else int_type;
            return if (is_nullable) self.type_table.getOrAddNullable(numeric_type) else numeric_type;
        }
        if (first_base == second_base) return self.type_table.getOrAddNullable(first_base);
        const first_kind = self.type_table.getTypePtrById(first_base).kind;
        const second_kind = self.type_table.getTypePtrById(second_base).kind;
        if (first_kind == .array and second_kind == .array) {
            const dynamic_type = self.type_table.getPrimitive(.dynamic);
            if (first_kind.array == dynamic_type or second_kind.array == dynamic_type) {
                const array_type = if (first_kind.array == dynamic_type) second_base else first_base;
                return if (is_nullable) self.type_table.getOrAddNullable(array_type) else array_type;
            }
            return self.err_dispatcher.invalidType(try self.type_table.name(first, self.allocator), try self.type_table.name(second, self.allocator), loc);
        }
        if (try self.canCoerce(first, second, loc)) return second;
        if (try self.canCoerce(second, first, loc)) return first;
        return self.err_dispatcher.invalidType(try self.type_table.name(first, self.allocator), try self.type_table.name(second, self.allocator), loc);
    }

    fn convertBranchValue(self: *Self, value: *ir.Value, result_type: tt.TypeId, loc: err.Loc) Errors!*ir.Value {
        const value_base = self.type_table.typeIdByNullableTypeId.get(value.type_id) orelse value.type_id;
        const result_base = self.type_table.typeIdByNullableTypeId.get(result_type) orelse result_type;
        const needs_numeric_conversion = value_base == self.type_table.getPrimitive(.int) and result_base == self.type_table.getPrimitive(.float);
        const value_kind = self.type_table.getTypePtrById(value_base).kind;
        const result_kind = self.type_table.getTypePtrById(result_base).kind;
        if (value.data == .i_array and value_kind == .array and result_kind == .array and value_kind.array == self.type_table.getPrimitive(.dynamic)) {
            value.type_id = result_base;
        }
        var converted = value;
        if (needs_numeric_conversion) {
            if (value.type_id != value_base and result_type == result_base) {
                return self.err_dispatcher.invalidType(try self.type_table.name(result_type, self.allocator), try self.type_table.name(value.type_id, self.allocator), loc);
            }
            converted = try ir.Value.init(self.allocator, .{
                .data = .{ .int_to_float = value },
                .type_id = if (value.type_id != value_base) result_type else result_base,
            });
        }
        if (!try self.canCoerce(converted.type_id, result_type, loc)) {
            return self.err_dispatcher.invalidType(try self.type_table.name(result_type, self.allocator), try self.type_table.name(converted.type_id, self.allocator), loc);
        }
        if (self.needsNullBox(result_type, converted.type_id)) {
            return ir.Value.init(self.allocator, .{ .data = .{ .null_box_init = .{ .value = converted } }, .type_id = result_type });
        }
        if (converted.type_id == self.type_table.getPrimitive(.null)) {
            return ir.Value.init(self.allocator, .{ .data = .i_null, .type_id = result_type });
        }
        return converted;
    }

    fn isAnonymousInit(exp: *const ast.ExpNode) bool {
        return exp.data == .anonymous_init;
    }

    fn evalExpWithContext(self: *Self, exp: *const ast.ExpNode, expected_type: ?tt.TypeId, context_type: ?tt.TypeId) Errors!*ir.Value {
        if (expected_type != null) return self.evalExpExpecting(exp, expected_type);
        if (isConditional(exp)) return self.evalConditionalWithContext(exp, null, context_type);
        const needs_context = isAnonymousInit(exp) or exp.data == .array_literal;
        return self.evalExpExpecting(exp, if (needs_context) context_type else null);
    }

    fn evalExpExpecting(self: *Self, exp: *const ast.ExpNode, expected_type_id: ?tt.TypeId) Errors!*ir.Value {
        if (isConditional(exp)) return self.evalConditional(exp, expected_type_id);
        if (expected_type_id) |expected| {
            const base_type_id = self.type_table.typeIdByNullableTypeId.get(expected) orelse expected;
            const base_kind = self.type_table.getTypePtrById(base_type_id).kind;

            if (isAnonymousInit(exp) and base_kind == .@"union") {
                return self.evalUnionInit(exp, exp.data.anonymous_init.arguments, base_type_id);
            }
            if (isAnonymousInit(exp) and base_kind == .@"struct") {
                return self.evalExpectedStructInit(exp, base_type_id);
            }

            if (exp.data == .array_literal and base_kind == .array) {
                return self.evalArrayLiteral(exp, base_kind.array);
            }
        }

        const value = try self.evalExp(exp);
        if (expected_type_id) |expected| {
            if (isValueLeaving(value)) {
                return ir.Value.init(self.allocator, .{ .data = value.data, .type_id = expected });
            }
        }
        return value;
    }

    fn evalExp(self: *Self, exp: *const ast.ExpNode) Errors!*ir.Value {
        switch (exp.*.data) {
            .if_exp, .if_capture_exp, .when_exp, .orelse_exp => return self.evalConditional(exp, null),
            .range => return self.err_dispatcher.invalidExpression("range in a capture loop or when case", "range value", exp.loc),
            .float_literal => {
                return ir.Value.init(self.allocator, .{ .data = .{ .i_float = exp.data.float_literal }, .type_id = self.type_table.getPrimitive(.float) });
            },
            .int_literal => {
                return ir.Value.init(self.allocator, .{ .data = .{ .i_int = exp.data.int_literal }, .type_id = self.type_table.getPrimitive(.int) });
            },
            .string_literal => {
                return ir.Value.init(self.allocator, .{ .data = .{ .i_string = exp.data.string_literal }, .type_id = self.type_table.getPrimitive(.string) });
            },
            .string_interpolation => |parts| {
                var values: std.ArrayList(*ir.Value) = .empty;
                for (parts) |part| {
                    const value = try self.evalExp(part);
                    if (self.type_table.getTypePtrById(value.type_id).kind == .void and !isValueLeaving(value)) {
                        return self.err_dispatcher.invalidExpression("value in string interpolation", "void", part.loc);
                    }
                    try values.append(self.allocator, value);
                }
                return ir.Value.init(self.allocator, .{
                    .data = .{ .string_interpolation = try values.toOwnedSlice(self.allocator) },
                    .type_id = self.type_table.getPrimitive(.string),
                });
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
                const call = try self.evalFnCall(exp);
                if (self.type_table.isErrorUnion(call.type_id)) {
                    return self.err_dispatcher.unhandledError(callName(exp), exp.loc);
                }
                return call;
            },
            .try_exp => {
                return self.evalTry(exp);
            },
            .catch_exp => {
                return self.evalCatch(exp, false);
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

                if (!try self.canCoerce(arg_type, param.type_id, _exp.loc)) {
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

        if (!is_struct_init) try self.noteCall(uid, fn_identifier, exp.loc);

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

    fn evalExpectedStructInit(self: *Self, exp: *const ast.ExpNode, type_id: tt.TypeId) Errors!*ir.Value {
        const structure = self.type_table.getTypePtrById(type_id).kind.@"struct";
        const initialization = exp.data.anonymous_init;
        if (initialization.arguments.len > structure.fields_in_order.len) {
            return self.err_dispatcher.invalidNumberOfArgs(structure.fields_in_order.len, initialization.arguments.len, exp.loc);
        }
        var arguments_by_name = std.StringHashMap(*ast.ExpNode).init(self.allocator);
        defer arguments_by_name.deinit();
        for (initialization.arguments) |argument| {
            if (argument.identifier) |name| {
                if (!structure.fields.contains(name)) return self.err_dispatcher.invalidStructField(structure.identifier, name, argument.exp.loc);
                if (arguments_by_name.contains(name)) return self.err_dispatcher.alreadyDefined(name, argument.exp.loc);
                try arguments_by_name.put(name, argument.exp);
            }
        }
        const keys = try self.allocator.alloc([]const u8, structure.fields_in_order.len);
        const values = try self.allocator.alloc(*ir.Value, structure.fields_in_order.len);
        for (structure.fields_in_order, 0..) |field, index| {
            keys[index] = field.identifier;
            const argument = if (initialization.are_arguments_named)
                arguments_by_name.get(field.identifier)
            else if (index < initialization.arguments.len)
                initialization.arguments[index].exp
            else
                null;
            const value = if (argument) |operand|
                try self.evalExpExpecting(operand, field.type_id)
            else if (field.default_value) |default_value|
                default_value
            else if (self.type_table.getTypePtrById(field.type_id).is_nullable)
                try ir.Value.init(self.allocator, .{ .data = .i_null, .type_id = field.type_id })
            else
                return self.err_dispatcher.missingArgument(field.identifier, exp.loc);
            const loc = if (argument) |operand| operand.loc else exp.loc;
            if (!try self.canCoerce(value.type_id, field.type_id, loc)) {
                return self.err_dispatcher.invalidType(try self.type_table.name(field.type_id, self.allocator), try self.type_table.name(value.type_id, self.allocator), loc);
            }
            values[index] = if (self.needsNullBox(field.type_id, value.type_id))
                try ir.Value.init(self.allocator, .{ .data = .{ .null_box_init = .{ .value = value } }, .type_id = field.type_id })
            else
                value;
        }
        return ir.Value.init(self.allocator, .{ .data = .{ .struct_init = .{ .keys = keys, .values = values } }, .type_id = type_id });
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
        if (!try self.canCoerce(raw_value.type_id, variant_type_id, arg.exp.loc)) {
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
        return self.indexInto(exp, try self.evalExp(exp.data.indexed.target));
    }

    fn indexInto(self: *Self, exp: *const ast.ExpNode, target: *ir.Value) !*ir.Value {
        const indexed_exp = exp.data.indexed;
        const is_nullable = indexed_exp.is_nullable;
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
            try self.noteVariableUse(symbol);
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
        var array_literal_type: tt.TypeId = expected_item_type_id orelse self.type_table.getPrimitive(.dynamic);

        for (array_literal.exps) |e| {
            // without a declared item type, later items follow the first one
            const item_type_id = expected_item_type_id orelse
                if (array_literal_type == self.type_table.getPrimitive(.dynamic)) null else array_literal_type;
            const exp_value = try self.evalExpExpecting(e, item_type_id);
            const exp_value_type = exp_value.type_id;

            if (array_literal_type == self.type_table.getPrimitive(.dynamic)) {
                array_literal_type = exp_value_type;
            }

            if (!try self.canCoerce(exp_value_type, array_literal_type, exp.loc)) {
                return self.err_dispatcher.invalidType(try self.type_table.name(array_literal_type, self.allocator), try self.type_table.name(exp_value_type, self.allocator), exp.loc);
            }

            const item_value = if (self.needsNullBox(array_literal_type, exp_value_type))
                try ir.Value.init(self.allocator, .{
                    .data = .{ .null_box_init = .{ .value = exp_value } },
                    .type_id = exp_value_type,
                })
            else
                exp_value;

            try values.append(self.allocator, item_value);
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
        if (isValueLeaving(exp_value)) return exp_value;
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
        if (isValueLeaving(left_value)) return left_value;
        const right_value = try self.evalExp(bin_exp.right);
        if (isValueLeaving(right_value)) {
            const instructions = try self.allocator.alloc(ir.Instruction, 2);
            instructions[0] = .{ .expression_stmt = .{ .value = left_value } };
            instructions[1] = .{ .expression_stmt = .{ .value = right_value } };
            return ir.Value.init(self.allocator, .{
                .data = .{ .block_value = .{ .instructions = instructions, .result_uid = self.scope.genUid(), .can_complete = false } },
                .type_id = right_value.type_id,
            });
        }
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

                if (self.isBoxedNullable(left_type) or self.isBoxedNullable(right_type)) {
                    return self.lowerNullableEquality(bin_exp.op, left_value, right_value, exp.loc);
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

    fn isBoxedNullable(self: *Self, type_id: tt.TypeId) bool {
        const base = self.type_table.typeIdByNullableTypeId.get(type_id) orelse return false;
        return self.needsNullBox(type_id, base);
    }

    fn hasValue(self: *Self, reference: *ir.Value) Errors!*ir.Value {
        const bool_type = self.type_table.getPrimitive(.boolean);
        if (!self.type_table.getTypePtrById(reference.type_id).is_nullable) {
            return ir.Value.init(self.allocator, .{ .data = .{ .i_bool = true }, .type_id = bool_type });
        }
        return ir.Value.init(self.allocator, .{
            .data = .{ .unary_op = .{ .kind = .box_not_null, .right = reference } },
            .type_id = bool_type,
        });
    }

    fn lowerNullableEquality(self: *Self, op: ast.BinaryOp, left: *ir.Value, right: *ir.Value, loc: err.Loc) Errors!*ir.Value {
        const bool_type = self.type_table.getPrimitive(.boolean);
        const left_base = self.type_table.typeIdByNullableTypeId.get(left.type_id) orelse left.type_id;
        const right_base = self.type_table.typeIdByNullableTypeId.get(right.type_id) orelse right.type_id;
        var result = BranchResult{ .uid = self.scope.genUid(), .expected_type_id = bool_type, .loc = loc };

        var instructions: std.ArrayList(ir.Instruction) = .empty;
        const left_reference = try self.storeTemporaryValue(&instructions, "compared left", left);
        const right_reference = try self.storeTemporaryValue(&instructions, "compared right", right);

        var compare_instructions: std.ArrayList(ir.Instruction) = .empty;
        const left_payload = try self.unwrapNullableValue(&compare_instructions, left_reference, left_base, "compared left value");
        const right_payload = try self.unwrapNullableValue(&compare_instructions, right_reference, right_base, "compared right value");
        const compared = try ir.Value.init(self.allocator, .{
            .data = .{ .binary_op = .{ .kind = self.astBinOpToIrBinOpKind(op, left_base, right_base), .left = left_payload, .right = right_payload } },
            .type_id = bool_type,
        });
        try compare_instructions.append(self.allocator, try self.storeBranchValue(compared, &result, loc));

        const null_compared = try ir.Value.init(self.allocator, .{
            .data = .{ .binary_op = .{ .kind = if (op == .eq) .b_cmp_eq else .b_cmp_neq, .left = try self.hasValue(left_reference), .right = try self.hasValue(right_reference) } },
            .type_id = bool_type,
        });
        const null_instructions = try self.allocator.alloc(ir.Instruction, 1);
        null_instructions[0] = try self.storeBranchValue(null_compared, &result, loc);

        const both_have_values = try ir.Value.init(self.allocator, .{
            .data = .{ .binary_op = .{ .kind = .b_and, .left = try self.hasValue(left_reference), .right = try self.hasValue(right_reference) } },
            .type_id = bool_type,
        });
        try instructions.append(self.allocator, .{ .branch_if = .{
            .condition = both_have_values,
            .then_block = try compare_instructions.toOwnedSlice(self.allocator),
            .else_block = null_instructions,
        } });
        return self.finishBranchResult(&result, try instructions.toOwnedSlice(self.allocator));
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
                const array_type_id = try self.type_table.getOrAddArray(inner_type_id);
                return if (type_annotation.is_nullable) try self.type_table.getOrAddNullable(array_type_id) else array_type_id;
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

const ErrorEdge = struct {
    from: tt.TypeId,
    to: tt.TypeId,
    loc: err.Loc,
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

const FnFrame = struct {
    fn_uid: usize,
    first_local_uid: usize,
};

const TopLevelCall = struct {
    callee_uid: usize,
    callee_name: []const u8,
    first_undeclared_uid: usize,
    is_default_value: bool,
    loc: err.Loc,
};

pub const Symbol = struct {
    const Self = @This();

    uid: usize,
    identifier: []const u8,
    type_id: tt.TypeId,

    kind: union(enum) {
        variable: struct {
            is_mut: bool,
            is_read_only: bool = false,
        },
        defined_type: struct {
            blueprint_type_id: tt.TypeId,
        },
        function: void,
    },
};

const WhenSubject = struct {
    value: *ir.Value,
    type_name: []const u8,
    is_nullable: bool,
    base_type: tt.TypeId,
    base_kind: @FieldType(tt.Type, "kind"),
};

const PendingWhenCheck = struct {
    when_stmt: ast.WhenStmt,
    values: [][]*ir.Value,
    set_type_id: tt.TypeId,
    is_null_missing: bool,
    when_loc: err.Loc,
};

const WhenRange = struct {
    start: *ir.Value,
    end: *ir.Value,
};

const WhenProngs = struct {
    values: [][]*ir.Value,
    ranges: [][]?WhenRange,
    seen_ranges: std.ArrayList(WhenRange) = .empty,
    seen_values: std.AutoHashMap(i64, void),
    seen_variants: std.StringHashMap(void),
    has_null_case: bool,
};

const WhenSubjectRefs = struct {
    subject: *ir.Value,
    unwrapped: *ir.Value,
};

const LoweredProng = struct {
    condition: *ir.Value,
    body: []const ir.Instruction,
};

const BranchResult = struct {
    uid: usize,
    expected_type_id: ?tt.TypeId,
    context_type_id: ?tt.TypeId = null,
    loc: err.Loc,
    values: std.ArrayList(BranchValue) = .empty,
};

const BranchValue = struct {
    value: *ir.Value,
    loc: err.Loc,
};
