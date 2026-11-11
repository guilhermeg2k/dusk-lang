const err = @import("compile_error.zig");
const ir = @import("ir.zig");
const ast = @import("ast.zig");
const tt = @import("type_table.zig");
const modules = @import("modules.zig");
const native = @import("../runtime/builtins/native.zig");
const prelude_bindings = &@import("../runtime/builtins/prelude.zig").bindings;
const allocPrint = std.fmt.allocPrint;
const std = @import("std");

pub const CompileError = err.CompileError;

pub const SemaAnalyzer = struct {
    const Self = @This();
    const max_copy_depth = 64;
    const lambda_name = "$lambda";
    const none_error_set_name = "none";
    const task_type_name = "Task";
    const max_captures = 255;

    allocator: std.mem.Allocator,
    src: []const u8 = "",

    externs: std.ArrayList(ir.Extern) = .empty,
    functions: std.ArrayList(ir.Func),
    defined_types: std.ArrayList(ir.DefinedType),

    type_table: tt.TypeTable,
    scope: Scope,
    err_reporter: err.CompileErrorReporter,

    fn_depth: usize = 0,
    is_in_prelude: bool = false,
    module: *ModuleScope = undefined,
    prelude_module: *ModuleScope = undefined,
    slot_modules: std.AutoHashMapUnmanaged(usize, *ModuleScope) = .empty,
    type_modules: std.AutoHashMapUnmanaged(tt.TypeId, *ModuleScope) = .empty,
    file_only_methods: std.AutoHashMapUnmanaged(tt.TypeId, void) = .empty,
    intrinsic_by_uid: std.AutoHashMapUnmanaged(usize, native.Intrinsic) = .empty,
    extern_uids: std.AutoHashMapUnmanaged(usize, void) = .empty,
    error_edges: std.ArrayList(ErrorEdge) = .empty,
    pending_when_checks: std.ArrayList(PendingWhenCheck) = .empty,
    constants: std.ArrayList(Constant) = .empty,
    constant_by_exp: std.AutoHashMapUnmanaged(*const ast.ExpNode, usize) = .empty,
    exp_types: std.AutoHashMapUnmanaged(*const ast.ExpNode, tt.TypeId) = .empty,
    generic_functions: std.ArrayList(GenericFunction) = .empty,
    generic_types: std.ArrayList(GenericType) = .empty,
    traits: std.ArrayList(Trait) = .empty,
    trait_self_type: ?tt.TypeId = null,
    is_hoisting: bool = false,
    pending_trait_checks: std.ArrayList(PendingTraitCheck) = .empty,
    copied_types: std.AutoHashMapUnmanaged(tt.TypeId, TypeCopy) = .empty,
    pending_copies: std.ArrayList(PendingCopy) = .empty,
    copy_depth: usize = 0,
    is_lowering_copy: bool = false,
    has_none_error_slot: bool = false,
    is_checking_opaque: bool = false,
    can_bind_void_slots: bool = false,
    none_error_set: tt.TypeId = undefined,
    lambda_contexts: std.ArrayList(LambdaContext) = .empty,
    boxed_uids: std.AutoHashMapUnmanaged(usize, void) = .empty,
    lambda_binding: ?[]const u8 = null,

    pub fn init(allocator: std.mem.Allocator) !Self {
        var type_table = try tt.TypeTable.init(allocator);

        const void_type_id = type_table.getPrimitive(.void);

        var self = Self{
            .allocator = allocator,
            .type_table = type_table,
            .scope = undefined,
            .err_reporter = .{ .allocator = allocator, .src = "" },
            .functions = .empty,
            .defined_types = .empty,
        };

        self.scope = try Scope.init(allocator, void_type_id);

        return self;
    }

    pub fn analyze(self: *Self, prelude: *const ast.Root, prelude_src: []const u8, loaded_modules: []const modules.Module) !ir.Program {
        self.prelude_module = try ModuleScope.create(self.allocator, "prelude", prelude_src, self.scope.symbol_table, true);
        self.prelude_module.bindings = prelude_bindings;
        self.scope.symbol_table.module = self.prelude_module;
        _ = self.enterModuleContext(self.prelude_module);
        try self.declareNoneErrorSet();
        try self.declareTaskType();
        try self.loadPrelude(prelude);

        const module_scopes = try self.allocator.alloc(*ModuleScope, loaded_modules.len);
        for (loaded_modules, 0..) |*module, i| {
            const root_table = try SymbolTable.init(self.allocator, self.prelude_module.root, null);
            module_scopes[i] = try ModuleScope.create(self.allocator, module.path, module.src, root_table, false);
            module_scopes[i].file_path = module.file_path;
            module_scopes[i].bindings = module.bindings;
            if (i + 1 < loaded_modules.len) {
                module_scopes[i].qualifier = std.fs.path.basename(module.path);
            }
            root_table.module = module_scopes[i];
            _ = self.enterModuleContext(module_scopes[i]);
            self.scope.symbol_table = root_table;
            try self.bindImports(module, module_scopes);
            try self.hoistDeclarations(&module.root);
            try self.registerExterns(&module.root);
            try self.checkPublicSignatures(&module.root);
            try self.checkTopLevelStatements(&module.root);
            _ = try self.visitBlock(&module.root);
        }
        try self.lowerPendingCopies();
        try self.resolveErrorEdges();
        try self.checkPendingWhens();

        const entry = module_scopes[module_scopes.len - 1];
        _ = self.enterModuleContext(entry);
        self.scope.symbol_table = entry.root;
        const instructions = try self.allocator.alloc(ir.Instruction, 1);
        instructions[0] = .{ .expression_stmt = .{ .value = try self.mainCall(&loaded_modules[loaded_modules.len - 1].root) } };

        return .{
            .instructions = instructions,
            .externs = try self.externs.toOwnedSlice(self.allocator),
            .boxed_uids = self.boxed_uids,
            .functions = try self.functions.toOwnedSlice(self.allocator),
            .defined_types = try self.defined_types.toOwnedSlice(self.allocator),
            .error_names = self.type_table.error_names.items,
        };
    }

    fn declareNoneErrorSet(self: *Self) !void {
        self.none_error_set = try self.createErrorSet(none_error_set_name, .none);
        try self.scope.symbol_table.put(.{
            .identifier = none_error_set_name,
            .uid = self.scope.genUid(),
            .type_id = self.type_table.getPrimitive(.meta),
            .kind = .{ .defined_type = .{ .blueprint_type_id = self.none_error_set } },
        });
    }

    fn declareTaskType(self: *Self) !void {
        try self.scope.symbol_table.put(.{
            .identifier = task_type_name,
            .uid = self.scope.genUid(),
            .type_id = self.type_table.getPrimitive(.meta),
            .kind = .{ .task_type = {} },
        });
    }

    fn loadPrelude(self: *Self, prelude: *const ast.Root) !void {
        try self.hoistDeclarations(prelude);
        try self.registerExterns(prelude);

        for (prelude.statements.items) |*stmt| {
            if (stmt.data != .func or stmt.data.func.is_extern) continue;
            _ = try self.visitFuncStmt(stmt);
        }
    }

    fn enterModuleContext(self: *Self, module: *ModuleScope) *ModuleScope {
        const saved = self.module;
        self.module = module;
        self.src = module.src;
        self.err_reporter.src = module.src;
        self.err_reporter.file_path = module.file_path;
        self.err_reporter.imported_modules = module.imports.keys();
        self.err_reporter.hidden_names = &module.hidden_names;
        self.is_in_prelude = module.is_prelude;
        return saved;
    }

    fn exitModuleContext(self: *Self, saved: *ModuleScope) void {
        _ = self.enterModuleContext(saved);
    }

    fn bindImports(self: *Self, module: *const modules.Module, module_scopes: []const *ModuleScope) !void {
        for (module.imports) |import| {
            const target = module_scopes[import.module_index];
            if (self.module.imports.contains(import.stmt.name)) {
                return self.err_reporter.alreadyDefined(import.stmt.name, import.stmt.path_loc);
            }
            try self.module.imports.put(self.allocator, import.stmt.name, target);
            var target_symbols = target.root.symbols.iterator();
            while (target_symbols.next()) |entry| {
                if (!entry.value_ptr.is_file_only) continue;
                const hidden_name = try std.fmt.allocPrint(self.allocator, "{s}::{s}", .{ import.stmt.name, entry.key_ptr.* });
                try self.module.hidden_names.put(self.allocator, hidden_name, {});
            }
            for (import.stmt.selected) |selected| {
                const symbol = target.root.symbols.get(selected.identifier) orelse {
                    return self.err_reporter.notDefinedInModule(selected.identifier, target.path, selected.loc);
                };
                if (symbol.is_file_only) {
                    return self.err_reporter.notVisible(selected.identifier, selected.loc);
                }
                if (self.module.root.get(selected.name) != null) {
                    return self.err_reporter.alreadyDefined(selected.name, selected.loc);
                }
                try self.module.selected.put(self.allocator, selected.name, symbol);
            }
        }
        self.err_reporter.imported_modules = self.module.imports.keys();
    }

    fn registerExterns(self: *Self, root: *const ast.Root) !void {
        const bindings = self.module.bindings orelse return;
        for (root.statements.items) |stmt| {
            if (stmt.data != .func or !stmt.data.func.is_extern) continue;
            const func_stmt = stmt.data.func;
            const symbol = try self.scope.symbol_table.getOrError(func_stmt.identifier);
            const binding = bindings.get(func_stmt.identifier) orelse {
                return self.err_reporter.externWithoutBinding(func_stmt.identifier, stmt.loc);
            };
            if (binding == .intrinsic) {
                try self.intrinsic_by_uid.put(self.allocator, symbol.uid, binding.intrinsic);
            }
            if (symbol.kind == .generic_function) {
                const generic_extern = try self.genericExtern(symbol.kind.generic_function, binding);
                try self.extern_uids.put(self.allocator, generic_extern.uid, {});
                try self.externs.append(self.allocator, generic_extern);
                continue;
            }
            const func = self.type_table.getTypePtrById(symbol.type_id).kind.function;
            try self.extern_uids.put(self.allocator, func.uid, {});
            try self.externs.append(self.allocator, .{
                .uid = func.uid,
                .identifier = func.identifier,
                .params = func.params,
                .binding = binding,
                .error_base = self.declaredErrorBase(func.return_type_id),
            });
        }
    }

    fn declaredErrorBase(self: *Self, return_type_id: tt.TypeId) i64 {
        const return_type = self.type_table.getTypePtrById(return_type_id);
        if (return_type.kind != .error_union) return 0;
        const set = self.errorSetOf(return_type.kind.error_union.error_set);
        if (set.kind != .declared) return 0;
        return std.mem.min(i64, set.ids.keys());
    }

    fn genericExtern(self: *Self, index: usize, binding: native.Binding) CompileError!ir.Extern {
        const generic = self.generic_functions.items[index];
        const args = try self.allocator.alloc(tt.TypeId, generic.func_stmt.slots.len);
        for (generic.func_stmt.slots, 0..) |slot, i| {
            args[i] = try self.addSlotType(slot);
        }
        const saved_table = try self.enterSlotScope(generic.func_stmt.slots, args);
        defer self.scope.symbol_table = saved_table;
        const symbol = try self.createEmptyFuncSymbol(generic.func_stmt.identifier);
        try self.fulfillFuncType(symbol, &generic.func_stmt.def, null, generic.loc);
        const func = self.type_table.getTypePtrById(symbol.type_id).kind.function;
        return .{ .uid = generic.extern_uid.?, .identifier = generic.func_stmt.identifier, .params = func.params, .binding = binding };
    }

    fn isDeclaration(stmt: ast.Statement) bool {
        return switch (stmt) {
            .import, .func, .@"struct", .@"enum", .@"union", .error_set, .trait => true,
            else => false,
        };
    }

    fn checkPublicSignatures(self: *Self, root: *const ast.Root) !void {
        for (root.statements.items) |stmt| {
            switch (stmt.data) {
                .func => |fs| {
                    if (fs.is_file_only) continue;
                    try self.checkPublicFnDef(fs.identifier, &fs.def);
                },
                .@"struct" => |ss| {
                    if (ss.is_file_only) continue;
                    try self.checkPublicFields(ss.def.fields);
                    try self.checkPublicFields(ss.def.static_fields);
                    try self.checkPublicMethods(ss.def.funcs);
                },
                .@"enum" => |es| {
                    if (es.is_file_only) continue;
                    try self.checkPublicFields(es.def.static_fields);
                    try self.checkPublicMethods(es.def.funcs);
                },
                .@"union" => |un| {
                    if (un.is_file_only) continue;
                    for (un.def.variants) |variant| try self.checkPublicAnnotation(un.identifier, variant.type);
                    try self.checkPublicFields(un.def.static_fields);
                    try self.checkPublicMethods(un.def.funcs);
                },
                .trait => |ts| {
                    if (ts.is_file_only) continue;
                    try self.checkPublicMethods(ts.methods);
                },
                .let => |ls| {
                    if (ls.is_file_only) continue;
                    const annotation = ls.type_annotation orelse continue;
                    try self.checkPublicAnnotation(ls.identifier, annotation);
                },
                else => {},
            }
        }
    }

    fn checkPublicFields(self: *Self, fields: []const ast.Field) CompileError!void {
        for (fields) |field| {
            if (field.is_file_only) continue;
            const annotation = field.type orelse continue;
            try self.checkPublicAnnotation(field.identifier, annotation);
        }
    }

    fn checkPublicMethods(self: *Self, funcs: []const ast.StatementNode) CompileError!void {
        for (funcs) |func_node| {
            const func = func_node.data.func;
            if (func.is_file_only) continue;
            try self.checkPublicFnDef(func.identifier, &func.def);
        }
    }

    fn checkPublicFnDef(self: *Self, declaration: []const u8, def: *const ast.FnDef) CompileError!void {
        for (def.params) |param| {
            const annotation = param.type_annotation orelse continue;
            try self.checkPublicAnnotation(declaration, annotation);
        }
        if (def.return_type) |return_type| {
            try self.checkPublicAnnotation(declaration, return_type);
        }
        const error_set = def.error_set orelse return;
        if (error_set != .named) return;
        if (self.isFileOnlyName(error_set.named.identifier)) {
            return self.err_reporter.privateTypeInPublicSignature(error_set.named.identifier, declaration, error_set.named.loc);
        }
    }

    fn checkPublicAnnotation(self: *Self, declaration: []const u8, annotation: *const ast.TypeAnnotation) CompileError!void {
        switch (annotation.type) {
            .@"struct" => |name| {
                if (self.isFileOnlyName(name)) {
                    return self.err_reporter.privateTypeInPublicSignature(name, declaration, annotation.loc);
                }
                for (annotation.args) |arg| try self.checkPublicAnnotation(declaration, arg);
            },
            .array => |inner| try self.checkPublicAnnotation(declaration, inner),
            .anonymous_struct => |anonymous| try self.checkPublicFields(anonymous.fields),
            .function => |func| {
                for (func.params) |param| try self.checkPublicAnnotation(declaration, param.type);
                try self.checkPublicAnnotation(declaration, func.return_type);
                const set_name = func.error_set orelse return;
                if (self.isFileOnlyName(set_name)) {
                    return self.err_reporter.privateTypeInPublicSignature(set_name, declaration, annotation.loc);
                }
            },
            else => {},
        }
    }

    fn isFileOnlyName(self: *Self, name: []const u8) bool {
        const symbol = self.module.root.symbols.get(name) orelse return false;
        return symbol.is_file_only;
    }

    fn checkTopLevelStatements(self: *Self, root: *const ast.Root) !void {
        for (root.statements.items) |stmt| {
            if (isDeclaration(stmt.data)) continue;
            if (stmt.data != .let) return self.err_reporter.statementOutsideMain(stmt.loc);
            const let_stmt = stmt.data.let;
            if (self.constant_by_exp.contains(let_stmt.value)) continue;
            return self.err_reporter.topLevelLetNotConstant(let_stmt.identifier, stmt.loc);
        }
    }

    fn mainCall(self: *Self, root: *const ast.Root) !*ir.Value {
        const main_stmt = for (root.statements.items) |stmt| {
            if (stmt.data == .func and std.mem.eql(u8, stmt.data.func.identifier, "main")) break stmt;
        } else return self.err_reporter.missingMain();

        const symbol = try self.scope.symbol_table.getOrError("main");
        const func = self.type_table.getTypePtrById(symbol.type_id).kind.function;
        const void_type = self.type_table.getPrimitive(.void);
        const return_type = self.type_table.getTypePtrById(func.return_type_id);
        const is_failing = return_type.kind == .error_union;
        const payload = if (is_failing) return_type.kind.error_union.payload else func.return_type_id;
        if (func.params.len > 0 or payload != void_type) return self.err_reporter.invalidMainSignature(main_stmt.loc);

        const call = try ir.Value.init(self.allocator, .{
            .data = .{ .fn_call = .{ .fn_uid = func.uid, .identifier = symbol.identifier, .args = &.{} } },
            .type_id = func.return_type_id,
        });
        if (!is_failing) return call;
        return ir.Value.init(self.allocator, .{
            .data = .{ .try_call = .{ .call = call, .is_top_level = true } },
            .type_id = void_type,
        });
    }

    fn resolveErrorEdges(self: *Self) !void {
        var is_changed = true;
        while (is_changed) {
            is_changed = false;
            for (self.error_edges.items) |edge| {
                const to_set = self.errorSetOf(edge.to);
                if (to_set.kind != .inferred) continue;
                for (self.errorSetOf(edge.from).ids.keys()) |id| {
                    if (to_set.has(id)) continue;
                    try to_set.ids.put(self.allocator, id, {});
                    is_changed = true;
                }
            }
        }

        for (self.error_edges.items) |edge| {
            const to_set = self.errorSetOf(edge.to);
            if (to_set.kind == .inferred) continue;
            for (self.errorSetOf(edge.from).ids.keys()) |id| {
                if (to_set.has(id)) continue;
                _ = self.enterModuleContext(edge.module);
                return self.err_reporter.errorNotInSet(
                    self.type_table.error_names.items[@intCast(id)],
                    try self.type_table.name(edge.to, self.allocator),
                    edge.loc,
                );
            }
        }
    }

    fn checkPendingWhens(self: *Self) !void {
        for (self.pending_when_checks.items) |check| {
            _ = self.enterModuleContext(check.module);
            const set = self.errorSetOf(check.set_type_id);
            var seen: std.AutoHashMap(i64, void) = .init(self.allocator);

            for (check.when_stmt.prongs, 0..) |prong, prong_idx| {
                for (check.values[prong_idx], 0..) |value, i| {
                    const id = value.data.i_int;
                    if (!set.has(id)) {
                        return self.err_reporter.errorNotInSet(
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
                return self.err_reporter.whenNotExhaustive(missing.items, check.when_loc);
            }
            if (check.when_stmt.else_block != null and missing.items.len == 0) {
                return self.err_reporter.whenUselessElse(check.when_stmt.else_loc.?);
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
        error_set.* = .{ .kind = .declared };

        for (stmt.variants, 0..) |variant, i| {
            if (variants.contains(variant.identifier)) {
                return self.err_reporter.alreadyDefined(variant.identifier, variant.loc);
            }
            const id = try self.type_table.addErrorName(try self.qualifiedName(self.module, stmt.identifier), variant.identifier);
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

    fn createErrorSet(self: *Self, identifier: []const u8, kind: tt.ErrorSet.Kind) !tt.TypeId {
        const error_set = try self.allocator.create(tt.ErrorSet);
        error_set.* = .{ .kind = kind };

        return self.type_table.addType(.{
            .kind = .{
                .@"enum" = .{
                    .identifier = identifier,
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
            .inferred => return self.createErrorSet(fn_name, .inferred),
            .named => |named| {
                const symbol = self.scope.symbol_table.getOrError(named.identifier) catch {
                    return self.err_reporter.typeNotDefined(named.identifier, named.loc);
                };
                if (symbol.kind == .type_slot) {
                    if (!self.type_table.isErrorSet(symbol.type_id)) {
                        return self.err_reporter.slotNotMarkedError(named.identifier, named.loc);
                    }
                    return symbol.type_id;
                }
                if (symbol.kind != .defined_type or !self.type_table.isErrorSet(symbol.kind.defined_type.blueprint_type_id)) {
                    return self.err_reporter.notAnErrorSet(named.identifier, named.loc);
                }
                return symbol.kind.defined_type.blueprint_type_id;
            },
        }
    }

    fn canCoerce(self: *Self, from: tt.TypeId, to: tt.TypeId, loc: err.Loc) !bool {
        if (self.type_table.canCoerce(from, to)) return true;
        if (try self.canCoerceThroughTypeList(from, to)) return true;

        const from_is_nullable = self.type_table.getTypePtrById(from).is_nullable;
        const to_is_nullable = self.type_table.getTypePtrById(to).is_nullable;
        if (from_is_nullable and !to_is_nullable) return false;

        const from_base = self.type_table.typeIdByNullableTypeId.get(from) orelse from;
        const to_base = self.type_table.typeIdByNullableTypeId.get(to) orelse to;
        const from_kind = self.type_table.getTypePtrById(from_base).kind;
        const to_kind = self.type_table.getTypePtrById(to_base).kind;
        if (from_kind == .task and to_kind == .task) {
            return self.canCoerceTask(from_kind.task, to_kind.task, loc);
        }
        if (!self.type_table.isErrorSet(from_base) or !self.type_table.isErrorSet(to_base)) return false;
        if (self.errorSetOf(from_base).kind != .inferred or self.errorSetOf(to_base).kind == .inferred) return false;

        try self.addErrorEdge(from_base, to_base, loc);
        return true;
    }

    fn canCoerceTask(self: *Self, from: tt.TaskType, to: tt.TaskType, loc: err.Loc) CompileError!bool {
        if (from.payload != to.payload) return false;
        if (from.error_set == to.error_set) return true;
        return self.canCoerce(from.error_set, to.error_set, loc);
    }

    fn canCoerceThroughTypeList(self: *Self, from: tt.TypeId, to: tt.TypeId) CompileError!bool {
        const from_slot = self.slotBase(from);
        const to_slot = self.slotBase(to);
        if ((from_slot == null) == (to_slot == null)) return false;
        const slot_id = from_slot orelse to_slot.?;
        const type_list = self.type_table.getTypePtrById(slot_id).kind.slot.type_list;
        if (type_list.len == 0) return false;
        for (type_list) |member| {
            const is_convertible = if (from_slot != null) self.canConvertTo(try self.substituteSlot(from, member), to) else self.canConvertTo(from, try self.substituteSlot(to, member));
            if (!is_convertible) return false;
        }
        return true;
    }

    fn checkPayloadErrorOverlap(self: *Self, error_set_type_id: tt.TypeId, payload: tt.TypeId, loc: err.Loc) !void {
        const payload_base = self.type_table.typeIdByNullableTypeId.get(payload) orelse payload;
        if (!self.type_table.isErrorSet(payload_base)) return;
        const error_set = self.errorSetOf(error_set_type_id);
        if (error_set.kind == .inferred) return;
        for (self.errorSetOf(payload_base).ids.keys()) |id| {
            if (!error_set.has(id)) continue;
            return self.err_reporter.payloadSharesErrors(
                try self.type_table.name(payload, self.allocator),
                try self.type_table.name(error_set_type_id, self.allocator),
                self.type_table.error_names.items[@intCast(id)],
                loc,
            );
        }
    }

    fn addErrorEdge(self: *Self, from: tt.TypeId, to: tt.TypeId, loc: err.Loc) !void {
        try self.error_edges.append(self.allocator, .{ .from = from, .to = to, .loc = loc, .module = self.module });
    }

    pub fn hoistDeclarations(self: *Self, root: *const ast.Root) !void {
        self.is_hoisting = true;
        try self.hoistDeclarationsOf(root);
        self.is_hoisting = false;
        for (self.pending_trait_checks.items) |pending| try self.checkSlotFits(pending.slot, pending.type_id, pending.loc);
        self.pending_trait_checks.clearRetainingCapacity();
    }

    fn hoistDeclarationsOf(self: *Self, root: *const ast.Root) !void {
        for (root.statements.items) |*stmt| {
            switch (stmt.data) {
                .error_set => |es| {
                    var symbol = try self.createErrorSetSymbol(&es);
                    symbol.is_file_only = es.is_file_only;
                    try self.recordTypeModule(symbol, self.module);
                    self.scope.symbol_table.put(symbol) catch return self.err_reporter.alreadyDefined(es.identifier, stmt.loc);
                },
                .trait => |ts| {
                    try self.traits.append(self.allocator, .{ .trait_stmt = &stmt.data.trait, .loc = stmt.loc, .module = self.module });
                    self.scope.symbol_table.put(.{
                        .identifier = ts.identifier,
                        .uid = self.scope.genUid(),
                        .type_id = self.type_table.getPrimitive(.meta),
                        .kind = .{ .trait = self.traits.items.len - 1 },
                        .is_file_only = ts.is_file_only,
                    }) catch return self.err_reporter.alreadyDefined(ts.identifier, stmt.loc);
                },
                .func => |fs| {
                    if (fs.is_extern and self.module.bindings == null) {
                        return self.err_reporter.externOutsideStd(fs.identifier, stmt.loc);
                    }
                    if (fs.slots.len > 0) {
                        var symbol = try self.createGenericFuncSymbol(&stmt.data.func, stmt.loc);
                        symbol.is_file_only = fs.is_file_only;
                        self.scope.symbol_table.put(symbol) catch return self.err_reporter.alreadyDefined(fs.identifier, stmt.loc);
                        continue;
                    }
                    var symbol = try self.createEmptyFuncSymbol(fs.identifier);
                    symbol.is_file_only = fs.is_file_only;
                    if (!fs.is_extern) {
                        try self.qualifyFunctionName(symbol, self.module);
                    }
                    self.scope.symbol_table.put(symbol) catch return self.err_reporter.alreadyDefined(fs.identifier, stmt.loc);
                },
                .@"struct" => |ss| {
                    if (ss.slots.len > 0) {
                        var symbol = try self.createGenericTypeSymbol(ss.identifier, ss.slots, .{ .@"struct" = &stmt.data.@"struct".def }, stmt.loc);
                        symbol.is_file_only = ss.is_file_only;
                        self.scope.symbol_table.put(symbol) catch return self.err_reporter.alreadyDefined(ss.identifier, stmt.loc);
                        continue;
                    }
                    var symbol = try self.createEmptyStructSymbol(ss.identifier);
                    symbol.is_file_only = ss.is_file_only;
                    try self.recordTypeModule(symbol, self.module);
                    self.scope.symbol_table.put(symbol) catch return self.err_reporter.alreadyDefined(ss.identifier, stmt.loc);
                },
                .@"enum" => |es| {
                    var symbol = try self.createEmptyEnumSymbol(es.identifier);
                    symbol.is_file_only = es.is_file_only;
                    try self.recordTypeModule(symbol, self.module);
                    self.scope.symbol_table.put(symbol) catch return self.err_reporter.alreadyDefined(es.identifier, stmt.loc);
                },
                .@"union" => |un| {
                    if (un.slots.len > 0) {
                        var symbol = try self.createGenericTypeSymbol(un.identifier, un.slots, .{ .@"union" = &stmt.data.@"union".def }, stmt.loc);
                        symbol.is_file_only = un.is_file_only;
                        self.scope.symbol_table.put(symbol) catch return self.err_reporter.alreadyDefined(un.identifier, stmt.loc);
                        continue;
                    }
                    var symbol = try self.createEmptyUnionSymbol(un.identifier);
                    symbol.is_file_only = un.is_file_only;
                    try self.recordTypeModule(symbol, self.module);
                    self.scope.symbol_table.put(symbol) catch return self.err_reporter.alreadyDefined(un.identifier, stmt.loc);
                },
                else => {},
            }
        }

        for (root.statements.items) |stmt| {
            if (stmt.data != .let) continue;
            const let_stmt = stmt.data.let;
            if (!self.isConstantCandidate(let_stmt)) continue;
            const index = self.constants.items.len;
            try self.constants.append(self.allocator, .{ .let_stmt = let_stmt, .loc = stmt.loc, .module = self.module });
            try self.constant_by_exp.put(self.allocator, let_stmt.value, index);
            self.scope.symbol_table.put(.{
                .identifier = let_stmt.identifier,
                .uid = self.scope.genUid(),
                .type_id = self.type_table.getPrimitive(.void),
                .kind = .{ .constant = index },
                .is_file_only = let_stmt.is_file_only,
            }) catch return self.err_reporter.alreadyDefined(let_stmt.identifier, stmt.loc);
        }

        for (root.statements.items) |stmt| {
            switch (stmt.data) {
                .func => |fs| {
                    if (fs.slots.len > 0) continue;
                    const symbol = try self.scope.symbol_table.getOrError(fs.identifier);
                    try self.fulfillFuncType(symbol, &fs.def, null, stmt.loc);
                },
                .@"struct" => |ss| {
                    const symbol = try self.scope.symbol_table.getOrError(ss.identifier);
                    if (symbol.kind == .generic_type) {
                        try self.createOpaqueCopy(symbol.kind.generic_type);
                        continue;
                    }
                    try self.fulfillStructType(symbol, &ss.def);
                },

                .@"enum" => |ss| {
                    const symbol = try self.scope.symbol_table.getOrError(ss.identifier);
                    try self.fulfillEnumType(symbol, &ss.def);
                },

                .trait => |ts| {
                    const symbol = try self.scope.symbol_table.getOrError(ts.identifier);
                    _ = try self.resolveTrait(symbol.kind.trait);
                },

                .@"union" => |us| {
                    const symbol = try self.scope.symbol_table.getOrError(us.identifier);
                    if (symbol.kind == .generic_type) {
                        try self.createOpaqueCopy(symbol.kind.generic_type);
                        continue;
                    }
                    try self.fulfillUnionType(symbol, &us.def, stmt.loc);
                },
                else => {},
            }
        }
    }

    fn isConstantCandidate(self: *Self, let_stmt: ast.LetStmt) bool {
        if (let_stmt.is_mut) return false;
        if (let_stmt.type_annotation) |annotation| {
            if (annotation.is_nullable or annotation.type == .array or annotation.type == .anonymous_struct) return false;
        }
        return self.isConstantExp(let_stmt.value);
    }

    fn isConstantExp(self: *Self, exp: *const ast.ExpNode) bool {
        return switch (exp.data) {
            .int_literal, .float_literal, .byte_literal, .string_literal, .bool_literal => true,
            .identifier => |name| {
                const symbol = self.scope.symbol_table.get(name) orelse return false;
                return symbol.kind == .constant;
            },
            .indexed => |indexed| indexed.index.data == .identifier and self.isTypeName(indexed.target) and !indexed.is_nullable,
            .unary_exp => |unary| self.isConstantExp(unary.right),
            .binary_exp => |binary| self.isConstantExp(binary.left) and self.isConstantExp(binary.right),
            else => false,
        };
    }

    fn isConstantType(self: *Self, type_id: tt.TypeId) bool {
        const type_ptr = self.type_table.getTypePtrById(type_id);
        if (type_ptr.is_nullable) return false;
        return switch (type_ptr.kind) {
            .int, .byte, .float, .boolean, .string, .@"enum" => true,
            else => false,
        };
    }

    fn resolveConstant(self: *Self, index: usize) CompileError!*ir.Value {
        if (self.constants.items[index].value) |value| return value;
        const constant = self.constants.items[index];
        if (constant.is_resolving) return self.err_reporter.notDefined(constant.let_stmt.identifier, constant.loc);
        self.constants.items[index].is_resolving = true;
        const saved_module = self.enterModuleContext(constant.module);
        defer self.exitModuleContext(saved_module);

        const saved_table = self.scope.symbol_table;
        self.scope.symbol_table = self.module.root;
        defer self.scope.symbol_table = saved_table;

        const annotated_type_id = if (constant.let_stmt.type_annotation) |annotation| try self.resolveTypeAnnotation(annotation, null) else null;
        var value = try self.evalExpExpecting(constant.let_stmt.value, annotated_type_id);
        if (annotated_type_id) |type_id| {
            if (!try self.canCoerce(value.type_id, type_id, constant.let_stmt.value.loc)) {
                return self.err_reporter.invalidType(
                    try self.type_table.name(type_id, self.allocator),
                    try self.type_table.name(value.type_id, self.allocator),
                    constant.let_stmt.value.loc,
                );
            }
            if (value.type_id != type_id) value = try ir.Value.init(self.allocator, .{ .data = value.data, .type_id = type_id });
        }
        if (!self.isConstantType(value.type_id)) {
            return self.err_reporter.invalidConstantType(constant.let_stmt.identifier, try self.type_table.name(value.type_id, self.allocator), constant.let_stmt.value.loc);
        }

        self.constants.items[index].value = value;
        self.constants.items[index].is_resolving = false;
        self.module.root.symbols.getPtr(constant.let_stmt.identifier).?.type_id = value.type_id;
        return value;
    }

    fn recordSlotModules(self: *Self, slots: []const ast.Slot) !void {
        for (slots) |slot| {
            if (slot.constraint.len == 0) continue;
            try self.slot_modules.put(self.allocator, @intFromPtr(slot.constraint.ptr), self.module);
        }
    }

    fn createGenericTypeSymbol(self: *Self, identifier: []const u8, slots: []const ast.Slot, def: @FieldType(GenericType, "def"), loc: err.Loc) !Symbol {
        try self.recordSlotModules(slots);
        const index = self.generic_types.items.len;
        try self.generic_types.append(self.allocator, .{ .identifier = identifier, .slots = slots, .def = def, .loc = loc, .module = self.module });
        return Symbol{
            .identifier = identifier,
            .uid = self.scope.genUid(),
            .kind = .{ .generic_type = index },
            .type_id = self.type_table.getPrimitive(.meta),
        };
    }

    fn createGenericFuncSymbol(self: *Self, func_stmt: *const ast.FuncStmt, loc: err.Loc) !Symbol {
        try self.recordSlotModules(func_stmt.slots);
        const index = self.generic_functions.items.len;
        const extern_uid = if (func_stmt.is_extern) self.scope.genFnUid() else null;
        try self.generic_functions.append(self.allocator, .{ .func_stmt = func_stmt, .loc = loc, .extern_uid = extern_uid, .module = self.module });
        return Symbol{
            .identifier = func_stmt.identifier,
            .uid = self.scope.genUid(),
            .kind = .{ .generic_function = index },
            .type_id = self.type_table.getPrimitive(.void),
        };
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

    fn recordTypeModule(self: *Self, symbol: Symbol, module: *ModuleScope) !void {
        try self.type_modules.put(self.allocator, symbol.kind.defined_type.blueprint_type_id, module);
        const type_ptr = self.type_table.getTypePtrById(symbol.kind.defined_type.blueprint_type_id);
        const identifier = switch (type_ptr.kind) {
            .@"struct" => |*kind| &kind.identifier,
            .@"enum" => |*kind| &kind.identifier,
            .@"union" => |*kind| &kind.identifier,
            else => return,
        };
        identifier.* = try self.qualifiedName(module, identifier.*);
    }

    fn qualifyFunctionName(self: *Self, symbol: Symbol, module: *ModuleScope) !void {
        const func = &self.type_table.getTypePtrById(symbol.type_id).kind.function;
        func.identifier = try self.qualifiedName(module, func.identifier);
    }

    fn qualifiedName(self: *Self, module: *ModuleScope, identifier: []const u8) ![]const u8 {
        const module_name = module.qualifier orelse return identifier;
        return std.fmt.allocPrint(self.allocator, "{s}::{s}", .{ module_name, identifier });
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
            return self.err_reporter.cantInferAnonymousStruct(exp.loc);
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

        var variants: std.StringHashMap(i64) = .init(self.allocator);

        const variants_in_order = try self.allocator.alloc([]const u8, enum_def.variants.len);

        const is_explicit = enum_def.variants[0].value != null;
        for (enum_def.variants, 0..) |variant, i| {
            const value: i64 = if (is_explicit) variant.value.? else @intCast(i);
            try variants.put(variant.identifier, value);
            variants_in_order[i] = variant.identifier;
        }
        self.type_table.getTypePtrById(blueprint_type_id).kind.@"enum".variants = variants;
        self.type_table.getTypePtrById(blueprint_type_id).kind.@"enum".variants_in_order = variants_in_order;

        const static_fields = try self.createStaticFields(enum_def.static_fields, blueprint_type_id);
        self.type_table.getTypePtrById(blueprint_type_id).kind.@"enum".static_fields = static_fields;
        const methods = try self.createMethodSignatures(enum_def.funcs, enum_symbol);
        self.type_table.getTypePtrById(blueprint_type_id).kind.@"enum".methods = methods;
    }

    fn fulfillUnionType(self: *Self, union_symbol: Symbol, union_def: *const ast.UnionDef, loc: err.Loc) !void {
        const blueprint_type_id = union_symbol.kind.defined_type.blueprint_type_id;

        if (union_def.variants.len > tt.Union.MAX_VARIANTS) {
            return self.err_reporter.tooManyUnionVariants(union_symbol.identifier, tt.Union.MAX_VARIANTS, loc);
        }

        var variants: std.StringHashMap(tt.TypeId) = .init(self.allocator);

        const variants_in_order = try self.allocator.alloc([]const u8, union_def.variants.len);
        for (union_def.variants, 0..) |variant, i| {
            try variants.put(variant.identifier, try self.resolveTypeAnnotation(variant.type, blueprint_type_id));
            variants_in_order[i] = variant.identifier;
        }

        self.type_table.getTypePtrById(blueprint_type_id).kind.@"union".variants = variants;
        self.type_table.getTypePtrById(blueprint_type_id).kind.@"union".variants_in_order = variants_in_order;

        const static_fields = try self.createStaticFields(union_def.static_fields, blueprint_type_id);
        self.type_table.getTypePtrById(blueprint_type_id).kind.@"union".static_fields = static_fields;
        const methods = try self.createMethodSignatures(union_def.funcs, union_symbol);
        self.type_table.getTypePtrById(blueprint_type_id).kind.@"union".methods = methods;
    }

    fn fulfillStructType(self: *Self, struct_symbol: Symbol, struct_def: *const ast.Struct) !void {
        const blueprint_type_id = struct_symbol.kind.defined_type.blueprint_type_id;
        var fields_in_order: std.ArrayList(tt.TypedIdentifier) = .empty;
        var fields: std.StringHashMap(tt.TypedIdentifier) = .init(self.allocator);
        const static_fields = try self.createStaticFields(struct_def.static_fields, blueprint_type_id);

        for (struct_def.fields) |field| {
            const struct_field = try self.createField(field, blueprint_type_id);
            try fields_in_order.append(
                self.allocator,
                struct_field,
            );
            try fields.put(field.identifier, struct_field);
        }

        const methods = try self.createMethodSignatures(struct_def.funcs, struct_symbol);
        for (struct_def.funcs) |func_node| {
            const name = func_node.data.func.identifier;
            if (!fields.contains(name)) continue;
            return self.err_reporter.fieldAndMethodSameName(name, struct_symbol.identifier, func_node.loc);
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

    fn createStaticFields(self: *Self, fields: []const ast.Field, owner_type_id: tt.TypeId) CompileError!std.StringHashMap(tt.TypedIdentifier) {
        var static_fields: std.StringHashMap(tt.TypedIdentifier) = .init(self.allocator);
        for (fields) |field| try static_fields.put(field.identifier, try self.createStaticField(field, owner_type_id));
        return static_fields;
    }

    fn createMethodSignatures(self: *Self, funcs: []const ast.StatementNode, owner_symbol: Symbol) CompileError!std.StringHashMap(tt.TypeId) {
        var methods: std.StringHashMap(tt.TypeId) = .init(self.allocator);
        for (funcs) |func_node| {
            const func = func_node.data.func;
            const fn_symbol = try self.createEmptyFuncSymbol(func.identifier);
            try self.fulfillFuncType(fn_symbol, &func.def, owner_symbol, func_node.loc);
            try methods.put(func.identifier, fn_symbol.type_id);
            if (func.is_file_only) {
                try self.file_only_methods.put(self.allocator, fn_symbol.type_id, {});
            }
        }
        return methods;
    }

    fn createStaticField(self: *Self, field: ast.Field, current_struct_type_id: tt.TypeId) CompileError!tt.TypedIdentifier {
        const loc = field.default_value.?.loc;
        if (field.is_mut) return self.err_reporter.staticFieldCantBeMutable(field.identifier, loc);
        if (!self.isConstantExp(field.default_value.?)) return self.err_reporter.staticFieldNotConstant(field.identifier, loc);
        var typed = try self.createField(field, current_struct_type_id);
        if (!self.isConstantType(typed.type_id)) return self.err_reporter.staticFieldNotConstant(field.identifier, loc);
        const value = typed.default_value.?;
        if (value.type_id != typed.type_id) typed.default_value = try ir.Value.init(self.allocator, .{ .data = value.data, .type_id = typed.type_id });
        return typed;
    }

    fn createField(self: *Self, field: ast.Field, current_struct_type_id: ?tt.TypeId) CompileError!tt.TypedIdentifier {
        var field_type_id: tt.TypeId = undefined;
        var field_default_value: ?*ir.Value = null;

        if (field.type) |_type| {
            field_type_id = try self.resolveTypeAnnotation(_type, current_struct_type_id);
        }

        if (field.default_value) |initial_value| {
            const value = try self.evalExpExpecting(initial_value, if (field.type != null) field_type_id else null);
            const value_type_id = value.type_id;
            field_default_value = value;

            if (field.type == null) {
                field_type_id = value_type_id;
            } else if (!try self.canCoerce(value_type_id, field_type_id, field.default_value.?.loc)) {
                return self.err_reporter.invalidType(
                    try field.type.?.value(self.allocator),
                    try self.type_table.name(value_type_id, self.allocator),
                    field.default_value.?.loc,
                );
            }
        }

        return tt.TypedIdentifier{
            .identifier = field.identifier,
            .is_mut = field.is_mut,
            .is_file_only = field.is_file_only,
            .type_id = field_type_id,
            .default_value = field_default_value,
        };
    }

    fn fulfillFuncType(self: *Self, fn_symbol: Symbol, fn_def: *const ast.FnDef, fn_owner_symbol: ?Symbol, loc: err.Loc) !void {
        var return_type_id: tt.TypeId = undefined;
        var params_metadata: std.ArrayList(tt.TypedIdentifier) = .empty;

        //create a temporary scope just for inline return type inference
        const saved_return_type = self.scope.return_type_id;
        try self.scope.enter(self.type_table.getPrimitive(.void));
        self.fn_depth += 1;
        defer self.fn_depth -= 1;

        for (fn_def.params) |param| {
            var param_default_value: ?*ir.Value = null;
            var param_type_id: tt.TypeId = undefined;

            if (param.type_annotation) |annotation| {
                const owner_type_id = if (fn_owner_symbol) |symbol| symbol.kind.defined_type.blueprint_type_id else null;
                param_type_id = self.resolveTypeAnnotation(annotation, owner_type_id) catch |failure| {
                    if (failure != error.SemaError) {
                        return failure;
                    }
                    return self.err_reporter.typeNotDefined(
                        try param.type_annotation.?.value(self.allocator),
                        loc,
                    );
                };

                if (param.default_value) |default_value| {
                    const value = try self.evalExpExpecting(default_value, param_type_id);
                    param_default_value = try self.coerceValue(value, param_type_id, default_value.loc);
                }
            } else {
                if (param.default_value) |default_value| {
                    const value = try self.evalExpExpecting(default_value, null);
                    param_default_value = value;
                    param_type_id = value.type_id;
                } else {
                    return self.err_reporter.invalidParameterType(param.identifier, loc);
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
                return self.err_reporter.alreadyDefined(param.identifier, loc);
            };

            try params_metadata.append(self.allocator, .{
                .identifier = param.identifier,
                .type_id = param_type_id,
                .is_mut = param.is_mut,
                .default_value = param_default_value,
            });
        }

        if (fn_def.return_type) |r_type| {
            return_type_id = self.resolveTypeAnnotation(r_type, null) catch |failure| {
                if (failure != error.SemaError) {
                    return failure;
                }
                return self.err_reporter.typeNotDefined(try r_type.value(self.allocator), loc);
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
            return_type_id = try self.errorUnionType(error_set_type_id, return_type_id);
        }

        //restore main scope
        self.scope.exit(saved_return_type);

        self.type_table.getTypePtrById(fn_symbol.type_id).kind.function.return_type_id = return_type_id;
        self.type_table.getTypePtrById(fn_symbol.type_id).kind.function.params = try params_metadata.toOwnedSlice(self.allocator);
    }

    pub fn visitBlock(self: *Self, block: *const ast.Block) CompileError!VisitedBlock {
        return self.visitBranchBlock(block, null);
    }

    fn visitBranchBlock(self: *Self, block: *const ast.Block, result: ?*BranchResult) CompileError!VisitedBlock {
        var instructions: std.ArrayList(ir.Instruction) = .empty;
        try self.scope.enter(self.scope.return_type_id);
        defer self.scope.exit(self.scope.return_type_id);

        for (block.statements.items, 0..) |stmt, statement_index| {
            const is_value_tail = result != null and statement_index + 1 == block.statements.items.len;
            if (is_value_tail and stmt.data == .expression) {
                try instructions.append(self.allocator, try self.visitBranchTail(&stmt, result.?));
                break;
            }
            if (self.fn_depth > 0 and isDeclaration(stmt.data)) return self.err_reporter.declarationInsideFunction(stmt.loc);
            const instruction = switch (stmt.data) {
                .let => try self.visitLetStmt(&stmt),
                .func => try self.visitFuncStmt(&stmt),
                .@"struct" => try self.visitStructStmt(&stmt),
                .@"enum" => try self.visitEnumStmt(&stmt),
                .@"union" => try self.visitUnionStmt(&stmt),
                .import, .error_set, .trait => null,
                .assign => try self.visitAssignStmt(&stmt),
                .@"for" => try self.visitForStmt(&stmt),
                .expression => try self.visitExpressionStmt(&stmt),
                .@"break" => ir.Instruction{ .break_stmt = {} },
                .@"continue" => ir.Instruction{ .continue_stmt = {} },
                .@"return" => try self.visitReturnStmt(&stmt),
                .for_capture => ir.Instruction{ .block = try self.visitForCaptureStmt(&stmt) },
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
                return self.err_reporter.invalidExpression("a final expression or an early exit", "a branch without a value", loc);
            }
        }

        return .{
            .is_returning = areAllPathsReturning(instructions.items),
            .block = try instructions.toOwnedSlice(self.allocator),
        };
    }

    fn visitBranchTail(self: *Self, statement: *const ast.StatementNode, result: *BranchResult) CompileError!ir.Instruction {
        const value = try self.evalExpWithContext(statement.data.expression, result.expected_type_id, result.context_type_id);
        return self.storeBranchValue(value, result, statement.loc);
    }

    fn storeBranchValue(self: *Self, value: *ir.Value, result: *BranchResult, loc: err.Loc) CompileError!ir.Instruction {
        if (isValueLeaving(value)) return .{ .expression_stmt = .{ .value = value } };
        if (value.type_id == self.type_table.getPrimitive(.void)) {
            return self.err_reporter.invalidExpression("a branch value", "void", loc);
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

    fn areAllPathsReturning(instructions: ir.Block) bool {
        return doesBlockExit(instructions, .returns);
    }

    fn isBlockLeaving(instructions: ir.Block) bool {
        return doesBlockExit(instructions, .leaves_block);
    }

    fn isValueReturning(value: *const ir.Value) bool {
        return doesValueExit(value, .returns);
    }

    fn isValueLeaving(value: *const ir.Value) bool {
        return doesValueExit(value, .leaves_block);
    }

    fn doesBlockExit(instructions: ir.Block, query: ExitQuery) bool {
        for (instructions) |inst| {
            switch (inst) {
                .return_stmt, .return_error => return true,
                .break_stmt, .continue_stmt => {
                    if (query == .leaves_block) return true;
                },
                .store_var => |store| {
                    if (doesValueExit(store.value, query)) return true;
                },
                .expression_stmt => |expression| {
                    if (doesValueExit(expression.value, query)) return true;
                },
                .update_indexed => |update| {
                    if (doesValueExit(update.target, query) or doesValueExit(update.value, query)) return true;
                },
                .loop => |loop| {
                    if (loop.condition) |condition| {
                        if (doesValueExit(condition, query)) return true;
                    }
                },
                .branch_if => |branch| {
                    if (doesValueExit(branch.condition, query)) return true;
                    if (branch.else_block.len > 0 and doesBlockExit(branch.then_block, query) and doesBlockExit(branch.else_block, query)) return true;
                },
                .when => |when| {
                    if (doesWhenExit(when, query)) return true;
                },
                .block => |scoped| {
                    if (doesBlockExit(scoped, query)) return true;
                },
                else => {},
            }
        }
        return false;
    }

    fn doesWhenExit(when: ir.When, query: ExitQuery) bool {
        if (!doesBlockExit(when.default_block, query)) return false;
        if (when.null_body) |body| {
            if (!doesBlockExit(body, query)) return false;
        }
        for (when.cases) |case| {
            if (!doesBlockExit(case.body, query)) return false;
        }
        return true;
    }

    fn doesValueExit(value: *const ir.Value, query: ExitQuery) bool {
        return switch (value.data) {
            .block_value => |block| switch (query) {
                .returns => doesBlockExit(block.instructions, .returns),
                .leaves_block => !block.can_complete,
            },
            .int_to_float, .array_length, .bytes_length => |operand| doesValueExit(operand, query),
            .binary_op => |binary| doesValueExit(binary.left, query) or doesValueExit(binary.right, query),
            .unary_op => |unary| doesValueExit(unary.right, query),
            .null_box_init => |box| if (box.value) |operand| doesValueExit(operand, query) else false,
            .fn_call => |call| doAnyValuesExit(call.args, query),
            .struct_init => |initialization| doAnyValuesExit(initialization.values, query),
            .i_array => |array| doAnyValuesExit(array.values, query),
            .string_interpolation => |parts| doAnyValuesExit(parts, query),
            .union_init => |initialization| if (initialization.value) |operand| doesValueExit(operand, query) else false,
            .indexed => |indexed| doesValueExit(indexed.target, query) or doesValueExit(indexed.index, query),
            .union_tag, .union_payload => |operand| doesValueExit(operand, query),
            .try_call => |call| doesValueExit(call.call, query),
            .catch_call => |call| doesValueExit(call.call, query),
            else => false,
        };
    }

    fn doAnyValuesExit(values: []const *ir.Value, query: ExitQuery) bool {
        for (values) |value| {
            if (doesValueExit(value, query)) return true;
        }
        return false;
    }

    fn visitExpressionStmt(self: *Self, stmt: *const ast.StatementNode) CompileError!ir.Instruction {
        const expression_stmt = stmt.data.expression;

        const value = switch (expression_stmt.data) {
            .if_exp => return self.lowerIf(expression_stmt, null),
            .if_capture_exp => return ir.Instruction{ .block = try self.lowerIfCapture(expression_stmt, null) },
            .when_exp => return ir.Instruction{ .block = try self.lowerWhen(expression_stmt, null) },
            .fn_call, .orelse_exp => try self.evalExp(expression_stmt),
            .try_exp => try self.evalTry(expression_stmt),
            .catch_exp => try self.evalCatch(expression_stmt, true, null),
            else => return self.err_reporter.invalidType("function", @tagName(expression_stmt.data), expression_stmt.loc),
        };
        if (self.isFailingTask(value.type_id)) {
            return self.err_reporter.unawaitedTaskCanFail(expression_stmt.loc);
        }

        return ir.Instruction{
            .expression_stmt = .{ .value = value },
        };
    }

    fn isFailingTask(self: *Self, type_id: tt.TypeId) bool {
        const kind = self.type_table.getTypePtrById(type_id).kind;
        if (kind != .task) return false;
        return self.errorSetOf(kind.task.error_set).kind != .none;
    }

    fn evalHandledCall(self: *Self, exp: *const ast.ExpNode, expected_type: ?tt.TypeId) CompileError!*ir.Value {
        const call = try self.evalFnCall(exp, expected_type);
        if (self.type_table.isErrorUnion(call.type_id)) return self.err_reporter.unhandledError(callName(exp), exp.loc);
        return call;
    }

    fn evalFailingCall(self: *Self, exp: *const ast.ExpNode, keyword: []const u8) CompileError!*ir.Value {
        if (exp.data != .fn_call) {
            return self.err_reporter.notAFailingCall(keyword, exp.loc);
        }

        const call = try self.evalFnCall(exp, null);
        const is_failing = self.type_table.isErrorUnion(call.type_id);
        if (!is_failing and !self.has_none_error_slot) {
            return self.err_reporter.notAFailingCall(keyword, exp.loc);
        }

        return call;
    }

    fn evalTry(self: *Self, exp: *const ast.ExpNode) CompileError!*ir.Value {
        const call = try self.evalFailingCall(exp.data.try_exp.operand, "try");
        if (!self.type_table.isErrorUnion(call.type_id)) return call;
        const error_union = self.type_table.getTypePtrById(call.type_id).kind.error_union;
        const is_top_level = self.fn_depth == 0;

        if (self.activeReturnInference()) |context_index| {
            try self.addErrorEdge(error_union.error_set, try self.lambdaErrorSet(context_index), exp.loc);
        } else if (!is_top_level) {
            if (!self.type_table.isErrorUnion(self.scope.return_type_id)) {
                return self.err_reporter.tryOutsideFailingFunction(exp.loc);
            }
            const current_set = self.type_table.getTypePtrById(self.scope.return_type_id).kind.error_union.error_set;
            try self.addErrorEdge(error_union.error_set, current_set, exp.loc);
        }

        return ir.Value.init(self.allocator, .{
            .data = .{ .try_call = .{ .call = call, .is_top_level = is_top_level } },
            .type_id = error_union.payload,
        });
    }

    fn evalCatch(self: *Self, exp: *const ast.ExpNode, is_statement: bool, expected_type_id: ?tt.TypeId) CompileError!*ir.Value {
        const catch_exp = exp.data.catch_exp;
        const call = try self.evalFailingCall(catch_exp.operand, "catch");
        if (!self.type_table.isErrorUnion(call.type_id)) return call;
        const error_union = self.type_table.getTypePtrById(call.type_id).kind.error_union;
        const payload = error_union.payload;

        switch (catch_exp.fallback) {
            .value => |fallback_exp| {
                const fallback = try self.evalExpExpecting(fallback_exp, payload);
                if (try self.nullableCatchType(payload, fallback.type_id, expected_type_id)) |nullable| {
                    return ir.Value.init(self.allocator, .{
                        .data = .{ .catch_call = .{
                            .call = call,
                            .fallback = .{ .value = try self.coerceValue(fallback, nullable, fallback_exp.loc) },
                            .is_success_boxed = self.isBoxedNullable(nullable),
                        } },
                        .type_id = nullable,
                    });
                }
                const boxed_fallback = try self.coerceValue(fallback, payload, fallback_exp.loc);

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
                if (is_value_used and !isBlockLeaving(body.block)) {
                    return self.err_reporter.catchBlockMustLeave(exp.loc);
                }

                return ir.Value.init(self.allocator, .{
                    .data = .{ .catch_call = .{
                        .call = call,
                        .fallback = .{ .block = .{
                            .capture_uid = captured_symbol.uid,
                            .capture_identifier = captured_symbol.identifier,
                            .capture_type_id = captured_symbol.type_id,
                            .instructions = body.block,
                        } },
                    } },
                    .type_id = payload,
                });
            },
        }
    }

    fn nullableCatchType(self: *Self, payload: tt.TypeId, fallback_type: tt.TypeId, expected_type_id: ?tt.TypeId) CompileError!?tt.TypeId {
        const expected = expected_type_id orelse return null;
        if (!self.type_table.getTypePtrById(expected).is_nullable) return null;
        if (self.type_table.getTypePtrById(payload).is_nullable) return null;
        if (self.type_table.canCoerce(fallback_type, payload)) return null;
        const nullable = try self.type_table.getOrAddNullable(payload);
        if (!self.type_table.canCoerce(fallback_type, nullable) or !self.type_table.canCoerce(nullable, expected)) return null;
        return nullable;
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

        if (self.constant_by_exp.get(let_stmt.value)) |index| {
            _ = try self.resolveConstant(index);
            return null;
        }

        const annotated_type_id = if (let_stmt.type_annotation) |type_annotation|
            try self.resolveTypeAnnotation(type_annotation, null)
        else
            null;

        const saved_binding = self.lambda_binding;
        self.lambda_binding = if (let_stmt.value.data == .lambda) let_stmt.identifier else null;
        const expression_value = try self.evalExpExpecting(let_stmt.value, annotated_type_id);
        self.lambda_binding = saved_binding;
        const expression_type_id = expression_value.type_id;

        if (annotated_type_id) |type_id| {
            var_type_id = type_id;
        } else {
            var_type_id = expression_type_id;
            const var_type_ptr = self.type_table.getTypePtrById(var_type_id);
            if (var_type_ptr.kind == .array and self.type_table.getTypePtrById(var_type_ptr.kind.array).kind == .dynamic) {
                return self.err_reporter.cantInferArrayLiteralType(let_stmt.value.loc);
            }
        }

        const boxed_value = try self.coerceValue(expression_value, var_type_id, stmt.data.let.value.loc);

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
            return self.err_reporter.alreadyDefined(let_stmt.identifier, stmt.loc);
        };
        if (let_stmt.is_mut) {
            try self.ensureNotReadOnlySource(let_stmt.value);
        }

        return ir.Instruction{ .store_var = .{
            .uid = uid,
            .identifier = let_stmt.identifier,
            .type_id = var_type_id,
            .value = boxed_value,
        } };
    }

    fn visitFuncStmt(self: *Self, stmt: *const ast.StatementNode) !?ir.Instruction {
        const func_stmt = stmt.data.func;
        if (func_stmt.is_extern) {
            return null;
        }
        const fn_symbol = self.scope.symbol_table.getOrError(func_stmt.identifier) catch {
            return self.err_reporter.notDefined(func_stmt.identifier, stmt.loc);
        };
        if (fn_symbol.kind == .generic_function) {
            try self.checkGenericFunction(self.generic_functions.items[fn_symbol.kind.generic_function]);
            return null;
        }
        const fn_metadata = self.type_table.getTypePtrById(fn_symbol.type_id).kind.function;
        const func = try self.visitFnDef(&func_stmt.def, fn_metadata.uid, fn_metadata, null, stmt.loc);
        try self.functions.append(self.allocator, func);
        return null;
    }

    fn checkGenericFunction(self: *Self, generic: GenericFunction) CompileError!void {
        const func_stmt = generic.func_stmt;
        const was_checking_opaque = self.is_checking_opaque;
        self.is_checking_opaque = true;
        defer self.is_checking_opaque = was_checking_opaque;
        const old_return_type = self.scope.return_type_id;
        try self.scope.enter(old_return_type);
        defer self.scope.exit(old_return_type);

        for (func_stmt.slots) |slot| {
            const slot_type_id = try self.addSlotType(slot);
            self.scope.symbol_table.put(.{
                .identifier = slot.identifier,
                .uid = self.scope.genUid(),
                .type_id = slot_type_id,
                .kind = .{ .type_slot = {} },
            }) catch return self.err_reporter.alreadyDefined(slot.identifier, slot.loc);
        }

        for (func_stmt.def.params) |param| {
            const annotation = param.type_annotation orelse continue;
            if (param.default_value == null or !mentionsSlot(annotation, func_stmt.slots)) continue;
            return self.err_reporter.slotParamCantHaveDefault(param.identifier, param.default_value.?.loc);
        }

        const symbol = try self.createEmptyFuncSymbol(func_stmt.identifier);
        try self.fulfillFuncType(symbol, &func_stmt.def, null, generic.loc);
        const metadata = self.type_table.getTypePtrById(symbol.type_id).kind.function;
        _ = try self.visitFnDef(&func_stmt.def, metadata.uid, metadata, null, generic.loc);
    }

    fn callGenericFunction(
        self: *Self,
        index: usize,
        fn_call: ast.FnCall,
        expected_type: ?tt.TypeId,
        loc: err.Loc,
        inferred_values: *std.AutoHashMapUnmanaged(*const ast.ExpNode, *ir.Value),
    ) CompileError!Symbol {
        const func_stmt = self.generic_functions.items[index].func_stmt;
        const bindings = try self.allocator.alloc(?tt.TypeId, func_stmt.slots.len);
        @memset(bindings, null);
        const could_bind_void_slots = self.can_bind_void_slots;
        self.can_bind_void_slots = self.generic_functions.items[index].extern_uid != null;
        defer self.can_bind_void_slots = could_bind_void_slots;

        var has_lambda_args = false;
        for (func_stmt.def.params, 0..) |param, param_index| {
            const annotation = param.type_annotation orelse continue;
            if (!mentionsSlot(annotation, func_stmt.slots) or areMentionedSlotsBound(annotation, func_stmt.slots, bindings)) continue;
            const arg_exp = argumentFor(fn_call, param_index, param.identifier) orelse continue;
            if (arg_exp.data == .lambda) {
                has_lambda_args = true;
                continue;
            }
            const value = try self.evalExpExpecting(arg_exp, null);
            try inferred_values.put(self.allocator, arg_exp, value);
            try self.inferSlots(annotation, value.type_id, func_stmt.slots, bindings, arg_exp.loc);
        }
        if (has_lambda_args) {
            for (func_stmt.def.params, 0..) |param, param_index| {
                const annotation = param.type_annotation orelse continue;
                const arg_exp = argumentFor(fn_call, param_index, param.identifier) orelse continue;
                try self.inferSlotsFromLambda(annotation, arg_exp, func_stmt.slots, bindings, inferred_values);
            }
        }
        try self.inferSlotsFromExpectedType(func_stmt, expected_type, bindings, loc);

        const args = try self.allocator.alloc(tt.TypeId, bindings.len);
        for (bindings, func_stmt.slots, 0..) |binding, slot, slot_index| {
            args[slot_index] = binding orelse return self.err_reporter.cantInferSlot(slot.identifier, func_stmt.identifier, loc);
        }
        return self.instantiateGenericFunction(index, args, loc);
    }

    fn inferSlotsFromLambda(
        self: *Self,
        annotation: *const ast.TypeAnnotation,
        arg_exp: *const ast.ExpNode,
        slots: []const ast.Slot,
        bindings: []?tt.TypeId,
        inferred_values: *std.AutoHashMapUnmanaged(*const ast.ExpNode, *ir.Value),
    ) CompileError!void {
        if (arg_exp.data != .lambda) return;
        if (areMentionedSlotsBound(annotation, slots, bindings)) return;
        const hints = try self.lambdaHints(annotation, slots, bindings, arg_exp.loc);
        const value = try self.evalLambda(arg_exp, hints);
        try self.exp_types.put(self.allocator, arg_exp, value.type_id);
        try inferred_values.put(self.allocator, arg_exp, value);
        try self.inferSlots(annotation, value.type_id, slots, bindings, arg_exp.loc);
    }

    fn lambdaHints(self: *Self, annotation: *const ast.TypeAnnotation, slots: []const ast.Slot, bindings: []const ?tt.TypeId, loc: err.Loc) CompileError!?LambdaHints {
        if (annotation.type != .function) {
            return null;
        }
        const func = annotation.type.function;
        const hints = try self.allocator.alloc(?tt.FuncTypeParam, func.params.len);
        const saved_table = try self.enterBoundSlotScope(slots, bindings);
        defer self.scope.symbol_table = saved_table;
        for (func.params, hints) |param, *hint| {
            hint.* = null;
            if (!areMentionedSlotsBound(param.type, slots, bindings)) continue;
            hint.* = .{ .type_id = try self.resolveTypeAnnotation(param.type, null), .is_mut = param.is_mut };
        }
        const is_return_known = isErrorSetBound(func.error_set, slots, bindings) and areMentionedSlotsBound(func.return_type, slots, bindings);
        const return_type_id = if (is_return_known) try self.resolveFuncReturnType(func, null, loc) else null;
        return .{ .params = hints, .return_type_id = return_type_id };
    }

    fn enterBoundSlotScope(self: *Self, slots: []const ast.Slot, bindings: []const ?tt.TypeId) CompileError!*SymbolTable {
        const saved_table = self.scope.symbol_table;
        self.scope.symbol_table = self.module.root;
        try self.scope.enter(self.scope.return_type_id);
        for (slots, bindings) |slot, binding| {
            const type_id = binding orelse continue;
            try self.scope.symbol_table.put(.{
                .identifier = slot.identifier,
                .uid = self.scope.genUid(),
                .type_id = type_id,
                .kind = .{ .type_slot = {} },
            });
        }
        return saved_table;
    }

    fn inferSlotsFromExpectedType(self: *Self, func_stmt: *const ast.FuncStmt, expected_type: ?tt.TypeId, bindings: []?tt.TypeId, loc: err.Loc) CompileError!void {
        const expected = expected_type orelse return;
        const return_annotation = func_stmt.def.return_type orelse return;
        if (func_stmt.def.error_set != null) return;
        for (bindings) |binding| {
            if (binding == null) break;
        } else return;

        const from_expected = try self.allocator.alloc(?tt.TypeId, bindings.len);
        @memset(from_expected, null);
        try self.inferSlots(return_annotation, expected, func_stmt.slots, from_expected, loc);
        for (bindings, from_expected) |*binding, expected_binding| {
            if (binding.* == null) binding.* = expected_binding;
        }
    }

    fn canConvertTo(self: *Self, from: tt.TypeId, to: tt.TypeId) bool {
        if (self.type_table.canCoerce(from, to)) return true;
        const from_base = self.type_table.typeIdByNullableTypeId.get(from) orelse from;
        const to_base = self.type_table.typeIdByNullableTypeId.get(to) orelse to;
        const is_widening = from_base == self.type_table.getPrimitive(.int) and to_base == self.type_table.getPrimitive(.float);
        return is_widening and (from_base == from or to_base != to);
    }

    fn argumentFor(fn_call: ast.FnCall, param_index: usize, param_name: []const u8) ?*ast.ExpNode {
        if (!fn_call.are_arguments_named) {
            if (param_index >= fn_call.arguments.len) return null;
            return fn_call.arguments[param_index].exp;
        }
        for (fn_call.arguments) |arg| {
            const name = arg.identifier orelse continue;
            if (std.mem.eql(u8, name, param_name)) return arg.exp;
        }
        return null;
    }

    fn inferSlots(self: *Self, annotation: *const ast.TypeAnnotation, arg_type: tt.TypeId, slots: []const ast.Slot, bindings: []?tt.TypeId, loc: err.Loc) CompileError!void {
        var type_id = arg_type;
        if (annotation.is_nullable) {
            if (self.type_table.getTypePtrById(type_id).kind == .null) return;
            type_id = self.type_table.typeIdByNullableTypeId.get(type_id) orelse type_id;
        }
        const kind = self.type_table.getTypePtrById(type_id).kind;
        switch (annotation.type) {
            .@"struct" => |name| {
                const is_task_pair = kind == .task and std.mem.eql(u8, name, task_type_name) and annotation.args.len > 0;
                if (is_task_pair) {
                    try self.inferSlots(annotation.args[0], kind.task.payload, slots, bindings, loc);
                    if (annotation.args.len > 1) {
                        try self.inferSlots(annotation.args[1], kind.task.error_set, slots, bindings, loc);
                    }
                    return;
                }
                const slot_index = for (slots, 0..) |slot, i| {
                    if (std.mem.eql(u8, slot.identifier, name)) {
                        break i;
                    }
                } else {
                    const copy = self.copied_types.get(type_id) orelse return;
                    const generic = self.generic_types.items[copy.generic];
                    if (!namesDeclaration(name, generic.identifier) or annotation.args.len != copy.args.len) return;
                    for (annotation.args, copy.args) |arg_annotation, copied_arg| {
                        try self.inferSlots(arg_annotation, copied_arg, slots, bindings, loc);
                    }
                    return;
                };
                try self.bindSlot(slots, bindings, slot_index, type_id, loc);
            },
            .array => |inner| {
                if (kind == .dynamic or kind == .null) return;
                if (kind != .array) {
                    return self.err_reporter.invalidType("an array", try self.type_table.name(arg_type, self.allocator), loc);
                }
                try self.inferSlots(inner, kind.array, slots, bindings, loc);
            },
            .function => |func| {
                const is_same_shape = kind == .func_type and kind.func_type.params.len == func.params.len;
                if (!is_same_shape) return;
                for (func.params, kind.func_type.params) |param, arg_param| {
                    try self.inferSlots(param.type, arg_param.type_id, slots, bindings, loc);
                }
                const return_type = self.type_table.getTypePtrById(kind.func_type.return_type_id);
                const is_arg_failing = return_type.kind == .error_union;
                if (slotIndex(func.error_set, slots)) |set_slot| {
                    const arg_set = if (is_arg_failing) return_type.kind.error_union.error_set else self.none_error_set;
                    try self.bindSlot(slots, bindings, set_slot, arg_set, loc);
                }
                const are_both_failing = func.error_set != null and is_arg_failing;
                const arg_return = if (are_both_failing) return_type.kind.error_union.payload else kind.func_type.return_type_id;
                try self.inferSlots(func.return_type, arg_return, slots, bindings, loc);
            },
            else => {},
        }
    }

    fn namesDeclaration(name: []const u8, identifier: []const u8) bool {
        if (std.mem.eql(u8, name, identifier)) return true;
        const separator = std.mem.lastIndexOf(u8, name, "::") orelse return false;
        return std.mem.eql(u8, name[separator + 2 ..], identifier);
    }

    fn bindSlot(self: *Self, slots: []const ast.Slot, bindings: []?tt.TypeId, slot_index: usize, type_id: tt.TypeId, loc: err.Loc) CompileError!void {
        const slot = slots[slot_index];
        const kind = self.type_table.getTypePtrById(type_id).kind;
        if (kind == .dynamic or kind == .null) return;
        if (kind == .void and !self.can_bind_void_slots) {
            return self.err_reporter.invalidSlotType(slot.identifier, try self.type_table.name(type_id, self.allocator), loc);
        }
        if (kind == .error_union) {
            return self.err_reporter.slotCantFail(slot.identifier, try self.type_table.syntaxName(type_id, self.allocator), loc);
        }
        const bound = bindings[slot_index] orelse {
            try self.checkSlotFits(slot, type_id, loc);
            bindings[slot_index] = type_id;
            return;
        };
        if (bound == type_id) return;
        if (!slot.is_error_set and self.canConvertTo(type_id, bound)) return;
        return self.err_reporter.conflictingSlotTypes(
            slot.identifier,
            try self.type_table.syntaxName(bound, self.allocator),
            try self.type_table.syntaxName(type_id, self.allocator),
            loc,
        );
    }

    fn slotIndex(name: ?[]const u8, slots: []const ast.Slot) ?usize {
        const identifier = name orelse return null;
        for (slots, 0..) |slot, i| {
            if (std.mem.eql(u8, slot.identifier, identifier)) return i;
        }
        return null;
    }

    fn instantiateGenericFunction(self: *Self, index: usize, args: []const tt.TypeId, loc: err.Loc) CompileError!Symbol {
        const generic = &self.generic_functions.items[index];
        for (generic.copies.items) |copy| {
            if (std.mem.eql(tt.TypeId, copy.args, args)) return copy.symbol;
        }

        for (generic.func_stmt.slots, args) |slot, arg| try self.checkSlotFits(slot, arg, loc);
        if (self.copy_depth >= max_copy_depth) {
            self.err_reporter.copy_context = null;
            return self.err_reporter.genericCopyTooDeep(generic.func_stmt.identifier, max_copy_depth, loc);
        }
        const name = try self.copyName(generic.func_stmt.identifier, args);

        const symbol = try self.createEmptyFuncSymbol(name);
        if (generic.extern_uid == null) {
            try self.qualifyFunctionName(symbol, generic.module);
        }
        try generic.copies.append(self.allocator, .{ .args = args, .symbol = symbol, .call_loc = loc, .call_src = self.src, .depth = self.copy_depth + 1 });
        const copy_index = generic.copies.items.len - 1;

        const saved_module = self.enterModuleContext(generic.module);
        defer self.exitModuleContext(saved_module);
        const saved_table = try self.enterSlotScope(generic.func_stmt.slots, args);
        defer self.scope.symbol_table = saved_table;
        const saved_return_type = self.scope.return_type_id;
        defer self.scope.return_type_id = saved_return_type;
        try self.fulfillFuncType(symbol, &generic.func_stmt.def, null, generic.loc);

        if (generic.extern_uid) |extern_uid| {
            self.type_table.getTypePtrById(symbol.type_id).kind.function.uid = extern_uid;
            return symbol;
        }
        if (!self.containsSlot(args)) try self.pending_copies.append(self.allocator, .{ .kind = .function, .generic = index, .copy = copy_index });
        return symbol;
    }

    fn createOpaqueCopy(self: *Self, index: usize) CompileError!void {
        const generic = self.generic_types.items[index];
        const args = try self.allocator.alloc(tt.TypeId, generic.slots.len);
        for (generic.slots, 0..) |slot, i| {
            for (generic.slots[0..i]) |previous| {
                if (std.mem.eql(u8, previous.identifier, slot.identifier)) return self.err_reporter.alreadyDefined(slot.identifier, slot.loc);
            }
            args[i] = try self.addSlotType(slot);
        }
        const symbol = try self.instantiateGenericType(index, args, generic.loc);
        self.generic_types.items[index].opaque_copy = symbol;
    }

    fn checkGenericType(self: *Self, index: usize) CompileError!void {
        const generic = self.generic_types.items[index];
        const opaque_copy = generic.opaque_copy orelse unreachable;
        const was_checking_opaque = self.is_checking_opaque;
        self.is_checking_opaque = true;
        defer self.is_checking_opaque = was_checking_opaque;
        for (generic.copies.items, 0..) |copy, copy_index| {
            if (copy.symbol.uid != opaque_copy.uid) continue;
            _ = try self.visitTypeCopy(index, copy_index);
            return;
        }
    }

    fn resolveGenericTypeAnnotation(self: *Self, index: usize, annotation: *ast.TypeAnnotation, current_defined_type_type_id: ?tt.TypeId) CompileError!tt.TypeId {
        const generic = self.generic_types.items[index];
        if (annotation.args.len != generic.slots.len) {
            return self.err_reporter.wrongSlotCount(generic.identifier, generic.slots.len, annotation.args.len, annotation.loc);
        }
        const args = try self.allocator.alloc(tt.TypeId, annotation.args.len);
        for (annotation.args, generic.slots, 0..) |arg, slot, i| {
            args[i] = try self.resolveTypeAnnotation(arg, current_defined_type_type_id);
            const kind = self.type_table.getTypePtrById(args[i]).kind;
            if (kind == .void or kind == .null) {
                return self.err_reporter.invalidSlotType(slot.identifier, try self.type_table.name(args[i], self.allocator), annotation.loc);
            }
        }
        const symbol = try self.instantiateGenericType(index, args, annotation.loc);
        return symbol.kind.defined_type.blueprint_type_id;
    }

    fn constructGenericType(
        self: *Self,
        index: usize,
        fn_call: ast.FnCall,
        expected_type: ?tt.TypeId,
        loc: err.Loc,
        inferred_values: *std.AutoHashMapUnmanaged(*const ast.ExpNode, *ir.Value),
    ) CompileError!Symbol {
        const generic = self.generic_types.items[index];
        const bindings = try self.allocator.alloc(?tt.TypeId, generic.slots.len);
        @memset(bindings, null);

        switch (generic.def) {
            .@"struct" => |struct_def| {
                var has_lambda_args = false;
                for (struct_def.fields, 0..) |field, field_index| {
                    const annotation = field.type orelse continue;
                    if (!mentionsSlot(annotation, generic.slots) or areMentionedSlotsBound(annotation, generic.slots, bindings)) continue;
                    const arg_exp = argumentFor(fn_call, field_index, field.identifier) orelse continue;
                    if (arg_exp.data == .lambda) {
                        has_lambda_args = true;
                        continue;
                    }
                    const value = try self.evalExpExpecting(arg_exp, null);
                    try inferred_values.put(self.allocator, arg_exp, value);
                    try self.inferSlots(annotation, value.type_id, generic.slots, bindings, arg_exp.loc);
                }
                if (has_lambda_args) {
                    for (struct_def.fields, 0..) |field, field_index| {
                        const annotation = field.type orelse continue;
                        const arg_exp = argumentFor(fn_call, field_index, field.identifier) orelse continue;
                        try self.inferSlotsFromLambda(annotation, arg_exp, generic.slots, bindings, inferred_values);
                    }
                }
            },
            .@"union" => |union_def| {
                for (fn_call.arguments) |arg| {
                    const name = arg.identifier orelse continue;
                    for (union_def.variants) |variant| {
                        if (!std.mem.eql(u8, variant.identifier, name) or !mentionsSlot(variant.type, generic.slots)) continue;
                        const value = try self.evalExpExpecting(arg.exp, null);
                        try self.inferSlots(variant.type, value.type_id, generic.slots, bindings, arg.exp.loc);
                    }
                }
            },
        }

        if (expected_type) |expected| {
            const base_id = self.type_table.typeIdByNullableTypeId.get(expected) orelse expected;
            if (self.copied_types.get(base_id)) |copy| {
                if (copy.generic == index) {
                    for (bindings, copy.args) |*binding, arg| {
                        if (binding.* == null) {
                            binding.* = arg;
                        }
                    }
                }
            }
        }

        const args = try self.allocator.alloc(tt.TypeId, bindings.len);
        for (bindings, generic.slots, 0..) |binding, slot, slot_index| {
            args[slot_index] = binding orelse return self.err_reporter.cantInferSlot(slot.identifier, generic.identifier, loc);
        }
        return self.instantiateGenericType(index, args, loc);
    }

    fn instantiateGenericType(self: *Self, index: usize, args: []const tt.TypeId, loc: err.Loc) CompileError!Symbol {
        const generic = &self.generic_types.items[index];
        for (generic.copies.items) |copy| {
            if (std.mem.eql(tt.TypeId, copy.args, args)) return copy.symbol;
        }
        for (generic.slots, args) |slot, arg| try self.checkSlotFits(slot, arg, loc);
        if (self.copy_depth >= max_copy_depth) {
            self.err_reporter.copy_context = null;
            return self.err_reporter.genericCopyTooDeep(generic.identifier, max_copy_depth, loc);
        }

        const name = try self.copyName(generic.identifier, args);
        const symbol = switch (generic.def) {
            .@"struct" => try self.createEmptyStructSymbol(name),
            .@"union" => try self.createEmptyUnionSymbol(name),
        };
        try self.recordTypeModule(symbol, generic.module);
        try generic.copies.append(self.allocator, .{ .args = args, .symbol = symbol, .call_loc = loc, .call_src = self.src, .depth = self.copy_depth + 1 });
        const copy_index = generic.copies.items.len - 1;
        try self.copied_types.put(self.allocator, symbol.kind.defined_type.blueprint_type_id, .{ .generic = index, .args = args });

        const saved_table = try self.enterSlotScope(generic.slots, args);
        defer self.scope.symbol_table = saved_table;
        const saved_return_type = self.scope.return_type_id;
        defer self.scope.return_type_id = saved_return_type;
        const saved_depth = self.copy_depth;
        self.copy_depth += 1;
        defer self.copy_depth = saved_depth;
        switch (generic.def) {
            .@"struct" => |struct_def| try self.fulfillStructType(symbol, struct_def),
            .@"union" => |union_def| try self.fulfillUnionType(symbol, union_def, generic.loc),
        }

        if (!self.containsSlot(args)) try self.pending_copies.append(self.allocator, .{ .kind = .type, .generic = index, .copy = copy_index });
        return symbol;
    }

    fn visitTypeCopy(self: *Self, index: usize, copy_index: usize) CompileError!ir.DefinedType {
        const generic = self.generic_types.items[index];
        const copy = generic.copies.items[copy_index];
        const saved_module = self.enterModuleContext(generic.module);
        defer self.exitModuleContext(saved_module);
        const saved_table = try self.enterSlotScope(generic.slots, copy.args);
        defer self.scope.symbol_table = saved_table;
        return switch (generic.def) {
            .@"struct" => |struct_def| .{ .kind = .{ .@"struct" = try self.visitStructDef(struct_def, copy.symbol) } },
            .@"union" => |union_def| try self.visitUnionDef(union_def, copy.symbol),
        };
    }

    fn lowerPendingCopies(self: *Self) CompileError!void {
        var next: usize = 0;
        while (next < self.pending_copies.items.len) : (next += 1) {
            const pending = self.pending_copies.items[next];
            if (pending.kind == .type) {
                try self.lowerTypeCopy(pending);
                continue;
            }
            const generic = self.generic_functions.items[pending.generic];
            const copy = generic.copies.items[pending.copy];

            const saved_lowering = self.enterCopyLowering(copy, generic.module);
            defer self.exitCopyLowering(saved_lowering);
            const saved_table = try self.enterSlotScope(generic.func_stmt.slots, copy.args);
            defer self.scope.symbol_table = saved_table;

            const metadata = self.type_table.getTypePtrById(copy.symbol.type_id).kind.function;
            const func = try self.visitFnDef(&generic.func_stmt.def, metadata.uid, metadata, null, generic.loc);
            try self.functions.append(self.allocator, func);
        }
        self.pending_copies.clearRetainingCapacity();
    }

    fn lowerTypeCopy(self: *Self, pending: PendingCopy) CompileError!void {
        const generic = self.generic_types.items[pending.generic];
        const copy = generic.copies.items[pending.copy];
        const saved_lowering = self.enterCopyLowering(copy, generic.module);
        defer self.exitCopyLowering(saved_lowering);
        try self.defined_types.append(self.allocator, try self.visitTypeCopy(pending.generic, pending.copy));
    }

    fn enterCopyLowering(self: *Self, copy: GenericCopy, module: *ModuleScope) CopyLowering {
        const saved = CopyLowering{
            .module = self.enterModuleContext(module),
            .depth = self.copy_depth,
            .copy_context = self.err_reporter.copy_context,
            .is_lowering_copy = self.is_lowering_copy,
            .has_none_error_slot = self.has_none_error_slot,
        };
        self.copy_depth = copy.depth;
        self.has_none_error_slot = std.mem.indexOfScalar(tt.TypeId, copy.args, self.none_error_set) != null;
        self.err_reporter.copy_context = .{ .name = copy.symbol.identifier, .call_loc = copy.call_loc, .call_src = copy.call_src };
        self.is_lowering_copy = true;
        return saved;
    }

    fn exitCopyLowering(self: *Self, saved: CopyLowering) void {
        self.exitModuleContext(saved.module);
        self.copy_depth = saved.depth;
        self.err_reporter.copy_context = saved.copy_context;
        self.is_lowering_copy = saved.is_lowering_copy;
        self.has_none_error_slot = saved.has_none_error_slot;
    }

    fn enterSlotScope(self: *Self, slots: []const ast.Slot, args: []const tt.TypeId) CompileError!*SymbolTable {
        const saved_table = self.scope.symbol_table;
        self.scope.symbol_table = self.module.root;
        try self.scope.enter(self.scope.return_type_id);
        for (slots, args) |slot, arg| {
            try self.scope.symbol_table.put(.{
                .identifier = slot.identifier,
                .uid = self.scope.genUid(),
                .type_id = arg,
                .kind = .{ .type_slot = {} },
            });
        }
        return saved_table;
    }

    fn addSlotType(self: *Self, slot: ast.Slot) CompileError!tt.TypeId {
        if (slot.is_error_set) {
            return self.createErrorSet(slot.identifier, .slot);
        }
        const constraint = try self.resolveConstraint(slot);
        const slot_type_id = try self.type_table.addType(.{ .kind = .{ .slot = .{ .identifier = slot.identifier, .type_list = constraint.type_list } }, .is_nullable = false });
        if (constraint.methods.len == 0) return slot_type_id;

        const methods = try self.allocator.alloc(tt.SlotMethod, constraint.methods.len);
        for (constraint.methods, methods) |method, *slot_method| {
            slot_method.* = .{ .identifier = method.func_stmt.identifier, .type_id = try self.traitMethodType(method, slot_type_id) };
        }
        self.type_table.getTypePtrById(slot_type_id).kind.slot.methods = methods;
        return slot_type_id;
    }

    fn resolveConstraint(self: *Self, slot: ast.Slot) CompileError!Constraint {
        const slot_module = self.slot_modules.get(@intFromPtr(slot.constraint.ptr)) orelse self.module;
        const saved_module = self.enterModuleContext(slot_module);
        defer self.exitModuleContext(saved_module);
        var constraint = Constraint{};
        var methods: std.ArrayList(TraitMethod) = .empty;
        var list_count: usize = 0;
        var trait_count: usize = 0;
        for (slot.constraint) |term| {
            if (term.len == 1) {
                if (self.methodTraitIndexOf(term[0])) |trait_index| {
                    const trait_constraint = try self.resolveTrait(trait_index);
                    try self.mergeTraitMethods(&methods, trait_constraint.methods, term[0].loc);
                    trait_count += 1;
                    continue;
                }
            }
            constraint.type_list = try self.resolveTypeList(term);
            list_count += 1;
        }
        if (list_count > 1 or (list_count > 0 and trait_count > 0)) return self.err_reporter.typeListWithTrait(slot.identifier, slot.loc);
        constraint.methods = try methods.toOwnedSlice(self.allocator);
        return constraint;
    }

    fn resolveTypeList(self: *Self, annotations: []const *ast.TypeAnnotation) CompileError![]const tt.TypeId {
        const saved_table = self.scope.symbol_table;
        self.scope.symbol_table = self.module.root;
        defer self.scope.symbol_table = saved_table;

        var members: std.ArrayList(tt.TypeId) = .empty;
        for (annotations) |annotation| {
            if (self.traitIndexOf(annotation)) |trait_index| {
                if (self.isMethodTrait(trait_index)) return self.err_reporter.typeListWithTrait(annotation.type.@"struct", annotation.loc);
                for ((try self.resolveTrait(trait_index)).type_list) |member| {
                    if (std.mem.indexOfScalar(tt.TypeId, members.items, member) == null) try members.append(self.allocator, member);
                }
                continue;
            }
            const member = try self.resolveTypeAnnotation(annotation, null);
            if (!self.isTypeListMember(member)) {
                return self.err_reporter.typeListNotPrimitive(try self.type_table.syntaxName(member, self.allocator), annotation.loc);
            }
            if (std.mem.indexOfScalar(tt.TypeId, members.items, member) == null) try members.append(self.allocator, member);
        }
        return members.toOwnedSlice(self.allocator);
    }

    fn traitIndexOf(self: *Self, annotation: *const ast.TypeAnnotation) ?usize {
        const is_plain_name = annotation.type == .@"struct" and !annotation.is_nullable and annotation.args.len == 0;
        if (!is_plain_name) {
            return null;
        }
        const symbol = self.module.root.get(annotation.type.@"struct") orelse return null;
        if (symbol.kind != .trait) return null;
        return symbol.kind.trait;
    }

    fn methodTraitIndexOf(self: *Self, annotation: *const ast.TypeAnnotation) ?usize {
        const trait_index = self.traitIndexOf(annotation) orelse return null;
        if (!self.isMethodTrait(trait_index)) return null;
        return trait_index;
    }

    fn isMethodTrait(self: *Self, index: usize) bool {
        return self.traits.items[index].trait_stmt.type_list.len == 0;
    }

    fn resolveTrait(self: *Self, index: usize) CompileError!Constraint {
        const trait = self.traits.items[index];
        if (trait.constraint) |constraint| return constraint;
        if (trait.is_resolving) return self.err_reporter.traitRefersToItself(trait.trait_stmt.identifier, trait.loc);
        self.traits.items[index].is_resolving = true;
        const saved_module = self.enterModuleContext(trait.module);
        defer self.exitModuleContext(saved_module);

        var constraint = Constraint{};
        if (!self.isMethodTrait(index)) {
            constraint.type_list = try self.resolveTypeList(trait.trait_stmt.type_list);
        } else {
            var methods: std.ArrayList(TraitMethod) = .empty;
            for (trait.trait_stmt.parents) |parent| {
                if (self.traitIndexOf(parent)) |parent_index| {
                    if (!self.isMethodTrait(parent_index)) return self.err_reporter.typeListWithTrait(trait.trait_stmt.identifier, parent.loc);
                    try self.mergeTraitMethods(&methods, (try self.resolveTrait(parent_index)).methods, parent.loc);
                    continue;
                }
                return self.err_reporter.notATrait(try parent.value(self.allocator), parent.loc);
            }
            var own: std.ArrayList(TraitMethod) = .empty;
            for (trait.trait_stmt.methods) |*method_node| {
                const method = TraitMethod{ .func_stmt = &method_node.data.func, .loc = method_node.loc, .module = trait.module };
                try self.checkTraitMethod(method, own.items);
                try own.append(self.allocator, method);
            }
            try self.mergeTraitMethods(&methods, own.items, trait.loc);
            constraint.methods = try methods.toOwnedSlice(self.allocator);
        }

        self.traits.items[index].constraint = constraint;
        self.traits.items[index].is_resolving = false;
        return constraint;
    }

    fn checkTraitMethod(self: *Self, method: TraitMethod, previous: []const TraitMethod) CompileError!void {
        const def = method.func_stmt.def;
        for (previous) |other| {
            if (std.mem.eql(u8, other.func_stmt.identifier, method.func_stmt.identifier)) return self.err_reporter.alreadyDefined(method.func_stmt.identifier, method.loc);
        }
        const takes_self = def.params.len > 0 and def.params[0].type_annotation != null and def.params[0].type_annotation.?.type == .type_self and !def.params[0].type_annotation.?.is_nullable;
        if (!takes_self) return self.err_reporter.traitMethodNeedsSelf(method.func_stmt.identifier, method.loc);
        if (def.error_set) |error_set| {
            if (error_set == .inferred) return self.err_reporter.traitMethodNeedsErrorSet(method.func_stmt.identifier, method.loc);
        }
        _ = try self.traitMethodType(method, try self.traitSelfType());
    }

    fn traitSelfType(self: *Self) CompileError!tt.TypeId {
        if (self.trait_self_type) |type_id| return type_id;
        const type_id = try self.type_table.addType(.{ .kind = .{ .slot = .{ .identifier = "@" } }, .is_nullable = false });
        self.trait_self_type = type_id;
        return type_id;
    }

    fn mergeTraitMethods(self: *Self, methods: *std.ArrayList(TraitMethod), added: []const TraitMethod, loc: err.Loc) CompileError!void {
        for (added) |method| {
            const existing = for (methods.items) |other| {
                if (std.mem.eql(u8, other.func_stmt.identifier, method.func_stmt.identifier)) break other;
            } else {
                try methods.append(self.allocator, method);
                continue;
            };
            if (existing.func_stmt == method.func_stmt) continue;
            const self_type = try self.traitSelfType();
            const existing_func = self.type_table.getTypePtrById(try self.traitMethodType(existing, self_type)).kind.function;
            const method_func = self.type_table.getTypePtrById(try self.traitMethodType(method, self_type)).kind.function;
            if (self.isSameSignature(existing_func, method_func)) continue;
            return self.err_reporter.clashingTraitMethods(method.func_stmt.identifier, loc);
        }
    }

    fn traitMethodType(self: *Self, method: TraitMethod, self_type: tt.TypeId) CompileError!tt.TypeId {
        const saved_module = self.enterModuleContext(method.module);
        defer self.exitModuleContext(saved_module);
        const saved_table = self.scope.symbol_table;
        self.scope.symbol_table = self.module.root;
        defer self.scope.symbol_table = saved_table;

        const saved_return_type = self.scope.return_type_id;
        defer self.scope.return_type_id = saved_return_type;

        const symbol = try self.createEmptyFuncSymbol(method.func_stmt.identifier);
        const owner = Symbol{ .identifier = "@", .uid = 0, .type_id = self_type, .kind = .{ .defined_type = .{ .blueprint_type_id = self_type } } };
        try self.fulfillFuncType(symbol, &method.func_stmt.def, owner, method.loc);
        return symbol.type_id;
    }

    fn isSameSignature(self: *Self, a: tt.Func, b: tt.Func) bool {
        if (a.params.len != b.params.len) return false;
        for (a.params, b.params) |a_param, b_param| {
            if (a_param.is_mut != b_param.is_mut or !self.isSameType(a_param.type_id, b_param.type_id)) return false;
        }
        return self.isSameType(a.return_type_id, b.return_type_id);
    }

    fn isSameType(self: *Self, a: tt.TypeId, b: tt.TypeId) bool {
        if (a == b) return true;
        const a_kind = self.type_table.getTypePtrById(a).kind;
        const b_kind = self.type_table.getTypePtrById(b).kind;
        if (a_kind != .error_union or b_kind != .error_union) return false;
        return a_kind.error_union.error_set == b_kind.error_union.error_set and self.isSameType(a_kind.error_union.payload, b_kind.error_union.payload);
    }

    fn isTypeListMember(self: *Self, type_id: tt.TypeId) bool {
        const type_ptr = self.type_table.getTypePtrById(type_id);
        if (type_ptr.is_nullable) return false;
        return switch (type_ptr.kind) {
            .int, .float, .byte, .string, .boolean => true,
            else => false,
        };
    }

    fn checkSlotFits(self: *Self, slot: ast.Slot, type_id: tt.TypeId, loc: err.Loc) CompileError!void {
        const is_error_set = self.type_table.isErrorSet(type_id) and !self.type_table.getTypePtrById(type_id).is_nullable;
        if (slot.is_error_set and !is_error_set) {
            return self.err_reporter.slotNeedsErrorSet(slot.identifier, try self.type_table.syntaxName(type_id, self.allocator), loc);
        }
        if (!slot.is_error_set and type_id == self.none_error_set) {
            return self.err_reporter.invalidSlotType(slot.identifier, none_error_set_name, loc);
        }
        if (slot.constraint.len == 0) return;
        const constraint = try self.resolveConstraint(slot);
        if (constraint.type_list.len > 0) {
            if (self.fitsTypeList(type_id, constraint.type_list)) return;
            return self.err_reporter.typeDoesntFitSlot(try self.fitTypeText(type_id), try self.constraintText(slot), loc);
        }
        if (constraint.methods.len == 0) return;
        if (self.is_hoisting) {
            try self.pending_trait_checks.append(self.allocator, .{ .slot = slot, .type_id = type_id, .loc = loc });
            return;
        }
        for (constraint.methods) |method| try self.checkMethodFits(slot, method, type_id, loc);
    }

    fn checkMethodFits(self: *Self, slot: ast.Slot, method: TraitMethod, type_id: tt.TypeId, loc: err.Loc) CompileError!void {
        const name = method.func_stmt.identifier;
        const required = self.type_table.getTypePtrById(try self.traitMethodType(method, type_id)).kind.function;
        const actual_type_id = self.methodOf(type_id, name) orelse {
            const reason = try allocPrint(self.allocator, "it has no method '{s}'", .{name});
            return self.err_reporter.doesntFitTrait(try self.fitTypeText(type_id), try self.constraintText(slot), reason, loc);
        };
        const is_hidden_method = self.file_only_methods.contains(actual_type_id) and self.isOutsideTypeModule(type_id);
        if (is_hidden_method) {
            const reason = try allocPrint(self.allocator, "its '{s}' is file-only", .{name});
            return self.err_reporter.doesntFitTrait(try self.fitTypeText(type_id), try self.constraintText(slot), reason, loc);
        }
        const actual = self.type_table.getTypePtrById(actual_type_id).kind.function;
        if (self.doesMethodFit(required, actual)) return;
        const reason = try allocPrint(self.allocator, "its '{s}' is {s}, the trait wants {s}", .{ name, try self.signatureText(actual), try self.signatureText(required) });
        return self.err_reporter.doesntFitTrait(try self.fitTypeText(type_id), try self.constraintText(slot), reason, loc);
    }

    fn doesMethodFit(self: *Self, required: tt.Func, actual: tt.Func) bool {
        if (required.params.len != actual.params.len) return false;
        for (required.params, actual.params, 0..) |required_param, actual_param, i| {
            const is_less_demanding_self = i == 0 and required_param.is_mut and !actual_param.is_mut;
            if (required_param.is_mut != actual_param.is_mut and !is_less_demanding_self) return false;
            if (!self.isSameType(required_param.type_id, actual_param.type_id)) return false;
        }
        return self.isSameType(required.return_type_id, actual.return_type_id);
    }

    fn methodOf(self: *Self, type_id: tt.TypeId, name: []const u8) ?tt.TypeId {
        return switch (self.type_table.getTypePtrById(type_id).kind) {
            .@"struct" => |s| s.methods.get(name),
            .@"enum" => |e| e.methods.get(name),
            .@"union" => |u| u.methods.get(name),
            .slot => |slot| for (slot.methods) |method| {
                if (std.mem.eql(u8, method.identifier, name)) break method.type_id;
            } else null,
            else => null,
        };
    }

    fn signatureText(self: *Self, func: tt.Func) CompileError![]const u8 {
        var text: std.ArrayList(u8) = .empty;
        try text.append(self.allocator, '(');
        for (func.params, 0..) |param, i| {
            if (i > 0) try text.appendSlice(self.allocator, ", ");
            if (param.is_mut) try text.appendSlice(self.allocator, "mut ");
            try text.print(self.allocator, "{s}: {s}", .{ param.identifier, try self.type_table.syntaxName(param.type_id, self.allocator) });
        }
        try text.print(self.allocator, ") -> {s}", .{try self.type_table.syntaxName(func.return_type_id, self.allocator)});
        return text.toOwnedSlice(self.allocator);
    }

    fn constraintText(self: *Self, slot: ast.Slot) CompileError![]const u8 {
        var text: std.ArrayList(u8) = .empty;
        try text.print(self.allocator, "{s}: ", .{slot.identifier});
        for (slot.constraint, 0..) |term, term_index| {
            if (term_index > 0) try text.appendSlice(self.allocator, " + ");
            for (term, 0..) |annotation, i| {
                if (i > 0) try text.appendSlice(self.allocator, " | ");
                try text.appendSlice(self.allocator, try annotation.value(self.allocator));
            }
        }
        return text.toOwnedSlice(self.allocator);
    }

    fn fitTypeText(self: *Self, type_id: tt.TypeId) CompileError![]const u8 {
        const name = try self.type_table.syntaxName(type_id, self.allocator);
        const kind = self.type_table.getTypePtrById(type_id).kind;
        if (kind != .slot or kind.slot.type_list.len == 0) return std.fmt.allocPrint(self.allocator, "'{s}'", .{name});

        var text: std.ArrayList(u8) = .empty;
        try text.print(self.allocator, "'{s}' (", .{name});
        for (kind.slot.type_list, 0..) |member, i| {
            if (i > 0) try text.appendSlice(self.allocator, " | ");
            try text.appendSlice(self.allocator, try self.type_table.syntaxName(member, self.allocator));
        }
        try text.append(self.allocator, ')');
        return text.toOwnedSlice(self.allocator);
    }

    fn fitsTypeList(self: *Self, type_id: tt.TypeId, type_list: []const tt.TypeId) bool {
        const kind = self.type_table.getTypePtrById(type_id).kind;
        if (kind != .slot) return std.mem.indexOfScalar(tt.TypeId, type_list, type_id) != null;
        if (kind.slot.type_list.len == 0) return false;
        for (kind.slot.type_list) |member| {
            if (std.mem.indexOfScalar(tt.TypeId, type_list, member) == null) return false;
        }
        return true;
    }

    fn copyName(self: *Self, identifier: []const u8, args: []const tt.TypeId) CompileError![]const u8 {
        var name: std.ArrayList(u8) = .empty;
        try name.appendSlice(self.allocator, identifier);
        try name.append(self.allocator, '[');
        for (args, 0..) |arg, i| {
            if (i > 0) try name.appendSlice(self.allocator, ", ");
            try name.appendSlice(self.allocator, try self.type_table.syntaxName(arg, self.allocator));
        }
        try name.append(self.allocator, ']');
        return name.toOwnedSlice(self.allocator);
    }

    fn containsSlot(self: *Self, args: []const tt.TypeId) bool {
        for (args) |arg| {
            if (self.typeContainsSlot(arg)) return true;
        }
        return false;
    }

    fn typeContainsSlot(self: *Self, type_id: tt.TypeId) bool {
        const base_id = self.type_table.typeIdByNullableTypeId.get(type_id) orelse type_id;
        return switch (self.type_table.getTypePtrById(base_id).kind) {
            .slot => true,
            .@"enum" => |e| e.error_set != null and e.error_set.?.kind == .slot,
            .array => |inner| self.typeContainsSlot(inner),
            .error_union => |eu| self.typeContainsSlot(eu.error_set) or self.typeContainsSlot(eu.payload),
            .task => |task| self.typeContainsSlot(task.payload) or self.typeContainsSlot(task.error_set),
            .func_type => |func| for (func.params) |param| {
                if (self.typeContainsSlot(param.type_id)) break true;
            } else self.typeContainsSlot(func.return_type_id),
            else => false,
        };
    }

    fn areMentionedSlotsBound(annotation: *const ast.TypeAnnotation, slots: []const ast.Slot, bindings: []const ?tt.TypeId) bool {
        return switch (annotation.type) {
            .@"struct" => |name| for (slots, bindings) |slot, binding| {
                if (std.mem.eql(u8, slot.identifier, name)) break binding != null;
            } else for (annotation.args) |arg| {
                if (!areMentionedSlotsBound(arg, slots, bindings)) break false;
            } else true,
            .array => |inner| areMentionedSlotsBound(inner, slots, bindings),
            .function => |func| for (func.params) |param| {
                if (!areMentionedSlotsBound(param.type, slots, bindings)) break false;
            } else isErrorSetBound(func.error_set, slots, bindings) and areMentionedSlotsBound(func.return_type, slots, bindings),
            else => true,
        };
    }

    fn mentionsSlot(annotation: *const ast.TypeAnnotation, slots: []const ast.Slot) bool {
        return switch (annotation.type) {
            .@"struct" => |name| for (slots) |slot| {
                if (std.mem.eql(u8, slot.identifier, name)) break true;
            } else for (annotation.args) |arg| {
                if (mentionsSlot(arg, slots)) break true;
            } else false,
            .array => |inner| mentionsSlot(inner, slots),
            .function => |func| for (func.params) |param| {
                if (mentionsSlot(param.type, slots)) break true;
            } else slotIndex(func.error_set, slots) != null or mentionsSlot(func.return_type, slots),
            else => false,
        };
    }

    fn isErrorSetBound(error_set: ?[]const u8, slots: []const ast.Slot, bindings: []const ?tt.TypeId) bool {
        const slot_index = slotIndex(error_set, slots) orelse return true;
        return bindings[slot_index] != null;
    }

    fn visitStructStmt(self: *Self, stmt: *const ast.StatementNode) !?ir.Instruction {
        const struct_stmt = stmt.data.@"struct";
        const struct_symbol = try self.scope.symbol_table.getOrError(struct_stmt.identifier);
        if (struct_symbol.kind == .generic_type) {
            try self.checkGenericType(struct_symbol.kind.generic_type);
            return null;
        }
        const struct_def = try self.visitStructDef(&struct_stmt.def, struct_symbol);
        try self.defined_types.append(self.allocator, .{ .kind = .{ .@"struct" = struct_def } });
        return null;
    }

    fn visitEnumStmt(self: *Self, stmt: *const ast.StatementNode) !?ir.Instruction {
        const enum_stmt = stmt.data.@"enum";
        const enum_symbol = try self.scope.symbol_table.getOrError(enum_stmt.identifier);
        const blueprint_type_id = enum_symbol.kind.defined_type.blueprint_type_id;
        const enum_def = self.type_table.getTypePtrById(blueprint_type_id).kind.@"enum";
        const funcs = try self.lowerMethods(enum_stmt.def.funcs, enum_def.methods);

        try self.defined_types.append(self.allocator, .{
            .kind = .{
                .@"enum" = .{
                    .uid = enum_symbol.uid,
                    .type_id = blueprint_type_id,
                    .identifier = enum_stmt.identifier,
                    .variants = enum_def.variants,
                    .funcs = funcs,
                },
            },
        });
        return null;
    }

    fn visitUnionStmt(self: *Self, stmt: *const ast.StatementNode) !?ir.Instruction {
        const union_stmt = stmt.data.@"union";
        const union_symbol = try self.scope.symbol_table.getOrError(union_stmt.identifier);
        if (union_symbol.kind == .generic_type) {
            try self.checkGenericType(union_symbol.kind.generic_type);
            return null;
        }
        try self.defined_types.append(self.allocator, try self.visitUnionDef(&union_stmt.def, union_symbol));
        return null;
    }

    fn visitUnionDef(self: *Self, union_ast: *const ast.UnionDef, union_symbol: Symbol) CompileError!ir.DefinedType {
        const blueprint_type_id = union_symbol.kind.defined_type.blueprint_type_id;
        const union_def = self.type_table.getTypePtrById(blueprint_type_id).kind.@"union";
        const funcs = try self.lowerMethods(union_ast.funcs, union_def.methods);

        return .{
            .kind = .{
                .@"union" = .{
                    .uid = union_symbol.uid,
                    .type_id = blueprint_type_id,
                    .identifier = union_symbol.identifier,
                    .variants = union_def.variants,
                    .funcs = funcs,
                },
            },
        };
    }

    fn lowerMethods(self: *Self, funcs: []const ast.StatementNode, methods: std.StringHashMap(tt.TypeId)) CompileError![]ir.Func {
        const lowered = try self.allocator.alloc(ir.Func, funcs.len);
        for (funcs, lowered) |func_node, *func| {
            const func_stmt = func_node.data.func;
            const fn_metadata = self.type_table.getTypePtrById(methods.get(func_stmt.identifier).?).kind.function;
            func.* = try self.visitFnDef(&func_stmt.def, fn_metadata.uid, fn_metadata, func_stmt.identifier, func_node.loc);
        }
        return lowered;
    }

    fn lowerIf(self: *Self, exp: *const ast.ExpNode, result: ?*BranchResult) CompileError!ir.Instruction {
        const if_stmt = exp.data.if_exp;
        if (result != null and if_stmt.else_block == null) {
            return self.err_reporter.invalidExpression("else in an if expression", "if without else", exp.loc);
        }

        const condition_value = try self.evalExp(if_stmt.condition);
        const condition_value_type = condition_value.type_id;

        if (!self.type_table.eql(condition_value_type, self.type_table.getPrimitive(.boolean))) {
            return self.err_reporter.invalidType("boolean", try self.type_table.name(condition_value_type, self.allocator), if_stmt.condition.loc);
        }

        return ir.Instruction{ .branch_if = .{
            .condition = condition_value,
            .then_block = (try self.visitBranchBlock(&if_stmt.then_block, result)).block,
            .else_block = if (if_stmt.else_block) |else_blc| (try self.visitBranchBlock(&else_blc, result)).block else &.{},
        } };
    }

    fn lowerIfCapture(self: *Self, exp: *const ast.ExpNode, result: ?*BranchResult) CompileError![]ir.Instruction {
        const capture = exp.data.if_capture_exp;
        if (result != null and capture.else_block == null) {
            return self.err_reporter.invalidExpression("else in an if expression", "if without else", exp.loc);
        }
        const subject = try self.evalExp(capture.exp);
        if (!self.type_table.getTypePtrById(subject.type_id).is_nullable) {
            return self.err_reporter.invalidType("nullable", try self.type_table.name(subject.type_id, self.allocator), capture.exp.loc);
        }
        if (capture.identifier.is_mut and self.findImmutablePlace(capture.exp) != null) {
            return self.err_reporter.unwrappedValueCantBeMutable(capture.identifier.name, exp.loc);
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
        if (capture.identifier.is_mut and !self.canBeMutable(captured_type_id)) {
            return self.err_reporter.primitiveCaptureCantBeMutable(capture.identifier.name, exp.loc);
        }
        const captured_symbol = Symbol{
            .uid = self.scope.genUid(),
            .identifier = capture.identifier.name,
            .type_id = captured_type_id,
            .kind = .{ .variable = .{ .is_mut = capture.identifier.is_mut, .is_borrowed = true } },
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
            try then_instructions.appendSlice(self.allocator, (try self.visitBranchBlock(&capture.body, result)).block);
        }
        const else_instructions = if (capture.else_block) |else_block| (try self.visitBranchBlock(&else_block, result)).block else &.{};
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

    fn lowerWhen(self: *Self, exp: *const ast.ExpNode, result: ?*BranchResult) CompileError![]ir.Instruction {
        const when_stmt = exp.data.when_exp;
        const subject = try self.resolveWhenSubject(when_stmt.subject);
        const prongs = try self.checkWhenProngs(&when_stmt, subject);
        try self.checkWhenExhaustive(exp, subject, prongs);

        var instructions: std.ArrayList(ir.Instruction) = .empty;
        const refs = try self.storeWhenSubject(&instructions, subject);

        const unconditional_prong_idx = if (when_stmt.else_block == null) findUnconditionalProng(when_stmt.prongs) else null;
        var cases: std.ArrayList(ir.WhenCase) = .empty;
        const case_of_prong = try self.allocator.alloc(?usize, when_stmt.prongs.len);
        @memset(case_of_prong, null);
        var default_block: ir.Block = &.{};
        var null_body: ?ir.Block = null;

        for (when_stmt.prongs, 0..) |*prong, prong_idx| {
            const lowered = switch (prong.pattern) {
                .null_case => {
                    null_body = (try self.visitBranchBlock(&prong.body, result)).block;
                    continue;
                },
                .variant => try self.lowerWhenVariantProng(prong, subject, refs.unwrapped, result),
                .values => try self.lowerWhenValuesProng(prong, prongs.values[prong_idx], prongs.ranges[prong_idx], refs.unwrapped, result),
            };
            if (unconditional_prong_idx == prong_idx) {
                default_block = lowered.body;
                continue;
            }
            case_of_prong[prong_idx] = cases.items.len;
            try cases.append(self.allocator, .{ .condition = lowered.condition, .body = lowered.body });
        }
        if (when_stmt.else_block) |else_block| default_block = (try self.visitBranchBlock(&else_block, result)).block;

        const null_check: ?*ir.Value = if (subject.is_nullable) try ir.Value.init(self.allocator, .{
            .data = .{ .unary_op = .{ .kind = .box_not_null, .right = refs.subject } },
            .type_id = self.type_table.getPrimitive(.boolean),
        }) else null;

        try instructions.append(self.allocator, .{ .when = .{
            .null_check = null_check,
            .null_body = null_body,
            .cases = try cases.toOwnedSlice(self.allocator),
            .table = try self.buildJumpTable(when_stmt, subject, prongs, case_of_prong, refs.unwrapped),
            .default_block = default_block,
        } });
        return instructions.toOwnedSlice(self.allocator);
    }

    const min_jump_table_values = 4;
    const max_jump_table_span = 1024;

    fn buildJumpTable(self: *Self, when_stmt: ast.WhenStmt, subject: WhenSubject, prongs: WhenProngs, case_of_prong: []const ?usize, unwrapped_ref: *ir.Value) CompileError!?ir.JumpTable {
        if (subject.base_kind == .string or subject.base_kind == .float) return null;
        var entries: std.ArrayList(JumpTableEntry) = .empty;
        for (when_stmt.prongs, 0..) |prong, prong_idx| {
            const case_index = case_of_prong[prong_idx];
            switch (prong.pattern) {
                .null_case => {},
                .variant => |variant| try entries.append(self.allocator, .{
                    .value = @intCast(unionTagIndex(subject.base_kind.@"union", variant.name)),
                    .case_index = case_index,
                }),
                .values => for (prongs.values[prong_idx], prongs.ranges[prong_idx]) |value, range| {
                    const bounds = range orelse {
                        try entries.append(self.allocator, .{ .value = value.data.i_int, .case_index = case_index });
                        continue;
                    };
                    const start = bounds.start.data.i_int;
                    const count = @as(i128, bounds.end.data.i_int) - start + 1;
                    if (count > max_jump_table_span) return null;
                    for (0..@intCast(count)) |offset| {
                        try entries.append(self.allocator, .{ .value = start + @as(i64, @intCast(offset)), .case_index = case_index });
                    }
                },
            }
        }
        if (entries.items.len < min_jump_table_values) return null;

        var min = entries.items[0].value;
        var max = min;
        for (entries.items) |entry| {
            min = @min(min, entry.value);
            max = @max(max, entry.value);
        }
        const span = @as(i128, max) - min + 1;
        if (span > max_jump_table_span or span > 2 * entries.items.len) return null;

        const case_by_offset = try self.allocator.alloc(?usize, @intCast(span));
        @memset(case_by_offset, null);
        for (entries.items) |entry| case_by_offset[@intCast(entry.value - min)] = entry.case_index;

        const table_subject = if (subject.base_kind == .@"union") try ir.Value.init(self.allocator, .{
            .data = .{ .union_tag = unwrapped_ref },
            .type_id = self.type_table.getPrimitive(.int),
        }) else unwrapped_ref;
        return .{ .subject = table_subject, .min = min, .case_by_offset = case_by_offset };
    }

    fn resolveWhenSubject(self: *Self, exp: *const ast.ExpNode) CompileError!WhenSubject {
        const value = try self.evalExp(exp);
        const type_name = try self.type_table.name(value.type_id, self.allocator);
        const is_nullable = self.type_table.getTypePtrById(value.type_id).is_nullable;
        const base_type = self.type_table.typeIdByNullableTypeId.get(value.type_id) orelse value.type_id;
        const base_kind = self.type_table.getTypePtrById(base_type).kind;

        switch (base_kind) {
            .int, .byte, .float, .string, .@"enum", .@"union" => {},
            else => return self.err_reporter.invalidType("int, byte, float, string, enum or union", type_name, exp.loc),
        }
        const is_slot_error_set = self.type_table.isErrorSet(base_type) and self.errorSetOf(base_type).kind == .slot;
        if (is_slot_error_set) {
            return self.err_reporter.cantMatchSlotErrors(base_kind.@"enum".identifier, exp.loc);
        }

        return .{
            .value = value,
            .type_name = type_name,
            .is_nullable = is_nullable,
            .base_type = base_type,
            .base_kind = base_kind,
            .is_mutable = self.findImmutablePlace(exp) == null,
            .exp = exp,
        };
    }

    fn checkWhenProngs(self: *Self, when_stmt: *const ast.WhenStmt, subject: WhenSubject) CompileError!WhenProngs {
        var prongs: WhenProngs = .{
            .values = try self.allocator.alloc([]*ir.Value, when_stmt.prongs.len),
            .ranges = try self.allocator.alloc([]?WhenRange, when_stmt.prongs.len),
            .seen_values = .init(self.allocator),
            .seen_strings = .init(self.allocator),
            .seen_variants = .init(self.allocator),
            .has_null_case = false,
        };

        for (when_stmt.prongs, 0..) |prong, prong_idx| {
            prongs.values[prong_idx] = &.{};
            prongs.ranges[prong_idx] = &.{};

            switch (prong.pattern) {
                .null_case => {
                    if (!subject.is_nullable) {
                        return self.err_reporter.whenInvalidCase(
                            try std.fmt.allocPrint(self.allocator, "'null' case on non-nullable '{s}'", .{subject.type_name}),
                            prong.loc,
                        );
                    }
                    if (prongs.has_null_case) return self.err_reporter.whenDuplicateCase("null", prong.loc);
                    prongs.has_null_case = true;
                },
                .variant => |variant| {
                    if (subject.base_kind != .@"union") {
                        return self.err_reporter.whenInvalidCase(
                            try std.fmt.allocPrint(self.allocator, "'variant: capture' cases are only for unions, found '{s}'", .{subject.type_name}),
                            prong.loc,
                        );
                    }
                    if (!subject.base_kind.@"union".variants.contains(variant.name)) {
                        return self.err_reporter.invalidUnionVariant(subject.base_kind.@"union".identifier, variant.name, prong.loc);
                    }
                    if (prongs.seen_variants.contains(variant.name)) return self.err_reporter.whenDuplicateCase(variant.name, prong.loc);
                    try prongs.seen_variants.put(variant.name, {});
                },
                .values => |exps| {
                    if (subject.base_kind == .@"union") {
                        return self.err_reporter.whenInvalidCase("union cases are written as 'variant: capture'", prong.loc);
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

                        if (subject.base_kind == .float) return self.err_reporter.whenInvalidCase("float cases must be ranges, like '0.0..1.0'", exp.loc);
                        const value = try self.evalWhenCaseValue(exp, subject.base_type);
                        values[i] = value;
                        ranges[i] = null;

                        if (subject.base_kind == .string) {
                            if (prongs.seen_strings.contains(value.data.i_string)) {
                                return self.err_reporter.whenDuplicateCase(self.err_reporter.src[exp.loc.start .. exp.loc.end + 1], exp.loc);
                            }
                            try prongs.seen_strings.put(value.data.i_string, {});
                            continue;
                        }

                        const key = value.data.i_int;

                        if (prongs.seen_values.contains(key)) {
                            return self.err_reporter.whenDuplicateCase(self.err_reporter.src[exp.loc.start .. exp.loc.end + 1], exp.loc);
                        }

                        if (subject.base_kind == .int or subject.base_kind == .byte) try self.checkWhenRangeOverlap(&prongs, .{ .start = value, .end = value }, exp.loc);
                        try prongs.seen_values.put(key, {});
                    }

                    prongs.values[prong_idx] = values;
                    prongs.ranges[prong_idx] = ranges;
                },
            }
        }

        return prongs;
    }

    fn isNumericWhenSubject(subject: WhenSubject) bool {
        return subject.base_kind == .int or subject.base_kind == .byte or subject.base_kind == .float;
    }

    fn needsElseWhenSubject(subject: WhenSubject) bool {
        return isNumericWhenSubject(subject) or subject.base_kind == .string;
    }

    fn evalWhenRangeBound(self: *Self, exp: *const ast.ExpNode, subject: WhenSubject) CompileError!*ir.Value {
        if (subject.base_kind == .int or subject.base_kind == .byte) return self.evalWhenCaseValue(exp, subject.base_type);
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

        if (literal == null) return self.err_reporter.whenInvalidCase("float range bounds must be numeric literals", exp.loc);
        if (!std.math.isFinite(literal.?)) return self.err_reporter.whenInvalidCase("float range bounds must be finite", exp.loc);
        return ir.Value.init(self.allocator, .{ .data = .{ .i_float = literal.? }, .type_id = subject.base_type });
    }

    fn evalWhenRange(self: *Self, exp: *const ast.ExpNode, subject: WhenSubject) CompileError!WhenRange {
        if (!isNumericWhenSubject(subject)) return self.err_reporter.whenInvalidCase("range cases require an int or float subject", exp.loc);
        const start = try self.evalWhenRangeBound(exp.data.range.start, subject);
        const end = try self.evalWhenRangeBound(exp.data.range.end, subject);
        const are_bounds_ordered = if (subject.base_kind != .float) start.data.i_int <= end.data.i_int else start.data.i_float <= end.data.i_float;
        if (!are_bounds_ordered) return self.err_reporter.whenInvalidCase("range start must not exceed its end", exp.loc);
        return .{ .start = start, .end = end };
    }

    fn checkWhenRangeOverlap(self: *Self, prongs: *WhenProngs, range: WhenRange, loc: err.Loc) CompileError!void {
        for (prongs.seen_ranges.items) |previous| {
            const has_overlap = if (range.start.data == .i_int)
                range.start.data.i_int <= previous.end.data.i_int and previous.start.data.i_int <= range.end.data.i_int
            else
                range.start.data.i_float <= previous.end.data.i_float and previous.start.data.i_float <= range.end.data.i_float;

            if (has_overlap) return self.err_reporter.whenDuplicateCase(self.err_reporter.src[loc.start .. loc.end + 1], loc);
        }

        try prongs.seen_ranges.append(self.allocator, range);
    }

    fn createBinaryValue(self: *Self, kind: ir.BinaryOpKind, left: *ir.Value, right: *ir.Value, type_id: tt.TypeId) CompileError!*ir.Value {
        return ir.Value.init(self.allocator, .{ .data = .{ .binary_op = .{ .kind = kind, .left = left, .right = right } }, .type_id = type_id });
    }

    fn lowerWhenRangeCondition(self: *Self, subject: *ir.Value, range: WhenRange) CompileError!*ir.Value {
        const boolean_type = self.type_table.getPrimitive(.boolean);
        const is_float = range.start.data == .i_float;
        const is_at_least_start = try self.createBinaryValue(if (is_float) .f_cmp_ge else .i_cmp_ge, subject, range.start, boolean_type);
        const is_at_most_end = try self.createBinaryValue(if (is_float) .f_cmp_le else .i_cmp_le, subject, range.end, boolean_type);
        return self.createBinaryValue(.b_and, is_at_least_start, is_at_most_end, boolean_type);
    }

    fn checkWhenExhaustive(self: *Self, exp: *const ast.ExpNode, subject: WhenSubject, prongs: WhenProngs) CompileError!void {
        const when_stmt = exp.data.when_exp;
        const when_loc: err.Loc = .{ .start = exp.loc.start, .end = when_stmt.subject.loc.end };

        if (self.type_table.isErrorSet(subject.base_type) and self.errorSetOf(subject.base_type).kind == .inferred) {
            try self.pending_when_checks.append(self.allocator, .{
                .when_stmt = when_stmt,
                .values = prongs.values,
                .set_type_id = subject.base_type,
                .is_null_missing = subject.is_nullable and !prongs.has_null_case,
                .when_loc = when_loc,
                .module = self.module,
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
            if (needsElseWhenSubject(subject)) return self.err_reporter.whenNeedsElse(subject.type_name, when_loc);
            if (missing.items.len > 0) return self.err_reporter.whenNotExhaustive(missing.items, when_loc);
        } else if (!needsElseWhenSubject(subject) and missing.items.len == 0) {
            return self.err_reporter.whenUselessElse(when_stmt.else_loc.?);
        }
    }

    fn storeWhenSubject(self: *Self, instructions: *std.ArrayList(ir.Instruction), subject: WhenSubject) CompileError!WhenSubjectRefs {
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

        try instructions.append(self.allocator, if (self.isBoxedNullable(subject.value.type_id))
            .{ .unwrap_null_box = .{
                .unwrapped = .{ .uid = unwrapped_uid, .identifier = "when", .type_id = subject.base_type },
                .null_box = subject_ref,
            } }
        else
            .{ .store_var = .{ .uid = unwrapped_uid, .identifier = "when", .type_id = subject.base_type, .value = subject_ref } });

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

    fn lowerWhenVariantProng(self: *Self, prong: *const ast.WhenProng, subject: WhenSubject, unwrapped_ref: *ir.Value, result: ?*BranchResult) CompileError!LoweredProng {
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
        if (variant.is_mut) {
            if (!self.canBeMutable(variant_type_id)) return self.err_reporter.primitiveCaptureCantBeMutable(variant.capture, prong.loc);
            if (!subject.is_mutable) return self.err_reporter.unwrappedValueCantBeMutable(variant.capture, prong.loc);
        }
        const captured_symbol = Symbol{
            .uid = self.scope.genUid(),
            .identifier = variant.capture,
            .type_id = variant_type_id,
            .kind = .{ .variable = .{ .is_mut = variant.is_mut, .is_borrowed = true } },
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

        try body.appendSlice(self.allocator, (try self.visitBranchBlock(&prong.body, result)).block);
        return .{ .condition = condition, .body = try body.toOwnedSlice(self.allocator) };
    }

    fn lowerWhenValuesProng(self: *Self, prong: *const ast.WhenProng, values: []const *ir.Value, ranges: []const ?WhenRange, unwrapped_ref: *ir.Value, result: ?*BranchResult) CompileError!LoweredProng {
        var condition: ?*ir.Value = null;
        for (values, ranges) |value, range| {
            const matches_pattern = if (range) |bounds|
                try self.lowerWhenRangeCondition(unwrapped_ref, bounds)
            else if (value.data == .i_string)
                try self.strEqual(unwrapped_ref, value)
            else
                try self.cmpEq(unwrapped_ref, value);
            condition = if (condition) |previous_condition| try ir.Value.init(self.allocator, .{
                .data = .{ .binary_op = .{ .kind = .b_or, .left = previous_condition, .right = matches_pattern } },
                .type_id = self.type_table.getPrimitive(.boolean),
            }) else matches_pattern;
        }

        return .{ .condition = condition.?, .body = (try self.visitBranchBlock(&prong.body, result)).block };
    }

    fn unionTagIndex(union_def: tt.Union, variant_name: []const u8) usize {
        for (union_def.variants_in_order, 0..) |name, i| {
            if (std.mem.eql(u8, name, variant_name)) return i;
        }
        unreachable;
    }

    fn evalWhenErrorCaseValue(self: *Self, exp: *const ast.ExpNode, expected_type: tt.TypeId) CompileError!*ir.Value {
        const value = try self.evalExp(exp);
        if (!self.type_table.isErrorSet(value.type_id) or self.type_table.getTypePtrById(value.type_id).is_nullable) {
            return self.err_reporter.invalidType(
                try self.type_table.name(expected_type, self.allocator),
                try self.type_table.name(value.type_id, self.allocator),
                exp.loc,
            );
        }
        if (value.data != .i_int) {
            return self.err_reporter.whenInvalidCase("error cases must be error names, like 'ParseError.Empty'", exp.loc);
        }

        const set = self.errorSetOf(expected_type);
        if (set.kind != .inferred and !set.has(value.data.i_int)) {
            return self.err_reporter.errorNotInSet(
                self.type_table.error_names.items[@intCast(value.data.i_int)],
                try self.type_table.name(expected_type, self.allocator),
                exp.loc,
            );
        }

        return ir.Value.init(self.allocator, .{ .data = value.data, .type_id = expected_type });
    }

    fn evalWhenCaseValue(self: *Self, exp: *const ast.ExpNode, expected_type: tt.TypeId) CompileError!*ir.Value {
        if (self.type_table.isErrorSet(expected_type)) {
            return self.evalWhenErrorCaseValue(exp, expected_type);
        }

        if (self.type_table.getTypePtrById(expected_type).kind == .string) {
            if (exp.data != .string_literal) {
                const value = try self.evalExp(exp);
                if (!self.type_table.eql(value.type_id, expected_type)) {
                    return self.err_reporter.invalidType("string", try self.type_table.name(value.type_id, self.allocator), exp.loc);
                }
                return self.err_reporter.whenInvalidCase("string cases must be string literals", exp.loc);
            }
            return ir.Value.init(self.allocator, .{ .data = .{ .i_string = exp.data.string_literal }, .type_id = expected_type });
        }

        if (self.isIntegerType(expected_type)) {
            const type_name = try self.type_table.name(expected_type, self.allocator);
            const number = intConstant(exp) orelse blk: {
                const value = try self.evalExp(exp);
                if (!self.type_table.canCoerce(value.type_id, expected_type)) {
                    return self.err_reporter.invalidType(type_name, try self.type_table.name(value.type_id, self.allocator), exp.loc);
                }
                if (!self.isConstantExp(exp)) {
                    return self.err_reporter.whenInvalidCase(try std.fmt.allocPrint(self.allocator, "{s} cases must be known at compile time: literals, constants, static fields and operators on them", .{type_name}), exp.loc);
                }
                break :blk try self.foldInt(value, exp.loc) orelse return self.err_reporter.whenInvalidCase(try std.fmt.allocPrint(self.allocator, "{s} cases must be {s} values known at compile time", .{ type_name, type_name }), exp.loc);
            };

            const is_byte = self.type_table.getTypePtrById(expected_type).kind == .byte;
            if (is_byte and (number < 0 or number > 255)) return self.err_reporter.byteOutOfRange(number, exp.loc);

            return ir.Value.init(self.allocator, .{ .data = .{ .i_int = number }, .type_id = expected_type });
        }

        const value = try self.evalExp(exp);
        if (!self.type_table.eql(value.type_id, expected_type)) {
            return self.err_reporter.invalidType(
                try self.type_table.name(expected_type, self.allocator),
                try self.type_table.name(value.type_id, self.allocator),
                exp.loc,
            );
        }
        if (value.data != .i_int) {
            return self.err_reporter.whenInvalidCase("enum cases must be variants, like 'Color.Red'", exp.loc);
        }
        return value;
    }

    fn foldInt(self: *Self, value: *const ir.Value, loc: err.Loc) CompileError!?i64 {
        switch (value.data) {
            .i_int => |number| return number,
            .unary_op => |unary| {
                if (unary.kind != .i_neg) return null;
                const operand = try self.foldInt(unary.right, loc) orelse return null;
                if (operand == std.math.minInt(i64)) return self.err_reporter.whenInvalidCase("this case value doesn't fit in an int", loc);
                return -operand;
            },
            .binary_op => |binary| {
                const left = try self.foldInt(binary.left, loc) orelse return null;
                const right = try self.foldInt(binary.right, loc) orelse return null;
                const result = switch (binary.kind) {
                    .i_add => @addWithOverflow(left, right),
                    .i_sub => @subWithOverflow(left, right),
                    .i_mult => @mulWithOverflow(left, right),
                    .trunc_div, .i_mod => {
                        if (right == 0) return self.err_reporter.whenInvalidCase("this case value divides by zero", loc);
                        if (binary.kind == .i_mod) return if (right == -1) 0 else @mod(left, right);
                        if (right == -1 and left == std.math.minInt(i64)) return self.err_reporter.whenInvalidCase("this case value doesn't fit in an int", loc);
                        return @divTrunc(left, right);
                    },
                    else => return null,
                };
                if (result[1] != 0) return self.err_reporter.whenInvalidCase("this case value doesn't fit in an int", loc);
                return result[0];
            },
            else => return null,
        }
    }

    fn strEqual(self: *Self, left: *ir.Value, right: *ir.Value) !*ir.Value {
        const symbol = self.prelude_module.root.symbols.get("strings_equal").?;
        const func = self.type_table.getTypePtrById(symbol.type_id).kind.function;
        const boolean_type = self.type_table.getPrimitive(.boolean);
        const args = try self.allocator.alloc(*ir.Value, 2);
        args[0] = left;
        args[1] = right;
        return ir.Value.init(self.allocator, .{
            .data = .{ .fn_call = .{ .fn_uid = func.uid, .identifier = symbol.identifier, .args = args } },
            .type_id = boolean_type,
        });
    }

    fn cmpEq(self: *Self, left: *ir.Value, right: *ir.Value) !*ir.Value {
        return ir.Value.init(self.allocator, .{
            .data = .{ .binary_op = .{ .kind = .i_cmp_eq, .left = left, .right = right } },
            .type_id = self.type_table.getPrimitive(.boolean),
        });
    }

    fn wrapBranch(self: *Self, condition: *ir.Value, then_block: ir.Block, else_block: ir.Block) !ir.Block {
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
                    return self.err_reporter.notDefined(id, assign_stmt.target.loc);
                };
                if (id_symbol.kind != .variable) {
                    return self.err_reporter.notMutable(id, assign_stmt.target.loc);
                }

                const assignment_value = try self.evalExpExpecting(assign_stmt.exp, id_symbol.type_id);

                if (!id_symbol.kind.variable.is_mut) {
                    return self.err_reporter.notMutable(id, assign_stmt.target.loc);
                }
                if (id_symbol.kind.variable.is_borrowed) {
                    return self.err_reporter.mutBorrowCantBeReassigned(id, assign_stmt.target.loc);
                }
                try self.ensureNotReadOnlySource(assign_stmt.exp);

                const boxed_value = try self.coerceValue(assignment_value, id_symbol.type_id, assign_stmt.exp.loc);

                if (self.outerVariableContext(id_symbol)) |context_index| {
                    const index = try self.resolveCapture(context_index, id_symbol, assign_stmt.target.loc);
                    return ir.Instruction{ .update_capture = .{ .index = index, .value = boxed_value } };
                }

                return ir.Instruction{ .store_var = .{
                    .identifier = id_symbol.identifier,
                    .uid = id_symbol.uid,
                    .value = boxed_value,
                    .type_id = id_symbol.type_id,
                } };
            },
            .indexed => {
                const index_exp = assign_stmt.target.data.indexed;
                const target = try self.evalExp(index_exp.target);
                const target_type = target.type_id;
                const indexed_target = try self.indexInto(assign_stmt.target, target);
                const target_base = self.type_table.typeIdByNullableTypeId.get(indexed_target.type_id) orelse indexed_target.type_id;
                const is_float_target = target_base == self.type_table.getPrimitive(.float);
                const expected_type_id = if (isAnonymousInit(assign_stmt.exp) or isConditional(assign_stmt.exp) or assign_stmt.exp.data == .array_literal or indexed_target.type_id == self.type_table.getPrimitive(.byte) or is_float_target)
                    indexed_target.type_id
                else
                    null;
                const assignment_value = try self.evalExpExpecting(assign_stmt.exp, expected_type_id);

                const is_static_target = self.definedTypeOf(index_exp.target) != null;
                if (!is_static_target) {
                    try self.ensureMutable(index_exp.target);
                    try self.ensureNotReadOnlySource(assign_stmt.exp);
                }

                const destination_type = try self.assignedFieldType(target_type, index_exp, indexed_target.type_id, is_static_target);
                const boxed_value = try self.coerceValue(assignment_value, destination_type, assign_stmt.exp.loc);

                return ir.Instruction{
                    .update_indexed = .{
                        .target = indexed_target,
                        .value = boxed_value,
                    },
                };
            },
            else => {
                return self.err_reporter.invalidAssignment(
                    "identifier or identifier[n]",
                    @tagName(assign_stmt.target.data),
                    assign_stmt.target.loc,
                );
            },
        }
    }

    fn isOutsideTypeModule(self: *Self, type_id: tt.TypeId) bool {
        const base = self.type_table.typeIdByNullableTypeId.get(type_id) orelse type_id;
        const module = self.type_modules.get(base) orelse return false;
        return module != self.module;
    }

    fn checkMemberVisible(self: *Self, type_id: tt.TypeId, member: []const u8, is_file_only: bool, loc: err.Loc) CompileError!void {
        if (!is_file_only) return;
        if (!self.isOutsideTypeModule(type_id)) return;
        return self.err_reporter.memberNotVisible(member, try self.type_table.syntaxName(type_id, self.allocator), loc);
    }

    fn checkStructBuildable(self: *Self, type_id: tt.TypeId, fields: []const tt.TypedIdentifier, loc: err.Loc) CompileError!void {
        if (!self.isOutsideTypeModule(type_id)) return;
        for (fields) |field| {
            const is_unbuildable = field.is_file_only and field.default_value == null;
            if (is_unbuildable) {
                return self.err_reporter.cantBuildOutsideModule(try self.type_table.syntaxName(type_id, self.allocator), field.identifier, loc);
            }
        }
    }

    fn checkFieldArgument(self: *Self, type_id: tt.TypeId, field: tt.TypedIdentifier, argument: ?*ast.ExpNode) CompileError!void {
        if (!field.is_file_only) return;
        const operand = argument orelse return;
        if (!self.isOutsideTypeModule(type_id)) return;
        return self.err_reporter.memberNotVisible(field.identifier, try self.type_table.syntaxName(type_id, self.allocator), operand.loc);
    }

    fn assignedFieldType(self: *Self, target_type: tt.TypeId, index_exp: ast.IndexedExp, indexed_type: tt.TypeId, is_static_target: bool) CompileError!tt.TypeId {
        switch (self.type_table.getTypePtrById(target_type).kind) {
            .buffer => return indexed_type,
            .array => |inner| return inner,
            else => {},
        }
        const member_name = index_exp.index.data.identifier;
        const member_loc = index_exp.index.loc;
        const field = switch (self.type_table.getTypePtrById(target_type).kind) {
            .@"struct" => |struct_def| static: {
                if (!is_static_target) {
                    const field = struct_def.fields.get(member_name) orelse return self.err_reporter.invalidStructField(struct_def.identifier, member_name, member_loc);
                    try self.checkMemberVisible(target_type, member_name, field.is_file_only, member_loc);
                    return field.type_id;
                }
                break :static struct_def.static_fields.get(member_name) orelse return self.err_reporter.invalidStaticStructField(struct_def.identifier, member_name, member_loc);
            },
            .@"enum" => |enum_def| enum_def.static_fields.get(member_name) orelse return self.err_reporter.notMutable(member_name, member_loc),
            .@"union" => |union_def| union_def.static_fields.get(member_name) orelse {
                if (union_def.variants.contains(member_name)) return self.err_reporter.notMutable(member_name, member_loc);
                return self.err_reporter.invalidUnionMember(union_def.identifier, member_name, member_loc);
            },
            else => unreachable,
        };
        if (!field.is_mut) return self.err_reporter.notMutable(field.identifier, member_loc);
        return field.type_id;
    }

    fn visitForStmt(self: *Self, stmt: *const ast.StatementNode) CompileError!ir.Instruction {
        const for_stmt = stmt.data.@"for";

        const condition_value = try self.evalExp(for_stmt.condition);
        const condition_value_type = condition_value.type_id;

        if (!self.type_table.eql(condition_value_type, self.type_table.getPrimitive(.boolean))) {
            return self.err_reporter.invalidType(
                "boolean",
                try self.type_table.name(condition_value_type, self.allocator),
                for_stmt.condition.loc,
            );
        }

        const block = try self.visitBlock(&for_stmt.do_block);

        return ir.Instruction{ .loop = .{
            .condition = condition_value,
            .do_block = block.block,
        } };
    }

    fn storeTemporaryValue(self: *Self, instructions: *std.ArrayList(ir.Instruction), name: []const u8, value: *ir.Value) CompileError!*ir.Value {
        const uid = self.scope.genUid();
        try instructions.append(self.allocator, .{ .store_var = .{ .uid = uid, .identifier = name, .type_id = value.type_id, .value = value } });
        return ir.Value.init(self.allocator, .{ .data = .{ .identifier = .{ .uid = uid, .name = name } }, .type_id = value.type_id });
    }

    fn bindLoopCapture(self: *Self, instructions: *std.ArrayList(ir.Instruction), name: []const u8, value: *ir.Value, is_mut: bool, loc: err.Loc) CompileError!?usize {
        if (std.mem.eql(u8, name, "_")) return null;
        if (self.scope.symbol_table.symbols.contains(name)) return self.err_reporter.alreadyDefined(name, loc);
        const reference = try self.storeTemporaryValue(instructions, name, value);
        try self.scope.symbol_table.put(.{
            .uid = reference.data.identifier.uid,
            .identifier = name,
            .type_id = value.type_id,
            .kind = .{ .variable = .{ .is_mut = is_mut, .is_borrowed = true } },
        });
        return reference.data.identifier.uid;
    }

    fn lowerCaptureLoopBody(self: *Self, capture: ast.ForCaptureStmt, item: *ir.Value, index: *ir.Value, loc: err.Loc) CompileError!ir.Block {
        var body: std.ArrayList(ir.Instruction) = .empty;
        try self.scope.enter(self.scope.return_type_id);
        defer self.scope.exit(self.scope.return_type_id);
        _ = try self.bindLoopCapture(&body, capture.item, item, capture.is_item_mut, loc);
        if (capture.index) |name| _ = try self.bindLoopCapture(&body, name, index, false, loc);
        try body.appendSlice(self.allocator, (try self.visitBlock(&capture.do_block)).block);
        return body.toOwnedSlice(self.allocator);
    }

    fn evalIntegerRangeBound(self: *Self, exp: *const ast.ExpNode) CompileError!*ir.Value {
        const value = try self.evalExp(exp);

        if (!self.isIntegerType(value.type_id)) {
            return self.err_reporter.invalidType("int", try self.type_table.name(value.type_id, self.allocator), exp.loc);
        }

        return value;
    }

    fn createLoopUpdate(reference: *ir.Value, value: *ir.Value) ir.Instruction {
        return .{ .store_var = .{ .uid = reference.data.identifier.uid, .identifier = reference.data.identifier.name, .type_id = reference.type_id, .value = value } };
    }

    fn appendLoopIncrement(self: *Self, instructions: *std.ArrayList(ir.Instruction), reference: *ir.Value) CompileError!void {
        const one = try ir.Value.init(self.allocator, .{ .data = .{ .i_int = 1 }, .type_id = reference.type_id });
        const next = try self.createBinaryValue(.i_add, reference, one, reference.type_id);
        try instructions.append(self.allocator, createLoopUpdate(reference, next));
    }

    fn lowerRangeAdvancement(self: *Self, cursor: *ir.Value, end: *ir.Value, is_running: *ir.Value, index: ?*ir.Value) CompileError!ir.Block {
        const is_last_item = try self.cmpEq(cursor, end);
        const stopped = try ir.Value.init(self.allocator, .{ .data = .{ .i_bool = false }, .type_id = is_running.type_id });
        const stop_instructions = try self.allocator.alloc(ir.Instruction, 1);
        stop_instructions[0] = createLoopUpdate(is_running, stopped);
        var advancement: std.ArrayList(ir.Instruction) = .empty;
        try self.appendLoopIncrement(&advancement, cursor);
        if (index) |reference| try self.appendLoopIncrement(&advancement, reference);
        return self.wrapBranch(is_last_item, stop_instructions, try advancement.toOwnedSlice(self.allocator));
    }

    fn visitForRangeStmt(self: *Self, stmt: *const ast.StatementNode) CompileError![]ir.Instruction {
        const capture = stmt.data.for_capture;
        if (capture.is_item_mut) return self.err_reporter.primitiveCaptureCantBeMutable(capture.item, stmt.loc);
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

    fn visitForCaptureStmt(self: *Self, stmt: *const ast.StatementNode) CompileError![]ir.Instruction {
        const capture = stmt.data.for_capture;
        if (capture.source.data == .range) return self.visitForRangeStmt(stmt);
        const source = try self.evalExp(capture.source);
        const source_type = self.type_table.getTypePtrById(source.type_id);
        const is_buffer_source = !source_type.is_nullable and (source_type.kind == .buffer or source_type.kind == .string);
        const is_array_source = !source_type.is_nullable and source_type.kind == .array;
        if (!is_array_source and !is_buffer_source) return self.err_reporter.invalidType("array, buffer or string", try self.type_table.name(source.type_id, self.allocator), capture.source.loc);
        const item_type = if (is_buffer_source) self.type_table.getPrimitive(.byte) else source_type.kind.array;
        if (capture.is_item_mut) {
            if (!self.canBeMutable(item_type)) return self.err_reporter.primitiveCaptureCantBeMutable(capture.item, stmt.loc);
            if (self.findImmutablePlace(capture.source) != null) return self.err_reporter.unwrappedValueCantBeMutable(capture.item, stmt.loc);
        }
        const integer_type = self.type_table.getPrimitive(.int);
        var instructions: std.ArrayList(ir.Instruction) = .empty;
        const source_reference = try self.storeTemporaryValue(&instructions, "loop source", source);
        const length_data: @FieldType(ir.Value, "data") = if (is_buffer_source) .{ .bytes_length = source_reference } else .{ .array_length = source_reference };
        const length = try ir.Value.init(self.allocator, .{ .data = length_data, .type_id = integer_type });
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

    fn visitStructDef(self: *Self, struct_def: *const ast.Struct, struct_symbol: Symbol) CompileError!ir.Struct {
        const identifier = struct_symbol.identifier;
        const blueprint_type_id = struct_symbol.kind.defined_type.blueprint_type_id;
        const prev_return_type = self.scope.return_type_id;
        try self.scope.enter(self.type_table.getPrimitive(.void));
        defer self.scope.exit(prev_return_type);
        var fields: std.ArrayList(ir.Field) = .empty;

        for (struct_def.fields) |field| {
            const annotated_type = if (field.type) |annotation| try self.resolveTypeAnnotation(annotation, blueprint_type_id) else null;
            const default_value: ?*ir.Value = if (field.default_value) |value| try self.evalExpExpecting(value, annotated_type) else null;
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

        const funcs = try self.lowerMethods(struct_def.funcs, self.type_table.getTypePtrById(blueprint_type_id).kind.@"struct".methods);

        return ir.Struct{
            .uid = struct_symbol.uid,
            .type_id = blueprint_type_id,
            .identifier = identifier,
            .fields = try fields.toOwnedSlice(self.allocator),
            .funcs = funcs,
        };
    }

    fn visitFnDef(
        self: *Self,
        fn_def: *const ast.FnDef,
        uid: usize,
        metadata: tt.Func,
        override_name: ?[]const u8,
        loc: err.Loc,
    ) CompileError!ir.Func {
        var params: std.ArrayList(ir.FuncParam) = .empty;

        const old_return_type = self.scope.return_type_id;
        try self.scope.enter(metadata.return_type_id);
        defer self.scope.exit(old_return_type);
        self.fn_depth += 1;
        defer self.fn_depth -= 1;
        for (metadata.params) |param| {
            const arg_uid = self.scope.genUid();

            const is_checked_mut = param.is_mut and !self.is_lowering_copy and !self.isSlotType(param.type_id);
            if (is_checked_mut and !self.canBeMutable(param.type_id)) {
                return self.err_reporter.primitiveParamsCantBeMutable(loc);
            }

            try self.scope.symbol_table.put(.{
                .uid = arg_uid,
                .identifier = param.identifier,
                .type_id = param.type_id,
                .kind = .{ .variable = .{ .is_mut = param.is_mut, .is_borrowed = true } },
            });

            try params.append(self.allocator, .{
                .uid = arg_uid,
                .identifier = param.identifier,
                .type_id = param.type_id,
                .default_value = param.default_value,
            });
        }

        const body = try self.visitBlock(&fn_def.body_block);
        if (self.activeReturnInference()) |context_index| {
            self.scope.return_type_id = try self.inferredReturnType(context_index);
        }
        const return_type_id = self.scope.return_type_id;
        const is_failing_void = self.type_table.isErrorUnion(self.scope.return_type_id) and
            self.type_table.getTypePtrById(self.scope.return_type_id).kind.error_union.payload == self.type_table.getPrimitive(.void);

        const body_return_type = if (body.is_returning) self.scope.return_type_id else self.type_table.getPrimitive(.void);
        if (!is_failing_void and !self.type_table.canCoerce(body_return_type, self.scope.return_type_id)) {
            return self.err_reporter.invalidFunctionReturnType(
                try self.type_table.name(self.scope.return_type_id, self.allocator),
                try self.type_table.name(body_return_type, self.allocator),
                loc,
            );
        }

        return ir.Func{
            .uid = uid,
            .identifier = if (override_name) |name| name else metadata.identifier,
            .params = try params.toOwnedSlice(self.allocator),
            .return_type = return_type_id,
            .body = body.block,
            .origin = .{ .src = self.src, .loc = loc, .copy_context = self.err_reporter.copy_context },
        };
    }

    fn visitReturnStmt(self: *Self, stmt: *const ast.StatementNode) !ir.Instruction {
        const return_stmt = stmt.data.@"return";
        if (self.activeReturnInference() != null) {
            return self.visitInferredReturnStmt(stmt);
        }

        if (self.type_table.isErrorUnion(self.scope.return_type_id)) {
            return self.visitFailingReturnStmt(stmt);
        }

        if (return_stmt.exp) |exp| {
            const exp_value = try self.evalExpExpecting(exp, self.scope.return_type_id);
            const exp_value_type = exp_value.type_id;

            if (!try self.canCoerce(exp_value_type, self.scope.return_type_id, exp.loc)) {
                return self.err_reporter.invalidFunctionReturnType(
                    try self.type_table.name(self.scope.return_type_id, self.allocator),
                    try self.type_table.name(exp_value_type, self.allocator),
                    exp.loc,
                );
            }

            const boxed_value = try self.boxForTarget(exp_value, self.scope.return_type_id);

            return ir.Instruction{ .return_stmt = .{
                .value = boxed_value,
            } };
        }

        if (!self.type_table.eql(self.scope.return_type_id, self.type_table.getPrimitive(.void))) {
            return self.err_reporter.invalidFunctionReturnType(
                try self.type_table.name(self.scope.return_type_id, self.allocator),
                "void",
                stmt.loc,
            );
        }

        return ir.Instruction{ .return_stmt = .{
            .value = null,
        } };
    }

    fn visitInferredReturnStmt(self: *Self, stmt: *const ast.StatementNode) CompileError!ir.Instruction {
        const exp = stmt.data.@"return".exp orelse {
            try self.inferReturnPayload(self.type_table.getPrimitive(.void), stmt.loc);
            return ir.Instruction{ .return_stmt = .{ .value = null } };
        };

        const value = try self.evalExp(exp);
        const is_error = self.type_table.isErrorSet(value.type_id) and !self.type_table.getTypePtrById(value.type_id).is_nullable;
        if (!is_error) {
            try self.inferReturnPayload(value.type_id, exp.loc);
            return ir.Instruction{ .return_stmt = .{ .value = value } };
        }

        const error_set = try self.lambdaErrorSet(self.activeReturnInference().?);
        if (value.data == .i_int) {
            try self.errorSetOf(error_set).ids.put(self.allocator, value.data.i_int, {});
        } else {
            try self.addErrorEdge(value.type_id, error_set, exp.loc);
        }
        return ir.Instruction{ .return_error = .{ .value = value } };
    }

    fn activeReturnInference(self: *Self) ?usize {
        const context = self.lambda_contexts.getLastOrNull() orelse return null;
        if (!context.is_return_inferred or context.body_depth != self.fn_depth) {
            return null;
        }
        return self.lambda_contexts.items.len - 1;
    }

    fn inferReturnPayload(self: *Self, type_id: tt.TypeId, loc: err.Loc) CompileError!void {
        const context = &self.lambda_contexts.items[self.activeReturnInference().?];
        const known = context.return_payload orelse {
            context.return_payload = type_id;
            return;
        };
        if (self.type_table.eql(known, type_id)) return;
        return self.err_reporter.lambdaReturnsDiffer(
            try self.type_table.name(known, self.allocator),
            try self.type_table.name(type_id, self.allocator),
            loc,
        );
    }

    fn lambdaErrorSet(self: *Self, context_index: usize) CompileError!tt.TypeId {
        if (self.lambda_contexts.items[context_index].error_set) |error_set| {
            return error_set;
        }
        const error_set = try self.createErrorSet(lambda_name, .inferred);
        self.lambda_contexts.items[context_index].error_set = error_set;
        return error_set;
    }

    fn inferredReturnType(self: *Self, context_index: usize) CompileError!tt.TypeId {
        const context = self.lambda_contexts.items[context_index];
        const payload = context.return_payload orelse self.type_table.getPrimitive(.void);
        const error_set = context.error_set orelse return payload;
        return self.errorUnionType(error_set, payload);
    }

    fn visitFailingReturnStmt(self: *Self, stmt: *const ast.StatementNode) CompileError!ir.Instruction {
        const return_stmt = stmt.data.@"return";
        const error_union = self.type_table.getTypePtrById(self.scope.return_type_id).kind.error_union;
        const payload = error_union.payload;

        const exp = return_stmt.exp orelse {
            if (payload != self.type_table.getPrimitive(.void)) {
                return self.err_reporter.invalidFunctionReturnType(
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
                if (target_set.kind == .inferred) {
                    try target_set.ids.put(self.allocator, value.data.i_int, {});
                } else if (!target_set.has(value.data.i_int)) {
                    return self.err_reporter.errorNotInSet(
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
            return self.err_reporter.invalidFunctionReturnType(
                try self.type_table.name(self.scope.return_type_id, self.allocator),
                try self.type_table.name(value.type_id, self.allocator),
                exp.loc,
            );
        }

        const boxed_value = try self.boxForTarget(value, payload);

        return ir.Instruction{ .return_stmt = .{ .value = boxed_value } };
    }

    fn isConditional(exp: *const ast.ExpNode) bool {
        return switch (exp.data) {
            .if_exp, .if_capture_exp, .when_exp, .orelse_exp => true,
            else => false,
        };
    }

    fn evalConditional(self: *Self, exp: *const ast.ExpNode, expected_type_id: ?tt.TypeId) CompileError!*ir.Value {
        return self.evalConditionalWithContext(exp, expected_type_id, null);
    }

    fn evalConditionalWithContext(self: *Self, exp: *const ast.ExpNode, expected_type_id: ?tt.TypeId, context_type_id: ?tt.TypeId) CompileError!*ir.Value {
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

    fn finishBranchResult(self: *Self, result: *BranchResult, instructions: ir.Block) CompileError!*ir.Value {
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
            .@"struct", .@"union", .array, .func_type => payload_type,
            else => null,
        };
    }

    fn evalOrelse(self: *Self, exp: *const ast.ExpNode, expected_type: ?tt.TypeId, context_type: ?tt.TypeId) CompileError!*ir.Value {
        const coalescing = exp.data.orelse_exp;
        const expected = if (expected_type == self.type_table.getPrimitive(.dynamic)) null else expected_type;
        const operand = try self.evalExpWithContext(coalescing.operand, null, expected orelse context_type);
        const null_type = self.type_table.getPrimitive(.null);
        const is_null_operand = operand.type_id == null_type;
        const is_nullable = self.type_table.getTypePtrById(operand.type_id).is_nullable;

        if (!is_nullable and !is_null_operand) {
            return self.err_reporter.invalidType("nullable or null", try self.type_table.name(operand.type_id, self.allocator), coalescing.operand.loc);
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

    fn unwrapNullableValue(self: *Self, instructions: *std.ArrayList(ir.Instruction), operand: *ir.Value, payload_type: tt.TypeId, name: []const u8) CompileError!*ir.Value {
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

    fn lowerOrelseBranch(self: *Self, operand: *ir.Value, payload_type: tt.TypeId, fallback: ir.Instruction, result: *BranchResult, loc: err.Loc) CompileError!*ir.Value {
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

    fn mergeBranchTypes(self: *Self, first: tt.TypeId, second: tt.TypeId, loc: err.Loc) CompileError!tt.TypeId {
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
            return self.err_reporter.invalidType(try self.type_table.name(first, self.allocator), try self.type_table.name(second, self.allocator), loc);
        }
        if (try self.canCoerce(first, second, loc)) return second;
        if (try self.canCoerce(second, first, loc)) return first;
        return self.err_reporter.invalidType(try self.type_table.name(first, self.allocator), try self.type_table.name(second, self.allocator), loc);
    }

    fn convertBranchValue(self: *Self, value: *ir.Value, result_type: tt.TypeId, loc: err.Loc) CompileError!*ir.Value {
        const value_base = self.type_table.typeIdByNullableTypeId.get(value.type_id) orelse value.type_id;
        const result_base = self.type_table.typeIdByNullableTypeId.get(result_type) orelse result_type;
        const value_kind = self.type_table.getTypePtrById(value_base).kind;
        const result_kind = self.type_table.getTypePtrById(result_base).kind;
        if (value.data == .i_array and value_kind == .array and result_kind == .array and value_kind.array == self.type_table.getPrimitive(.dynamic)) {
            value.type_id = result_base;
        }
        const converted = try self.coerceValue(value, result_type, loc);
        if (converted.type_id == self.type_table.getPrimitive(.null)) {
            return ir.Value.init(self.allocator, .{ .data = .i_null, .type_id = result_type });
        }
        return converted;
    }

    fn isAnonymousInit(exp: *const ast.ExpNode) bool {
        return exp.data == .anonymous_init;
    }

    fn evalExpWithContext(self: *Self, exp: *const ast.ExpNode, expected_type: ?tt.TypeId, context_type: ?tt.TypeId) CompileError!*ir.Value {
        if (expected_type != null) return self.evalExpExpecting(exp, expected_type);
        if (isConditional(exp)) return self.evalConditionalWithContext(exp, null, context_type);
        const needs_context = isAnonymousInit(exp) or exp.data == .array_literal or exp.data == .lambda;
        return self.evalExpExpecting(exp, if (needs_context) context_type else null);
    }

    fn evalExpExpecting(self: *Self, exp: *const ast.ExpNode, expected_type_id: ?tt.TypeId) CompileError!*ir.Value {
        const value = try self.evalExpExpectingUnrecorded(exp, expected_type_id);
        try self.exp_types.put(self.allocator, exp, value.type_id);
        return value;
    }

    fn evalExpExpectingUnrecorded(self: *Self, exp: *const ast.ExpNode, expected_type_id: ?tt.TypeId) CompileError!*ir.Value {
        if (isConditional(exp)) {
            return self.evalConditional(exp, expected_type_id);
        }
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
            const is_lambda_for_function_type = exp.data == .lambda and base_kind == .func_type;
            if (is_lambda_for_function_type) {
                return self.evalLambda(exp, try self.functionTypeHints(base_kind.func_type));
            }
            const is_name_for_function_type = exp.data == .identifier and base_kind == .func_type;
            if (is_name_for_function_type) {
                if (self.scope.symbol_table.get(exp.data.identifier)) |symbol| {
                    if (symbol.kind == .generic_function) {
                        return self.evalGenericFuncRef(exp, symbol.kind.generic_function, base_kind.func_type);
                    }
                }
            }
            if (base_kind == .byte) {
                if (intConstant(exp)) |number| {
                    return self.evalByteConstant(number, exp.loc);
                }
            }
        }

        const value = switch (exp.data) {
            .fn_call => try self.evalHandledCall(exp, expected_type_id),
            .catch_exp => try self.evalCatch(exp, false, expected_type_id),
            else => try self.evalExp(exp),
        };
        const expected = expected_type_id orelse return value;
        if (isValueLeaving(value)) {
            return ir.Value.init(self.allocator, .{ .data = value.data, .type_id = expected });
        }
        return self.promoteToFloat(value, expected);
    }

    fn promoteToFloat(self: *Self, value: *ir.Value, expected: tt.TypeId) CompileError!*ir.Value {
        const float_type = self.type_table.getPrimitive(.float);
        const value_base = self.type_table.typeIdByNullableTypeId.get(value.type_id) orelse value.type_id;
        const expected_base = self.type_table.typeIdByNullableTypeId.get(expected) orelse expected;
        if (value_base != self.type_table.getPrimitive(.int) or expected_base != float_type) return value;
        const value_is_nullable = value_base != value.type_id;
        if (value_is_nullable and expected == expected_base) return value;
        if (value.data == .i_int) {
            return ir.Value.init(self.allocator, .{ .data = .{ .i_float = @floatFromInt(value.data.i_int) }, .type_id = float_type });
        }
        return ir.Value.init(self.allocator, .{
            .data = .{ .int_to_float = value },
            .type_id = if (value_is_nullable) expected else float_type,
        });
    }

    fn intConstant(exp: *const ast.ExpNode) ?i64 {
        return switch (exp.data) {
            .int_literal => |number| number,
            .byte_literal => |byte| byte,
            .unary_exp => |unary| if (unary.op == .neg and unary.right.data == .int_literal) -unary.right.data.int_literal else null,
            else => null,
        };
    }

    fn evalByteConstant(self: *Self, number: i64, loc: err.Loc) CompileError!*ir.Value {
        if (number < 0 or number > 255) return self.err_reporter.byteOutOfRange(number, loc);
        return ir.Value.init(self.allocator, .{ .data = .{ .i_int = number }, .type_id = self.type_table.getPrimitive(.byte) });
    }

    fn evalFloatCast(self: *Self, operand: *const ast.ExpNode) CompileError!*ir.Value {
        const value = try self.evalExp(operand);
        if (isValueLeaving(value)) return value;
        const float_type = self.type_table.getPrimitive(.float);
        if (value.type_id == float_type) return value;
        if (self.slotBase(value.type_id) != null) {
            if (!try self.areSlotMembersNumeric(value.type_id)) {
                return self.err_reporter.invalidType("int, byte or float", try self.type_table.name(value.type_id, self.allocator), operand.loc);
            }
            return ir.Value.init(self.allocator, .{ .data = .{ .int_to_float = value }, .type_id = float_type });
        }
        if (!self.isIntegerType(value.type_id)) {
            return self.err_reporter.invalidType("int, byte or float", try self.type_table.name(value.type_id, self.allocator), operand.loc);
        }
        if (value.data == .i_int) {
            return ir.Value.init(self.allocator, .{ .data = .{ .i_float = @floatFromInt(value.data.i_int) }, .type_id = float_type });
        }
        return ir.Value.init(self.allocator, .{ .data = .{ .int_to_float = value }, .type_id = float_type });
    }

    fn areSlotMembersNumeric(self: *Self, type_id: tt.TypeId) CompileError!bool {
        const slot_id = self.slotBase(type_id).?;
        const type_list = self.type_table.getTypePtrById(slot_id).kind.slot.type_list;
        if (type_list.len == 0) return false;
        const float_type = self.type_table.getPrimitive(.float);
        for (type_list) |member| {
            const member_type = try self.substituteSlot(type_id, member);
            if (member_type != float_type and !self.isIntegerType(member_type)) return false;
        }
        return true;
    }

    fn evalExp(self: *Self, exp: *const ast.ExpNode) CompileError!*ir.Value {
        const value = try self.evalExpUnrecorded(exp);
        try self.exp_types.put(self.allocator, exp, value.type_id);
        return value;
    }

    fn evalExpUnrecorded(self: *Self, exp: *const ast.ExpNode) CompileError!*ir.Value {
        switch (exp.*.data) {
            .if_exp, .if_capture_exp, .when_exp, .orelse_exp => return self.evalConditional(exp, null),
            .range => return self.err_reporter.invalidExpression("range in a capture loop or when case", "range value", exp.loc),
            .float_literal => {
                return ir.Value.init(self.allocator, .{ .data = .{ .i_float = exp.data.float_literal }, .type_id = self.type_table.getPrimitive(.float) });
            },
            .int_literal => {
                return ir.Value.init(self.allocator, .{ .data = .{ .i_int = exp.data.int_literal }, .type_id = self.type_table.getPrimitive(.int) });
            },
            .byte_literal => |byte| {
                return ir.Value.init(self.allocator, .{ .data = .{ .i_int = byte }, .type_id = self.type_table.getPrimitive(.byte) });
            },

            .string_literal => {
                return ir.Value.init(self.allocator, .{ .data = .{ .i_string = exp.data.string_literal }, .type_id = self.type_table.getPrimitive(.string) });
            },
            .string_interpolation => |parts| {
                var values: std.ArrayList(*ir.Value) = .empty;
                for (parts) |part| {
                    const value = try self.evalExp(part);
                    if (self.type_table.getTypePtrById(value.type_id).kind == .void and !isValueLeaving(value)) {
                        return self.err_reporter.invalidExpression("value in string interpolation", "void", part.loc);
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
            .fn_call => return self.evalHandledCall(exp, null),
            .try_exp => {
                return self.evalTry(exp);
            },
            .catch_exp => {
                return self.evalCatch(exp, false, null);
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
            .lambda => return self.evalLambda(exp, null),
        }
    }

    fn functionTypeHints(self: *Self, func_type: tt.FuncType) CompileError!LambdaHints {
        const hints = try self.allocator.alloc(?tt.FuncTypeParam, func_type.params.len);
        for (func_type.params, hints) |param, *hint| {
            hint.* = param;
        }
        return .{ .params = hints, .return_type_id = func_type.return_type_id };
    }

    fn evalLambda(self: *Self, exp: *const ast.ExpNode, hints: ?LambdaHints) CompileError!*ir.Value {
        const typed = try self.typedLambdaDef(exp, hints);
        const def = typed.def;

        try self.lambda_contexts.append(self.allocator, .{
            .floor = self.scope.next_uid,
            .body_depth = self.fn_depth + 1,
            .is_return_inferred = typed.is_return_inferred,
        });
        const context_index = self.lambda_contexts.items.len - 1;
        defer _ = self.lambda_contexts.pop();

        const symbol = try self.createEmptyFuncSymbol(lambda_name);
        try self.fulfillFuncType(symbol, def, null, exp.loc);
        const signature = self.type_table.getTypePtrById(symbol.type_id).kind.function;
        var func = try self.visitFnDef(def, signature.uid, signature, null, exp.loc);
        const func_metadata = &self.type_table.getTypePtrById(symbol.type_id).kind.function;
        func_metadata.return_type_id = func.return_type;
        const metadata = func_metadata.*;

        const captures = self.lambda_contexts.items[context_index].captures.items;
        const capture_infos = try self.allocator.alloc(ir.Capture, captures.len);
        const sources = try self.allocator.alloc(*ir.Value, captures.len);
        for (captures, capture_infos, sources) |capture, *info, *source| {
            info.* = .{ .type_id = capture.type_id, .is_boxed = capture.is_boxed };
            source.* = capture.source;
        }
        func.captures = capture_infos;
        if (!self.is_checking_opaque) {
            try self.functions.append(self.allocator, func);
        }

        const func_type_id = try self.funcValueType(metadata);
        if (captures.len == 0) {
            return ir.Value.init(self.allocator, .{
                .data = .{ .func_ref = .{ .fn_uid = metadata.uid, .name = lambda_name } },
                .type_id = func_type_id,
            });
        }
        return ir.Value.init(self.allocator, .{
            .data = .{ .make_closure = .{ .fn_uid = metadata.uid, .sources = sources } },
            .type_id = func_type_id,
        });
    }

    fn typedLambdaDef(self: *Self, exp: *const ast.ExpNode, hints: ?LambdaHints) CompileError!TypedLambda {
        const def = &exp.data.lambda;
        for (def.params) |param| {
            const default_value = param.default_value orelse continue;
            return self.err_reporter.lambdaParamCantHaveDefault(param.identifier, default_value.loc);
        }

        const typed_def = try self.allocator.create(ast.FnDef);
        typed_def.* = def.*;
        typed_def.params = try self.typedLambdaParams(exp, if (hints) |known| known.params else null);
        if (def.return_type != null) {
            return .{ .def = typed_def, .is_return_inferred = false };
        }

        const known_return = if (hints) |known| known.return_type_id else null;
        const return_type_id = known_return orelse self.type_table.getPrimitive(.void);
        typed_def.return_type = try ast.TypeAnnotation.init(self.allocator, .{ .type = .{ .resolved = return_type_id }, .is_nullable = false });
        return .{ .def = typed_def, .is_return_inferred = known_return == null };
    }

    fn typedLambdaParams(self: *Self, exp: *const ast.ExpNode, hints: ?[]const ?tt.FuncTypeParam) CompileError![]const ast.FnParam {
        const def = &exp.data.lambda;
        const known = hints orelse {
            for (def.params) |param| {
                if (param.type_annotation == null) {
                    return self.err_reporter.lambdaParamNeedsType(param.identifier, exp.loc);
                }
            }
            return def.params;
        };
        if (known.len != def.params.len) {
            return self.err_reporter.wrongLambdaParamCount(def.params.len, known.len, exp.loc);
        }

        const params = try self.allocator.alloc(ast.FnParam, def.params.len);
        for (def.params, known, params) |param, hint, *typed| {
            typed.* = param;
            if (hint) |expected| {
                if (expected.is_mut != param.is_mut) {
                    return self.err_reporter.lambdaParamMutDiffers(param.identifier, expected.is_mut, exp.loc);
                }
            }
            if (param.type_annotation != null) continue;
            const expected = hint orelse return self.err_reporter.lambdaParamNeedsType(param.identifier, exp.loc);
            typed.type_annotation = try ast.TypeAnnotation.init(self.allocator, .{ .type = .{ .resolved = expected.type_id }, .is_nullable = false });
        }
        return params;
    }

    fn outerVariableContext(self: *Self, symbol: Symbol) ?usize {
        if (symbol.kind != .variable) {
            return null;
        }
        const context = self.lambda_contexts.getLastOrNull() orelse return null;
        if (symbol.uid >= context.floor) {
            return null;
        }
        return self.lambda_contexts.items.len - 1;
    }

    fn resolveCapture(self: *Self, context_index: usize, symbol: Symbol, loc: err.Loc) CompileError!u8 {
        for (self.lambda_contexts.items[context_index].captures.items, 0..) |capture, i| {
            if (capture.symbol_uid == symbol.uid) {
                return @intCast(i);
            }
        }
        const count = self.lambda_contexts.items[context_index].captures.items.len;
        if (count == max_captures) {
            return self.err_reporter.tooManyCaptures(max_captures, loc);
        }

        const is_boxed = symbol.kind.variable.is_mut and !symbol.kind.variable.is_borrowed;
        const is_parent_local = context_index == 0 or symbol.uid >= self.lambda_contexts.items[context_index - 1].floor;
        const cell_type = self.type_table.getPrimitive(.cell);
        const raw_type = if (is_boxed) cell_type else symbol.type_id;
        const source = if (is_parent_local and is_boxed)
            try ir.Value.init(self.allocator, .{ .data = .{ .var_box = symbol.uid }, .type_id = cell_type })
        else if (is_parent_local)
            try ir.Value.init(self.allocator, .{ .data = .{ .identifier = .{ .uid = symbol.uid, .name = symbol.identifier } }, .type_id = symbol.type_id })
        else
            try ir.Value.init(self.allocator, .{ .data = .{ .capture_raw = try self.resolveCapture(context_index - 1, symbol, loc) }, .type_id = raw_type });

        if (is_boxed) {
            try self.boxed_uids.put(self.allocator, symbol.uid, {});
        }
        try self.lambda_contexts.items[context_index].captures.append(self.allocator, .{
            .symbol_uid = symbol.uid,
            .type_id = symbol.type_id,
            .is_boxed = is_boxed,
            .source = source,
        });
        return @intCast(count);
    }

    fn evalCaptureRef(self: *Self, context_index: usize, symbol: Symbol, loc: err.Loc) CompileError!*ir.Value {
        const index = try self.resolveCapture(context_index, symbol, loc);
        const capture = self.lambda_contexts.items[context_index].captures.items[index];
        return ir.Value.init(self.allocator, .{
            .data = .{ .capture_ref = .{ .index = index, .is_boxed = capture.is_boxed } },
            .type_id = symbol.type_id,
        });
    }

    fn evalCopyCall(self: *Self, copy_uid: usize, identifier: []const u8, argument: *const ast.ExpNode) CompileError!*ir.Value {
        const value = try self.evalExp(argument);
        if (isValueLeaving(value)) return value;
        if (!self.isChangeableType(value.type_id)) return value;
        const args = try self.allocator.alloc(*ir.Value, 1);
        args[0] = value;
        return ir.Value.init(self.allocator, .{
            .data = .{ .fn_call = .{ .fn_uid = copy_uid, .identifier = identifier, .args = args } },
            .type_id = value.type_id,
        });
    }

    fn isChangeableType(self: *Self, type_id: tt.TypeId) bool {
        const base_id = self.type_table.typeIdByNullableTypeId.get(type_id) orelse type_id;
        if (base_id == self.type_table.getPrimitive(.null)) return false;
        return self.canBeChangedThrough(base_id);
    }

    fn evalFnCall(self: *Self, exp: *const ast.ExpNode, expected_type: ?tt.TypeId) !*ir.Value {
        const fn_call = exp.data.fn_call;
        var fn_identifier: []const u8 = undefined;
        var uid: usize = undefined;
        var params: []const tt.TypedIdentifier = undefined;
        var return_type: tt.TypeId = undefined;

        var is_struct_init = false;
        var callee: ?*ir.Value = null;

        var arg_exp_by_param_name = std.StringHashMap(*ast.ExpNode).init(self.allocator);
        defer arg_exp_by_param_name.deinit();

        var fn_call_arguments_values: std.ArrayList(*ir.Value) = .empty;
        var inferred_values: std.AutoHashMapUnmanaged(*const ast.ExpNode, *ir.Value) = .empty;

        resolve: switch (fn_call.target.data) {
            .identifier => |id| {
                const symbol = self.scope.symbol_table.getOrError(id) catch {
                    return self.notDefined(id, exp.loc);
                };

                fn_identifier = symbol.identifier;

                if (self.intrinsic_by_uid.get(symbol.uid)) |intrinsic| {
                    if (try self.evalIntrinsicCall(intrinsic, symbol, fn_call)) |value| {
                        return value;
                    }
                }

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
                                return self.err_reporter.invalidType(
                                    "function or struct",
                                    try self.type_table.name(symbol.type_id, self.allocator),
                                    exp.loc,
                                );
                            },
                            else => unreachable,
                        }
                    },
                    .constant => return self.err_reporter.invalidType(
                        "function or struct",
                        try self.type_table.name((try self.resolveConstant(symbol.kind.constant)).type_id, self.allocator),
                        exp.loc,
                    ),
                    .generic_type => |index| {
                        const copy_symbol = try self.constructGenericType(index, fn_call, expected_type, exp.loc, &inferred_values);
                        const blueprint_type_id = copy_symbol.kind.defined_type.blueprint_type_id;
                        switch (self.type_table.getTypePtrById(blueprint_type_id).kind) {
                            .@"struct" => |struct_def| {
                                params = struct_def.fields_in_order;
                                return_type = blueprint_type_id;
                                uid = copy_symbol.uid;
                                is_struct_init = true;
                            },
                            .@"union" => return self.evalUnionInit(exp, fn_call.arguments, blueprint_type_id),
                            else => unreachable,
                        }
                    },
                    .generic_function => |index| {
                        const copy_symbol = try self.callGenericFunction(index, fn_call, expected_type, exp.loc, &inferred_values);
                        const func = self.type_table.getTypePtrById(copy_symbol.type_id).kind.function;
                        params = func.params;
                        return_type = func.return_type_id;
                        uid = func.uid;
                        fn_identifier = func.identifier;
                    },
                    .type_slot => return self.err_reporter.slotUsedAsValue(id, exp.loc),
                    .trait => return self.err_reporter.traitUsedAsType(id, exp.loc),
                    .task_type => return self.err_reporter.taskNeedsSpawn(exp.loc),
                    .variable => {
                        const type_ptr = self.type_table.getTypePtrById(symbol.type_id);
                        if (type_ptr.kind == .func_type) {
                            callee = try self.evalCallee(fn_call.target);
                        } else if (type_ptr.kind == .@"struct") {
                            const struct_def = type_ptr.kind.@"struct";
                            params = struct_def.fields_in_order;
                            return_type = symbol.type_id;
                            uid = symbol.uid;
                            is_struct_init = true;
                        } else {
                            return self.err_reporter.invalidType(
                                "function or struct",
                                try self.type_table.name(symbol.type_id, self.allocator),
                                exp.loc,
                            );
                        }
                    },
                }
            },
            .indexed => |indexed| if (indexed.index.data != .identifier) {
                callee = try self.evalCallee(fn_call.target);
            } else {
                const target = try self.evalExp(indexed.target);
                const fn_name = indexed.index.data.identifier;
                if (try self.fieldCallee(fn_call.target, target)) |field| {
                    callee = field;
                    break :resolve;
                }
                const fn_type_id = try self.methodOrError(target.type_id, fn_name, exp.loc);
                try self.checkMemberVisible(target.type_id, fn_name, self.file_only_methods.contains(fn_type_id), exp.loc);
                const fn_metadata = self.type_table.getTypePtrById(fn_type_id).kind.function;
                const is_slot = self.type_table.getTypePtrById(target.type_id).kind == .slot;
                const takes_self = is_slot or (fn_metadata.params.len > 0 and target.type_id == fn_metadata.params[0].type_id);
                if (takes_self and self.shouldAutoBindSelf(target)) {
                    if (fn_metadata.params[0].is_mut) {
                        try self.ensureMutable(indexed.target);
                    }
                    try fn_call_arguments_values.append(self.allocator, target);
                }
                params = fn_metadata.params;
                return_type = fn_metadata.return_type_id;
                uid = fn_metadata.uid;
                fn_identifier = fn_name;
            },
            else => callee = try self.evalCallee(fn_call.target),
        }

        if (callee) |value| {
            if (fn_call.are_arguments_named) {
                return self.err_reporter.namedArgumentsOnValue(exp.loc);
            }
            const func_type = self.type_table.getTypePtrById(value.type_id).kind.func_type;
            const value_params = try self.allocator.alloc(tt.TypedIdentifier, func_type.params.len);
            for (func_type.params, value_params) |param, *value_param| {
                value_param.* = .{ .identifier = "", .type_id = param.type_id, .is_mut = param.is_mut, .default_value = null };
            }
            params = value_params;
            return_type = func_type.return_type_id;
            fn_identifier = callName(exp);
        }

        const call_args_len = fn_call.arguments.len;
        if (is_struct_init) {
            try self.checkStructBuildable(return_type, params, exp.loc);
        }

        var required_params_len: usize = 0;
        for (params) |param| {
            if (param.default_value == null) {
                required_params_len += 1;
            }
        }

        //call args + auto binded struct
        const total_args_len = call_args_len + fn_call_arguments_values.items.len;

        if (required_params_len > total_args_len) {
            return self.err_reporter.invalidNumberOfArgs(required_params_len, total_args_len, exp.loc);
        }

        if (total_args_len > params.len) {
            return self.err_reporter.invalidNumberOfArgs(params.len, total_args_len, exp.loc);
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
            if (is_struct_init) {
                try self.checkFieldArgument(return_type, param, arg_exp);
            }

            if (arg_exp) |_exp| {
                const evaluated = inferred_values.get(_exp) orelse try self.evalExpExpecting(_exp, param.type_id);
                const arg_type = evaluated.type_id;
                const fn_call_arg_value = try self.coerceValue(evaluated, param.type_id, _exp.loc);

                const arg_type_ptr = self.type_table.getTypePtrById(arg_type);
                if (arg_type_ptr.kind == .array and self.type_table.getTypePtrById(arg_type_ptr.kind.array).kind == .dynamic) {
                    fn_call_arg_value.type_id = param.type_id;
                }

                if (param.is_mut and self.isChangeableType(arg_type) and (_exp.data == .identifier or _exp.data == .indexed)) {
                    try self.ensureMutable(_exp);
                }
                if (param.is_mut) {
                    try self.ensureNotReadOnlySource(_exp);
                }
                try fn_call_arguments_values.append(self.allocator, fn_call_arg_value);
            } else if (param.default_value) |value| {
                try fn_call_arguments_values.append(self.allocator, try self.boxForTarget(value, param.type_id));
            } else {
                return self.err_reporter.missingArgument(param.identifier, exp.loc);
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

        if (callee) |value| {
            return ir.Value.init(self.allocator, .{
                .data = .{ .call_value = .{ .callee = value, .args = try fn_call_arguments_values.toOwnedSlice(self.allocator) } },
                .type_id = return_type,
            });
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

    fn evalCallee(self: *Self, target: *const ast.ExpNode) CompileError!*ir.Value {
        return self.checkCallee(try self.evalExp(target), target.loc);
    }

    fn checkCallee(self: *Self, value: *ir.Value, loc: err.Loc) CompileError!*ir.Value {
        const type_ptr = self.type_table.getTypePtrById(value.type_id);
        if (type_ptr.is_nullable) {
            return self.err_reporter.nullableMustBeUnwraped(try self.type_table.name(value.type_id, self.allocator), loc);
        }
        if (type_ptr.kind != .func_type) {
            return self.err_reporter.invalidType("a function", try self.type_table.name(value.type_id, self.allocator), loc);
        }
        return value;
    }

    fn fieldCallee(self: *Self, exp: *const ast.ExpNode, owner: *ir.Value) CompileError!?*ir.Value {
        const name = exp.data.indexed.index.data.identifier;
        if (self.methodOf(owner.type_id, name) != null) {
            return null;
        }
        if (self.isTypeName(exp.data.indexed.target)) {
            return null;
        }
        const owner_type = self.type_table.getTypePtrById(owner.type_id);
        if (owner_type.kind != .@"struct") {
            return null;
        }
        const field = owner_type.kind.@"struct".fields.get(name) orelse return null;
        if (self.type_table.getTypePtrById(field.type_id).kind != .func_type) {
            return null;
        }
        try self.checkMemberVisible(owner.type_id, name, field.is_file_only, exp.loc);
        const value = try self.indexInto(exp, owner);
        try self.exp_types.put(self.allocator, exp, value.type_id);
        return try self.checkCallee(value, exp.loc);
    }

    fn evalIntrinsicCall(self: *Self, intrinsic: native.Intrinsic, symbol: Symbol, fn_call: ast.FnCall) CompileError!?*ir.Value {
        if (fn_call.arguments.len != 1) return null;
        const argument = fn_call.arguments[0].exp;
        return switch (intrinsic) {
            .to_float => try self.evalFloatCast(argument),
            .copy => {
                const copy_uid = self.generic_functions.items[symbol.kind.generic_function].extern_uid.?;
                return try self.evalCopyCall(copy_uid, symbol.identifier, argument);
            },
            .array_len, .array_pop, .bytes_len, .new_buffer, .to_byte, .to_int, .append, .task_spawn => null,
        };
    }

    fn methodOrError(self: *Self, type_id: tt.TypeId, name: []const u8, loc: err.Loc) CompileError!tt.TypeId {
        if (self.methodOf(type_id, name)) |method| return method;
        return switch (self.type_table.getTypePtrById(type_id).kind) {
            .@"struct" => |s| self.err_reporter.invalidStructFunction(s.identifier, name, loc),
            .@"enum" => |e| self.err_reporter.invalidEnumFunction(e.identifier, name, loc),
            .@"union" => |u| self.err_reporter.invalidUnionFunction(u.identifier, name, loc),
            .slot => |slot| self.err_reporter.slotHasNoMethod(slot.identifier, name, loc),
            else => self.err_reporter.invalidType(
                "a defined type such as struct/enums or union",
                try self.type_table.name(type_id, self.allocator),
                loc,
            ),
        };
    }

    fn evalExpectedStructInit(self: *Self, exp: *const ast.ExpNode, type_id: tt.TypeId) CompileError!*ir.Value {
        const structure = self.type_table.getTypePtrById(type_id).kind.@"struct";
        const initialization = exp.data.anonymous_init;
        try self.checkStructBuildable(type_id, structure.fields_in_order, exp.loc);
        if (initialization.arguments.len > structure.fields_in_order.len) {
            return self.err_reporter.invalidNumberOfArgs(structure.fields_in_order.len, initialization.arguments.len, exp.loc);
        }
        var arguments_by_name = std.StringHashMap(*ast.ExpNode).init(self.allocator);
        defer arguments_by_name.deinit();
        for (initialization.arguments) |argument| {
            if (argument.identifier) |name| {
                if (!structure.fields.contains(name)) return self.err_reporter.invalidStructField(structure.identifier, name, argument.exp.loc);
                if (arguments_by_name.contains(name)) return self.err_reporter.alreadyDefined(name, argument.exp.loc);
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
            try self.checkFieldArgument(type_id, field, argument);
            const value = if (argument) |operand|
                try self.evalExpExpecting(operand, field.type_id)
            else if (field.default_value) |default_value|
                default_value
            else if (self.type_table.getTypePtrById(field.type_id).is_nullable)
                try ir.Value.init(self.allocator, .{ .data = .i_null, .type_id = field.type_id })
            else
                return self.err_reporter.missingArgument(field.identifier, exp.loc);
            const loc = if (argument) |operand| operand.loc else exp.loc;
            values[index] = try self.coerceValue(value, field.type_id, loc);
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
            return self.err_reporter.invalidType(
                "a single named variant argument",
                try self.type_table.name(blueprint_type_id, self.allocator),
                exp.loc,
            );
        }

        const arg = arguments[0];
        const variant_name = arg.identifier.?;

        const variant_type_id = union_def.variants.get(variant_name) orelse {
            return self.err_reporter.invalidUnionVariant(union_def.identifier, variant_name, exp.loc);
        };

        const raw_value = try self.evalExpExpecting(arg.exp, variant_type_id);
        const value = try self.coerceValue(raw_value, variant_type_id, arg.exp.loc);

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
            return self.err_reporter.nullableMustBeUnwraped(try self.type_table.name(target_type, self.allocator), exp.loc);
        }

        if (is_nullable and !self.type_table.getTypePtrById(target_type).is_nullable) {
            return self.err_reporter.unnecessaryOptionalChain(try self.type_table.name(target_type, self.allocator), exp.loc);
        }

        switch (self.type_table.getTypePtrById(target_type).kind) {
            .array => {
                const index = try self.evalExp(indexed_exp.index);
                const index_type = index.type_id;

                if (!self.isIntegerType(index_type)) {
                    return self.err_reporter.invalidIndexing("int", try self.type_table.name(index_type, self.allocator), exp.loc);
                }

                const inner_type = self.type_table.getTypePtrById(target_type).kind.array;
                const result_type = if (is_nullable) try self.type_table.getOrAddNullable(inner_type) else inner_type;

                return ir.Value.init(self.allocator, .{
                    .data = .{ .indexed = .{ .target = target, .index = index } },
                    .type_id = result_type,
                });
            },
            .string => return self.err_reporter.stringIndexing(exp.loc),
            .buffer => {
                if (is_nullable) return self.err_reporter.invalidType("buffer", try self.type_table.name(target_type, self.allocator), exp.loc);
                const index = try self.evalExp(indexed_exp.index);
                if (!self.isIntegerType(index.type_id)) {
                    return self.err_reporter.invalidIndexing("int", try self.type_table.name(index.type_id, self.allocator), exp.loc);
                }
                return ir.Value.init(self.allocator, .{
                    .data = .{ .indexed = .{ .target = target, .index = index } },
                    .type_id = self.type_table.getPrimitive(.byte),
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
                            return self.err_reporter.invalidStructField(struct_def.identifier, member_name, exp.loc);
                        }
                        if (field_member) |field| {
                            try self.checkMemberVisible(target_type, member_name, field.is_file_only, exp.loc);
                        }
                        if (field_member == null) {
                            if (static_field_member) |constant| return constant.default_value.?;
                            return self.err_reporter.methodAsValue(member_name, exp.loc);
                        }
                        if (self.definedTypeOf(indexed_exp.target) != null) {
                            return self.err_reporter.invalidStaticStructField(struct_def.identifier, member_name, exp.loc);
                        }

                        const field_type_id = field_member.?.type_id;
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

                        if (enum_def.static_fields.get(member_name)) |constant| return constant.default_value.?;

                        if (enum_def.methods.contains(member_name)) {
                            return self.err_reporter.methodAsValue(member_name, exp.loc);
                        }

                        return self.err_reporter.invalidEnumVariant(enum_def.identifier, member_name, exp.loc);
                    },
                    else => unreachable,
                };
            },

            .@"union" => {
                const union_def = self.type_table.getTypePtrById(target_type).kind.@"union";
                return switch (indexed_exp.index.data) {
                    .identifier => |member_name| {
                        if (union_def.static_fields.get(member_name)) |constant| return constant.default_value.?;
                        if (self.definedTypeOf(indexed_exp.target) != null) {
                            return self.err_reporter.invalidStaticUnionField(union_def.identifier, member_name, exp.loc);
                        }
                        // reading a variant is always ?T: null when it isn't the active one
                        if (union_def.methods.contains(member_name)) {
                            return self.err_reporter.methodAsValue(member_name, exp.loc);
                        }
                        const variant_type_id = union_def.variants.get(member_name) orelse {
                            return self.err_reporter.invalidUnionMember(union_def.identifier, member_name, exp.loc);
                        };
                        const result_type = try self.type_table.getOrAddNullable(variant_type_id);

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
                return self.err_reporter.invalidType("array, struct, enum or union", try self.type_table.name(target_type, self.allocator), exp.loc);
            },
        }
    }

    fn evalIdentifier(self: *Self, exp: *const ast.ExpNode) !*ir.Value {
        const id = exp.data.identifier;
        const id_symbol = self.scope.symbol_table.get(id);

        if (id_symbol) |symbol| {
            if (symbol.kind == .constant) {
                return self.resolveConstant(symbol.kind.constant);
            }
            if (symbol.kind == .function) {
                return self.evalFuncRef(symbol.type_id, exp.loc);
            }
            if (self.outerVariableContext(symbol)) |context_index| {
                return self.evalCaptureRef(context_index, symbol, exp.loc);
            }
            if (symbol.kind == .generic_function) {
                return self.err_reporter.genericNeedsCall(id, exp.loc);
            }
            if (symbol.kind == .generic_type) {
                const opaque_copy = self.generic_types.items[symbol.kind.generic_type].opaque_copy orelse unreachable;
                return ir.Value.init(self.allocator, .{
                    .data = .{ .identifier = .{ .uid = opaque_copy.uid, .name = symbol.identifier } },
                    .type_id = opaque_copy.kind.defined_type.blueprint_type_id,
                });
            }
            if (symbol.kind == .type_slot) {
                return self.err_reporter.slotUsedAsValue(id, exp.loc);
            }
            if (symbol.kind == .trait) {
                return self.err_reporter.traitUsedAsType(id, exp.loc);
            }
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

        return self.notDefined(id, exp.loc);
    }

    fn notDefined(self: *Self, id: []const u8, loc: err.Loc) CompileError {
        const binding = self.lambda_binding orelse return self.err_reporter.notDefined(id, loc);
        if (!std.mem.eql(u8, binding, id)) {
            return self.err_reporter.notDefined(id, loc);
        }
        return self.err_reporter.lambdaCallsItself(id, loc);
    }

    fn evalFuncRef(self: *Self, func_type_id: tt.TypeId, loc: err.Loc) CompileError!*ir.Value {
        const func = self.type_table.getTypePtrById(func_type_id).kind.function;
        if (self.extern_uids.contains(func.uid)) {
            return self.err_reporter.builtinAsValue(func.identifier, loc);
        }
        return ir.Value.init(self.allocator, .{
            .data = .{ .func_ref = .{ .fn_uid = func.uid, .name = func.identifier } },
            .type_id = try self.funcValueType(func),
        });
    }

    fn funcValueType(self: *Self, func: tt.Func) CompileError!tt.TypeId {
        const params = try self.allocator.alloc(tt.FuncTypeParam, func.params.len);
        for (func.params, params) |param, *value_param| {
            value_param.* = .{ .type_id = param.type_id, .is_mut = param.is_mut };
        }
        return self.type_table.getOrAddFuncType(.{ .params = params, .return_type_id = func.return_type_id });
    }

    fn evalGenericFuncRef(self: *Self, exp: *const ast.ExpNode, index: usize, func_type: tt.FuncType) CompileError!*ir.Value {
        const func_stmt = self.generic_functions.items[index].func_stmt;
        const bindings = try self.allocator.alloc(?tt.TypeId, func_stmt.slots.len);
        @memset(bindings, null);

        if (func_stmt.def.params.len == func_type.params.len) {
            for (func_stmt.def.params, func_type.params) |param, expected_param| {
                const annotation = param.type_annotation orelse continue;
                try self.inferSlots(annotation, expected_param.type_id, func_stmt.slots, bindings, exp.loc);
            }
        }
        if (func_stmt.def.return_type) |return_annotation| {
            const return_type = self.type_table.getTypePtrById(func_type.return_type_id);
            const expected_return = if (return_type.kind == .error_union) return_type.kind.error_union.payload else func_type.return_type_id;
            try self.inferSlots(return_annotation, expected_return, func_stmt.slots, bindings, exp.loc);
        }

        const args = try self.allocator.alloc(tt.TypeId, bindings.len);
        for (bindings, func_stmt.slots, 0..) |binding, slot, slot_index| {
            args[slot_index] = binding orelse return self.err_reporter.cantInferSlot(slot.identifier, func_stmt.identifier, exp.loc);
        }
        const copy_symbol = try self.instantiateGenericFunction(index, args, exp.loc);
        return self.evalFuncRef(copy_symbol.type_id, exp.loc);
    }

    fn isSlotType(self: *Self, type_id: tt.TypeId) bool {
        const base_id = self.type_table.typeIdByNullableTypeId.get(type_id) orelse type_id;
        return self.type_table.getTypePtrById(base_id).kind == .slot;
    }

    fn canBeMutable(self: *Self, type_id: tt.TypeId) bool {
        const kind = self.type_table.getTypePtrById(type_id).kind;
        if (kind == .slot) return kind.slot.methods.len > 0;
        return self.canBeChangedThrough(type_id);
    }

    fn canBeChangedThrough(self: *Self, type_id: tt.TypeId) bool {
        return switch (self.type_table.getTypePtrById(type_id).kind) {
            .array, .buffer, .@"struct", .@"union" => true,
            else => false,
        };
    }

    fn isTypeName(self: *Self, exp: *const ast.ExpNode) bool {
        if (exp.data != .identifier) return false;
        const sym = self.scope.symbol_table.get(exp.data.identifier) orelse return false;
        return sym.kind == .defined_type or sym.kind == .generic_type;
    }

    fn definedTypeOf(self: *Self, exp: *const ast.ExpNode) ?tt.TypeId {
        if (exp.data != .identifier) return null;
        const sym = self.scope.symbol_table.get(exp.data.identifier) orelse return null;
        if (sym.kind == .generic_type) {
            const opaque_copy = self.generic_types.items[sym.kind.generic_type].opaque_copy orelse return null;
            return opaque_copy.kind.defined_type.blueprint_type_id;
        }
        if (sym.kind != .defined_type) return null;
        return sym.kind.defined_type.blueprint_type_id;
    }

    fn staticFieldOf(self: *Self, indexed: ast.IndexedExp) ?tt.TypedIdentifier {
        const type_id = self.definedTypeOf(indexed.target) orelse return null;
        if (indexed.index.data != .identifier) return null;
        const static_fields = switch (self.type_table.getTypePtrById(type_id).kind) {
            .@"struct" => |s| s.static_fields,
            .@"enum" => |e| e.static_fields,
            .@"union" => |u| u.static_fields,
            else => return null,
        };
        return static_fields.get(indexed.index.data.identifier);
    }

    fn findImmutablePlace(self: *Self, exp: *const ast.ExpNode) ?[]const u8 {
        switch (exp.data) {
            .identifier => |name| {
                const sym = self.scope.symbol_table.get(name) orelse return null;
                if (sym.kind == .variable and sym.kind.variable.is_mut) return null;
                return name;
            },
            .indexed => |indexed| {
                if (self.staticFieldOf(indexed)) |field| {
                    if (field.is_mut) return null;
                    return field.identifier;
                }
                return self.findImmutablePlace(indexed.target);
            },
            else => return null,
        }
    }

    fn ensureNotReadOnlySource(self: *Self, value: *const ast.ExpNode) CompileError!void {
        if (self.is_in_prelude or self.is_lowering_copy) return;
        const source = self.readOnlySourceOf(value) orelse return;
        const text = self.src[source.loc.start..@min(source.loc.end + 1, self.src.len)];
        return self.err_reporter.readOnlyIntoMutable(text, source.loc);
    }

    fn readOnlySourceOf(self: *Self, exp: *const ast.ExpNode) ?*const ast.ExpNode {
        switch (exp.data) {
            .identifier, .indexed => {
                const type_id = self.exp_types.get(exp) orelse return null;
                if (!self.isChangeableType(type_id)) return null;
                if (self.findImmutablePlace(exp) == null) return null;
                return exp;
            },
            .array_literal => |literal| return self.firstReadOnlySource(literal.exps),
            .anonymous_init => |anonymous| return self.firstReadOnlyArgument(anonymous.arguments),
            .fn_call => |call| {
                if (!self.isConstructorCall(call)) return null;
                return self.firstReadOnlyArgument(call.arguments);
            },
            .try_exp => |try_exp| return self.readOnlySourceOf(try_exp.operand),
            .orelse_exp => |orelse_exp| return self.readOnlySourceOf(orelse_exp.operand) orelse self.readOnlySourceOf(orelse_exp.fallback),
            .catch_exp => |catch_exp| return self.readOnlySourceOf(catch_exp.operand) orelse switch (catch_exp.fallback) {
                .value => |fallback| self.readOnlySourceOf(fallback),
                .block => |block| self.readOnlyBlockTail(&block.body),
            },
            .if_exp => |if_exp| return self.readOnlyBlockTail(&if_exp.then_block) orelse if (if_exp.else_block) |*block| self.readOnlyBlockTail(block) else null,
            .if_capture_exp => |capture| return self.readOnlyBlockTail(&capture.body) orelse if (capture.else_block) |*block| self.readOnlyBlockTail(block) else null,
            .when_exp => |when_exp| {
                for (when_exp.prongs) |*prong| {
                    if (self.readOnlyBlockTail(&prong.body)) |source| return source;
                }
                return if (when_exp.else_block) |*block| self.readOnlyBlockTail(block) else null;
            },
            else => return null,
        }
    }

    fn firstReadOnlySource(self: *Self, exps: []const *ast.ExpNode) ?*const ast.ExpNode {
        for (exps) |item| {
            if (self.readOnlySourceOf(item)) |source| return source;
        }
        return null;
    }

    fn firstReadOnlyArgument(self: *Self, arguments: []const ast.FnCallArg) ?*const ast.ExpNode {
        for (arguments) |argument| {
            if (self.readOnlySourceOf(argument.exp)) |source| return source;
        }
        return null;
    }

    fn readOnlyBlockTail(self: *Self, block: *const ast.Block) ?*const ast.ExpNode {
        const statements = block.statements.items;
        if (statements.len == 0) return null;
        const last = statements[statements.len - 1];
        if (last.data != .expression) return null;
        return self.readOnlySourceOf(last.data.expression);
    }

    fn isConstructorCall(self: *Self, call: ast.FnCall) bool {
        if (call.target.data != .identifier) return false;
        const symbol = self.scope.symbol_table.get(call.target.data.identifier) orelse return false;
        return symbol.kind == .defined_type or symbol.kind == .generic_type;
    }

    fn ensureMutable(self: *Self, exp: *const ast.ExpNode) CompileError!void {
        const name = self.findImmutablePlace(exp) orelse return;
        return self.err_reporter.notMutable(name, exp.loc);
    }

    fn shouldAutoBindSelf(self: *Self, target: *ir.Value) bool {
        if (target.data == .identifier) {
            if (self.scope.symbol_table.get(target.data.identifier.name)) |sym| {
                return sym.kind == .variable or sym.kind == .constant;
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

            const float_type = self.type_table.getPrimitive(.float);
            if (array_literal_type == self.type_table.getPrimitive(.int) and exp_value_type == float_type) {
                array_literal_type = float_type;
                for (values.items) |*previous| previous.* = try self.promoteToFloat(previous.*, float_type);
            }

            const item_value = try self.coerceValue(exp_value, array_literal_type, exp.loc);

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

        if (self.isSlotType(exp_type)) {
            return ir.Value.init(self.allocator, .{
                .data = .{ .unary_op = .{
                    .kind = self.astUnaryOpToIrUnaryOpKind(unary_exp.op, exp_type),
                    .right = exp_value,
                } },
                .type_id = try self.slotUnaryOpType(unary_exp.op, exp_type, exp.loc),
            });
        }

        switch (unary_exp.op) {
            .neg => {
                if (!self.isTypeNumber(exp_type)) {
                    return self.err_reporter.invalidType(
                        "int or float",
                        try self.type_table.name(exp_type, self.allocator),
                        exp.loc,
                    );
                }
            },
            .not => {
                if (!self.type_table.eql(exp_type, self.type_table.getPrimitive(.boolean))) {
                    return self.err_reporter.invalidType(
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
            .type_id = if (exp_type == self.type_table.getPrimitive(.byte)) self.type_table.getPrimitive(.int) else exp_type,
        });
    }

    fn slotUnaryOpType(self: *Self, op: ast.UnaryOp, type_id: tt.TypeId, loc: err.Loc) CompileError!tt.TypeId {
        const op_text = if (op == .neg) "-" else "!";
        var results = SlotResults{};
        for (try self.operandTypeList(op_text, type_id, loc)) |member| {
            const operand = try self.substituteSlot(type_id, member);
            const result = self.unaryOpType(op, operand) orelse return self.err_reporter.slotOperatorNotAllowedOn(
                op_text,
                try self.type_table.name(type_id, self.allocator),
                try self.type_table.syntaxName(member, self.allocator),
                loc,
            );
            results.add(result, member, member);
        }
        return self.slotResultType(&results, op_text, type_id, type_id, loc);
    }

    fn unaryOpType(self: *Self, op: ast.UnaryOp, type_id: tt.TypeId) ?tt.TypeId {
        return switch (op) {
            .neg => if (!self.isTypeNumber(type_id)) null else if (type_id == self.type_table.getPrimitive(.byte)) self.type_table.getPrimitive(.int) else type_id,
            .not => if (self.type_table.eql(type_id, self.type_table.getPrimitive(.boolean))) type_id else null,
        };
    }

    fn isTypeNumber(self: *Self, t: tt.TypeId) bool {
        return self.type_table.eql(t, self.type_table.getPrimitive(.float)) or self.isIntegerType(t);
    }

    fn isIntegerType(self: *Self, t: tt.TypeId) bool {
        return self.type_table.eql(t, self.type_table.getPrimitive(.int)) or self.type_table.eql(t, self.type_table.getPrimitive(.byte));
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

        if (self.isSlotType(left_type) or self.isSlotType(right_type)) {
            return ir.Value.init(self.allocator, .{
                .data = .{ .binary_op = .{
                    .kind = self.astBinOpToIrBinOpKind(bin_exp.op, left_type, right_type),
                    .left = left_value,
                    .right = right_value,
                } },
                .type_id = try self.slotBinaryOpType(bin_exp.op, left_type, right_type, exp.loc),
            });
        }

        var op_type: tt.TypeId = self.type_table.getPrimitive(.void);

        switch (bin_exp.op) {
            .add, .sub, .mult, .mod, .div, .trunc_div, .lt, .lt_or_eq, .gt, .gt_or_eq => {
                op_type = self.binaryOpType(bin_exp.op, left_type, right_type) orelse return self.err_reporter.invalidType(
                    "int or float",
                    try self.type_table.name(left_type, self.allocator),
                    exp.loc,
                );
            },
            .bool_or, .bool_and => {
                op_type = self.binaryOpType(bin_exp.op, left_type, right_type) orelse return self.err_reporter.invalidType(
                    "boolean",
                    try self.type_table.name(left_type, self.allocator),
                    exp.loc,
                );
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

                try self.checkEquality(bin_exp.op, left_type, right_type, exp.loc);

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

    fn checkEquality(self: *Self, op: ast.BinaryOp, left_type: tt.TypeId, right_type: tt.TypeId, loc: err.Loc) CompileError!void {
        const op_text = binaryOpText(op);
        const left_base = self.type_table.typeIdByNullableTypeId.get(left_type) orelse left_type;
        const right_base = self.type_table.typeIdByNullableTypeId.get(right_type) orelse right_type;
        const left_kind = self.type_table.getTypePtrById(left_base).kind;
        const right_kind = self.type_table.getTypePtrById(right_base).kind;

        if (left_kind == .null or right_kind == .null) {
            const other = if (left_kind == .null) right_type else left_type;
            if (self.type_table.getTypePtrById(other).is_nullable or self.type_table.getTypePtrById(other).kind == .null) return;
            return self.err_reporter.cantCompareWithNull(op_text, try self.type_table.name(other, self.allocator), loc);
        }
        if (left_kind == .string and right_kind == .string) {
            return self.err_reporter.cantCompareStrings(op_text, loc);
        }
        if (left_kind == .@"union" and right_kind == .@"union") {
            return self.err_reporter.cantCompareUnions(op_text, loc);
        }
        if (left_kind == .func_type and right_kind == .func_type) {
            return self.err_reporter.cantCompareFunctions(op_text, loc);
        }
        if (left_kind == .task and right_kind == .task) {
            return self.err_reporter.cantCompareTasks(op_text, loc);
        }
        if (self.isTypeNumber(left_base) and self.isTypeNumber(right_base)) return;
        if (self.type_table.isErrorSet(left_base) and self.type_table.isErrorSet(right_base)) return;
        if (left_base == right_base) return;
        return self.err_reporter.cantCompareTypes(
            op_text,
            try self.type_table.name(left_type, self.allocator),
            try self.type_table.name(right_type, self.allocator),
            loc,
        );
    }

    fn binaryOpType(self: *Self, op: ast.BinaryOp, left_type: tt.TypeId, right_type: tt.TypeId) ?tt.TypeId {
        const float_type = self.type_table.getPrimitive(.float);
        const int_type = self.type_table.getPrimitive(.int);
        const bool_type = self.type_table.getPrimitive(.boolean);
        switch (op) {
            .add, .sub, .mult, .mod, .div, .trunc_div => {
                if (!self.isTypeNumber(left_type) or !self.isTypeNumber(right_type)) return null;
                return switch (op) {
                    .div => float_type,
                    .trunc_div => int_type,
                    else => if (self.type_table.eql(left_type, float_type) or self.type_table.eql(right_type, float_type)) float_type else int_type,
                };
            },
            .lt, .lt_or_eq, .gt, .gt_or_eq => {
                if (!self.isTypeNumber(left_type) or !self.isTypeNumber(right_type)) return null;
                return bool_type;
            },
            .bool_or, .bool_and => {
                if (!self.type_table.eql(left_type, bool_type) or !self.type_table.eql(right_type, bool_type)) return null;
                return bool_type;
            },
            .eq, .not_eq => return bool_type,
        }
    }

    fn slotBinaryOpType(self: *Self, op: ast.BinaryOp, left_type: tt.TypeId, right_type: tt.TypeId, loc: err.Loc) CompileError!tt.TypeId {
        const op_text = binaryOpText(op);
        const is_equality = op == .eq or op == .not_eq;
        const null_type = self.type_table.getPrimitive(.null);
        if (is_equality and (left_type == null_type or right_type == null_type)) return self.type_table.getPrimitive(.boolean);

        const left_list = try self.operandTypeList(op_text, left_type, loc);
        const right_list = try self.operandTypeList(op_text, right_type, loc);
        const is_same_slot = self.slotBase(left_type) != null and self.slotBase(left_type) == self.slotBase(right_type);

        var results = SlotResults{};
        for (left_list, 0..) |left_member, left_index| {
            for (right_list, 0..) |right_member, right_index| {
                if (is_same_slot and left_index != right_index) continue;
                const left = try self.substituteSlot(left_type, left_member);
                const right = try self.substituteSlot(right_type, right_member);
                const slot_type = if (self.slotBase(left_type) != null) left_type else right_type;
                const member = if (self.slotBase(left_type) != null) left_member else right_member;
                if (is_equality and (self.isStringType(left) or self.isStringType(right))) {
                    return self.err_reporter.slotStringEquality(op_text, try self.type_table.name(slot_type, self.allocator), loc);
                }
                const result = self.binaryOpType(op, left, right) orelse return self.err_reporter.slotOperatorNotAllowedOn(
                    op_text,
                    try self.type_table.name(slot_type, self.allocator),
                    try self.type_table.syntaxName(member, self.allocator),
                    loc,
                );
                results.add(result, left_member, right_member);
            }
        }
        return self.slotResultType(&results, op_text, left_type, right_type, loc);
    }

    fn operandTypeList(self: *Self, op_text: []const u8, type_id: tt.TypeId, loc: err.Loc) CompileError![]const tt.TypeId {
        const slot_id = self.slotBase(type_id) orelse return self.allocator.dupe(tt.TypeId, &.{type_id});
        const type_list = self.type_table.getTypePtrById(slot_id).kind.slot.type_list;
        if (type_list.len == 0) return self.err_reporter.slotOperatorNotAllowed(op_text, try self.type_table.name(type_id, self.allocator), loc);
        return type_list;
    }

    fn slotResultType(self: *Self, results: *const SlotResults, op_text: []const u8, left_type: tt.TypeId, right_type: tt.TypeId, loc: err.Loc) CompileError!tt.TypeId {
        const differing = results.differing orelse return results.first.?.result;
        if (results.gives_left_member and self.slotBase(left_type) != null) return self.slotBase(left_type).?;
        if (results.gives_right_member and self.slotBase(right_type) != null) return self.slotBase(right_type).?;
        const first = results.first.?;
        const first_member = if (self.slotBase(left_type) != null) first.left_member else first.right_member;
        const second_member = if (self.slotBase(left_type) != null) differing.left_member else differing.right_member;
        return self.err_reporter.slotResultDiffers(
            op_text,
            try self.type_table.syntaxName(first_member, self.allocator),
            try self.type_table.syntaxName(first.result, self.allocator),
            try self.type_table.syntaxName(second_member, self.allocator),
            try self.type_table.syntaxName(differing.result, self.allocator),
            loc,
        );
    }

    fn slotBase(self: *Self, type_id: tt.TypeId) ?tt.TypeId {
        const base_id = self.type_table.typeIdByNullableTypeId.get(type_id) orelse type_id;
        if (self.type_table.getTypePtrById(base_id).kind != .slot) return null;
        return base_id;
    }

    fn substituteSlot(self: *Self, type_id: tt.TypeId, member: tt.TypeId) CompileError!tt.TypeId {
        if (self.slotBase(type_id) == null) return type_id;
        if (self.type_table.getTypePtrById(type_id).is_nullable) return self.type_table.getOrAddNullable(member);
        return member;
    }

    fn isStringType(self: *Self, type_id: tt.TypeId) bool {
        const base_id = self.type_table.typeIdByNullableTypeId.get(type_id) orelse type_id;
        return self.type_table.getTypePtrById(base_id).kind == .string;
    }

    fn binaryOpText(op: ast.BinaryOp) []const u8 {
        return switch (op) {
            .add => "+",
            .sub => "-",
            .mult => "*",
            .div => "/",
            .trunc_div => "//",
            .mod => "%",
            .eq => "==",
            .not_eq => "!=",
            .lt => "<",
            .lt_or_eq => "<=",
            .gt => ">",
            .gt_or_eq => ">=",
            .bool_or => "or",
            .bool_and => "and",
        };
    }

    fn isBoxedNullable(self: *Self, type_id: tt.TypeId) bool {
        const base = self.type_table.typeIdByNullableTypeId.get(type_id) orelse return false;
        return self.needsNullBox(type_id, base);
    }

    fn hasValue(self: *Self, reference: *ir.Value) CompileError!*ir.Value {
        const bool_type = self.type_table.getPrimitive(.boolean);
        if (!self.type_table.getTypePtrById(reference.type_id).is_nullable) {
            return ir.Value.init(self.allocator, .{ .data = .{ .i_bool = true }, .type_id = bool_type });
        }
        return ir.Value.init(self.allocator, .{
            .data = .{ .unary_op = .{ .kind = .box_not_null, .right = reference } },
            .type_id = bool_type,
        });
    }

    fn lowerNullableEquality(self: *Self, op: ast.BinaryOp, left: *ir.Value, right: *ir.Value, loc: err.Loc) CompileError!*ir.Value {
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

    fn coerceValue(self: *Self, value: *ir.Value, target_type: tt.TypeId, loc: err.Loc) CompileError!*ir.Value {
        if (!self.type_table.canCoerce(value.type_id, target_type) and try self.canCoerceThroughTypeList(value.type_id, target_type)) {
            return ir.Value.init(self.allocator, .{ .data = value.data, .type_id = target_type });
        }
        const promoted = try self.promoteToFloat(value, target_type);
        if (!try self.canCoerce(promoted.type_id, target_type, loc)) {
            return self.err_reporter.invalidType(
                try self.type_table.name(target_type, self.allocator),
                try self.type_table.name(promoted.type_id, self.allocator),
                loc,
            );
        }
        return self.boxForTarget(promoted, target_type);
    }

    fn boxForTarget(self: *Self, value: *ir.Value, target_type: tt.TypeId) CompileError!*ir.Value {
        if (!self.needsNullBox(target_type, value.type_id)) return value;
        return ir.Value.init(self.allocator, .{ .data = .{ .null_box_init = .{ .value = value } }, .type_id = target_type });
    }

    fn needsNullBox(self: *Self, target_type: tt.TypeId, value_type: tt.TypeId) bool {
        const target = self.type_table.getTypePtrById(target_type);
        if (!target.is_nullable) return false;
        if (value_type == self.type_table.getPrimitive(.null)) return false;
        if (self.type_table.getTypePtrById(value_type).is_nullable) return false;
        const base_id = self.type_table.typeIdByNullableTypeId.get(target_type).?;
        const base = self.type_table.getTypePtrById(base_id);
        return switch (base.kind) {
            .int, .byte, .float, .boolean, .@"enum" => true,
            else => false,
        };
    }

    pub fn resolveTypeAnnotation(self: *Self, type_annotation: *ast.TypeAnnotation, current_defined_type_type_id: ?tt.TypeId) !tt.TypeId {
        switch (type_annotation.type) {
            .primitive => |primitive_name| {
                if (std.mem.eql(u8, primitive_name, "float")) return if (type_annotation.is_nullable) try self.type_table.getOrAddNullable(self.type_table.getPrimitive(.float)) else self.type_table.getPrimitive(.float);
                if (std.mem.eql(u8, primitive_name, "byte")) return if (type_annotation.is_nullable) try self.type_table.getOrAddNullable(self.type_table.getPrimitive(.byte)) else self.type_table.getPrimitive(.byte);
                if (std.mem.eql(u8, primitive_name, "int")) return if (type_annotation.is_nullable) try self.type_table.getOrAddNullable(self.type_table.getPrimitive(.int)) else self.type_table.getPrimitive(.int);
                if (std.mem.eql(u8, primitive_name, "buffer")) return if (type_annotation.is_nullable) try self.type_table.getOrAddNullable(self.type_table.getPrimitive(.buffer)) else self.type_table.getPrimitive(.buffer);
                if (std.mem.eql(u8, primitive_name, "string")) return if (type_annotation.is_nullable) try self.type_table.getOrAddNullable(self.type_table.getPrimitive(.string)) else self.type_table.getPrimitive(.string);
                if (std.mem.eql(u8, primitive_name, "bool")) return if (type_annotation.is_nullable) try self.type_table.getOrAddNullable(self.type_table.getPrimitive(.boolean)) else self.type_table.getPrimitive(.boolean);
                if (std.mem.eql(u8, primitive_name, "void")) {
                    return self.type_table.getPrimitive(.void);
                }
                if (std.mem.eql(u8, primitive_name, "dynamic")) return if (type_annotation.is_nullable) try self.type_table.getOrAddNullable(self.type_table.getPrimitive(.dynamic)) else self.type_table.getPrimitive(.dynamic);

                return CompileError.SemaError;
            },
            .@"struct" => |struct_name| {
                if (self.is_in_prelude and std.mem.eql(u8, struct_name, "dynamic")) {
                    return self.type_table.getPrimitive(.dynamic);
                }
                const symbol = self.scope.symbol_table.get(struct_name) orelse {
                    return self.err_reporter.typeNotDefined(struct_name, type_annotation.loc);
                };
                if (symbol.kind == .task_type) {
                    const task_type_id = try self.resolveTaskTypeAnnotation(type_annotation, current_defined_type_type_id);
                    return if (type_annotation.is_nullable) try self.type_table.getOrAddNullable(task_type_id) else task_type_id;
                }
                if (symbol.kind != .generic_type and type_annotation.args.len > 0) {
                    return self.err_reporter.wrongSlotCount(struct_name, 0, type_annotation.args.len, type_annotation.loc);
                }
                const blueprint_type_id = switch (symbol.kind) {
                    .defined_type => |dt| dt.blueprint_type_id,
                    .type_slot => symbol.type_id,
                    .trait => return self.err_reporter.traitUsedAsType(struct_name, type_annotation.loc),
                    .generic_type => |index| try self.resolveGenericTypeAnnotation(index, type_annotation, current_defined_type_type_id),
                    else => return CompileError.SemaError,
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
                return CompileError.SemaError;
            },
            .function => |func| {
                const type_id = try self.resolveFuncTypeAnnotation(func, current_defined_type_type_id, type_annotation.loc);
                return if (type_annotation.is_nullable) try self.type_table.getOrAddNullable(type_id) else type_id;
            },
            .resolved => |type_id| return type_id,
        }
    }

    fn resolveTaskTypeAnnotation(self: *Self, annotation: *ast.TypeAnnotation, current_defined_type_type_id: ?tt.TypeId) CompileError!tt.TypeId {
        const has_valid_slot_count = annotation.args.len == 1 or annotation.args.len == 2;
        if (!has_valid_slot_count) {
            return self.err_reporter.wrongSlotCount(task_type_name, 2, annotation.args.len, annotation.loc);
        }
        const payload = try self.resolveTypeAnnotation(@constCast(annotation.args[0]), current_defined_type_type_id);
        if (annotation.args.len == 1) {
            return self.type_table.getOrAddTask(.{ .payload = payload, .error_set = self.none_error_set });
        }
        const error_set = try self.resolveTypeAnnotation(@constCast(annotation.args[1]), current_defined_type_type_id);
        if (!self.type_table.isErrorSet(error_set)) {
            return self.err_reporter.slotNeedsErrorSet("E", try self.type_table.syntaxName(error_set, self.allocator), annotation.args[1].loc);
        }
        return self.type_table.getOrAddTask(.{ .payload = payload, .error_set = error_set });
    }

    fn resolveFuncTypeAnnotation(self: *Self, func: ast.FuncTypeAnnotation, current_defined_type_type_id: ?tt.TypeId, loc: err.Loc) CompileError!tt.TypeId {
        const params = try self.allocator.alloc(tt.FuncTypeParam, func.params.len);
        for (func.params, params) |param, *resolved| {
            const type_id = try self.resolveTypeAnnotation(param.type, current_defined_type_type_id);
            const is_checked_mut = param.is_mut and !self.namesSlot(param.type);
            if (is_checked_mut and !self.canBeMutable(type_id)) {
                return self.err_reporter.primitiveParamsCantBeMutable(loc);
            }
            resolved.* = .{ .type_id = type_id, .is_mut = param.is_mut };
        }
        const return_type_id = try self.resolveFuncReturnType(func, current_defined_type_type_id, loc);
        return self.type_table.getOrAddFuncType(.{ .params = params, .return_type_id = return_type_id });
    }

    fn resolveFuncReturnType(self: *Self, func: ast.FuncTypeAnnotation, current_defined_type_type_id: ?tt.TypeId, loc: err.Loc) CompileError!tt.TypeId {
        const payload = try self.resolveTypeAnnotation(func.return_type, current_defined_type_type_id);
        const set_name = func.error_set orelse return payload;
        const error_set_type_id = try self.resolveFnErrorSet(.{ .named = .{ .identifier = set_name, .loc = loc } }, "");
        return self.errorUnionType(error_set_type_id, payload);
    }

    fn errorUnionType(self: *Self, error_set: tt.TypeId, payload: tt.TypeId) CompileError!tt.TypeId {
        if (self.errorSetOf(error_set).kind == .none) return payload;
        return self.type_table.addType(.{
            .kind = .{ .error_union = .{ .error_set = error_set, .payload = payload } },
            .is_nullable = false,
        });
    }

    fn namesSlot(self: *Self, annotation: *const ast.TypeAnnotation) bool {
        if (annotation.type != .@"struct") return false;
        const symbol = self.scope.symbol_table.get(annotation.type.@"struct") orelse return false;
        return symbol.kind == .type_slot;
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
    module: *ModuleScope,
};

const Scope = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    symbol_table: *SymbolTable,
    return_type_id: tt.TypeId,
    next_uid: usize,
    next_fn_uid: usize,

    pub fn init(alloc: std.mem.Allocator, return_type_id: tt.TypeId) !Self {
        return Self{
            .allocator = alloc,
            .symbol_table = try SymbolTable.init(alloc, null, std.StringHashMap(Symbol).init(alloc)),
            .return_type_id = return_type_id,
            .next_uid = 0,
            .next_fn_uid = 0,
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
    module: ?*ModuleScope = null,

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
            return CompileError.SemaError;
        }
        if (self.module) |module| {
            if (module.selected.contains(symbol.identifier)) {
                return CompileError.SemaError;
            }
            const can_reuse_prelude_names = module.bindings != null;
            const is_prelude_name = if (self.parent) |parent| parent.getPublic(symbol.identifier) != null else false;
            if (is_prelude_name and !can_reuse_prelude_names) {
                return CompileError.SemaError;
            }
        }

        try self.symbols.put(symbol.identifier, symbol);
    }

    fn remove(self: *Self, identifier: []const u8) bool {
        return self.symbols.remove(identifier);
    }

    fn get(self: *Self, identifier: []const u8) ?Symbol {
        if (self.symbols.get(identifier)) |s| return s;
        if (self.module) |module| {
            if (module.selected.get(identifier)) |s| return s;
            if (module.getQualified(identifier)) |s| return s;
        }
        if (self.parent) |parent| {
            if (self.module != null) return parent.getPublic(identifier);
            return parent.get(identifier);
        }
        return null;
    }

    fn getPublic(self: *Self, identifier: []const u8) ?Symbol {
        const symbol = self.get(identifier) orelse return null;
        if (symbol.is_file_only) return null;
        return symbol;
    }

    fn getOrError(self: *Self, id: []const u8) !Symbol {
        if (self.get(id)) |s| return s;
        return CompileError.SemaError;
    }
};

const ModuleScope = struct {
    const Self = @This();

    path: []const u8,
    src: []const u8,
    root: *SymbolTable,
    is_prelude: bool,
    file_path: ?[]const u8 = null,
    qualifier: ?[]const u8 = null,
    bindings: ?*const native.BindingMap = null,
    imports: std.StringArrayHashMapUnmanaged(*ModuleScope) = .empty,
    selected: std.StringHashMapUnmanaged(Symbol) = .empty,
    hidden_names: std.StringHashMapUnmanaged(void) = .empty,

    fn create(allocator: std.mem.Allocator, path: []const u8, src: []const u8, root: *SymbolTable, is_prelude: bool) !*Self {
        const ptr = try allocator.create(Self);
        ptr.* = .{ .path = path, .src = src, .root = root, .is_prelude = is_prelude };
        return ptr;
    }

    fn getQualified(self: *Self, identifier: []const u8) ?Symbol {
        const separator = std.mem.indexOf(u8, identifier, "::") orelse return null;
        const target = self.imports.get(identifier[0..separator]) orelse return null;
        const symbol = target.root.symbols.get(identifier[separator + 2 ..]) orelse return null;
        if (symbol.is_file_only) return null;
        return symbol;
    }
};

const VisitedBlock = struct {
    block: ir.Block,
    is_returning: bool,
};

const ExitQuery = enum { returns, leaves_block };

const Constant = struct {
    let_stmt: ast.LetStmt,
    loc: err.Loc,
    module: *ModuleScope,
    value: ?*ir.Value = null,
    is_resolving: bool = false,
};

pub const Symbol = struct {
    const Self = @This();

    uid: usize,
    identifier: []const u8,
    type_id: tt.TypeId,
    is_file_only: bool = false,

    kind: union(enum) {
        variable: struct {
            is_mut: bool,
            is_borrowed: bool = false,
        },
        defined_type: struct {
            blueprint_type_id: tt.TypeId,
        },
        function: void,
        constant: usize,
        generic_function: usize,
        generic_type: usize,
        type_slot: void,
        trait: usize,
        task_type: void,
    },
};


const LambdaContext = struct {
    floor: usize,
    captures: std.ArrayList(LambdaCapture) = .empty,
    body_depth: usize = 0,
    is_return_inferred: bool = false,
    return_payload: ?tt.TypeId = null,
    error_set: ?tt.TypeId = null,
};

const LambdaHints = struct {
    params: []const ?tt.FuncTypeParam,
    return_type_id: ?tt.TypeId,
};

const TypedLambda = struct {
    def: *const ast.FnDef,
    is_return_inferred: bool,
};

const LambdaCapture = struct {
    symbol_uid: usize,
    type_id: tt.TypeId,
    is_boxed: bool,
    source: *ir.Value,
};

const WhenSubject = struct {
    value: *ir.Value,
    type_name: []const u8,
    is_nullable: bool,
    base_type: tt.TypeId,
    base_kind: @FieldType(tt.Type, "kind"),
    is_mutable: bool,
    exp: *const ast.ExpNode,
};

const GenericFunction = struct {
    func_stmt: *const ast.FuncStmt,
    loc: err.Loc,
    module: *ModuleScope,
    extern_uid: ?usize = null,
    copies: std.ArrayList(GenericCopy) = .empty,
};

const CopyLowering = struct {
    module: *ModuleScope,
    depth: usize,
    copy_context: ?err.CopyContext,
    is_lowering_copy: bool,
    has_none_error_slot: bool,
};

const GenericCopy = struct {
    args: []const tt.TypeId,
    symbol: Symbol,
    call_loc: err.Loc,
    call_src: []const u8,
    depth: usize,
};

const GenericType = struct {
    identifier: []const u8,
    slots: []const ast.Slot,
    def: union(enum) {
        @"struct": *const ast.Struct,
        @"union": *const ast.UnionDef,
    },
    loc: err.Loc,
    module: *ModuleScope,
    copies: std.ArrayList(GenericCopy) = .empty,
    opaque_copy: ?Symbol = null,
};

const Trait = struct {
    trait_stmt: *const ast.TraitStmt,
    loc: err.Loc,
    module: *ModuleScope,
    constraint: ?Constraint = null,
    is_resolving: bool = false,
};

const Constraint = struct {
    type_list: []const tt.TypeId = &.{},
    methods: []const TraitMethod = &.{},
};

const TraitMethod = struct {
    func_stmt: *const ast.FuncStmt,
    loc: err.Loc,
    module: *ModuleScope,
};

const PendingTraitCheck = struct {
    slot: ast.Slot,
    type_id: tt.TypeId,
    loc: err.Loc,
};

const SlotResults = struct {
    const Entry = struct {
        result: tt.TypeId,
        left_member: tt.TypeId,
        right_member: tt.TypeId,
    };

    first: ?Entry = null,
    differing: ?Entry = null,
    gives_left_member: bool = true,
    gives_right_member: bool = true,

    fn add(self: *SlotResults, result: tt.TypeId, left_member: tt.TypeId, right_member: tt.TypeId) void {
        const entry = Entry{ .result = result, .left_member = left_member, .right_member = right_member };
        if (result != left_member) self.gives_left_member = false;
        if (result != right_member) self.gives_right_member = false;
        const first = self.first orelse {
            self.first = entry;
            return;
        };
        if (self.differing == null and first.result != result) self.differing = entry;
    }
};

const TypeCopy = struct {
    generic: usize,
    args: []const tt.TypeId,
};

const PendingCopy = struct {
    kind: enum { function, type },
    generic: usize,
    copy: usize,
};

const PendingWhenCheck = struct {
    when_stmt: ast.WhenStmt,
    values: [][]*ir.Value,
    set_type_id: tt.TypeId,
    is_null_missing: bool,
    when_loc: err.Loc,
    module: *ModuleScope,
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
    seen_strings: std.StringHashMap(void),
    seen_variants: std.StringHashMap(void),
    has_null_case: bool,
};

const JumpTableEntry = struct {
    value: i64,
    case_index: ?usize,
};

const WhenSubjectRefs = struct {
    subject: *ir.Value,
    unwrapped: *ir.Value,
};

const LoweredProng = struct {
    condition: *ir.Value,
    body: ir.Block,
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
