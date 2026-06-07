const std = @import("std");
const wasm_builtins = @import("wasm_builtins.zig");
const wasmtime = @import("wasmtime.zig");
const sema = @import("sema.zig");
const wt = wasmtime.wasmtime;

pub const WasmRuntime = struct {
    const Self = @This();

    engine: *wt.wasm_engine_t,
    store: *wt.wasmtime_store_t,
    linker: *wt.wasmtime_linker_t,
    context: ?*wt.wasmtime_context_t,
    type_table: *sema.TypeTable,
    wasm_builtins_instance: wasm_builtins.WasmBuiltins,

    pub fn init(writer: *std.Io.Writer, type_table: *sema.TypeTable) !Self {
        const engine = wt.wasm_engine_new() orelse return error.EngineInitFailed;

        const store = wt.wasmtime_store_new(engine, null, null) orelse {
            wt.wasm_engine_delete(engine);
            return error.StoreInitFailed;
        };

        const linker = wt.wasmtime_linker_new(engine) orelse {
            wt.wasmtime_store_delete(store);
            wt.wasm_engine_delete(engine);
            return error.LinkerInitFailed;
        };

        const context = wt.wasmtime_store_context(store);

        const wasm_builtins_instance = try wasm_builtins.WasmBuiltins.init(std.heap.c_allocator, writer);

        var self = Self{
            .engine = engine,
            .store = store,
            .linker = linker,
            .context = context,
            .type_table = type_table,
            .wasm_builtins_instance = wasm_builtins_instance,
        };

        try self.registerHostImports(writer);

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.wasm_builtins_instance.deinit();
        wt.wasmtime_linker_delete(self.linker);
        wt.wasmtime_store_delete(self.store);
        wt.wasm_engine_delete(self.engine);
    }

    pub fn eval(self: *Self, wasm_bytes: []const u8) !void {
        var module: ?*wt.wasmtime_module_t = undefined;
        defer wt.wasmtime_module_delete(module.?);

        const compile_err = wt.wasmtime_module_new(self.engine, wasm_bytes.ptr, wasm_bytes.len, &module);
        if (compile_err) |e| {
            defer wt.wasmtime_error_delete(e);
            std.log.err("WASM compile failed", .{});
            return error.CompileFailed;
        }

        var instance: wt.wasmtime_instance_t = undefined;
        var trap: ?*wt.wasm_trap_t = undefined;
        const inst_err = wt.wasmtime_linker_instantiate(self.linker, self.context, module.?, &instance, &trap);

        if (inst_err) |e| {
            defer wt.wasmtime_error_delete(e);
            std.log.err("WASM instantiate failed", .{});
            return error.InstantiateFailed;
        }

        if (trap) |t| {
            defer wt.wasm_trap_delete(t);
            std.log.err("WASM trap", .{});
            return error.RuntimeTrap;
        }
    }

    fn registerHostImports(self: *Self, writer: *std.Io.Writer) !void {
        const writer_ptr: *anyopaque = @ptrCast(@alignCast(writer));

        for (self.wasm_builtins_instance.builtins) |def| {
            switch (def.kind) {
                .host_import => |host| {
                    const name = std.fmt.allocPrint(std.heap.c_allocator, "{s}_{d}", .{ def.identifier, def.uid }) catch unreachable;
                    defer std.heap.c_allocator.free(name);
                    try self.defineHostFunc(name, host.params, host.results, host.callback, writer_ptr);
                },
                .host_import_raw => |raw| {
                    try self.defineRawHostFunc(def.identifier, raw.param_kinds, raw.result_kinds, raw.callback, writer_ptr);
                },
                .inline_expr => {},
            }
        }
    }

    fn defineHostFunc(
        self: *Self,
        name: []const u8,
        params: []const sema.TypeId,
        results: []const sema.TypeId,
        callback: wt.wasmtime_func_callback_t,
        writer_ptr: *anyopaque,
    ) !void {
        var param_types: std.ArrayList(?*wt.wasm_valtype_t) = .empty;
        defer param_types.deinit(std.heap.c_allocator);

        for (params) |type_id| {
            const v = wt.wasm_valtype_new(self.typeIdToWasmtimeType(self.type_table, type_id));
            try param_types.append(std.heap.c_allocator, v);
        }

        var result_types: std.ArrayList(?*wt.wasm_valtype_t) = .empty;
        defer result_types.deinit(std.heap.c_allocator);
        for (results) |type_id| {
            const v = wt.wasm_valtype_new(self.typeIdToWasmtimeType(self.type_table, type_id));
            try result_types.append(std.heap.c_allocator, v);
        }

        var params_vec: wt.wasm_valtype_vec_t = undefined;
        var results_vec: wt.wasm_valtype_vec_t = undefined;
        wt.wasm_valtype_vec_new(&params_vec, param_types.items.len, @ptrCast(param_types.items.ptr));
        wt.wasm_valtype_vec_new(&results_vec, result_types.items.len, @ptrCast(result_types.items.ptr));

        const func_type = wt.wasm_functype_new(&params_vec, &results_vec);
        defer wt.wasm_functype_delete(func_type);

        const err = wt.wasmtime_linker_define_func(
            self.linker,
            "env",
            3,
            name.ptr,
            name.len,
            func_type,
            callback,
            writer_ptr,
            null,
        );

        if (err) |e| {
            defer wt.wasmtime_error_delete(e);
            wt.wasmtime_linker_delete(self.linker);
            wt.wasmtime_store_delete(self.store);
            wt.wasm_engine_delete(self.engine);
            return error.LinkerFuncFailed;
        }
    }

    fn defineRawHostFunc(
        self: *Self,
        name: []const u8,
        param_kinds: []const wt.wasm_valkind_t,
        result_kinds: []const wt.wasm_valkind_t,
        callback: wt.wasmtime_func_callback_t,
        writer_ptr: *anyopaque,
    ) !void {
        var param_types: std.ArrayList(?*wt.wasm_valtype_t) = .empty;
        defer param_types.deinit(std.heap.c_allocator);

        for (param_kinds) |kind| {
            try param_types.append(std.heap.c_allocator, wt.wasm_valtype_new(kind));
        }

        var result_types: std.ArrayList(?*wt.wasm_valtype_t) = .empty;
        defer result_types.deinit(std.heap.c_allocator);
        for (result_kinds) |kind| {
            try result_types.append(std.heap.c_allocator, wt.wasm_valtype_new(kind));
        }

        var params_vec: wt.wasm_valtype_vec_t = undefined;
        var results_vec: wt.wasm_valtype_vec_t = undefined;
        wt.wasm_valtype_vec_new(&params_vec, param_types.items.len, @ptrCast(param_types.items.ptr));
        wt.wasm_valtype_vec_new(&results_vec, result_types.items.len, @ptrCast(result_types.items.ptr));

        const func_type = wt.wasm_functype_new(&params_vec, &results_vec);
        defer wt.wasm_functype_delete(func_type);

        const err = wt.wasmtime_linker_define_func(
            self.linker,
            "env",
            3,
            name.ptr,
            name.len,
            func_type,
            callback,
            writer_ptr,
            null,
        );

        if (err) |e| {
            defer wt.wasmtime_error_delete(e);
            wt.wasmtime_linker_delete(self.linker);
            wt.wasmtime_store_delete(self.store);
            wt.wasm_engine_delete(self.engine);
            return error.LinkerFuncFailed;
        }
    }

    fn typeIdToWasmtimeType(_: *Self, type_table: *sema.TypeTable, type_id: sema.TypeId) wt.wasm_valkind_t {
        const t = type_table.getTypePtrById(type_id);
        return switch (t.kind) {
            .int => wt.WASMTIME_I64,
            .float => wt.WASMTIME_F64,
            .boolean => wt.WASMTIME_I32,
            .string => wt.WASMTIME_ANYREF,
            else => unreachable,
        };
    }
};
