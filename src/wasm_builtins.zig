const std = @import("std");
const binaryen = @import("binaryen.zig");
const wasmtime = @import("wasmtime.zig");
const sema = @import("sema.zig");

const B = binaryen.binaryen;
const wt = wasmtime.wasmtime;
const TypeId = sema.TypeId;
const TypeTable = sema.TypeTable;

pub const HostImportDef = struct {
    params: []const TypeId,
    results: []const TypeId,
    callback: wt.wasmtime_func_callback_t,
};

pub const InlineExprFn = *const fn (module: B.BinaryenModuleRef, arg: B.BinaryenExpressionRef) B.BinaryenExpressionRef;

pub const BuiltIn = struct {
    uid: u32,
    identifier: []const u8,
    kind: union(enum) {
        host_import: HostImportDef,
        inline_expr: InlineExprFn,
    },
};

pub const WasmBuiltins = struct {
    const Self = @This();

    pub var stdout_writer: *std.Io.Writer = undefined;

    builtins: []const BuiltIn,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, type_table: *const TypeTable, writer: ?*std.Io.Writer) !Self {
        if (writer) |w| stdout_writer = w;
        const builtin_arr = try allocator.alloc(BuiltIn, 2);
        builtin_arr[0] = try echoBuiltIn(allocator, type_table);
        builtin_arr[1] = assertBuiltIn();
        return Self{ .builtins = builtin_arr, .allocator = allocator };
    }

    pub fn deinit(self: *Self) void {
        for (self.builtins) |builtin| {
            switch (builtin.kind) {
                .host_import => |host| {
                    if (host.params.len > 0) self.allocator.free(host.params);
                    if (host.results.len > 0) self.allocator.free(host.results);
                },
                .inline_expr => {},
            }
        }
        self.allocator.free(self.builtins);
    }

    pub fn getBuiltin(self: *const Self, uid: usize) ?BuiltIn {
        for (self.builtins) |def| {
            if (def.uid == uid) return def;
        }
        return null;
    }
};

fn echoBuiltIn(allocator: std.mem.Allocator, type_table: *const TypeTable) !BuiltIn {
    const params = try allocator.alloc(TypeId, 1);
    params[0] = type_table.getPrimitive(.int);
    return BuiltIn{
        .uid = 0,
        .identifier = "echo",
        .kind = .{ .host_import = .{
            .params = params,
            .results = &.{},
            .callback = echoCallback,
        }},
    };
}

fn assertBuiltIn() BuiltIn {
    return BuiltIn{
        .uid = 6,
        .identifier = "assert",
        .kind = .{ .inline_expr = assertExpr },
    };
}

fn assertExpr(module: B.BinaryenModuleRef, arg: B.BinaryenExpressionRef) B.BinaryenExpressionRef {
    const not_cond = B.BinaryenUnary(module, B.BinaryenEqZInt32(), arg);
    return B.BinaryenIf(module, not_cond, B.BinaryenUnreachable(module), B.BinaryenNop(module));
}

fn echoCallback(
    _: ?*anyopaque,
    _: ?*wt.wasmtime_caller_t,
    args: [*c]const wt.wasmtime_val_t,
    _: usize,
    _: [*c]wt.wasmtime_val_t,
    _: usize,
) callconv(.c) ?*wt.wasm_trap_t {
    WasmBuiltins.stdout_writer.print("{d}\n", .{args[0].of.i64}) catch {};
    WasmBuiltins.stdout_writer.flush() catch {};
    return null;
}
