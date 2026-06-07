const std = @import("std");
const binaryen = @import("binaryen.zig");
const wasmtime = @import("wasmtime.zig");
const sema = @import("sema.zig");
const ir = @import("ir.zig");

const B = binaryen.binaryen;
const wt = wasmtime.wasmtime;
const TypeId = sema.TypeId;
const TypeTable = sema.TypeTable;

pub const HostImportDef = struct {
    params: []const TypeId,
    results: []const TypeId,
    callback: wt.wasmtime_func_callback_t,
};

const HostImportRaw = struct {
    params: B.BinaryenType,
    results: B.BinaryenType,
    param_kinds: []const wt.wasm_valkind_t,
    result_kinds: []const wt.wasm_valkind_t,
    callback: wt.wasmtime_func_callback_t,
};

pub const InlineExprFn = *const fn (
    module: B.BinaryenModuleRef,
    allocator: std.mem.Allocator,
    type_table: *TypeTable,
    fc: ir.FnCall,
    ctx: *anyopaque,
    genValue: *const fn (*anyopaque, *ir.Value) anyerror!B.BinaryenExpressionRef,
) anyerror!B.BinaryenExpressionRef;

pub const BuiltIn = struct {
    uid: u32,
    identifier: []const u8,
    kind: union(enum) {
        host_import: HostImportDef,
        host_import_raw: HostImportRaw,
        inline_expr: InlineExprFn,
    },
};

pub const WasmBuiltins = struct {
    const Self = @This();

    pub var stdout_writer: *std.Io.Writer = undefined;

    builtins: []const BuiltIn,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, writer: ?*std.Io.Writer) !Self {
        if (writer) |w| stdout_writer = w;
        const builtin_arr = try allocator.alloc(BuiltIn, 5);
        builtin_arr[0] = echoExprBuiltIn();
        builtin_arr[1] = assertBuiltIn();
        builtin_arr[2] = try printStrBuiltIn(allocator);
        builtin_arr[3] = concatBuiltIn();
        builtin_arr[4] = try echoIntBuiltIn(allocator);
        return Self{ .builtins = builtin_arr, .allocator = allocator };
    }

    pub fn deinit(self: *Self) void {
        for (self.builtins) |builtin| {
            switch (builtin.kind) {
                .host_import => |host| {
                    if (host.params.len > 0) self.allocator.free(host.params);
                    if (host.results.len > 0) self.allocator.free(host.results);
                },
                .host_import_raw => |raw| {
                    if (raw.param_kinds.len > 0) self.allocator.free(raw.param_kinds);
                    if (raw.result_kinds.len > 0) self.allocator.free(raw.result_kinds);
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

fn echoExprBuiltIn() BuiltIn {
    return BuiltIn{
        .uid = 0,
        .identifier = "echo",
        .kind = .{ .inline_expr = echoExpr },
    };
}

fn echoExpr(
    module: B.BinaryenModuleRef,
    allocator: std.mem.Allocator,
    type_table: *TypeTable,
    fc: ir.FnCall,
    ctx: *anyopaque,
    genValueFn: *const fn (*anyopaque, *ir.Value) anyerror!B.BinaryenExpressionRef,
) anyerror!B.BinaryenExpressionRef {
    if (fc.args.len == 0) return B.BinaryenNop(module);

    const arg = fc.args[0];
    const arg_type = type_table.getTypePtrById(arg.type_id);

    if (arg_type.kind == .string) {
        return echoString(module, allocator, arg);
    }

    if (arg_type.kind == .boolean) {
        return echoBool(module, allocator, ctx, genValueFn, arg);
    }

    const arg_val = try genValueFn(ctx, arg);
    const converted = if (arg_type.kind == .float)
        B.BinaryenUnary(module, B.BinaryenTruncSFloat64ToInt64(), arg_val)
    else
        arg_val;

    const result_args = try allocator.alloc(B.BinaryenExpressionRef, 1);
    result_args[0] = converted;
    return B.BinaryenCall(module, "echo_int", result_args.ptr, 1, B.BinaryenTypeNone());
}

fn echoString(module: B.BinaryenModuleRef, allocator: std.mem.Allocator, arg: *ir.Value) anyerror!B.BinaryenExpressionRef {
    return switch (arg.data) {
        .i_string => |s| {
            const str_block = try emitPrintStringBlock(module, allocator, s);
            const nl_block = try emitPrintStringBlock(module, allocator, "\n");
            return bineBlock(module, &.{ str_block, nl_block });
        },
        .fn_call => |inner_fc| {
            if (inner_fc.fn_uid == 4 and inner_fc.args[0].data == .i_string and inner_fc.args[1].data == .i_string) {
                const left = inner_fc.args[0].data.i_string;
                const right = inner_fc.args[1].data.i_string;
                const result = try std.fmt.allocPrint(allocator, "{s}{s}\n", .{ left, right });
                return emitPrintStringBlock(module, allocator, result);
            }
            return B.BinaryenNop(module);
        },
        else => B.BinaryenNop(module),
    };
}

fn echoBool(
    module: B.BinaryenModuleRef,
    allocator: std.mem.Allocator,
    ctx: *anyopaque,
    genValueFn: *const fn (*anyopaque, *ir.Value) anyerror!B.BinaryenExpressionRef,
    arg: *ir.Value,
) anyerror!B.BinaryenExpressionRef {
    const bool_val = try genValueFn(ctx, arg);
    const is_false = B.BinaryenUnary(module, B.BinaryenEqZInt32(), bool_val);
    const then_block = try emitPrintStringBlock(module, allocator, "false\n");
    const else_block = try emitPrintStringBlock(module, allocator, "true\n");
    return B.BinaryenIf(module, is_false, then_block, else_block);
}

fn emitPrintStringBlock(module: B.BinaryenModuleRef, allocator: std.mem.Allocator, s: []const u8) anyerror!B.BinaryenExpressionRef {
    const children = try allocator.alloc(B.BinaryenExpressionRef, s.len + 1);
    for (s, 0..) |byte, i| {
        children[i] = B.BinaryenStore(
            module,
            1,
            @intCast(i),
            0,
            B.BinaryenConst(module, B.BinaryenLiteralInt32(0)),
            B.BinaryenConst(module, B.BinaryenLiteralInt32(byte)),
            B.BinaryenTypeInt32(),
            "memory",
        );
    }
    const zero = B.BinaryenConst(module, B.BinaryenLiteralInt32(0));
    const len = B.BinaryenConst(module, B.BinaryenLiteralInt32(@intCast(s.len)));
    const call_args = try allocator.alloc(B.BinaryenExpressionRef, 2);
    call_args[0] = zero;
    call_args[1] = len;
    children[s.len] = B.BinaryenCall(module, "print_str", call_args.ptr, 2, B.BinaryenTypeNone());
    return bineBlock(module, children);
}

fn bineBlock(module: B.BinaryenModuleRef, children: []const B.BinaryenExpressionRef) B.BinaryenExpressionRef {
    return B.BinaryenBlock(module, null, @ptrCast(@constCast(children.ptr)), @intCast(children.len), B.BinaryenTypeNone());
}

fn assertBuiltIn() BuiltIn {
    return BuiltIn{
        .uid = 6,
        .identifier = "assert",
        .kind = .{ .inline_expr = assertExpr },
    };
}

fn assertExpr(
    module: B.BinaryenModuleRef,
    _: std.mem.Allocator,
    _: *TypeTable,
    fc: ir.FnCall,
    ctx: *anyopaque,
    genValueFn: *const fn (*anyopaque, *ir.Value) anyerror!B.BinaryenExpressionRef,
) anyerror!B.BinaryenExpressionRef {
    const arg_val = try genValueFn(ctx, fc.args[0]);
    const not_cond = B.BinaryenUnary(module, B.BinaryenEqZInt32(), arg_val);
    return B.BinaryenIf(module, not_cond, B.BinaryenUnreachable(module), B.BinaryenNop(module));
}

fn echoIntBuiltIn(allocator: std.mem.Allocator) !BuiltIn {
    const params = B.BinaryenTypeInt64();

    const param_kinds = try allocator.alloc(wt.wasm_valkind_t, 1);
    param_kinds[0] = wt.WASMTIME_I64;

    return BuiltIn{
        .uid = 7,
        .identifier = "echo_int",
        .kind = .{ .host_import_raw = .{
            .params = params,
            .results = B.BinaryenTypeNone(),
            .param_kinds = param_kinds,
            .result_kinds = &.{},
            .callback = echoIntCallback,
        } },
    };
}

fn echoIntCallback(
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

fn printStrBuiltIn(allocator: std.mem.Allocator) !BuiltIn {
    var param_types = [_]B.BinaryenType{ B.BinaryenTypeInt32(), B.BinaryenTypeInt32() };
    const params = B.BinaryenTypeCreate(&param_types, 2);

    const param_kinds = try allocator.alloc(wt.wasm_valkind_t, 2);
    param_kinds[0] = wt.WASMTIME_I32;
    param_kinds[1] = wt.WASMTIME_I32;

    return BuiltIn{
        .uid = 999,
        .identifier = "print_str",
        .kind = .{ .host_import_raw = .{
            .params = params,
            .results = B.BinaryenTypeNone(),
            .param_kinds = param_kinds,
            .result_kinds = &.{},
            .callback = printStrCallback,
        } },
    };
}

fn printStrCallback(
    _: ?*anyopaque,
    caller: ?*wt.wasmtime_caller_t,
    args: [*c]const wt.wasmtime_val_t,
    _: usize,
    _: [*c]wt.wasmtime_val_t,
    _: usize,
) callconv(.c) ?*wt.wasm_trap_t {
    const ptr: usize = @intCast(args[0].of.i32);
    const len: usize = @intCast(args[1].of.i32);

    var extern_val: wt.wasmtime_extern_t = undefined;
    _ = wt.wasmtime_caller_export_get(caller, "memory", 6, &extern_val);

    const context = wt.wasmtime_caller_context(caller);
    const mem_data = wt.wasmtime_memory_data(context, &extern_val.of.memory);

    WasmBuiltins.stdout_writer.writeAll(mem_data[ptr .. ptr + len]) catch {};
    WasmBuiltins.stdout_writer.flush() catch {};
    return null;
}

fn concatBuiltIn() BuiltIn {
    return BuiltIn{
        .uid = 4,
        .identifier = "concat",
        .kind = .{ .inline_expr = concatExpr },
    };
}

fn concatExpr(
    module: B.BinaryenModuleRef,
    _: std.mem.Allocator,
    _: *TypeTable,
    fc: ir.FnCall,
    ctx: *anyopaque,
    genValueFn: *const fn (*anyopaque, *ir.Value) anyerror!B.BinaryenExpressionRef,
) anyerror!B.BinaryenExpressionRef {
    const left = try genValueFn(ctx, fc.args[0]);
    const right = try genValueFn(ctx, fc.args[1]);
    return B.BinaryenStringConcat(module, left, right);
}
