const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const native = @import("../native.zig");
const api = @import("../../tasks/sqlite_api.zig");
const sqlite = @import("../../tasks/sqlite.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const bytesArg = native.bytesArg;
const SqliteError = sqlite.SqliteError;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "version", Binding{ .native = &version } },
    .{ "open_handle", Binding{ .native = &openHandle } },
    .{ "execute_handle", Binding{ .native = &executeHandle } },
    .{ "execute_with_handle", Binding{ .native = &executeWithHandle } },
    .{ "close_handle", Binding{ .native = &closeHandle } },
    .{ "error_offset", Binding{ .native = &errorOffset } },
    .{ "take_error_message", Binding{ .native = &takeErrorMessage } },
    .{ "own", Binding{ .native = &native.ownResource } },
    .{ "release", Binding{ .native = &native.releaseResource } },
});

pub const error_sets = .{SqliteError};

fn version(vm: *VM, _: []v.Value) anyerror!NativeResult {
    return native.ok(try vm.allocBytesValue(std.mem.span(api.sqlite3_libversion()), .string));
}

fn openHandle(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const path = bytesArg(args, 0);
    if (std.mem.indexOfScalar(u8, path, 0) != null) {
        return native.fail(SqliteError.CantOpen);
    }
    const settings: sqlite.OpenSettings = .{
        .journal = @enumFromInt(args[1].int64),
        .has_foreign_keys = args[2].bool,
        .busy_timeout_ms = args[3].int64,
        .is_read_only = args[4].bool,
        .can_create = args[5].bool,
    };
    const task = vm.running_task.?;
    try vm.event_loop.sqlite.open(&vm.event_loop, &vm.scheduler, task, path, settings);
    task.park();
    return native.parked();
}

fn executeHandle(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const sql = bytesArg(args, 1);
    if (std.mem.indexOfScalar(u8, sql, 0) != null) {
        return native.fail(SqliteError.InvalidSql);
    }
    const task = vm.running_task.?;
    const is_queued = try vm.event_loop.sqlite.execute(&vm.event_loop, &vm.scheduler, task, args[0].int64, sql);
    if (!is_queued) {
        return native.fail(SqliteError.Closed);
    }
    task.park();
    return native.parked();
}

fn executeWithHandle(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const sql = bytesArg(args, 1);
    if (std.mem.indexOfScalar(u8, sql, 0) != null) {
        return native.fail(SqliteError.InvalidSql);
    }
    const allocator = vm.event_loop.allocator;
    const params = try copyParams(allocator, args[2..7]);
    const task = vm.running_task.?;
    const is_queued = vm.event_loop.sqlite.executeWith(&vm.event_loop, &vm.scheduler, task, args[0].int64, sql, params) catch |failure| {
        sqlite.freeParams(allocator, params);
        return failure;
    };
    if (!is_queued) {
        sqlite.freeParams(allocator, params);
        return native.fail(SqliteError.Closed);
    }
    task.park();
    return native.parked();
}

fn copyParams(allocator: std.mem.Allocator, arrays: []v.Value) ![]sqlite.Param {
    const names = itemsOf(arrays[0]);
    const kinds = itemsOf(arrays[1]);
    const ints = itemsOf(arrays[2]);
    const floats = itemsOf(arrays[3]);
    const texts = itemsOf(arrays[4]);
    const params = try allocator.alloc(sqlite.Param, kinds.len);
    var copied: usize = 0;
    errdefer {
        sqlite.freeParams(allocator, params[0..copied]);
    }
    var next_int: usize = 0;
    var next_float: usize = 0;
    var next_text: usize = 0;
    for (kinds, names, params) |kind_value, name_value, *param| {
        const name = try std.fmt.allocPrintSentinel(allocator, ":{s}", .{textOf(name_value)}, 0);
        param.* = .{ .name = name, .kind = @enumFromInt(kind_value.int64) };
        copied += 1;
        switch (param.kind) {
            .null => {},
            .int => {
                param.int = ints[next_int].int64;
                next_int += 1;
            },
            .float => {
                param.float = floats[next_float].float64;
                next_float += 1;
            },
            .text => {
                param.text = try allocator.dupe(u8, textOf(texts[next_text]));
                next_text += 1;
            },
        }
    }
    return params;
}

fn itemsOf(value: v.Value) []v.Value {
    const array = v.HeapValue.getParentPtr(v.Array, value.heap_obj.followRelocated());
    return array.getDataPtr()[0..array.len];
}

fn textOf(value: v.Value) []const u8 {
    return v.HeapValue.getParentPtr(v.Bytes, value.heap_obj.followRelocated()).slice();
}

fn closeHandle(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const task = vm.running_task.?;
    const is_queued = try vm.event_loop.sqlite.close(&vm.event_loop, &vm.scheduler, task, args[0].int64);
    if (!is_queued) {
        return native.ok(v.NULL_VALUE);
    }
    task.park();
    return native.parked();
}

fn errorOffset(vm: *VM, _: []v.Value) anyerror!NativeResult {
    return native.ok(.{ .int64 = vm.event_loop.sqlite.failureOffset(vm.running_task.?) });
}

fn takeErrorMessage(vm: *VM, _: []v.Value) anyerror!NativeResult {
    const failure = vm.event_loop.sqlite.takeFailure(vm.running_task.?) orelse return native.ok(try vm.allocBytesValue("", .string));
    defer sqlite.freeMessage(failure.message);
    return native.ok(try vm.allocBytesValue(failure.message orelse "", .string));
}
