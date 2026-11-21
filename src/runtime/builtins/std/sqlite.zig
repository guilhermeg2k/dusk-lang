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
