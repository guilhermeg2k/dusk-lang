const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const os = @import("../../os.zig");
const native = @import("../native.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const OsError = os.OsError;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "platform", Binding{ .native = &platform } },
    .{ "args", Binding{ .native = &arguments } },
    .{ "env", Binding{ .native = &environmentValue } },
    .{ "cwd", Binding{ .native = &currentDirectory } },
    .{ "set_exit_code", Binding{ .native = &setExitCode } },
    .{ "signal_watch", Binding{ .native = &watchSignal } },
    .{ "signal_wait", Binding{ .native = &waitSignal } },
    .{ "signal_close", Binding{ .native = &closeSignal } },
    .{ "own", Binding{ .native = &native.ownResource } },
    .{ "release", Binding{ .native = &native.releaseResource } },
});

pub const error_sets = .{OsError};

fn platform(_: *VM, _: []v.Value) anyerror!NativeResult {
    return native.ok(.{ .int64 = @intFromEnum(os.platform()) });
}

fn arguments(vm: *VM, _: []v.Value) anyerror!NativeResult {
    for (vm.program_args) |arg| {
        if (!std.unicode.utf8ValidateSlice(arg)) {
            return native.fail(OsError.InvalidUtf8);
        }
    }
    return native.ok(try vm.allocStringArray(vm.program_args));
}

fn environmentValue(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const name = native.bytesArg(args, 0);
    const is_invalid_name = name.len == 0 or std.mem.indexOfAny(u8, name, "=\x00") != null;
    if (is_invalid_name) {
        return native.fail(OsError.InvalidArgument);
    }
    const environment = vm.environment orelse return native.ok(v.NULL_VALUE);
    const value = environment.get(name) orelse return native.ok(v.NULL_VALUE);
    if (!std.unicode.utf8ValidateSlice(value)) {
        return native.fail(OsError.InvalidUtf8);
    }
    return native.ok(try vm.allocBytesValue(value, .string));
}

fn currentDirectory(vm: *VM, _: []v.Value) anyerror!NativeResult {
    const io = vm.io orelse return native.fail(OsError.Unsupported);
    const path = std.process.currentPathAlloc(io, vm.allocator) catch |failure| {
        return switch (failure) {
            error.OutOfMemory => error.OutOfMemory,
            error.CurrentDirUnlinked => native.fail(OsError.NotFound),
            else => native.fail(OsError.Other),
        };
    };
    defer vm.allocator.free(path);
    if (!std.unicode.utf8ValidateSlice(path)) {
        return native.fail(OsError.InvalidUtf8);
    }
    return native.ok(try vm.allocBytesValue(path, .string));
}

fn setExitCode(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const code = args[0].int64;
    const is_valid_code = code >= 0 and code <= 255;
    if (!is_valid_code) {
        return native.fail(OsError.InvalidArgument);
    }
    vm.exit_code = @intCast(code);
    return native.ok(v.NULL_VALUE);
}

fn watchSignal(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const index = vm.event_loop.watchSignal(&vm.scheduler, args[0].int64) catch |failure| {
        if (failure == error.OutOfMemory) {
            return error.OutOfMemory;
        }
        return native.fail(if (failure == error.Unsupported) OsError.Unsupported else OsError.Other);
    };
    return native.ok(.{ .int64 = @intCast(index) });
}

fn waitSignal(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const subscription = vm.event_loop.signalAt(args[0].int64) orelse return native.fail(OsError.Closed);
    const is_parked = subscription.parkWaiter(vm.running_task.?) catch |failure| {
        return native.fail(if (failure == error.Busy) OsError.Busy else OsError.Closed);
    };
    return if (is_parked) native.parked() else native.ok(v.NULL_VALUE);
}

fn closeSignal(vm: *VM, args: []v.Value) anyerror!NativeResult {
    vm.event_loop.closeSignal(args[0].int64);
    return native.ok(v.NULL_VALUE);
}
