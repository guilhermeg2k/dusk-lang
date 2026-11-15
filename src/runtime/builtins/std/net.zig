const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const native = @import("../native.zig");
const io = @import("io.zig");
const tcp = @import("../../tasks/tcp.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const ok = native.ok;
const fail = native.fail;
const bytesArg = native.bytesArg;

pub const NetError = tcp.NetError;
const IoError = io.IoError;
pub const error_sets = .{NetError};

pub const bindings = native.BindingMap.initComptime(.{
    .{ "tcp_connect_handle", Binding{ .native = &tcpConnect } },
    .{ "tcp_listen_handle", Binding{ .native = &tcpListen } },
    .{ "listener_accept", Binding{ .native = &listenerAccept } },
    .{ "listener_port", Binding{ .native = &listenerPort } },
    .{ "listener_close", Binding{ .native = &listenerClose } },
    .{ "stream_read_bytes", Binding{ .native = &io.streamReadBytes } },
    .{ "stream_read_line", Binding{ .native = &io.streamReadLine } },
    .{ "stream_read_all", Binding{ .native = &io.streamReadAll } },
    .{ "stream_read_all_bytes", Binding{ .native = &io.streamReadAllBytes } },
    .{ "stream_write", Binding{ .native = &io.streamWrite } },
    .{ "stream_write_bytes", Binding{ .native = &io.streamWrite } },
    .{ "stream_close", Binding{ .native = &io.streamClose } },
});

fn tcpConnect(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return openTcp(vm, args, .connect, timeoutArg(args[2].int64));
}

fn tcpListen(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const backlog: c_int = @intCast(std.math.clamp(args[2].int64, 1, std.math.maxInt(u16)));
    return openTcp(vm, args, .{ .listen = backlog }, null);
}

fn openTcp(vm: *VM, args: []v.Value, purpose: tcp.Purpose, timeout_ms: ?u64) anyerror!NativeResult {
    const port = tcp.validPort(args[1].int64) orelse return fail(NetError.InvalidAddress);
    const current = vm.running_task.?;
    const progress = tcp.open(&vm.event_loop, &vm.scheduler, current, purpose, bytesArg(args, 0), port, timeout_ms) catch return fail(NetError.Other);
    switch (progress) {
        .opened => |handle| return ok(.{ .int64 = @intCast(handle) }),
        .failed => |failure| return fail(failure),
        .waiting => {
            current.park();
            return native.parked();
        },
    }
}

fn timeoutArg(timeout_ms: i64) ?u64 {
    return if (timeout_ms < 0) null else @intCast(timeout_ms);
}

fn listenerAccept(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const listener = vm.event_loop.listenerAt(args[0].int64) orelse return fail(IoError.Closed);
    if (listener.waiter != null) {
        return fail(IoError.Busy);
    }
    if (listener.takeReadyConnection()) |outcome| {
        return switch (outcome) {
            .connection => |handle| ok(.{ .int64 = @intCast(handle) }),
            .failure => |failure| fail(failure),
        };
    }
    const current = vm.running_task.?;
    listener.parkAccepter(current, timeoutArg(args[1].int64)) catch return fail(IoError.Other);
    current.park();
    return native.parked();
}

fn listenerPort(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const listener = vm.event_loop.listenerAt(args[0].int64) orelse return ok(.{ .int64 = 0 });
    return ok(.{ .int64 = listener.port() });
}

fn listenerClose(vm: *VM, args: []v.Value) anyerror!NativeResult {
    vm.event_loop.closeListener(args[0].int64);
    return ok(.{ .null = {} });
}
