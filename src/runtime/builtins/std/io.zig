const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const native = @import("../native.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const ok = native.ok;
const bytesArg = native.bytesArg;
const fail = native.fail;
const uv = @import("../../tasks/uv.zig");
const RuntimeErrorReporter = @import("../../runtime_error.zig").RuntimeErrorReporter;

pub const IoError = @import("../../tasks/stream.zig").IoError;
pub const error_sets = .{IoError};

pub const bindings = native.BindingMap.initComptime(.{
    .{ "print", Binding{ .native = &print } },
    .{ "println", Binding{ .native = &println } },
    .{ "eprint", Binding{ .native = &eprint } },
    .{ "eprintln", Binding{ .native = &eprintln } },
    .{ "stdin_handle", Binding{ .native = &stdinHandle } },
    .{ "stream_read_bytes", Binding{ .native = &streamReadBytes } },
    .{ "is_terminal_fd", Binding{ .native = &isTerminalFd } },
});

fn print(vm: *VM, args: []v.Value) anyerror!NativeResult {
    try vm.stdio.writeBytes(.stdout, bytesArg(args, 0));
    return ok(.{ .null = {} });
}

fn println(vm: *VM, args: []v.Value) anyerror!NativeResult {
    try vm.stdio.writeLine(.stdout, bytesArg(args, 0));
    return ok(.{ .null = {} });
}

fn eprint(vm: *VM, args: []v.Value) anyerror!NativeResult {
    try vm.stdio.writeBytes(.stderr, bytesArg(args, 0));
    return ok(.{ .null = {} });
}

fn eprintln(vm: *VM, args: []v.Value) anyerror!NativeResult {
    try vm.stdio.writeLine(.stderr, bytesArg(args, 0));
    return ok(.{ .null = {} });
}

fn stdinHandle(vm: *VM, _: []v.Value) anyerror!NativeResult {
    const handle = try vm.event_loop.openStdin(&vm.scheduler, vm.stdin_fd);
    return ok(.{ .int64 = @intCast(handle) });
}

fn streamReadBytes(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const max = args[1].int64;
    if (max < 1) {
        return RuntimeErrorReporter.invalidReadSize(max);
    }
    const stream = vm.event_loop.streamAt(args[0].int64) orelse return fail(IoError.Closed);
    if (stream.waiter != null) {
        return fail(IoError.Busy);
    }
    const available = stream.bufferedBytes();
    if (available.len > 0) {
        const count = @min(available.len, @as(usize, @intCast(max)));
        const value = try vm.allocBytesValue(available[0..count], .buffer);
        stream.consume(count);
        return ok(value);
    }
    if (stream.failure) |failure| {
        return fail(failure);
    }
    if (stream.has_ended) {
        return ok(v.NULL_VALUE);
    }
    const current = vm.running_task.?;
    stream.parkReader(current, @intCast(max)) catch return fail(IoError.Other);
    current.park();
    return native.parked();
}

fn isTerminalFd(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const fd = args[0].int64;
    const is_terminal = switch (fd) {
        0 => uv.uv_guess_handle(vm.stdin_fd) == .tty,
        1 => vm.stdio.is_stdout_terminal,
        else => uv.uv_guess_handle(@intCast(fd)) == .tty,
    };
    return ok(.{ .bool = is_terminal });
}
