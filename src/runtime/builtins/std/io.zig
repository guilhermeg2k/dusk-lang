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
const ReadMode = @import("../../tasks/stream.zig").ReadMode;
const Stream = @import("../../stdio.zig").Stream;
pub const error_sets = .{IoError};

pub const bindings = native.BindingMap.initComptime(.{
    .{ "print", Binding{ .native = &print } },
    .{ "println", Binding{ .native = &println } },
    .{ "eprint", Binding{ .native = &eprint } },
    .{ "eprintln", Binding{ .native = &eprintln } },
    .{ "stdin_handle", Binding{ .native = &stdinHandle } },
    .{ "stream_read_bytes", Binding{ .native = &streamReadBytes } },
    .{ "stream_read_exact", Binding{ .native = &streamReadExact } },
    .{ "stream_read_line", Binding{ .native = &streamReadLine } },
    .{ "stream_read_all", Binding{ .native = &streamReadAll } },
    .{ "stream_read_all_bytes", Binding{ .native = &streamReadAllBytes } },
    .{ "is_terminal_fd", Binding{ .native = &isTerminalFd } },
    .{ "stream_close", Binding{ .native = &streamClose } },
    .{ "stdio_write", Binding{ .native = &stdioWrite } },
    .{ "stdio_write_bytes", Binding{ .native = &stdioWrite } },
    .{ "stdio_flush", Binding{ .native = &stdioFlush } },
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
    return ok(.{ .int64 = handle });
}

pub fn streamReadBytes(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const max = args[1].int64;
    if (max < 1) {
        return RuntimeErrorReporter.invalidReadSize(max);
    }
    return readStream(vm, args[0].int64, .{ .bytes = @intCast(max) }, args[2].int64);
}

pub fn streamReadExact(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const count = args[1].int64;
    if (count < 1) {
        return RuntimeErrorReporter.invalidReadSize(count);
    }
    return readStream(vm, args[0].int64, .{ .exact = @intCast(count) }, args[2].int64);
}

pub fn streamReadLine(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const max = args[1].int64;
    const limit: ?usize = if (max < 0) null else @intCast(max);
    return readStream(vm, args[0].int64, .{ .line = limit }, args[2].int64);
}

pub fn streamReadAll(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return readStream(vm, args[0].int64, .all_text, args[1].int64);
}

pub fn streamReadAllBytes(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return readStream(vm, args[0].int64, .all_bytes, args[1].int64);
}

pub fn readStream(vm: *VM, handle: i64, mode: ReadMode, timeout_ms: i64) anyerror!NativeResult {
    const stream = vm.event_loop.streamAt(handle) orelse return fail(IoError.Closed);
    if (stream.waiter != null) {
        return fail(IoError.Busy);
    }
    stream.ensureHandshakeStarted();
    if (stream.readyOutcome(mode)) |outcome| {
        switch (outcome.result) {
            .data => |data| {
                const value = try vm.allocBytesValue(data, mode.valueKind());
                stream.consume(outcome.consumed);
                return ok(value);
            },
            .end => return ok(v.NULL_VALUE),
            .failure => |failure| {
                stream.consume(outcome.consumed);
                return fail(failure);
            },
            .http_failure => return fail(IoError.Other),
        }
    }
    const is_stdin = vm.event_loop.stdin_handle == handle;
    if (is_stdin) {
        try vm.stdio.flushStdout();
    }
    const current = vm.running_task.?;
    const timeout: ?u64 = if (timeout_ms < 0) null else @intCast(timeout_ms);
    stream.parkReader(current, mode, timeout) catch return fail(IoError.Other);
    current.park();
    return native.parked();
}

pub fn streamWrite(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const stream = vm.event_loop.streamAt(args[0].int64) orelse return fail(IoError.Closed);
    const current = vm.running_task.?;
    const outcome = stream.write(current, bytesArg(args, 1)) catch return fail(IoError.Other);
    switch (outcome) {
        .done => return ok(.{ .null = {} }),
        .failure => |failure| return fail(failure),
        .parked => {
            current.park();
            return native.parked();
        },
    }
}

pub fn streamSetWriteTimeout(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const stream = vm.event_loop.streamAt(args[0].int64) orelse return ok(.{ .null = {} });
    const timeout_ms = args[1].int64;
    stream.setWriteTimeout(if (timeout_ms < 0) null else @intCast(timeout_ms));
    return ok(.{ .null = {} });
}

pub fn streamFlush(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const stream = vm.event_loop.streamAt(args[0].int64) orelse return fail(IoError.Closed);
    const current = vm.running_task.?;
    const outcome = stream.flush(current) catch return fail(IoError.Other);
    switch (outcome) {
        .done => return ok(.{ .null = {} }),
        .failure => |failure| return fail(failure),
        .parked => {
            current.park();
            return native.parked();
        },
    }
}

pub fn streamClose(vm: *VM, args: []v.Value) anyerror!NativeResult {
    vm.event_loop.closeStream(args[0].int64);
    return ok(.{ .null = {} });
}

fn stdioWrite(vm: *VM, args: []v.Value) anyerror!NativeResult {
    vm.stdio.tryWriteBytes(stdioStream(args[0].int64), bytesArg(args, 1)) catch |failure| return writeFailure(failure);
    return ok(.{ .null = {} });
}

fn stdioFlush(vm: *VM, args: []v.Value) anyerror!NativeResult {
    vm.stdio.tryFlush(stdioStream(args[0].int64)) catch |failure| return writeFailure(failure);
    return ok(.{ .null = {} });
}

fn stdioStream(fd: i64) Stream {
    return if (fd == 2) .stderr else .stdout;
}

fn writeFailure(failure: anyerror) NativeResult {
    return fail(if (failure == error.BrokenPipe) IoError.BrokenPipe else IoError.Other);
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
