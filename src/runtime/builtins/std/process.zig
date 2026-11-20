const std = @import("std");
const builtin = @import("builtin");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const process = @import("../../tasks/process.zig");
const uv = @import("../../tasks/uv.zig");
const paths = @import("../../path.zig").NativePaths;
const native = @import("../native.zig");
const io = @import("io.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const ProcessError = process.ProcessError;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "run_handle", Binding{ .native = &runHandle } },
    .{ "start_handle", Binding{ .native = &startHandle } },
    .{ "capture_bytes", Binding{ .native = &captureBytes } },
    .{ "child_status", Binding{ .native = &childStatus } },
    .{ "release_run", Binding{ .native = &releaseRun } },
    .{ "bytes_to_text", Binding{ .native = &bytesToText } },
    .{ "child_wait", Binding{ .native = &childWait } },
    .{ "child_kill", Binding{ .native = &childKill } },
    .{ "child_pipe", Binding{ .native = &childPipe } },
    .{ "child_close", Binding{ .native = &childClose } },
    .{ "stream_read_bytes", Binding{ .native = &io.streamReadBytes } },
    .{ "stream_read_exact", Binding{ .native = &io.streamReadExact } },
    .{ "stream_read_line", Binding{ .native = &io.streamReadLine } },
    .{ "stream_read_all", Binding{ .native = &io.streamReadAll } },
    .{ "stream_read_all_bytes", Binding{ .native = &io.streamReadAllBytes } },
    .{ "stream_write", Binding{ .native = &io.streamWrite } },
    .{ "stream_write_bytes", Binding{ .native = &io.streamWrite } },
    .{ "stream_flush", Binding{ .native = &io.streamFlush } },
    .{ "stream_close", Binding{ .native = &io.streamClose } },
    .{ "own", Binding{ .native = &native.ownResource } },
    .{ "release", Binding{ .native = &native.releaseResource } },
});

pub const error_sets = .{ProcessError};

fn runHandle(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const started = uv.uv_hrtime();
    const timeout_ms = args[3].int64;
    const max_output = args[4].int64;
    const are_invalid_options = timeout_ms < -1 or max_output < 0;
    if (are_invalid_options) {
        return native.fail(ProcessError.InvalidArgument);
    }
    const arena = try createArena(vm);
    var is_arena_handed_off = false;
    defer if (!is_arena_handed_off) {
        destroyArena(vm, arena);
    };
    var prepared = prepareLaunch(vm, args, arena.allocator()) catch |failure| return launchFailure(failure);
    if (timeout_ms == 0) {
        return native.fail(ProcessError.TimedOut);
    }
    prepared.launch.modes = .{ 1, 2, 2 };
    prepared.launch.is_captured = true;
    prepared.launch.max_output = std.math.cast(usize, max_output) orelse return native.fail(ProcessError.InvalidArgument);
    prepared.launch.timeout_ms = if (timeout_ms < 0) null else @intCast(timeout_ms);
    prepared.launch.started_ns = started;
    if (prepared.candidates) |candidates| {
        const task = vm.running_task.?;
        process.ExecutableLookup.start(&vm.event_loop, &vm.scheduler, task, arena, candidates, prepared.launch) catch |failure| return launchFailure(failure);
        is_arena_handed_off = true;
        task.park();
        return native.parked();
    }
    const child = launchChild(vm, prepared.launch) catch |failure| return launchFailure(failure);
    child.park(vm.running_task.?, null) catch |failure| {
        child.release();
        return launchFailure(failure);
    };
    return native.parked();
}

fn createArena(vm: *VM) !*std.heap.ArenaAllocator {
    const arena = try vm.allocator.create(std.heap.ArenaAllocator);
    arena.* = std.heap.ArenaAllocator.init(vm.allocator);
    return arena;
}

fn destroyArena(vm: *VM, arena: *std.heap.ArenaAllocator) void {
    arena.deinit();
    vm.allocator.destroy(arena);
}

fn launchChild(vm: *VM, options: process.Launch) !*process.Process {
    return process.Process.launch(&vm.event_loop, &vm.scheduler, options) catch |failure| {
        try vm.event_loop.poll();
        return failure;
    };
}

const PreparedLaunch = struct {
    launch: process.Launch,
    candidates: ?[]const [:0]const u8 = null,
};

fn prepareLaunch(vm: *VM, args: []v.Value, allocator: std.mem.Allocator) !PreparedLaunch {
    const host_io = vm.io orelse return error.Unsupported;
    args[0].heap_obj = args[0].heap_obj.followRelocated();
    const array = v.HeapValue.getParentPtr(v.Array, args[0].heap_obj);
    if (array.len == 0) {
        return error.InvalidArgument;
    }
    const argv = try allocator.allocSentinel(?[*:0]const u8, array.len, null);
    for (array.getDataPtr()[0..array.len], argv) |item, *arg| {
        const text = v.HeapValue.getParentPtr(v.Bytes, item.heap_obj).slice();
        try validateText(text);
        arg.* = (try allocator.dupeZ(u8, text)).ptr;
    }
    const program = std.mem.span(argv[0].?);
    if (program.len == 0) {
        return error.InvalidArgument;
    }
    const current = try std.process.currentPathAlloc(host_io, allocator);
    try validateText(current);
    var cwd: [:0]const u8 = current;
    if (native.optionalBytesArg(args, 1)) |text| {
        try validateText(text);
        const resolved = paths.resolve(allocator, current, text) catch |failure| {
            if (failure == error.NotAbsolute) {
                return error.InvalidArgument;
            }
            return failure;
        };
        cwd = try allocator.dupeZ(u8, resolved);
    }
    var entries: std.ArrayList(?[*:0]const u8) = .empty;
    if (@as(u64, @bitCast(args[2])) != 0) {
        const map = v.HeapValue.getParentPtr(v.Map, args[2].heap_obj);
        if (map.getStorage()) |storage| {
            for (storage.usedEntries()) |entry| {
                if (entry.isRemoved()) continue;
                const name = v.HeapValue.getParentPtr(v.Bytes, entry.key.heap_obj).slice();
                const value = v.HeapValue.getParentPtr(v.Bytes, entry.value.heap_obj).slice();
                try addEnvironment(allocator, &entries, name, value);
            }
        }
    } else if (vm.environment) |environment| {
        for (environment.array_hash_map.keys(), environment.array_hash_map.values()) |name, value| {
            try addEnvironment(allocator, &entries, name, value);
        }
    }
    const env = try allocator.allocSentinel(?[*:0]const u8, entries.items.len, null);
    @memcpy(env, entries.items);
    var prepared: PreparedLaunch = .{ .launch = .{ .file = argv[0].?, .args = argv.ptr, .env = env.ptr, .cwd = cwd.ptr, .modes = .{ 1, 0, 0 }, .started_ns = uv.uv_hrtime() } };
    try checkInterpreterScript(program);
    if (hasPathSeparator(program)) {
        prepared.launch.file = (try allocator.dupeZ(u8, try paths.resolve(allocator, cwd, program))).ptr;
        return prepared;
    }
    prepared.candidates = try executableCandidates(vm, allocator, program, cwd);
    return prepared;
}

fn validateText(text: []const u8) !void {
    if (std.mem.indexOfScalar(u8, text, 0) != null) {
        return error.InvalidArgument;
    }
    if (!std.unicode.utf8ValidateSlice(text)) {
        return error.InvalidUtf8;
    }
}

fn addEnvironment(allocator: std.mem.Allocator, entries: *std.ArrayList(?[*:0]const u8), name: []const u8, value: []const u8) !void {
    try validateText(name);
    try validateText(value);
    const is_hidden_drive = builtin.os.tag == .windows and name.len == 3 and name[0] == '=' and std.ascii.isAlphabetic(name[1]) and name[2] == ':';
    const is_invalid_name = name.len == 0 or (!is_hidden_drive and std.mem.indexOfScalar(u8, name, '=') != null);
    if (is_invalid_name) {
        return error.InvalidArgument;
    }
    const entry = try std.fmt.allocPrintSentinel(allocator, "{s}={s}", .{ name, value }, 0);
    try entries.append(allocator, entry.ptr);
}

fn checkInterpreterScript(program: []const u8) !void {
    if (builtin.os.tag != .windows) return;
    const extension = std.fs.path.extension(program);
    const needs_interpreter = std.ascii.eqlIgnoreCase(extension, ".cmd") or std.ascii.eqlIgnoreCase(extension, ".bat");
    if (needs_interpreter) {
        return error.InvalidExecutable;
    }
}

fn hasPathSeparator(program: []const u8) bool {
    return std.mem.indexOfScalar(u8, program, '/') != null or (builtin.os.tag == .windows and std.mem.indexOfAny(u8, program, "\\:") != null);
}

fn executableCandidates(vm: *VM, allocator: std.mem.Allocator, program: []const u8, cwd: []const u8) ![]const [:0]const u8 {
    const environment = vm.environment orelse return error.NotFound;
    const path = environment.get("PATH") orelse return error.NotFound;
    try validateText(path);
    const suffixes: []const []const u8 = if (builtin.os.tag == .windows) &.{ "", ".exe", ".com" } else &.{""};
    var candidates: std.ArrayList([:0]const u8) = .empty;
    var parts = std.mem.splitScalar(u8, path, std.fs.path.delimiter);
    while (parts.next()) |part| {
        const directory = try paths.resolve(allocator, cwd, part);
        const candidate = try std.fs.path.join(allocator, &.{ directory, program });
        for (suffixes) |suffix| {
            try candidates.append(allocator, try std.fmt.allocPrintSentinel(allocator, "{s}{s}", .{ candidate, suffix }, 0));
        }
    }
    return candidates.items;
}

fn launchFailure(failure: anyerror) !NativeResult {
    if (failure == error.OutOfMemory) {
        return error.OutOfMemory;
    }
    return native.fail(process.processErrorOf(failure));
}

fn startHandle(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const arena = try createArena(vm);
    var is_arena_handed_off = false;
    defer if (!is_arena_handed_off) {
        destroyArena(vm, arena);
    };
    var prepared = prepareLaunch(vm, args, arena.allocator()) catch |failure| return launchFailure(failure);
    for (&prepared.launch.modes, args[3..6]) |*mode, arg| {
        const is_valid_mode = arg.int64 >= 0 and arg.int64 <= 2;
        if (!is_valid_mode) {
            return native.fail(ProcessError.InvalidArgument);
        }
        mode.* = @intCast(arg.int64);
    }
    const has_custom_output = (prepared.launch.modes[1] == 0 and vm.stdio.stdout == .custom) or (prepared.launch.modes[2] == 0 and vm.stdio.stderr == .custom);
    if (has_custom_output) {
        return native.fail(ProcessError.Unsupported);
    }
    if (prepared.launch.modes[1] == 0) {
        try vm.stdio.tryFlush(.stdout);
    }
    if (prepared.launch.modes[2] == 0) {
        try vm.stdio.tryFlush(.stderr);
    }
    if (prepared.launch.modes[0] == 0) {
        vm.event_loop.pauseStdinWhenIdle();
    }
    if (prepared.candidates) |candidates| {
        const task = vm.running_task.?;
        process.ExecutableLookup.start(&vm.event_loop, &vm.scheduler, task, arena, candidates, prepared.launch) catch |failure| return launchFailure(failure);
        is_arena_handed_off = true;
        task.park();
        return native.parked();
    }
    const child = launchChild(vm, prepared.launch) catch |failure| return launchFailure(failure);
    return native.ok(.{ .int64 = @intCast(child.table_index) });
}

fn captureBytes(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const child = vm.event_loop.processAt(args[0].int64).?;
    return native.ok(try vm.allocBytesValue(child.outputs[@intCast(args[1].int64)].items, .buffer));
}

fn childStatus(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const child = vm.event_loop.processAt(args[0].int64).?;
    return native.ok(try vm.allocIntArray(&.{ child.exit_status, child.term_signal }));
}

fn releaseRun(vm: *VM, args: []v.Value) anyerror!NativeResult {
    vm.event_loop.processAt(args[0].int64).?.release();
    return native.ok(v.NULL_VALUE);
}

fn bytesToText(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const text = native.bytesArg(args, 0);
    if (!std.unicode.utf8ValidateSlice(text)) {
        return native.fail(ProcessError.InvalidUtf8);
    }
    const length = text.len;
    const result = try vm.allocString(length);
    @memcpy(result.mutableSlice(), native.bytesArg(args, 0));
    return native.stringResult(result);
}

fn childWait(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const child = vm.event_loop.processAt(args[0].int64) orelse return native.fail(ProcessError.Closed);
    const timeout = args[1].int64;
    if (timeout < -1) {
        return native.fail(ProcessError.InvalidArgument);
    }
    if (child.has_exited) {
        return native.ok(try vm.allocIntArray(&.{ child.exit_status, child.term_signal }));
    }
    if (child.waiter != null) {
        return native.fail(ProcessError.Busy);
    }
    if (timeout == 0) {
        return native.fail(ProcessError.TimedOut);
    }
    child.park(vm.running_task.?, if (timeout < 0) null else @intCast(timeout)) catch |failure| return launchFailure(failure);
    return native.parked();
}

fn childKill(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const child = vm.event_loop.processAt(args[0].int64) orelse return native.fail(ProcessError.Closed);
    child.kill() catch |failure| return launchFailure(failure);
    return native.ok(v.NULL_VALUE);
}

fn childPipe(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const child = vm.event_loop.processAt(args[0].int64) orelse return native.fail(ProcessError.Closed);
    const index = child.pipe_handles[@intCast(args[1].int64)] orelse return native.fail(ProcessError.NotPiped);
    if (vm.event_loop.streamAt(@intCast(index)) == null) {
        return native.fail(ProcessError.Closed);
    }
    return native.ok(.{ .int64 = @intCast(index) });
}

fn childClose(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const child = vm.event_loop.processAt(args[0].int64) orelse return native.ok(v.NULL_VALUE);
    if (!child.has_exited) {
        return native.fail(ProcessError.Busy);
    }
    child.release();
    return native.ok(v.NULL_VALUE);
}
