const std = @import("std");
const v = @import("../../value.zig");
const io = @import("io.zig");
const VM = @import("../../vm.zig").VM;
const native = @import("../native.zig");
const FsJob = @import("../../io/fs/fs_job.zig").FsJob;
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const bytesArg = native.bytesArg;

pub const FsError = @import("../../io/fs/fs_job.zig").FsError;
pub const error_sets = .{FsError};

pub const bindings = native.BindingMap.initComptime(.{
    .{ "own", Binding{ .native = &native.ownResource } },
    .{ "release", Binding{ .native = &native.releaseResource } },
    .{ "exists", Binding{ .native = &exists } },
    .{ "stat_numbers", Binding{ .native = &statNumbers } },
    .{ "read", Binding{ .native = &readText } },
    .{ "read_bytes", Binding{ .native = &readBytes } },
    .{ "write", Binding{ .native = &writeFile } },
    .{ "write_bytes", Binding{ .native = &writeFile } },
    .{ "write_atomic", Binding{ .native = &writeAtomic } },
    .{ "write_bytes_atomic", Binding{ .native = &writeAtomic } },
    .{ "append", Binding{ .native = &appendText } },
    .{ "read_dir", Binding{ .native = &readDir } },
    .{ "make_dir", Binding{ .native = &makeDir } },
    .{ "delete", Binding{ .native = &delete } },
    .{ "rename", Binding{ .native = &rename } },
    .{ "chmod", Binding{ .native = &chmod } },
    .{ "real_path", Binding{ .native = &realPath } },
    .{ "resolve_under", Binding{ .native = &resolveUnder } },
    .{ "open_descriptor", Binding{ .native = &openDescriptor } },
    .{ "file_stream", Binding{ .native = &fileStream } },
    .{ "stream_read_bytes", Binding{ .native = &io.streamReadBytes } },
    .{ "stream_read_exact", Binding{ .native = &io.streamReadExact } },
    .{ "stream_read_line", Binding{ .native = &io.streamReadLine } },
    .{ "stream_read_all", Binding{ .native = &io.streamReadAll } },
    .{ "stream_read_all_bytes", Binding{ .native = &io.streamReadAllBytes } },
    .{ "stream_close", Binding{ .native = &io.streamClose } },
    .{ "stream_write", Binding{ .native = &io.streamWrite } },
    .{ "stream_write_bytes", Binding{ .native = &io.streamWrite } },
    .{ "stream_flush", Binding{ .native = &io.streamFlush } },
    .{ "stream_position", Binding{ .native = &streamPosition } },
    .{ "stream_seek", Binding{ .native = &streamSeek } },
    .{ "stream_file_size", Binding{ .native = &streamFileSize } },
});

pub fn openDescriptor(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const path = bytesArg(args, 0);
    if (std.mem.indexOfScalar(u8, path, 0) != null) {
        return native.fail(FsError.InvalidPath);
    }
    const permissions = parsePermissions(args[2].int64) orelse return native.fail(FsError.InvalidPath);
    const job = try FsJob.create(vm.event_loop.allocator, .open_file, path, "");
    errdefer job.destroy();
    job.open_mode = @enumFromInt(args[1].int64);
    job.permissions = permissions;
    job.should_follow_symlinks = args[3].bool;
    const current = vm.running_task.?;
    try vm.event_loop.startFsJob(&vm.scheduler, current, job);
    current.park();
    return native.parked();
}

pub fn fileStream(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const stream_id = try vm.event_loop.openFileStream(&vm.scheduler, @intCast(args[0].int64), args[1].int64, @enumFromInt(args[2].int64));
    return native.ok(.{ .int64 = stream_id });
}

fn streamPosition(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const stream = vm.event_loop.streamAt(args[0].int64) orelse return native.ok(.{ .int64 = 0 });
    return native.ok(.{ .int64 = stream.filePosition() });
}

fn streamSeek(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const stream = vm.event_loop.streamAt(args[0].int64) orelse return native.fail(io.IoError.Closed);
    if (stream.seekFile(args[1].int64)) |failure| {
        return native.fail(failure);
    }
    return native.ok(.{ .null = {} });
}

fn streamFileSize(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const stream = vm.event_loop.streamAt(args[0].int64) orelse return native.fail(io.IoError.Closed);
    const job = try FsJob.create(vm.event_loop.allocator, .file_size, "", "");
    errdefer job.destroy();
    job.file = stream.file;
    job.stream = stream;
    stream.requests_in_flight += 1;
    errdefer stream.requests_in_flight -= 1;
    const current = vm.running_task.?;
    try vm.event_loop.startFsJob(&vm.scheduler, current, job);
    current.park();
    return native.parked();
}

fn exists(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startFsJob(vm, .exists, bytesArg(args, 0), "");
}

fn statNumbers(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startFsJob(vm, .stat, bytesArg(args, 0), "");
}

fn readText(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startReadJob(vm, .read_text, bytesArg(args, 0), args[1].int64);
}

fn readBytes(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startReadJob(vm, .read_bytes, bytesArg(args, 0), args[1].int64);
}

fn writeFile(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startWriteJob(vm, .write, bytesArg(args, 0), bytesArg(args, 1), args[2].int64, args[3].bool);
}

fn writeAtomic(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startWriteJob(vm, .write_atomic, bytesArg(args, 0), bytesArg(args, 1), args[2].int64, false);
}

fn appendText(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startWriteJob(vm, .append, bytesArg(args, 0), bytesArg(args, 1), args[2].int64, args[3].bool);
}

fn readDir(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startFsJob(vm, .read_dir, bytesArg(args, 0), "");
}

fn makeDir(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const path = bytesArg(args, 0);
    if (std.mem.indexOfScalar(u8, path, 0) != null) {
        return native.fail(FsError.InvalidPath);
    }
    const permissions = parsePermissions(args[2].int64) orelse return native.fail(FsError.InvalidPath);
    const kind: FsJob.Kind = if (args[1].bool) .make_dir_recursive else .make_dir;
    const job = try FsJob.create(vm.event_loop.allocator, kind, path, "");
    errdefer job.destroy();
    job.permissions = permissions;
    const current = vm.running_task.?;
    try vm.event_loop.startFsJob(&vm.scheduler, current, job);
    current.park();
    return native.parked();
}

fn delete(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const kind: FsJob.Kind = if (args[1].bool) .delete_recursive else .delete;
    return startFsJob(vm, kind, bytesArg(args, 0), "");
}

fn rename(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startFsJob(vm, .rename, bytesArg(args, 0), bytesArg(args, 1));
}

fn chmod(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const path = bytesArg(args, 0);
    if (std.mem.indexOfScalar(u8, path, 0) != null) {
        return native.fail(FsError.InvalidPath);
    }
    const permissions = parsePermissions(args[1].int64) orelse return native.fail(FsError.InvalidPath);
    const job = try FsJob.create(vm.event_loop.allocator, .chmod, path, "");
    errdefer job.destroy();
    job.permissions = permissions;
    const current = vm.running_task.?;
    try vm.event_loop.startFsJob(&vm.scheduler, current, job);
    current.park();
    return native.parked();
}

fn realPath(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startFsJob(vm, .real_path, bytesArg(args, 0), "");
}

fn resolveUnder(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startFsJob(vm, .resolve_under, bytesArg(args, 0), bytesArg(args, 1));
}

fn startReadJob(vm: *VM, kind: FsJob.Kind, path: []const u8, max_bytes: i64) anyerror!NativeResult {
    if (std.mem.indexOfScalar(u8, path, 0) != null) {
        return native.fail(FsError.InvalidPath);
    }
    const job = try FsJob.create(vm.event_loop.allocator, kind, path, "");
    errdefer job.destroy();
    job.max_bytes = if (max_bytes < 0) null else @intCast(max_bytes);
    const current = vm.running_task.?;
    try vm.event_loop.startFsJob(&vm.scheduler, current, job);
    current.park();
    return native.parked();
}

fn startWriteJob(vm: *VM, kind: FsJob.Kind, path: []const u8, input: []const u8, raw_permissions: i64, should_follow_symlinks: bool) anyerror!NativeResult {
    if (std.mem.indexOfScalar(u8, path, 0) != null) {
        return native.fail(FsError.InvalidPath);
    }
    const permissions = parsePermissions(raw_permissions) orelse return native.fail(FsError.InvalidPath);
    const job = try FsJob.create(vm.event_loop.allocator, kind, path, input);
    errdefer job.destroy();
    job.permissions = permissions;
    job.should_follow_symlinks = should_follow_symlinks;
    const current = vm.running_task.?;
    try vm.event_loop.startFsJob(&vm.scheduler, current, job);
    current.park();
    return native.parked();
}

fn parsePermissions(raw: i64) ?c_int {
    if (raw < 0 or raw > 0o7777) {
        return null;
    }
    return @intCast(raw);
}

fn startFsJob(vm: *VM, kind: FsJob.Kind, path: []const u8, input: []const u8) anyerror!NativeResult {
    const has_second_path = kind == .rename or kind == .resolve_under;
    const has_nul_path = std.mem.indexOfScalar(u8, path, 0) != null or (has_second_path and std.mem.indexOfScalar(u8, input, 0) != null);
    if (has_nul_path) {
        if (kind == .exists) {
            return native.ok(.{ .bool = false });
        }
        return native.fail(FsError.InvalidPath);
    }
    const job = try FsJob.create(vm.event_loop.allocator, kind, path, input);
    errdefer job.destroy();
    const current = vm.running_task.?;
    try vm.event_loop.startFsJob(&vm.scheduler, current, job);
    current.park();
    return native.parked();
}
