const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const native = @import("../native.zig");
const FsJob = @import("../../tasks/fs_job.zig").FsJob;
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const bytesArg = native.bytesArg;

pub const FsError = @import("../../tasks/fs_job.zig").FsError;
pub const error_sets = .{FsError};

pub const bindings = native.BindingMap.initComptime(.{
    .{ "exists", Binding{ .native = &exists } },
    .{ "stat_numbers", Binding{ .native = &statNumbers } },
    .{ "read_text", Binding{ .native = &readText } },
    .{ "read_bytes", Binding{ .native = &readBytes } },
    .{ "write_text", Binding{ .native = &writeFile } },
    .{ "write_bytes", Binding{ .native = &writeFile } },
    .{ "append_text", Binding{ .native = &appendText } },
    .{ "read_dir", Binding{ .native = &readDir } },
    .{ "make_dir", Binding{ .native = &makeDir } },
    .{ "remove", Binding{ .native = &remove } },
    .{ "rename", Binding{ .native = &rename } },
});

fn exists(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startFsJob(vm, .exists, bytesArg(args, 0), "");
}

fn statNumbers(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startFsJob(vm, .stat, bytesArg(args, 0), "");
}

fn readText(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startFsJob(vm, .read_text, bytesArg(args, 0), "");
}

fn readBytes(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startFsJob(vm, .read_bytes, bytesArg(args, 0), "");
}

fn writeFile(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startFsJob(vm, .write, bytesArg(args, 0), bytesArg(args, 1));
}

fn appendText(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startFsJob(vm, .append, bytesArg(args, 0), bytesArg(args, 1));
}

fn readDir(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startFsJob(vm, .read_dir, bytesArg(args, 0), "");
}

fn makeDir(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const kind: FsJob.Kind = if (args[1].bool) .make_dir_recursive else .make_dir;
    return startFsJob(vm, kind, bytesArg(args, 0), "");
}

fn remove(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const kind: FsJob.Kind = if (args[1].bool) .remove_recursive else .remove;
    return startFsJob(vm, kind, bytesArg(args, 0), "");
}

fn rename(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startFsJob(vm, .rename, bytesArg(args, 0), bytesArg(args, 1));
}

fn startFsJob(vm: *VM, kind: FsJob.Kind, path: []const u8, input: []const u8) anyerror!NativeResult {
    const job = try FsJob.create(vm.event_loop.allocator, kind, path, input);
    errdefer job.destroy();
    const current = vm.running_task.?;
    try vm.event_loop.startFsJob(&vm.scheduler, current, job);
    current.park();
    return native.parked();
}
