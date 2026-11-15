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
});

fn exists(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startFsJob(vm, .exists, bytesArg(args, 0));
}

fn statNumbers(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startFsJob(vm, .stat, bytesArg(args, 0));
}

fn readText(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startFsJob(vm, .read_text, bytesArg(args, 0));
}

fn readBytes(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return startFsJob(vm, .read_bytes, bytesArg(args, 0));
}

fn startFsJob(vm: *VM, kind: FsJob.Kind, path: []const u8) anyerror!NativeResult {
    const job = try FsJob.create(vm.event_loop.allocator, kind, path);
    errdefer job.destroy();
    const current = vm.running_task.?;
    try vm.event_loop.startFsJob(&vm.scheduler, current, job);
    current.park();
    return native.parked();
}
