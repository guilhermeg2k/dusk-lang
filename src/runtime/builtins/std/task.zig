const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const RuntimeErrorReporter = @import("../../runtime_error.zig").RuntimeErrorReporter;
const native = @import("../native.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "sleep", Binding{ .native = &sleep } },
});

fn sleep(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const ms = args[0].int64;
    if (ms < 0) {
        return RuntimeErrorReporter.negativeSleep(ms);
    }
    const current = vm.running_task.?;
    if (ms == 0) {
        try vm.scheduler.wake(current, .{ .value = v.NULL_VALUE, .kind = .null });
        return native.parked();
    }
    try vm.event_loop.startTimer(&vm.scheduler, @intCast(ms), current);
    current.park();
    return native.parked();
}
