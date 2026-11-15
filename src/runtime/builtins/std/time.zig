const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const EventLoop = @import("../../tasks/event_loop.zig").EventLoop;
const native = @import("../native.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "now_ms", Binding{ .native = &nowMilliseconds } },
    .{ "monotonic_ns", Binding{ .native = &monotonicNanoseconds } },
});

fn nowMilliseconds(_: *VM, _: []v.Value) anyerror!NativeResult {
    return native.ok(.{ .int64 = try EventLoop.wallTimeMilliseconds() });
}

fn monotonicNanoseconds(_: *VM, _: []v.Value) anyerror!NativeResult {
    return native.ok(.{ .int64 = try EventLoop.monotonicTimeNanoseconds() });
}
