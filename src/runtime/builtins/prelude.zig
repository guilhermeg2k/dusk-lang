const std = @import("std");
const v = @import("../value.zig");
const format = @import("../format.zig");
const VM = @import("../vm.zig").VM;
const RuntimeErrorReporter = @import("../runtime_error.zig").RuntimeErrorReporter;
const native = @import("native.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const ok = native.ok;
const parked = native.parked;
const bytesArg = native.bytesArg;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "echo", Binding{ .native = &echo } },
    .{ "assert", Binding{ .native = &assert } },
    .{ "unchecked_to_byte", Binding{ .intrinsic = .to_byte } },
    .{ "unchecked_to_int", Binding{ .intrinsic = .to_int } },
    .{ "to_float", Binding{ .intrinsic = .to_float } },
    .{ "copy", Binding{ .intrinsic = .copy } },
    .{ "spawn", Binding{ .intrinsic = .task_spawn } },
    .{ "await", Binding{ .native = &taskAwait } },
    .{ "strings_equal", Binding{ .native = &stringsEqual } },
});

pub fn echo(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const writer = vm.stdout orelse return ok(.{ .null = {} });
    const ty: v.ValueType = @enumFromInt(@as(u8, @intCast(args[1].int64)));
    format.printValue(writer, args[0], ty);
    writer.print("\n", .{}) catch {};
    //todo: not treating error
    writer.flush() catch {};
    return ok(.{ .null = {} });
}

pub fn assert(_: *VM, args: []v.Value) anyerror!NativeResult {
    if (!args[0].bool) return RuntimeErrorReporter.assertionFailed();
    return ok(.{ .null = {} });
}

pub fn taskAwait(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const target = v.HeapValue.getParentPtr(v.Task, args[0].heap_obj).state;
    target.is_awaited = true;
    if (target.status == .done) {
        return .{ .value = target.result, .has_failed = target.has_failed };
    }
    const current = vm.running_task.?;
    if (target == current) {
        return RuntimeErrorReporter.taskAwaitsItself();
    }
    try target.waiters.append(vm.scheduler.allocator, current);
    current.park();
    return parked();
}

fn stringsEqual(_: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .bool = std.mem.eql(u8, bytesArg(args, 0), bytesArg(args, 1)) });
}
