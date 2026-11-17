const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const Plan = @import("../../serialization_plan.zig").Plan;
const native = @import("../native.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "field_count", Binding{ .native = &fieldCount } },
    .{ "field_name", Binding{ .native = &fieldName } },
    .{ "find_field", Binding{ .native = &findField } },
    .{ "find_field_in", Binding{ .native = &findFieldIn } },
    .{ "derive_decode", Binding{ .serialization = .derive_decode } },
    .{ "derive_encode", Binding{ .serialization = .derive_encode } },
    .{ "blank", Binding{ .serialization = .blank } },
    .{ "field_default", Binding{ .serialization = .field_default } },
});

fn fieldCount(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const plan = planAt(vm, args[0].int64) orelse return native.ok(.{ .int64 = 0 });
    return native.ok(.{ .int64 = @intCast(plan.names.len) });
}

fn fieldName(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const name = nameAt(vm, args[0].int64, args[1].int64) orelse "";
    const result = try vm.allocString(name.len);
    @memcpy(result.mutableSlice(), name);
    return native.stringResult(result);
}

fn findField(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const plan = planAt(vm, args[0].int64) orelse return native.ok(.{ .int64 = -1 });
    return native.ok(.{ .int64 = indexOfName(plan, native.bytesArg(args, 1)) });
}

fn findFieldIn(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const plan = planAt(vm, args[0].int64) orelse return native.ok(.{ .int64 = -1 });
    const text = native.bytesArg(args, 1);
    const start = args[2].int64;
    const stop = args[3].int64;
    const is_in_bounds = start >= 0 and start <= stop and stop <= text.len;
    if (!is_in_bounds) {
        return native.ok(.{ .int64 = -1 });
    }
    return native.ok(.{ .int64 = indexOfName(plan, text[@intCast(start)..@intCast(stop)]) });
}

fn planAt(vm: *VM, id: i64) ?Plan {
    const plans = vm.program.serialization_plans;
    const is_out_of_range = id < 0 or id >= plans.len;
    if (is_out_of_range) {
        return null;
    }
    return plans[@intCast(id)];
}

fn nameAt(vm: *VM, id: i64, index: i64) ?[]const u8 {
    const plan = planAt(vm, id) orelse return null;
    const is_out_of_range = index < 0 or index >= plan.names.len;
    if (is_out_of_range) {
        return null;
    }
    return plan.names[@intCast(index)];
}

fn indexOfName(plan: Plan, name: []const u8) i64 {
    for (plan.names, 0..) |candidate, i| {
        if (std.mem.eql(u8, candidate, name)) {
            return @intCast(i);
        }
    }
    return -1;
}
