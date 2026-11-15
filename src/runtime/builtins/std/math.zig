const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const RuntimeErrorReporter = @import("../../runtime_error.zig").RuntimeErrorReporter;
const native = @import("../native.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const ok = native.ok;
const fail = native.fail;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "min", Binding{ .native = &arrayMin } },
    .{ "max", Binding{ .native = &arrayMax } },
    .{ "abs", Binding{ .native = &intAbs } },
    .{ "f_abs", Binding{ .native = &floatAbs } },
    .{ "clamp", Binding{ .native = &intClamp } },
    .{ "f_clamp", Binding{ .native = &floatClamp } },
    .{ "floor", Binding{ .native = &floor } },
    .{ "ceil", Binding{ .native = &ceil } },
    .{ "round", Binding{ .native = &round } },
    .{ "trunc", Binding{ .native = &trunc } },
    .{ "sqrt", Binding{ .native = &sqrt } },
    .{ "pow", Binding{ .native = &pow } },
    .{ "exp", Binding{ .native = &exp } },
    .{ "log", Binding{ .native = &log } },
    .{ "log2", Binding{ .native = &log2 } },
    .{ "log10", Binding{ .native = &log10 } },
    .{ "sin", Binding{ .native = &sin } },
    .{ "cos", Binding{ .native = &cos } },
    .{ "tan", Binding{ .native = &tan } },
    .{ "asin", Binding{ .native = &asin } },
    .{ "acos", Binding{ .native = &acos } },
    .{ "atan", Binding{ .native = &atan } },
    .{ "atan2", Binding{ .native = &atan2 } },
    .{ "is_nan", Binding{ .native = &isNan } },
    .{ "is_inf", Binding{ .native = &isInf } },
    .{ "is_finite", Binding{ .native = &isFinite } },
});

pub const MathError = enum { InvalidBounds };
pub const error_sets = .{MathError};

fn arrayMin(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return arrayExtremum(vm, args, .min);
}

fn arrayExtremum(vm: *VM, args: []v.Value, comptime operation: enum { min, max }) !NativeResult {
    const array = v.HeapValue.getParentPtr(v.Array, args[0].heap_obj.followRelocated());
    if (array.len == 0) {
        return ok(v.NULL_VALUE);
    }
    const items = array.getDataPtr()[0..array.len];
    var result = items[0];
    switch (array.kind) {
        .int64 => {
            for (items[1..]) |item| {
                result.int64 = if (operation == .min) @min(result.int64, item.int64) else @max(result.int64, item.int64);
            }
        },
        .float64 => {
            for (items[1..]) |item| {
                if (std.math.isNan(result.float64)) break;
                if (std.math.isNan(item.float64)) {
                    result = item;
                    break;
                }
                const are_both_zero = result.float64 == 0 and item.float64 == 0;
                if (are_both_zero) {
                    const should_use_item = if (operation == .min) std.math.signbit(item.float64) else !std.math.signbit(item.float64);
                    if (should_use_item) {
                        result = item;
                    }
                    continue;
                }
                result.float64 = if (operation == .min) @min(result.float64, item.float64) else @max(result.float64, item.float64);
            }
        },
        else => unreachable,
    }
    return ok(try vm.allocNullBox(result, array.kind));
}

fn arrayMax(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return arrayExtremum(vm, args, .max);
}

fn intAbs(_: *VM, args: []v.Value) anyerror!NativeResult {
    const value = args[0].int64;
    if (value == std.math.minInt(i64)) {
        return RuntimeErrorReporter.negationOverflow(value);
    }
    return ok(.{ .int64 = @intCast(@abs(value)) });
}

fn floatAbs(_: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .float64 = @abs(args[0].float64) });
}

fn intClamp(_: *VM, args: []v.Value) anyerror!NativeResult {
    const lower = args[1].int64;
    const upper = args[2].int64;
    if (lower > upper) {
        return fail(MathError.InvalidBounds);
    }
    return ok(.{ .int64 = @min(@max(args[0].int64, lower), upper) });
}

fn floatClamp(_: *VM, args: []v.Value) anyerror!NativeResult {
    const value = args[0].float64;
    const lower = args[1].float64;
    const upper = args[2].float64;
    const are_invalid_bounds = std.math.isNan(lower) or std.math.isNan(upper) or lower > upper;
    if (are_invalid_bounds) {
        return fail(MathError.InvalidBounds);
    }
    if (std.math.isNan(value)) {
        return ok(.{ .float64 = value });
    }
    return ok(.{ .float64 = @min(@max(value, lower), upper) });
}

fn floor(_: *VM, args: []v.Value) anyerror!NativeResult {
    return roundedIntResult(@floor(args[0].float64));
}

fn roundedIntResult(value: f64) !NativeResult {
    const is_in_range = value >= -0x1p63 and value < 0x1p63;
    if (!is_in_range) {
        return RuntimeErrorReporter.floatOutOfIntRange(value);
    }
    return ok(.{ .int64 = @intFromFloat(value) });
}

fn ceil(_: *VM, args: []v.Value) anyerror!NativeResult {
    return roundedIntResult(@ceil(args[0].float64));
}

fn round(_: *VM, args: []v.Value) anyerror!NativeResult {
    return roundedIntResult(@round(args[0].float64));
}

fn trunc(_: *VM, args: []v.Value) anyerror!NativeResult {
    return roundedIntResult(@trunc(args[0].float64));
}

fn sqrt(_: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .float64 = @sqrt(args[0].float64) });
}

fn pow(_: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .float64 = std.math.pow(f64, args[0].float64, args[1].float64) });
}

fn exp(_: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .float64 = @exp(args[0].float64) });
}

fn log(_: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .float64 = @log(args[0].float64) });
}

fn log2(_: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .float64 = @log2(args[0].float64) });
}

fn log10(_: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .float64 = @log10(args[0].float64) });
}

fn sin(_: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .float64 = @sin(args[0].float64) });
}

fn cos(_: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .float64 = @cos(args[0].float64) });
}

fn tan(_: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .float64 = @tan(args[0].float64) });
}

fn asin(_: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .float64 = std.math.asin(args[0].float64) });
}

fn acos(_: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .float64 = std.math.acos(args[0].float64) });
}

fn atan(_: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .float64 = std.math.atan(args[0].float64) });
}

fn atan2(_: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .float64 = std.math.atan2(args[0].float64, args[1].float64) });
}

fn isNan(_: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .bool = std.math.isNan(args[0].float64) });
}

fn isInf(_: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .bool = std.math.isInf(args[0].float64) });
}

fn isFinite(_: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .bool = std.math.isFinite(args[0].float64) });
}
