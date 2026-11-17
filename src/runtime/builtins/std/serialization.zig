const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const serialization_plan = @import("../../serialization_plan.zig");
const native = @import("../native.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "field_count", Binding{ .native = &fieldCount } },
    .{ "field_name", Binding{ .native = &fieldName } },
    .{ "find_field", Binding{ .native = &findField } },
    .{ "find_field_in", Binding{ .native = &findFieldIn } },
    .{ "variant_count", Binding{ .native = &variantCount } },
    .{ "variant_name", Binding{ .native = &variantName } },
    .{ "find_variant", Binding{ .native = &findVariant } },
    .{ "find_variant_in", Binding{ .native = &findVariantIn } },
    .{ "plan_of", Binding{ .serialization = .plan_of } },
    .{ "is_native", Binding{ .serialization = .is_native } },
    .{ "derive_decode", Binding{ .serialization = .derive_decode } },
    .{ "derive_encode", Binding{ .serialization = .derive_encode } },
    .{ "blank", Binding{ .serialization = .blank } },
    .{ "field_default", Binding{ .serialization = .field_default } },
});

fn fieldCount(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const fields = fieldsOf(vm, args[0].int64) orelse return native.ok(.{ .int64 = 0 });
    return native.ok(.{ .int64 = @intCast(fields.len) });
}

fn fieldName(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const name = nameAt(vm, args[0].int64, args[1].int64) orelse "";
    const result = try vm.allocString(name.len);
    @memcpy(result.mutableSlice(), name);
    return native.stringResult(result);
}

fn findField(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const fields = fieldsOf(vm, args[0].int64) orelse return native.ok(.{ .int64 = -1 });
    return native.ok(.{ .int64 = indexOfName(fields, native.bytesArg(args, 1)) });
}

fn findFieldIn(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const fields = fieldsOf(vm, args[0].int64) orelse return native.ok(.{ .int64 = -1 });
    const text = native.bytesArg(args, 1);
    const start = args[2].int64;
    const stop = args[3].int64;
    const is_in_bounds = start >= 0 and start <= stop and stop <= text.len;
    if (!is_in_bounds) {
        return native.ok(.{ .int64 = -1 });
    }
    return native.ok(.{ .int64 = indexOfName(fields, text[@intCast(start)..@intCast(stop)]) });
}

fn variantCount(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const shape = variantShapeOf(vm, args[0].int64) orelse return native.ok(.{ .int64 = 0 });
    return native.ok(.{ .int64 = @intCast(variantCountOf(shape)) });
}

fn variantName(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const shape = variantShapeOf(vm, args[0].int64);
    const index = args[1].int64;
    const is_in_range = shape != null and index >= 0 and index < variantCountOf(shape.?);
    const name = if (is_in_range) variantNameAt(shape.?, @intCast(index)) else "";
    const result = try vm.allocString(name.len);
    @memcpy(result.mutableSlice(), name);
    return native.stringResult(result);
}

fn findVariant(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const shape = variantShapeOf(vm, args[0].int64) orelse return native.ok(.{ .int64 = -1 });
    return native.ok(.{ .int64 = indexOfVariant(shape, native.bytesArg(args, 1)) });
}

fn findVariantIn(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const shape = variantShapeOf(vm, args[0].int64) orelse return native.ok(.{ .int64 = -1 });
    const text = native.bytesArg(args, 1);
    const start = args[2].int64;
    const stop = args[3].int64;
    const is_in_bounds = start >= 0 and start <= stop and stop <= text.len;
    if (!is_in_bounds) {
        return native.ok(.{ .int64 = -1 });
    }
    return native.ok(.{ .int64 = indexOfVariant(shape, text[@intCast(start)..@intCast(stop)]) });
}

fn variantShapeOf(vm: *VM, id: i64) ?serialization_plan.Shape {
    const plans = vm.program.serialization_plans;
    const is_out_of_range = id < 0 or id >= plans.len;
    if (is_out_of_range) {
        return null;
    }
    const shape = plans[@intCast(id)].shape;
    return switch (shape) {
        .enumeration, .tagged_union => shape,
        else => null,
    };
}

fn variantCountOf(shape: serialization_plan.Shape) usize {
    return switch (shape) {
        .enumeration => |variants| variants.len,
        .tagged_union => |tagged| tagged.variants.len,
        else => 0,
    };
}

fn variantNameAt(shape: serialization_plan.Shape, index: usize) []const u8 {
    return switch (shape) {
        .enumeration => |variants| variants[index].name,
        .tagged_union => |tagged| tagged.variants[index].name,
        else => "",
    };
}

fn indexOfVariant(shape: serialization_plan.Shape, text: []const u8) i64 {
    for (0..variantCountOf(shape)) |i| {
        const is_named = switch (shape) {
            .enumeration => |variants| variants[i].isNamed(text),
            .tagged_union => |tagged| tagged.variants[i].isNamed(text),
            else => false,
        };
        if (is_named) {
            return @intCast(i);
        }
    }
    return -1;
}

fn fieldsOf(vm: *VM, id: i64) ?[]const serialization_plan.Field {
    const plans = vm.program.serialization_plans;
    const is_out_of_range = id < 0 or id >= plans.len;
    if (is_out_of_range) {
        return null;
    }
    return switch (plans[@intCast(id)].shape) {
        .structure => |structure| structure.fields,
        else => null,
    };
}

fn nameAt(vm: *VM, id: i64, index: i64) ?[]const u8 {
    const fields = fieldsOf(vm, id) orelse return null;
    const is_out_of_range = index < 0 or index >= fields.len;
    if (is_out_of_range) {
        return null;
    }
    return fields[@intCast(index)].name;
}

fn indexOfName(fields: []const serialization_plan.Field, name: []const u8) i64 {
    for (fields, 0..) |field, i| {
        if (field.isNamed(name)) {
            return @intCast(i);
        }
    }
    return -1;
}
