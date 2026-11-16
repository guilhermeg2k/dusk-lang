const std = @import("std");
const v = @import("../value.zig");
const VM = @import("../vm.zig").VM;
const reflection_format = @import("../reflection_format.zig");

pub const BindingMap = std.StaticStringMap(Binding);

pub const Binding = union(enum) {
    native: NativeFnPtr,
    intrinsic: Intrinsic,
    reflection: ReflectionBinding,
};

pub const ReflectionBinding = struct {
    operation: ReflectionOperation,
    func: NativeFnPtr,
    requirements: ?ValueRequirements = null,
};

pub const ValueRequirements = struct {
    allowed_kinds: u32,
    needs_string_keys: bool,

    pub fn init(comptime kinds: []const reflection_format.Kind, needs_string_keys: bool) ValueRequirements {
        var mask: u32 = 0;
        for (kinds) |kind| {
            mask |= @as(u32, 1) << @intFromEnum(kind);
        }
        return .{ .allowed_kinds = mask, .needs_string_keys = needs_string_keys };
    }
};

pub const ReflectionOperation = enum {
    type_of,
    type_metadata,
    type_and_result_descriptor,
    value,
    cast,
    result_descriptor,
    result_type,
    require_value,

    pub fn hiddenArgumentCount(self: ReflectionOperation) u8 {
        return switch (self) {
            .type_of, .value, .type_and_result_descriptor => 2,
            .cast, .result_descriptor, .result_type, .type_metadata => 1,
            .require_value => 0,
        };
    }
};

pub const NativeFnPtr = *const fn (vm: *VM, args: []v.Value) anyerror!NativeResult;

pub const NativeResult = struct { value: v.Value, has_failed: bool = false, is_parked: bool = false };

pub const Intrinsic = enum {
    array_len,
    bytes_len,
    new_buffer,
    to_byte,
    to_int,
    to_float,
    append,
    copy,
    task_spawn,
    strings_equal,
};

pub fn ok(value: v.Value) NativeResult {
    return .{ .value = value };
}

pub fn fail(variant: anytype) NativeResult {
    return .{ .value = .{ .int64 = @intFromEnum(variant) }, .has_failed = true };
}

pub fn parked() NativeResult {
    return .{ .value = v.NULL_VALUE, .is_parked = true };
}

pub fn stringResult(result: *v.Bytes) NativeResult {
    return ok(.{ .heap_obj = &result.obj });
}

pub fn optionalBytesArg(args: []const v.Value, index: usize) ?[]const u8 {
    if (@as(u64, @bitCast(args[index])) == 0) {
        return null;
    }
    return bytesArg(args, index);
}

pub fn bytesArg(args: []const v.Value, index: usize) []const u8 {
    return v.HeapValue.getParentPtr(v.Bytes, args[index].heap_obj).slice();
}
