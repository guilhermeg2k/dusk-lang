const std = @import("std");
const v = @import("../value.zig");
const VM = @import("../vm.zig").VM;

pub const BindingMap = std.StaticStringMap(Binding);

pub const Binding = union(enum) {
    native: NativeFnPtr,
    intrinsic: Intrinsic,
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
