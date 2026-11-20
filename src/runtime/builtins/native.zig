const std = @import("std");
const v = @import("../value.zig");
const VM = @import("../vm.zig").VM;

pub const BindingMap = std.StaticStringMap(Binding);

pub const Binding = union(enum) {
    native: NativeFnPtr,
    intrinsic: Intrinsic,
    serialization: SerializationOperation,
};

pub const SerializationOperation = enum {
    derive_decode,
    derive_encode,
    blank,
    field_default,
    plan_of,
    is_proxied,
    has_proxies,
};

pub const NativeFnPtr = *const fn (vm: *VM, args: []v.Value) anyerror!NativeResult;

pub const NativeResult = struct { value: v.Value, has_failed: bool = false, is_parked: bool = false };

pub fn ownResource(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const id = try vm.event_loop.resources.add(vm.event_loop.allocator, @enumFromInt(args[0].int64), args[1].int64);
    const size = v.Resource.calc_size();
    try vm.maybeCollectGarbage(size);
    const owner = try v.Resource.init(vm.getCurrentHeapAllocator(), id);
    vm.increaseHeapAllocationCount(size);
    try vm.heap.trackResource(&owner.obj);
    return .{ .value = .{ .heap_obj = &owner.obj } };
}

pub fn releaseResource(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const raw: u64 = @bitCast(args[0]);
    if (raw == 0) {
        return .{ .value = .{ .null = {} } };
    }
    const owner = v.HeapValue.getParentPtr(v.Resource, args[0].heap_obj);
    vm.event_loop.resources.release(owner.id());
    return .{ .value = .{ .null = {} } };
}

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
