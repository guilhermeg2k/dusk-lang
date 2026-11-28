const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const native = @import("../native.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const ok = native.ok;
const bytesArg = native.bytesArg;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "len", Binding{ .intrinsic = .bytes_len } },
    .{ "unchecked_new", Binding{ .intrinsic = .new_buffer } },
    .{ "from_string", Binding{ .native = &bufferFrom } },
    .{ "put", Binding{ .native = &put } },
    .{ "put_bytes", Binding{ .native = &put } },
    .{ "slice", Binding{ .native = &slice } },
});

pub const BufferError = enum { NegativeSize, OutOfRange };
pub const error_sets = .{BufferError};

fn bufferFrom(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const size = v.Bytes.calc_size(bytesArg(args, 0).len);
    try vm.maybeCollectGarbage(size);
    const bytes = try v.Bytes.init(vm.getCurrentHeapAllocator(), bytesArg(args, 0), .buffer);
    vm.increaseHeapAllocationCount(size);
    return ok(.{ .heap_obj = &bytes.obj });
}

fn put(_: *VM, args: []v.Value) anyerror!NativeResult {
    return putBytes(args, bytesArg(args, 2));
}

fn putBytes(args: []v.Value, source: []const u8) NativeResult {
    const target = v.HeapValue.getParentPtr(v.Bytes, args[0].heap_obj).mutableSlice();
    const at = args[1].int64;
    const fits = at >= 0 and at <= target.len and source.len <= target.len - @as(usize, @intCast(at));
    if (!fits) {
        return native.fail(BufferError.OutOfRange);
    }
    const start: usize = @intCast(at);
    std.mem.copyForwards(u8, target[start .. start + source.len], source);
    return ok(.{ .int64 = @intCast(start + source.len) });
}

fn slice(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const length = bytesArg(args, 0).len;
    const start = args[1].int64;
    const stop = args[2].int64;
    const is_in_range = start >= 0 and start <= stop and stop <= length;
    if (!is_in_range) {
        return native.fail(BufferError.OutOfRange);
    }
    const size = v.Bytes.calc_size(@intCast(stop - start));
    try vm.maybeCollectGarbage(size);
    const bytes = try v.Bytes.init(vm.getCurrentHeapAllocator(), bytesArg(args, 0)[@intCast(start)..@intCast(stop)], .buffer);
    vm.increaseHeapAllocationCount(size);
    return ok(.{ .heap_obj = &bytes.obj });
}
