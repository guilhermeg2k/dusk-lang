const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const native = @import("../native.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const ok = native.ok;
const fail = native.fail;

pub const ArrayError = enum { Empty, OutOfRange };
pub const error_sets = .{ArrayError};

pub const bindings = native.BindingMap.initComptime(.{
    .{ "len", Binding{ .intrinsic = .array_len } },
    .{ "append", Binding{ .intrinsic = .append } },
    .{ "pop", Binding{ .native = &arrayPop } },
    .{ "insert", Binding{ .native = &arrayInsert } },
    .{ "remove", Binding{ .native = &arrayRemove } },
    .{ "sort", Binding{ .native = &arraySort } },
    .{ "extend", Binding{ .native = &arrayExtend } },
    .{ "clear", Binding{ .native = &arrayClear } },
    .{ "index_of", Binding{ .native = &arrayIndexOf } },
    .{ "contains", Binding{ .native = &arrayContains } },
});

fn arrayPop(_: *VM, args: []v.Value) anyerror!NativeResult {
    const array = arrayArg(args, 0);
    if (array.len == 0) {
        return fail(ArrayError.Empty);
    }
    array.len -= 1;
    return ok(array.getDataPtr()[array.len]);
}

fn arrayInsert(vm: *VM, args: []v.Value) anyerror!NativeResult {
    var array = arrayArg(args, 0);
    const index_value = args[1].int64;
    const is_out_of_range = index_value < 0 or index_value > array.len;
    if (is_out_of_range) {
        return fail(ArrayError.OutOfRange);
    }
    if (array.needsResize()) {
        array = try vm.growArray(&args[0]);
    }
    const index: usize = @intCast(index_value);
    const items = array.getDataPtr();
    std.mem.copyBackwards(v.Value, items[index + 1 .. array.len + 1], items[index..array.len]);
    items[index] = args[2];
    array.len += 1;
    return ok(.{ .null = {} });
}

fn arrayRemove(_: *VM, args: []v.Value) anyerror!NativeResult {
    const array = arrayArg(args, 0);
    const index_value = args[1].int64;
    const is_out_of_range = index_value < 0 or index_value >= array.len;
    if (is_out_of_range) {
        return fail(ArrayError.OutOfRange);
    }
    const index: usize = @intCast(index_value);
    const items = array.getDataPtr();
    const removed = items[index];
    std.mem.copyForwards(v.Value, items[index .. array.len - 1], items[index + 1 .. array.len]);
    array.len -= 1;
    return ok(removed);
}

fn arraySort(_: *VM, args: []v.Value) anyerror!NativeResult {
    const array = arrayArg(args, 0);
    std.sort.heap(v.Value, array.getDataPtr()[0..array.len], array.kind, isItemBefore);
    return ok(args[0]);
}

fn isItemBefore(kind: v.ValueType, left: v.Value, right: v.Value) bool {
    return switch (kind) {
        .int64 => left.int64 < right.int64,
        .float64 => if (std.math.isNan(right.float64)) !std.math.isNan(left.float64) else left.float64 < right.float64,
        .string => std.mem.order(u8, v.HeapValue.getParentPtr(v.Bytes, left.heap_obj).slice(), v.HeapValue.getParentPtr(v.Bytes, right.heap_obj).slice()) == .lt,
        else => unreachable,
    };
}

fn arrayExtend(vm: *VM, args: []v.Value) anyerror!NativeResult {
    var array = arrayArg(args, 0);
    const item_count = arrayArg(args, 1).len;
    if (item_count == 0) {
        return ok(.{ .null = {} });
    }
    const new_len = std.math.add(usize, array.len, item_count) catch return error.OutOfMemory;
    if (new_len > array.capacity) {
        const capacity = @max(new_len, array.calcNewCapacity());
        const size = v.Array.calc_size(capacity);
        try vm.maybeCollectGarbage(size);
        array = arrayArg(args, 0);
        const grown = try v.Array.init(vm.getCurrentHeapAllocator(), array.kind, capacity);
        @memcpy(grown.getDataPtr()[0..array.len], array.getDataPtr()[0..array.len]);
        grown.len = array.len;
        array.obj.relocated = &grown.obj;
        args[0].heap_obj = &grown.obj;
        array = grown;
        vm.increaseHeapAllocationCount(size);
    }
    const items = arrayArg(args, 1);
    @memcpy(array.getDataPtr()[array.len..new_len], items.getDataPtr()[0..item_count]);
    array.len = new_len;
    return ok(.{ .null = {} });
}

fn arrayClear(_: *VM, args: []v.Value) anyerror!NativeResult {
    arrayArg(args, 0).len = 0;
    return ok(.{ .null = {} });
}

fn arrayIndexOf(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const index = findItem(args) orelse return ok(v.NULL_VALUE);
    return ok(try vm.allocNullBox(.{ .int64 = @intCast(index) }, .int64));
}

fn arrayContains(_: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .bool = findItem(args) != null });
}

fn findItem(args: []v.Value) ?usize {
    const array = arrayArg(args, 0);
    const items = array.getDataPtr()[0..array.len];
    for (items, 0..) |item, i| {
        if (isSameItem(array.kind, item, args[1])) return i;
    }
    return null;
}

fn isSameItem(kind: v.ValueType, left: v.Value, right: v.Value) bool {
    return switch (kind) {
        .int64 => left.int64 == right.int64,
        .float64 => left.float64 == right.float64,
        .bool => left.bool == right.bool,
        .string => std.mem.eql(u8, v.HeapValue.getParentPtr(v.Bytes, left.heap_obj).slice(), v.HeapValue.getParentPtr(v.Bytes, right.heap_obj).slice()),
        else => left.heap_obj == right.heap_obj,
    };
}

fn arrayArg(args: []v.Value, index: usize) *v.Array {
    const current = args[index].heap_obj.followRelocated();
    args[index].heap_obj = current;
    return v.HeapValue.getParentPtr(v.Array, current);
}
