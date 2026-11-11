const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const native = @import("../native.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const ok = native.ok;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "len", Binding{ .intrinsic = .array_len } },
    .{ "append", Binding{ .intrinsic = .append } },
    .{ "unchecked_pop", Binding{ .intrinsic = .array_pop } },
    .{ "unchecked_insert", Binding{ .native = &arrayInsert } },
    .{ "unchecked_remove", Binding{ .native = &arrayRemove } },
    .{ "clear", Binding{ .native = &arrayClear } },
    .{ "index_of", Binding{ .native = &arrayIndexOf } },
    .{ "contains", Binding{ .native = &arrayContains } },
});

fn arrayInsert(vm: *VM, args: []v.Value) anyerror!NativeResult {
    var array = arrayArg(args, 0);
    if (array.needsResize()) array = try vm.growArray(&args[0]);
    const index: usize = @intCast(args[1].int64);
    const items = array.getDataPtr();
    std.mem.copyBackwards(v.Value, items[index + 1 .. array.len + 1], items[index..array.len]);
    items[index] = args[2];
    array.len += 1;
    return ok(.{ .null = {} });
}

fn arrayRemove(_: *VM, args: []v.Value) anyerror!NativeResult {
    const array = arrayArg(args, 0);
    const index: usize = @intCast(args[1].int64);
    const items = array.getDataPtr();
    const removed = items[index];
    std.mem.copyForwards(v.Value, items[index .. array.len - 1], items[index + 1 .. array.len]);
    array.len -= 1;
    return ok(removed);
}

fn arrayClear(_: *VM, args: []v.Value) anyerror!NativeResult {
    arrayArg(args, 0).len = 0;
    return ok(.{ .null = {} });
}

fn arrayIndexOf(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const index = findItem(args) orelse return ok(v.NULL_VALUE);
    return ok(try vm.boxInt(@intCast(index)));
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
