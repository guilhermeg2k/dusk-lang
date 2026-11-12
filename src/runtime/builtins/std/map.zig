const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const maps = @import("../../map.zig");
const native = @import("../native.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const ok = native.ok;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "len", Binding{ .native = &mapLen } },
    .{ "has", Binding{ .native = &mapHas } },
    .{ "remove", Binding{ .native = &mapRemove } },
    .{ "keys", Binding{ .native = &mapKeys } },
    .{ "values", Binding{ .native = &mapValues } },
    .{ "clear", Binding{ .native = &mapClear } },
});

fn mapLen(_: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .int64 = @intCast(mapArg(args).count()) });
}

fn mapHas(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .bool = maps.findValue(vm, mapArg(args), args[1]) != null });
}

fn mapRemove(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .bool = maps.removeEntry(vm, mapArg(args), args[1]) });
}

fn mapKeys(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(try collectEntries(vm, args, .key));
}

fn mapValues(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(try collectEntries(vm, args, .value));
}

fn mapClear(_: *VM, args: []v.Value) anyerror!NativeResult {
    maps.clear(mapArg(args));
    return ok(.{ .null = {} });
}

const EntryPart = enum { key, value };

fn collectEntries(vm: *VM, args: []v.Value, comptime part: EntryPart) !v.Value {
    const count = mapArg(args).count();
    try vm.maybeCollectGarbage(v.Array.calc_size(if (count != 0) count else 8));
    const map = mapArg(args);
    const kind = if (part == .key) map.key_type else map.value_type;
    const array = try v.Array.init(vm.getCurrentHeapAllocator(), kind, count);
    vm.increaseHeapAllocationCount(v.Array.calc_size(array.capacity));
    const storage = map.getStorage() orelse return .{ .heap_obj = &array.obj };
    for (storage.usedEntries()) |entry| {
        if (entry.isRemoved()) continue;
        try array.append(if (part == .key) entry.key else entry.value);
    }
    return .{ .heap_obj = &array.obj };
}

fn mapArg(args: []v.Value) *v.Map {
    return v.HeapValue.getParentPtr(v.Map, args[0].heap_obj);
}
