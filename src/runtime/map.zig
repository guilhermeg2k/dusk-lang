const std = @import("std");
const v = @import("value.zig");
const VM = @import("vm.zig").VM;

pub fn newMap(vm: *VM, key_type: v.ValueType, value_type: v.ValueType) !v.Value {
    const size = v.Map.calc_size();
    try vm.maybeCollectGarbage(size);
    const map = try v.Map.init(vm.getCurrentHeapAllocator(), key_type, value_type);
    vm.increaseHeapAllocationCount(size);
    return .{ .heap_obj = &map.obj };
}

pub fn setEntry(vm: *VM, map_index: usize, key_index: usize, value_index: usize) !void {
    const hash = hashKey(vm, mapAt(vm, map_index).key_type, vm.stack[key_index]);
    if (mapAt(vm, map_index).find(vm.stack[key_index], hash)) |entry| {
        entry.value = vm.stack[value_index];
        return;
    }
    try ensureRoomForEntry(vm, map_index);
    mapAt(vm, map_index).append(vm.stack[key_index], vm.stack[value_index], hash);
}

pub fn readEntry(vm: *VM, map_index: usize, key_index: usize) !v.Value {
    const map = mapAt(vm, map_index);
    const value = findValue(vm, map, vm.stack[key_index]) orelse return v.NULL_VALUE;
    if (map.value_type.isHeapType() or map.value_type == .null) return value;
    return boxValue(vm, value, map.value_type);
}

pub fn findEntryInto(vm: *VM, found_index: usize, map_index: usize, key_index: usize) void {
    const value = findValue(vm, mapAt(vm, map_index), vm.stack[key_index]);
    vm.stack[found_index] = if (value != null) v.TRUE_VALUE else v.FALSE_VALUE;
    vm.stack[found_index + 1] = value orelse v.NULL_VALUE;
}

pub fn removeEntry(vm: *VM, map: *v.Map, key: v.Value) bool {
    return map.remove(key, hashKey(vm, map.key_type, key));
}

pub fn advanceCursor(vm: *VM, map_index: usize, cursor_index: usize) v.Value {
    const storage = mapAt(vm, map_index).getStorage() orelse return v.FALSE_VALUE;
    const cursor = Cursor.unpack(vm.stack[cursor_index]);
    const entries = storage.usedEntries();
    var position = cursor.resumePosition(entries);
    while (position < entries.len) : (position += 1) {
        if (entries[position].isRemoved()) continue;
        vm.stack[cursor_index] = Cursor.pack(.{ .next_position = position + 1, .seq = entries[position].seq });
        return v.TRUE_VALUE;
    }
    return v.FALSE_VALUE;
}

pub fn entryAtCursor(vm: *VM, map_index: usize, cursor_index: usize) *v.MapEntry {
    const storage = mapAt(vm, map_index).getStorage().?;
    return &storage.entries()[Cursor.unpack(vm.stack[cursor_index]).next_position - 1];
}

const Cursor = struct {
    next_position: u32,
    seq: u32,

    fn unpack(value: v.Value) Cursor {
        const raw: u64 = @bitCast(value.int64);
        return .{ .next_position = @truncate(raw), .seq = @truncate(raw >> 32) };
    }

    fn pack(cursor: Cursor) v.Value {
        const raw = @as(u64, cursor.next_position) | (@as(u64, cursor.seq) << 32);
        return .{ .int64 = @bitCast(raw) };
    }

    fn resumePosition(cursor: Cursor, entries: []const v.MapEntry) u32 {
        if (cursor.next_position == 0) return 0;
        const visited = cursor.next_position - 1;
        const is_visited_in_place = visited < entries.len and entries[visited].seq == cursor.seq;
        if (is_visited_in_place) return cursor.next_position;
        return firstEntryAfter(entries, cursor.seq);
    }

    fn firstEntryAfter(entries: []const v.MapEntry, seq: u32) u32 {
        var low: usize = 0;
        var high: usize = entries.len;
        while (low < high) {
            const middle = low + (high - low) / 2;
            if (entries[middle].seq <= seq) {
                low = middle + 1;
            } else {
                high = middle;
            }
        }
        return @intCast(low);
    }
};

pub fn findValue(vm: *VM, map: *v.Map, key: v.Value) ?v.Value {
    const entry = map.find(key, hashKey(vm, map.key_type, key)) orelse return null;
    return entry.value;
}

fn boxValue(vm: *VM, value: v.Value, kind: v.ValueType) !v.Value {
    const size = v.NullBox.calc_size();
    try vm.maybeCollectGarbage(size);
    const box = try v.NullBox.init(vm.getCurrentHeapAllocator(), value, kind);
    vm.increaseHeapAllocationCount(size);
    return .{ .heap_obj = &box.obj };
}

fn mapAt(vm: *VM, index: usize) *v.Map {
    return v.HeapValue.getParentPtr(v.Map, vm.stack[index].heap_obj);
}

fn hashKey(vm: *VM, key_type: v.ValueType, key: v.Value) u32 {
    const raw: u64 = switch (key_type) {
        .int64 => @bitCast(key.int64),
        .bool => @intFromBool(key.bool),
        .string => std.hash.Wyhash.hash(0, v.HeapValue.getParentPtr(v.Bytes, key.heap_obj).slice()),
        .@"struct" => structIdentity(vm, v.HeapValue.getParentPtr(v.Struct, key.heap_obj)),
        else => unreachable,
    };
    return @as(u32, @truncate(std.hash.int(raw))) & ~v.MapEntry.removed_bit;
}

fn structIdentity(vm: *VM, s: *v.Struct) u32 {
    if (s.identity != 0) return s.identity;
    s.identity = vm.next_struct_identity;
    vm.next_struct_identity +%= 1;
    if (vm.next_struct_identity == 0) {
        vm.next_struct_identity = 1;
    }
    return s.identity;
}

fn ensureRoomForEntry(vm: *VM, map_index: usize) !void {
    const old_storage = mapAt(vm, map_index).getStorage();
    if (old_storage) |storage| {
        if (!storage.isFull()) return;
    }
    const capacity = if (old_storage) |storage| storage.nextCapacity() else v.MapStorage.min_capacity;
    const size = v.MapStorage.calc_size(capacity);
    try vm.maybeCollectGarbage(size);
    const map = mapAt(vm, map_index);
    const storage = try v.MapStorage.init(vm.getCurrentHeapAllocator(), map.key_type, map.value_type, capacity);
    vm.increaseHeapAllocationCount(size);
    if (map.getStorage()) |previous| {
        storage.copyLiveEntriesFrom(previous);
    }
    map.storage = .{ .heap_obj = &storage.obj };
}
