const std = @import("std");
const RuntimeErrorReporter = @import("runtime_error.zig").RuntimeErrorReporter;
const bc = @import("bytecode.zig");
const TaskState = @import("tasks/scheduler.zig").TaskState;

pub const NULL_VALUE: Value = @bitCast(@as(u64, 0));

pub const TRUE_VALUE = Value{
    .bool = true,
};

pub const FALSE_VALUE = Value{
    .bool = false,
};

pub const Value = extern union {
    const Self = @This();

    int64: i64,
    float64: f64,
    bool: bool,
    null: void,
    heap_obj: *HeapValue,
};

const HeapValueType = enum(u8) {
    array,
    @"struct",
    string,
    buffer,
    @"union",
    null_box,
    closure,
    cell,
    task,
    map,
    map_storage,
};

pub const HeapValue = extern struct {
    const Self = @This();

    kind: HeapValueType,
    gc_forward: ?*HeapValue = null,
    //note: maybe we should remove this relocated to decrease the size of this struct
    // needs to evaluate the tradeoffs
    relocated: ?*HeapValue = null,

    pub fn getParentPtr(comptime T: type, ptr: *Self) *T {
        return @as(*T, @fieldParentPtr("obj", ptr));
    }

    pub fn followRelocated(ptr: *Self) *Self {
        var cur = ptr;
        while (cur.relocated) |rel| cur = rel;
        return cur;
    }
};

pub const Struct = extern struct {
    const Self = @This();

    obj: HeapValue,
    descriptor_id: u16,
    field_count: u8,
    identity: u32,

    pub fn init(allocator: std.mem.Allocator, field_size: u8, desc_id: u16) !*Self {
        const total_bytes = Self.calc_size(field_size);

        const raw_memory = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(Self)), total_bytes);
        const struct_ptr: *Self = @ptrCast(@alignCast(raw_memory.ptr));

        struct_ptr.obj = .{
            .kind = .@"struct",
        };

        struct_ptr.descriptor_id = desc_id;
        struct_ptr.field_count = field_size;
        struct_ptr.identity = 0;
        @memset(struct_ptr.getDataPtr()[0..field_size], NULL_VALUE);

        return struct_ptr;
    }

    pub fn get(self: *const Self, i: usize) Value {
        const items = self.getDataPtr();
        return items[i];
    }

    pub fn set(self: *Self, i: usize, value: Value) void {
        const items = self.getDataPtr();
        items[i] = value;
    }

    pub fn getDataPtr(self: *const Self) [*]Value {
        const data_pointer = @as([*]Self, @ptrCast(@constCast(self))) + 1;
        return @ptrCast(data_pointer);
    }

    pub fn calc_size(field_count: usize) usize {
        return @sizeOf(Self) + (@sizeOf(Value) * field_count);
    }

    pub fn clone(self: *const Self, allocator: std.mem.Allocator) !*Self {
        const size = Self.calc_size(self.field_count);
        const old_bytes = @as([*]const u8, @ptrCast(self))[0..size];
        const new_bytes = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(Self)), size);
        @memcpy(new_bytes, old_bytes);
        const new_obj: *Self = @ptrCast(@alignCast(new_bytes.ptr));
        new_obj.obj.gc_forward = null;
        new_obj.obj.relocated = null;
        return new_obj;
    }
};

pub const Union = extern struct {
    const Self = @This();

    pub const NO_TAG: u16 = 0xFFFF;

    obj: HeapValue,
    descriptor_id: u16,
    active_tag: u16,

    pub fn init(allocator: std.mem.Allocator, desc_id: u16) !*Self {
        const total_bytes = Self.calc_size();

        const raw_memory = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(Self)), total_bytes);
        const union_ptr: *Self = @ptrCast(@alignCast(raw_memory.ptr));

        union_ptr.obj = .{
            .kind = .@"union",
        };

        union_ptr.descriptor_id = desc_id;
        union_ptr.active_tag = NO_TAG;

        return union_ptr;
    }

    pub fn get(self: *const Self, tag: u16) Value {
        const items = self.getDataPtr();

        if (self.active_tag != NO_TAG and tag == self.active_tag) {
            return items[0];
        }

        return NULL_VALUE;
    }

    pub fn set(self: *Self, tag: u16, value: Value) void {
        const items = self.getDataPtr();
        items[0] = value;
        self.active_tag = tag;
    }

    pub fn getDataPtr(self: *const Self) [*]Value {
        const data_pointer = @as([*]Self, @ptrCast(@constCast(self))) + 1;
        return @ptrCast(data_pointer);
    }

    pub fn calc_size() usize {
        return @sizeOf(Self) + (@sizeOf(Value));
    }

    pub fn clone(self: *const Self, allocator: std.mem.Allocator) !*Self {
        const size = Self.calc_size();
        const old_bytes = @as([*]const u8, @ptrCast(self))[0..size];
        const new_bytes = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(Self)), size);
        @memcpy(new_bytes, old_bytes);
        const new_obj: *Self = @ptrCast(@alignCast(new_bytes.ptr));
        new_obj.obj.gc_forward = null;
        new_obj.obj.relocated = null;
        return new_obj;
    }
};

pub const Array = extern struct {
    const Self = @This();

    obj: HeapValue,
    kind: ValueType,
    len: usize,
    capacity: usize,

    pub fn init(allocator: std.mem.Allocator, kind: ValueType, requested_capacity: usize) !*Self {
        const capacity = if (requested_capacity != 0) requested_capacity else 8;
        const total_bytes = Self.calc_size(capacity);
        const raw_memory = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(Self)), total_bytes);

        const array = @as(*Self, @ptrCast(raw_memory.ptr));

        array.obj = .{
            .kind = .array,
        };

        array.len = 0;
        array.kind = kind;
        array.capacity = capacity;

        return array;
    }

    pub fn get(self: *const Self, i: i64) !Value {
        if (i < 0 or i >= self.len) return RuntimeErrorReporter.arrayOutOfBounds(i, self.len);

        const items = self.getDataPtr();
        return items[@intCast(i)];
    }

    pub fn set(self: *Self, i: i64, value: Value) !void {
        if (i < 0 or i >= self.len) return RuntimeErrorReporter.arrayOutOfBounds(i, self.len);

        const items = self.getDataPtr();
        items[@intCast(i)] = value;
    }

    pub fn pop(self: *Self) !Value {
        //note: poping null if len == 0
        if (self.len == 0) {
            return NULL_VALUE;
        }

        self.len -= 1;

        return self.getDataPtr()[self.len];
    }

    pub fn needsResize(self: *Self) bool {
        return self.len + 1 > self.capacity;
    }

    pub fn calcNewCapacity(self: *Self) usize {
        return self.capacity + @max(self.capacity / 2, 1);
    }

    pub fn resize(self: *Self, allocator: std.mem.Allocator) !*Self {
        var cur_ptr = self;
        const new_capacity = self.calcNewCapacity();
        const new_size = Self.calc_size(new_capacity);
        const old_size = Self.calc_size(self.capacity);

        const old_raw_slice = @as([*]align(@alignOf(Self)) u8, @ptrCast(cur_ptr))[0..old_size];
        const new_raw_slice = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(Self)), new_size);

        @memcpy(new_raw_slice[0..old_size], old_raw_slice);

        const new_ptr = @as(*Self, @ptrCast(new_raw_slice.ptr));

        new_ptr.obj = .{
            .kind = .array,
        };

        cur_ptr.obj.relocated = &new_ptr.obj;
        cur_ptr = new_ptr;
        cur_ptr.capacity = new_capacity;
        return cur_ptr;
    }

    pub fn append(self: *Self, value: Value) !void {
        const items = self.getDataPtr();
        items[self.len] = value;
        self.len += 1;
    }

    pub fn getDataPtr(self: *const Self) [*]Value {
        const data_pointer = @as([*]Self, @ptrCast(@constCast(self))) + 1;
        return @ptrCast(data_pointer);
    }

    pub fn calc_size(capacity: usize) usize {
        const self_size = @sizeOf(Array);
        const data_size = @sizeOf(Value) * capacity;
        return self_size + data_size;
    }

    pub fn clone(self: *const Self, allocator: std.mem.Allocator) !*Self {
        const size = Self.calc_size(self.capacity);
        const old_bytes = @as([*]const u8, @ptrCast(self))[0..size];
        const new_bytes = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(Self)), size);
        @memcpy(new_bytes, old_bytes);
        const new_obj: *Self = @ptrCast(@alignCast(new_bytes.ptr));
        new_obj.obj.gc_forward = null;
        new_obj.obj.relocated = null;
        return new_obj;
    }
};

pub const Map = extern struct {
    const Self = @This();

    obj: HeapValue,
    key_type: ValueType,
    value_type: ValueType,
    next_seq: u32,
    storage: Value,

    pub fn init(allocator: std.mem.Allocator, key_type: ValueType, value_type: ValueType) !*Self {
        const ptr = try allocator.create(Self);
        ptr.* = .{ .obj = .{ .kind = .map }, .key_type = key_type, .value_type = value_type, .next_seq = 1, .storage = NULL_VALUE };
        return ptr;
    }

    pub fn getStorage(self: *const Self) ?*MapStorage {
        if (@as(u64, @bitCast(self.storage)) == 0) return null;
        return HeapValue.getParentPtr(MapStorage, self.storage.heap_obj);
    }

    pub fn count(self: *const Self) usize {
        const storage = self.getStorage() orelse return 0;
        return storage.count;
    }

    pub fn calc_size() usize {
        return @sizeOf(Self);
    }

    pub fn clone(self: *const Self, allocator: std.mem.Allocator) !*Self {
        const ptr = try allocator.create(Self);
        ptr.* = self.*;
        ptr.obj.gc_forward = null;
        ptr.obj.relocated = null;
        return ptr;
    }
};

pub const MapEntry = extern struct {
    pub const removed_bit: u32 = 1 << 31;

    key: Value,
    value: Value,
    hash: u32,
    seq: u32,

    pub fn isRemoved(self: *const MapEntry) bool {
        return self.hash & removed_bit != 0;
    }
};

pub const MapStorage = extern struct {
    const Self = @This();

    pub const min_capacity: u32 = 8;

    obj: HeapValue,
    key_type: ValueType,
    value_type: ValueType,
    len: u32,
    count: u32,
    capacity: u32,
    index_mask: u32,

    pub fn init(allocator: std.mem.Allocator, key_type: ValueType, value_type: ValueType, capacity: u32) !*Self {
        const raw_memory = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(Self)), Self.calc_size(capacity));
        const storage: *Self = @ptrCast(raw_memory.ptr);
        storage.* = .{
            .obj = .{ .kind = .map_storage },
            .key_type = key_type,
            .value_type = value_type,
            .len = 0,
            .count = 0,
            .capacity = capacity,
            .index_mask = indexSlotCount(capacity) - 1,
        };
        @memset(storage.index(), 0);
        return storage;
    }

    pub fn entries(self: *const Self) []MapEntry {
        const data_pointer: [*]MapEntry = @ptrCast(@as([*]Self, @ptrCast(@constCast(self))) + 1);
        return data_pointer[0..self.capacity];
    }

    pub fn usedEntries(self: *const Self) []MapEntry {
        return self.entries()[0..self.len];
    }

    pub fn index(self: *const Self) []u32 {
        const after_entries: [*]u32 = @ptrCast(self.entries().ptr + self.capacity);
        return after_entries[0 .. self.index_mask + 1];
    }

    pub fn isFull(self: *const Self) bool {
        return self.len == self.capacity;
    }

    pub fn indexSlotCount(capacity: u32) u32 {
        return std.math.ceilPowerOfTwoAssert(u32, capacity * 2);
    }

    pub fn calc_size(capacity: u32) usize {
        return @sizeOf(Self) + @sizeOf(MapEntry) * @as(usize, capacity) + @sizeOf(u32) * @as(usize, indexSlotCount(capacity));
    }

    pub fn clone(self: *const Self, allocator: std.mem.Allocator) !*Self {
        const size = Self.calc_size(self.capacity);
        const old_bytes = @as([*]const u8, @ptrCast(self))[0..size];
        const new_bytes = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(Self)), size);
        @memcpy(new_bytes, old_bytes);
        const new_obj: *Self = @ptrCast(@alignCast(new_bytes.ptr));
        new_obj.obj.gc_forward = null;
        new_obj.obj.relocated = null;
        return new_obj;
    }
};

pub const Bytes = extern struct {
    const Self = @This();

    obj: HeapValue,
    len: usize,

    pub fn init(allocator: std.mem.Allocator, source: []const u8, kind: HeapValueType) !*Self {
        const bytes = try Self.initUndefined(allocator, source.len, kind);
        @memcpy(bytes.getDataPtr()[0..source.len], source);
        return bytes;
    }

    pub fn initZeroed(allocator: std.mem.Allocator, len: usize, kind: HeapValueType) !*Self {
        const bytes = try Self.initUndefined(allocator, len, kind);
        @memset(bytes.getDataPtr()[0..len], 0);
        return bytes;
    }

    pub fn initUndefined(allocator: std.mem.Allocator, len: usize, kind: HeapValueType) !*Self {
        const raw_memory = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(Self)), Self.calc_size(len));
        const bytes = @as(*Self, @ptrCast(raw_memory.ptr));
        bytes.obj = .{ .kind = kind };
        bytes.len = len;
        return bytes;
    }

    pub fn slice(self: *const Self) []const u8 {
        return self.getDataPtr()[0..self.len];
    }

    pub fn mutableSlice(self: *Self) []u8 {
        return self.getDataPtr()[0..self.len];
    }

    pub fn eql(self: *const Self, other: *const Self) bool {
        if (self == other) return true;
        if (self.len != other.len) return false;
        return std.mem.eql(u8, self.slice(), other.slice());
    }

    pub fn getDataPtr(self: *const Self) [*]u8 {
        const data_pointer = @as([*]Self, @ptrCast(@constCast(self))) + 1;
        return @ptrCast(data_pointer);
    }

    pub fn calc_size(len: usize) usize {
        return @sizeOf(Self) + len;
    }

    pub fn clone(self: *const Self, allocator: std.mem.Allocator) !*Self {
        const size = Self.calc_size(self.len);
        const old_bytes = @as([*]const u8, @ptrCast(self))[0..size];
        const new_bytes = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(Self)), size);
        @memcpy(new_bytes, old_bytes);

        const new_obj: *Self = @ptrCast(@alignCast(new_bytes.ptr));
        new_obj.obj.gc_forward = null;
        new_obj.obj.relocated = null;

        return new_obj;
    }
};

pub const NullBox = extern struct {
    const Self = @This();

    obj: HeapValue,
    kind: ValueType,
    value: Value,

    pub fn init(allocator: std.mem.Allocator, value: Value, kind: ValueType) !*Self {
        const ptr = try allocator.create(Self);
        ptr.value = value;
        ptr.kind = kind;
        ptr.obj = .{ .kind = .null_box };
        return ptr;
    }

    pub fn clone(self: *Self, allocator: std.mem.Allocator) !*Self {
        return try Self.init(allocator, self.value, self.kind);
    }

    pub fn calc_size() usize {
        return @sizeOf(Self);
    }
};

pub const Closure = extern struct {
    const Self = @This();

    obj: HeapValue,
    function: *const bc.Function,
    capture_count: u32,

    pub fn initStatic(function: *const bc.Function) Self {
        return .{ .obj = .{ .kind = .closure }, .function = function, .capture_count = 0 };
    }

    pub fn isStatic(obj: *const HeapValue) bool {
        return obj.kind == .closure and HeapValue.getParentPtr(Self, @constCast(obj)).capture_count == 0;
    }

    pub fn init(allocator: std.mem.Allocator, function: *const bc.Function, capture_count: usize) !*Self {
        const raw_memory = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(Self)), Self.calc_size(capture_count));
        const closure: *Self = @ptrCast(raw_memory.ptr);
        closure.* = .{ .obj = .{ .kind = .closure }, .function = function, .capture_count = @intCast(capture_count) };
        return closure;
    }

    pub fn captures(self: *Self) []Value {
        const data_pointer: [*]Value = @ptrCast(@as([*]Self, @ptrCast(self)) + 1);
        return data_pointer[0..self.capture_count];
    }

    pub fn calc_size(capture_count: usize) usize {
        return @sizeOf(Self) + capture_count * @sizeOf(Value);
    }

    pub fn clone(self: *const Self, allocator: std.mem.Allocator) !*Self {
        const size = Self.calc_size(self.capture_count);
        const old_bytes = @as([*]const u8, @ptrCast(self))[0..size];
        const new_bytes = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(Self)), size);
        @memcpy(new_bytes, old_bytes);
        const new_obj: *Self = @ptrCast(@alignCast(new_bytes.ptr));
        new_obj.obj.gc_forward = null;
        new_obj.obj.relocated = null;
        return new_obj;
    }
};

pub const Cell = extern struct {
    const Self = @This();

    obj: HeapValue,
    kind: ValueType,
    value: Value,

    pub fn init(allocator: std.mem.Allocator, value: Value, kind: ValueType) !*Self {
        const ptr = try allocator.create(Self);
        ptr.* = .{ .obj = .{ .kind = .cell }, .kind = kind, .value = value };
        return ptr;
    }

    pub fn clone(self: *Self, allocator: std.mem.Allocator) !*Self {
        return Self.init(allocator, self.value, self.kind);
    }

    pub fn calc_size() usize {
        return @sizeOf(Self);
    }
};

pub const Task = extern struct {
    const Self = @This();

    obj: HeapValue,
    state: *TaskState,

    pub fn init(allocator: std.mem.Allocator, state: *TaskState) !*Self {
        const ptr = try allocator.create(Self);
        ptr.* = .{ .obj = .{ .kind = .task }, .state = state };
        return ptr;
    }

    pub fn clone(self: *Self, allocator: std.mem.Allocator) !*Self {
        return Self.init(allocator, self.state);
    }

    pub fn calc_size() usize {
        return @sizeOf(Self);
    }
};

pub const ValueType = enum(u8) {
    int64,
    float64,
    bool,
    null,

    string,
    buffer,
    //note: maybe this needs to get the inner type?
    array,
    @"struct",
    @"union",
    nullable,
    function,
    cell,
    task,
    map,

    pub fn isHeapType(self: ValueType) bool {
        return switch (self) {
            .string, .buffer, .array, .@"struct", .@"union", .nullable, .function, .cell, .task, .map => true,
            else => false,
        };
    }
};
