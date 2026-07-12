const std = @import("std");
const ir = @import("ir.zig");
const RunTimeError = @import("error.zig").RunTimeError;
const tt = @import("type_table.zig");

const NULL_VALUE = Value{
    .null = {},
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

    pub fn followRelocated(ptr: *Self) ?*Self {
        var cur = ptr;
        while (cur.relocated) |rel| {
            cur = rel;
        }

        if (cur == ptr) {
            return null;
        }

        return cur;
    }
};

pub const Struct = extern struct {
    const Self = @This();

    obj: HeapValue,
    descriptor_id: u16,
    field_count: u8,

    pub fn init(allocator: std.mem.Allocator, field_size: u8, desc_id: u16) !*Self {
        const total_bytes = Self.calc_size(field_size);

        const raw_memory = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(Self)), total_bytes);
        const struct_ptr: *Self = @ptrCast(@alignCast(raw_memory.ptr));

        struct_ptr.obj = .{
            .kind = .@"struct",
        };

        struct_ptr.descriptor_id = desc_id;
        struct_ptr.field_count = field_size;

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

pub const Array = extern struct {
    const Self = @This();

    obj: HeapValue,
    kind: ValueType,
    len: usize,
    capacity: usize,

    pub fn init(allocator: std.mem.Allocator, kind: ValueType, capacity: usize) !*Self {
        const total_bytes = Self.calc_size(capacity);
        const raw_memory = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(Self)), total_bytes);

        const array = @as(*Self, @ptrCast(raw_memory.ptr));

        array.obj = .{
            .kind = .array,
        };

        array.len = 0;
        array.kind = kind;
        array.capacity = if (capacity != 0) capacity else 8;

        return array;
    }

    pub fn get(self: *const Self, i: usize) !Value {
        if (i >= self.len) {
            return RunTimeError.ArrayOutOfBounds;
        }

        const items = self.getDataPtr();
        return items[i];
    }

    pub fn set(self: *Self, i: usize, value: Value) !void {
        if (i >= self.len) {
            return RunTimeError.ArrayOutOfBounds;
        }

        const items = self.getDataPtr();
        items[i] = value;
    }

    pub fn pop(self: *Self) !Value {
        //note: poping null if len == 0
        if (self.len == 0) {
            return NULL_VALUE;
        }

        self.len -= 1;

        return try self.get(self.len);
    }

    pub fn needsResize(self: *Self) bool {
        return self.len + 1 > self.capacity;
    }

    pub fn calcNewCapacity(self: *Self) usize {
        return self.capacity + self.capacity / 2;
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

pub const String = extern struct {
    const Self = @This();

    obj: HeapValue,
    len: usize,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !*Self {
        const total_bytes = Self.calc_size(source.len);
        const raw_memory = try allocator.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(Self)), total_bytes);
        const string_ptr = @as(*Self, @ptrCast(raw_memory.ptr));

        string_ptr.obj = .{
            .kind = .string,
        };

        string_ptr.len = source.len;

        const data = string_ptr.getDataPtr();
        @memcpy(data[0..source.len], source);

        return string_ptr;
    }

    pub fn slice(self: *const Self) []const u8 {
        return self.getDataPtr()[0..self.len];
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

pub const TypedValue = struct {
    value: Value,
    type: ValueType,

    pub fn from_ir_value(ir_value: *const ir.Value) TypedValue {
        return switch (ir_value.data) {
            .i_int => |i| TypedValue{ .value = .{ .int64 = i }, .type = .int64 },
            .i_float => |f| TypedValue{ .value = .{ .float64 = f }, .type = .float64 },
            .i_bool => |b| TypedValue{ .value = .{ .bool = b }, .type = .bool },
            .i_string => unreachable,
            .i_null, .i_void => TypedValue{ .value = .{ .null = {} }, .type = .null },
            .i_array => TypedValue{ .value = undefined, .type = .array },
            else => unreachable,
        };
    }
};

pub const ValueType = enum(u8) {
    int64,
    float64,
    bool,
    null,

    string,
    //note: maybe this needs to get the inner type?
    array,
    @"struct",

    pub fn isHeapType(self: ValueType) bool {
        return switch (self) {
            .string, .array, .@"struct" => true,
            else => false,
        };
    }

    pub fn from_ir_value(value: *const ir.Value) ValueType {
        return switch (value.data) {
            .i_int => .int64,
            .i_float => .float64,
            .i_bool => .bool,
            .i_string => .string,
            .i_null, .i_void => .null,
            .i_array => .array,
            .binary_op => |bo| switch (bo.kind) {
                .i_add, .i_sub, .i_mult, .i_mod, .trunc_div => .int64,
                .i_cmp_eq, .i_cmp_neq, .i_cmp_lt, .i_cmp_le, .i_cmp_ge, .i_cmp_gt => .bool,
                .f_add, .f_sub, .f_mult, .f_div, .f_mod => .float64,
                .f_cmp_eq, .f_cmp_neq, .f_cmp_lt, .f_cmp_le, .f_cmp_ge, .f_cmp_gt => .bool,
                .b_and, .b_or, .b_cmp_eq, .b_cmp_neq => .bool,
            },
            .unary_op => |uo| switch (uo.kind) {
                .not => .bool,
                .i_neg => .int64,
                .f_neg => .float64,
            },
            else => .null,
        };
    }

    pub fn fromTypeId(type_table: *tt.TypeTable, type_id: tt.TypeId) ValueType {
        const ty = type_table.getTypePtrById(type_id);
        return switch (ty.kind) {
            .int => .int64,
            .float => .float64,
            .boolean => .bool,
            .string => .string,
            .array => .array,
            .@"struct" => .@"struct",
            .@"enum" => .int64,
            else => .null,
        };
    }
};

pub const TypedValueHashContext = struct {
    pub fn hash(_: @This(), tv: TypedValue) u64 {
        var hasher = std.hash.Wyhash.init(0);
        const tag: u8 = @intFromEnum(tv.type);
        hasher.update(std.mem.asBytes(&tag));
        switch (tv.type) {
            .int64 => hasher.update(std.mem.asBytes(&tv.value.int64)),
            .float64 => hasher.update(std.mem.asBytes(&@as(u64, @bitCast(tv.value.float64)))),
            .bool => hasher.update(std.mem.asBytes(&tv.value.bool)),
            .null => {},
            .string => unreachable,
            .array => unreachable,
            .@"struct" => unreachable,
        }
        return hasher.final();
    }

    pub fn eql(_: @This(), a: TypedValue, b: TypedValue) bool {
        if (a.type != b.type) return false;
        return switch (a.type) {
            .int64 => a.value.int64 == b.value.int64,
            .float64 => @as(u64, @bitCast(a.value.float64)) == @as(u64, @bitCast(b.value.float64)),
            .bool => a.value.bool == b.value.bool,
            .null => true,
            .string => unreachable,
            .array => unreachable,
            .@"struct" => unreachable,
        };
    }
};
