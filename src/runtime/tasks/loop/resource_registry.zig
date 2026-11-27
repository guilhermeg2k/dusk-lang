const std = @import("std");
const ResourceId = @import("../../value.zig").ResourceId;

pub const ResourceKind = enum(i64) { stream, listener, compression, certificate };

pub const ResourceRegistry = struct {
    const Self = @This();

    slots: std.ArrayList(Slot) = .empty,
    free_indexes: std.ArrayList(u32) = .empty,

    pub const Slot = struct {
        kind: ResourceKind = .stream,
        handle: i64 = -1,
        slot_version: u32 = 1,
        is_released: bool = false,
        is_used: bool = false,
    };

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        self.slots.deinit(allocator);
        self.free_indexes.deinit(allocator);
    }

    pub fn add(self: *Self, allocator: std.mem.Allocator, kind: ResourceKind, handle: i64) !ResourceId {
        try self.free_indexes.ensureTotalCapacity(allocator, self.slots.items.len + 1);
        const slot_index = self.free_indexes.pop() orelse blk: {
            try self.slots.append(allocator, .{});
            break :blk @as(u32, @intCast(self.slots.items.len - 1));
        };
        const slot = &self.slots.items[slot_index];
        slot.* = .{ .kind = kind, .handle = handle, .slot_version = slot.slot_version, .is_used = true };
        return .{ .registry_slot_index = slot_index, .registry_slot_version = slot.slot_version };
    }

    pub fn release(self: *Self, id: ResourceId) void {
        const slot = self.find(id) orelse return;
        slot.is_released = true;
    }

    pub fn take(self: *Self, id: ResourceId) ?Slot {
        const slot = self.find(id) orelse return null;
        const taken = slot.*;
        slot.is_used = false;
        slot.slot_version +%= 1;
        if (slot.slot_version == 0) {
            slot.slot_version = 1;
        }
        self.free_indexes.appendAssumeCapacity(id.registry_slot_index);
        return taken;
    }

    pub fn liveCount(self: *const Self) usize {
        return self.slots.items.len - self.free_indexes.items.len;
    }

    fn find(self: *Self, id: ResourceId) ?*Slot {
        if (id.registry_slot_index >= self.slots.items.len) {
            return null;
        }
        const slot = &self.slots.items[id.registry_slot_index];
        const is_current = slot.is_used and slot.slot_version == id.registry_slot_version;
        return if (is_current) slot else null;
    }
};
