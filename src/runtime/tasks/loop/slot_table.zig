const std = @import("std");

pub fn SlotTable(comptime T: type) type {
    return struct {
        const Self = @This();

        entries: std.ArrayList(?*T) = .empty,
        slot_versions: std.ArrayList(u32) = .empty,
        free_indexes: std.ArrayList(u32) = .empty,

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            self.entries.deinit(allocator);
            self.slot_versions.deinit(allocator);
            self.free_indexes.deinit(allocator);
        }

        pub fn reserve(self: *Self, allocator: std.mem.Allocator) !void {
            try self.entries.ensureUnusedCapacity(allocator, 1);
            try self.slot_versions.ensureUnusedCapacity(allocator, 1);
            try self.free_indexes.ensureTotalCapacity(allocator, self.entries.items.len + 1);
        }

        pub fn addAssumeCapacity(self: *Self, item: *T) i64 {
            if (self.free_indexes.pop()) |index| {
                self.entries.items[index] = item;
                return idOf(index, self.slot_versions.items[index]);
            }
            const index: u32 = @intCast(self.entries.items.len);
            self.entries.appendAssumeCapacity(item);
            self.slot_versions.appendAssumeCapacity(1);
            return idOf(index, 1);
        }

        pub fn get(self: *const Self, id: i64) ?*T {
            const index = self.slotIndexOf(id) orelse return null;
            return self.entries.items[index];
        }

        pub fn remove(self: *Self, id: i64) ?*T {
            const index = self.slotIndexOf(id) orelse return null;
            const item = self.entries.items[index] orelse return null;
            self.entries.items[index] = null;
            var slot_version = self.slot_versions.items[index] +% 1;
            if (slot_version == 0 or slot_version > std.math.maxInt(i32)) {
                slot_version = 1;
            }
            self.slot_versions.items[index] = slot_version;
            self.free_indexes.appendAssumeCapacity(@intCast(index));
            return item;
        }

        pub fn liveCount(self: *const Self) usize {
            return self.entries.items.len - self.free_indexes.items.len;
        }

        fn slotIndexOf(self: *const Self, id: i64) ?usize {
            if (id < 0) {
                return null;
            }
            const raw: u64 = @intCast(id);
            const index: usize = @intCast(raw & 0xffff_ffff);
            const slot_version: u32 = @intCast(raw >> 32);
            const is_current = index < self.entries.items.len and self.slot_versions.items[index] == slot_version;
            return if (is_current) index else null;
        }

        fn idOf(index: u32, slot_version: u32) i64 {
            return @intCast((@as(u64, slot_version) << 32) | index);
        }
    };
}
