const std = @import("std");

pub fn HandleTable(comptime T: type) type {
    return struct {
        const Self = @This();

        entries: std.ArrayList(?*T) = .empty,
        generations: std.ArrayList(u32) = .empty,
        free_indexes: std.ArrayList(u32) = .empty,

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            self.entries.deinit(allocator);
            self.generations.deinit(allocator);
            self.free_indexes.deinit(allocator);
        }

        pub fn reserve(self: *Self, allocator: std.mem.Allocator) !void {
            try self.entries.ensureUnusedCapacity(allocator, 1);
            try self.generations.ensureUnusedCapacity(allocator, 1);
            try self.free_indexes.ensureTotalCapacity(allocator, self.entries.items.len + 1);
        }

        pub fn addAssumeCapacity(self: *Self, item: *T) usize {
            if (self.free_indexes.pop()) |index| {
                self.entries.items[index] = item;
                return handleOf(index, self.generations.items[index]);
            }
            const index: u32 = @intCast(self.entries.items.len);
            self.entries.appendAssumeCapacity(item);
            self.generations.appendAssumeCapacity(1);
            return handleOf(index, 1);
        }

        pub fn get(self: *const Self, handle: i64) ?*T {
            const index = self.indexOf(handle) orelse return null;
            return self.entries.items[index];
        }

        pub fn remove(self: *Self, handle: i64) ?*T {
            const index = self.indexOf(handle) orelse return null;
            const item = self.entries.items[index] orelse return null;
            self.entries.items[index] = null;
            var generation = self.generations.items[index] +% 1;
            if (generation == 0 or generation > std.math.maxInt(i32)) {
                generation = 1;
            }
            self.generations.items[index] = generation;
            self.free_indexes.appendAssumeCapacity(@intCast(index));
            return item;
        }

        pub fn liveCount(self: *const Self) usize {
            return self.entries.items.len - self.free_indexes.items.len;
        }

        fn indexOf(self: *const Self, handle: i64) ?usize {
            if (handle < 0) {
                return null;
            }
            const raw: u64 = @intCast(handle);
            const index: usize = @intCast(raw & 0xffff_ffff);
            const generation: u32 = @intCast(raw >> 32);
            const is_current = index < self.entries.items.len and self.generations.items[index] == generation;
            return if (is_current) index else null;
        }

        fn handleOf(index: u32, generation: u32) usize {
            return (@as(usize, generation) << 32) | index;
        }
    };
}
