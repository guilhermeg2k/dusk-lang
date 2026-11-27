const std = @import("std");

pub const max_idle_per_key = 8;
pub const idle_timeout_ms = 30_000;

pub const ConnectionPool = struct {
    const Self = @This();

    idle: std.StringArrayHashMapUnmanaged(std.ArrayList(i64)) = .empty,

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        for (self.idle.keys(), self.idle.values()) |key, *stream_ids| {
            allocator.free(key);
            stream_ids.deinit(allocator);
        }
        self.idle.deinit(allocator);
    }

    pub fn takeNewest(self: *Self, allocator: std.mem.Allocator, key: []const u8) ?i64 {
        const entry = self.idle.getEntry(key) orelse return null;
        const stream_id = entry.value_ptr.pop();
        if (entry.value_ptr.items.len == 0) {
            const owned_key = entry.key_ptr.*;
            entry.value_ptr.deinit(allocator);
            _ = self.idle.swapRemove(key);
            allocator.free(owned_key);
        }
        return stream_id;
    }

    pub fn add(self: *Self, allocator: std.mem.Allocator, key: []const u8, stream_id: i64) !?i64 {
        if (!self.idle.contains(key)) {
            const owned_key = try allocator.dupe(u8, key);
            errdefer allocator.free(owned_key);
            try self.idle.put(allocator, owned_key, .empty);
        }
        const stream_ids = self.idle.getPtr(key).?;
        const evicted = if (stream_ids.items.len >= max_idle_per_key) stream_ids.orderedRemove(0) else null;
        try stream_ids.append(allocator, stream_id);
        return evicted;
    }

    pub fn remove(self: *Self, allocator: std.mem.Allocator, key: []const u8, stream_id: i64) void {
        const entry = self.idle.getEntry(key) orelse return;
        const index = std.mem.indexOfScalar(i64, entry.value_ptr.items, stream_id) orelse return;
        _ = entry.value_ptr.orderedRemove(index);
        if (entry.value_ptr.items.len == 0) {
            const owned_key = entry.key_ptr.*;
            entry.value_ptr.deinit(allocator);
            _ = self.idle.swapRemove(key);
            allocator.free(owned_key);
        }
    }
};
