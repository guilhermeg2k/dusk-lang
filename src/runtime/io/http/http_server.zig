const std = @import("std");

pub const ServerState = struct {
    const Self = @This();

    idle_streams: std.AutoArrayHashMapUnmanaged(i64, void) = .empty,

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        self.idle_streams.deinit(allocator);
    }

    pub fn setIdle(self: *Self, allocator: std.mem.Allocator, stream_id: i64, is_idle: bool) !void {
        if (!is_idle) {
            _ = self.idle_streams.swapRemove(stream_id);
            return;
        }
        try self.idle_streams.put(allocator, stream_id, {});
    }
};
