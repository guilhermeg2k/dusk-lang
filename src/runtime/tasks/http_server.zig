const std = @import("std");

pub const ServerState = struct {
    const Self = @This();

    is_closing: bool = false,
    idle_streams: std.AutoArrayHashMapUnmanaged(usize, void) = .empty,

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        self.idle_streams.deinit(allocator);
    }

    pub fn setIdle(self: *Self, allocator: std.mem.Allocator, handle: usize, is_idle: bool) !bool {
        if (!is_idle) {
            _ = self.idle_streams.swapRemove(handle);
            return true;
        }
        if (self.is_closing) return false;
        try self.idle_streams.put(allocator, handle, {});
        return true;
    }

    pub fn takeIdleStreams(self: *Self) std.AutoArrayHashMapUnmanaged(usize, void) {
        const streams = self.idle_streams;
        self.idle_streams = .empty;
        return streams;
    }
};
