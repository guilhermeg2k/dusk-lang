const std = @import("std");
const SlotWaiter = @import("event_loop.zig").SlotWaiter;

pub const HostSlots = struct {
    const Self = @This();

    hosts: std.StringArrayHashMapUnmanaged(Host) = .empty,

    const Host = struct {
        open: usize = 0,
        waiters: std.ArrayList(*SlotWaiter) = .empty,
    };

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        for (self.hosts.keys(), self.hosts.values()) |key, *host| {
            for (host.waiters.items) |waiter| {
                waiter.destroy();
            }
            host.waiters.deinit(allocator);
            allocator.free(key);
        }
        self.hosts.deinit(allocator);
    }

    pub fn tryTake(self: *Self, allocator: std.mem.Allocator, key: []const u8, limit: ?usize) !bool {
        const host = try self.hostFor(allocator, key);
        if (limit) |max| {
            if (host.open >= max) {
                return false;
            }
        }
        host.open += 1;
        return true;
    }

    pub fn release(self: *Self, key: []const u8) ?*SlotWaiter {
        const host = self.hosts.getPtr(key) orelse return null;
        if (takeFirst(host)) |waiter| {
            return waiter;
        }
        host.open -|= 1;
        return null;
    }

    pub fn addWaiter(self: *Self, allocator: std.mem.Allocator, key: []const u8, waiter: *SlotWaiter) !void {
        const host = try self.hostFor(allocator, key);
        try host.waiters.append(allocator, waiter);
    }

    pub fn removeWaiter(self: *Self, key: []const u8, waiter: *SlotWaiter) void {
        const host = self.hosts.getPtr(key) orelse return;
        const index = std.mem.indexOfScalar(*SlotWaiter, host.waiters.items, waiter) orelse return;
        _ = host.waiters.orderedRemove(index);
    }

    fn hostFor(self: *Self, allocator: std.mem.Allocator, key: []const u8) !*Host {
        if (self.hosts.getPtr(key)) |host| {
            return host;
        }
        const owned_key = try allocator.dupe(u8, key);
        errdefer allocator.free(owned_key);
        try self.hosts.put(allocator, owned_key, .{});
        return self.hosts.getPtr(key).?;
    }

    fn takeFirst(host: *Host) ?*SlotWaiter {
        if (host.waiters.items.len == 0) {
            return null;
        }
        return host.waiters.orderedRemove(0);
    }
};
