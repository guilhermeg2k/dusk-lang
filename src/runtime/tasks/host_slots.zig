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

    pub fn release(self: *Self, allocator: std.mem.Allocator, key: []const u8) ?*SlotWaiter {
        const entry = self.hosts.getEntry(key) orelse return null;
        if (takeFirst(entry.value_ptr)) |waiter| {
            return waiter;
        }
        entry.value_ptr.open -|= 1;
        if (entry.value_ptr.open == 0 and entry.value_ptr.waiters.items.len == 0) {
            const owned_key = entry.key_ptr.*;
            entry.value_ptr.waiters.deinit(allocator);
            _ = self.hosts.swapRemove(key);
            allocator.free(owned_key);
        }
        return null;
    }

    pub fn addWaiter(self: *Self, allocator: std.mem.Allocator, key: []const u8, waiter: *SlotWaiter) !void {
        const host = try self.hostFor(allocator, key);
        try host.waiters.append(allocator, waiter);
    }

    pub fn removeWaiter(self: *Self, allocator: std.mem.Allocator, key: []const u8, waiter: *SlotWaiter) void {
        const entry = self.hosts.getEntry(key) orelse return;
        const index = std.mem.indexOfScalar(*SlotWaiter, entry.value_ptr.waiters.items, waiter) orelse return;
        _ = entry.value_ptr.waiters.orderedRemove(index);
        if (entry.value_ptr.open == 0 and entry.value_ptr.waiters.items.len == 0) {
            const owned_key = entry.key_ptr.*;
            entry.value_ptr.waiters.deinit(allocator);
            _ = self.hosts.swapRemove(key);
            allocator.free(owned_key);
        }
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
