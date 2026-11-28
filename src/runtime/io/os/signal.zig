const std = @import("std");
const builtin = @import("builtin");
const uv = @import("../../tasks/loop/uv.zig");
const EventLoop = @import("../../tasks/event_loop.zig").EventLoop;
const HandleOwner = @import("../../tasks/event_loop.zig").HandleOwner;
const onHandleClosed = @import("../../tasks/event_loop.zig").onHandleClosed;
const Scheduler = @import("../../tasks/scheduler.zig").Scheduler;
const TaskState = @import("../../tasks/scheduler.zig").TaskState;
const v = @import("../../value.zig");
const OsError = @import("../../os.zig").OsError;

pub const Subscription = struct {
    owner: HandleOwner = .{ .kind = .signal },
    event_loop: *EventLoop,
    scheduler: *Scheduler,
    signal_memory: []align(16) u8,
    waiter: ?*TaskState = null,
    has_pending_signal: bool = false,
    is_closing: bool = false,

    pub fn create(event_loop: *EventLoop, scheduler: *Scheduler, signal: i64) !*Subscription {
        const number = try signalNumber(signal);
        const loop = try event_loop.ensureLoopStarted();
        const allocator = event_loop.allocator;
        const self = try allocator.create(Subscription);
        var is_handle_initialized = false;
        errdefer if (!is_handle_initialized) {
            allocator.destroy(self);
        };
        const memory = try allocator.alignedAlloc(u8, .@"16", uv.uv_handle_size(.signal));
        errdefer if (!is_handle_initialized) {
            allocator.free(memory);
        };
        self.* = .{ .event_loop = event_loop, .scheduler = scheduler, .signal_memory = memory };
        if (uv.uv_signal_init(loop, self.handle()) < 0) {
            return error.SignalInitFailed;
        }
        is_handle_initialized = true;
        uv.uv_handle_set_data(self.handle(), &self.owner);
        if (uv.uv_signal_start(self.handle(), onSignal, number) < 0) {
            self.is_closing = true;
            uv.uv_close(self.handle(), onHandleClosed);
            return error.SignalStartFailed;
        }
        uv.uv_unref(self.handle());
        return self;
    }

    pub fn destroy(self: *Subscription) void {
        const allocator = self.event_loop.allocator;
        allocator.free(self.signal_memory);
        allocator.destroy(self);
    }

    pub fn parkWaiter(self: *Subscription, task: *TaskState) error{ Closed, Busy }!bool {
        if (self.is_closing) {
            return error.Closed;
        }
        if (self.waiter != null) {
            return error.Busy;
        }
        if (self.has_pending_signal) {
            self.has_pending_signal = false;
            return false;
        }
        self.waiter = task;
        self.event_loop.pending_operations += 1;
        uv.uv_ref(self.handle());
        task.park();
        return true;
    }

    pub fn close(self: *Subscription) void {
        if (self.is_closing) return;
        self.is_closing = true;
        _ = uv.uv_signal_stop(self.handle());
        if (self.waiter) |task| {
            self.waiter = null;
            self.event_loop.pending_operations -= 1;
            if (!self.event_loop.is_shutting_down) {
                self.scheduler.wake(task, .{ .value = .{ .int64 = @intFromEnum(OsError.Closed) }, .kind = .int64, .has_failed = true, .error_is_relative = true }) catch |failure| {
                    self.event_loop.callback_failure = failure;
                };
            }
        }
        uv.uv_close(self.handle(), onHandleClosed);
    }

    fn handle(self: *Subscription) *uv.Handle {
        return @ptrCast(self.signal_memory.ptr);
    }
};

fn signalNumber(signal: i64) !c_int {
    return switch (signal) {
        0 => if (builtin.os.tag == .windows) 2 else @intCast(@intFromEnum(std.posix.SIG.INT)),
        1 => if (builtin.os.tag == .windows) error.Unsupported else @intCast(@intFromEnum(std.posix.SIG.TERM)),
        2 => if (builtin.os.tag == .windows) 28 else @intCast(@intFromEnum(std.posix.SIG.WINCH)),
        else => error.Unsupported,
    };
}

fn onSignal(handle: *uv.Handle, _: c_int) callconv(.c) void {
    const self: *Subscription = @alignCast(@fieldParentPtr("owner", HandleOwner.of(handle)));
    if (self.is_closing) return;
    const task = self.waiter orelse {
        self.has_pending_signal = true;
        return;
    };
    self.waiter = null;
    self.event_loop.pending_operations -= 1;
    uv.uv_unref(handle);
    self.scheduler.wake(task, .{ .value = v.NULL_VALUE, .kind = .null }) catch |failure| {
        self.event_loop.callback_failure = failure;
    };
}
