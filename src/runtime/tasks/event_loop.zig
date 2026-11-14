const std = @import("std");
const uv = @import("uv.zig");
const Scheduler = @import("scheduler.zig").Scheduler;
const TaskState = @import("scheduler.zig").TaskState;
const v = @import("../value.zig");

const handle_alignment = std.mem.Alignment.@"16";

pub const EventLoop = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    loop: ?*uv.Loop = null,
    pending_operations: usize = 0,
    callback_failure: ?anyerror = null,

    pub fn deinit(self: *Self) void {
        const loop = self.loop orelse return;
        uv.uv_walk(loop, closeLeftoverHandle, null);
        _ = uv.uv_run(loop, .default);
        _ = uv.uv_loop_close(loop);
        self.allocator.free(loopMemory(loop));
        self.loop = null;
    }

    pub fn poll(self: *Self) !void {
        return self.run(.nowait);
    }

    pub fn waitForEvent(self: *Self) !void {
        return self.run(.once);
    }

    fn run(self: *Self, mode: uv.RunMode) !void {
        const loop = self.loop orelse return;
        _ = uv.uv_run(loop, mode);
        const failure = self.callback_failure orelse return;
        self.callback_failure = null;
        return failure;
    }

    fn loopMemory(loop: *uv.Loop) []align(16) u8 {
        const bytes: [*]align(16) u8 = @ptrCast(@alignCast(loop));
        return bytes[0..uv.uv_loop_size()];
    }

    pub fn startTimer(self: *Self, scheduler: *Scheduler, ms: u64, task: *TaskState) !void {
        const loop = try self.ensureLoopStarted();
        const request = try self.allocator.create(TimerRequest);
        const timer_memory = self.allocator.alignedAlloc(u8, handle_alignment, uv.uv_handle_size(.timer)) catch |failure| {
            self.allocator.destroy(request);
            return failure;
        };
        request.* = .{ .event_loop = self, .scheduler = scheduler, .task = task, .timer_memory = timer_memory };
        const timer = request.timer();
        if (uv.uv_timer_init(loop, timer) != 0) {
            self.allocator.free(timer_memory);
            self.allocator.destroy(request);
            return error.TimerFailed;
        }
        uv.uv_handle_set_data(timer, request);
        uv.uv_update_time(loop);
        if (uv.uv_timer_start(timer, onTimer, ms, 0) != 0) {
            uv.uv_close(timer, onClose);
            return error.TimerFailed;
        }
        self.pending_operations += 1;
    }

    fn ensureLoopStarted(self: *Self) !*uv.Loop {
        if (self.loop) |loop| return loop;
        const memory = try self.allocator.alignedAlloc(u8, handle_alignment, uv.uv_loop_size());
        const loop: *uv.Loop = @ptrCast(memory.ptr);
        if (uv.uv_loop_init(loop) != 0) {
            self.allocator.free(memory);
            return error.EventLoopFailed;
        }
        self.loop = loop;
        return loop;
    }

    pub fn hasPendingOperations(self: *const Self) bool {
        return self.pending_operations > 0;
    }
};

fn closeLeftoverHandle(handle: *uv.Handle, _: ?*anyopaque) callconv(.c) void {
    if (uv.uv_is_closing(handle) != 0) return;
    uv.uv_close(handle, onClose);
}

fn onClose(handle: *uv.Handle) callconv(.c) void {
    const request = timerRequestOf(handle);
    const allocator = request.event_loop.allocator;
    allocator.free(request.timer_memory);
    allocator.destroy(request);
}

fn timerRequestOf(handle: *uv.Handle) *TimerRequest {
    return @ptrCast(@alignCast(uv.uv_handle_get_data(handle).?));
}

const TimerRequest = struct {
    event_loop: *EventLoop,
    scheduler: *Scheduler,
    task: *TaskState,
    timer_memory: []align(16) u8,

    fn timer(self: *TimerRequest) *uv.Handle {
        return @ptrCast(self.timer_memory.ptr);
    }
};

fn onTimer(handle: *uv.Handle) callconv(.c) void {
    const request = timerRequestOf(handle);
    const event_loop = request.event_loop;
    event_loop.pending_operations -= 1;
    request.scheduler.wake(request.task, .{ .value = v.NULL_VALUE, .kind = .null }) catch |failure| {
        event_loop.callback_failure = failure;
    };
    uv.uv_close(handle, onClose);
}
