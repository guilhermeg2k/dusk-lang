const std = @import("std");
const uv = @import("uv.zig");
const scheduler = @import("scheduler.zig");
const v = @import("value.zig");

const handle_alignment = std.mem.Alignment.@"16";

pub const EventLoop = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    loop: ?*uv.Loop = null,
    pending: usize = 0,
    failure: ?anyerror = null,

    pub fn startTimer(self: *Self, tasks: *scheduler.Scheduler, ms: u64, task: *scheduler.TaskState) !void {
        const loop = try self.ensureLoopStarted();
        const request = try self.allocator.create(TimerRequest);
        const timer_memory = self.allocator.alignedAlloc(u8, handle_alignment, uv.uv_handle_size(.timer)) catch |failure| {
            self.allocator.destroy(request);
            return failure;
        };
        request.* = .{ .event_loop = self, .tasks = tasks, .task = task, .timer_memory = timer_memory };
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
        self.pending += 1;
    }

    pub fn hasPending(self: *const Self) bool {
        return self.pending > 0;
    }

    pub fn waitOnce(self: *Self) !void {
        return self.run(.once);
    }

    pub fn poll(self: *Self) !void {
        return self.run(.nowait);
    }

    fn run(self: *Self, mode: uv.RunMode) !void {
        const loop = self.loop orelse return;
        _ = uv.uv_run(loop, mode);
        const failure = self.failure orelse return;
        self.failure = null;
        return failure;
    }

    pub fn deinit(self: *Self) void {
        const loop = self.loop orelse return;
        uv.uv_walk(loop, closeHandle, null);
        _ = uv.uv_run(loop, .default);
        _ = uv.uv_loop_close(loop);
        self.allocator.free(loopMemory(loop));
        self.loop = null;
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

    fn loopMemory(loop: *uv.Loop) []align(16) u8 {
        const bytes: [*]align(16) u8 = @ptrCast(@alignCast(loop));
        return bytes[0..uv.uv_loop_size()];
    }
};

const TimerRequest = struct {
    event_loop: *EventLoop,
    tasks: *scheduler.Scheduler,
    task: *scheduler.TaskState,
    timer_memory: []align(16) u8,

    fn timer(self: *TimerRequest) *uv.Handle {
        return @ptrCast(self.timer_memory.ptr);
    }
};

fn requestOf(handle: *uv.Handle) *TimerRequest {
    return @ptrCast(@alignCast(uv.uv_handle_get_data(handle).?));
}

fn onTimer(handle: *uv.Handle) callconv(.c) void {
    const request = requestOf(handle);
    const event_loop = request.event_loop;
    event_loop.pending -= 1;
    request.tasks.deliver(request.task, .{ .value = v.NULL_VALUE, .kind = .null }) catch |failure| {
        event_loop.failure = failure;
    };
    uv.uv_close(handle, onClose);
}

fn onClose(handle: *uv.Handle) callconv(.c) void {
    const request = requestOf(handle);
    const allocator = request.event_loop.allocator;
    allocator.free(request.timer_memory);
    allocator.destroy(request);
}

fn closeHandle(handle: *uv.Handle, _: ?*anyopaque) callconv(.c) void {
    if (uv.uv_is_closing(handle) != 0) return;
    uv.uv_close(handle, onClose);
}
