const std = @import("std");
const uv = @import("uv.zig");
const Scheduler = @import("scheduler.zig").Scheduler;
const TaskState = @import("scheduler.zig").TaskState;
const v = @import("../value.zig");
const FsJob = @import("fs_job.zig").FsJob;
const Stream = @import("stream.zig").Stream;
const Listener = @import("tcp.zig").Listener;
const RuntimeError = @import("../runtime_error.zig").RuntimeError;
const RuntimeErrorReporter = @import("../runtime_error.zig").RuntimeErrorReporter;

const handle_alignment = std.mem.Alignment.@"16";

pub const EventLoop = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    loop: ?*uv.Loop = null,
    pending_operations: usize = 0,
    callback_failure: ?anyerror = null,
    streams: std.ArrayList(?*Stream) = .empty,
    listeners: std.ArrayList(?*Listener) = .empty,
    stdin_handle: ?usize = null,

    pub fn deinit(self: *Self) void {
        for (self.streams.items) |entry| {
            const stream = entry orelse continue;
            stream.releaseAtExit();
        }
        for (self.listeners.items) |entry| {
            const listener = entry orelse continue;
            listener.releaseAtExit();
        }
        defer self.streams.deinit(self.allocator);
        defer self.listeners.deinit(self.allocator);
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
        uv.uv_handle_set_data(timer, &request.owner);
        uv.uv_update_time(loop);
        if (uv.uv_timer_start(timer, onTimer, ms, 0) != 0) {
            uv.uv_close(timer, onHandleClosed);
            return error.TimerFailed;
        }
        self.pending_operations += 1;
    }

    pub fn startFsJob(self: *Self, scheduler: *Scheduler, task: ?*TaskState, job: *FsJob) !void {
        const loop = try self.ensureLoopStarted();
        job.event_loop = self;
        job.scheduler = scheduler;
        job.task = task;
        job.loop = loop;
        const work = job.workRequest();
        uv.uv_req_set_data(work, job);
        if (uv.uv_queue_work(loop, work, onFsJobWork, onFsJobDone) != 0) {
            return error.PoolJobFailed;
        }
        self.pending_operations += 1;
    }

    pub fn openStdin(self: *Self, scheduler: *Scheduler, file: c_int) !usize {
        if (self.stdin_handle) |handle| {
            return handle;
        }
        const loop = try self.ensureLoopStarted();
        try self.streams.ensureUnusedCapacity(self.allocator, 1);
        const stream = try Stream.open(self.allocator, self, scheduler, loop, file);
        const handle = self.streams.items.len;
        self.streams.appendAssumeCapacity(stream);
        self.stdin_handle = handle;
        return handle;
    }

    pub fn openTcpStream(self: *Self, scheduler: *Scheduler) !usize {
        const loop = try self.ensureLoopStarted();
        try self.streams.ensureUnusedCapacity(self.allocator, 1);
        const stream = try Stream.openTcp(self.allocator, self, scheduler, loop);
        const handle = self.streams.items.len;
        self.streams.appendAssumeCapacity(stream);
        return handle;
    }

    pub fn openListener(self: *Self, scheduler: *Scheduler) !usize {
        const loop = try self.ensureLoopStarted();
        try self.listeners.ensureUnusedCapacity(self.allocator, 1);
        const listener = try Listener.create(self.allocator, self, scheduler, loop);
        const handle = self.listeners.items.len;
        self.listeners.appendAssumeCapacity(listener);
        return handle;
    }

    pub fn closeListener(self: *Self, handle: i64) void {
        const listener = self.listenerAt(handle) orelse return;
        self.listeners.items[@intCast(handle)] = null;
        listener.close();
    }

    pub fn listenerAt(self: *Self, handle: i64) ?*Listener {
        const is_valid = handle >= 0 and handle < self.listeners.items.len;
        if (!is_valid) {
            return null;
        }
        return self.listeners.items[@intCast(handle)];
    }

    pub fn closeStream(self: *Self, handle: i64) void {
        const stream = self.streamAt(handle) orelse return;
        self.streams.items[@intCast(handle)] = null;
        stream.close();
    }

    pub fn streamAt(self: *Self, handle: i64) ?*Stream {
        const is_valid = handle >= 0 and handle < self.streams.items.len;
        if (!is_valid) {
            return null;
        }
        return self.streams.items[@intCast(handle)];
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

    pub fn wallTimeMilliseconds() RuntimeError!i64 {
        var time: uv.Timeval64 = undefined;
        const status = uv.uv_gettimeofday(&time);
        if (status != 0) {
            return RuntimeErrorReporter.clockFailed("wall", std.mem.span(uv.uv_err_name(status)));
        }
        return wallMillisecondsFromTimeval(time);
    }

    pub fn wallMillisecondsFromTimeval(time: uv.Timeval64) RuntimeError!i64 {
        const milliseconds = @as(i128, time.tv_sec) * 1000 + @divFloor(@as(i128, time.tv_usec), 1000);
        return std.math.cast(i64, milliseconds) orelse RuntimeErrorReporter.clockOutOfRange("wall");
    }

    pub fn monotonicTimeNanoseconds() RuntimeError!i64 {
        return monotonicNanosecondsFromReading(uv.uv_hrtime());
    }

    pub fn monotonicNanosecondsFromReading(nanoseconds: u64) RuntimeError!i64 {
        return std.math.cast(i64, nanoseconds) orelse RuntimeErrorReporter.clockOutOfRange("monotonic");
    }
};

pub const HandleOwner = struct {
    kind: Kind,

    pub const Kind = enum { timer, stream, stream_timer, listener };

    pub fn of(handle: *uv.Handle) *HandleOwner {
        return @ptrCast(@alignCast(uv.uv_handle_get_data(handle).?));
    }
};

fn closeLeftoverHandle(handle: *uv.Handle, _: ?*anyopaque) callconv(.c) void {
    if (uv.uv_is_closing(handle) != 0) return;
    uv.uv_close(handle, onHandleClosed);
}

pub fn onHandleClosed(handle: *uv.Handle) callconv(.c) void {
    const owner = HandleOwner.of(handle);
    switch (owner.kind) {
        .timer => {
            const request: *TimerRequest = @alignCast(@fieldParentPtr("owner", owner));
            const allocator = request.event_loop.allocator;
            allocator.free(request.timer_memory);
            allocator.destroy(request);
        },
        .stream => {
            const stream: *Stream = @alignCast(@fieldParentPtr("owner", owner));
            stream.markHandleClosed();
        },
        .stream_timer => {
            const stream: *Stream = @alignCast(@fieldParentPtr("timer_owner", owner));
            stream.markHandleClosed();
        },
        .listener => {
            const listener: *Listener = @alignCast(@fieldParentPtr("owner", owner));
            listener.markHandleClosed();
        },
    }
}

fn timerRequestOf(handle: *uv.Handle) *TimerRequest {
    return @alignCast(@fieldParentPtr("owner", HandleOwner.of(handle)));
}

const TimerRequest = struct {
    owner: HandleOwner = .{ .kind = .timer },
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
    uv.uv_close(handle, onHandleClosed);
}

fn onFsJobWork(work: *uv.Request) callconv(.c) void {
    fsJobOf(work).runOnPoolThread();
}

fn fsJobOf(work: *uv.Request) *FsJob {
    return @ptrCast(@alignCast(uv.uv_req_get_data(work).?));
}

fn onFsJobDone(work: *uv.Request, _: c_int) callconv(.c) void {
    const job = fsJobOf(work);
    defer job.destroy();
    const event_loop = job.event_loop;
    event_loop.pending_operations -= 1;
    if (job.stream) |stream| {
        stream.finishChunk(job.takeContents(), job.failure != null);
        return;
    }
    const wake_result = job.takeWakeResult();
    job.scheduler.wake(job.task.?, wake_result) catch |failure| {
        wake_result.payload.free(event_loop.allocator);
        event_loop.callback_failure = failure;
    };
}
