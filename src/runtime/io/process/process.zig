const std = @import("std");
const uv = @import("../../tasks/loop/uv.zig");
const EventLoop = @import("../../tasks/event_loop.zig").EventLoop;
const HandleOwner = @import("../../tasks/event_loop.zig").HandleOwner;
const onHandleClosed = @import("../../tasks/event_loop.zig").onHandleClosed;
const Scheduler = @import("../../tasks/scheduler.zig").Scheduler;
const TaskState = @import("../../tasks/scheduler.zig").TaskState;
const WakeResult = @import("../../tasks/scheduler.zig").WakeResult;

pub const ProcessError = enum { InvalidArgument, NotFound, PermissionDenied, InvalidExecutable, ResourceLimit, TimedOut, OutputTooLarge, InvalidUtf8, Unsupported, Other, Busy, Closed, NotPiped };

const access_executable: c_int = 1;
const mode_type_mask = 0o170000;
const mode_regular_file = 0o100000;

pub const Launch = struct {
    io: std.Io,
    file: [*:0]const u8,
    args: [*:null]const ?[*:0]const u8,
    env: [*:null]const ?[*:0]const u8,
    cwd: [*:0]const u8,
    modes: [3]c_int,
    is_captured: bool = false,
    timeout_ms: ?u64 = null,
    max_output: usize = 0,
    started: std.Io.Timestamp,
};

pub const Process = struct {
    owner: HandleOwner = .{ .kind = .process },
    timer_owner: HandleOwner = .{ .kind = .process_timer },
    event_loop: *EventLoop,
    scheduler: *Scheduler,
    table_index: i64,
    process_memory: []align(16) u8 = &.{},
    timer_memory: []align(16) u8 = &.{},
    captures: [2]CapturePipe = undefined,
    outputs: [2]std.ArrayList(u8) = .{ .empty, .empty },
    pipe_handles: [3]?i64 = .{ null, null, null },
    open_handles: usize = 0,
    max_output: usize = 0,
    output_size: usize = 0,
    waiter: ?*TaskState = null,
    failure: ?ProcessError = null,
    exit_status: i64 = 0,
    term_signal: i64 = 0,
    is_captured: bool = false,
    is_released: bool = false,
    should_release_on_exit: bool = false,
    is_timer_running: bool = false,
    has_spawned: bool = false,
    has_exited: bool = false,

    pub fn launch(event_loop: *EventLoop, scheduler: *Scheduler, options: Launch) !*Process {
        const loop = try event_loop.ensureLoopStarted();
        const allocator = event_loop.allocator;
        try event_loop.processes.reserve(allocator);
        const self = try allocator.create(Process);
        self.* = .{ .event_loop = event_loop, .scheduler = scheduler, .table_index = 0, .is_captured = options.is_captured, .max_output = options.max_output };
        self.captures = .{ .{ .process = self, .channel = 0 }, .{ .process = self, .channel = 1 } };
        self.table_index = event_loop.processes.addAssumeCapacity(self);
        errdefer self.release();
        var pipes: [3]?*uv.Handle = .{ null, null, null };
        for (options.modes, 0..) |mode, channel| {
            if (mode != 2) continue;
            if (options.is_captured) {
                try self.captures[channel - 1].init(loop);
                pipes[channel] = self.captures[channel - 1].handle();
            } else {
                const index = try event_loop.openProcessPipe(scheduler);
                self.pipe_handles[channel] = index;
                pipes[channel] = event_loop.streamAt(index).?.uvHandle();
            }
        }
        self.process_memory = try allocator.alignedAlloc(u8, .@"16", uv.uv_handle_size(.process));
        const status = uv.dusk_process_spawn(loop, self.handle(), options.file, options.args, options.env, options.cwd, &pipes, &options.modes, onExit);
        self.open_handles += 1;
        uv.uv_handle_set_data(self.handle(), &self.owner);
        if (status < 0) {
            uv.uv_close(self.handle(), onHandleClosed);
            return statusFailure(status);
        }
        self.has_spawned = true;
        event_loop.live_processes += 1;
        if (self.is_captured) {
            for (&self.captures) |*capture| {
                if (uv.uv_read_start(capture.handle(), onCaptureAlloc, onCaptureRead) < 0) {
                    return error.Other;
                }
            }
            if (options.timeout_ms) |ms| {
                const elapsed_ns = options.started.durationTo(std.Io.Clock.awake.now(options.io)).nanoseconds;
                const elapsed: u64 = if (elapsed_ns > 0) @intCast(@divTrunc(elapsed_ns, std.time.ns_per_ms)) else 0;
                try self.startTimer(ms -| elapsed);
            }
        }
        return self;
    }

    pub fn park(self: *Process, task: *TaskState, timeout_ms: ?u64) !void {
        if (self.waiter != null) {
            return error.Busy;
        }
        if (!self.is_captured) {
            if (timeout_ms) |ms| {
                try self.startTimer(ms);
            }
        }
        self.waiter = task;
        self.event_loop.pending_operations += 1;
        task.park();
    }

    fn startTimer(self: *Process, ms: u64) !void {
        if (self.timer_memory.len == 0) {
            const memory = try self.event_loop.allocator.alignedAlloc(u8, .@"16", uv.uv_handle_size(.timer));
            if (uv.uv_timer_init(self.event_loop.loop.?, @ptrCast(memory.ptr)) < 0) {
                self.event_loop.allocator.free(memory);
                return error.Other;
            }
            self.timer_memory = memory;
            self.open_handles += 1;
            uv.uv_handle_set_data(self.timer(), &self.timer_owner);
        }
        uv.uv_update_time(self.event_loop.loop.?);
        if (uv.uv_timer_start(self.timer(), onTimeout, ms, 0) < 0) {
            return error.Other;
        }
        self.is_timer_running = true;
    }

    fn timer(self: *Process) *uv.Handle {
        return @ptrCast(self.timer_memory.ptr);
    }

    pub fn kill(self: *Process) !void {
        const is_finished = !self.has_spawned or self.has_exited;
        if (is_finished) return;
        const status = uv.dusk_process_kill(self.handle());
        const has_failed = status < 0 and !isErrorNamed(status, "ESRCH");
        if (has_failed) {
            return statusFailure(status);
        }
    }

    fn handle(self: *Process) *uv.Handle {
        return @ptrCast(self.process_memory.ptr);
    }

    pub fn release(self: *Process) void {
        self.is_released = true;
        if (self.waiter != null) {
            self.waiter = null;
            self.event_loop.pending_operations -= 1;
        }
        self.kill() catch |failure| {
            self.event_loop.callback_failure = failure;
        };
        for (&self.captures) |*capture| {
            capture.close();
        }
        self.closePipes();
        self.stopTimer();
        if (self.timer_memory.len != 0) {
            if (uv.uv_is_closing(self.timer()) == 0) {
                uv.uv_close(self.timer(), onHandleClosed);
            }
        }
        self.freeIfClosed();
    }

    pub fn releaseOnceExited(self: *Process) void {
        const is_running = self.has_spawned and !self.has_exited;
        if (!is_running) {
            self.release();
            return;
        }
        self.closePipes();
        self.should_release_on_exit = true;
    }

    pub fn closePipes(self: *Process) void {
        for (&self.pipe_handles) |*entry| {
            const stream_handle = entry.* orelse continue;
            self.event_loop.closeStream(stream_handle);
            entry.* = null;
        }
    }

    fn stopTimer(self: *Process) void {
        if (!self.is_timer_running) return;
        _ = uv.uv_timer_stop(self.timer());
        self.is_timer_running = false;
    }

    pub fn markHandleClosed(self: *Process) void {
        self.open_handles -= 1;
        self.freeIfClosed();
    }

    fn freeIfClosed(self: *Process) void {
        const can_free = self.is_released and self.open_handles == 0;
        if (!can_free) return;
        const allocator = self.event_loop.allocator;
        _ = self.event_loop.processes.remove(self.table_index);
        for (&self.outputs) |*output| {
            output.deinit(allocator);
        }
        for (&self.captures) |*capture| {
            allocator.free(capture.memory);
            allocator.free(capture.read_memory);
        }
        allocator.free(self.process_memory);
        allocator.free(self.timer_memory);
        allocator.destroy(self);
    }

    fn fail(self: *Process, failure: ProcessError) void {
        if (self.failure != null) return;
        self.failure = failure;
        for (&self.captures) |*capture| {
            capture.close();
        }
        self.kill() catch |cause| {
            self.event_loop.callback_failure = cause;
        };
        self.completeIfReady();
    }

    fn completeIfReady(self: *Process) void {
        if (!self.has_exited) return;
        if (self.is_released) return;
        if (self.is_captured) {
            const has_output = self.captures[0].has_ended and self.captures[1].has_ended;
            if (self.failure == null and !has_output) return;
        }
        const task = self.waiter orelse return;
        self.waiter = null;
        self.stopTimer();
        self.event_loop.pending_operations -= 1;
        if (self.failure) |failure| {
            self.wake(task, .{ .value = .{ .int64 = @intFromEnum(failure) }, .kind = .int64, .has_failed = true, .error_is_relative = true });
            self.release();
            return;
        }
        if (self.is_captured) {
            self.wake(task, .{ .value = .{ .int64 = self.table_index }, .kind = .int64 });
            return;
        }
        const values = self.event_loop.allocator.dupe(i64, &.{ self.exit_status, self.term_signal }) catch {
            self.event_loop.callback_failure = error.OutOfMemory;
            return;
        };
        self.wake(task, .{ .value = .{ .int64 = 0 }, .kind = .array, .payload = .{ .ints = values } });
    }

    fn wake(self: *Process, task: *TaskState, result: WakeResult) void {
        self.scheduler.wake(task, result) catch |failure| {
            result.payload.free(self.event_loop.allocator);
            self.event_loop.callback_failure = failure;
        };
    }
};

pub fn processErrorOf(failure: anyerror) ProcessError {
    return switch (failure) {
        error.InvalidArgument, error.NotAbsolute => .InvalidArgument,
        error.NotFound, error.FileNotFound, error.NotDir, error.CurrentDirUnlinked => .NotFound,
        error.PermissionDenied, error.AccessDenied => .PermissionDenied,
        error.InvalidExecutable => .InvalidExecutable,
        error.ResourceLimit => .ResourceLimit,
        error.TimedOut => .TimedOut,
        error.InvalidUtf8 => .InvalidUtf8,
        error.Unsupported => .Unsupported,
        error.Busy => .Busy,
        else => .Other,
    };
}

pub const ExecutableLookup = struct {
    event_loop: *EventLoop,
    scheduler: *Scheduler,
    task: *TaskState,
    loop: *uv.Loop,
    arena: *std.heap.ArenaAllocator,
    candidates: []const [:0]const u8,
    launch: Launch,
    work_memory: []align(16) u8,
    fs_memory: []align(16) u8,
    found_index: ?usize = null,
    has_denied_candidate: bool = false,

    pub fn start(event_loop: *EventLoop, scheduler: *Scheduler, task: *TaskState, arena: *std.heap.ArenaAllocator, candidates: []const [:0]const u8, launch: Launch) !void {
        const loop = try event_loop.ensureLoopStarted();
        const allocator = event_loop.allocator;
        const self = try allocator.create(ExecutableLookup);
        errdefer allocator.destroy(self);
        const work_memory = try allocator.alignedAlloc(u8, .@"16", uv.uv_req_size(.work));
        errdefer allocator.free(work_memory);
        const fs_memory = try allocator.alignedAlloc(u8, .@"16", uv.uv_req_size(.fs));
        errdefer allocator.free(fs_memory);
        self.* = .{ .event_loop = event_loop, .scheduler = scheduler, .task = task, .loop = loop, .arena = arena, .candidates = candidates, .launch = launch, .work_memory = work_memory, .fs_memory = fs_memory };
        const work: *uv.Request = @ptrCast(work_memory.ptr);
        uv.uv_req_set_data(work, self);
        if (uv.uv_queue_work(loop, work, onLookupWork, onLookupDone) != 0) {
            return error.Other;
        }
        event_loop.pending_operations += 1;
    }

    fn destroy(self: *ExecutableLookup) void {
        const allocator = self.event_loop.allocator;
        const arena_allocator = self.arena.child_allocator;
        self.arena.deinit();
        arena_allocator.destroy(self.arena);
        allocator.free(self.work_memory);
        allocator.free(self.fs_memory);
        allocator.destroy(self);
    }

    fn findOnPoolThread(self: *ExecutableLookup) void {
        const request: *uv.Request = @ptrCast(self.fs_memory.ptr);
        for (self.candidates, 0..) |candidate, index| {
            const access_status = uv.uv_fs_access(self.loop, request, candidate.ptr, access_executable, null);
            uv.uv_fs_req_cleanup(request);
            if (access_status < 0) {
                const is_denied = isErrorNamed(access_status, "EACCES") or isErrorNamed(access_status, "EPERM");
                self.has_denied_candidate = self.has_denied_candidate or is_denied;
                continue;
            }
            const stat_status = uv.uv_fs_stat(self.loop, request, candidate.ptr, null);
            const is_regular_file = stat_status >= 0 and uv.uv_fs_get_statbuf(request).st_mode & mode_type_mask == mode_regular_file;
            uv.uv_fs_req_cleanup(request);
            if (!is_regular_file) {
                self.has_denied_candidate = true;
                continue;
            }
            self.found_index = index;
            return;
        }
    }

    fn launchFound(self: *ExecutableLookup) void {
        const index = self.found_index orelse {
            self.fail(if (self.has_denied_candidate) .PermissionDenied else .NotFound);
            return;
        };
        var launch = self.launch;
        launch.file = self.candidates[index].ptr;
        const child = Process.launch(self.event_loop, self.scheduler, launch) catch |failure| {
            self.failWith(failure);
            return;
        };
        if (!launch.is_captured) {
            self.wake(.{ .value = .{ .int64 = child.table_index }, .kind = .int64 });
            return;
        }
        child.park(self.task, null) catch |failure| {
            child.release();
            self.failWith(failure);
        };
    }

    fn failWith(self: *ExecutableLookup, failure: anyerror) void {
        if (failure == error.OutOfMemory) {
            self.event_loop.callback_failure = error.OutOfMemory;
            return;
        }
        self.fail(processErrorOf(failure));
    }

    fn fail(self: *ExecutableLookup, failure: ProcessError) void {
        self.wake(.{ .value = .{ .int64 = @intFromEnum(failure) }, .kind = .int64, .has_failed = true, .error_is_relative = true });
    }

    fn wake(self: *ExecutableLookup, result: WakeResult) void {
        self.scheduler.wake(self.task, result) catch |failure| {
            self.event_loop.callback_failure = failure;
        };
    }
};

fn lookupOf(work: *uv.Request) *ExecutableLookup {
    return @ptrCast(@alignCast(uv.uv_req_get_data(work).?));
}

fn onLookupWork(work: *uv.Request) callconv(.c) void {
    lookupOf(work).findOnPoolThread();
}

fn onLookupDone(work: *uv.Request, _: c_int) callconv(.c) void {
    const self = lookupOf(work);
    defer self.destroy();
    self.event_loop.pending_operations -= 1;
    if (self.event_loop.is_shutting_down) return;
    self.launchFound();
}

const CapturePipe = struct {
    owner: HandleOwner = .{ .kind = .process_pipe },
    process: *Process,
    channel: usize,
    memory: []align(16) u8 = &.{},
    read_memory: []u8 = &.{},
    has_ended: bool = false,

    fn init(self: *CapturePipe, loop: *uv.Loop) !void {
        const allocator = self.process.event_loop.allocator;
        self.read_memory = try allocator.alloc(u8, 65536);
        const memory = try allocator.alignedAlloc(u8, .@"16", uv.uv_handle_size(.named_pipe));
        if (uv.uv_pipe_init(loop, @ptrCast(memory.ptr), 0) < 0) {
            allocator.free(memory);
            return error.Other;
        }
        self.memory = memory;
        self.process.open_handles += 1;
        uv.uv_handle_set_data(self.handle(), &self.owner);
    }

    fn handle(self: *CapturePipe) *uv.Handle {
        return @ptrCast(self.memory.ptr);
    }

    fn close(self: *CapturePipe) void {
        self.has_ended = true;
        if (self.memory.len == 0) return;
        if (uv.uv_is_closing(self.handle()) != 0) return;
        _ = uv.uv_read_stop(self.handle());
        uv.uv_close(self.handle(), onHandleClosed);
    }
};

pub fn captureHandleClosed(owner: *HandleOwner) void {
    const pipe: *CapturePipe = @alignCast(@fieldParentPtr("owner", owner));
    pipe.process.markHandleClosed();
}

fn onCaptureAlloc(handle: *uv.Handle, _: usize, buffer: *uv.Buffer) callconv(.c) void {
    const pipe: *CapturePipe = @alignCast(@fieldParentPtr("owner", HandleOwner.of(handle)));
    buffer.* = .{ .base = pipe.read_memory.ptr, .len = @intCast(pipe.read_memory.len) };
}

fn onCaptureRead(handle: *uv.Handle, count: isize, _: *const uv.Buffer) callconv(.c) void {
    const pipe: *CapturePipe = @alignCast(@fieldParentPtr("owner", HandleOwner.of(handle)));
    const process = pipe.process;
    const should_ignore = process.is_released or process.failure != null;
    if (should_ignore) return;
    if (count > 0) {
        const length: usize = @intCast(count);
        if (length > process.max_output - process.output_size) {
            process.fail(.OutputTooLarge);
            return;
        }
        process.outputs[pipe.channel].appendSlice(process.event_loop.allocator, pipe.read_memory[0..length]) catch {
            process.event_loop.callback_failure = error.OutOfMemory;
            process.fail(.Other);
            return;
        };
        process.output_size += length;
    } else if (count < 0) {
        if (!isErrorNamed(@intCast(count), "EOF")) {
            process.fail(.Other);
            return;
        }
        pipe.close();
        process.completeIfReady();
    }
}

fn onExit(handle: *uv.Handle, status: i64, signal: c_int) callconv(.c) void {
    const self: *Process = @alignCast(@fieldParentPtr("owner", HandleOwner.of(handle)));
    self.has_exited = true;
    self.exit_status = status;
    self.term_signal = signal;
    self.event_loop.live_processes -= 1;
    uv.uv_close(handle, onHandleClosed);
    self.completeIfReady();
    const should_release = self.should_release_on_exit and !self.is_released;
    if (should_release) {
        self.release();
    }
}

fn onTimeout(timer: *uv.Handle) callconv(.c) void {
    const self: *Process = @alignCast(@fieldParentPtr("timer_owner", HandleOwner.of(timer)));
    self.is_timer_running = false;
    if (self.is_released) return;
    if (self.is_captured) {
        self.fail(.TimedOut);
        return;
    }
    const task = self.waiter orelse return;
    self.waiter = null;
    self.event_loop.pending_operations -= 1;
    self.wake(task, .{ .value = .{ .int64 = @intFromEnum(ProcessError.TimedOut) }, .kind = .int64, .has_failed = true, .error_is_relative = true });
}

pub fn statusFailure(status: c_int) anyerror {
    const is_missing = isErrorNamed(status, "ENOENT") or isErrorNamed(status, "ENOTDIR");
    if (is_missing) {
        return error.NotFound;
    }
    const is_denied = isErrorNamed(status, "EACCES") or isErrorNamed(status, "EPERM");
    if (is_denied) {
        return error.PermissionDenied;
    }
    if (isErrorNamed(status, "ENOEXEC")) {
        return error.InvalidExecutable;
    }
    if (isErrorNamed(status, "ENOMEM")) {
        return error.OutOfMemory;
    }
    const is_resource_limit = isErrorNamed(status, "EAGAIN") or isErrorNamed(status, "EMFILE") or isErrorNamed(status, "ENFILE");
    if (is_resource_limit) {
        return error.ResourceLimit;
    }
    const is_unsupported = isErrorNamed(status, "ENOSYS") or isErrorNamed(status, "ENOTSUP");
    if (is_unsupported) {
        return error.Unsupported;
    }
    return error.Other;
}

fn isErrorNamed(status: c_int, name: []const u8) bool {
    return std.mem.eql(u8, std.mem.span(uv.uv_err_name(status)), name);
}
