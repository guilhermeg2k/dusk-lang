const std = @import("std");
const uv = @import("uv.zig");
const v = @import("../value.zig");
const EventLoop = @import("event_loop.zig").EventLoop;
const HandleOwner = @import("event_loop.zig").HandleOwner;
const Scheduler = @import("scheduler.zig").Scheduler;
const TaskState = @import("scheduler.zig").TaskState;
const WakeResult = @import("scheduler.zig").WakeResult;
const FsJob = @import("fs_job.zig").FsJob;
const onHandleClosed = @import("event_loop.zig").onHandleClosed;

const handle_alignment = std.mem.Alignment.@"16";
const read_chunk_size = 64 * 1024;
const high_water_mark = 1024 * 1024;

pub const Stream = struct {
    const Self = @This();

    pub const Kind = enum { tty, pipe, file, tcp, closed_input };

    owner: HandleOwner = .{ .kind = .stream },
    timer_owner: HandleOwner = .{ .kind = .stream_timer },
    allocator: std.mem.Allocator,
    event_loop: *EventLoop,
    scheduler: *Scheduler,
    kind: Kind,
    loop: *uv.Loop,
    file: c_int,
    handle_memory: []align(16) u8 = &.{},
    timer_memory: []align(16) u8 = &.{},
    open_handles: u8 = 0,
    is_timer_running: bool = false,
    buffered: std.ArrayList(u8) = .empty,
    read_offset: usize = 0,
    is_reading: bool = false,
    has_ended: bool = false,
    is_closed: bool = false,
    has_chunk_in_flight: bool = false,
    requests_in_flight: usize = 0,
    failure: ?IoError = null,
    waiter: ?Waiter = null,

    pub const Waiter = struct {
        task: *TaskState,
        mode: ReadMode,
    };

    pub fn open(allocator: std.mem.Allocator, event_loop: *EventLoop, scheduler: *Scheduler, loop: *uv.Loop, file: c_int) !*Self {
        const stream = try allocator.create(Self);
        errdefer allocator.destroy(stream);
        const kind: Kind = switch (uv.uv_guess_handle(file)) {
            .tty => .tty,
            .named_pipe => .pipe,
            .file => .file,
            else => .closed_input,
        };
        stream.* = .{ .allocator = allocator, .event_loop = event_loop, .scheduler = scheduler, .kind = kind, .loop = loop, .file = file };
        switch (kind) {
            .tty, .pipe => stream.initHandle(loop, file),
            .file => {},
            .closed_input => stream.has_ended = true,
            .tcp => unreachable,
        }
        return stream;
    }

    pub fn openTcp(allocator: std.mem.Allocator, event_loop: *EventLoop, scheduler: *Scheduler, loop: *uv.Loop) !*Self {
        const stream = try allocator.create(Self);
        errdefer allocator.destroy(stream);
        const memory = try allocator.alignedAlloc(u8, handle_alignment, uv.uv_handle_size(.tcp));
        errdefer allocator.free(memory);
        if (uv.uv_tcp_init(loop, @ptrCast(memory.ptr)) != 0) {
            return error.TcpInitFailed;
        }
        stream.* = .{ .allocator = allocator, .event_loop = event_loop, .scheduler = scheduler, .kind = .tcp, .loop = loop, .file = -1, .handle_memory = memory, .open_handles = 1 };
        uv.uv_handle_set_data(stream.uvHandle(), &stream.owner);
        return stream;
    }

    fn initHandle(self: *Self, loop: *uv.Loop, file: c_int) void {
        const handle_type: uv.HandleType = if (self.kind == .tty) .tty else .named_pipe;
        const memory = self.allocator.alignedAlloc(u8, handle_alignment, uv.uv_handle_size(handle_type)) catch {
            self.failure = .Other;
            return;
        };
        const handle: *uv.Handle = @ptrCast(memory.ptr);
        const status = if (self.kind == .tty) uv.uv_tty_init(loop, handle, file, 1) else uv.uv_pipe_init(loop, handle, 0);
        if (status != 0) {
            self.allocator.free(memory);
            self.failure = .Other;
            return;
        }
        self.handle_memory = memory;
        self.open_handles += 1;
        uv.uv_handle_set_data(handle, &self.owner);
        const has_failed_to_open = self.kind == .pipe and uv.uv_pipe_open(handle, file) != 0;
        if (has_failed_to_open) {
            self.failure = .Other;
        }
    }

    pub fn destroy(self: *Self) void {
        self.buffered.deinit(self.allocator);
        if (self.handle_memory.len > 0) {
            self.allocator.free(self.handle_memory);
        }
        if (self.timer_memory.len > 0) {
            self.allocator.free(self.timer_memory);
        }
        const owns_file = self.kind == .file and self.file > 2;
        if (owns_file) {
            closeFileNow(self.allocator, self.loop, self.file);
        }
        self.allocator.destroy(self);
    }

    pub fn releaseAtExit(self: *Self) void {
        self.is_closed = true;
        self.releaseIfDone();
    }

    pub fn markHandleClosed(self: *Self) void {
        self.open_handles -= 1;
        self.releaseIfDone();
    }

    pub fn finishRequest(self: *Self) void {
        self.requests_in_flight -= 1;
        self.releaseIfDone();
    }

    fn releaseIfDone(self: *Self) void {
        const is_done = self.is_closed and self.open_handles == 0 and !self.has_chunk_in_flight and self.requests_in_flight == 0;
        if (is_done) {
            self.destroy();
        }
    }

    pub fn close(self: *Self) void {
        if (self.waiter) |waiter| {
            self.waiter = null;
            self.stopTimer();
            self.event_loop.pending_operations -= 1;
            self.scheduler.wake(waiter.task, failureResult(.Closed)) catch |failure| {
                self.event_loop.callback_failure = failure;
            };
        }
        self.is_closed = true;
        if (self.handle_memory.len > 0) {
            self.stopReading();
            uv.uv_close(self.uvHandle(), onHandleClosed);
        }
        if (self.timer_memory.len > 0) {
            self.stopTimer();
            uv.uv_close(self.timerHandle(), onHandleClosed);
        }
        self.releaseIfDone();
    }

    pub fn bufferedBytes(self: *const Self) []const u8 {
        return self.buffered.items[self.read_offset..];
    }

    pub fn consume(self: *Self, count: usize) void {
        self.read_offset += count;
        const is_mostly_consumed = self.read_offset * 2 >= self.buffered.items.len;
        if (is_mostly_consumed) {
            self.compactBuffer();
        }
    }

    fn compactBuffer(self: *Self) void {
        const remaining = self.buffered.items.len - self.read_offset;
        std.mem.copyForwards(u8, self.buffered.items[0..remaining], self.buffered.items[self.read_offset..]);
        self.buffered.shrinkRetainingCapacity(remaining);
        self.read_offset = 0;
    }

    pub fn readyOutcome(self: *const Self, mode: ReadMode) ?ReadOutcome {
        return switch (mode) {
            .bytes => |max| self.bytesOutcome(max),
            .line => |max| self.lineOutcome(max),
            .all_text => self.allOutcome(true),
            .all_bytes => self.allOutcome(false),
        };
    }

    fn bytesOutcome(self: *const Self, max: usize) ?ReadOutcome {
        const available = self.bufferedBytes();
        if (available.len > 0) {
            const count = @min(available.len, max);
            return .{ .result = .{ .data = available[0..count] }, .consumed = count };
        }
        return self.endOutcome();
    }

    fn lineOutcome(self: *const Self, max: ?usize) ?ReadOutcome {
        const available = self.bufferedBytes();
        if (std.mem.indexOfScalar(u8, available, '\n')) |newline| {
            return lineFrom(available[0..newline], newline + 1, max);
        }
        if (max) |limit| {
            if (available.len > limit) {
                return .{ .result = .{ .failure = .LineTooLong } };
            }
        }
        const is_last_line = available.len > 0 and self.has_ended;
        if (is_last_line) {
            return lineFrom(available, available.len, max);
        }
        return self.endOutcome();
    }

    fn allOutcome(self: *const Self, needs_utf8: bool) ?ReadOutcome {
        if (self.failure) |failure| {
            return .{ .result = .{ .failure = failure } };
        }
        if (!self.has_ended) return null;
        const available = self.bufferedBytes();
        if (needs_utf8 and !std.unicode.utf8ValidateSlice(available)) {
            return .{ .result = .{ .failure = .InvalidUtf8 }, .consumed = available.len };
        }
        return .{ .result = .{ .data = available }, .consumed = available.len };
    }

    fn endOutcome(self: *const Self) ?ReadOutcome {
        if (self.failure) |failure| {
            return .{ .result = .{ .failure = failure } };
        }
        if (self.has_ended) {
            return .{ .result = .end };
        }
        return null;
    }

    pub fn parkReader(self: *Self, task: *TaskState, mode: ReadMode, timeout_ms: ?u64) !void {
        try self.startReading();
        if (timeout_ms) |ms| {
            try self.startTimer(ms);
        }
        self.waiter = .{ .task = task, .mode = mode };
        self.event_loop.pending_operations += 1;
    }

    fn startTimer(self: *Self, ms: u64) !void {
        if (self.timer_memory.len == 0) {
            const memory = try self.allocator.alignedAlloc(u8, handle_alignment, uv.uv_handle_size(.timer));
            if (uv.uv_timer_init(self.loop, @ptrCast(memory.ptr)) != 0) {
                self.allocator.free(memory);
                return error.TimerFailed;
            }
            self.timer_memory = memory;
            self.open_handles += 1;
            uv.uv_handle_set_data(self.timerHandle(), &self.timer_owner);
        }
        uv.uv_update_time(self.loop);
        if (uv.uv_timer_start(self.timerHandle(), onReadTimeout, ms, 0) != 0) {
            return error.TimerFailed;
        }
        self.is_timer_running = true;
    }

    fn stopTimer(self: *Self) void {
        if (!self.is_timer_running) return;
        _ = uv.uv_timer_stop(self.timerHandle());
        self.is_timer_running = false;
    }

    fn timerHandle(self: *Self) *uv.Handle {
        return @ptrCast(self.timer_memory.ptr);
    }

    fn startReading(self: *Self) !void {
        switch (self.kind) {
            .file => try self.startChunk(),
            .closed_input => {},
            .tty, .pipe, .tcp => {
                if (self.is_reading) return;
                if (uv.uv_read_start(self.uvHandle(), onAlloc, onRead) != 0) {
                    return error.StreamReadFailed;
                }
                self.is_reading = true;
            },
        }
    }

    fn startChunk(self: *Self) !void {
        if (self.has_chunk_in_flight) return;
        const job = try FsJob.create(self.allocator, .read_chunk, "", "");
        errdefer job.destroy();
        job.file = self.file;
        job.stream = self;
        try self.event_loop.startFsJob(self.scheduler, null, job);
        self.has_chunk_in_flight = true;
    }

    pub fn finishChunk(self: *Self, contents: ?[]u8, has_failed: bool) void {
        self.has_chunk_in_flight = false;
        const allocator = self.allocator;
        defer {
            if (contents) |data| {
                allocator.free(data);
            }
        }
        if (self.is_closed) {
            self.destroy();
            return;
        }
        if (has_failed) {
            self.failure = .Other;
        } else if (contents) |data| {
            if (data.len == 0) {
                self.has_ended = true;
            } else {
                self.buffered.appendSlice(self.allocator, data) catch {
                    self.failure = .Other;
                };
            }
        }
        self.serveWaiter();
        const needs_more = self.waiter != null and !self.has_ended and self.failure == null;
        if (needs_more) {
            self.startChunk() catch {
                self.failure = .Other;
                self.serveWaiter();
            };
        }
    }

    fn stopReading(self: *Self) void {
        if (!self.is_reading) return;
        _ = uv.uv_read_stop(self.uvHandle());
        self.is_reading = false;
    }

    pub fn uvHandle(self: *Self) *uv.Handle {
        return @ptrCast(self.handle_memory.ptr);
    }

    fn serveWaiter(self: *Self) void {
        const waiter = self.waiter orelse return;
        const outcome = self.readyOutcome(waiter.mode) orelse return;
        const wake_result: WakeResult = switch (outcome.result) {
            .data => |data| result: {
                const bytes = self.allocator.dupe(u8, data) catch break :result failureResult(.Other);
                break :result .{ .value = v.NULL_VALUE, .kind = waiter.mode.valueKind(), .payload = .{ .bytes = bytes } };
            },
            .end => .{ .value = v.NULL_VALUE, .kind = .null },
            .failure => |failure| failureResult(failure),
        };
        self.consume(outcome.consumed);
        self.waiter = null;
        self.stopTimer();
        self.event_loop.pending_operations -= 1;
        self.scheduler.wake(waiter.task, wake_result) catch |failure| {
            wake_result.payload.free(self.allocator);
            self.event_loop.callback_failure = failure;
        };
    }

    fn applyBackpressure(self: *Self) void {
        const is_full = self.waiter == null and self.bufferedBytes().len >= high_water_mark;
        if (is_full or self.has_ended or self.failure != null) {
            self.stopReading();
        }
    }
};

pub const ReadMode = union(enum) {
    bytes: usize,
    line: ?usize,
    all_text,
    all_bytes,

    pub fn valueKind(self: ReadMode) v.ValueType {
        return switch (self) {
            .line, .all_text => .string,
            .bytes, .all_bytes => .buffer,
        };
    }
};

pub const ReadOutcome = struct {
    result: union(enum) {
        data: []const u8,
        end,
        failure: IoError,
    },
    consumed: usize = 0,
};

fn lineFrom(raw: []const u8, consumed: usize, max: ?usize) ReadOutcome {
    const has_carriage_return = raw.len > 0 and raw[raw.len - 1] == '\r';
    const line = if (has_carriage_return) raw[0 .. raw.len - 1] else raw;
    if (max) |limit| {
        if (line.len > limit) {
            return .{ .result = .{ .failure = .LineTooLong } };
        }
    }
    if (!std.unicode.utf8ValidateSlice(line)) {
        return .{ .result = .{ .failure = .InvalidUtf8 }, .consumed = consumed };
    }
    return .{ .result = .{ .data = line }, .consumed = consumed };
}

pub const IoError = enum {
    Closed,
    Busy,
    BrokenPipe,
    ConnectionReset,
    LineTooLong,
    InvalidUtf8,
    TimedOut,
    Other,
};

fn closeFileNow(allocator: std.mem.Allocator, loop: *uv.Loop, file: c_int) void {
    const request_memory = allocator.alignedAlloc(u8, handle_alignment, uv.uv_req_size(.fs)) catch return;
    defer allocator.free(request_memory);
    const request: *uv.Request = @ptrCast(request_memory.ptr);
    _ = uv.uv_fs_close(loop, request, file, null);
    uv.uv_fs_req_cleanup(request);
}

fn failureResult(failure: IoError) WakeResult {
    return .{ .value = .{ .int64 = @intFromEnum(failure) }, .kind = .int64, .has_failed = true, .error_is_relative = true };
}

fn streamOf(handle: *uv.Handle) *Stream {
    return @alignCast(@fieldParentPtr("owner", HandleOwner.of(handle)));
}

fn onReadTimeout(timer: *uv.Handle) callconv(.c) void {
    const stream: *Stream = @alignCast(@fieldParentPtr("timer_owner", HandleOwner.of(timer)));
    stream.is_timer_running = false;
    const waiter = stream.waiter orelse return;
    stream.waiter = null;
    stream.event_loop.pending_operations -= 1;
    stream.scheduler.wake(waiter.task, failureResult(.TimedOut)) catch |failure| {
        stream.event_loop.callback_failure = failure;
    };
    stream.applyBackpressure();
}

fn onAlloc(handle: *uv.Handle, _: usize, buffer: *uv.Buffer) callconv(.c) void {
    const stream = streamOf(handle);
    stream.buffered.ensureUnusedCapacity(stream.allocator, read_chunk_size) catch {
        buffer.* = .{ .base = undefined, .len = 0 };
        return;
    };
    const free_space = stream.buffered.unusedCapacitySlice();
    buffer.* = .{ .base = free_space.ptr, .len = @intCast(free_space.len) };
}

fn onRead(handle: *uv.Handle, read_count: isize, _: *const uv.Buffer) callconv(.c) void {
    const stream = streamOf(handle);
    if (read_count > 0) {
        stream.buffered.items.len += @intCast(read_count);
    } else if (read_count < 0) {
        const name = std.mem.span(uv.uv_err_name(@intCast(read_count)));
        if (std.mem.eql(u8, name, "EOF")) {
            stream.has_ended = true;
        } else {
            stream.failure = if (std.mem.eql(u8, name, "ECONNRESET")) .ConnectionReset else .Other;
        }
    }
    stream.serveWaiter();
    stream.applyBackpressure();
}
