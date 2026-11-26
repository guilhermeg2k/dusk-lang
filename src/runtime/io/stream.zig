const std = @import("std");
const uv = @import("../tasks/loop/uv.zig");
const v = @import("../value.zig");
const EventLoop = @import("../tasks/event_loop.zig").EventLoop;
const HandleOwner = @import("../tasks/event_loop.zig").HandleOwner;
const Scheduler = @import("../tasks/scheduler.zig").Scheduler;
const TaskState = @import("../tasks/scheduler.zig").TaskState;
const WakeResult = @import("../tasks/scheduler.zig").WakeResult;
const onHandleClosed = @import("../tasks/event_loop.zig").onHandleClosed;
const tls = @import("net/tls.zig");
const TlsLayer = @import("net/tls_layer.zig").TlsLayer;
const ServerCertificate = @import("net/tls_layer.zig").ServerCertificate;
const cipher_chunk_size = @import("net/tls_layer.zig").cipher_chunk_size;
const HttpReader = @import("http/http_reader.zig").HttpReader;
const MessageKind = @import("http/http_reader.zig").MessageKind;
const stream_read = @import("stream_read.zig");
const stream_tls = @import("stream_tls.zig");
const stream_file = @import("stream_file.zig");
const stream_http = @import("stream_http.zig");

pub const IoError = stream_read.IoError;
pub const ReadMode = stream_read.ReadMode;
pub const BodyRead = stream_read.BodyRead;
pub const ReadOutcome = stream_read.ReadOutcome;
pub const HandshakeWait = stream_tls.HandshakeWait;

pub const handle_alignment = std.mem.Alignment.@"16";
pub const max_uv_chunk: usize = 1 << 30;

const read_chunk_size = 64 * 1024;
const high_water_mark = 1024 * 1024;

pub const Stream = struct {
    const Self = @This();

    pub const Kind = enum { tty, pipe, file, tcp, closed_input };
    pub const OutputWaiter = stream_file.OutputWaiter;

    pub const Waiter = struct {
        task: *TaskState,
        mode: ReadMode,
    };

    owner: HandleOwner = .{ .kind = .stream },
    timer_owner: HandleOwner = .{ .kind = .stream_timer },
    handshake_timer_owner: HandleOwner = .{ .kind = .stream_handshake_timer },
    write_timer_owner: HandleOwner = .{ .kind = .stream_write_timer },
    allocator: std.mem.Allocator,
    event_loop: *EventLoop,
    scheduler: *Scheduler,
    kind: Kind,
    loop: *uv.Loop,
    file: c_int,
    handle_memory: []align(16) u8 = &.{},
    timer_memory: []align(16) u8 = &.{},
    handshake_timer_memory: []align(16) u8 = &.{},
    write_timer_memory: []align(16) u8 = &.{},
    open_handles: u8 = 0,
    is_timer_running: bool = false,
    is_handshake_timer_running: bool = false,
    is_write_timer_running: bool = false,
    write_timeout_ms: ?u64 = null,
    queued_writes: usize = 0,
    has_write_timed_out: bool = false,
    buffered: std.ArrayList(u8) = .empty,
    read_offset: usize = 0,
    is_reading: bool = false,
    has_ended: bool = false,
    is_closed: bool = false,
    has_chunk_in_flight: bool = false,
    requests_in_flight: usize = 0,
    peer: ?uv.SocketAddress = null,
    tls_layer: ?*TlsLayer = null,
    tunnel_layer: ?*TlsLayer = null,
    is_starting_tls: bool = false,
    is_reuse_forbidden: bool = false,
    host_key: ?[]u8 = null,
    http_reader: ?*HttpReader = null,
    table_index: i64 = 0,
    owns_file: bool = false,
    can_write_file: bool = false,
    is_append: bool = false,
    file_offset: i64 = 0,
    pending_output: std.ArrayList(u8) = .empty,
    has_output_in_flight: bool = false,
    output_waiters: std.ArrayList(OutputWaiter) = .empty,
    output_failure: ?IoError = null,
    pool_key: ?[]u8 = null,
    failure: ?IoError = null,
    waiter: ?Waiter = null,

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
        _ = uv.uv_tcp_nodelay(@ptrCast(memory.ptr), 1);
        stream.* = .{ .allocator = allocator, .event_loop = event_loop, .scheduler = scheduler, .kind = .tcp, .loop = loop, .file = -1, .handle_memory = memory, .open_handles = 1 };
        uv.uv_handle_set_data(stream.uvHandle(), &stream.owner);
        return stream;
    }

    pub fn destroy(self: *Self) void {
        if (self.tls_layer) |layer| {
            layer.destroy();
        }
        if (self.tunnel_layer) |tunnel| {
            tunnel.destroy();
        }
        if (self.host_key) |key| {
            self.allocator.free(key);
        }
        if (self.http_reader) |reader| {
            reader.destroy();
        }
        if (self.pool_key) |key| {
            self.allocator.free(key);
        }
        self.buffered.deinit(self.allocator);
        self.pending_output.deinit(self.allocator);
        self.output_waiters.deinit(self.allocator);
        if (self.handle_memory.len > 0) {
            self.allocator.free(self.handle_memory);
        }
        if (self.timer_memory.len > 0) {
            self.allocator.free(self.timer_memory);
        }
        if (self.handshake_timer_memory.len > 0) {
            self.allocator.free(self.handshake_timer_memory);
        }
        if (self.write_timer_memory.len > 0) {
            self.allocator.free(self.write_timer_memory);
        }
        const is_plain_file = self.kind == .file or self.kind == .closed_input;
        const should_close_file = is_plain_file and (self.owns_file or self.file > 2);
        if (should_close_file) {
            stream_file.closeFileNow(self.allocator, self.loop, self.file);
        }
        self.allocator.destroy(self);
    }

    pub fn rememberPeer(self: *Self) void {
        var address: uv.SocketAddress = .{};
        var length: c_int = @sizeOf(uv.SocketAddress);
        if (uv.uv_tcp_getpeername(self.uvHandle(), &address, &length) != 0) return;
        self.peer = address;
    }

    pub fn releaseAtExit(self: *Self) void {
        self.is_closed = true;
        stream_file.writeOutputNow(self);
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

    pub fn releaseIfDone(self: *Self) void {
        const is_done = self.is_closed and self.open_handles == 0 and !self.has_chunk_in_flight and !self.has_output_in_flight and self.requests_in_flight == 0;
        if (is_done) {
            self.destroy();
        }
    }

    pub fn close(self: *Self) void {
        if (self.waiter) |waiter| {
            self.waiter = null;
            self.stopTimer();
            self.event_loop.pending_operations -= 1;
            self.scheduler.wake(waiter.task, waiter.mode.wakeFailure(.Closed)) catch |failure| {
                self.event_loop.callback_failure = failure;
            };
        }
        stream_tls.close(self);
        stream_file.failOutputWaiters(self, .Closed);
        self.is_closed = true;
        self.releaseHostSlot();
        if (self.handle_memory.len > 0) {
            self.stopReading();
            uv.uv_close(self.uvHandle(), onHandleClosed);
        }
        if (self.timer_memory.len > 0) {
            self.stopTimer();
            uv.uv_close(self.timerHandle(), onHandleClosed);
        }
        if (self.handshake_timer_memory.len > 0) {
            stream_tls.stopHandshakeTimer(self);
            uv.uv_close(stream_tls.handshakeTimerHandle(self), onHandleClosed);
        }
        if (self.write_timer_memory.len > 0) {
            self.stopWriteTimer();
            uv.uv_close(self.writeTimerHandle(), onHandleClosed);
        }
        self.releaseIfDone();
    }

    pub fn holdHostSlot(self: *Self, key: []const u8) !void {
        self.host_key = try self.allocator.dupe(u8, key);
    }

    pub fn releaseHostSlot(self: *Self) void {
        const key = self.host_key orelse return;
        self.host_key = null;
        defer self.allocator.free(key);
        self.event_loop.releaseHostSlot(key);
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

    pub fn readyOutcome(self: *Self, mode: ReadMode) ?ReadOutcome {
        return switch (mode) {
            .bytes => |max| self.readSource().bytesOutcome(max),
            .exact => |count| self.readSource().exactOutcome(count),
            .line => |max| self.readSource().lineOutcome(max),
            .all_text => self.readSource().allOutcome(true),
            .all_bytes => self.readSource().allOutcome(false),
            .any_data => self.readSource().anyDataOutcome(),
            .http_head => stream_http.headOutcome(self),
            .http_body => |read| stream_http.bodyOutcome(self, read),
            .http_whole_body => |cap| stream_http.wholeBodyOutcome(self, cap),
        };
    }

    pub fn parkReader(self: *Self, task: *TaskState, mode: ReadMode, timeout_ms: ?u64) !void {
        try self.startReading();
        if (self.kind != .file) {
            if (timeout_ms) |ms| {
                try self.startTimer(ms);
            }
        }
        self.waiter = .{ .task = task, .mode = mode };
        self.event_loop.pending_operations += 1;
    }

    pub fn startTimerHandle(self: *Self, memory: *[]align(16) u8, owner: *HandleOwner, callback: uv.TimerCallback, ms: u64) !void {
        if (memory.len == 0) {
            const timer_memory = try self.allocator.alignedAlloc(u8, handle_alignment, uv.uv_handle_size(.timer));
            if (uv.uv_timer_init(self.loop, @ptrCast(timer_memory.ptr)) != 0) {
                self.allocator.free(timer_memory);
                return error.TimerFailed;
            }
            memory.* = timer_memory;
            self.open_handles += 1;
            uv.uv_handle_set_data(@ptrCast(timer_memory.ptr), owner);
        }
        uv.uv_update_time(self.loop);
        if (uv.uv_timer_start(@ptrCast(memory.ptr), callback, ms, 0) != 0) {
            return error.TimerFailed;
        }
    }

    pub fn stopTimer(self: *Self) void {
        if (!self.is_timer_running) return;
        _ = uv.uv_timer_stop(self.timerHandle());
        self.is_timer_running = false;
    }

    pub fn startReading(self: *Self) !void {
        switch (self.kind) {
            .file => {
                if (self.can_write_file) {
                    return error.StreamReadFailed;
                }
                try stream_file.startChunk(self);
            },
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

    pub fn stopReading(self: *Self) void {
        if (!self.is_reading) return;
        _ = uv.uv_read_stop(self.uvHandle());
        self.is_reading = false;
    }

    pub fn uvHandle(self: *Self) *uv.Handle {
        return @ptrCast(self.handle_memory.ptr);
    }

    pub fn serveWaiter(self: *Self) void {
        const waiter = self.waiter orelse return;
        const outcome = self.readyOutcome(waiter.mode) orelse return;
        const wake_result: WakeResult = switch (outcome.result) {
            .data => |data| result: {
                const bytes = self.allocator.dupe(u8, data) catch break :result stream_read.failureResult(.Other);
                break :result .{ .value = v.NULL_VALUE, .kind = waiter.mode.valueKind(), .payload = .{ .bytes = bytes } };
            },
            .end => .{ .value = v.NULL_VALUE, .kind = .null },
            .failure => |failure| waiter.mode.wakeFailure(failure),
            .http_failure => |failure| stream_read.httpFailureResult(failure),
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

    pub fn applyBackpressure(self: *Self) void {
        const is_full = self.waiter == null and self.bufferedBytes().len >= high_water_mark;
        if (is_full or self.has_ended or self.failure != null) {
            self.stopReading();
        }
    }

    pub fn write(self: *Self, task: *TaskState, data: []const u8) !WriteOutcome {
        if (data.len == 0) return .done;
        if (self.kind == .file) {
            return stream_file.write(self, task, data);
        }
        const layer = self.tls_layer orelse return self.sendBytes(task, data);
        return stream_tls.write(self, layer, task, data);
    }

    pub fn sendBytes(self: *Self, task: ?*TaskState, data: []const u8) !WriteOutcome {
        if (data.len == 0) return .done;
        const try_len = @min(data.len, max_uv_chunk);
        const buffers = [_]uv.Buffer{.{ .base = @constCast(data.ptr), .len = @intCast(try_len) }};
        const status = uv.uv_try_write(self.uvHandle(), &buffers, 1);
        const written: usize = if (status >= 0) @intCast(status) else 0;
        if (written == data.len) return .done;
        const is_waiting_for_room = status >= 0 or stream_read.isErrorNamed(status, "EAGAIN");
        if (!is_waiting_for_room) {
            return .{ .failure = stream_read.ioErrorFromStatus(status) };
        }
        const request = try WriteRequest.create(self, task, data[written..]);
        const write_status = uv.uv_write(request.uvRequest(), self.uvHandle(), &request.buffers, 1, onWrite);
        if (write_status != 0) {
            request.destroy();
            return .{ .failure = stream_read.ioErrorFromStatus(write_status) };
        }
        self.requests_in_flight += 1;
        self.queued_writes += 1;
        if (task != null) {
            self.event_loop.pending_operations += 1;
        }
        if (!self.is_write_timer_running) {
            self.startWriteTimer() catch {};
        }
        return .parked;
    }

    pub fn setWriteTimeout(self: *Self, timeout_ms: ?u64) void {
        self.write_timeout_ms = timeout_ms;
    }

    pub fn wakeTask(self: *Self, task: *TaskState, wake_result: WakeResult) void {
        self.scheduler.wake(task, wake_result) catch |failure| {
            self.event_loop.callback_failure = failure;
        };
    }

    pub fn finishChunk(self: *Self, contents: ?[]u8, has_failed: bool) void {
        stream_file.finishChunk(self, contents, has_failed);
    }

    pub fn flush(self: *Self, task: *TaskState) !WriteOutcome {
        return stream_file.flush(self, task);
    }

    pub fn filePosition(self: *const Self) i64 {
        return stream_file.filePosition(self);
    }

    pub fn seekFile(self: *Self, target: i64) ?IoError {
        return stream_file.seekFile(self, target);
    }

    pub fn flushOutputBeforeClose(self: *Self) void {
        stream_file.flushOutputBeforeClose(self);
    }

    pub fn finishOutput(self: *Self, written_bytes: usize, has_failed: bool) void {
        stream_file.finishOutput(self, written_bytes, has_failed);
    }

    pub fn startHttpMessage(self: *Self, kind: MessageKind) !void {
        return stream_http.startHttpMessage(self, kind);
    }

    pub fn isReusable(self: *const Self) bool {
        return stream_http.isReusable(self);
    }

    pub fn enterPool(self: *Self, key: []const u8) !void {
        return stream_http.enterPool(self, key);
    }

    pub fn leavePool(self: *Self) void {
        stream_http.leavePool(self);
    }

    pub fn isTls(self: *const Self) bool {
        return self.tls_layer != null;
    }

    pub fn canMoveTlsIntoTunnel(self: *const Self) bool {
        const layer = self.tls_layer orelse return false;
        return layer.handshake == .done and self.tunnel_layer == null;
    }

    pub fn beginWaitingForTrustStore(self: *Self) void {
        self.is_starting_tls = true;
        self.stopReading();
    }

    pub fn startClientTls(self: *Self, task: *TaskState, config: *tls.Config, owns_config: bool, host: []const u8, timeout_ms: ?u64, is_tunneled: bool) !HandshakeWait {
        return stream_tls.startClientTls(self, task, config, owns_config, host, timeout_ms, is_tunneled);
    }

    pub fn startServerTls(self: *Self, task: *TaskState, certificate: *ServerCertificate, timeout_ms: ?u64) !HandshakeWait {
        return stream_tls.startServerTls(self, task, certificate, timeout_ms);
    }

    pub fn awaitHandshake(self: *Self, task: *TaskState, timeout_ms: ?u64) !HandshakeWait {
        return stream_tls.awaitHandshake(self, task, timeout_ms);
    }

    pub fn useServerTls(self: *Self, certificate: *ServerCertificate) !void {
        return stream_tls.useServerTls(self, certificate);
    }

    pub fn ensureHandshakeStarted(self: *Self) void {
        stream_tls.ensureHandshakeStarted(self);
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

    fn compactBuffer(self: *Self) void {
        const remaining = self.buffered.items.len - self.read_offset;
        std.mem.copyForwards(u8, self.buffered.items[0..remaining], self.buffered.items[self.read_offset..]);
        self.buffered.shrinkRetainingCapacity(remaining);
        self.read_offset = 0;
    }

    fn readSource(self: *const Self) stream_read.ReadSource {
        return .{ .available = self.bufferedBytes(), .has_ended = self.has_ended, .failure = self.failure };
    }

    fn startTimer(self: *Self, ms: u64) !void {
        try self.startTimerHandle(&self.timer_memory, &self.timer_owner, onReadTimeout, ms);
        self.is_timer_running = true;
    }

    fn timerHandle(self: *Self) *uv.Handle {
        return @ptrCast(self.timer_memory.ptr);
    }

    fn startWriteTimer(self: *Self) !void {
        const ms = self.write_timeout_ms orelse return;
        try self.startTimerHandle(&self.write_timer_memory, &self.write_timer_owner, onWriteTimeout, ms);
        self.is_write_timer_running = true;
    }

    fn finishQueuedWrite(self: *Self) void {
        self.queued_writes -= 1;
        self.stopWriteTimer();
        const has_more_writes = self.queued_writes > 0 and !self.is_closed;
        if (!has_more_writes) return;
        self.startWriteTimer() catch {};
    }

    fn stopWriteTimer(self: *Self) void {
        if (!self.is_write_timer_running) return;
        _ = uv.uv_timer_stop(self.writeTimerHandle());
        self.is_write_timer_running = false;
    }

    fn writeTimerHandle(self: *Self) *uv.Handle {
        return @ptrCast(self.write_timer_memory.ptr);
    }
};

pub const WriteOutcome = union(enum) {
    done,
    parked,
    failure: IoError,
};

const WriteRequest = struct {
    allocator: std.mem.Allocator,
    stream: *Stream,
    task: ?*TaskState,
    bytes: []u8,
    buffers: [1]uv.Buffer,
    request_memory: []align(16) u8,

    fn create(stream: *Stream, task: ?*TaskState, data: []const u8) !*WriteRequest {
        const allocator = stream.allocator;
        const request = try allocator.create(WriteRequest);
        errdefer allocator.destroy(request);
        const bytes = try allocator.dupe(u8, data);
        errdefer allocator.free(bytes);
        const memory = try allocator.alignedAlloc(u8, handle_alignment, uv.uv_req_size(.write));
        request.* = .{
            .allocator = allocator,
            .stream = stream,
            .task = task,
            .bytes = bytes,
            .buffers = .{.{ .base = bytes.ptr, .len = @intCast(bytes.len) }},
            .request_memory = memory,
        };
        uv.uv_req_set_data(request.uvRequest(), request);
        return request;
    }

    fn destroy(self: *WriteRequest) void {
        const allocator = self.allocator;
        allocator.free(self.request_memory);
        allocator.free(self.bytes);
        allocator.destroy(self);
    }

    fn uvRequest(self: *WriteRequest) *uv.Request {
        return @ptrCast(self.request_memory.ptr);
    }
};

fn onWrite(uv_request: *uv.Request, status: c_int) callconv(.c) void {
    const request: *WriteRequest = @ptrCast(@alignCast(uv.uv_req_get_data(uv_request).?));
    defer request.destroy();
    const stream = request.stream;
    stream.finishQueuedWrite();
    const failure: IoError = if (stream.has_write_timed_out) .TimedOut else stream_read.ioErrorFromStatus(status);
    if (request.task) |task| {
        stream.event_loop.pending_operations -= 1;
        const wake_result: WakeResult = if (status == 0) stream_read.doneResult() else stream_read.failureResult(failure);
        stream.wakeTask(task, wake_result);
    } else if (status != 0 and !stream.is_closed) {
        stream.failure = failure;
        if (stream.tls_layer) |layer| {
            if (layer.handshake == .running) {
                stream_tls.finishHandshake(stream, .failed);
            }
        }
    }
    stream.finishRequest();
}

fn onWriteTimeout(timer: *uv.Handle) callconv(.c) void {
    const stream: *Stream = @alignCast(@fieldParentPtr("write_timer_owner", HandleOwner.of(timer)));
    stream.is_write_timer_running = false;
    stream.has_write_timed_out = true;
    stream.event_loop.closeStream(stream.table_index);
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
    stream.scheduler.wake(waiter.task, waiter.mode.wakeFailure(.TimedOut)) catch |failure| {
        stream.event_loop.callback_failure = failure;
    };
    stream.applyBackpressure();
}

fn onAlloc(handle: *uv.Handle, _: usize, buffer: *uv.Buffer) callconv(.c) void {
    const stream = streamOf(handle);
    if (stream.tunnel_layer) |tunnel| {
        buffer.* = .{ .base = &tunnel.cipher_scratch, .len = cipher_chunk_size };
        return;
    }
    if (stream.tls_layer) |layer| {
        buffer.* = .{ .base = &layer.cipher_scratch, .len = cipher_chunk_size };
        return;
    }
    stream.buffered.ensureUnusedCapacity(stream.allocator, read_chunk_size) catch {
        buffer.* = .{ .base = undefined, .len = 0 };
        return;
    };
    const free_space = stream.buffered.unusedCapacitySlice();
    buffer.* = .{ .base = free_space.ptr, .len = @intCast(free_space.len) };
}

fn onRead(handle: *uv.Handle, read_count: isize, _: *const uv.Buffer) callconv(.c) void {
    const stream = streamOf(handle);
    if (stream.tunnel_layer) |tunnel| {
        stream_tls.receiveTunnelRead(stream, tunnel, read_count);
        stream_http.evictIfUnusable(stream);
        return;
    }
    if (stream.tls_layer) |layer| {
        stream_tls.receiveTlsRead(stream, layer, read_count);
        stream_http.evictIfUnusable(stream);
        return;
    }
    if (read_count > 0) {
        stream.buffered.items.len += @intCast(read_count);
    } else if (read_count < 0) {
        const status: c_int = @intCast(read_count);
        if (stream_read.isErrorNamed(status, "EOF")) {
            stream.has_ended = true;
        } else {
            stream.failure = stream_read.ioErrorFromStatus(status);
        }
    }
    stream.serveWaiter();
    stream.applyBackpressure();
    stream_http.evictIfUnusable(stream);
}
