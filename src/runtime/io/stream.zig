const std = @import("std");
const uv = @import("../tasks/loop/uv.zig");
const v = @import("../value.zig");
const EventLoop = @import("../tasks/event_loop.zig").EventLoop;
const HandleOwner = @import("../tasks/event_loop.zig").HandleOwner;
const Scheduler = @import("../tasks/scheduler.zig").Scheduler;
const TaskState = @import("../tasks/scheduler.zig").TaskState;
const WakeResult = @import("../tasks/scheduler.zig").WakeResult;
const FsJob = @import("fs/fs_job.zig").FsJob;
const tls = @import("net/tls.zig");
const TlsLayer = @import("net/tls_layer.zig").TlsLayer;
const ServerCertificate = @import("net/tls_layer.zig").ServerCertificate;
const HandshakeWaiter = @import("net/tls_layer.zig").HandshakeWaiter;
const cipher_chunk_size = @import("net/tls_layer.zig").cipher_chunk_size;
const netErrorFromHandshake = @import("net/tls_layer.zig").netErrorFromHandshake;
const NetError = @import("net/tcp.zig").NetError;
const HttpReader = @import("http/http_reader.zig").HttpReader;
const MessageKind = @import("http/http_reader.zig").MessageKind;
const HttpError = @import("http/http.zig").HttpError;
const httpErrorFromIo = @import("http/http_reader.zig").httpErrorFromIo;
const max_http_head_size = @import("http/http_reader.zig").max_head_size;
const idle_timeout_ms = @import("http/connection_pool.zig").idle_timeout_ms;
const onHandleClosed = @import("../tasks/event_loop.zig").onHandleClosed;

const handle_alignment = std.mem.Alignment.@"16";
const read_chunk_size = 64 * 1024;
const high_water_mark = 1024 * 1024;
const tls_record_size = 16 * 1024 + 256;

const file_output_limit = 64 * 1024;
const max_uv_chunk: usize = 1 << 30;
const max_tls_handshake_bytes: usize = 256 * 1024;

pub const Stream = struct {
    const Self = @This();

    pub const Kind = enum { tty, pipe, file, tcp, closed_input };

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

    pub const Waiter = struct {
        task: *TaskState,
        mode: ReadMode,
    };

    pub const OutputWaiter = struct {
        task: *TaskState,
        is_flush: bool,
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
        _ = uv.uv_tcp_nodelay(@ptrCast(memory.ptr), 1);
        stream.* = .{ .allocator = allocator, .event_loop = event_loop, .scheduler = scheduler, .kind = .tcp, .loop = loop, .file = -1, .handle_memory = memory, .open_handles = 1 };
        uv.uv_handle_set_data(stream.uvHandle(), &stream.owner);
        return stream;
    }

    pub fn rememberPeer(self: *Self) void {
        var address: uv.SocketAddress = .{};
        var length: c_int = @sizeOf(uv.SocketAddress);
        if (uv.uv_tcp_getpeername(self.uvHandle(), &address, &length) != 0) return;
        self.peer = address;
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
            closeFileNow(self.allocator, self.loop, self.file);
        }
        self.allocator.destroy(self);
    }

    pub fn releaseAtExit(self: *Self) void {
        self.is_closed = true;
        self.writeOutputNow();
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
        if (self.tls_layer) |layer| {
            self.closeTls(layer);
        } else if (self.tunnel_layer) |tunnel| {
            if (tunnel.handshake == .done and self.failure == null) {
                _ = tls.dusk_tls_close_notify(tunnel.session);
                _ = self.sendTunnelOutput(tunnel, null) catch {};
            }
        }
        self.failOutputWaiters(.Closed);
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
            self.stopHandshakeTimer();
            uv.uv_close(self.handshakeTimerHandle(), onHandleClosed);
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

    fn releaseHostSlot(self: *Self) void {
        const key = self.host_key orelse return;
        self.host_key = null;
        defer self.allocator.free(key);
        self.event_loop.releaseHostSlot(key);
    }

    fn closeTls(self: *Self, layer: *TlsLayer) void {
        var waiters = layer.takeHandshakeWaiters();
        defer waiters.deinit(self.allocator);
        for (waiters.items) |waiter| {
            defer waiter.free(self.allocator);
            self.event_loop.pending_operations -= 1;
            const wake_result = if (waiter.pending_write == null) netFailureResult(.HandshakeFailed) else failureResult(.Closed);
            self.wakeTask(waiter.task, wake_result);
        }
        if (layer.handshake == .done) {
            _ = tls.dusk_tls_close_notify(layer.session);
            self.tryWriteCipherOutput(layer.session, self.tunnel_layer);
        }
        const tunnel = self.tunnel_layer orelse return;
        _ = tls.dusk_tls_close_notify(tunnel.session);
        self.tryWriteCipherOutput(tunnel.session, null);
    }

    fn tryWriteCipherOutput(self: *Self, session: *tls.Session, through: ?*TlsLayer) void {
        const length = tls.dusk_tls_output_length(session);
        if (length == 0) return;
        const output = tls.dusk_tls_output_bytes(session)[0..length];
        if (through) |tunnel| {
            _ = tls.dusk_tls_write(tunnel.session, output.ptr, output.len);
            tls.dusk_tls_consume_output(session, length);
            self.tryWriteCipherOutput(tunnel.session, null);
            return;
        }
        const buffers = [_]uv.Buffer{.{ .base = @constCast(output.ptr), .len = @intCast(output.len) }};
        _ = uv.uv_try_write(self.uvHandle(), &buffers, 1);
        tls.dusk_tls_consume_output(session, length);
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

    pub fn readyOutcome(self: *Self, mode: ReadMode) ?ReadOutcome {
        return switch (mode) {
            .bytes => |max| self.readSource().bytesOutcome(max),
            .exact => |count| self.readSource().exactOutcome(count),
            .line => |max| self.readSource().lineOutcome(max),
            .all_text => self.readSource().allOutcome(true),
            .all_bytes => self.readSource().allOutcome(false),
            .any_data => self.readSource().anyDataOutcome(),
            .http_head => self.httpHeadOutcome(),
            .http_body => |read| self.httpBodyOutcome(read),
            .http_whole_body => |cap| self.httpWholeBodyOutcome(cap),
        };
    }

    fn readSource(self: *const Self) ReadSource {
        return .{ .available = self.bufferedBytes(), .has_ended = self.has_ended, .failure = self.failure };
    }

    pub fn startHttpMessage(self: *Self, kind: MessageKind) !void {
        if (self.http_reader) |reader| {
            return reader.startMessage(kind);
        }
        self.http_reader = try HttpReader.create(self.allocator, kind);
    }

    pub fn isReusable(self: *const Self) bool {
        const reader = self.http_reader orelse return false;
        const is_idle = self.waiter == null and !self.is_closed and !self.has_ended and self.failure == null and self.bufferedBytes().len == 0;
        const is_tls_ready = if (self.tls_layer) |layer| layer.handshake == .done else true;
        return is_idle and is_tls_ready and !self.is_reuse_forbidden and reader.isMessageComplete() and reader.canKeepAlive();
    }

    pub fn enterPool(self: *Self, key: []const u8) !void {
        self.releaseHostSlot();
        self.pool_key = try self.allocator.dupe(u8, key);
        self.buffered.clearAndFree(self.allocator);
        self.read_offset = 0;
        if (self.http_reader) |reader| {
            reader.releaseBody();
        }
        try self.startReading();
        try self.startTimerHandle(&self.timer_memory, &self.timer_owner, onIdleTimeout, idle_timeout_ms);
        self.is_timer_running = true;
    }

    pub fn leavePool(self: *Self) void {
        const key = self.pool_key orelse return;
        self.allocator.free(key);
        self.pool_key = null;
        self.stopTimer();
    }

    fn evictIfUnusable(self: *Self) void {
        if (self.pool_key == null or self.isReusable()) return;
        self.evictFromPool();
    }

    fn evictFromPool(self: *Self) void {
        const key = self.pool_key orelse return;
        self.event_loop.connection_pool.remove(self.allocator, key, self.table_index);
        self.leavePool();
        self.event_loop.closeStream(self.table_index);
    }

    fn httpHeadOutcome(self: *Self) ?ReadOutcome {
        const reader = self.http_reader orelse return httpFailure(.Other);
        const available = self.bufferedBytes();
        switch (reader.feedHead(available)) {
            .done => |length| {
                if (length > max_http_head_size) {
                    return httpFailure(.HeadersTooLarge);
                }
                return .{ .result = .{ .data = available[0..length] }, .consumed = length };
            },
            .failure => |failure| return httpFailure(failure),
            .want_more => {},
        }
        if (available.len > max_http_head_size) {
            return httpFailure(.HeadersTooLarge);
        }
        if (self.failure) |failure| {
            return .{ .result = .{ .failure = failure } };
        }
        if (!self.has_ended) {
            return null;
        }
        if (available.len == 0) {
            return .{ .result = .end };
        }
        return httpFailure(reader.invalidMessage());
    }

    fn httpBodyOutcome(self: *Self, read: BodyRead) ?ReadOutcome {
        const reader = self.http_reader orelse return .{ .result = .{ .failure = .Other } };
        reader.dropTakenBody();
        if (self.feedHttpBody(reader, std.math.maxInt(usize)) != null) {
            return .{ .result = .{ .failure = .Other } };
        }
        reader.decodeBody(read.decodeLimit()) catch return .{ .result = .{ .failure = .Other } };
        var has_ended = reader.isMessageComplete();
        const has_no_more_input = self.has_ended and self.bufferedBytes().len == 0;
        if (!has_ended and has_no_more_input) {
            if (!reader.finishBody()) {
                return .{ .result = .{ .failure = .UnexpectedEnd } };
            }
            has_ended = true;
        }
        if (has_ended and reader.hasUndecodedBody()) {
            has_ended = false;
        } else if (has_ended and !reader.isDecodedToEnd()) {
            return .{ .result = .{ .failure = .UnexpectedEnd } };
        }
        const source: ReadSource = .{ .available = reader.pendingBody(), .has_ended = has_ended, .failure = if (has_ended) null else self.failure };
        const found = switch (read) {
            .bytes => |max| source.bytesOutcome(max),
            .line => |max| source.lineOutcome(max),
            .all_text => source.allOutcome(true),
            .all_bytes => source.allOutcome(false),
        };
        var outcome = found orelse return null;
        reader.taken = outcome.consumed;
        outcome.consumed = 0;
        return outcome;
    }

    fn feedHttpBody(self: *Self, reader: *HttpReader, max: usize) ?HttpError {
        const wire_limit = max +| max_http_head_size;
        while (!reader.isMessageComplete()) {
            const available = self.bufferedBytes();
            const room = max -| reader.body.items.len;
            const wire_room = wire_limit -| reader.body_wire_bytes;
            const step_limit = @min(room, wire_room);
            const used = reader.feedBody(available[0..@min(available.len, step_limit)]) catch |failure| {
                if (failure == error.OutOfMemory) {
                    return .Other;
                }
                reader.was_body_invalid = true;
                return reader.invalidMessage();
            };
            reader.body_wire_bytes +|= used;
            self.consume(used);
            if (!reader.isMessageComplete() and reader.body_wire_bytes >= wire_limit) {
                reader.was_body_too_large = true;
                return .BodyTooLarge;
            }
            if (used == 0) break;
        }
        return null;
    }

    fn httpWholeBodyOutcome(self: *Self, cap: usize) ?ReadOutcome {
        const reader = self.http_reader orelse return httpFailure(.Other);
        if (self.feedHttpBody(reader, cap +| 1)) |failure| {
            return httpFailure(failure);
        }
        if (reader.body.items.len > cap) {
            reader.was_body_too_large = true;
            return httpFailure(.BodyTooLarge);
        }
        if (reader.isMessageComplete()) {
            return .{ .result = .{ .data = reader.body.items } };
        }
        if (self.failure) |failure| {
            return .{ .result = .{ .failure = failure } };
        }
        if (!self.has_ended) {
            return null;
        }
        if (reader.finishBody()) {
            return .{ .result = .{ .data = reader.body.items } };
        }
        return httpFailure(reader.invalidMessage());
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

    fn startTimer(self: *Self, ms: u64) !void {
        try self.startTimerHandle(&self.timer_memory, &self.timer_owner, onReadTimeout, ms);
        self.is_timer_running = true;
    }

    fn startTimerHandle(self: *Self, memory: *[]align(16) u8, owner: *HandleOwner, callback: uv.TimerCallback, ms: u64) !void {
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
            .file => {
                if (self.can_write_file) {
                    return error.StreamReadFailed;
                }
                try self.startChunk();
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

    fn startChunk(self: *Self) !void {
        if (self.has_chunk_in_flight) return;
        const job = try FsJob.create(self.allocator, .read_chunk, "", "");
        errdefer job.destroy();
        job.file = self.file;
        job.offset = if (self.owns_file) self.file_offset else -1;
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
            self.releaseIfDone();
            return;
        }
        if (has_failed) {
            self.failure = .Other;
        } else if (contents) |data| {
            if (data.len == 0) {
                self.has_ended = true;
            } else {
                self.file_offset += @intCast(data.len);
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
            .failure => |failure| waiter.mode.wakeFailure(failure),
            .http_failure => |failure| httpFailureResult(failure),
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

    pub fn write(self: *Self, task: *TaskState, data: []const u8) !WriteOutcome {
        if (data.len == 0) return .done;
        if (self.kind == .file) {
            return self.writeFile(task, data);
        }
        const layer = self.tls_layer orelse return self.sendBytes(task, data);
        if (layer.handshake != .done) {
            self.ensureHandshakeStarted();
        }
        switch (layer.handshake) {
            .failed => return .{ .failure = .HandshakeFailed },
            .pending, .running => {
                const bytes = try self.allocator.dupe(u8, data);
                errdefer self.allocator.free(bytes);
                try layer.handshake_waiters.append(self.allocator, .{ .task = task, .pending_write = bytes });
                self.event_loop.pending_operations += 1;
                return .parked;
            },
            .done => {},
        }
        if (tls.dusk_tls_write(layer.session, data.ptr, data.len) < 0) {
            return .{ .failure = .Other };
        }
        return self.sendCipherOutput(task);
    }

    fn writeFile(self: *Self, task: *TaskState, data: []const u8) !WriteOutcome {
        if (!self.can_write_file) {
            return .{ .failure = .Other };
        }
        if (self.takeOutputFailure()) |failure| {
            return .{ .failure = failure };
        }
        try self.pending_output.appendSlice(self.allocator, data);
        if (self.pending_output.items.len < file_output_limit) {
            return .done;
        }
        try self.startOutputJob();
        if (self.pending_output.items.len < file_output_limit) {
            return .done;
        }
        try self.output_waiters.append(self.allocator, .{ .task = task, .is_flush = false });
        self.event_loop.pending_operations += 1;
        return .parked;
    }

    pub fn flush(self: *Self, task: *TaskState) !WriteOutcome {
        if (self.kind != .file) {
            return .done;
        }
        if (self.takeOutputFailure()) |failure| {
            return .{ .failure = failure };
        }
        const has_output = self.pending_output.items.len > 0 or self.has_output_in_flight;
        if (!has_output) {
            return .done;
        }
        try self.startOutputJob();
        try self.output_waiters.append(self.allocator, .{ .task = task, .is_flush = true });
        self.event_loop.pending_operations += 1;
        return .parked;
    }

    pub fn filePosition(self: *const Self) i64 {
        if (self.can_write_file) {
            return self.file_offset + @as(i64, @intCast(self.pending_output.items.len));
        }
        const unread: i64 = @intCast(self.buffered.items.len - self.read_offset);
        return self.file_offset - unread;
    }

    pub fn seekFile(self: *Self, target: i64) ?IoError {
        const can_seek = self.owns_file and !self.is_append and target >= 0;
        if (!can_seek) {
            return .Other;
        }
        if (self.waiter != null or self.has_chunk_in_flight) {
            return .Busy;
        }
        const has_output = self.pending_output.items.len > 0 or self.has_output_in_flight;
        if (has_output) {
            return .Other;
        }
        self.file_offset = target;
        self.buffered.clearRetainingCapacity();
        self.read_offset = 0;
        self.has_ended = false;
        return null;
    }

    fn takeOutputFailure(self: *Self) ?IoError {
        const failure = self.output_failure orelse return null;
        self.output_failure = null;
        return failure;
    }

    pub fn flushOutputBeforeClose(self: *Self) void {
        self.startOutputJob() catch self.writeOutputNow();
    }

    fn startOutputJob(self: *Self) !void {
        const can_start = !self.has_output_in_flight and self.pending_output.items.len > 0;
        if (!can_start) return;
        const job = try FsJob.create(self.allocator, .write_chunk, "", "");
        errdefer job.destroy();
        const owned_output = try self.pending_output.toOwnedSlice(self.allocator);
        job.output = owned_output;
        job.file = self.file;
        job.offset = if (self.is_append) -1 else self.file_offset;
        job.stream = self;
        self.event_loop.startFsJob(self.scheduler, null, job) catch |failure| {
            self.pending_output = std.ArrayList(u8).fromOwnedSlice(owned_output);
            job.output = null;
            return failure;
        };
        self.has_output_in_flight = true;
    }

    pub fn finishOutput(self: *Self, written_bytes: usize, has_failed: bool) void {
        self.has_output_in_flight = false;
        self.file_offset += @intCast(written_bytes);
        if (has_failed) {
            self.output_failure = .Other;
        }
        if (self.is_closed) {
            self.failOutputWaiters(.Closed);
            self.writeOutputNow();
            self.releaseIfDone();
            return;
        }
        if (self.output_failure == null) {
            self.startOutputJob() catch {
                self.output_failure = .Other;
            };
        }
        self.wakeOutputWaiters();
    }

    fn failOutputWaiters(self: *Self, failure: IoError) void {
        for (self.output_waiters.items) |waiter| {
            self.event_loop.pending_operations -= 1;
            self.wakeTask(waiter.task, failureResult(failure));
        }
        self.output_waiters.clearRetainingCapacity();
    }

    fn wakeOutputWaiters(self: *Self) void {
        const is_drained = !self.has_output_in_flight and self.pending_output.items.len == 0;
        const failure = self.output_failure;
        var kept: usize = 0;
        var has_woken = false;
        for (self.output_waiters.items) |waiter| {
            const has_room = self.pending_output.items.len < file_output_limit;
            const can_continue = failure != null or (if (waiter.is_flush) is_drained else has_room);
            if (!can_continue) {
                self.output_waiters.items[kept] = waiter;
                kept += 1;
                continue;
            }
            self.event_loop.pending_operations -= 1;
            const wake_result: WakeResult = if (failure) |found| failureResult(found) else doneResult();
            self.wakeTask(waiter.task, wake_result);
            has_woken = true;
        }
        self.output_waiters.shrinkRetainingCapacity(kept);
        if (has_woken) {
            self.output_failure = null;
        }
    }

    fn writeOutputNow(self: *Self) void {
        const has_output = self.pending_output.items.len > 0 and self.output_failure == null and !self.has_output_in_flight;
        if (!has_output) return;
        const request_memory = self.allocator.alignedAlloc(u8, handle_alignment, uv.uv_req_size(.fs)) catch return;
        defer self.allocator.free(request_memory);
        const request: *uv.Request = @ptrCast(request_memory.ptr);
        var written: usize = 0;
        const output = self.pending_output.items;
        while (written < output.len) {
            const chunk_len = @min(output.len - written, max_uv_chunk);
            const buffer = uv.Buffer{ .base = output.ptr + written, .len = @intCast(chunk_len) };
            const offset: i64 = if (self.is_append) -1 else self.file_offset + @as(i64, @intCast(written));
            const status = uv.uv_fs_write(self.loop, request, self.file, @ptrCast(&buffer), 1, offset, null);
            uv.uv_fs_req_cleanup(request);
            if (status <= 0) break;
            written += @intCast(status);
        }
        self.file_offset += @intCast(written);
        self.pending_output.clearRetainingCapacity();
    }

    fn sendBytes(self: *Self, task: ?*TaskState, data: []const u8) !WriteOutcome {
        if (data.len == 0) return .done;
        const try_len = @min(data.len, max_uv_chunk);
        const buffers = [_]uv.Buffer{.{ .base = @constCast(data.ptr), .len = @intCast(try_len) }};
        const status = uv.uv_try_write(self.uvHandle(), &buffers, 1);
        const written: usize = if (status >= 0) @intCast(status) else 0;
        if (written == data.len) return .done;
        const is_waiting_for_room = status >= 0 or isErrorNamed(status, "EAGAIN");
        if (!is_waiting_for_room) {
            return .{ .failure = ioErrorFromStatus(status) };
        }
        const request = try WriteRequest.create(self, task, data[written..]);
        const write_status = uv.uv_write(request.uvRequest(), self.uvHandle(), &request.buffers, 1, onWrite);
        if (write_status != 0) {
            request.destroy();
            return .{ .failure = ioErrorFromStatus(write_status) };
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
        self.is_starting_tls = false;
        const layer = TlsLayer.createClient(self.allocator, config, owns_config, host) catch |failure| {
            if (owns_config) {
                tls.dusk_tls_config_free(config);
            }
            return failure;
        };
        if (is_tunneled and self.tls_layer != null) {
            self.tunnel_layer = self.tls_layer;
        }
        self.tls_layer = layer;
        self.feedBufferedIntoTls(layer);
        return self.awaitHandshake(task, timeout_ms);
    }

    pub fn startServerTls(self: *Self, task: *TaskState, certificate: *ServerCertificate, timeout_ms: ?u64) !HandshakeWait {
        const layer = try TlsLayer.createServer(self.allocator, certificate);
        self.tls_layer = layer;
        self.feedBufferedIntoTls(layer);
        return self.awaitHandshake(task, timeout_ms);
    }

    fn feedBufferedIntoTls(self: *Self, layer: *TlsLayer) void {
        const early = self.bufferedBytes();
        if (early.len == 0) return;
        _ = tls.dusk_tls_feed(layer.session, early.ptr, early.len);
        self.consume(early.len);
    }

    pub fn awaitHandshake(self: *Self, task: *TaskState, timeout_ms: ?u64) !HandshakeWait {
        const layer = self.tls_layer orelse return .done;
        self.ensureHandshakeStarted();
        switch (layer.handshake) {
            .done => return .done,
            .failed => return .{ .failure = layer.handshake_failure },
            .pending, .running => {},
        }
        if (timeout_ms) |ms| {
            if (!self.is_handshake_timer_running) {
                try self.startTimerHandle(&self.handshake_timer_memory, &self.handshake_timer_owner, onHandshakeTimeout, ms);
                self.is_handshake_timer_running = true;
            }
        }
        try layer.handshake_waiters.append(self.allocator, .{ .task = task, .pending_write = null });
        self.event_loop.pending_operations += 1;
        return .parked;
    }

    fn stopHandshakeTimer(self: *Self) void {
        if (!self.is_handshake_timer_running) return;
        _ = uv.uv_timer_stop(self.handshakeTimerHandle());
        self.is_handshake_timer_running = false;
    }

    fn handshakeTimerHandle(self: *Self) *uv.Handle {
        return @ptrCast(self.handshake_timer_memory.ptr);
    }

    pub fn useServerTls(self: *Self, certificate: *ServerCertificate) !void {
        self.tls_layer = try TlsLayer.createServer(self.allocator, certificate);
    }

    pub fn ensureHandshakeStarted(self: *Self) void {
        const layer = self.tls_layer orelse return;
        if (layer.handshake != .pending) return;
        layer.handshake = .running;
        self.startReading() catch {
            layer.handshake = .failed;
            self.failure = .HandshakeFailed;
            return;
        };
        self.stepHandshake(false);
    }

    fn stepHandshake(self: *Self, should_wake: bool) void {
        const layer = self.tls_layer.?;
        const status = tls.dusk_tls_handshake(layer.session);
        self.flushCipherOutput();
        const outcome: TlsLayer.Handshake = switch (status) {
            .want_more => if (self.failure != null) .failed else return,
            .done => .done,
            else => .failed,
        };
        if (outcome == .failed and status != .want_more) {
            layer.handshake_failure = netErrorFromHandshake(status);
        }
        if (!should_wake) {
            layer.handshake = outcome;
            if (outcome == .failed) {
                self.failure = .HandshakeFailed;
            }
            return;
        }
        self.finishHandshake(outcome);
    }

    fn finishHandshake(self: *Self, outcome: TlsLayer.Handshake) void {
        const layer = self.tls_layer.?;
        layer.handshake = outcome;
        self.stopHandshakeTimer();
        if (outcome == .failed) {
            self.failure = .HandshakeFailed;
        }
        var waiters = layer.takeHandshakeWaiters();
        defer waiters.deinit(self.allocator);
        for (waiters.items) |waiter| {
            defer waiter.free(self.allocator);
            self.event_loop.pending_operations -= 1;
            const wake_result = self.handshakeWakeResult(waiter, outcome) orelse continue;
            self.wakeTask(waiter.task, wake_result);
        }
        if (outcome == .done) {
            self.decryptAvailable();
            return;
        }
        self.serveWaiter();
        self.applyBackpressure();
    }

    fn handshakeWakeResult(self: *Self, waiter: HandshakeWaiter, outcome: TlsLayer.Handshake) ?WakeResult {
        const layer = self.tls_layer.?;
        const bytes = waiter.pending_write orelse {
            return if (outcome == .done) doneResult() else netFailureResult(layer.handshake_failure);
        };
        if (outcome != .done) {
            return failureResult(.HandshakeFailed);
        }
        if (tls.dusk_tls_write(layer.session, bytes.ptr, bytes.len) < 0) {
            self.failure = .Other;
            return failureResult(.Other);
        }
        const sent = self.sendCipherOutput(waiter.task) catch return failureResult(.Other);
        return switch (sent) {
            .done => doneResult(),
            .parked => null,
            .failure => |failure| failureResult(failure),
        };
    }

    fn receiveTlsRead(self: *Self, layer: *TlsLayer, read_count: isize) void {
        if (read_count > 0) {
            self.receiveCiphertext(layer, layer.cipher_scratch[0..@intCast(read_count)]);
            return;
        }
        if (read_count == 0) return;
        const status: c_int = @intCast(read_count);
        if (isErrorNamed(status, "EOF")) {
            self.has_ended = true;
        } else {
            self.failure = ioErrorFromStatus(status);
        }
        if (layer.handshake == .running) {
            self.finishHandshake(.failed);
            return;
        }
        self.serveWaiter();
        self.applyBackpressure();
    }

    fn receiveTunnelRead(self: *Self, tunnel: *TlsLayer, read_count: isize) void {
        const layer = self.tls_layer orelse return;
        if (read_count <= 0) {
            self.receiveTlsRead(layer, read_count);
            return;
        }
        if (tls.dusk_tls_feed(tunnel.session, &tunnel.cipher_scratch, @intCast(read_count)) != .done) {
            self.failTlsRead(layer, .Other);
            return;
        }
        self.forwardTunnelPlaintext(tunnel, layer);
    }

    fn forwardTunnelPlaintext(self: *Self, tunnel: *TlsLayer, layer: *TlsLayer) void {
        while (true) {
            const count = tls.dusk_tls_read(tunnel.session, &layer.cipher_scratch, cipher_chunk_size);
            if (count > 0) {
                self.receiveCiphertext(layer, layer.cipher_scratch[0..@intCast(count)]);
                continue;
            }
            if (count == @intFromEnum(tls.Status.end)) {
                self.has_ended = true;
                if (layer.handshake == .running) {
                    self.finishHandshake(.failed);
                    return;
                }
                self.serveWaiter();
                self.applyBackpressure();
            } else if (count < 0) {
                self.failTlsRead(layer, .Other);
                return;
            }
            break;
        }
        const outcome = self.sendTunnelOutput(tunnel, null) catch {
            self.failTlsRead(layer, .Other);
            return;
        };
        switch (outcome) {
            .failure => |failure| self.failTlsRead(layer, failure),
            .done, .parked => {},
        }
    }

    fn failTlsRead(self: *Self, layer: *TlsLayer, failure: IoError) void {
        self.failure = failure;
        if (layer.handshake == .running) {
            self.finishHandshake(.failed);
            return;
        }
        self.serveWaiter();
        self.applyBackpressure();
    }

    fn receiveCiphertext(self: *Self, layer: *TlsLayer, data: []const u8) void {
        if (layer.handshake == .running) {
            layer.handshake_bytes_received +|= data.len;
            if (layer.handshake_bytes_received > max_tls_handshake_bytes) {
                self.finishHandshake(.failed);
                return;
            }
        }
        if (tls.dusk_tls_feed(layer.session, data.ptr, data.len) != .done) {
            self.failTlsRead(layer, .Other);
            return;
        }
        switch (layer.handshake) {
            .running => self.stepHandshake(true),
            .done => self.decryptAvailable(),
            .pending, .failed => {},
        }
    }

    fn decryptAvailable(self: *Self) void {
        const layer = self.tls_layer.?;
        while (true) {
            self.buffered.ensureUnusedCapacity(self.allocator, tls_record_size) catch {
                self.failure = .Other;
                break;
            };
            const space = self.buffered.unusedCapacitySlice();
            const count = tls.dusk_tls_read(layer.session, space.ptr, space.len);
            if (count > 0) {
                self.buffered.items.len += @intCast(count);
                continue;
            }
            if (count == @intFromEnum(tls.Status.end)) {
                self.has_ended = true;
            } else if (count < 0) {
                self.failure = .Other;
            }
            break;
        }
        self.flushCipherOutput();
        self.serveWaiter();
        self.applyBackpressure();
    }

    fn sendCipherOutput(self: *Self, task: ?*TaskState) !WriteOutcome {
        const session = self.tls_layer.?.session;
        const length = tls.dusk_tls_output_length(session);
        if (length == 0) return .done;
        const output = tls.dusk_tls_output_bytes(session)[0..length];
        const tunnel = self.tunnel_layer orelse {
            const outcome = try self.sendBytes(task, output);
            tls.dusk_tls_consume_output(session, length);
            return outcome;
        };
        if (tls.dusk_tls_write(tunnel.session, output.ptr, output.len) < 0) {
            return .{ .failure = .Other };
        }
        tls.dusk_tls_consume_output(session, length);
        return self.sendTunnelOutput(tunnel, task);
    }

    fn sendTunnelOutput(self: *Self, tunnel: *TlsLayer, task: ?*TaskState) !WriteOutcome {
        const length = tls.dusk_tls_output_length(tunnel.session);
        if (length == 0) return .done;
        const outcome = try self.sendBytes(task, tls.dusk_tls_output_bytes(tunnel.session)[0..length]);
        tls.dusk_tls_consume_output(tunnel.session, length);
        return outcome;
    }

    fn flushCipherOutput(self: *Self) void {
        if (self.is_closed) return;
        const outcome = self.sendCipherOutput(null) catch {
            self.failure = .Other;
            return;
        };
        switch (outcome) {
            .failure => |failure| self.failure = failure,
            .done, .parked => {},
        }
    }

    fn wakeTask(self: *Self, task: *TaskState, wake_result: WakeResult) void {
        self.scheduler.wake(task, wake_result) catch |failure| {
            self.event_loop.callback_failure = failure;
        };
    }
};

pub const HandshakeWait = union(enum) {
    done,
    parked,
    failure: NetError,
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
    const failure: IoError = if (stream.has_write_timed_out) .TimedOut else ioErrorFromStatus(status);
    if (request.task) |task| {
        stream.event_loop.pending_operations -= 1;
        const wake_result: WakeResult = if (status == 0) doneResult() else failureResult(failure);
        stream.wakeTask(task, wake_result);
    } else if (status != 0 and !stream.is_closed) {
        stream.failure = failure;
        if (stream.tls_layer) |layer| {
            if (layer.handshake == .running) {
                stream.finishHandshake(.failed);
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

pub const ReadMode = union(enum) {
    bytes: usize,
    exact: usize,
    line: ?usize,
    all_text,
    all_bytes,
    any_data,
    http_head,
    http_body: BodyRead,
    http_whole_body: usize,

    pub fn valueKind(self: ReadMode) v.ValueType {
        return switch (self) {
            .line, .all_text => .string,
            .bytes, .exact, .all_bytes, .any_data, .http_head, .http_whole_body => .buffer,
            .http_body => |read| switch (read) {
                .line, .all_text => .string,
                .bytes, .all_bytes => .buffer,
            },
        };
    }

    pub fn wakeFailure(self: ReadMode, failure: IoError) WakeResult {
        return switch (self) {
            .http_head, .http_whole_body => httpFailureResult(httpErrorFromIo(failure)),
            else => failureResult(failure),
        };
    }
};

pub const BodyRead = union(enum) {
    bytes: usize,
    line: ?usize,
    all_text,
    all_bytes,

    fn decodeLimit(self: BodyRead) usize {
        return switch (self) {
            .bytes => |max| @max(max, min_decoded_bytes),
            .line, .all_text, .all_bytes => std.math.maxInt(usize),
        };
    }
};

const min_decoded_bytes = 64 * 1024;

pub const ReadOutcome = struct {
    result: union(enum) {
        data: []const u8,
        end,
        failure: IoError,
        http_failure: HttpError,
    },
    consumed: usize = 0,
};

const ReadSource = struct {
    available: []const u8,
    has_ended: bool,
    failure: ?IoError,

    fn bytesOutcome(self: ReadSource, max: usize) ?ReadOutcome {
        const available = self.available;
        if (available.len > 0) {
            const count = @min(available.len, max);
            return .{ .result = .{ .data = available[0..count] }, .consumed = count };
        }
        return self.endOutcome();
    }

    fn exactOutcome(self: ReadSource, count: usize) ?ReadOutcome {
        const available = self.available;
        if (available.len >= count) {
            return .{ .result = .{ .data = available[0..count] }, .consumed = count };
        }
        const has_ended_partway = self.has_ended and available.len > 0 and self.failure == null;
        if (has_ended_partway) {
            return .{ .result = .{ .failure = .UnexpectedEnd } };
        }
        return self.endOutcome();
    }

    fn lineOutcome(self: ReadSource, max: ?usize) ?ReadOutcome {
        const available = self.available;
        if (std.mem.indexOfScalar(u8, available, '\n')) |newline| {
            return lineFrom(available[0..newline], newline + 1, max);
        }
        if (max) |limit| {
            const might_end_with_crlf = available.len == limit + 1 and available[limit] == '\r' and !self.has_ended;
            if (available.len > limit and !might_end_with_crlf) {
                return .{ .result = .{ .failure = .LineTooLong } };
            }
        }
        const is_last_line = available.len > 0 and self.has_ended;
        if (is_last_line) {
            return lineFrom(available, available.len, max);
        }
        return self.endOutcome();
    }

    fn allOutcome(self: ReadSource, needs_utf8: bool) ?ReadOutcome {
        if (self.failure) |failure| {
            return .{ .result = .{ .failure = failure } };
        }
        if (!self.has_ended) return null;
        const available = self.available;
        if (needs_utf8 and !std.unicode.utf8ValidateSlice(available)) {
            return .{ .result = .{ .failure = .InvalidUtf8 }, .consumed = available.len };
        }
        return .{ .result = .{ .data = available }, .consumed = available.len };
    }

    fn anyDataOutcome(self: ReadSource) ?ReadOutcome {
        if (self.available.len > 0) {
            return .{ .result = .{ .data = self.available[0..0] } };
        }
        return self.endOutcome();
    }

    fn endOutcome(self: ReadSource) ?ReadOutcome {
        if (self.failure) |failure| {
            return .{ .result = .{ .failure = failure } };
        }
        if (self.has_ended) {
            return .{ .result = .end };
        }
        return null;
    }
};

fn httpFailure(failure: HttpError) ReadOutcome {
    return .{ .result = .{ .http_failure = failure } };
}

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
    UnexpectedEnd,
    HandshakeFailed,
    Other,
};

const io_errors_by_name = std.StaticStringMap(IoError).initComptime(.{
    .{ "EPIPE", .BrokenPipe },
    .{ "ECONNRESET", .ConnectionReset },
    .{ "ECANCELED", .Closed },
});

fn ioErrorFromStatus(status: c_int) IoError {
    return io_errors_by_name.get(std.mem.span(uv.uv_err_name(status))) orelse .Other;
}

fn isErrorNamed(status: c_int, name: []const u8) bool {
    return std.mem.eql(u8, std.mem.span(uv.uv_err_name(status)), name);
}

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

fn netFailureResult(failure: NetError) WakeResult {
    return .{ .value = .{ .int64 = @intFromEnum(failure) }, .kind = .int64, .has_failed = true, .error_is_relative = true };
}

fn httpFailureResult(failure: HttpError) WakeResult {
    return .{ .value = .{ .int64 = @intFromEnum(failure) }, .kind = .int64, .has_failed = true, .error_is_relative = true };
}

fn doneResult() WakeResult {
    return .{ .value = v.NULL_VALUE, .kind = .null };
}

fn streamOf(handle: *uv.Handle) *Stream {
    return @alignCast(@fieldParentPtr("owner", HandleOwner.of(handle)));
}

fn onIdleTimeout(timer: *uv.Handle) callconv(.c) void {
    const stream: *Stream = @alignCast(@fieldParentPtr("timer_owner", HandleOwner.of(timer)));
    stream.is_timer_running = false;
    stream.evictFromPool();
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

fn onHandshakeTimeout(timer: *uv.Handle) callconv(.c) void {
    const stream: *Stream = @alignCast(@fieldParentPtr("handshake_timer_owner", HandleOwner.of(timer)));
    stream.is_handshake_timer_running = false;
    const layer = stream.tls_layer orelse return;
    if (layer.isHandshakeOver()) return;
    layer.handshake_failure = .TimedOut;
    stream.finishHandshake(.failed);
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
        stream.receiveTunnelRead(tunnel, read_count);
        stream.evictIfUnusable();
        return;
    }
    if (stream.tls_layer) |layer| {
        stream.receiveTlsRead(layer, read_count);
        stream.evictIfUnusable();
        return;
    }
    if (read_count > 0) {
        stream.buffered.items.len += @intCast(read_count);
    } else if (read_count < 0) {
        const status: c_int = @intCast(read_count);
        if (isErrorNamed(status, "EOF")) {
            stream.has_ended = true;
        } else {
            stream.failure = ioErrorFromStatus(status);
        }
    }
    stream.serveWaiter();
    stream.applyBackpressure();
    stream.evictIfUnusable();
}
