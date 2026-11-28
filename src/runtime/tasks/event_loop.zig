const std = @import("std");
const uv = @import("loop/uv.zig");
const Scheduler = @import("scheduler.zig").Scheduler;
const TaskState = @import("scheduler.zig").TaskState;
const v = @import("../value.zig");
const FsJob = @import("../io/fs/fs_job.zig").FsJob;
const Stream = @import("../io/stream/stream.zig").Stream;
const Listener = @import("../io/net/tcp.zig").Listener;
const OpenRequest = @import("../io/net/tcp.zig").OpenRequest;
const ServerCertificate = @import("../io/net/tls_layer.zig").ServerCertificate;
const CompressionStream = @import("../io/compression/compression.zig").CompressionStream;
const CompressionJob = @import("../io/compression/compression_job.zig").CompressionJob;
const HostSlots = @import("../io/http/host_slots.zig").HostSlots;
const ResourceRegistry = @import("loop/resource_registry.zig").ResourceRegistry;
const SlotTable = @import("loop/slot_table.zig").SlotTable;
const HttpError = @import("../io/http/http.zig").HttpError;
const WakeResult = @import("scheduler.zig").WakeResult;
const TrustStore = @import("../io/net/trust_store.zig").TrustStore;
const ConnectionPool = @import("../io/http/connection_pool.zig").ConnectionPool;
const ServerState = @import("../io/http/http_server.zig").ServerState;
const Process = @import("../io/process/process.zig").Process;
const process_runtime = @import("../io/process/process.zig");
const Subscription = @import("../io/os/signal.zig").Subscription;
const TerminalOutput = @import("../io/terminal.zig").TerminalOutput;
const TerminalSize = @import("../io/terminal.zig").TerminalSize;
const WorkerDispatch = @import("loop/worker_dispatch.zig").WorkerDispatch;
const WorkerPool = @import("loop/worker_pool.zig").WorkerPool;
const Databases = @import("../io/sqlite/sqlite.zig").Databases;
const SqliteTimer = @import("../io/sqlite/sqlite.zig").OperationTimer;
const LocalTimezoneLookup = @import("../io/time/timezone_job.zig").LocalLookup;

const handle_alignment = std.mem.Alignment.@"16";

pub const EventLoop = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    loop: ?*uv.Loop = null,
    pending_operations: usize = 0,
    live_processes: usize = 0,
    processes: SlotTable(Process) = .{},
    is_shutting_down: bool = false,
    callback_failure: ?anyerror = null,
    streams: SlotTable(Stream) = .{},
    listeners: SlotTable(Listener) = .{},
    certificates: SlotTable(ServerCertificate) = .{},
    trust_store: TrustStore = .{},
    connection_pool: ConnectionPool = .{},
    http_servers: SlotTable(ServerState) = .{},
    compression_streams: SlotTable(CompressionStream) = .{},
    compression_workers: ?*WorkerDispatch = null,
    host_slots: HostSlots = .{},
    resources: ResourceRegistry = .{},
    stdin_stream_id: ?i64 = null,
    signal_subscriptions: SlotTable(Subscription) = .{},
    terminal_outputs: [2]?*TerminalOutput = .{ null, null },
    has_used_raw_terminal: bool = false,
    worker_dispatches: std.ArrayList(*WorkerDispatch) = .empty,
    sqlite: Databases = .{},
    local_timezone: LocalTimezoneLookup = .{},

    pub fn deinit(self: *Self) void {
        self.is_shutting_down = true;
        self.restoreTerminal();
        for (self.processes.entries.items) |entry| {
            const process = entry orelse continue;
            process.release();
        }
        while (self.live_processes > 0) {
            _ = uv.uv_run(self.loop.?, .once);
        }
        defer self.processes.deinit(self.allocator);
        self.sqlite.interruptAll();
        for (self.worker_dispatches.items) |dispatch| {
            dispatch.shutDown();
        }
        self.sqlite.deinit(self.allocator);
        defer self.worker_dispatches.deinit(self.allocator);
        for (self.signal_subscriptions.entries.items) |entry| {
            const subscription = entry orelse continue;
            subscription.close();
        }
        defer self.signal_subscriptions.deinit(self.allocator);
        for (self.terminal_outputs) |entry| {
            const output = entry orelse continue;
            output.close();
        }
        for (self.streams.entries.items) |entry| {
            const stream = entry orelse continue;
            stream.releaseAtExit();
        }
        for (self.listeners.entries.items) |entry| {
            const listener = entry orelse continue;
            listener.releaseAtExit();
        }
        defer self.streams.deinit(self.allocator);
        defer self.listeners.deinit(self.allocator);
        defer self.freeCertificates();
        defer self.connection_pool.deinit(self.allocator);
        defer self.freeHttpServers();
        defer self.freeCompressionStreams();
        defer self.host_slots.deinit(self.allocator);
        defer self.resources.deinit(self.allocator);
        defer self.local_timezone.deinit(self.allocator);
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

    pub fn startWorkerDispatch(self: *Self, options: WorkerPool.Options) !*WorkerDispatch {
        try self.worker_dispatches.ensureUnusedCapacity(self.allocator, 1);
        const dispatch = try WorkerDispatch.create(self, options);
        self.worker_dispatches.appendAssumeCapacity(dispatch);
        return dispatch;
    }

    pub fn watchSignal(self: *Self, scheduler: *Scheduler, signal: i64) !i64 {
        try self.signal_subscriptions.reserve(self.allocator);
        const subscription = try Subscription.create(self, scheduler, signal);
        return self.signal_subscriptions.addAssumeCapacity(subscription);
    }

    pub fn signalAt(self: *Self, handle: i64) ?*Subscription {
        return self.signal_subscriptions.get(handle);
    }

    pub fn closeSignal(self: *Self, handle: i64) void {
        const subscription = self.signal_subscriptions.remove(handle) orelse return;
        subscription.close();
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

    pub fn startCompressionJob(self: *Self, scheduler: *Scheduler, task: *TaskState, settings: CompressionJob.Settings, input: []const u8) !void {
        const dispatch = self.compression_workers orelse blk: {
            const started = try self.startWorkerDispatch(.{ .thread_count = 2, .max_outstanding = 128 });
            self.compression_workers = started;
            break :blk started;
        };
        const entry = try CompressionJob.create(self.allocator, settings, input);
        errdefer entry.destroy();
        entry.scheduler = scheduler;
        entry.task = task;
        try dispatch.submit(&entry.job);
    }

    pub fn parkForHostSlot(self: *Self, scheduler: *Scheduler, task: *TaskState, key: []const u8, timeout_ms: ?u64) !void {
        const loop = try self.ensureLoopStarted();
        const waiter = try self.allocator.create(SlotWaiter);
        errdefer self.allocator.destroy(waiter);
        const owned_key = try self.allocator.dupe(u8, key);
        errdefer self.allocator.free(owned_key);
        waiter.* = .{ .event_loop = self, .scheduler = scheduler, .task = task, .key = owned_key };
        try self.host_slots.addWaiter(self.allocator, key, waiter);
        const ms = timeout_ms orelse return;
        waiter.timer_memory = self.allocator.alignedAlloc(u8, handle_alignment, uv.uv_handle_size(.timer)) catch |failure| {
            self.host_slots.removeWaiter(self.allocator, key, waiter);
            return failure;
        };
        if (uv.uv_timer_init(loop, waiter.timer()) != 0) {
            self.host_slots.removeWaiter(self.allocator, key, waiter);
            self.allocator.free(waiter.timer_memory);
            waiter.timer_memory = &.{};
            return error.TimerFailed;
        }
        uv.uv_handle_set_data(waiter.timer(), &waiter.owner);
        uv.uv_update_time(loop);
        _ = uv.uv_timer_start(waiter.timer(), onSlotTimeout, ms, 0);
        self.pending_operations += 1;
    }

    pub fn releaseHostSlot(self: *Self, key: []const u8) void {
        const waiter = self.host_slots.release(self.allocator, key) orelse return;
        self.handSlotToWaiter(waiter);
    }

    fn handSlotToWaiter(self: *Self, waiter: *SlotWaiter) void {
        waiter.scheduler.wake(waiter.task, .{ .value = v.NULL_VALUE, .kind = .null }) catch |failure| {
            self.callback_failure = failure;
        };
        if (waiter.timer_memory.len == 0) {
            waiter.destroy();
            return;
        }
        _ = uv.uv_timer_stop(waiter.timer());
        self.pending_operations -= 1;
        uv.uv_close(waiter.timer(), onHandleClosed);
    }

    pub fn closeResource(self: *Self, id: v.ResourceId) void {
        const slot = self.resources.take(id) orelse return;
        switch (slot.kind) {
            .stream => {
                if (self.streamAt(slot.table_id)) |stream| {
                    stream.flushOutputBeforeClose();
                }
                self.closeStream(slot.table_id);
            },
            .listener => self.closeListener(slot.table_id),
            .compression => self.closeCompressionStream(slot.table_id),
            .process => {
                const process = self.processAt(slot.table_id) orelse return;
                process.releaseOnceExited();
            },
            .signal => self.closeSignal(slot.table_id),
            .certificate => self.closeCertificate(slot.table_id),
            .database => {
                _ = self.sqlite.close(self, null, null, slot.table_id) catch false;
            },
            .http_server => self.closeHttpServer(slot.table_id),
        }
    }

    pub fn openStdin(self: *Self, scheduler: *Scheduler, file: c_int) !i64 {
        if (self.stdin_stream_id) |stream_id| {
            return stream_id;
        }
        const loop = try self.ensureLoopStarted();
        try self.streams.reserve(self.allocator);
        const stream = try Stream.open(self.allocator, self, scheduler, loop, file);
        const stream_id = self.streams.addAssumeCapacity(stream);
        stream.id = stream_id;
        self.stdin_stream_id = stream_id;
        return stream_id;
    }

    pub fn openFileStream(self: *Self, scheduler: *Scheduler, file: c_int, size: i64, mode: FsJob.OpenMode) !i64 {
        const loop = try self.ensureLoopStarted();
        try self.streams.reserve(self.allocator);
        const stream = try Stream.open(self.allocator, self, scheduler, loop, file);
        stream.owns_file = true;
        stream.can_write_file = mode != .read;
        stream.is_append = mode == .append;
        stream.file_offset = if (mode == .append) size else 0;
        const stream_id = self.streams.addAssumeCapacity(stream);
        stream.id = stream_id;
        return stream_id;
    }

    pub fn openTcpStream(self: *Self, scheduler: *Scheduler) !i64 {
        const loop = try self.ensureLoopStarted();
        try self.streams.reserve(self.allocator);
        const stream = try Stream.openTcp(self.allocator, self, scheduler, loop);
        const stream_id = self.streams.addAssumeCapacity(stream);
        stream.id = stream_id;
        return stream_id;
    }

    pub fn openProcessPipe(self: *Self, scheduler: *Scheduler) !i64 {
        const loop = try self.ensureLoopStarted();
        try self.streams.reserve(self.allocator);
        const stream = try Stream.openProcessPipe(self.allocator, self, scheduler, loop);
        const stream_id = self.streams.addAssumeCapacity(stream);
        stream.id = stream_id;
        return stream_id;
    }

    pub fn restoreTerminal(self: *Self) void {
        if (!self.has_used_raw_terminal) return;
        _ = uv.uv_tty_reset_mode();
    }

    pub fn terminalSize(self: *Self, file: c_int) !?TerminalSize {
        const slot = &self.terminal_outputs[if (file == 2) 1 else 0];
        if (slot.* == null) {
            const loop = try self.ensureLoopStarted();
            slot.* = TerminalOutput.open(self.allocator, loop, file) catch |failure| {
                if (failure == error.OutOfMemory) {
                    return failure;
                }
                return null;
            };
        }
        return slot.*.?.size();
    }

    pub fn pauseStdinWhenIdle(self: *Self) void {
        const stream_id = self.stdin_stream_id orelse return;
        const stream = self.streamAt(stream_id) orelse return;
        stream.pauseReadingWhenIdle();
    }

    pub fn closeChildPipes(self: *Self) void {
        for (self.processes.entries.items) |entry| {
            const process = entry orelse continue;
            process.closePipes();
        }
    }

    pub fn processAt(self: *Self, handle: i64) ?*Process {
        const process = self.processes.get(handle) orelse return null;
        if (process.is_released) {
            return null;
        }
        return process;
    }

    pub fn openListener(self: *Self, scheduler: *Scheduler) !i64 {
        const loop = try self.ensureLoopStarted();
        try self.listeners.reserve(self.allocator);
        const listener = try Listener.create(self.allocator, self, scheduler, loop);
        const listener_id = self.listeners.addAssumeCapacity(listener);
        return listener_id;
    }

    pub fn addCertificate(self: *Self, certificate: *ServerCertificate) !i64 {
        try self.certificates.reserve(self.allocator);
        return self.certificates.addAssumeCapacity(certificate);
    }

    pub fn certificateAt(self: *Self, certificate_id: i64) ?*ServerCertificate {
        return self.certificates.get(certificate_id);
    }

    pub fn closeCertificate(self: *Self, certificate_id: i64) void {
        const certificate = self.certificates.remove(certificate_id) orelse return;
        certificate.release(self.allocator);
    }

    fn freeCertificates(self: *Self) void {
        for (self.certificates.entries.items) |entry| {
            const certificate = entry orelse continue;
            certificate.release(self.allocator);
        }
        self.certificates.deinit(self.allocator);
        self.trust_store.deinit(self.allocator);
    }

    pub fn closeListener(self: *Self, listener_id: i64) void {
        const listener = self.listeners.remove(listener_id) orelse return;
        listener.close();
    }

    pub fn listenerAt(self: *Self, listener_id: i64) ?*Listener {
        return self.listeners.get(listener_id);
    }

    pub fn closeStream(self: *Self, stream_id: i64) void {
        const stream = self.streams.remove(stream_id) orelse return;
        stream.close();
    }

    pub fn addCompressionStream(self: *Self, stream: *CompressionStream) !i64 {
        try self.compression_streams.reserve(self.allocator);
        return self.compression_streams.addAssumeCapacity(stream);
    }

    pub fn compressionStreamAt(self: *Self, compression_id: i64) ?*CompressionStream {
        return self.compression_streams.get(compression_id);
    }

    pub fn closeCompressionStream(self: *Self, compression_id: i64) void {
        const stream = self.compression_streams.remove(compression_id) orelse return;
        stream.destroy(self.allocator);
    }

    fn freeCompressionStreams(self: *Self) void {
        for (self.compression_streams.entries.items) |entry| {
            const stream = entry orelse continue;
            stream.destroy(self.allocator);
        }
        self.compression_streams.deinit(self.allocator);
    }

    pub fn addHttpServer(self: *Self) !i64 {
        try self.http_servers.reserve(self.allocator);
        const server = try self.allocator.create(ServerState);
        server.* = .{};
        return self.http_servers.addAssumeCapacity(server);
    }

    pub fn httpServerAt(self: *Self, server_id: i64) ?*ServerState {
        return self.http_servers.get(server_id);
    }

    pub fn closeHttpServer(self: *Self, server_id: i64) void {
        const server = self.http_servers.remove(server_id) orelse return;
        defer {
            server.deinit(self.allocator);
            self.allocator.destroy(server);
        }
        for (server.idle_streams.keys()) |stream_id| {
            self.closeStream(stream_id);
        }
    }

    fn freeHttpServers(self: *Self) void {
        for (self.http_servers.entries.items) |entry| {
            const server = entry orelse continue;
            server.deinit(self.allocator);
            self.allocator.destroy(server);
        }
        self.http_servers.deinit(self.allocator);
    }

    pub fn streamAt(self: *Self, stream_id: i64) ?*Stream {
        return self.streams.get(stream_id);
    }

    pub fn ensureLoopStarted(self: *Self) !*uv.Loop {
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
        return self.pending_operations > 0 or self.live_processes > 0;
    }
};

pub const HandleOwner = struct {
    kind: Kind,

    pub const Kind = enum { process, process_lifetime_timer, process_wait_timer, process_pipe, signal, timer, stream, stream_timer, stream_handshake_timer, stream_write_timer, listener, listener_timer, open_timer, slot_timer, terminal_output, worker_async, sqlite_timer };

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
        .process => {
            const process: *Process = @alignCast(@fieldParentPtr("owner", owner));
            process.markHandleClosed();
        },
        .process_lifetime_timer => {
            const process: *Process = @alignCast(@fieldParentPtr("lifetime_timer_owner", owner));
            process.markHandleClosed();
        },
        .process_wait_timer => {
            const process: *Process = @alignCast(@fieldParentPtr("wait_timer_owner", owner));
            process.markHandleClosed();
        },
        .process_pipe => process_runtime.captureHandleClosed(owner),
        .signal => {
            const subscription: *Subscription = @alignCast(@fieldParentPtr("owner", owner));
            subscription.destroy();
        },
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
        .terminal_output => {
            const output: *TerminalOutput = @alignCast(@fieldParentPtr("owner", owner));
            output.destroy();
        },
        .stream_timer => {
            const stream: *Stream = @alignCast(@fieldParentPtr("timer_owner", owner));
            stream.markHandleClosed();
        },
        .stream_handshake_timer => {
            const stream: *Stream = @alignCast(@fieldParentPtr("handshake_timer_owner", owner));
            stream.markHandleClosed();
        },
        .stream_write_timer => {
            const stream: *Stream = @alignCast(@fieldParentPtr("write_timer_owner", owner));
            stream.markHandleClosed();
        },
        .listener => {
            const listener: *Listener = @alignCast(@fieldParentPtr("owner", owner));
            listener.markHandleClosed();
        },
        .listener_timer => {
            const listener: *Listener = @alignCast(@fieldParentPtr("timer_owner", owner));
            listener.markHandleClosed();
        },
        .open_timer => {
            const request: *OpenRequest = @alignCast(@fieldParentPtr("owner", owner));
            request.markTimerClosed();
        },
        .worker_async => {
            const dispatch: *WorkerDispatch = @alignCast(@fieldParentPtr("owner", owner));
            dispatch.destroy();
        },
        .slot_timer => {
            const waiter: *SlotWaiter = @alignCast(@fieldParentPtr("owner", owner));
            waiter.event_loop.host_slots.removeWaiter(waiter.event_loop.allocator, waiter.key, waiter);
            waiter.destroy();
        },
        .sqlite_timer => {
            const timer: *SqliteTimer = @alignCast(@fieldParentPtr("owner", owner));
            timer.destroy();
        },
    }
}

pub const SlotWaiter = struct {
    owner: HandleOwner = .{ .kind = .slot_timer },
    event_loop: *EventLoop,
    scheduler: *Scheduler,
    task: *TaskState,
    key: []u8,
    timer_memory: []align(16) u8 = &.{},

    fn timer(self: *SlotWaiter) *uv.Handle {
        return @ptrCast(self.timer_memory.ptr);
    }

    pub fn destroy(self: *SlotWaiter) void {
        const allocator = self.event_loop.allocator;
        if (self.timer_memory.len > 0) {
            allocator.free(self.timer_memory);
        }
        allocator.free(self.key);
        allocator.destroy(self);
    }
};

fn onSlotTimeout(handle: *uv.Handle) callconv(.c) void {
    const waiter: *SlotWaiter = @alignCast(@fieldParentPtr("owner", HandleOwner.of(handle)));
    const event_loop = waiter.event_loop;
    event_loop.host_slots.removeWaiter(event_loop.allocator, waiter.key, waiter);
    event_loop.pending_operations -= 1;
    const failure: WakeResult = .{ .value = .{ .int64 = @intFromEnum(HttpError.TimedOut) }, .kind = .int64, .has_failed = true, .error_is_relative = true };
    waiter.scheduler.wake(waiter.task, failure) catch |wake_failure| {
        event_loop.callback_failure = wake_failure;
    };
    uv.uv_close(handle, onHandleClosed);
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
    if (event_loop.is_shutting_down) {
        if (job.kind == .open_file and job.failure == null and job.file >= 0) {
            job.closeFile(job.file);
        }
        if (job.stream) |stream| {
            if (job.kind == .file_size) {
                stream.finishRequest();
            } else if (job.kind == .write_chunk) {
                stream.finishOutput(job.written_bytes, job.failure != null);
            } else {
                stream.finishChunk(job.takeContents(), job.failure != null);
            }
        }
        return;
    }
    if (job.stream) |stream| {
        if (job.kind == .write_chunk) {
            stream.finishOutput(job.written_bytes, job.failure != null);
            return;
        }
        if (job.kind == .read_chunk) {
            stream.finishChunk(job.takeContents(), job.failure != null);
            return;
        }
        stream.finishRequest();
    }
    const wake_result = job.takeWakeResult();
    job.scheduler.wake(job.task.?, wake_result) catch |failure| {
        wake_result.payload.free(event_loop.allocator);
        event_loop.callback_failure = failure;
    };
}
