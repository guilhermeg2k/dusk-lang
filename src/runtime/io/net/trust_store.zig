const std = @import("std");
const builtin = @import("builtin");
const uv = @import("../../tasks/loop/uv.zig");
const tls = @import("tls.zig");
const tls_layer = @import("tls_layer.zig");
const EventLoop = @import("../../tasks/event_loop.zig").EventLoop;
const TaskState = @import("../../tasks/scheduler.zig").TaskState;
const Scheduler = @import("../../tasks/scheduler.zig").Scheduler;
const WakeResult = @import("../../tasks/scheduler.zig").WakeResult;
const NetError = @import("tcp.zig").NetError;
const v = @import("../../value.zig");
const Bundle = std.crypto.Certificate.Bundle;
const der = std.crypto.Certificate.der;

const handle_alignment = std.mem.Alignment.@"16";
const env_path_separator = if (builtin.os.tag == .windows) ';' else ':';

pub const TrustStore = struct {
    const Self = @This();

    state: enum { idle, loading, ready } = .idle,
    config: ?*tls.Config = null,
    scheduler: ?*Scheduler = null,
    waiters: std.ArrayList(Waiter) = .empty,

    pub const Waiter = struct {
        task: *TaskState,
        stream_id: i64,
        host: []u8,
        timeout_ms: ?u64,
        started_at_ms: u64,
        is_tunneled: bool,
    };

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        for (self.waiters.items) |waiter| {
            allocator.free(waiter.host);
        }
        self.waiters.deinit(allocator);
        if (self.config) |config| {
            tls.dusk_tls_config_free(config);
        }
    }

    pub fn parkStarter(self: *Self, event_loop: *EventLoop, scheduler: *Scheduler, task: *TaskState, stream_id: i64, host: []const u8, timeout_ms: ?u64, is_tunneled: bool) !void {
        const allocator = event_loop.allocator;
        self.scheduler = scheduler;
        const loop = try event_loop.ensureLoopStarted();
        if (self.state == .idle) {
            try LoadJob.start(event_loop);
            self.state = .loading;
        }
        const host_copy = try allocator.dupe(u8, host);
        errdefer allocator.free(host_copy);
        uv.uv_update_time(loop);
        try self.waiters.append(allocator, .{
            .task = task,
            .stream_id = stream_id,
            .host = host_copy,
            .timeout_ms = timeout_ms,
            .started_at_ms = uv.uv_now(loop),
            .is_tunneled = is_tunneled,
        });
        event_loop.pending_operations += 1;
    }

    fn finishLoading(self: *Self, event_loop: *EventLoop, bundle: *const Bundle) void {
        self.config = newSystemConfig(bundle);
        self.state = if (self.config == null) .idle else .ready;
        var waiters = self.waiters;
        self.waiters = .empty;
        defer waiters.deinit(event_loop.allocator);
        for (waiters.items) |waiter| {
            defer event_loop.allocator.free(waiter.host);
            event_loop.pending_operations -= 1;
            const wake_result = continueStarter(event_loop, self.config, waiter) orelse continue;
            self.scheduler.?.wake(waiter.task, wake_result) catch |failure| {
                event_loop.callback_failure = failure;
            };
        }
    }
};

fn continueStarter(event_loop: *EventLoop, config: ?*tls.Config, waiter: TrustStore.Waiter) ?WakeResult {
    const stream = event_loop.streamAt(waiter.stream_id) orelse return netFailure(.HandshakeFailed);
    stream.is_starting_tls = false;
    const shared_config = config orelse return netFailure(.Other);
    const remaining_timeout: ?u64 = if (waiter.timeout_ms) |ms| blk: {
        const loop = event_loop.loop orelse break :blk ms;
        uv.uv_update_time(loop);
        const elapsed = uv.uv_now(loop) -| waiter.started_at_ms;
        if (elapsed >= ms) {
            return netFailure(.TimedOut);
        }
        break :blk ms - elapsed;
    } else null;
    const wait = stream.startClientTls(waiter.task, shared_config, false, waiter.host, remaining_timeout, waiter.is_tunneled) catch return netFailure(.Other);
    return switch (wait) {
        .parked => null,
        .done => .{ .value = v.NULL_VALUE, .kind = .null },
        .failure => |failure| netFailure(failure),
    };
}

fn newSystemConfig(bundle: *const Bundle) ?*tls.Config {
    tls_layer.ensureTlsReady() catch return null;
    const config = tls.dusk_tls_client_config_new(1) orelse return null;
    var offsets = bundle.map.valueIterator();
    while (offsets.next()) |offset| {
        const element = der.Element.parse(bundle.bytes.items, offset.*) catch continue;
        const certificate = bundle.bytes.items[offset.*..element.slice.end];
        _ = tls.dusk_tls_config_add_ca_der(config, certificate.ptr, certificate.len);
    }
    return config;
}

fn netFailure(failure: NetError) WakeResult {
    return .{ .value = .{ .int64 = @intFromEnum(failure) }, .kind = .int64, .has_failed = true, .error_is_relative = true };
}

const LoadJob = struct {
    event_loop: *EventLoop,
    work_memory: []align(16) u8,
    cert_file: ?[]u8,
    cert_dirs: ?[]u8,
    bundle: Bundle = .empty,

    fn start(event_loop: *EventLoop) !void {
        const allocator = event_loop.allocator;
        const loop = try event_loop.ensureLoopStarted();
        const job = try allocator.create(LoadJob);
        errdefer allocator.destroy(job);
        const memory = try allocator.alignedAlloc(u8, handle_alignment, uv.uv_req_size(.work));
        job.* = .{ .event_loop = event_loop, .work_memory = memory, .cert_file = copyEnv(allocator, "SSL_CERT_FILE"), .cert_dirs = copyEnv(allocator, "SSL_CERT_DIR") };
        uv.uv_req_set_data(job.workRequest(), job);
        if (uv.uv_queue_work(loop, job.workRequest(), onLoadWork, onLoadDone) != 0) {
            job.destroy();
            return error.PoolJobFailed;
        }
        event_loop.pending_operations += 1;
    }

    fn destroy(self: *LoadJob) void {
        const allocator = self.event_loop.allocator;
        self.bundle.deinit(allocator);
        if (self.cert_file) |path| {
            allocator.free(path);
        }
        if (self.cert_dirs) |paths| {
            allocator.free(paths);
        }
        allocator.free(self.work_memory);
        allocator.destroy(self);
    }

    fn workRequest(self: *LoadJob) *uv.Request {
        return @ptrCast(self.work_memory.ptr);
    }

    fn loadOnPoolThread(self: *LoadJob) void {
        const allocator = self.event_loop.allocator;
        var threaded: std.Io.Threaded = .init(allocator, .{});
        defer threaded.deinit();
        const io = threaded.io();
        const now = std.Io.Clock.real.now(io);
        const has_override = self.cert_file != null or self.cert_dirs != null;
        if (!has_override) {
            self.bundle.rescan(allocator, io, now) catch {};
            return;
        }
        if (self.cert_file) |path| {
            addFile(&self.bundle, allocator, io, now, path);
        }
        const dirs = self.cert_dirs orelse return;
        var paths = std.mem.splitScalar(u8, dirs, env_path_separator);
        while (paths.next()) |path| {
            if (path.len == 0) continue;
            addDir(&self.bundle, allocator, io, now, path);
        }
    }
};

fn copyEnv(allocator: std.mem.Allocator, name: [*:0]const u8) ?[]u8 {
    const value = std.c.getenv(name) orelse return null;
    const text = std.mem.span(value);
    if (text.len == 0) {
        return null;
    }
    return allocator.dupe(u8, text) catch null;
}

fn addFile(bundle: *Bundle, allocator: std.mem.Allocator, io: std.Io, now: std.Io.Timestamp, path: []const u8) void {
    if (std.Io.Dir.path.isAbsolute(path)) {
        bundle.addCertsFromFilePathAbsolute(allocator, io, now, path) catch {};
        return;
    }
    bundle.addCertsFromFilePath(allocator, io, now, std.Io.Dir.cwd(), path) catch {};
}

fn addDir(bundle: *Bundle, allocator: std.mem.Allocator, io: std.Io, now: std.Io.Timestamp, path: []const u8) void {
    if (std.Io.Dir.path.isAbsolute(path)) {
        bundle.addCertsFromDirPathAbsolute(allocator, io, now, path) catch {};
        return;
    }
    bundle.addCertsFromDirPath(allocator, io, std.Io.Dir.cwd(), path) catch {};
}

fn loadJobOf(work: *uv.Request) *LoadJob {
    return @ptrCast(@alignCast(uv.uv_req_get_data(work).?));
}

fn onLoadWork(work: *uv.Request) callconv(.c) void {
    loadJobOf(work).loadOnPoolThread();
}

fn onLoadDone(work: *uv.Request, _: c_int) callconv(.c) void {
    const job = loadJobOf(work);
    defer job.destroy();
    const event_loop = job.event_loop;
    event_loop.pending_operations -= 1;
    if (event_loop.is_shutting_down) return;
    event_loop.trust_store.finishLoading(event_loop, &job.bundle);
}
