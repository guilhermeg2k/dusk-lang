const std = @import("std");
const builtin = @import("builtin");
const uv = @import("uv.zig");
const EventLoop = @import("event_loop.zig").EventLoop;
const HandleOwner = @import("event_loop.zig").HandleOwner;
const onHandleClosed = @import("event_loop.zig").onHandleClosed;
const Scheduler = @import("scheduler.zig").Scheduler;
const TaskState = @import("scheduler.zig").TaskState;
const WakeResult = @import("scheduler.zig").WakeResult;
const Stream = @import("stream.zig").Stream;
const IoError = @import("stream.zig").IoError;
const ServerCertificate = @import("tls_layer.zig").ServerCertificate;

const handle_alignment = std.mem.Alignment.@"16";
const max_ip_length = 64;
const max_host_length = 255;
const max_ready_connections = 64;

pub const NetError = enum {
    HostNotFound,
    ConnectionRefused,
    AddressInUse,
    NetworkUnreachable,
    PermissionDenied,
    InvalidAddress,
    TimedOut,
    HandshakeFailed,
    UntrustedCertificate,
    ExpiredCertificate,
    WrongHostCertificate,
    InvalidCertificate,
    Other,
};

const net_errors_by_name = std.StaticStringMap(NetError).initComptime(.{
    .{ "ECONNREFUSED", .ConnectionRefused },
    .{ "EADDRINUSE", .AddressInUse },
    .{ "EADDRNOTAVAIL", .InvalidAddress },
    .{ "EAFNOSUPPORT", .InvalidAddress },
    .{ "EINVAL", .InvalidAddress },
    .{ "ENETUNREACH", .NetworkUnreachable },
    .{ "EHOSTUNREACH", .NetworkUnreachable },
    .{ "EACCES", .PermissionDenied },
    .{ "EPERM", .PermissionDenied },
    .{ "ETIMEDOUT", .TimedOut },
    .{ "EAI_NONAME", .HostNotFound },
    .{ "EAI_NODATA", .HostNotFound },
    .{ "EAI_FAIL", .HostNotFound },
    .{ "EAI_AGAIN", .HostNotFound },
    .{ "EAI_ADDRFAMILY", .HostNotFound },
});

pub const Purpose = union(enum) {
    connect,
    listen: c_int,
};

pub const OpenProgress = union(enum) {
    waiting,
    opened: i64,
    failed: NetError,
};

pub fn open(event_loop: *EventLoop, scheduler: *Scheduler, task: *TaskState, purpose: Purpose, host: []const u8, port: u16, timeout_ms: ?u64) !OpenProgress {
    const request = try OpenRequest.create(event_loop, scheduler, task, purpose, port);
    if (timeout_ms) |ms| {
        request.startTimer(ms) catch |failure| {
            request.finishWithoutWaking();
            return failure;
        };
    }
    const progress = request.start(host);
    if (progress == .waiting) {
        event_loop.pending_operations += 1;
        return progress;
    }
    request.finishWithoutWaking();
    return progress;
}

pub const OpenRequest = struct {
    const Self = @This();

    owner: HandleOwner = .{ .kind = .open_timer },
    allocator: std.mem.Allocator,
    event_loop: *EventLoop,
    scheduler: *Scheduler,
    task: *TaskState,
    purpose: Purpose,
    port: u16,
    addresses: std.ArrayList(uv.SocketAddress) = .empty,
    next_address: usize = 0,
    last_failure: NetError = .HostNotFound,
    attempt: ?Attempt = null,
    connect_memory: []align(16) u8 = &.{},
    lookup_memory: []align(16) u8 = &.{},
    timer_memory: []align(16) u8 = &.{},
    timer_state: enum { none, open, closing } = .none,
    is_looking_up: bool = false,
    is_finished: bool = false,

    const Attempt = struct {
        handle: i64,
        stream: *Stream,
    };

    fn create(event_loop: *EventLoop, scheduler: *Scheduler, task: *TaskState, purpose: Purpose, port: u16) !*Self {
        const allocator = event_loop.allocator;
        const request = try allocator.create(Self);
        request.* = .{ .allocator = allocator, .event_loop = event_loop, .scheduler = scheduler, .task = task, .purpose = purpose, .port = port };
        return request;
    }

    fn destroy(self: *Self) void {
        const allocator = self.allocator;
        self.addresses.deinit(allocator);
        allocator.free(self.connect_memory);
        allocator.free(self.lookup_memory);
        allocator.free(self.timer_memory);
        allocator.destroy(self);
    }

    fn startTimer(self: *Self, ms: u64) !void {
        const loop = try self.event_loop.ensureLoopStarted();
        const memory = try self.allocator.alignedAlloc(u8, handle_alignment, uv.uv_handle_size(.timer));
        if (uv.uv_timer_init(loop, @ptrCast(memory.ptr)) != 0) {
            self.allocator.free(memory);
            return error.TimerFailed;
        }
        self.timer_memory = memory;
        self.timer_state = .open;
        uv.uv_handle_set_data(self.timerHandle(), &self.owner);
        uv.uv_update_time(loop);
        if (uv.uv_timer_start(self.timerHandle(), onOpenTimeout, ms, 0) != 0) {
            return error.TimerFailed;
        }
    }

    fn start(self: *Self, host: []const u8) OpenProgress {
        const address = parseIpAddress(host, self.port) orelse return self.startLookup(host);
        self.addresses.append(self.allocator, address) catch return .{ .failed = .Other };
        return self.openNextAddress();
    }

    fn startLookup(self: *Self, host: []const u8) OpenProgress {
        const is_valid_host = host.len > 0 and host.len <= max_host_length and std.mem.indexOfScalar(u8, host, 0) == null;
        if (!is_valid_host) {
            return .{ .failed = .InvalidAddress };
        }
        const loop = self.event_loop.ensureLoopStarted() catch return .{ .failed = .Other };
        self.lookup_memory = self.allocator.alignedAlloc(u8, handle_alignment, uv.uv_req_size(.getaddrinfo)) catch return .{ .failed = .Other };
        uv.uv_req_set_data(self.lookupRequest(), self);
        var node: [max_host_length:0]u8 = @splat(0);
        @memcpy(node[0..host.len], host);
        var service: [8:0]u8 = @splat(0);
        _ = std.fmt.bufPrint(&service, "{d}", .{self.port}) catch return .{ .failed = .Other };
        const hints = std.mem.zeroInit(uv.AddressInfo, .{ .family = std.c.AF.UNSPEC, .socktype = std.c.SOCK.STREAM });
        const status = uv.uv_getaddrinfo(loop, self.lookupRequest(), onLookup, &node, &service, &hints);
        if (status != 0) {
            return .{ .failed = netErrorFromStatus(status) };
        }
        self.is_looking_up = true;
        return .waiting;
    }

    fn openNextAddress(self: *Self) OpenProgress {
        while (self.next_address < self.addresses.items.len) {
            const address = &self.addresses.items[self.next_address];
            self.next_address += 1;
            const progress = switch (self.purpose) {
                .connect => self.startConnecting(address),
                .listen => |backlog| self.listenOn(address, backlog),
            };
            switch (progress) {
                .failed => |failure| self.last_failure = failure,
                else => return progress,
            }
        }
        return .{ .failed = self.last_failure };
    }

    fn startConnecting(self: *Self, address: *const uv.SocketAddress) OpenProgress {
        if (self.connect_memory.len == 0) {
            self.connect_memory = self.allocator.alignedAlloc(u8, handle_alignment, uv.uv_req_size(.connect)) catch return .{ .failed = .Other };
            uv.uv_req_set_data(self.connectRequest(), self);
        }
        const handle = self.event_loop.openTcpStream(self.scheduler) catch return .{ .failed = .Other };
        const stream = self.event_loop.streamAt(handle).?;
        const status = uv.uv_tcp_connect(self.connectRequest(), stream.uvHandle(), address, onConnect);
        if (status != 0) {
            self.event_loop.closeStream(handle);
            return .{ .failed = netErrorFromStatus(status) };
        }
        stream.requests_in_flight += 1;
        self.attempt = .{ .handle = handle, .stream = stream };
        return .waiting;
    }

    fn listenOn(self: *Self, address: *const uv.SocketAddress, backlog: c_int) OpenProgress {
        const handle = self.event_loop.openListener(self.scheduler) catch return .{ .failed = .Other };
        const listener = self.event_loop.listenerAt(handle).?;
        if (listener.bindAndListen(address, backlog)) |failure| {
            self.event_loop.closeListener(handle);
            return .{ .failed = failure };
        }
        return .{ .opened = handle };
    }

    fn continueOpening(self: *Self) void {
        switch (self.openNextAddress()) {
            .waiting => {},
            .opened => |handle| self.finish(handleResult(handle)),
            .failed => |failure| self.finish(netFailure(failure)),
        }
    }

    fn finish(self: *Self, wake_result: WakeResult) void {
        self.event_loop.pending_operations -= 1;
        self.scheduler.wake(self.task, wake_result) catch |failure| {
            self.event_loop.callback_failure = failure;
        };
        self.finishWithoutWaking();
    }

    fn finishWithoutWaking(self: *Self) void {
        self.is_finished = true;
        if (self.timer_state == .open) {
            self.timer_state = .closing;
            uv.uv_close(self.timerHandle(), onHandleClosed);
        }
        self.destroyIfDone();
    }

    fn isAbandoned(self: *const Self) bool {
        return self.is_finished or self.event_loop.is_shutting_down;
    }

    fn abandon(self: *Self) void {
        self.is_finished = true;
        self.destroyIfDone();
    }

    pub fn markTimerClosed(self: *Self) void {
        self.timer_state = .none;
        self.destroyIfDone();
    }

    fn destroyIfDone(self: *Self) void {
        const is_done = self.is_finished and !self.is_looking_up and self.attempt == null and self.timer_state == .none;
        if (is_done) {
            self.destroy();
        }
    }

    fn timerHandle(self: *Self) *uv.Handle {
        return @ptrCast(self.timer_memory.ptr);
    }

    fn lookupRequest(self: *Self) *uv.Request {
        return @ptrCast(self.lookup_memory.ptr);
    }

    fn connectRequest(self: *Self) *uv.Request {
        return @ptrCast(self.connect_memory.ptr);
    }
};

fn openRequestOf(uv_request: *uv.Request) *OpenRequest {
    return @ptrCast(@alignCast(uv.uv_req_get_data(uv_request).?));
}

fn onLookup(uv_request: *uv.Request, status: c_int, result: ?*uv.AddressInfo) callconv(.c) void {
    defer uv.uv_freeaddrinfo(result);
    const request = openRequestOf(uv_request);
    request.is_looking_up = false;
    if (request.isAbandoned()) {
        request.abandon();
        return;
    }
    if (status != 0) {
        request.finish(netFailure(netErrorFromStatus(status)));
        return;
    }
    var entry = result;
    while (entry) |info| : (entry = info.next) {
        const source = info.addr orelse continue;
        const length = @min(@as(usize, @intCast(info.addrlen)), @sizeOf(uv.SocketAddress));
        var address: uv.SocketAddress = .{};
        @memcpy(address.bytes[0..length], @as([*]const u8, @ptrCast(source))[0..length]);
        request.addresses.append(request.allocator, address) catch {
            request.finish(netFailure(.Other));
            return;
        };
    }
    request.continueOpening();
}

fn onConnect(uv_request: *uv.Request, status: c_int) callconv(.c) void {
    const request = openRequestOf(uv_request);
    const attempt = request.attempt.?;
    request.attempt = null;
    const was_closed = attempt.stream.is_closed;
    if (was_closed) {
        attempt.stream.finishRequest();
    } else {
        attempt.stream.requests_in_flight -= 1;
    }
    if (request.isAbandoned()) {
        if (!was_closed) {
            request.event_loop.closeStream(@intCast(attempt.handle));
        }
        request.abandon();
        return;
    }
    if (status == 0) {
        attempt.stream.rememberPeer();
        request.finish(handleResult(attempt.handle));
        return;
    }
    request.event_loop.closeStream(@intCast(attempt.handle));
    request.last_failure = netErrorFromStatus(status);
    request.continueOpening();
}

fn onOpenTimeout(timer: *uv.Handle) callconv(.c) void {
    const request: *OpenRequest = @alignCast(@fieldParentPtr("owner", HandleOwner.of(timer)));
    if (request.is_finished) return;
    if (request.is_looking_up) {
        _ = uv.uv_cancel(request.lookupRequest());
    }
    if (request.attempt) |attempt| {
        request.event_loop.closeStream(@intCast(attempt.handle));
    }
    request.finish(netFailure(.TimedOut));
}

pub const Listener = struct {
    const Self = @This();

    owner: HandleOwner = .{ .kind = .listener },
    timer_owner: HandleOwner = .{ .kind = .listener_timer },
    allocator: std.mem.Allocator,
    event_loop: *EventLoop,
    scheduler: *Scheduler,
    loop: *uv.Loop,
    handle_memory: []align(16) u8,
    timer_memory: []align(16) u8 = &.{},
    open_handles: u8 = 1,
    is_timer_running: bool = false,
    is_closed: bool = false,
    ready_connections: std.ArrayList(i64) = .empty,
    has_unaccepted_connection: bool = false,
    failure: ?IoError = null,
    waiter: ?*TaskState = null,
    tls_certificate: ?*ServerCertificate = null,

    pub const AcceptOutcome = union(enum) {
        connection: i64,
        failure: IoError,
    };

    pub fn create(allocator: std.mem.Allocator, event_loop: *EventLoop, scheduler: *Scheduler, loop: *uv.Loop) !*Self {
        const listener = try allocator.create(Self);
        errdefer allocator.destroy(listener);
        const memory = try allocator.alignedAlloc(u8, handle_alignment, uv.uv_handle_size(.tcp));
        errdefer allocator.free(memory);
        if (uv.uv_tcp_init(loop, @ptrCast(memory.ptr)) != 0) {
            return error.TcpInitFailed;
        }
        listener.* = .{ .allocator = allocator, .event_loop = event_loop, .scheduler = scheduler, .loop = loop, .handle_memory = memory };
        uv.uv_handle_set_data(listener.uvHandle(), &listener.owner);
        return listener;
    }

    pub fn destroy(self: *Self) void {
        self.ready_connections.deinit(self.allocator);
        self.allocator.free(self.handle_memory);
        self.allocator.free(self.timer_memory);
        self.allocator.destroy(self);
    }

    pub fn bindAndListen(self: *Self, address: *const uv.SocketAddress, backlog: c_int) ?NetError {
        const bind_status = uv.uv_tcp_bind(self.uvHandle(), address, 0);
        if (bind_status != 0) {
            return netErrorFromStatus(bind_status);
        }
        const listen_status = uv.uv_listen(self.uvHandle(), backlog, onConnection);
        if (listen_status != 0) {
            return netErrorFromStatus(listen_status);
        }
        return null;
    }

    pub fn port(self: *Self) i64 {
        var address: uv.SocketAddress = .{};
        var length: c_int = @sizeOf(uv.SocketAddress);
        if (uv.uv_tcp_getsockname(self.uvHandle(), &address, &length) != 0) {
            return 0;
        }
        return addressPort(&address);
    }

    pub fn takeReadyConnection(self: *Self) ?AcceptOutcome {
        if (self.ready_connections.items.len == 0) {
            self.queueReadyConnections();
        }
        if (self.failure) |failure| {
            self.failure = null;
            return .{ .failure = failure };
        }
        if (self.ready_connections.items.len == 0) {
            return null;
        }
        return .{ .connection = self.ready_connections.orderedRemove(0) };
    }

    fn queueReadyConnections(self: *Self) void {
        if (!self.has_unaccepted_connection) return;
        while (self.ready_connections.items.len < max_ready_connections) {
            self.ready_connections.ensureUnusedCapacity(self.allocator, 1) catch {
                self.failure = .Other;
                return;
            };
            if (self.has_unaccepted_connection) {
                self.has_unaccepted_connection = false;
                const handle = self.acceptConnection() catch {
                    self.failure = .Other;
                    return;
                };
                self.ready_connections.appendAssumeCapacity(handle);
                continue;
            }
            const handle = self.acceptNextFromSocket() orelse return;
            self.ready_connections.appendAssumeCapacity(handle);
        }
    }

    fn acceptConnection(self: *Self) !i64 {
        const handle = try self.event_loop.openTcpStream(self.scheduler);
        const stream = self.event_loop.streamAt(handle).?;
        if (uv.uv_accept(self.uvHandle(), stream.uvHandle()) != 0) {
            self.event_loop.closeStream(handle);
            return error.AcceptFailed;
        }
        try self.prepareAcceptedStream(handle);
        return handle;
    }

    fn acceptNextFromSocket(self: *Self) ?i64 {
        if (builtin.os.tag == .windows) {
            return null;
        }
        var listener_socket: uv.OsFile = undefined;
        if (uv.uv_fileno(self.uvHandle(), &listener_socket) != 0) {
            return null;
        }
        const socket = acceptWaitingSocket(listener_socket) orelse return null;
        const handle = self.event_loop.openTcpStream(self.scheduler) catch {
            _ = std.c.close(socket);
            return null;
        };
        const stream = self.event_loop.streamAt(handle).?;
        if (uv.uv_tcp_open(stream.uvHandle(), socket) != 0) {
            _ = std.c.close(socket);
            self.event_loop.closeStream(handle);
            return null;
        }
        self.prepareAcceptedStream(handle) catch return null;
        return handle;
    }

    fn prepareAcceptedStream(self: *Self, handle: i64) !void {
        const stream = self.event_loop.streamAt(handle).?;
        stream.rememberPeer();
        const certificate = self.tls_certificate orelse return;
        stream.useServerTls(certificate.config) catch {
            self.event_loop.closeStream(@intCast(handle));
            return error.AcceptFailed;
        };
    }

    pub fn parkAccepter(self: *Self, task: *TaskState, timeout_ms: ?u64) !void {
        if (timeout_ms) |ms| {
            try self.startTimer(ms);
        }
        self.waiter = task;
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
        if (uv.uv_timer_start(self.timerHandle(), onAcceptTimeout, ms, 0) != 0) {
            return error.TimerFailed;
        }
        self.is_timer_running = true;
    }

    fn stopTimer(self: *Self) void {
        if (!self.is_timer_running) return;
        _ = uv.uv_timer_stop(self.timerHandle());
        self.is_timer_running = false;
    }

    pub fn close(self: *Self) void {
        if (self.waiter) |task| {
            self.waiter = null;
            self.stopTimer();
            self.event_loop.pending_operations -= 1;
            self.scheduler.wake(task, ioFailure(.Closed)) catch |failure| {
                self.event_loop.callback_failure = failure;
            };
        }
        for (self.ready_connections.items) |handle| {
            self.event_loop.closeStream(handle);
        }
        self.ready_connections.clearRetainingCapacity();
        self.is_closed = true;
        uv.uv_close(self.uvHandle(), onHandleClosed);
        if (self.timer_memory.len > 0) {
            uv.uv_close(self.timerHandle(), onHandleClosed);
        }
    }

    pub fn releaseAtExit(self: *Self) void {
        self.is_closed = true;
        self.releaseIfDone();
    }

    pub fn markHandleClosed(self: *Self) void {
        self.open_handles -= 1;
        self.releaseIfDone();
    }

    fn releaseIfDone(self: *Self) void {
        const is_done = self.is_closed and self.open_handles == 0;
        if (is_done) {
            self.destroy();
        }
    }

    fn serveWaiter(self: *Self) void {
        const task = self.waiter orelse return;
        const outcome = self.takeReadyConnection() orelse return;
        const wake_result: WakeResult = switch (outcome) {
            .connection => |handle| handleResult(handle),
            .failure => |failure| ioFailure(failure),
        };
        self.waiter = null;
        self.stopTimer();
        self.event_loop.pending_operations -= 1;
        self.scheduler.wake(task, wake_result) catch |failure| {
            self.event_loop.callback_failure = failure;
        };
    }

    fn uvHandle(self: *Self) *uv.Handle {
        return @ptrCast(self.handle_memory.ptr);
    }

    fn timerHandle(self: *Self) *uv.Handle {
        return @ptrCast(self.timer_memory.ptr);
    }
};

fn onConnection(server: *uv.Handle, status: c_int) callconv(.c) void {
    const listener: *Listener = @alignCast(@fieldParentPtr("owner", HandleOwner.of(server)));
    if (status < 0) {
        listener.failure = .Other;
    } else {
        listener.has_unaccepted_connection = true;
    }
    listener.serveWaiter();
}

fn acceptWaitingSocket(listener_socket: c_int) ?c_int {
    while (true) {
        const socket = acceptSocketWithFlags(listener_socket);
        if (socket >= 0) {
            return socket;
        }
        switch (std.posix.errno(socket)) {
            .INTR, .CONNABORTED => continue,
            else => return null,
        }
    }
}

fn acceptSocketWithFlags(listener_socket: c_int) c_int {
    const has_accept4 = builtin.os.tag == .linux or builtin.os.tag == .freebsd;
    if (has_accept4) {
        return std.c.accept4(listener_socket, null, null, std.posix.SOCK.NONBLOCK | std.posix.SOCK.CLOEXEC);
    }
    const socket = std.c.accept(listener_socket, null, null);
    if (socket >= 0) {
        _ = std.c.fcntl(socket, std.posix.F.SETFD, @as(c_int, std.posix.FD_CLOEXEC));
    }
    return socket;
}

fn onAcceptTimeout(timer: *uv.Handle) callconv(.c) void {
    const listener: *Listener = @alignCast(@fieldParentPtr("timer_owner", HandleOwner.of(timer)));
    listener.is_timer_running = false;
    const task = listener.waiter orelse return;
    listener.waiter = null;
    listener.event_loop.pending_operations -= 1;
    listener.scheduler.wake(task, ioFailure(.TimedOut)) catch |failure| {
        listener.event_loop.callback_failure = failure;
    };
}

pub fn addressHost(address: *const uv.SocketAddress, text: []u8) ?[]const u8 {
    if (uv.uv_ip_name(address, text.ptr, text.len) != 0) {
        return null;
    }
    return std.mem.sliceTo(text, 0);
}

pub fn addressPort(address: *const uv.SocketAddress) i64 {
    return std.mem.readInt(u16, address.bytes[2..4], .big);
}

pub fn validPort(port: i64) ?u16 {
    return std.math.cast(u16, port);
}

fn parseIpAddress(host: []const u8, port: u16) ?uv.SocketAddress {
    const is_valid_length = host.len > 0 and host.len < max_ip_length;
    if (!is_valid_length) {
        return null;
    }
    var text: [max_ip_length:0]u8 = @splat(0);
    @memcpy(text[0..host.len], host);
    var address: uv.SocketAddress = .{};
    if (uv.uv_ip4_addr(&text, port, &address) == 0) {
        return address;
    }
    address = .{};
    if (uv.uv_ip6_addr(&text, port, &address) == 0) {
        return address;
    }
    return null;
}

fn netErrorFromStatus(status: c_int) NetError {
    return net_errors_by_name.get(std.mem.span(uv.uv_err_name(status))) orelse .Other;
}

fn handleResult(handle: i64) WakeResult {
    return .{ .value = .{ .int64 = handle }, .kind = .int64 };
}

fn netFailure(failure: NetError) WakeResult {
    return .{ .value = .{ .int64 = @intFromEnum(failure) }, .kind = .int64, .has_failed = true, .error_is_relative = true };
}

fn ioFailure(failure: IoError) WakeResult {
    return .{ .value = .{ .int64 = @intFromEnum(failure) }, .kind = .int64, .has_failed = true, .error_is_relative = true };
}
