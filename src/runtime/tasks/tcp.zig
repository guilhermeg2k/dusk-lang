const std = @import("std");
const uv = @import("uv.zig");
const EventLoop = @import("event_loop.zig").EventLoop;
const HandleOwner = @import("event_loop.zig").HandleOwner;
const onHandleClosed = @import("event_loop.zig").onHandleClosed;
const Scheduler = @import("scheduler.zig").Scheduler;
const TaskState = @import("scheduler.zig").TaskState;
const WakeResult = @import("scheduler.zig").WakeResult;
const Stream = @import("stream.zig").Stream;
const IoError = @import("stream.zig").IoError;

const handle_alignment = std.mem.Alignment.@"16";
const max_ip_length = 64;

pub const NetError = enum {
    HostNotFound,
    ConnectionRefused,
    AddressInUse,
    NetworkUnreachable,
    PermissionDenied,
    InvalidAddress,
    TimedOut,
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
});

pub fn connect(event_loop: *EventLoop, scheduler: *Scheduler, task: *TaskState, address: *const uv.SocketAddress) !?NetError {
    const handle = try event_loop.openTcpStream(scheduler);
    const stream = event_loop.streamAt(@intCast(handle)).?;
    const request = ConnectRequest.create(event_loop, scheduler, task, stream, handle) catch |failure| {
        event_loop.closeStream(@intCast(handle));
        return failure;
    };
    const status = uv.uv_tcp_connect(request.uvRequest(), stream.uvHandle(), address, onConnect);
    if (status != 0) {
        request.destroy();
        event_loop.closeStream(@intCast(handle));
        return netErrorFromStatus(status);
    }
    stream.requests_in_flight += 1;
    event_loop.pending_operations += 1;
    return null;
}

const ConnectRequest = struct {
    event_loop: *EventLoop,
    scheduler: *Scheduler,
    task: *TaskState,
    stream: *Stream,
    handle: usize,
    request_memory: []align(16) u8,

    fn create(event_loop: *EventLoop, scheduler: *Scheduler, task: *TaskState, stream: *Stream, handle: usize) !*ConnectRequest {
        const allocator = event_loop.allocator;
        const request = try allocator.create(ConnectRequest);
        errdefer allocator.destroy(request);
        const memory = try allocator.alignedAlloc(u8, handle_alignment, uv.uv_req_size(.connect));
        request.* = .{ .event_loop = event_loop, .scheduler = scheduler, .task = task, .stream = stream, .handle = handle, .request_memory = memory };
        uv.uv_req_set_data(request.uvRequest(), request);
        return request;
    }

    fn destroy(self: *ConnectRequest) void {
        const allocator = self.event_loop.allocator;
        allocator.free(self.request_memory);
        allocator.destroy(self);
    }

    fn uvRequest(self: *ConnectRequest) *uv.Request {
        return @ptrCast(self.request_memory.ptr);
    }
};

fn onConnect(uv_request: *uv.Request, status: c_int) callconv(.c) void {
    const request: *ConnectRequest = @ptrCast(@alignCast(uv.uv_req_get_data(uv_request).?));
    defer request.destroy();
    const event_loop = request.event_loop;
    const stream = request.stream;
    event_loop.pending_operations -= 1;
    if (stream.is_closed) {
        stream.finishRequest();
        return;
    }
    stream.requests_in_flight -= 1;
    var wake_result: WakeResult = .{ .value = .{ .int64 = @intCast(request.handle) }, .kind = .int64 };
    if (status != 0) {
        event_loop.closeStream(@intCast(request.handle));
        wake_result = netFailure(netErrorFromStatus(status));
    }
    request.scheduler.wake(request.task, wake_result) catch |failure| {
        event_loop.callback_failure = failure;
    };
}

pub const Listener = struct {
    const Self = @This();

    owner: HandleOwner = .{ .kind = .listener },
    allocator: std.mem.Allocator,
    event_loop: *EventLoop,
    scheduler: *Scheduler,
    handle_memory: []align(16) u8,
    is_handle_open: bool = true,
    is_closed: bool = false,
    pending_connections: usize = 0,
    failure: ?IoError = null,
    waiter: ?*TaskState = null,

    pub const AcceptOutcome = union(enum) {
        connection: usize,
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
        listener.* = .{ .allocator = allocator, .event_loop = event_loop, .scheduler = scheduler, .handle_memory = memory };
        uv.uv_handle_set_data(listener.uvHandle(), &listener.owner);
        return listener;
    }

    pub fn destroy(self: *Self) void {
        self.allocator.free(self.handle_memory);
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
        return std.mem.readInt(u16, address.bytes[2..4], .big);
    }

    pub fn takeReadyConnection(self: *Self) ?AcceptOutcome {
        if (self.failure) |failure| {
            self.failure = null;
            return .{ .failure = failure };
        }
        if (self.pending_connections == 0) {
            return null;
        }
        self.pending_connections -= 1;
        const handle = self.acceptConnection() catch return .{ .failure = .Other };
        return .{ .connection = handle };
    }

    fn acceptConnection(self: *Self) !usize {
        const handle = try self.event_loop.openTcpStream(self.scheduler);
        const stream = self.event_loop.streamAt(@intCast(handle)).?;
        if (uv.uv_accept(self.uvHandle(), stream.uvHandle()) != 0) {
            self.event_loop.closeStream(@intCast(handle));
            return error.AcceptFailed;
        }
        return handle;
    }

    pub fn parkAccepter(self: *Self, task: *TaskState) void {
        self.waiter = task;
        self.event_loop.pending_operations += 1;
    }

    pub fn close(self: *Self) void {
        if (self.waiter) |task| {
            self.waiter = null;
            self.event_loop.pending_operations -= 1;
            self.scheduler.wake(task, ioFailure(.Closed)) catch |failure| {
                self.event_loop.callback_failure = failure;
            };
        }
        self.is_closed = true;
        uv.uv_close(self.uvHandle(), onHandleClosed);
    }

    pub fn releaseAtExit(self: *Self) void {
        self.is_closed = true;
        self.releaseIfDone();
    }

    pub fn markHandleClosed(self: *Self) void {
        self.is_handle_open = false;
        self.releaseIfDone();
    }

    fn releaseIfDone(self: *Self) void {
        const is_done = self.is_closed and !self.is_handle_open;
        if (is_done) {
            self.destroy();
        }
    }

    fn serveWaiter(self: *Self) void {
        const task = self.waiter orelse return;
        const outcome = self.takeReadyConnection() orelse return;
        const wake_result: WakeResult = switch (outcome) {
            .connection => |handle| .{ .value = .{ .int64 = @intCast(handle) }, .kind = .int64 },
            .failure => |failure| ioFailure(failure),
        };
        self.waiter = null;
        self.event_loop.pending_operations -= 1;
        self.scheduler.wake(task, wake_result) catch |failure| {
            self.event_loop.callback_failure = failure;
        };
    }

    fn uvHandle(self: *Self) *uv.Handle {
        return @ptrCast(self.handle_memory.ptr);
    }
};

fn onConnection(server: *uv.Handle, status: c_int) callconv(.c) void {
    const listener: *Listener = @alignCast(@fieldParentPtr("owner", HandleOwner.of(server)));
    if (status < 0) {
        listener.failure = .Other;
    } else {
        listener.pending_connections += 1;
    }
    listener.serveWaiter();
}

pub fn parseIpAddress(host: []const u8, port: i64) ?uv.SocketAddress {
    const is_valid_port = port >= 0 and port <= std.math.maxInt(u16);
    const is_valid_length = host.len > 0 and host.len < max_ip_length;
    if (!is_valid_port or !is_valid_length) {
        return null;
    }
    var text: [max_ip_length:0]u8 = @splat(0);
    @memcpy(text[0..host.len], host);
    var address: uv.SocketAddress = .{};
    if (uv.uv_ip4_addr(&text, @intCast(port), &address) == 0) {
        return address;
    }
    address = .{};
    if (uv.uv_ip6_addr(&text, @intCast(port), &address) == 0) {
        return address;
    }
    return null;
}

fn netErrorFromStatus(status: c_int) NetError {
    return net_errors_by_name.get(std.mem.span(uv.uv_err_name(status))) orelse .Other;
}

fn netFailure(failure: NetError) WakeResult {
    return .{ .value = .{ .int64 = @intFromEnum(failure) }, .kind = .int64, .has_failed = true, .error_is_relative = true };
}

fn ioFailure(failure: IoError) WakeResult {
    return .{ .value = .{ .int64 = @intFromEnum(failure) }, .kind = .int64, .has_failed = true, .error_is_relative = true };
}
