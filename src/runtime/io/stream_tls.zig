const uv = @import("../tasks/loop/uv.zig");
const HandleOwner = @import("../tasks/event_loop.zig").HandleOwner;
const TaskState = @import("../tasks/scheduler.zig").TaskState;
const WakeResult = @import("../tasks/scheduler.zig").WakeResult;
const Stream = @import("stream.zig").Stream;
const WriteOutcome = @import("stream.zig").WriteOutcome;
const stream_read = @import("stream_read.zig");
const IoError = stream_read.IoError;
const tls = @import("net/tls.zig");
const TlsLayer = @import("net/tls_layer.zig").TlsLayer;
const ServerCertificate = @import("net/tls_layer.zig").ServerCertificate;
const HandshakeWaiter = @import("net/tls_layer.zig").HandshakeWaiter;
const cipher_chunk_size = @import("net/tls_layer.zig").cipher_chunk_size;
const netErrorFromHandshake = @import("net/tls_layer.zig").netErrorFromHandshake;
const NetError = @import("net/tcp.zig").NetError;

const tls_record_size = 16 * 1024 + 256;
const max_tls_handshake_bytes: usize = 256 * 1024;

pub const HandshakeWait = union(enum) {
    done,
    parked,
    failure: NetError,
};

pub fn startClientTls(stream: *Stream, task: *TaskState, config: *tls.Config, owns_config: bool, host: []const u8, timeout_ms: ?u64, is_tunneled: bool) !HandshakeWait {
    stream.is_starting_tls = false;
    const layer = TlsLayer.createClient(stream.allocator, config, owns_config, host) catch |failure| {
        if (owns_config) {
            tls.dusk_tls_config_free(config);
        }
        return failure;
    };
    if (is_tunneled and stream.tls_layer != null) {
        stream.tunnel_layer = stream.tls_layer;
    }
    stream.tls_layer = layer;
    feedBufferedIntoTls(stream, layer);
    return awaitHandshake(stream, task, timeout_ms);
}

pub fn startServerTls(stream: *Stream, task: *TaskState, certificate: *ServerCertificate, timeout_ms: ?u64) !HandshakeWait {
    const layer = try TlsLayer.createServer(stream.allocator, certificate);
    stream.tls_layer = layer;
    feedBufferedIntoTls(stream, layer);
    return awaitHandshake(stream, task, timeout_ms);
}

pub fn useServerTls(stream: *Stream, certificate: *ServerCertificate) !void {
    stream.tls_layer = try TlsLayer.createServer(stream.allocator, certificate);
}

pub fn awaitHandshake(stream: *Stream, task: *TaskState, timeout_ms: ?u64) !HandshakeWait {
    const layer = stream.tls_layer orelse return .done;
    ensureHandshakeStarted(stream);
    switch (layer.handshake) {
        .done => return .done,
        .failed => return .{ .failure = layer.handshake_failure },
        .pending, .running => {},
    }
    if (timeout_ms) |ms| {
        if (!stream.is_handshake_timer_running) {
            try stream.startTimerHandle(&stream.handshake_timer_memory, &stream.handshake_timer_owner, onHandshakeTimeout, ms);
            stream.is_handshake_timer_running = true;
        }
    }
    try layer.handshake_waiters.append(stream.allocator, .{ .task = task, .pending_write = null });
    stream.event_loop.pending_operations += 1;
    return .parked;
}

pub fn ensureHandshakeStarted(stream: *Stream) void {
    const layer = stream.tls_layer orelse return;
    if (layer.handshake != .pending) return;
    layer.handshake = .running;
    stream.startReading() catch {
        layer.handshake = .failed;
        stream.failure = .HandshakeFailed;
        return;
    };
    stepHandshake(stream, false);
}

pub fn write(stream: *Stream, layer: *TlsLayer, task: *TaskState, data: []const u8) !WriteOutcome {
    if (layer.handshake != .done) {
        ensureHandshakeStarted(stream);
    }
    switch (layer.handshake) {
        .failed => return .{ .failure = .HandshakeFailed },
        .pending, .running => {
            const bytes = try stream.allocator.dupe(u8, data);
            errdefer stream.allocator.free(bytes);
            try layer.handshake_waiters.append(stream.allocator, .{ .task = task, .pending_write = bytes });
            stream.event_loop.pending_operations += 1;
            return .parked;
        },
        .done => {},
    }
    if (tls.dusk_tls_write(layer.session, data.ptr, data.len) < 0) {
        return .{ .failure = .Other };
    }
    return sendCipherOutput(stream, task);
}

pub fn close(stream: *Stream) void {
    if (stream.tls_layer) |layer| {
        closeLayer(stream, layer);
        return;
    }
    const tunnel = stream.tunnel_layer orelse return;
    if (tunnel.handshake == .done and stream.failure == null) {
        _ = tls.dusk_tls_close_notify(tunnel.session);
        _ = sendTunnelOutput(stream, tunnel, null) catch {};
    }
}

pub fn stopHandshakeTimer(stream: *Stream) void {
    if (!stream.is_handshake_timer_running) return;
    _ = uv.uv_timer_stop(handshakeTimerHandle(stream));
    stream.is_handshake_timer_running = false;
}

pub fn handshakeTimerHandle(stream: *Stream) *uv.Handle {
    return @ptrCast(stream.handshake_timer_memory.ptr);
}

pub fn receiveTlsRead(stream: *Stream, layer: *TlsLayer, read_count: isize) void {
    if (read_count > 0) {
        receiveCiphertext(stream, layer, layer.cipher_scratch[0..@intCast(read_count)]);
        return;
    }
    if (read_count == 0) return;
    const status: c_int = @intCast(read_count);
    if (stream_read.isErrorNamed(status, "EOF")) {
        stream.has_ended = true;
    } else {
        stream.failure = stream_read.ioErrorFromStatus(status);
    }
    if (layer.handshake == .running) {
        finishHandshake(stream, .failed);
        return;
    }
    stream.serveWaiter();
    stream.applyBackpressure();
}

pub fn receiveTunnelRead(stream: *Stream, tunnel: *TlsLayer, read_count: isize) void {
    const layer = stream.tls_layer orelse return;
    if (read_count <= 0) {
        receiveTlsRead(stream, layer, read_count);
        return;
    }
    if (tls.dusk_tls_feed(tunnel.session, &tunnel.cipher_scratch, @intCast(read_count)) != .done) {
        failTlsRead(stream, layer, .Other);
        return;
    }
    forwardTunnelPlaintext(stream, tunnel, layer);
}

pub fn finishHandshake(stream: *Stream, outcome: TlsLayer.Handshake) void {
    const layer = stream.tls_layer.?;
    layer.handshake = outcome;
    stopHandshakeTimer(stream);
    if (outcome == .failed) {
        stream.failure = .HandshakeFailed;
    }
    var waiters = layer.takeHandshakeWaiters();
    defer waiters.deinit(stream.allocator);
    for (waiters.items) |waiter| {
        defer waiter.free(stream.allocator);
        stream.event_loop.pending_operations -= 1;
        const wake_result = handshakeWakeResult(stream, waiter, outcome) orelse continue;
        stream.wakeTask(waiter.task, wake_result);
    }
    if (outcome == .done) {
        decryptAvailable(stream);
        return;
    }
    stream.serveWaiter();
    stream.applyBackpressure();
}

fn feedBufferedIntoTls(stream: *Stream, layer: *TlsLayer) void {
    const early = stream.bufferedBytes();
    if (early.len == 0) return;
    _ = tls.dusk_tls_feed(layer.session, early.ptr, early.len);
    stream.consume(early.len);
}

fn closeLayer(stream: *Stream, layer: *TlsLayer) void {
    var waiters = layer.takeHandshakeWaiters();
    defer waiters.deinit(stream.allocator);
    for (waiters.items) |waiter| {
        defer waiter.free(stream.allocator);
        stream.event_loop.pending_operations -= 1;
        const wake_result = if (waiter.pending_write == null) stream_read.netFailureResult(.HandshakeFailed) else stream_read.failureResult(.Closed);
        stream.wakeTask(waiter.task, wake_result);
    }
    if (layer.handshake == .done) {
        _ = tls.dusk_tls_close_notify(layer.session);
        tryWriteCipherOutput(stream, layer.session, stream.tunnel_layer);
    }
    const tunnel = stream.tunnel_layer orelse return;
    _ = tls.dusk_tls_close_notify(tunnel.session);
    tryWriteCipherOutput(stream, tunnel.session, null);
}

fn tryWriteCipherOutput(stream: *Stream, session: *tls.Session, through: ?*TlsLayer) void {
    const length = tls.dusk_tls_output_length(session);
    if (length == 0) return;
    const output = tls.dusk_tls_output_bytes(session)[0..length];
    if (through) |tunnel| {
        _ = tls.dusk_tls_write(tunnel.session, output.ptr, output.len);
        tls.dusk_tls_consume_output(session, length);
        tryWriteCipherOutput(stream, tunnel.session, null);
        return;
    }
    const buffers = [_]uv.Buffer{.{ .base = @constCast(output.ptr), .len = @intCast(output.len) }};
    _ = uv.uv_try_write(stream.uvHandle(), &buffers, 1);
    tls.dusk_tls_consume_output(session, length);
}

fn stepHandshake(stream: *Stream, should_wake: bool) void {
    const layer = stream.tls_layer.?;
    const status = tls.dusk_tls_handshake(layer.session);
    flushCipherOutput(stream);
    const outcome: TlsLayer.Handshake = switch (status) {
        .want_more => if (stream.failure != null) .failed else return,
        .done => .done,
        else => .failed,
    };
    if (outcome == .failed and status != .want_more) {
        layer.handshake_failure = netErrorFromHandshake(status);
    }
    if (!should_wake) {
        layer.handshake = outcome;
        if (outcome == .failed) {
            stream.failure = .HandshakeFailed;
        }
        return;
    }
    finishHandshake(stream, outcome);
}

fn handshakeWakeResult(stream: *Stream, waiter: HandshakeWaiter, outcome: TlsLayer.Handshake) ?WakeResult {
    const layer = stream.tls_layer.?;
    const bytes = waiter.pending_write orelse {
        return if (outcome == .done) stream_read.doneResult() else stream_read.netFailureResult(layer.handshake_failure);
    };
    if (outcome != .done) {
        return stream_read.failureResult(.HandshakeFailed);
    }
    if (tls.dusk_tls_write(layer.session, bytes.ptr, bytes.len) < 0) {
        stream.failure = .Other;
        return stream_read.failureResult(.Other);
    }
    const sent = sendCipherOutput(stream, waiter.task) catch return stream_read.failureResult(.Other);
    return switch (sent) {
        .done => stream_read.doneResult(),
        .parked => null,
        .failure => |failure| stream_read.failureResult(failure),
    };
}

fn forwardTunnelPlaintext(stream: *Stream, tunnel: *TlsLayer, layer: *TlsLayer) void {
    while (true) {
        const count = tls.dusk_tls_read(tunnel.session, &layer.cipher_scratch, cipher_chunk_size);
        if (count > 0) {
            receiveCiphertext(stream, layer, layer.cipher_scratch[0..@intCast(count)]);
            continue;
        }
        if (count == @intFromEnum(tls.Status.end)) {
            stream.has_ended = true;
            if (layer.handshake == .running) {
                finishHandshake(stream, .failed);
                return;
            }
            stream.serveWaiter();
            stream.applyBackpressure();
        } else if (count < 0) {
            failTlsRead(stream, layer, .Other);
            return;
        }
        break;
    }
    const outcome = sendTunnelOutput(stream, tunnel, null) catch {
        failTlsRead(stream, layer, .Other);
        return;
    };
    switch (outcome) {
        .failure => |failure| failTlsRead(stream, layer, failure),
        .done, .parked => {},
    }
}

fn failTlsRead(stream: *Stream, layer: *TlsLayer, failure: IoError) void {
    stream.failure = failure;
    if (layer.handshake == .running) {
        finishHandshake(stream, .failed);
        return;
    }
    stream.serveWaiter();
    stream.applyBackpressure();
}

fn receiveCiphertext(stream: *Stream, layer: *TlsLayer, data: []const u8) void {
    if (layer.handshake == .running) {
        layer.handshake_bytes_received +|= data.len;
        if (layer.handshake_bytes_received > max_tls_handshake_bytes) {
            finishHandshake(stream, .failed);
            return;
        }
    }
    if (tls.dusk_tls_feed(layer.session, data.ptr, data.len) != .done) {
        failTlsRead(stream, layer, .Other);
        return;
    }
    switch (layer.handshake) {
        .running => stepHandshake(stream, true),
        .done => decryptAvailable(stream),
        .pending, .failed => {},
    }
}

fn decryptAvailable(stream: *Stream) void {
    const layer = stream.tls_layer.?;
    while (true) {
        stream.buffered.ensureUnusedCapacity(stream.allocator, tls_record_size) catch {
            stream.failure = .Other;
            break;
        };
        const space = stream.buffered.unusedCapacitySlice();
        const count = tls.dusk_tls_read(layer.session, space.ptr, space.len);
        if (count > 0) {
            stream.buffered.items.len += @intCast(count);
            continue;
        }
        if (count == @intFromEnum(tls.Status.end)) {
            stream.has_ended = true;
        } else if (count < 0) {
            stream.failure = .Other;
        }
        break;
    }
    flushCipherOutput(stream);
    stream.serveWaiter();
    stream.applyBackpressure();
}

fn sendCipherOutput(stream: *Stream, task: ?*TaskState) !WriteOutcome {
    const session = stream.tls_layer.?.session;
    const length = tls.dusk_tls_output_length(session);
    if (length == 0) return .done;
    const output = tls.dusk_tls_output_bytes(session)[0..length];
    const tunnel = stream.tunnel_layer orelse {
        const outcome = try stream.sendBytes(task, output);
        tls.dusk_tls_consume_output(session, length);
        return outcome;
    };
    if (tls.dusk_tls_write(tunnel.session, output.ptr, output.len) < 0) {
        return .{ .failure = .Other };
    }
    tls.dusk_tls_consume_output(session, length);
    return sendTunnelOutput(stream, tunnel, task);
}

fn sendTunnelOutput(stream: *Stream, tunnel: *TlsLayer, task: ?*TaskState) !WriteOutcome {
    const length = tls.dusk_tls_output_length(tunnel.session);
    if (length == 0) return .done;
    const outcome = try stream.sendBytes(task, tls.dusk_tls_output_bytes(tunnel.session)[0..length]);
    tls.dusk_tls_consume_output(tunnel.session, length);
    return outcome;
}

fn flushCipherOutput(stream: *Stream) void {
    if (stream.is_closed) return;
    const outcome = sendCipherOutput(stream, null) catch {
        stream.failure = .Other;
        return;
    };
    switch (outcome) {
        .failure => |failure| stream.failure = failure,
        .done, .parked => {},
    }
}

fn onHandshakeTimeout(timer: *uv.Handle) callconv(.c) void {
    const stream: *Stream = @alignCast(@fieldParentPtr("handshake_timer_owner", HandleOwner.of(timer)));
    stream.is_handshake_timer_running = false;
    const layer = stream.tls_layer orelse return;
    if (layer.isHandshakeOver()) return;
    layer.handshake_failure = .TimedOut;
    finishHandshake(stream, .failed);
}
