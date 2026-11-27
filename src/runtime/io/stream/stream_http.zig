const std = @import("std");
const uv = @import("../../tasks/loop/uv.zig");
const HandleOwner = @import("../../tasks/event_loop.zig").HandleOwner;
const Stream = @import("stream.zig").Stream;
const stream_read = @import("stream_read.zig");
const BodyRead = stream_read.BodyRead;
const ReadOutcome = stream_read.ReadOutcome;
const ReadSource = stream_read.ReadSource;
const HttpReader = @import("../http/http_reader.zig").HttpReader;
const MessageKind = @import("../http/http_reader.zig").MessageKind;
const HttpError = @import("../http/http.zig").HttpError;
const max_http_head_size = @import("../http/http_reader.zig").max_head_size;
const idle_timeout_ms = @import("../http/connection_pool.zig").idle_timeout_ms;

pub fn startHttpMessage(stream: *Stream, kind: MessageKind) !void {
    if (stream.http_reader) |reader| {
        return reader.startMessage(kind);
    }
    stream.http_reader = try HttpReader.create(stream.allocator, kind);
}

pub fn isReusable(stream: *const Stream) bool {
    const reader = stream.http_reader orelse return false;
    const is_idle = stream.waiter == null and !stream.is_closed and !stream.has_ended and stream.failure == null and stream.bufferedBytes().len == 0;
    const is_tls_ready = if (stream.tls_layer) |layer| layer.handshake == .done else true;
    return is_idle and is_tls_ready and !stream.is_reuse_forbidden and reader.isMessageComplete() and reader.canKeepAlive();
}

pub fn enterPool(stream: *Stream, key: []const u8) !void {
    stream.releaseHostSlot();
    stream.pool_key = try stream.allocator.dupe(u8, key);
    stream.buffered.clearAndFree(stream.allocator);
    stream.read_offset = 0;
    if (stream.http_reader) |reader| {
        reader.releaseBody();
    }
    try stream.startReading();
    try stream.startTimerHandle(&stream.timer_memory, &stream.timer_owner, onIdleTimeout, idle_timeout_ms);
    stream.is_timer_running = true;
}

pub fn leavePool(stream: *Stream) void {
    const key = stream.pool_key orelse return;
    stream.allocator.free(key);
    stream.pool_key = null;
    stream.stopTimer();
}

pub fn evictIfUnusable(stream: *Stream) void {
    if (stream.pool_key == null or isReusable(stream)) return;
    evictFromPool(stream);
}

pub fn evictFromPool(stream: *Stream) void {
    const key = stream.pool_key orelse return;
    stream.event_loop.connection_pool.remove(stream.allocator, key, stream.table_index);
    leavePool(stream);
    stream.event_loop.closeStream(stream.table_index);
}

pub fn headOutcome(stream: *Stream) ?ReadOutcome {
    const reader = stream.http_reader orelse return stream_read.httpFailure(.Other);
    const available = stream.bufferedBytes();
    switch (reader.feedHead(available)) {
        .done => |length| {
            if (length > max_http_head_size) {
                return stream_read.httpFailure(.HeadersTooLarge);
            }
            return .{ .result = .{ .data = available[0..length] }, .consumed = length };
        },
        .failure => |failure| return stream_read.httpFailure(failure),
        .want_more => {},
    }
    if (available.len > max_http_head_size) {
        return stream_read.httpFailure(.HeadersTooLarge);
    }
    if (stream.failure) |failure| {
        return .{ .result = .{ .failure = failure } };
    }
    if (!stream.has_ended) {
        return null;
    }
    if (available.len == 0) {
        return .{ .result = .end };
    }
    return stream_read.httpFailure(reader.invalidMessage());
}

pub fn bodyOutcome(stream: *Stream, read: BodyRead) ?ReadOutcome {
    const reader = stream.http_reader orelse return .{ .result = .{ .failure = .Other } };
    reader.dropTakenBody();
    if (feedHttpBody(stream, reader, std.math.maxInt(usize)) != null) {
        return .{ .result = .{ .failure = .Other } };
    }
    reader.decodeBody(read.decodeLimit()) catch return .{ .result = .{ .failure = .Other } };
    var has_ended = reader.isMessageComplete();
    const has_no_more_input = stream.has_ended and stream.bufferedBytes().len == 0;
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
    const source: ReadSource = .{ .available = reader.pendingBody(), .has_ended = has_ended, .failure = if (has_ended) null else stream.failure };
    const found = switch (read) {
        .bytes => |max| source.bytesOutcome(max),
        .line => |max| source.lineOutcome(max),
        .all_text => source.allOutcome(true, null),
        .all_bytes => source.allOutcome(false, null),
    };
    var outcome = found orelse return null;
    reader.taken = outcome.consumed;
    outcome.consumed = 0;
    return outcome;
}

pub fn wholeBodyOutcome(stream: *Stream, cap: usize) ?ReadOutcome {
    const reader = stream.http_reader orelse return stream_read.httpFailure(.Other);
    if (feedHttpBody(stream, reader, cap +| 1)) |failure| {
        return stream_read.httpFailure(failure);
    }
    if (reader.body.items.len > cap) {
        reader.was_body_too_large = true;
        return stream_read.httpFailure(.BodyTooLarge);
    }
    if (reader.isMessageComplete()) {
        return .{ .result = .{ .data = reader.body.items } };
    }
    if (stream.failure) |failure| {
        return .{ .result = .{ .failure = failure } };
    }
    if (!stream.has_ended) {
        return null;
    }
    if (reader.finishBody()) {
        return .{ .result = .{ .data = reader.body.items } };
    }
    return stream_read.httpFailure(reader.invalidMessage());
}

fn feedHttpBody(stream: *Stream, reader: *HttpReader, max: usize) ?HttpError {
    const wire_limit = max +| max_http_head_size;
    while (!reader.isMessageComplete()) {
        const available = stream.bufferedBytes();
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
        stream.consume(used);
        if (!reader.isMessageComplete() and reader.body_wire_bytes >= wire_limit) {
            reader.was_body_too_large = true;
            return .BodyTooLarge;
        }
        if (used == 0) break;
    }
    return null;
}

fn onIdleTimeout(timer: *uv.Handle) callconv(.c) void {
    const stream: *Stream = @alignCast(@fieldParentPtr("timer_owner", HandleOwner.of(timer)));
    stream.is_timer_running = false;
    evictFromPool(stream);
}
