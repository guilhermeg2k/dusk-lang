const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const native = @import("../native.zig");
const build_zon = @import("build_zon");
const io = @import("io.zig");
const net = @import("net.zig");
const fs = @import("fs.zig");
const http = @import("../../tasks/http.zig");
const ReadMode = @import("../../tasks/stream.zig").ReadMode;
const EventLoop = @import("../../tasks/event_loop.zig").EventLoop;
const httpErrorFromIo = @import("../../tasks/http_reader.zig").httpErrorFromIo;
const RuntimeErrorReporter = @import("../../runtime_error.zig").RuntimeErrorReporter;
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const ok = native.ok;
const fail = native.fail;
const bytesArg = native.bytesArg;

pub const HttpError = http.HttpError;
pub const error_sets = .{HttpError};

const replacement_character = "\u{FFFD}";
const user_agent = @tagName(build_zon.name) ++ "-http-client";

pub const bindings = native.BindingMap.initComptime(.{
    .{ "own", Binding{ .native = &native.ownResource } },
    .{ "release", Binding{ .native = &native.releaseResource } },
    .{ "tcp_connect_handle", Binding{ .native = &net.tcpConnect } },
    .{ "stream_start_tls", Binding{ .native = &net.streamStartTls } },
    .{ "stream_start_tunneled_tls", Binding{ .native = &net.streamStartTunneledTls } },
    .{ "base64", Binding{ .native = &base64 } },
    .{ "user_agent", Binding{ .native = &userAgent } },
    .{ "encoded_url", Binding{ .native = &encodedUrl } },
    .{ "is_loopback_host", Binding{ .native = &isLoopbackHost } },
    .{ "stream_forbid_reuse", Binding{ .native = &streamForbidReuse } },
    .{ "stream_has_final_status", Binding{ .native = &streamHasFinalStatus } },
    .{ "host_slot_take", Binding{ .native = &hostSlotTake } },
    .{ "host_slot_release", Binding{ .native = &hostSlotRelease } },
    .{ "stream_hold_host_slot", Binding{ .native = &streamHoldHostSlot } },
    .{ "stream_write", Binding{ .native = &io.streamWrite } },
    .{ "stream_write_bytes", Binding{ .native = &io.streamWrite } },
    .{ "stream_set_write_timeout", Binding{ .native = &io.streamSetWriteTimeout } },
    .{ "stream_close", Binding{ .native = &io.streamClose } },
    .{ "open_descriptor", Binding{ .native = &fs.openDescriptor } },
    .{ "file_stream", Binding{ .native = &fs.fileStream } },
    .{ "stream_read_bytes", Binding{ .native = &io.streamReadBytes } },
    .{ "stream_read_head", Binding{ .native = &streamReadHead } },
    .{ "stream_read_whole_body", Binding{ .native = &streamReadWholeBody } },
    .{ "stream_body_bytes", Binding{ .native = &streamBodyBytes } },
    .{ "stream_body_line", Binding{ .native = &streamBodyLine } },
    .{ "stream_body_all", Binding{ .native = &streamBodyAll } },
    .{ "stream_body_all_bytes", Binding{ .native = &streamBodyAllBytes } },
    .{ "parse_head", Binding{ .native = &parseHead } },
    .{ "conn_handle", Binding{ .native = &connHandle } },
    .{ "stream_wait_data", Binding{ .native = &streamWaitData } },
    .{ "stream_read_request_head", Binding{ .native = &streamReadRequestHead } },
    .{ "stream_drain_request_body", Binding{ .native = &streamDrainRequestBody } },
    .{ "parse_request_head", Binding{ .native = &parseRequestHead } },
    .{ "http_date", Binding{ .native = &httpDate } },
    .{ "claim_continue", Binding{ .native = &claimContinue } },
    .{ "request_body_state", Binding{ .native = &requestBodyState } },
    .{ "request_body_failure", Binding{ .native = &requestBodyFailure } },
    .{ "mark_request_body_too_large", Binding{ .native = &markRequestBodyTooLarge } },
    .{ "server_state_new", Binding{ .native = &serverStateNew } },
    .{ "server_set_idle", Binding{ .native = &serverSetIdle } },
    .{ "server_close", Binding{ .native = &serverClose } },
    .{ "server_is_closing", Binding{ .native = &serverIsClosing } },
    .{ "stream_decode_body", Binding{ .native = &streamDecodeBody } },
    .{ "pool_take", Binding{ .native = &poolTake } },
    .{ "pool_put", Binding{ .native = &poolPut } },
    .{ "is_method", Binding{ .native = &isMethod } },
    .{ "is_header_name", Binding{ .native = &isHeaderName } },
    .{ "is_header_value", Binding{ .native = &isHeaderValue } },
    .{ "invalid_compression_level", Binding{ .native = &invalidCompressionLevel } },
});

fn streamReadHead(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const stream = vm.event_loop.streamAt(args[0].int64) orelse return fail(HttpError.Closed);
    if (stream.waiter != null) {
        return fail(HttpError.Other);
    }
    stream.startHttpMessage(if (args[1].bool) .head_response else .response) catch return fail(HttpError.Other);
    return readHttp(vm, args[0].int64, .http_head, args[2].int64);
}

fn connHandle(_: *VM, args: []v.Value) anyerror!NativeResult {
    const conn = v.HeapValue.getParentPtr(v.Struct, args[0].heap_obj);
    return ok(conn.get(0));
}

fn streamWaitData(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return io.readStream(vm, args[0].int64, .any_data, args[1].int64);
}

fn streamReadRequestHead(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const stream = vm.event_loop.streamAt(args[0].int64) orelse return fail(HttpError.Closed);
    if (stream.waiter != null) {
        return fail(HttpError.Other);
    }
    stream.startHttpMessage(.request) catch return fail(HttpError.Other);
    return readHttp(vm, args[0].int64, .http_head, args[1].int64);
}

fn streamDrainRequestBody(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const cap: usize = @intCast(@max(args[1].int64, 0));
    if (vm.event_loop.streamAt(args[0].int64)) |stream| {
        if (stream.http_reader) |reader| {
            reader.releaseBody();
        }
    }
    return readHttp(vm, args[0].int64, .{ .http_whole_body = cap }, args[2].int64);
}

fn claimContinue(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const stream = vm.event_loop.streamAt(args[0].int64) orelse return ok(.{ .bool = false });
    const reader = stream.http_reader orelse return ok(.{ .bool = false });
    const can_claim = !reader.has_sent_continue and !reader.isMessageComplete();
    reader.has_sent_continue = true;
    return ok(.{ .bool = can_claim });
}

const RequestBodyState = enum(i64) { complete, unread, waiting_for_continue, failed, gone };
const RequestBodyFailure = enum(i64) { none, too_large, invalid };

fn requestBodyState(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .int64 = @intFromEnum(bodyStateOf(vm, args[0].int64, args[1].bool)) });
}

fn bodyStateOf(vm: *VM, handle: i64, expects_continue: bool) RequestBodyState {
    const stream = vm.event_loop.streamAt(handle) orelse return .gone;
    const reader = stream.http_reader orelse return .gone;
    if (reader.isMessageComplete()) {
        return .complete;
    }
    if (reader.was_body_too_large or reader.was_body_invalid) {
        return .failed;
    }
    const waits_for_continue = expects_continue and !reader.has_sent_continue;
    return if (waits_for_continue) .waiting_for_continue else .unread;
}

fn streamReadWholeBody(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const cap: usize = @intCast(@max(args[1].int64, 0));
    return readHttp(vm, args[0].int64, .{ .http_whole_body = cap }, args[2].int64);
}

fn streamBodyBytes(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const max = args[1].int64;
    if (max < 1) {
        return RuntimeErrorReporter.invalidReadSize(max);
    }
    return io.readStream(vm, args[0].int64, .{ .http_body = .{ .bytes = @intCast(max) } }, args[2].int64);
}

fn streamBodyLine(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const max = args[1].int64;
    const limit: ?usize = if (max < 0) null else @intCast(max);
    return io.readStream(vm, args[0].int64, .{ .http_body = .{ .line = limit } }, args[2].int64);
}

fn streamBodyAll(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return io.readStream(vm, args[0].int64, .{ .http_body = .all_text }, args[1].int64);
}

fn streamBodyAllBytes(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return io.readStream(vm, args[0].int64, .{ .http_body = .all_bytes }, args[1].int64);
}

fn readHttp(vm: *VM, handle: i64, mode: ReadMode, timeout_ms: i64) anyerror!NativeResult {
    const stream = vm.event_loop.streamAt(handle) orelse return fail(HttpError.Closed);
    if (stream.waiter != null) {
        return fail(HttpError.Other);
    }
    stream.ensureHandshakeStarted();
    if (stream.readyOutcome(mode)) |outcome| {
        switch (outcome.result) {
            .data => |data| {
                const value = try vm.allocBytesValue(data, .buffer);
                stream.consume(outcome.consumed);
                return ok(value);
            },
            .end => return ok(v.NULL_VALUE),
            .failure => |failure| {
                stream.consume(outcome.consumed);
                return fail(httpErrorFromIo(failure));
            },
            .http_failure => |failure| return fail(failure),
        }
    }
    const current = vm.running_task.?;
    const timeout: ?u64 = if (timeout_ms < 0) null else @intCast(timeout_ms);
    stream.parkReader(current, mode, timeout) catch return fail(HttpError.Other);
    current.park();
    return native.parked();
}

fn streamDecodeBody(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const stream = vm.event_loop.streamAt(args[0].int64) orelse return ok(.{ .null = {} });
    const reader = stream.http_reader orelse return ok(.{ .null = {} });
    try reader.startDecoding(@enumFromInt(args[1].int64));
    return ok(.{ .null = {} });
}

fn poolTake(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const event_loop = &vm.event_loop;
    try event_loop.poll();
    const key = bytesArg(args, 0);
    while (event_loop.connection_pool.takeNewest(event_loop.allocator, key)) |handle| {
        const stream = event_loop.streamAt(handle) orelse continue;
        stream.leavePool();
        if (!stream.isReusable()) {
            event_loop.closeStream(handle);
            continue;
        }
        return ok(.{ .int64 = handle });
    }
    return ok(.{ .int64 = -1 });
}

fn poolPut(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const event_loop = &vm.event_loop;
    const handle = args[1].int64;
    const stream = event_loop.streamAt(handle) orelse return ok(.{ .null = {} });
    if (stream.pool_key != null) {
        return ok(.{ .null = {} });
    }
    if (!stream.isReusable()) {
        event_loop.closeStream(handle);
        return ok(.{ .null = {} });
    }
    stream.enterPool(bytesArg(args, 0)) catch {
        stream.leavePool();
        event_loop.closeStream(handle);
        return ok(.{ .null = {} });
    };
    const evicted = event_loop.connection_pool.add(event_loop.allocator, bytesArg(args, 0), handle) catch {
        stream.leavePool();
        event_loop.closeStream(handle);
        return ok(.{ .null = {} });
    };
    const oldest = evicted orelse return ok(.{ .null = {} });
    if (event_loop.streamAt(oldest)) |oldest_stream| {
        oldest_stream.leavePool();
    }
    event_loop.closeStream(oldest);
    return ok(.{ .null = {} });
}

fn parseHead(vm: *VM, args: []v.Value) anyerror!NativeResult {
    var arena = std.heap.ArenaAllocator.init(std.heap.smp_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const head = try allocator.dupe(u8, bytesArg(args, 0));
    const parser = http.dusk_http_response_parser_new(@intFromBool(args[1].bool)) orelse return error.OutOfMemory;
    defer http.dusk_http_parser_free(parser);
    var used: usize = 0;
    if (http.dusk_http_feed_head(parser, head.ptr, head.len, &used) != .done) {
        return fail(HttpError.InvalidResponse);
    }
    const header_count = http.dusk_http_header_count(parser);
    const parts = try allocator.alloc([]const u8, 2 + header_count * 2);
    parts[0] = try std.fmt.allocPrint(allocator, "{d}", .{http.dusk_http_status_code(parser)});
    var reason_start: usize = 0;
    var reason_end: usize = 0;
    http.dusk_http_reason_span(parser, &reason_start, &reason_end);
    parts[1] = try validText(allocator, head[reason_start..reason_end]);
    const offsets = http.dusk_http_header_offsets(parser);
    for (0..header_count) |i| {
        const entry = offsets.?[i * http.offsets_per_header ..][0..http.offsets_per_header];
        parts[2 + i * 2] = head[entry[0]..entry[1]];
        parts[3 + i * 2] = try validText(allocator, head[entry[2]..entry[3]]);
    }
    return ok(try vm.allocStringArray(parts));
}

fn parseRequestHead(vm: *VM, args: []v.Value) anyerror!NativeResult {
    var arena = std.heap.ArenaAllocator.init(std.heap.smp_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const head = try allocator.dupe(u8, bytesArg(args, 0));
    const parser = http.dusk_http_request_parser_new() orelse return error.OutOfMemory;
    defer http.dusk_http_parser_free(parser);
    var used: usize = 0;
    if (http.dusk_http_feed_head(parser, head.ptr, head.len, &used) != .done) {
        return fail(HttpError.InvalidRequest);
    }
    const header_count = http.dusk_http_header_count(parser);
    const parts = try allocator.alloc([]const u8, 4 + header_count * 2);
    parts[0] = std.mem.span(http.dusk_http_method_name(parser));
    var target_start: usize = 0;
    var target_end: usize = 0;
    http.dusk_http_url_span(parser, &target_start, &target_end);
    parts[1] = head[target_start..target_end];
    parts[2] = try std.fmt.allocPrint(allocator, "{d}", .{http.dusk_http_version(parser)});
    parts[3] = if (http.dusk_http_head_keeps_alive(parser) != 0) "1" else "0";
    const offsets = http.dusk_http_header_offsets(parser);
    for (0..header_count) |i| {
        const entry = offsets.?[i * http.offsets_per_header ..][0..http.offsets_per_header];
        parts[4 + i * 2] = head[entry[0]..entry[1]];
        parts[5 + i * 2] = try validText(allocator, head[entry[2]..entry[3]]);
    }
    return ok(try vm.allocStringArray(parts));
}

fn requestBodyFailure(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const failure: RequestBodyFailure = blk: {
        const stream = vm.event_loop.streamAt(args[0].int64) orelse break :blk .none;
        const reader = stream.http_reader orelse break :blk .none;
        break :blk if (reader.was_body_too_large) .too_large else if (reader.was_body_invalid) .invalid else .none;
    };
    return ok(.{ .int64 = @intFromEnum(failure) });
}

fn markRequestBodyTooLarge(vm: *VM, args: []v.Value) anyerror!NativeResult {
    if (vm.event_loop.streamAt(args[0].int64)) |stream| {
        if (stream.http_reader) |reader| {
            reader.was_body_too_large = true;
        }
    }
    return ok(.{ .null = {} });
}

fn serverStateNew(vm: *VM, _: []v.Value) anyerror!NativeResult {
    return ok(.{ .int64 = @intCast(try vm.event_loop.addHttpServer()) });
}

fn serverSetIdle(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const server = vm.event_loop.httpServerAt(args[0].int64) orelse return ok(.{ .bool = false });
    const is_kept = try server.setIdle(vm.event_loop.allocator, @intCast(args[1].int64), args[2].bool);
    return ok(.{ .bool = is_kept });
}

fn serverClose(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const server = vm.event_loop.httpServerAt(args[0].int64) orelse return ok(.{ .null = {} });
    server.is_closing = true;
    var idle = server.takeIdleStreams();
    defer idle.deinit(vm.event_loop.allocator);
    for (idle.keys()) |handle| {
        vm.event_loop.closeStream(@intCast(handle));
    }
    return ok(.{ .null = {} });
}

fn serverIsClosing(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const server = vm.event_loop.httpServerAt(args[0].int64) orelse return ok(.{ .bool = true });
    return ok(.{ .bool = server.is_closing });
}

fn httpDate(vm: *VM, _: []v.Value) anyerror!NativeResult {
    const seconds: u64 = @intCast(@max(@divFloor(try EventLoop.wallTimeMilliseconds(), 1000), 0));
    var text: [29]u8 = undefined;
    return ok(try vm.allocBytesValue(formatHttpDate(seconds, &text), .string));
}

fn formatHttpDate(seconds: u64, text: *[29]u8) []const u8 {
    const day_names = [_][]const u8{ "Thu", "Fri", "Sat", "Sun", "Mon", "Tue", "Wed" };
    const month_names = [_][]const u8{ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };
    const epoch_seconds: std.time.epoch.EpochSeconds = .{ .secs = seconds };
    const epoch_day = epoch_seconds.getEpochDay();
    const year_day = epoch_day.calculateYearDay();
    const month_day = year_day.calculateMonthDay();
    const day_seconds = epoch_seconds.getDaySeconds();
    return std.fmt.bufPrint(text, "{s}, {d:0>2} {s} {d} {d:0>2}:{d:0>2}:{d:0>2} GMT", .{
        day_names[epoch_day.day % 7],
        month_day.day_index + 1,
        month_names[month_day.month.numeric() - 1],
        year_day.year,
        day_seconds.getHoursIntoDay(),
        day_seconds.getMinutesIntoHour(),
        day_seconds.getSecondsIntoMinute(),
    }) catch unreachable;
}

fn validText(allocator: std.mem.Allocator, bytes: []const u8) ![]const u8 {
    if (std.unicode.utf8ValidateSlice(bytes)) {
        return bytes;
    }
    var text: std.ArrayList(u8) = .empty;
    var index: usize = 0;
    while (index < bytes.len) {
        const length = std.unicode.utf8ByteSequenceLength(bytes[index]) catch 0;
        const is_valid_sequence = length > 0 and index + length <= bytes.len and std.unicode.utf8ValidateSlice(bytes[index .. index + length]);
        if (!is_valid_sequence) {
            try text.appendSlice(allocator, replacement_character);
            index += 1;
            continue;
        }
        try text.appendSlice(allocator, bytes[index .. index + length]);
        index += length;
    }
    return text.items;
}

fn isMethod(_: *VM, args: []v.Value) anyerror!NativeResult {
    const method = bytesArg(args, 0);
    for (method) |char| {
        if (!isTokenCharacter(char) or std.ascii.isLower(char)) {
            return ok(.{ .bool = false });
        }
    }
    return ok(.{ .bool = method.len > 0 });
}

fn isHeaderName(_: *VM, args: []v.Value) anyerror!NativeResult {
    const name = bytesArg(args, 0);
    for (name) |char| {
        if (!isTokenCharacter(char)) {
            return ok(.{ .bool = false });
        }
    }
    return ok(.{ .bool = name.len > 0 });
}

fn isTokenCharacter(char: u8) bool {
    if (std.ascii.isAlphanumeric(char)) {
        return true;
    }
    return std.mem.indexOfScalar(u8, "!#$%&'*+-.^_`|~", char) != null;
}

fn isHeaderValue(_: *VM, args: []v.Value) anyerror!NativeResult {
    for (bytesArg(args, 0)) |char| {
        const is_control = (char < ' ' and char != '\t') or char == 0x7f;
        if (is_control) {
            return ok(.{ .bool = false });
        }
    }
    return ok(.{ .bool = true });
}

fn base64(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const text = bytesArg(args, 0);
    const encoder = std.base64.standard.Encoder;
    const encoded = try std.heap.smp_allocator.alloc(u8, encoder.calcSize(text.len));
    defer std.heap.smp_allocator.free(encoded);
    _ = encoder.encode(encoded, text);
    return ok(try vm.allocBytesValue(encoded, .string));
}

fn userAgent(vm: *VM, _: []v.Value) anyerror!NativeResult {
    return ok(try vm.allocBytesValue(user_agent, .string));
}

fn hostSlotTake(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const event_loop = &vm.event_loop;
    const host = bytesArg(args, 0);
    const limit: ?usize = if (args[1].int64 <= 0) null else @intCast(args[1].int64);
    if (try event_loop.host_slots.tryTake(event_loop.allocator, host, limit)) {
        return ok(.{ .null = {} });
    }
    const current = vm.running_task.?;
    const timeout_ms: ?u64 = if (args[2].int64 < 0) null else @intCast(args[2].int64);
    try event_loop.parkForHostSlot(&vm.scheduler, current, bytesArg(args, 0), timeout_ms);
    current.park();
    return native.parked();
}

fn isLoopbackHost(_: *VM, args: []v.Value) anyerror!NativeResult {
    const raw = bytesArg(args, 0);
    const host = if (std.mem.endsWith(u8, raw, ".")) raw[0 .. raw.len - 1] else raw;
    if (std.ascii.eqlIgnoreCase(host, "localhost")) {
        return ok(.{ .bool = true });
    }
    if (host.len > ".localhost".len and std.ascii.endsWithIgnoreCase(host, ".localhost")) {
        return ok(.{ .bool = true });
    }
    if (std.Io.net.Ip4Address.parse(host, 0)) |ip4| {
        return ok(.{ .bool = ip4.bytes[0] == 127 });
    } else |_| {}
    const unbracketed = if (host.len >= 2 and host[0] == '[' and host[host.len - 1] == ']') host[1 .. host.len - 1] else host;
    if (std.Io.net.Ip6Address.parse(unbracketed, 0)) |ip6| {
        const b = ip6.bytes;
        const is_v6_loopback = std.mem.eql(u8, b[0..15], &([_]u8{0} ** 15)) and b[15] == 1;
        const is_v4_mapped_loopback = std.mem.eql(u8, b[0..10], &([_]u8{0} ** 10)) and b[10] == 0xff and b[11] == 0xff and b[12] == 127;
        return ok(.{ .bool = is_v6_loopback or is_v4_mapped_loopback });
    } else |_| {}
    return ok(.{ .bool = false });
}

fn hostSlotRelease(vm: *VM, args: []v.Value) anyerror!NativeResult {
    vm.event_loop.releaseHostSlot(bytesArg(args, 0));
    return ok(.{ .null = {} });
}

fn streamHoldHostSlot(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const stream = vm.event_loop.streamAt(args[0].int64) orelse {
        vm.event_loop.releaseHostSlot(bytesArg(args, 1));
        return ok(.{ .null = {} });
    };
    stream.holdHostSlot(bytesArg(args, 1)) catch {
        vm.event_loop.releaseHostSlot(bytesArg(args, 1));
    };
    return ok(.{ .null = {} });
}

fn streamForbidReuse(vm: *VM, args: []v.Value) anyerror!NativeResult {
    if (vm.event_loop.streamAt(args[0].int64)) |stream| {
        stream.is_reuse_forbidden = true;
    }
    return ok(.{ .null = {} });
}

fn streamHasFinalStatus(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const stream = vm.event_loop.streamAt(args[0].int64) orelse return ok(.{ .bool = false });
    const head = stream.bufferedBytes();
    const prefix = "HTTP/1.";
    const has_status = head.len >= prefix.len + 3 and std.mem.startsWith(u8, head, prefix);
    if (!has_status) {
        return ok(.{ .bool = false });
    }
    return ok(.{ .bool = head[prefix.len + 2] != '1' });
}

fn encodedUrl(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const text = bytesArg(args, 0);
    const rest_start = urlPathStart(text);
    var extra: usize = 0;
    for (text[rest_start..]) |byte| {
        if (needsUrlEscape(byte)) {
            extra += 2;
        }
    }
    if (extra == 0) {
        return ok(args[0]);
    }
    const allocator = std.heap.smp_allocator;
    const encoded = try allocator.alloc(u8, text.len + extra);
    defer allocator.free(encoded);
    @memcpy(encoded[0..rest_start], text[0..rest_start]);
    var at = rest_start;
    for (text[rest_start..]) |byte| {
        if (!needsUrlEscape(byte)) {
            encoded[at] = byte;
            at += 1;
            continue;
        }
        const digits = "0123456789ABCDEF";
        encoded[at] = '%';
        encoded[at + 1] = digits[byte >> 4];
        encoded[at + 2] = digits[byte & 0x0f];
        at += 3;
    }
    return ok(try vm.allocBytesValue(encoded, .string));
}

fn urlPathStart(text: []const u8) usize {
    const scheme_end = std.mem.indexOf(u8, text, "://") orelse return 0;
    const authority_start = scheme_end + 3;
    const authority_end = std.mem.indexOfAnyPos(u8, text, authority_start, "/?#") orelse return text.len;
    return authority_end;
}

fn needsUrlEscape(byte: u8) bool {
    if (byte <= 0x20 or byte >= 0x7f) {
        return true;
    }
    return std.mem.indexOfScalar(u8, "\"<>\\^`{|}", byte) != null;
}

fn invalidCompressionLevel(_: *VM, args: []v.Value) anyerror!NativeResult {
    return RuntimeErrorReporter.httpInvalidCompressionLevel(args[0].int64);
}
