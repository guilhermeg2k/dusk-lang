const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const native = @import("../native.zig");
const io = @import("io.zig");
const net = @import("net.zig");
const http = @import("../../tasks/http.zig");
const ReadMode = @import("../../tasks/stream.zig").ReadMode;
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
const flate = std.compress.flate;
const gunzip_chunk_size = 16 * 1024;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "tcp_connect_handle", Binding{ .native = &net.tcpConnect } },
    .{ "stream_start_tls", Binding{ .native = &net.streamStartTls } },
    .{ "stream_write", Binding{ .native = &io.streamWrite } },
    .{ "stream_write_bytes", Binding{ .native = &io.streamWrite } },
    .{ "stream_close", Binding{ .native = &io.streamClose } },
    .{ "stream_read_head", Binding{ .native = &streamReadHead } },
    .{ "stream_read_whole_body", Binding{ .native = &streamReadWholeBody } },
    .{ "stream_body_bytes", Binding{ .native = &streamBodyBytes } },
    .{ "stream_body_line", Binding{ .native = &streamBodyLine } },
    .{ "stream_body_all", Binding{ .native = &streamBodyAll } },
    .{ "stream_body_all_bytes", Binding{ .native = &streamBodyAllBytes } },
    .{ "parse_head", Binding{ .native = &parseHead } },
    .{ "gunzip", Binding{ .native = &gunzip } },
    .{ "pool_take", Binding{ .native = &poolTake } },
    .{ "pool_put", Binding{ .native = &poolPut } },
    .{ "is_method", Binding{ .native = &isMethod } },
    .{ "is_header_name", Binding{ .native = &isHeaderName } },
    .{ "is_header_value", Binding{ .native = &isHeaderValue } },
    .{ "body_given_twice", Binding{ .native = &bodyGivenTwice } },
});

fn streamReadHead(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const stream = vm.event_loop.streamAt(args[0].int64) orelse return fail(HttpError.Closed);
    if (stream.waiter != null) {
        return fail(HttpError.Other);
    }
    stream.startHttpResponse(args[1].bool) catch return fail(HttpError.Other);
    return readHttp(vm, args[0].int64, .http_head, args[2].int64);
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

fn gunzip(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const allocator = std.heap.smp_allocator;
    const cap: usize = @intCast(@max(args[1].int64, 0));
    const window = try allocator.alloc(u8, flate.max_window_len);
    defer allocator.free(window);
    var input: std.Io.Reader = .fixed(bytesArg(args, 0));
    var decompress: flate.Decompress = .init(&input, .gzip, window);
    var plain: std.ArrayList(u8) = .empty;
    defer plain.deinit(allocator);
    var chunk: [gunzip_chunk_size]u8 = undefined;
    while (true) {
        const count = decompress.reader.readSliceShort(&chunk) catch return fail(HttpError.InvalidResponse);
        if (count == 0) break;
        if (plain.items.len + count > cap) {
            return fail(HttpError.BodyTooLarge);
        }
        try plain.appendSlice(allocator, chunk[0..count]);
    }
    if (!matchesGzipFooter(&decompress, plain.items)) {
        return fail(HttpError.InvalidResponse);
    }
    return ok(try vm.allocBytesValue(plain.items, .buffer));
}

fn matchesGzipFooter(decompress: *const flate.Decompress, plain: []const u8) bool {
    if (decompress.state != .end) {
        return false;
    }
    const footer = decompress.container_metadata.gzip;
    return footer.crc == std.hash.Crc32.hash(plain) and footer.count == @as(u32, @truncate(plain.len));
}

fn poolTake(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const event_loop = &vm.event_loop;
    try event_loop.poll();
    const key = bytesArg(args, 0);
    while (event_loop.connection_pool.takeNewest(key)) |handle| {
        const stream = event_loop.streamAt(@intCast(handle)) orelse continue;
        stream.leavePool();
        if (!stream.isReusable()) {
            event_loop.closeStream(@intCast(handle));
            continue;
        }
        return ok(.{ .int64 = @intCast(handle) });
    }
    return ok(.{ .int64 = -1 });
}

fn poolPut(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const event_loop = &vm.event_loop;
    const handle = args[1].int64;
    const stream = event_loop.streamAt(handle) orelse return ok(.{ .null = {} });
    if (!stream.isReusable()) {
        event_loop.closeStream(handle);
        return ok(.{ .null = {} });
    }
    stream.enterPool(bytesArg(args, 0)) catch {
        stream.leavePool();
        event_loop.closeStream(handle);
        return ok(.{ .null = {} });
    };
    const evicted = event_loop.connection_pool.add(event_loop.allocator, bytesArg(args, 0), @intCast(handle)) catch {
        stream.leavePool();
        event_loop.closeStream(handle);
        return ok(.{ .null = {} });
    };
    const oldest = evicted orelse return ok(.{ .null = {} });
    if (event_loop.streamAt(@intCast(oldest))) |oldest_stream| {
        oldest_stream.leavePool();
    }
    event_loop.closeStream(@intCast(oldest));
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

fn bodyGivenTwice(_: *VM, _: []v.Value) anyerror!NativeResult {
    return RuntimeErrorReporter.httpBodyGivenTwice();
}
