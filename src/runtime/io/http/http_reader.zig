const std = @import("std");
const http = @import("http.zig");
const compression = @import("../compression/compression.zig");
const IoError = @import("../stream/stream.zig").IoError;
const HttpError = http.HttpError;

pub const max_head_size = 64 * 1024;
const decode_chunk_size = 64 * 1024;

pub const HttpReader = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    parser: *http.Parser,
    is_request: bool,
    head_fed: usize = 0,
    body_wire_bytes: usize = 0,
    body: std.ArrayList(u8) = .empty,
    body_offset: usize = 0,
    taken: usize = 0,
    has_sent_continue: bool = false,
    was_body_too_large: bool = false,
    was_body_invalid: bool = false,
    decoder: ?*compression.Zip = null,
    decoder_format: compression.Format = .gzip,
    plain: std.ArrayList(u8) = .empty,
    plain_offset: usize = 0,
    has_decoded_any: bool = false,
    is_decoder_at_end: bool = false,
    has_pending_decoder_output: bool = false,

    pub fn create(allocator: std.mem.Allocator, kind: MessageKind) !*Self {
        const reader = try allocator.create(Self);
        errdefer allocator.destroy(reader);
        reader.* = .{ .allocator = allocator, .parser = try newParser(kind), .is_request = kind == .request };
        return reader;
    }

    pub fn destroy(self: *Self) void {
        http.dusk_http_parser_free(self.parser);
        compression.dusk_zip_free(self.decoder);
        self.body.deinit(self.allocator);
        self.plain.deinit(self.allocator);
        self.allocator.destroy(self);
    }

    pub fn startMessage(self: *Self, kind: MessageKind) !void {
        const parser = try newParser(kind);
        http.dusk_http_parser_free(self.parser);
        self.parser = parser;
        self.is_request = kind == .request;
        self.head_fed = 0;
        self.body_wire_bytes = 0;
        self.body.clearRetainingCapacity();
        self.body_offset = 0;
        self.taken = 0;
        self.has_sent_continue = false;
        self.was_body_too_large = false;
        self.was_body_invalid = false;
        self.has_pending_decoder_output = false;
        self.stopDecoding();
    }

    pub fn releaseBody(self: *Self) void {
        self.body.clearAndFree(self.allocator);
        self.body_offset = 0;
        self.taken = 0;
        self.stopDecoding();
        self.plain.clearAndFree(self.allocator);
    }

    pub fn startDecoding(self: *Self, format: compression.Format) !void {
        self.stopDecoding();
        self.decoder = compression.dusk_zip_decompressor_new(format) orelse return error.OutOfMemory;
        self.decoder_format = format;
    }

    fn stopDecoding(self: *Self) void {
        compression.dusk_zip_free(self.decoder);
        self.decoder = null;
        self.plain.clearRetainingCapacity();
        self.plain_offset = 0;
        self.has_decoded_any = false;
        self.is_decoder_at_end = false;
    }

    pub fn pendingBody(self: *const Self) []const u8 {
        if (self.decoder != null) {
            return self.plain.items[self.plain_offset..];
        }
        return self.body.items[self.body_offset..];
    }

    pub fn dropTakenBody(self: *Self) void {
        if (self.decoder != null) {
            self.dropTakenPlain();
            return;
        }
        self.body_offset += self.taken;
        self.taken = 0;
        if (self.body_offset == 0) return;
        const remaining = self.body.items.len - self.body_offset;
        std.mem.copyForwards(u8, self.body.items[0..remaining], self.body.items[self.body_offset..]);
        self.body.shrinkRetainingCapacity(remaining);
        self.body_offset = 0;
    }

    fn dropTakenPlain(self: *Self) void {
        self.plain_offset += self.taken;
        self.taken = 0;
        if (self.plain_offset == 0) return;
        const remaining = self.plain.items.len - self.plain_offset;
        std.mem.copyForwards(u8, self.plain.items[0..remaining], self.plain.items[self.plain_offset..]);
        self.plain.shrinkRetainingCapacity(remaining);
        self.plain_offset = 0;
    }

    pub fn decodeBody(self: *Self, limit: usize) error{ InvalidBody, OutOfMemory }!void {
        if (self.decoder == null) return;
        defer self.dropDecodedBody();
        while (self.hasUndecodedBody()) {
            const plain_pending = self.plain.items.len - self.plain_offset;
            if (plain_pending >= limit) return;
            if (self.is_decoder_at_end) {
                if (self.decoder_format != .gzip or compression.dusk_zip_restart(self.decoder.?) != .ok) {
                    return error.InvalidBody;
                }
                self.is_decoder_at_end = false;
            }
            try self.plain.ensureUnusedCapacity(self.allocator, decode_chunk_size);
            const raw = self.body.items[self.body_offset..];
            const room = self.plain.unusedCapacitySlice();
            var consumed: usize = 0;
            var produced: usize = 0;
            const status = compression.dusk_zip_decompress(self.decoder.?, raw.ptr, raw.len, room.ptr, room.len, &consumed, &produced);
            if (status == .invalid and self.canRetryAsRawDeflate()) {
                try self.startDecoding(.deflate);
                continue;
            }
            self.plain.items.len += produced;
            self.body_offset += consumed;
            self.has_decoded_any = self.has_decoded_any or consumed > 0;
            switch (status) {
                .done => {
                    self.is_decoder_at_end = true;
                    self.has_pending_decoder_output = false;
                },
                .ok => {
                    self.has_pending_decoder_output = produced == room.len;
                    if (consumed == 0 and produced == 0) {
                        if (raw.len == 0) break;
                        return error.InvalidBody;
                    }
                },
                .out_of_memory => return error.OutOfMemory,
                else => return error.InvalidBody,
            }
        }
    }

    fn canRetryAsRawDeflate(self: *const Self) bool {
        return self.decoder_format == .zlib and !self.has_decoded_any and self.plain.items.len == 0;
    }

    fn dropDecodedBody(self: *Self) void {
        if (self.body_offset == 0) return;
        const remaining = self.body.items.len - self.body_offset;
        std.mem.copyForwards(u8, self.body.items[0..remaining], self.body.items[self.body_offset..]);
        self.body.shrinkRetainingCapacity(remaining);
        self.body_offset = 0;
    }

    pub fn hasUndecodedBody(self: *const Self) bool {
        return self.decoder != null and (self.body.items.len > self.body_offset or self.has_pending_decoder_output);
    }

    pub fn isDecodedToEnd(self: *const Self) bool {
        if (self.decoder == null) {
            return true;
        }
        return self.is_decoder_at_end or !self.has_decoded_any;
    }

    pub fn feedHead(self: *Self, available: []const u8) HeadStep {
        if (self.head_fed >= available.len) {
            return .want_more;
        }
        const input = available[self.head_fed..];
        var used: usize = 0;
        const status = http.dusk_http_feed_head(self.parser, input.ptr, input.len, &used);
        self.head_fed += used;
        return switch (status) {
            .done => .{ .done = self.head_fed },
            .want_more => .want_more,
            .out_of_memory => .{ .failure = .Other },
            else => .{ .failure = self.invalidMessage() },
        };
    }

    pub fn invalidMessage(self: *const Self) HttpError {
        return if (self.is_request) .InvalidRequest else .InvalidResponse;
    }

    pub fn feedBody(self: *Self, input: []const u8) error{ InvalidResponse, OutOfMemory }!usize {
        try self.body.ensureUnusedCapacity(self.allocator, input.len);
        const out = self.body.unusedCapacitySlice();
        var used: usize = 0;
        var produced: usize = 0;
        const status = http.dusk_http_feed_body(self.parser, input.ptr, input.len, out.ptr, &used, &produced);
        self.body.items.len += produced;
        return switch (status) {
            .done, .want_more => used,
            .out_of_memory => error.OutOfMemory,
            else => error.InvalidResponse,
        };
    }

    pub fn finishBody(self: *Self) bool {
        return http.dusk_http_finish_body(self.parser) == .done;
    }

    pub fn isMessageComplete(self: *const Self) bool {
        return http.dusk_http_is_message_complete(self.parser) != 0;
    }

    pub fn canKeepAlive(self: *const Self) bool {
        return http.dusk_http_can_keep_alive(self.parser) != 0;
    }
};

pub const MessageKind = enum { request, response, head_response };

pub const HeadStep = union(enum) {
    done: usize,
    want_more,
    failure: HttpError,
};

fn newParser(kind: MessageKind) error{OutOfMemory}!*http.Parser {
    const parser = switch (kind) {
        .request => http.dusk_http_request_parser_new(),
        .response => http.dusk_http_response_parser_new(0),
        .head_response => http.dusk_http_response_parser_new(1),
    };
    return parser orelse error.OutOfMemory;
}

pub fn httpErrorFromIo(failure: IoError) HttpError {
    return switch (failure) {
        .Closed => .Closed,
        .BrokenPipe, .ConnectionReset => .ConnectionReset,
        .TimedOut => .TimedOut,
        .UnexpectedEnd => .InvalidResponse,
        .HandshakeFailed => .HandshakeFailed,
        .Busy, .LineTooLong, .InvalidUtf8, .TooLarge, .NotTerminal, .Other => .Other,
    };
}
