const std = @import("std");
const http = @import("http.zig");
const IoError = @import("stream.zig").IoError;
const HttpError = http.HttpError;

pub const max_head_size = 64 * 1024;

pub const HttpReader = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    parser: *http.Parser,
    head_fed: usize = 0,
    body: std.ArrayList(u8) = .empty,
    body_offset: usize = 0,
    taken: usize = 0,

    pub fn create(allocator: std.mem.Allocator, is_head_request: bool) !*Self {
        const reader = try allocator.create(Self);
        errdefer allocator.destroy(reader);
        reader.* = .{ .allocator = allocator, .parser = try newParser(is_head_request) };
        return reader;
    }

    pub fn destroy(self: *Self) void {
        http.dusk_http_parser_free(self.parser);
        self.body.deinit(self.allocator);
        self.allocator.destroy(self);
    }

    pub fn startResponse(self: *Self, is_head_request: bool) !void {
        const parser = try newParser(is_head_request);
        http.dusk_http_parser_free(self.parser);
        self.parser = parser;
        self.head_fed = 0;
        self.body.clearRetainingCapacity();
        self.body_offset = 0;
        self.taken = 0;
    }

    pub fn releaseBody(self: *Self) void {
        self.body.clearAndFree(self.allocator);
        self.body_offset = 0;
        self.taken = 0;
    }

    pub fn pendingBody(self: *const Self) []const u8 {
        return self.body.items[self.body_offset..];
    }

    pub fn dropTakenBody(self: *Self) void {
        self.body_offset += self.taken;
        self.taken = 0;
        if (self.body_offset == 0) return;
        const remaining = self.body.items.len - self.body_offset;
        std.mem.copyForwards(u8, self.body.items[0..remaining], self.body.items[self.body_offset..]);
        self.body.shrinkRetainingCapacity(remaining);
        self.body_offset = 0;
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
            else => .{ .failure = .InvalidResponse },
        };
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

pub const HeadStep = union(enum) {
    done: usize,
    want_more,
    failure: HttpError,
};

fn newParser(is_head_request: bool) error{OutOfMemory}!*http.Parser {
    return http.dusk_http_response_parser_new(@intFromBool(is_head_request)) orelse error.OutOfMemory;
}

pub fn httpErrorFromIo(failure: IoError) HttpError {
    return switch (failure) {
        .Closed => .Closed,
        .BrokenPipe, .ConnectionReset => .ConnectionReset,
        .TimedOut => .TimedOut,
        .UnexpectedEnd => .InvalidResponse,
        .HandshakeFailed => .HandshakeFailed,
        .Busy, .LineTooLong, .InvalidUtf8, .Other => .Other,
    };
}
