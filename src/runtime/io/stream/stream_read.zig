const std = @import("std");
const uv = @import("../../tasks/loop/uv.zig");
const v = @import("../../value.zig");
const WakeResult = @import("../../tasks/scheduler.zig").WakeResult;
const NetError = @import("../net/tcp.zig").NetError;
const HttpError = @import("../http/http.zig").HttpError;
const httpErrorFromIo = @import("../http/http_reader.zig").httpErrorFromIo;

const min_decoded_bytes = 64 * 1024;

pub const IoError = enum {
    Closed,
    Busy,
    BrokenPipe,
    ConnectionReset,
    LineTooLong,
    InvalidUtf8,
    TimedOut,
    UnexpectedEnd,
    HandshakeFailed,
    TooLarge,
    NotTerminal,
    Other,
};

pub const ReadMode = union(enum) {
    bytes: usize,
    exact: usize,
    line: ?usize,
    all_text: ?usize,
    all_bytes: ?usize,
    any_data,
    http_head,
    http_body: BodyRead,
    http_whole_body: usize,

    pub fn valueKind(self: ReadMode) v.ValueType {
        return switch (self) {
            .line, .all_text => .string,
            .bytes, .exact, .all_bytes, .any_data, .http_head, .http_whole_body => .buffer,
            .http_body => |read| switch (read) {
                .line, .all_text => .string,
                .bytes, .all_bytes => .buffer,
            },
        };
    }

    pub fn wakeFailure(self: ReadMode, failure: IoError) WakeResult {
        return switch (self) {
            .http_head, .http_whole_body => httpFailureResult(httpErrorFromIo(failure)),
            else => failureResult(failure),
        };
    }
};

pub const BodyRead = union(enum) {
    bytes: usize,
    line: ?usize,
    all_text,
    all_bytes,

    pub fn decodeLimit(self: BodyRead) usize {
        return switch (self) {
            .bytes => |max| @max(max, min_decoded_bytes),
            .line, .all_text, .all_bytes => std.math.maxInt(usize),
        };
    }
};

pub const ReadOutcome = struct {
    result: union(enum) {
        data: []const u8,
        end,
        failure: IoError,
        http_failure: HttpError,
    },
    consumed: usize = 0,
};

pub const ReadSource = struct {
    available: []const u8,
    has_ended: bool,
    failure: ?IoError,

    pub fn bytesOutcome(self: ReadSource, max: usize) ?ReadOutcome {
        const available = self.available;
        if (available.len > 0) {
            const count = @min(available.len, max);
            return .{ .result = .{ .data = available[0..count] }, .consumed = count };
        }
        return self.endOutcome();
    }

    pub fn exactOutcome(self: ReadSource, count: usize) ?ReadOutcome {
        const available = self.available;
        if (available.len >= count) {
            return .{ .result = .{ .data = available[0..count] }, .consumed = count };
        }
        const has_ended_partway = self.has_ended and available.len > 0 and self.failure == null;
        if (has_ended_partway) {
            return .{ .result = .{ .failure = .UnexpectedEnd } };
        }
        return self.endOutcome();
    }

    pub fn lineOutcome(self: ReadSource, max: ?usize) ?ReadOutcome {
        const available = self.available;
        if (std.mem.indexOfScalar(u8, available, '\n')) |newline| {
            return lineFrom(available[0..newline], newline + 1, max);
        }
        if (max) |limit| {
            const might_end_with_crlf = available.len == limit + 1 and available[limit] == '\r' and !self.has_ended;
            if (available.len > limit and !might_end_with_crlf) {
                return .{ .result = .{ .failure = .LineTooLong } };
            }
        }
        const is_last_line = available.len > 0 and self.has_ended;
        if (is_last_line) {
            return lineFrom(available, available.len, max);
        }
        return self.endOutcome();
    }

    pub fn allOutcome(self: ReadSource, needs_utf8: bool, max_bytes: ?usize) ?ReadOutcome {
        if (self.failure) |failure| {
            return .{ .result = .{ .failure = failure } };
        }
        if (max_bytes) |limit| {
            if (self.available.len > limit) {
                return .{ .result = .{ .failure = .TooLarge } };
            }
        }
        if (!self.has_ended) return null;
        const available = self.available;
        if (needs_utf8 and !std.unicode.utf8ValidateSlice(available)) {
            return .{ .result = .{ .failure = .InvalidUtf8 }, .consumed = available.len };
        }
        return .{ .result = .{ .data = available }, .consumed = available.len };
    }

    pub fn anyDataOutcome(self: ReadSource) ?ReadOutcome {
        if (self.available.len > 0) {
            return .{ .result = .{ .data = self.available[0..0] } };
        }
        return self.endOutcome();
    }

    fn endOutcome(self: ReadSource) ?ReadOutcome {
        if (self.failure) |failure| {
            return .{ .result = .{ .failure = failure } };
        }
        if (self.has_ended) {
            return .{ .result = .end };
        }
        return null;
    }
};

fn lineFrom(raw: []const u8, consumed: usize, max: ?usize) ReadOutcome {
    const has_carriage_return = raw.len > 0 and raw[raw.len - 1] == '\r';
    const line = if (has_carriage_return) raw[0 .. raw.len - 1] else raw;
    if (max) |limit| {
        if (line.len > limit) {
            return .{ .result = .{ .failure = .LineTooLong } };
        }
    }
    if (!std.unicode.utf8ValidateSlice(line)) {
        return .{ .result = .{ .failure = .InvalidUtf8 }, .consumed = consumed };
    }
    return .{ .result = .{ .data = line }, .consumed = consumed };
}

pub fn httpFailure(failure: HttpError) ReadOutcome {
    return .{ .result = .{ .http_failure = failure } };
}

const io_errors_by_name = std.StaticStringMap(IoError).initComptime(.{
    .{ "EPIPE", .BrokenPipe },
    .{ "ECONNRESET", .ConnectionReset },
    .{ "ECANCELED", .Closed },
});

pub fn ioErrorFromStatus(status: c_int) IoError {
    return io_errors_by_name.get(std.mem.span(uv.uv_err_name(status))) orelse .Other;
}

pub fn isErrorNamed(status: c_int, name: []const u8) bool {
    return std.mem.eql(u8, std.mem.span(uv.uv_err_name(status)), name);
}

pub fn failureResult(failure: IoError) WakeResult {
    return .{ .value = .{ .int64 = @intFromEnum(failure) }, .kind = .int64, .has_failed = true, .error_is_relative = true };
}

pub fn netFailureResult(failure: NetError) WakeResult {
    return .{ .value = .{ .int64 = @intFromEnum(failure) }, .kind = .int64, .has_failed = true, .error_is_relative = true };
}

pub fn httpFailureResult(failure: HttpError) WakeResult {
    return .{ .value = .{ .int64 = @intFromEnum(failure) }, .kind = .int64, .has_failed = true, .error_is_relative = true };
}

pub fn doneResult() WakeResult {
    return .{ .value = v.NULL_VALUE, .kind = .null };
}
