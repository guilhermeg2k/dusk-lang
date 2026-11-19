const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const EventLoop = @import("../../tasks/event_loop.zig").EventLoop;
const Civil = @import("../../calendar.zig").Civil;
const native = @import("../native.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "now_ms", Binding{ .native = &nowMilliseconds } },
    .{ "monotonic_ns", Binding{ .native = &monotonicNanoseconds } },
    .{ "format_timestamp", Binding{ .native = &formatTimestamp } },
    .{ "parse_timestamp", Binding{ .native = &parseTimestamp } },
});

pub const TimeError = enum { InvalidTimestamp, Overflow };
pub const error_sets = .{TimeError};

fn nowMilliseconds(_: *VM, _: []v.Value) anyerror!NativeResult {
    return native.ok(.{ .int64 = try EventLoop.wallTimeMilliseconds() });
}

fn monotonicNanoseconds(_: *VM, _: []v.Value) anyerror!NativeResult {
    return native.ok(.{ .int64 = try EventLoop.monotonicTimeNanoseconds() });
}

fn formatTimestamp(vm: *VM, args: []v.Value) anyerror!NativeResult {
    var buffer: [32]u8 = undefined;
    const text = formatUnixMilliseconds(args[0].int64, &buffer);
    const result = try vm.allocString(text.len);
    @memcpy(result.mutableSlice(), text);
    return native.stringResult(result);
}

fn formatUnixMilliseconds(milliseconds: i64, buffer: *[32]u8) []const u8 {
    const civil = Civil.fromUnixMilliseconds(milliseconds);
    const sign: []const u8 = if (civil.year < 0) "-" else if (civil.year > 9999) "+" else "";
    return std.fmt.bufPrint(buffer, "{s}{d:0>4}-{d:0>2}-{d:0>2}T{d:0>2}:{d:0>2}:{d:0>2}.{d:0>3}Z", .{
        sign,
        @abs(civil.year),
        civil.month,
        civil.day,
        civil.hour,
        civil.minute,
        civil.second,
        civil.millisecond,
    }) catch unreachable;
}

fn parseTimestamp(_: *VM, args: []v.Value) anyerror!NativeResult {
    var parser = IsoParser{ .text = native.bytesArg(args, 0) };
    const milliseconds = parser.parse() catch |err| return switch (err) {
        error.InvalidTimestamp => native.fail(TimeError.InvalidTimestamp),
        error.Overflow => native.fail(TimeError.Overflow),
    };
    return native.ok(.{ .int64 = milliseconds });
}

const IsoParser = struct {
    text: []const u8,
    cursor: usize = 0,

    fn parse(self: *IsoParser) error{ InvalidTimestamp, Overflow }!i64 {
        const is_negative_year = self.take('-');
        const has_positive_sign = if (is_negative_year) false else self.take('+');
        const year_start = self.cursor;
        var year: i64 = 0;
        var year_remainder: i64 = 0;
        var has_year_overflow = false;
        while (self.cursor < self.text.len and std.ascii.isDigit(self.text[self.cursor])) {
            const digit = self.text[self.cursor] - '0';
            year_remainder = @mod(year_remainder * 10 + digit, 400);
            if (year > 1000000000) {
                has_year_overflow = true;
            } else {
                year = year * 10 + digit;
            }
            self.cursor += 1;
        }
        const year_digits = self.cursor - year_start;
        const has_signed_year = is_negative_year or has_positive_sign;
        const has_valid_year = if (has_signed_year) year_digits >= 4 else year_digits == 4;
        if (!has_valid_year) {
            return error.InvalidTimestamp;
        }
        const has_invalid_positive_year = has_positive_sign and (year_digits < 5 or year <= 9999);
        if (has_invalid_positive_year) {
            return error.InvalidTimestamp;
        }
        try self.expect('-');
        const month = try self.twoDigits();
        try self.expect('-');
        const day = try self.twoDigits();
        try self.expect('T');
        const hour = try self.twoDigits();
        try self.expect(':');
        const minute = try self.twoDigits();
        try self.expect(':');
        const second = try self.twoDigits();
        var millisecond: u16 = 0;
        if (self.take('.')) {
            const fraction_start = self.cursor;
            while (self.cursor < self.text.len and std.ascii.isDigit(self.text[self.cursor])) {
                if (self.cursor - fraction_start < 3) {
                    millisecond = millisecond * 10 + self.text[self.cursor] - '0';
                }
                self.cursor += 1;
            }
            const digits = self.cursor - fraction_start;
            if (digits == 0) {
                return error.InvalidTimestamp;
            }
            if (digits == 1) {
                millisecond *= 100;
            } else if (digits == 2) {
                millisecond *= 10;
            }
        }
        var offset_seconds: i32 = 0;
        if (!self.take('Z')) {
            const is_negative_offset = self.take('-');
            if (!is_negative_offset) {
                try self.expect('+');
            }
            const offset_hour = try self.twoDigits();
            try self.expect(':');
            const offset_minute = try self.twoDigits();
            const has_valid_offset = offset_hour < 24 and offset_minute < 60;
            if (!has_valid_offset) {
                return error.InvalidTimestamp;
            }
            offset_seconds = @as(i32, offset_hour) * 3600 + @as(i32, offset_minute) * 60;
            if (is_negative_offset) {
                offset_seconds = -offset_seconds;
            }
        }
        if (self.cursor != self.text.len) {
            return error.InvalidTimestamp;
        }
        const calendar_year = if (has_year_overflow) 1000000000 + year_remainder else year;
        const civil = Civil{
            .year = if (is_negative_year) -calendar_year else calendar_year,
            .month = month,
            .day = day,
            .hour = hour,
            .minute = minute,
            .second = second,
            .millisecond = millisecond,
        };
        if (!civil.isValid()) {
            return error.InvalidTimestamp;
        }
        if (has_year_overflow) {
            return error.Overflow;
        }
        return civil.toUnixMilliseconds(offset_seconds);
    }

    fn take(self: *IsoParser, byte: u8) bool {
        const has_byte = self.cursor < self.text.len and self.text[self.cursor] == byte;
        if (!has_byte) {
            return false;
        }
        self.cursor += 1;
        return true;
    }

    fn expect(self: *IsoParser, byte: u8) error{InvalidTimestamp}!void {
        if (!self.take(byte)) {
            return error.InvalidTimestamp;
        }
    }

    fn twoDigits(self: *IsoParser) error{InvalidTimestamp}!u8 {
        if (self.text.len - self.cursor < 2) {
            return error.InvalidTimestamp;
        }
        const digits = self.text[self.cursor..][0..2];
        const are_digits = std.ascii.isDigit(digits[0]) and std.ascii.isDigit(digits[1]);
        if (!are_digits) {
            return error.InvalidTimestamp;
        }
        self.cursor += 2;
        return (digits[0] - '0') * 10 + digits[1] - '0';
    }
};
