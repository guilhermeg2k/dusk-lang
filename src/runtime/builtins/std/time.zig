const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const Civil = @import("../../calendar.zig").Civil;
const formatIsoMilliseconds = @import("../../time_format.zig").formatIsoMilliseconds;
const PatternFormatter = @import("../../time_format.zig").PatternFormatter;
const LocalJob = @import("../../io/time/timezone_job.zig").LocalJob;
const RuntimeError = @import("../../runtime_error.zig").RuntimeError;
const RuntimeErrorReporter = @import("../../runtime_error.zig").RuntimeErrorReporter;
const native = @import("../native.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "now_ms", Binding{ .native = &nowMilliseconds } },
    .{ "monotonic_ns", Binding{ .native = &monotonicNanoseconds } },
    .{ "format_timestamp", Binding{ .native = &formatTimestamp } },
    .{ "format_utc_timestamp", Binding{ .native = &formatUtcTimestamp } },
    .{ "format_timestamp_pattern", Binding{ .native = &formatTimestampPattern } },
    .{ "parse_timestamp", Binding{ .native = &parseTimestamp } },
    .{ "scale_nanoseconds", Binding{ .native = &scaleNanoseconds } },
    .{ "add_nanoseconds", Binding{ .native = &addNanoseconds } },
    .{ "subtract_nanoseconds", Binding{ .native = &subtractNanoseconds } },
    .{ "add_timestamp_duration", Binding{ .native = &addTimestampDuration } },
    .{ "subtract_timestamp_duration", Binding{ .native = &subtractTimestampDuration } },
    .{ "timestamp_difference_nanoseconds", Binding{ .native = &timestampDifferenceNanoseconds } },
    .{ "load_timezone_id", Binding{ .native = &loadTimezoneId } },
    .{ "timezone_name", Binding{ .native = &timezoneName } },
    .{ "timezone_offset_seconds", Binding{ .native = &timezoneOffsetSeconds } },
    .{ "local_timezone_id", Binding{ .native = &localTimezoneId } },
});

pub const TimeError = @import("../../time_error.zig").TimeError;
pub const error_sets = .{TimeError};

pub fn wallTimeMilliseconds(io: std.Io) RuntimeError!i64 {
    return wallMillisecondsFromNanoseconds(std.Io.Clock.real.now(io).nanoseconds);
}

pub fn wallMillisecondsFromNanoseconds(nanoseconds: i96) RuntimeError!i64 {
    const milliseconds = @divFloor(nanoseconds, std.time.ns_per_ms);
    return std.math.cast(i64, milliseconds) orelse RuntimeErrorReporter.clockOutOfRange("wall");
}

pub fn monotonicTimeNanoseconds(io: std.Io) RuntimeError!i64 {
    return monotonicNanosecondsFromReading(std.Io.Clock.awake.now(io).nanoseconds);
}

pub fn monotonicNanosecondsFromReading(nanoseconds: i96) RuntimeError!i64 {
    return std.math.cast(i64, nanoseconds) orelse RuntimeErrorReporter.clockOutOfRange("monotonic");
}

fn nowMilliseconds(vm: *VM, _: []v.Value) anyerror!NativeResult {
    return native.ok(.{ .int64 = try wallTimeMilliseconds(vm.io.?) });
}

fn monotonicNanoseconds(vm: *VM, _: []v.Value) anyerror!NativeResult {
    return native.ok(.{ .int64 = try monotonicTimeNanoseconds(vm.io.?) });
}

fn formatTimestamp(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const milliseconds = args[0].int64;
    const offset_seconds = vm.timezones.offsetSeconds(args[1].int64, milliseconds);
    var buffer: [40]u8 = undefined;
    const text = formatIsoMilliseconds(milliseconds, offset_seconds, &buffer) catch return native.fail(TimeError.PrecisionLoss);
    return allocateTimestampString(vm, text);
}

fn allocateTimestampString(vm: *VM, text: []const u8) !NativeResult {
    const result = try vm.allocString(text.len);
    @memcpy(result.mutableSlice(), text);
    return native.stringResult(result);
}

fn formatUtcTimestamp(vm: *VM, args: []v.Value) anyerror!NativeResult {
    var buffer: [40]u8 = undefined;
    const text = formatIsoMilliseconds(args[0].int64, 0, &buffer) catch unreachable;
    return allocateTimestampString(vm, text);
}

fn formatTimestampPattern(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const milliseconds = args[0].int64;
    const offset_seconds = vm.timezones.offsetSeconds(args[2].int64, milliseconds);
    const formatter = PatternFormatter.init(milliseconds, offset_seconds);
    const length = formatter.measure(native.bytesArg(args, 1)) catch |failure| return switch (failure) {
        error.InvalidFormat => native.fail(TimeError.InvalidFormat),
        error.PrecisionLoss => native.fail(TimeError.PrecisionLoss),
        error.OutOfMemory => error.OutOfMemory,
    };
    _ = std.math.add(usize, @sizeOf(v.Bytes), length) catch return error.OutOfMemory;
    const result = try vm.allocString(length);
    formatter.write(native.bytesArg(args, 1), result.mutableSlice());
    return native.stringResult(result);
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

fn scaleNanoseconds(_: *VM, args: []v.Value) anyerror!NativeResult {
    const value = args[0].int64;
    const multiplier = args[1].int64;
    const nanoseconds = std.math.cast(i64, @as(i128, value) * multiplier) orelse return RuntimeErrorReporter.durationOutOfRange(value, unitName(multiplier));
    return native.ok(.{ .int64 = nanoseconds });
}

fn unitName(multiplier: i64) []const u8 {
    return switch (multiplier) {
        1000 => "microseconds",
        1000000 => "milliseconds",
        1000000000 => "seconds",
        60000000000 => "minutes",
        else => "hours",
    };
}

fn checkedIntegerResult(value: i128) NativeResult {
    const result = std.math.cast(i64, value) orelse return native.fail(TimeError.Overflow);
    return native.ok(.{ .int64 = result });
}

fn addNanoseconds(_: *VM, args: []v.Value) anyerror!NativeResult {
    return checkedIntegerResult(@as(i128, args[0].int64) + args[1].int64);
}

fn subtractNanoseconds(_: *VM, args: []v.Value) anyerror!NativeResult {
    return checkedIntegerResult(@as(i128, args[0].int64) - args[1].int64);
}

fn addTimestampDuration(_: *VM, args: []v.Value) anyerror!NativeResult {
    const nanoseconds = args[1].int64;
    if (@rem(nanoseconds, 1000000) != 0) {
        return native.fail(TimeError.PrecisionLoss);
    }
    return checkedIntegerResult(@as(i128, args[0].int64) + @divTrunc(nanoseconds, 1000000));
}

fn subtractTimestampDuration(_: *VM, args: []v.Value) anyerror!NativeResult {
    const nanoseconds = args[1].int64;
    if (@rem(nanoseconds, 1000000) != 0) {
        return native.fail(TimeError.PrecisionLoss);
    }
    return checkedIntegerResult(@as(i128, args[0].int64) - @divTrunc(nanoseconds, 1000000));
}

fn timestampDifferenceNanoseconds(_: *VM, args: []v.Value) anyerror!NativeResult {
    const difference = @as(i128, args[0].int64) - args[1].int64;
    return checkedIntegerResult(difference * 1000000);
}

fn loadTimezoneId(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const identifier = (try vm.timezones.loadNamed(vm.allocator, native.bytesArg(args, 0))) orelse return native.fail(TimeError.UnknownTimeZone);
    return native.ok(.{ .int64 = identifier });
}

fn timezoneName(vm: *VM, args: []v.Value) anyerror!NativeResult {
    var buffer: [32]u8 = undefined;
    const text = vm.timezones.zoneName(args[0].int64, &buffer);
    const result = try vm.allocString(text.len);
    @memcpy(result.mutableSlice(), text);
    return native.stringResult(result);
}

fn timezoneOffsetSeconds(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return native.ok(.{ .int64 = vm.timezones.offsetSeconds(args[0].int64, args[1].int64) });
}

fn localTimezoneId(vm: *VM, _: []v.Value) anyerror!NativeResult {
    if (vm.timezones.local_identifier) |identifier| {
        return native.ok(.{ .int64 = identifier });
    }
    const current = vm.running_task.?;
    LocalJob.start(&vm.event_loop, &vm.scheduler, current, vm.environment) catch |failure| {
        if (failure == error.OutOfMemory) {
            return failure;
        }
        return native.fail(TimeError.LocalTimeUnavailable);
    };
    current.park();
    return native.parked();
}
