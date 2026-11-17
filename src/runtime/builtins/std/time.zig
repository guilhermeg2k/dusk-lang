const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const EventLoop = @import("../../tasks/event_loop.zig").EventLoop;
const native = @import("../native.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "now_ms", Binding{ .native = &nowMilliseconds } },
    .{ "monotonic_ns", Binding{ .native = &monotonicNanoseconds } },
    .{ "format_timestamp", Binding{ .native = &formatTimestamp } },
    .{ "parse_timestamp", Binding{ .native = &parseTimestamp } },
});

pub const TimeError = enum { InvalidTimestamp };
pub const error_sets = .{TimeError};

fn parseTimestamp(_: *VM, args: []v.Value) anyerror!NativeResult {
    const milliseconds = parseIsoMilliseconds(native.bytesArg(args, 0)) orelse return native.fail(TimeError.InvalidTimestamp);
    return native.ok(.{ .int64 = milliseconds });
}

fn parseIsoMilliseconds(text: []const u8) ?i64 {
    const has_shape = text.len >= 20 and text[4] == '-' and text[7] == '-' and text[10] == 'T' and text[13] == ':' and text[16] == ':' and text[text.len - 1] == 'Z';
    if (!has_shape) {
        return null;
    }
    const year = digitsValue(text[0..4]) orelse return null;
    const month = digitsValue(text[5..7]) orelse return null;
    const day = digitsValue(text[8..10]) orelse return null;
    const hour = digitsValue(text[11..13]) orelse return null;
    const minute = digitsValue(text[14..16]) orelse return null;
    const second = digitsValue(text[17..19]) orelse return null;
    var millisecond: i64 = 0;
    const fraction = text[19 .. text.len - 1];
    if (fraction.len > 0) {
        const has_digits = fraction.len >= 2 and fraction[0] == '.';
        if (!has_digits) {
            return null;
        }
        const digits = fraction[1..];
        _ = digitsValue(digits) orelse return null;
        for (0..3) |i| {
            const digit: i64 = if (i < digits.len) digits[i] - '0' else 0;
            millisecond = millisecond * 10 + digit;
        }
    }
    const is_leap_year = @mod(year, 4) == 0 and (@mod(year, 100) != 0 or @mod(year, 400) == 0);
    const month_lengths = [_]i64{ 31, if (is_leap_year) 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
    const is_in_range = month >= 1 and month <= 12 and day >= 1 and day <= month_lengths[@intCast(@max(month, 1) - 1)] and hour < 24 and minute < 60 and second < 60;
    if (!is_in_range) {
        return null;
    }
    const shifted_year = if (month <= 2) year - 1 else year;
    const era = @divFloor(shifted_year, 400);
    const year_of_era = shifted_year - era * 400;
    const shifted_month = if (month > 2) month - 3 else month + 9;
    const day_of_year = @divFloor(153 * shifted_month + 2, 5) + day - 1;
    const day_of_era = year_of_era * 365 + @divFloor(year_of_era, 4) - @divFloor(year_of_era, 100) + day_of_year;
    const days = era * 146097 + day_of_era - 719468;
    return days * 86400000 + hour * 3600000 + minute * 60000 + second * 1000 + millisecond;
}

fn digitsValue(text: []const u8) ?i64 {
    var value: i64 = 0;
    for (text) |c| {
        if (!std.ascii.isDigit(c)) {
            return null;
        }
        value = value * 10 + (c - '0');
    }
    return value;
}

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
    const days_since_year_zero = @divFloor(milliseconds, 86400000) + 719528;
    const cycle = @divFloor(days_since_year_zero, 146097);
    var day_in_year = @mod(days_since_year_zero, 146097);
    var year_in_cycle: std.time.epoch.Year = 0;
    while (true) {
        const days_in_year = std.time.epoch.getDaysInYear(year_in_cycle);
        if (day_in_year < days_in_year) break;
        day_in_year -= days_in_year;
        year_in_cycle += 1;
    }
    const year = cycle * 400 + year_in_cycle;
    const year_and_day = std.time.epoch.YearAndDay{ .year = year_in_cycle, .day = @intCast(day_in_year) };
    const date = year_and_day.calculateMonthDay();
    const day_milliseconds: u32 = @intCast(@mod(milliseconds, 86400000));
    const sign: []const u8 = if (year < 0) "-" else if (year > 9999) "+" else "";
    return std.fmt.bufPrint(buffer, "{s}{d:0>4}-{d:0>2}-{d:0>2}T{d:0>2}:{d:0>2}:{d:0>2}.{d:0>3}Z", .{
        sign,
        @abs(year),
        date.month.numeric(),
        @as(u8, date.day_index) + 1,
        @divTrunc(day_milliseconds, 3600000),
        @mod(@divTrunc(day_milliseconds, 60000), 60),
        @mod(@divTrunc(day_milliseconds, 1000), 60),
        @mod(day_milliseconds, 1000),
    }) catch unreachable;
}
