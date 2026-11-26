const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const RuntimeError = @import("../../runtime_error.zig").RuntimeError;
const RuntimeErrorReporter = @import("../../runtime_error.zig").RuntimeErrorReporter;
const native = @import("../native.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "now_ms", Binding{ .native = &nowMilliseconds } },
    .{ "monotonic_ns", Binding{ .native = &monotonicNanoseconds } },
    .{ "format_timestamp", Binding{ .native = &formatTimestamp } },
});

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
    return native.ok(.{ .int64 = try wallTimeMilliseconds(vm.io) });
}

fn monotonicNanoseconds(vm: *VM, _: []v.Value) anyerror!NativeResult {
    return native.ok(.{ .int64 = try monotonicTimeNanoseconds(vm.io) });
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
