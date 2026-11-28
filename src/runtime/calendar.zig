const std = @import("std");

pub const Civil = struct {
    year: i64,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
    second: u8,
    millisecond: u16,

    pub fn fromUnixMilliseconds(milliseconds: i128) Civil {
        const days = @divFloor(milliseconds, 86400000) + 719468;
        const era = @divFloor(days, 146097);
        const day_of_era = days - era * 146097;
        const year_of_era = @divTrunc(day_of_era - @divTrunc(day_of_era, 1460) + @divTrunc(day_of_era, 36524) - @divTrunc(day_of_era, 146096), 365);
        const day_of_year = day_of_era - (365 * year_of_era + @divTrunc(year_of_era, 4) - @divTrunc(year_of_era, 100));
        const shifted_month = @divTrunc(5 * day_of_year + 2, 153);
        const month = shifted_month + @as(i128, if (shifted_month < 10) 3 else -9);
        const day_milliseconds = @mod(milliseconds, 86400000);
        return .{
            .year = @intCast(era * 400 + year_of_era + @intFromBool(month <= 2)),
            .month = @intCast(month),
            .day = @intCast(day_of_year - @divTrunc(153 * shifted_month + 2, 5) + 1),
            .hour = @intCast(@divTrunc(day_milliseconds, 3600000)),
            .minute = @intCast(@mod(@divTrunc(day_milliseconds, 60000), 60)),
            .second = @intCast(@mod(@divTrunc(day_milliseconds, 1000), 60)),
            .millisecond = @intCast(@mod(day_milliseconds, 1000)),
        };
    }

    pub fn toUnixMilliseconds(self: Civil, offset_seconds: i32) error{ InvalidTimestamp, Overflow }!i64 {
        if (!self.isValid()) {
            return error.InvalidTimestamp;
        }
        const days = daysFromCivil(self.year, self.month, self.day);
        const milliseconds = days * 86400000 + @as(i128, self.hour) * 3600000 + @as(i128, self.minute) * 60000 + @as(i128, self.second) * 1000 + self.millisecond - @as(i128, offset_seconds) * 1000;
        return std.math.cast(i64, milliseconds) orelse error.Overflow;
    }

    pub fn isValid(self: Civil) bool {
        const has_valid_month = self.month >= 1 and self.month <= 12;
        if (!has_valid_month) {
            return false;
        }
        const is_leap_year = isLeapYear(self.year);
        const month_lengths = [_]u8{ 31, if (is_leap_year) 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
        return self.day >= 1 and self.day <= month_lengths[self.month - 1] and self.hour < 24 and self.minute < 60 and self.second < 60 and self.millisecond < 1000;
    }
};

pub fn daysFromCivil(year: i64, month: u8, day: u8) i128 {
    const shifted_year = @as(i128, year) - @intFromBool(month <= 2);
    const era = @divFloor(shifted_year, 400);
    const year_of_era = shifted_year - era * 400;
    const shifted_month = @as(i128, month) + @as(i128, if (month > 2) -3 else 9);
    const day_of_year = @divTrunc(153 * shifted_month + 2, 5) + day - 1;
    const day_of_era = year_of_era * 365 + @divTrunc(year_of_era, 4) - @divTrunc(year_of_era, 100) + day_of_year;
    return era * 146097 + day_of_era - 719468;
}

pub fn isLeapYear(year: i64) bool {
    return @mod(year, 4) == 0 and (@mod(year, 100) != 0 or @mod(year, 400) == 0);
}

test "calendar converts every day of a Gregorian cycle and full-range milliseconds" {
    for (0..146097) |day| {
        const milliseconds = @as(i64, @intCast(day)) * 86400000 - 62167219200000;
        const civil = Civil.fromUnixMilliseconds(milliseconds);
        try std.testing.expectEqual(milliseconds, try civil.toUnixMilliseconds(0));
        const previous_cycle = Civil.fromUnixMilliseconds(@as(i128, milliseconds) - 146097 * 86400000);
        try std.testing.expectEqual(civil.year - 400, previous_cycle.year);
        try std.testing.expectEqual(civil.month, previous_cycle.month);
        try std.testing.expectEqual(civil.day, previous_cycle.day);
    }
    var bits: u64 = 0;
    for (0..4096) |_| {
        const milliseconds: i64 = @bitCast(bits);
        try std.testing.expectEqual(milliseconds, try Civil.fromUnixMilliseconds(milliseconds).toUnixMilliseconds(0));
        bits = bits *% 6364136223846793005 +% 1442695040888963407;
    }
    for ([_]i64{ std.math.minInt(i64), std.math.maxInt(i64) }) |milliseconds| {
        for ([_]i32{ -86399, 0, 86399 }) |offset| {
            const civil = Civil.fromUnixMilliseconds(@as(i128, milliseconds) + @as(i128, offset) * 1000);
            try std.testing.expectEqual(milliseconds, try civil.toUnixMilliseconds(offset));
        }
    }
}
