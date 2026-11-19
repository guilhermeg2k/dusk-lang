const std = @import("std");

pub const Recurrence = struct {
    standard_offset: i32,
    daylight_offset: ?i32 = null,
    start: DayRule = .{},
    end: DayRule = .{},

    pub fn parse(text: []const u8) !Recurrence {
        var parser = Parser{ .text = text };
        try parser.skipName();
        var result = Recurrence{ .standard_offset = -(try parser.clock(24)) };
        if (parser.position == text.len) {
            return result;
        }
        try parser.skipName();
        result.daylight_offset = if (parser.hasNext(',')) result.standard_offset + 3600 else -(try parser.clock(24));
        try parser.expect(',');
        result.start = try parser.dayRule();
        try parser.expect(',');
        result.end = try parser.dayRule();
        if (parser.position != text.len) {
            return error.InvalidRule;
        }
        return result;
    }

    pub fn offsetSeconds(self: Recurrence, timestamp: i64) i32 {
        const daylight_offset = self.daylight_offset orelse return self.standard_offset;
        const year = yearFromSeconds(timestamp);
        var latest: i128 = std.math.minInt(i128);
        var offset = self.standard_offset;
        for ([_]i128{ year - 1, year, year + 1 }) |candidate| {
            const start = self.start.transition(candidate, self.standard_offset);
            if (start <= timestamp and start >= latest) {
                latest = start;
                offset = daylight_offset;
            }
            const end = self.end.transition(candidate, daylight_offset);
            if (end <= timestamp and end >= latest) {
                latest = end;
                offset = self.standard_offset;
            }
        }
        return offset;
    }
};

const Parser = struct {
    text: []const u8,
    position: usize = 0,

    fn skipName(self: *Parser) !void {
        const is_quoted = self.hasNext('<');
        if (is_quoted) {
            self.position += 1;
        }
        const start = self.position;
        while (self.position < self.text.len) {
            const byte = self.text[self.position];
            const is_name_byte = std.ascii.isAlphabetic(byte) or (is_quoted and (std.ascii.isDigit(byte) or byte == '+' or byte == '-'));
            if (!is_name_byte) break;
            self.position += 1;
        }
        if (self.position - start < 3) {
            return error.InvalidRule;
        }
        if (is_quoted) {
            try self.expect('>');
        }
    }

    fn hasNext(self: *const Parser, byte: u8) bool {
        return self.position < self.text.len and self.text[self.position] == byte;
    }

    fn expect(self: *Parser, byte: u8) !void {
        if (!self.hasNext(byte)) {
            return error.InvalidRule;
        }
        self.position += 1;
    }

    fn clock(self: *Parser, maximum_hour: u32) !i32 {
        const is_negative = self.hasNext('-');
        const has_sign = is_negative or self.hasNext('+');
        if (has_sign) {
            self.position += 1;
        }
        const hour = try self.number(maximum_hour);
        var minute: u32 = 0;
        var second: u32 = 0;
        if (self.hasNext(':')) {
            self.position += 1;
            minute = try self.number(59);
            if (self.hasNext(':')) {
                self.position += 1;
                second = try self.number(59);
            }
        }
        const seconds: i32 = @intCast(hour * 3600 + minute * 60 + second);
        return if (is_negative) -seconds else seconds;
    }

    fn number(self: *Parser, maximum: u32) !u32 {
        const start = self.position;
        var result: u32 = 0;
        while (self.position < self.text.len and std.ascii.isDigit(self.text[self.position])) {
            const digit: u32 = self.text[self.position] - '0';
            if (result > maximum / 10) {
                return error.InvalidRule;
            }
            result = result * 10 + digit;
            if (result > maximum) {
                return error.InvalidRule;
            }
            self.position += 1;
        }
        if (start == self.position) {
            return error.InvalidRule;
        }
        return result;
    }

    fn dayRule(self: *Parser) !DayRule {
        var result: DayRule = .{};
        if (self.hasNext('M')) {
            self.position += 1;
            result.kind = .month_week_day;
            result.first = try self.number(12);
            try self.expect('.');
            result.second = try self.number(5);
            try self.expect('.');
            result.third = try self.number(6);
            const has_zero_field = result.first == 0 or result.second == 0;
            if (has_zero_field) {
                return error.InvalidRule;
            }
        } else {
            const is_julian = self.hasNext('J');
            if (is_julian) {
                self.position += 1;
            }
            result.kind = if (is_julian) .julian else .ordinal;
            result.first = try self.number(365);
            if (is_julian and result.first == 0) {
                return error.InvalidRule;
            }
        }
        if (self.hasNext('/')) {
            self.position += 1;
            result.seconds = try self.clock(167);
        }
        return result;
    }
};

const DayRule = struct {
    kind: enum { month_week_day, julian, ordinal } = .ordinal,
    first: u32 = 0,
    second: u32 = 0,
    third: u32 = 0,
    seconds: i32 = 7200,

    fn transition(self: DayRule, year: i128, previous_offset: i32) i128 {
        var days = daysFromCivil(year, 1, 1);
        switch (self.kind) {
            .month_week_day => {
                const month: i128 = self.first;
                const first = daysFromCivil(year, month, 1);
                var day = 1 + @mod(@as(i128, self.third) - @mod(first + 4, 7), 7) + (@as(i128, self.second) - 1) * 7;
                const next_year = year + @intFromBool(month == 12);
                const next_month = daysFromCivil(next_year, @mod(month, 12) + 1, 1);
                if (first + day - 1 >= next_month) {
                    day -= 7;
                }
                days = first + day - 1;
            },
            .julian => {
                const needs_leap_day = isLeapYear(year) and self.first >= 60;
                days += @as(i128, self.first) - 1 + @intFromBool(needs_leap_day);
            },
            .ordinal => days += self.first,
        }
        return days * 86400 + self.seconds - previous_offset;
    }
};

fn daysFromCivil(year: i128, month: i128, day: i128) i128 {
    const shifted_year = year - @intFromBool(month <= 2);
    const era = @divFloor(shifted_year, 400);
    const year_of_era = @mod(shifted_year, 400);
    const shifted_month = if (month > 2) month - 3 else month + 9;
    const day_of_year = @divFloor(153 * shifted_month + 2, 5) + day - 1;
    const day_of_era = year_of_era * 365 + @divFloor(year_of_era, 4) - @divFloor(year_of_era, 100) + day_of_year;
    return era * 146097 + day_of_era - 719468;
}

fn isLeapYear(year: i128) bool {
    return @mod(year, 4) == 0 and (@mod(year, 100) != 0 or @mod(year, 400) == 0);
}

fn yearFromSeconds(timestamp: i64) i128 {
    const days = @divFloor(@as(i128, timestamp), 86400) + 719468;
    const era = @divFloor(days, 146097);
    const day_of_era = @mod(days, 146097);
    const year_of_era = @divFloor(day_of_era - @divFloor(day_of_era, 1460) + @divFloor(day_of_era, 36524) - @divFloor(day_of_era, 146096), 365);
    const year = year_of_era + era * 400;
    const day_of_year = day_of_era - (365 * year_of_era + @divFloor(year_of_era, 4) - @divFloor(year_of_era, 100));
    const month = @divFloor(5 * day_of_year + 2, 153);
    return year + @intFromBool(month >= 10);
}

test "recurrence rejects malformed and unbounded fields" {
    for ([_][]const u8{
        "",                "UTC",                        "UT0",                         "<UTC0",                          "UTC999999999999999999999999",
        "UTC0DST",         "UTC0DST,M0.1.0,M10.1.0",     "UTC0DST,M3.0.0,M10.1.0",      "UTC0DST,M3.1.7,M10.1.0",         "UTC0DST,J0,J300",
        "UTC0DST,366,300", "UTC0DST,M3.1.0/168,M10.1.0", "UTC0DST,M3.1.0/2:60,M10.1.0", "UTC0DST,M3.1.0,M10.1.0trailing",
    }) |text| {
        try std.testing.expectError(error.InvalidRule, Recurrence.parse(text));
    }
}
