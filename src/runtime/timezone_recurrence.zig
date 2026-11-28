const std = @import("std");
const calendar = @import("calendar.zig");

pub const Recurrence = struct {
    standard_offset: i32,
    daylight_offset: ?i32 = null,
    start: DayRule = .{},
    end: DayRule = .{},

    pub fn parse(text: []const u8) !Recurrence {
        if (text.len > 512) {
            return error.InvalidRule;
        }
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
        const year = calendar.Civil.fromUnixMilliseconds(@as(i128, timestamp) * 1000).year;
        var latest: i128 = std.math.minInt(i128);
        var offset = daylight_offset;
        var candidate = year + 1;
        while (candidate >= year - 400) : (candidate -= 1) {
            const can_stop_search = candidate < year - 1 and latest != std.math.minInt(i128);
            if (can_stop_search) break;
            const start = self.start.transition(candidate, self.standard_offset);
            const end = self.end.transition(candidate, daylight_offset);
            const year_seconds: i128 = if (calendar.isLeapYear(candidate)) 31622400 else 31536000;
            const has_transitions = end < start or (start < end and end - start < year_seconds);
            if (!has_transitions) continue;
            const has_started = start <= timestamp and start >= latest;
            if (has_started) {
                latest = start;
                offset = daylight_offset;
            }
            const has_ended = end <= timestamp and end > latest;
            if (has_ended) {
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

    fn transition(self: DayRule, year: i64, previous_offset: i32) i128 {
        var days = calendar.daysFromCivil(year, 1, 1);
        switch (self.kind) {
            .month_week_day => {
                const month: u8 = @intCast(self.first);
                const first = calendar.daysFromCivil(year, month, 1);
                var day = 1 + @mod(@as(i128, self.third) - @mod(first + 4, 7), 7) + (@as(i128, self.second) - 1) * 7;
                const next_year = year + @intFromBool(month == 12);
                const next_month = calendar.daysFromCivil(next_year, @mod(month, 12) + 1, 1);
                if (first + day - 1 >= next_month) {
                    day -= 7;
                }
                days = first + day - 1;
            },
            .julian => {
                const needs_leap_day = calendar.isLeapYear(year) and self.first >= 60;
                days += @as(i128, self.first) - 1 + @intFromBool(needs_leap_day);
            },
            .ordinal => days += self.first,
        }
        return days * 86400 + self.seconds - previous_offset;
    }
};

test "recurrence rejects malformed and unbounded fields" {
    for ([_][]const u8{
        "",                "UTC",                        "UT0",                         "<UTC0",                          "UTC999999999999999999999999",
        "UTC0DST",         "UTC0DST,M0.1.0,M10.1.0",     "UTC0DST,M3.0.0,M10.1.0",      "UTC0DST,M3.1.7,M10.1.0",         "UTC0DST,J0,J300",
        "UTC0DST,366,300", "UTC0DST,M3.1.0/168,M10.1.0", "UTC0DST,M3.1.0/2:60,M10.1.0", "UTC0DST,M3.1.0,M10.1.0trailing",
    }) |text| {
        try std.testing.expectError(error.InvalidRule, Recurrence.parse(text));
    }
}

test "recurrence supports permanent daylight time, coincident transitions and extreme years" {
    for ([_][]const u8{ "XXX3EDT4,0/0,J365/23", "EST5EDT,0/0,J365/25" }) |text| {
        const recurrence = try Recurrence.parse(text);
        for ([_]i64{ -9223372036854776, 0, 951782400, 4102444800, 9223372036854775 }) |timestamp| {
            try std.testing.expectEqual(@as(i32, -14400), recurrence.offsetSeconds(timestamp));
        }
    }
    const coincident = try Recurrence.parse("STD0DST,J60/0,J60/1");
    for ([_]i64{ std.math.minInt(i64), -1, 0, 951782400, std.math.maxInt(i64) }) |timestamp| {
        try std.testing.expectEqual(@as(i32, 3600), coincident.offsetSeconds(timestamp));
    }
    const leap_only = try Recurrence.parse("STD0DST,59/0,J60/1");
    const Case = struct { year: i64, month: u8, day: u8, offset: i32 };
    for ([_]Case{
        .{ .year = 2000, .month = 2, .day = 29, .offset = 3600 },
        .{ .year = 2000, .month = 3, .day = 1, .offset = 0 },
        .{ .year = 2001, .month = 2, .day = 28, .offset = 0 },
        .{ .year = 2100, .month = 3, .day = 1, .offset = 0 },
        .{ .year = 2400, .month = 2, .day = 29, .offset = 3600 },
        .{ .year = 2401, .month = 1, .day = 1, .offset = 0 },
    }) |case| {
        const timestamp: i64 = @intCast(calendar.daysFromCivil(case.year, case.month, case.day) * 86400 + 43200);
        try std.testing.expectEqual(case.offset, leap_only.offsetSeconds(timestamp));
    }
    const fixed = try Recurrence.parse("<+0530>-5:30");
    try std.testing.expectEqual(@as(i32, 19800), fixed.offsetSeconds(0));
    for ([_][]const u8{ "UTC0\x00", "UTC0\n", "UTC0DST,M3.1.0,M10.1.0,extra", "<A+>0", "UTC0DST,M3.1.0/2:00:60,M10.1.0" }) |text| {
        try std.testing.expectError(error.InvalidRule, Recurrence.parse(text));
    }
}
