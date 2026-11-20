const std = @import("std");
const Civil = @import("calendar.zig").Civil;

pub fn formatIsoMilliseconds(milliseconds: i64, offset_seconds: i32, buffer: *[40]u8) error{PrecisionLoss}![]const u8 {
    if (@rem(offset_seconds, 60) != 0) {
        return error.PrecisionLoss;
    }
    const civil = localCivil(milliseconds, offset_seconds);
    const year_sign = yearSign(civil.year);
    const date = std.fmt.bufPrint(buffer, "{s}{d:0>4}-{d:0>2}-{d:0>2}T{d:0>2}:{d:0>2}:{d:0>2}.{d:0>3}", .{
        year_sign,
        @abs(civil.year),
        civil.month,
        civil.day,
        civil.hour,
        civil.minute,
        civil.second,
        civil.millisecond,
    }) catch unreachable;
    const suffix = try formatOffset(offset_seconds, buffer[date.len..]);
    return buffer[0 .. date.len + suffix.len];
}

fn localCivil(milliseconds: i64, offset_seconds: i32) Civil {
    return Civil.fromUnixMilliseconds(@as(i128, milliseconds) + @as(i128, offset_seconds) * 1000);
}

fn yearSign(year: i64) []const u8 {
    return if (year < 0) "-" else if (year > 9999) "+" else "";
}

fn formatOffset(offset_seconds: i32, buffer: []u8) error{PrecisionLoss}![]const u8 {
    if (@rem(offset_seconds, 60) != 0) {
        return error.PrecisionLoss;
    }
    if (offset_seconds == 0) {
        buffer[0] = 'Z';
        return buffer[0..1];
    }
    const magnitude = @abs(offset_seconds);
    const sign: u8 = if (offset_seconds < 0) '-' else '+';
    return std.fmt.bufPrint(buffer, "{c}{d:0>2}:{d:0>2}", .{ sign, magnitude / 3600, magnitude / 60 % 60 }) catch unreachable;
}

pub const PatternFormatter = struct {
    civil: Civil,
    offset_seconds: i32,

    pub fn init(milliseconds: i64, offset_seconds: i32) PatternFormatter {
        return .{ .civil = localCivil(milliseconds, offset_seconds), .offset_seconds = offset_seconds };
    }

    pub fn measure(self: PatternFormatter, pattern: []const u8) error{ InvalidFormat, PrecisionLoss, OutOfMemory }!usize {
        var scanner = PatternScanner{ .pattern = pattern };
        var length: usize = 0;
        var needs_minute_offset = false;
        while (try scanner.next()) |part| {
            const part_length = switch (part) {
                .literal => |literal| literal.len,
                .token => |token| length: {
                    if (token == .offset) {
                        needs_minute_offset = true;
                    }
                    break :length self.tokenLength(token);
                },
            };
            length = std.math.add(usize, length, part_length) catch return error.OutOfMemory;
        }
        const has_imprecise_offset = needs_minute_offset and @rem(self.offset_seconds, 60) != 0;
        if (has_imprecise_offset) {
            return error.PrecisionLoss;
        }
        return length;
    }

    fn tokenLength(self: PatternFormatter, token: Token) usize {
        return switch (token) {
            .year => length: {
                var buffer: [40]u8 = undefined;
                break :length self.tokenText(token, &buffer).len;
            },
            .millisecond => 3,
            .offset => if (self.offset_seconds == 0) 1 else 4 + std.fmt.count("{d:0>2}", .{@abs(self.offset_seconds) / 3600}),
            else => 2,
        };
    }

    pub fn write(self: PatternFormatter, pattern: []const u8, output: []u8) void {
        var scanner = PatternScanner{ .pattern = pattern };
        var position: usize = 0;
        while (scanner.next() catch unreachable) |part| {
            var buffer: [40]u8 = undefined;
            const text = switch (part) {
                .literal => |literal| literal,
                .token => |token| self.tokenText(token, &buffer),
            };
            @memcpy(output[position..][0..text.len], text);
            position += text.len;
        }
        std.debug.assert(position == output.len);
    }

    fn tokenText(self: PatternFormatter, token: Token, buffer: *[40]u8) []const u8 {
        const value: u16 = switch (token) {
            .year => return std.fmt.bufPrint(buffer, "{s}{d:0>4}", .{ yearSign(self.civil.year), @abs(self.civil.year) }) catch unreachable,
            .offset => return formatOffset(self.offset_seconds, buffer) catch unreachable,
            .month => self.civil.month,
            .day => self.civil.day,
            .hour => self.civil.hour,
            .minute => self.civil.minute,
            .second => self.civil.second,
            .millisecond => return std.fmt.bufPrint(buffer, "{d:0>3}", .{self.civil.millisecond}) catch unreachable,
        };
        return std.fmt.bufPrint(buffer, "{d:0>2}", .{value}) catch unreachable;
    }
};

const Token = enum { year, month, day, hour, minute, second, millisecond, offset };
const Part = union(enum) { literal: []const u8, token: Token };
const tokens = std.StaticStringMap(Token).initComptime(.{
    .{ "YYYY", .year }, .{ "MM", .month },  .{ "DD", .day },          .{ "HH", .hour },
    .{ "mm", .minute }, .{ "ss", .second }, .{ "SSS", .millisecond }, .{ "Z", .offset },
});

const PatternScanner = struct {
    pattern: []const u8,
    position: usize = 0,

    fn next(self: *PatternScanner) error{InvalidFormat}!?Part {
        if (self.position == self.pattern.len) {
            return null;
        }
        const start = self.position;
        const byte = self.pattern[start];
        self.position += 1;
        if (byte == '[') {
            if (self.take('[')) {
                return .{ .literal = self.pattern[start .. start + 1] };
            }
            const literal_start = self.position;
            while (self.position < self.pattern.len) {
                const literal_byte = self.pattern[self.position];
                if (literal_byte == '[') {
                    return error.InvalidFormat;
                }
                if (literal_byte == ']') {
                    const literal = self.pattern[literal_start..self.position];
                    self.position += 1;
                    return .{ .literal = literal };
                }
                self.position += 1;
            }
            return error.InvalidFormat;
        }
        if (byte == ']') {
            if (!self.take(']')) {
                return error.InvalidFormat;
            }
            return .{ .literal = self.pattern[start .. start + 1] };
        }
        if (std.ascii.isAlphabetic(byte)) {
            while (self.position < self.pattern.len and self.pattern[self.position] == byte) {
                self.position += 1;
            }
            const token = tokens.get(self.pattern[start..self.position]) orelse return error.InvalidFormat;
            return .{ .token = token };
        }
        while (self.position < self.pattern.len) {
            const next_byte = self.pattern[self.position];
            const is_syntax = next_byte == '[' or next_byte == ']' or std.ascii.isAlphabetic(next_byte);
            if (is_syntax) break;
            self.position += 1;
        }
        return .{ .literal = self.pattern[start..self.position] };
    }

    fn take(self: *PatternScanner, byte: u8) bool {
        const has_byte = self.position < self.pattern.len and self.pattern[self.position] == byte;
        if (!has_byte) {
            return false;
        }
        self.position += 1;
        return true;
    }
};

test "ISO formatting keeps wide local fields at both timestamp limits" {
    var buffer: [40]u8 = undefined;
    try std.testing.expectEqualStrings("+292278994-08-18T07:11:55.807+23:59", try formatIsoMilliseconds(std.math.maxInt(i64), 86340, &buffer));
    try std.testing.expectEqualStrings("-292275055-05-15T16:48:04.192-23:59", try formatIsoMilliseconds(std.math.minInt(i64), -86340, &buffer));
    try std.testing.expectEqualStrings("+292278994-08-16T07:13:55.807-23:59", try formatIsoMilliseconds(std.math.maxInt(i64), -86340, &buffer));
    try std.testing.expectEqualStrings("-292275055-05-17T16:46:04.192+23:59", try formatIsoMilliseconds(std.math.minInt(i64), 86340, &buffer));
    try std.testing.expectEqualStrings("1969-12-31T23:59:59.999Z", try formatIsoMilliseconds(-1, 0, &buffer));
    try std.testing.expectEqualStrings("1969-12-31T20:29:59.999-03:30", try formatIsoMilliseconds(-1, -12600, &buffer));
    for ([_]i32{ -86399, -561, -30, 1, 30, 561, 86399 }) |offset| {
        try std.testing.expectError(error.PrecisionLoss, formatIsoMilliseconds(0, offset, &buffer));
    }
}

test "custom patterns recognize complete tokens and exact literal escapes" {
    const Case = struct { pattern: []const u8, expected: []const u8 };
    for ([_]Case{
        .{ .pattern = "YYYY-MM-DD[T]HH:mm:ss.SSSZ", .expected = "2026-11-18T14:30:45.123Z" },
        .{ .pattern = "YYYYMMDDHHmmssSSSZ", .expected = "20261118143045123Z" },
        .{ .pattern = "[Created on] DD/MM/YYYY [at] HH:mm", .expected = "Created on 18/11/2026 at 14:30" },
        .{ .pattern = "[[]]", .expected = "[]" },
        .{ .pattern = "[[YYYY]]", .expected = "[2026]" },
        .{ .pattern = "[[[YYYY]]]", .expected = "[YYYY]" },
        .{ .pattern = "[a]]]", .expected = "a]" },
        .{ .pattern = "年YYYY月MM日DD\x00", .expected = "年2026月11日18\x00" },
        .{ .pattern = "[][Z][]", .expected = "Z" },
        .{ .pattern = "", .expected = "" },
    }) |case| {
        const formatter = PatternFormatter.init(1795012245123, 0);
        const length = try formatter.measure(case.pattern);
        try std.testing.expectEqual(case.expected.len, length);
        const output = try std.testing.allocator.alloc(u8, length + 2);
        defer std.testing.allocator.free(output);
        @memset(output, 0xa5);
        formatter.write(case.pattern, output[1 .. length + 1]);
        try std.testing.expectEqualStrings(case.expected, output[1 .. length + 1]);
        try std.testing.expectEqual(@as(u8, 0xa5), output[0]);
        try std.testing.expectEqual(@as(u8, 0xa5), output[length + 1]);
    }
}

test "custom patterns reject malformed runs and escapes before offset precision" {
    const formatter = PatternFormatter.init(0, 30);
    for ([_][]const u8{ "Y", "YYYYY", "MMMM", "mmmmm", "yyyy", "SS", "ZZ", "T", "hello", "[", "]", "[text", "[a[b]", "[a]]", "[[]", "Z[", "ZZ[" }) |pattern| {
        try std.testing.expectError(error.InvalidFormat, formatter.measure(pattern));
    }
    try std.testing.expectError(error.PrecisionLoss, formatter.measure("Z"));
    try std.testing.expectEqual(@as(usize, 1), try formatter.measure("[Z]"));
    try std.testing.expectEqual(@as(usize, 0), try formatter.measure("[]"));
    var buffer: [32]u8 = undefined;
    const pattern = "YYYY-MM-DD HH:mm:ss.SSS";
    const length = try formatter.measure(pattern);
    formatter.write(pattern, buffer[0..length]);
    try std.testing.expectEqualStrings("1970-01-01 00:00:30.000", buffer[0..length]);
}
