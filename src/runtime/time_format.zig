const std = @import("std");
const Civil = @import("calendar.zig").Civil;

pub fn formatIsoMilliseconds(milliseconds: i64, offset_seconds: i32, buffer: *[40]u8) error{PrecisionLoss}![]const u8 {
    if (@rem(offset_seconds, 60) != 0) {
        return error.PrecisionLoss;
    }
    const local_milliseconds = @as(i128, milliseconds) + @as(i128, offset_seconds) * 1000;
    const civil = Civil.fromUnixMilliseconds(local_milliseconds);
    const year_sign: []const u8 = if (civil.year < 0) "-" else if (civil.year > 9999) "+" else "";
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
    if (offset_seconds == 0) {
        buffer[date.len] = 'Z';
        return buffer[0 .. date.len + 1];
    }
    const magnitude = @abs(offset_seconds);
    const offset_sign: u8 = if (offset_seconds < 0) '-' else '+';
    const suffix = std.fmt.bufPrint(buffer[date.len..], "{c}{d:0>2}:{d:0>2}", .{ offset_sign, magnitude / 3600, magnitude / 60 % 60 }) catch unreachable;
    return buffer[0 .. date.len + suffix.len];
}

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
