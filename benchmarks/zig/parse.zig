const std = @import("std");

pub fn main(init: std.process.Init) !void {
    var buf: [256]u8 = undefined;
    var w = std.Io.File.stdout().writer(init.io, &buf);
    const out = &w.interface;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var rows: std.ArrayList([]const u8) = .empty;
    var i: u64 = 0;
    while (i < 500000) : (i += 1) {
        try rows.append(allocator, try std.fmt.allocPrint(allocator, "{d},{d},item{d}", .{ i, (i * 7) % 1000, i }));
    }
    const csv = try std.mem.join(allocator, "\n", rows.items);

    var total: i64 = 0;
    var count: i64 = 0;
    var lines = std.mem.splitScalar(u8, csv, '\n');
    while (lines.next()) |row| {
        var fields = std.mem.splitScalar(u8, row, ',');
        const first = try std.fmt.parseInt(i64, fields.next().?, 10);
        const second = try std.fmt.parseInt(i64, fields.next().?, 10);
        total += first + second;
        count += 1;
    }

    try out.print("{d}\n", .{total + count});
    try out.flush();
}
