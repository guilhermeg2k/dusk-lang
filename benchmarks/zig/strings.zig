const std = @import("std");

pub fn main(init: std.process.Init) !void {
    var buf: [256]u8 = undefined;
    var w = std.Io.File.stdout().writer(init.io, &buf);
    const out = &w.interface;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const name = "dusk";
    var total: u64 = 0;
    var i: u64 = 0;
    while (i < 1000000) : (i += 1) {
        _ = arena.reset(.retain_capacity);
        const allocator = arena.allocator();
        const line = try std.fmt.allocPrint(allocator, "item {d}: value={d} tag={s}", .{ i, i * 3, name });
        var fields: std.ArrayList([]const u8) = .empty;
        var pieces = std.mem.splitScalar(u8, line, ' ');
        while (pieces.next()) |piece| try fields.append(allocator, piece);
        const joined = try std.mem.join(allocator, ",", fields.items);
        total += line.len + fields.items.len + joined.len;
    }

    try out.print("{d}\n", .{total});
    try out.flush();
}
