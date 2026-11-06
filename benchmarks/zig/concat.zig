const std = @import("std");

const N: usize = 200000;

pub fn main(init: std.process.Init) !void {
    var buf: [256]u8 = undefined;
    var w = std.Io.File.stdout().writer(init.io, &buf);
    const out = &w.interface;

    const allocator = std.heap.page_allocator;
    var front = try allocator.alloc(u8, N * 2);
    var back = try allocator.alloc(u8, N * 2);
    var len: usize = 0;
    var i: usize = 0;
    while (i < N) : (i += 1) {
        @memcpy(back[0..len], front[0..len]);
        @memcpy(back[len..][0..2], "ab");
        len += 2;
        std.mem.swap([]u8, &front, &back);
    }

    try out.print("{d}\n", .{len});
    try out.flush();
}
