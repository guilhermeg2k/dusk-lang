const std = @import("std");

fn fib(n: i64) i64 {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

pub fn main(init: std.process.Init) !void {
    var buf: [256]u8 = undefined;
    var w = std.Io.File.stdout().writer(init.io, &buf);
    const out = &w.interface;
    try out.print("{d}\n", .{fib(35)});
    try out.flush();
}
