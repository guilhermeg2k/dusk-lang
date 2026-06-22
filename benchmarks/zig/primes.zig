const std = @import("std");

const LIMIT: usize = 100000;

var sieve: [LIMIT + 1]bool = undefined;

pub fn main(init: std.process.Init) !void {
    var buf: [256]u8 = undefined;
    var w = std.Io.File.stdout().writer(init.io, &buf);
    const out = &w.interface;

    @memset(&sieve, true);
    sieve[0] = false;
    sieve[1] = false;

    var p: usize = 2;
    while (p * p <= LIMIT) : (p += 1) {
        if (sieve[p]) {
            var m: usize = p * p;
            while (m <= LIMIT) : (m += p) sieve[m] = false;
        }
    }

    var count: u64 = 0;
    var k: usize = 2;
    while (k <= LIMIT) : (k += 1) {
        if (sieve[k]) count += 1;
    }

    try out.print("{d}\n", .{count});
    try out.flush();
}
