const std = @import("std");

const N: usize = 10000;

var data: [N]i64 = undefined;

fn quicksort(a: []i64, lo: isize, hi: isize) void {
    if (lo >= hi) return;

    const mid: usize = @intCast(lo + @divTrunc(hi - lo, 2));
    const pivot = a[mid];
    var i: isize = lo;
    var j: isize = hi;

    while (i <= j) {
        while (a[@intCast(i)] < pivot) i += 1;
        while (a[@intCast(j)] > pivot) j -= 1;
        if (i <= j) {
            const ui: usize = @intCast(i);
            const uj: usize = @intCast(j);
            const t = a[ui];
            a[ui] = a[uj];
            a[uj] = t;
            i += 1;
            j -= 1;
        }
    }

    quicksort(a, lo, j);
    quicksort(a, i, hi);
}

pub fn main(init: std.process.Init) !void {
    var buf: [256]u8 = undefined;
    var w = std.Io.File.stdout().writer(init.io, &buf);
    const out = &w.interface;

    var i: usize = 0;
    while (i < N) : (i += 1) data[i] = @intCast(N - i);

    quicksort(&data, 0, @intCast(N - 1));

    try out.print("{d}\n", .{data[5000]});
    try out.flush();
}
