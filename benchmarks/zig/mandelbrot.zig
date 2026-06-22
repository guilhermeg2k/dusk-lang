const std = @import("std");

const W: usize = 800;
const H: usize = 800;
const MAX_ITER: u32 = 1000;

pub fn main(init: std.process.Init) !void {
    var buf: [256]u8 = undefined;
    var w = std.Io.File.stdout().writer(init.io, &buf);
    const out = &w.interface;

    const fw: f64 = @floatFromInt(W);
    const fh: f64 = @floatFromInt(H);

    var sum: u64 = 0;
    var py: usize = 0;
    while (py < H) : (py += 1) {
        const cy: f64 = -1.0 + (@as(f64, @floatFromInt(py)) / fh) * 2.0;
        var px: usize = 0;
        while (px < W) : (px += 1) {
            const cx: f64 = -2.5 + (@as(f64, @floatFromInt(px)) / fw) * 3.5;

            var zx: f64 = 0.0;
            var zy: f64 = 0.0;
            var zx2: f64 = 0.0;
            var zy2: f64 = 0.0;
            var iter: u32 = 0;
            while (zx2 + zy2 <= 4.0 and iter < MAX_ITER) : (iter += 1) {
                zy = 2.0 * zx * zy + cy;
                zx = zx2 - zy2 + cx;
                zx2 = zx * zx;
                zy2 = zy * zy;
            }
            sum += iter;
        }
    }

    try out.print("{d}\n", .{sum});
    try out.flush();
}
