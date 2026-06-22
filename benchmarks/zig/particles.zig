const std = @import("std");

const N: usize = 100000;

const Particle = struct { x: f64, y: f64, vx: f64, vy: f64 };

var particles: [N]Particle = undefined;

pub fn main(init: std.process.Init) !void {
    var buf: [256]u8 = undefined;
    var w = std.Io.File.stdout().writer(init.io, &buf);
    const out = &w.interface;

    var i: usize = 0;
    while (i < N) : (i += 1) {
        const fi: f64 = @floatFromInt(i);
        particles[i] = .{ .x = fi, .y = fi * 2.0, .vx = 0.5, .vy = -0.5 };
    }

    var step: usize = 0;
    while (step < 100) : (step += 1) {
        i = 0;
        while (i < N) : (i += 1) {
            particles[i].x += particles[i].vx;
            particles[i].y += particles[i].vy;
        }
    }

    try out.print("{d}\n", .{particles[49999].x});
    try out.flush();
}
