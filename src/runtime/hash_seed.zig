const std = @import("std");

var generator = std.Random.SplitMix64.init(0);

pub fn next() u64 {
    return generator.next();
}

pub fn randomize(io: std.Io) void {
    var random_start: u64 = undefined;
    io.random(std.mem.asBytes(&random_start));
    generator = .init(random_start);
}
