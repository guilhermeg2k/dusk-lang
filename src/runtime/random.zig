const std = @import("std");

pub const Engine = std.Random.Xoshiro256;
pub const RandomError = enum { InvalidBounds, NegativeSize, EntropyUnavailable, Unsupported };

pub fn seeded(seed: i64) Engine {
    return .init(@bitCast(seed));
}

pub fn nextInt(engine: *Engine, min: i64, max: i64) error{InvalidBounds}!i64 {
    if (min > max) {
        return error.InvalidBounds;
    }
    if (min == max) {
        return min;
    }
    return engine.random().intRangeAtMost(i64, min, max);
}

pub fn nextFloat(engine: *Engine) f64 {
    return engine.random().float(f64);
}

pub fn fillBytes(engine: *Engine, bytes: []u8) void {
    if (bytes.len == 0) return;
    engine.random().bytes(bytes);
}
