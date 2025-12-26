pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    var args = std.process.args();
    _ = args.next();
    const file_name = args.next();

    var dusk_c = try Dusk.init(allocator);
    if (file_name) |name| {
        try dusk_c.compileAndRunFile(name);
    }
}

const std = @import("std");
const dusk = @import("dusk.zig");
const Dusk = dusk.Dusk;
