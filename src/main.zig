pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    var args = std.process.args();
    _ = args.next();
    const file_name = args.next();

    var buffer: [65536]u8 = undefined;
    const stdout_writer: std.io.Writer = std.fs.File.stdout().writer(&buffer).interface;

    var dusk_c = Dusk{ .allocator = allocator, .stdout_writer = stdout_writer };
    if (file_name) |name| {
        try dusk_c.runFile(name);
    }
}

const std = @import("std");
const dusk = @import("dusk.zig");
const Dusk = dusk.Dusk;
