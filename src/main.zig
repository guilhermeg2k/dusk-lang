pub fn main(init: std.process.Init) !void {
    const allocator = init.arena.allocator();

    const args = try init.minimal.args.toSlice(init.arena.allocator());
    if (args.len < 2) {
        std.log.err("Invalid number of arguments\n", .{});
        return;
    }

    const file_name = args[1];
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
