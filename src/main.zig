const std = @import("std");
const dusk = @import("dusk.zig");
const Dusk = dusk.Dusk;

pub fn main(init: std.process.Init) !void {
    const allocator = init.arena.allocator();

    const args = try init.minimal.args.toSlice(allocator);
    if (args.len < 2) {
        std.log.err("Invalid number of arguments\n", .{});
        return;
    }

    const file_name = args[1];
    var buffer: [65536]u8 = undefined;
    var stdout_file_writer = std.Io.File.stdout().writer(init.io, &buffer);
    const stdout_writer = &stdout_file_writer.interface;

    var dusk_c = Dusk{ .allocator = allocator, .stdout_writer = stdout_writer, .io = init.io };
    try dusk_c.runFile(file_name);
}
