const std = @import("std");
const dusk = @import("dusk.zig");
const Dusk = dusk.Dusk;

const build_zon = @import("build_zon");
const CliErrorReporter = @import("error.zig").CliErrorReporter;

pub fn main(init: std.process.Init) !void {
    const allocator = init.arena.allocator();

    const args = try init.minimal.args.toSlice(allocator);

    var buffer: [65536]u8 = undefined;
    var stdout_file_writer = std.Io.File.stdout().writer(init.io, &buffer);
    const stdout_writer = &stdout_file_writer.interface;

    if (args.len >= 2 and std.mem.eql(u8, args[1], "--version")) {
        try stdout_writer.print("{s}\n", .{build_zon.version});
        try stdout_writer.flush();
        return;
    }

    if (args.len < 2) {
        CliErrorReporter.usage();
        std.process.exit(1);
    }

    const file_name = args[1];
    var dusk_c = Dusk{ .allocator = allocator, .stdout_writer = stdout_writer, .io = init.io };
    dusk_c.runFile(file_name) catch {
        stdout_writer.flush() catch {};
        std.process.exit(1);
    };
}
