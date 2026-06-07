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

    const use_wasm = args.len > 1 and std.mem.eql(u8, args[1], "--wasm");
    const file_idx: usize = if (use_wasm) @as(usize, 2) else 1;
    if (args.len <= file_idx) {
        std.log.err("Missing file argument\n", .{});
        return;
    }
    const file_name = args[file_idx];

    var buffer: [65536]u8 = undefined;
    var stdout_file_writer = std.Io.File.stdout().writer(init.io, &buffer);
    const stdout_writer = &stdout_file_writer.interface;

    var dusk_c = Dusk{ .allocator = allocator, .stdout_writer = stdout_writer, .io = init.io };

    if (use_wasm) {
        const cwd = std.Io.Dir.cwd();
        const src = try cwd.readFileAlloc(init.io, file_name, allocator, .unlimited);
        try dusk_c.compileToWasm(src);
    } else {
        try dusk_c.runFile(file_name);
    }
}
