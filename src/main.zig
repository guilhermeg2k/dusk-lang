const std = @import("std");
const builtin = @import("builtin");
const dusk = @import("dusk.zig");
const Dusk = dusk.Dusk;
const Stdio = dusk.Stdio;

const build_zon = @import("build_zon");
const CliErrorReporter = @import("cli_error.zig").CliErrorReporter;

pub fn main(init: std.process.Init) !void {
    openMissingStandardFiles();
    const allocator = init.arena.allocator();

    const args = try init.minimal.args.toSlice(allocator);

    var stdout_buffer: [65536]u8 = undefined;
    var stdout_file_writer = std.Io.File.stdout().writerStreaming(init.io, &stdout_buffer);
    var stderr_buffer: [1024]u8 = undefined;
    var stderr_file_writer = std.Io.File.stderr().writerStreaming(init.io, &stderr_buffer);

    var stdio = Stdio{
        .stdout = .{ .file = &stdout_file_writer },
        .stderr = .{ .file = &stderr_file_writer },
        .is_stdout_terminal = std.Io.File.stdout().isTty(init.io) catch false,
    };

    if (args.len >= 2 and std.mem.eql(u8, args[1], "--version")) {
        stdio.write(.stdout, build_zon.version, true) catch |failure| {
            if (failure == error.BrokenPipe) {
                exitAfterBrokenPipe();
            }
            std.process.exit(1);
        };
        stdio.flushStdout() catch |failure| {
            if (failure == error.BrokenPipe) {
                exitAfterBrokenPipe();
            }
            std.process.exit(1);
        };
        return;
    }

    if (args.len < 2) {
        CliErrorReporter.usage();
        std.process.exit(1);
    }

    const file_name = args[1];
    var dusk_c = Dusk{ .allocator = allocator, .stdio = stdio, .io = init.io };
    dusk_c.runFile(file_name) catch |failure| {
        if (failure == error.BrokenPipe) {
            exitAfterBrokenPipe();
        }
        std.process.exit(1);
    };
}

fn openMissingStandardFiles() void {
    if (builtin.os.tag == .windows) return;
    for (0..3) |file| {
        const is_closed = std.posix.errno(std.c.fcntl(@intCast(file), std.posix.F.GETFD)) == .BADF;
        if (is_closed) {
            _ = std.c.open("/dev/null", .{ .ACCMODE = .RDWR });
        }
    }
}

fn exitAfterBrokenPipe() noreturn {
    if (builtin.os.tag != .windows) {
        const default_action: std.posix.Sigaction = .{
            .handler = .{ .handler = std.posix.SIG.DFL },
            .mask = std.posix.sigemptyset(),
            .flags = 0,
        };
        std.posix.sigaction(.PIPE, &default_action, null);
        std.posix.raise(.PIPE) catch {};
    }
    std.process.exit(1);
}
