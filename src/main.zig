const std = @import("std");
const builtin = @import("builtin");
const dusk = @import("dusk.zig");
const Dusk = dusk.Dusk;
const Stdio = dusk.Stdio;

const build_zon = @import("build_zon");
const CliErrorReporter = @import("cli_error.zig").CliErrorReporter;
const CompileErrorReporter = @import("compiler/compile_error.zig").CompileErrorReporter;
const RuntimeErrorReporter = @import("runtime/runtime_error.zig").RuntimeErrorReporter;

pub fn main(init: std.process.Init) !u8 {
    openMissingStandardFiles();
    raiseOpenFileLimit();
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

    const is_version_request = args.len >= 2 and std.mem.eql(u8, args[1], "--version");
    if (is_version_request) {
        stdio.writeLine(.stdout, build_zon.version) catch |failure| {
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
        return 0;
    }

    if (args.len < 2) {
        CliErrorReporter.usage();
        std.process.exit(1);
    }

    enableTerminalEscapeCodes(init.io);
    const file_name = args[1];
    var dusk_c = Dusk{ .allocator = allocator, .stdio = stdio, .io = init.io, .program_args = args[2..], .environment = init.environ_map };
    dusk_c.runFile(file_name) catch |failure| {
        if (failure == error.BrokenPipe) {
            exitAfterBrokenPipe();
        }
        reportUnreportedFailure(failure);
        std.process.exit(1);
    };
    try stderr_file_writer.interface.flush();
    return dusk_c.exit_code;
}

fn enableTerminalEscapeCodes(io: std.Io) void {
    if (builtin.os.tag != .windows) return;
    for ([_]std.Io.File{ std.Io.File.stdout(), std.Io.File.stderr() }) |file| {
        file.enableAnsiEscapeCodes(io) catch {};
    }
}

fn openMissingStandardFiles() void {
    if (builtin.os.tag == .windows) return;
    for (0..3) |fd| {
        const is_closed = std.posix.errno(std.c.fcntl(@intCast(fd), std.posix.F.GETFD)) == .BADF;
        if (is_closed) {
            _ = std.c.open("/dev/null", .{ .ACCMODE = .RDWR });
        }
    }
}

fn raiseOpenFileLimit() void {
    if (builtin.os.tag == .windows) return;
    var limits = std.posix.getrlimit(.NOFILE) catch return;
    const wanted = if (builtin.os.tag.isDarwin()) @min(limits.max, std.c.OPEN_MAX) else limits.max;
    if (limits.cur >= wanted) return;
    limits.cur = wanted;
    std.posix.setrlimit(.NOFILE, limits) catch return;
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

fn reportUnreportedFailure(failure: anyerror) void {
    const is_reported = CompileErrorReporter.has_reported_error or RuntimeErrorReporter.has_reported_error or CliErrorReporter.has_reported_error;
    if (is_reported) return;
    std.debug.print("error: {s}\n", .{@errorName(failure)});
}
