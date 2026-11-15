const std = @import("std");
const RuntimeErrorReporter = @import("runtime_error.zig").RuntimeErrorReporter;

pub const Stream = enum { stdout, stderr };

pub const Stdio = struct {
    const Self = @This();

    stdout: Output = .none,
    stderr: Output = .none,
    is_stdout_terminal: bool = false,

    pub fn writeBytes(self: *Self, stream: Stream, bytes: []const u8) !void {
        switch (stream) {
            .stdout => {
                self.stdout.writeAll(bytes) catch |failure| return reportWriteFailure(.stdout, failure);
                const has_finished_line = self.is_stdout_terminal and std.mem.indexOfScalar(u8, bytes, '\n') != null;
                if (has_finished_line) {
                    try self.flushStdout();
                }
            },
            .stderr => {
                self.stderr.writeAll(bytes) catch |failure| return reportWriteFailure(.stderr, failure);
                self.stderr.flush() catch |failure| return reportWriteFailure(.stderr, failure);
            },
        }
    }

    pub fn flushStdout(self: *Self) !void {
        self.stdout.flush() catch |failure| return reportWriteFailure(.stdout, failure);
    }
};

pub const Output = union(enum) {
    none,
    file: *std.Io.File.Writer,
    custom: *std.Io.Writer,

    pub fn writer(self: Output) ?*std.Io.Writer {
        return switch (self) {
            .none => null,
            .file => |file| &file.interface,
            .custom => |custom| custom,
        };
    }

    fn writeAll(self: Output, bytes: []const u8) anyerror!void {
        const out = self.writer() orelse return;
        out.writeAll(bytes) catch return self.writeFailureCause();
    }

    fn flush(self: Output) anyerror!void {
        const out = self.writer() orelse return;
        out.flush() catch return self.writeFailureCause();
    }

    fn writeFailureCause(self: Output) anyerror {
        return switch (self) {
            .file => |file| file.err orelse error.WriteFailed,
            .none, .custom => error.WriteFailed,
        };
    }
};

fn reportWriteFailure(stream: Stream, failure: anyerror) anyerror {
    if (failure == error.BrokenPipe) {
        return error.BrokenPipe;
    }
    return RuntimeErrorReporter.cantWriteOutput(@tagName(stream), failure);
}
