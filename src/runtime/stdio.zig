const std = @import("std");
const RuntimeErrorReporter = @import("runtime_error.zig").RuntimeErrorReporter;

pub const Stream = enum { stdout, stderr };

pub const Stdio = struct {
    const Self = @This();

    stdout: Output = .none,
    stderr: Output = .none,
    is_stdout_terminal: bool = false,

    pub fn writeBytes(self: *Self, stream: Stream, bytes: []const u8) !void {
        try self.append(stream, bytes);
        try self.flushAfterWrite(stream, bytes);
    }

    pub fn writeLine(self: *Self, stream: Stream, bytes: []const u8) !void {
        try self.append(stream, bytes);
        try self.append(stream, "\n");
        try self.flushAfterWrite(stream, "\n");
    }

    pub fn flushStdout(self: *Self) !void {
        self.stdout.flush() catch |failure| return reportWriteFailure(.stdout, failure);
    }

    pub fn tryWriteBytes(self: *Self, stream: Stream, bytes: []const u8) anyerror!void {
        const out = self.output(stream);
        errdefer out.discardBuffered();
        try out.writeAll(bytes);
        if (self.needsFlushAfterWrite(stream, bytes)) {
            try out.flush();
        }
    }

    pub fn tryFlush(self: *Self, stream: Stream) anyerror!void {
        const out = self.output(stream);
        errdefer out.discardBuffered();
        try out.flush();
    }

    fn append(self: *Self, stream: Stream, bytes: []const u8) !void {
        self.output(stream).writeAll(bytes) catch |failure| return reportWriteFailure(stream, failure);
    }

    fn flushAfterWrite(self: *Self, stream: Stream, written: []const u8) !void {
        if (!self.needsFlushAfterWrite(stream, written)) return;
        self.output(stream).flush() catch |failure| return reportWriteFailure(stream, failure);
    }

    fn needsFlushAfterWrite(self: *const Self, stream: Stream, written: []const u8) bool {
        return switch (stream) {
            .stdout => self.is_stdout_terminal and std.mem.indexOfScalar(u8, written, '\n') != null,
            .stderr => true,
        };
    }

    fn output(self: *const Self, stream: Stream) Output {
        return switch (stream) {
            .stdout => self.stdout,
            .stderr => self.stderr,
        };
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

    fn discardBuffered(self: Output) void {
        const out = self.writer() orelse return;
        out.end = 0;
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
