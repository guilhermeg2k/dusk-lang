const std = @import("std");

pub const CliErrorReporter = struct {
    pub fn usage() void {
        std.debug.print("usage: dusk <file.dsk>\n       dusk --version\n", .{});
    }

    pub fn cantReadFile(path: []const u8, failure: anyerror) anyerror {
        const reason = switch (failure) {
            error.FileNotFound => "file not found",
            error.AccessDenied, error.PermissionDenied => "permission denied",
            error.IsDir => "it's a directory",
            else => @errorName(failure),
        };
        std.debug.print("error: can't read '{s}': {s}\n", .{ path, reason });
        return failure;
    }
};
