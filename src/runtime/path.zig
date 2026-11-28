const std = @import("std");
const builtin = @import("builtin");

pub const Style = enum { posix, windows };
pub const PathError = enum { NotAbsolute };
pub const NativePaths = Paths(if (builtin.os.tag == .windows) .windows else .posix);

pub fn Paths(comptime style: Style) type {
    return struct {
        pub fn normalize(allocator: std.mem.Allocator, text: []const u8) ![]u8 {
            return normalizeParts(allocator, &.{text});
        }

        fn normalizeParts(allocator: std.mem.Allocator, parts: []const []const u8) ![]u8 {
            return switch (style) {
                .posix => std.fs.path.resolvePosix(allocator, parts),
                .windows => std.fs.path.resolveWindows(allocator, parts),
            };
        }

        pub fn basename(text: []const u8) []const u8 {
            return switch (style) {
                .posix => std.fs.path.basenamePosix(text),
                .windows => std.fs.path.basenameWindows(text),
            };
        }

        pub fn dirname(text: []const u8) ?[]const u8 {
            return switch (style) {
                .posix => std.fs.path.dirnamePosix(text),
                .windows => std.fs.path.dirnameWindows(text),
            };
        }

        pub fn extension(text: []const u8) []const u8 {
            return basenameExtension(basename(text));
        }

        fn basenameExtension(name: []const u8) []const u8 {
            if (std.mem.eql(u8, name, "..")) {
                return name[name.len..];
            }
            const index = std.mem.lastIndexOfScalar(u8, name, '.') orelse return name[name.len..];
            if (index == 0) {
                return name[name.len..];
            }
            return name[index..];
        }

        pub fn stem(text: []const u8) []const u8 {
            const name = basename(text);
            return name[0 .. name.len - basenameExtension(name).len];
        }

        pub fn isAbsolute(text: []const u8) bool {
            return switch (style) {
                .posix => std.fs.path.isAbsolutePosix(text),
                .windows => switch (std.fs.path.getWin32PathType(u8, text)) {
                    .drive_absolute, .unc_absolute, .local_device, .root_local_device => true,
                    .relative, .rooted, .drive_relative => false,
                },
            };
        }

        pub fn resolve(allocator: std.mem.Allocator, base: []const u8, text: []const u8) ![]u8 {
            if (!isAbsolute(base)) {
                return error.NotAbsolute;
            }
            const result = try normalizeParts(allocator, &.{ base, text });
            errdefer allocator.free(result);
            if (!isAbsolute(result)) {
                return error.NotAbsolute;
            }
            return result;
        }

        pub fn relative(allocator: std.mem.Allocator, from: []const u8, to: []const u8) ![]u8 {
            const are_absolute = isAbsolute(from) and isAbsolute(to);
            if (!are_absolute) {
                return error.NotAbsolute;
            }
            const result = try switch (style) {
                .posix => std.fs.path.relativePosix(allocator, from, from, to),
                .windows => std.fs.path.relativeWindows(allocator, from, null, from, to),
            };
            if (result.len == 0) {
                allocator.free(result);
                return allocator.dupe(u8, ".");
            }
            return result;
        }
    };
}
