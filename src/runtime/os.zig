const builtin = @import("builtin");

pub const Platform = enum { Linux, MacOS, Windows, Other };
pub const OsError = enum { InvalidArgument, InvalidUtf8, NotFound, Unsupported, Busy, Closed, Other };

pub fn platform() Platform {
    return switch (builtin.os.tag) {
        .linux => .Linux,
        .macos => .MacOS,
        .windows => .Windows,
        else => .Other,
    };
}
