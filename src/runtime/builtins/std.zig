const std = @import("std");
const native = @import("native.zig");

pub const Module = struct {
    path: []const u8,
    source: []const u8,
    bindings: *const native.BindingMap,
};

pub const modules = [_]Module{
    .{ .path = "math", .source = @embedFile("std/math.dsk"), .bindings = &@import("std/math.zig").bindings },
    .{ .path = "buffer", .source = @embedFile("std/buffer.dsk"), .bindings = &@import("std/buffer.zig").bindings },
    .{ .path = "string", .source = @embedFile("std/string.dsk"), .bindings = &@import("std/string.zig").bindings },
    .{ .path = "array", .source = @embedFile("std/array.dsk"), .bindings = &@import("std/array.zig").bindings },
    .{ .path = "task", .source = @embedFile("std/task.dsk"), .bindings = &@import("std/task.zig").bindings },
    .{ .path = "map", .source = @embedFile("std/map.dsk"), .bindings = &@import("std/map.zig").bindings },
    .{ .path = "io", .source = @embedFile("std/io.dsk"), .bindings = &@import("std/io.zig").bindings },
    .{ .path = "fs", .source = @embedFile("std/fs.dsk"), .bindings = &@import("std/fs.zig").bindings },
    .{ .path = "time", .source = @embedFile("std/time.dsk"), .bindings = &@import("std/time.zig").bindings },
    .{ .path = "net", .source = @embedFile("std/net.dsk"), .bindings = &@import("std/net.zig").bindings },
    .{ .path = "url", .source = @embedFile("std/url.dsk"), .bindings = &@import("std/url.zig").bindings },
    .{ .path = "http", .source = @embedFile("std/http.dsk"), .bindings = &@import("std/http.zig").bindings },
    .{ .path = "json", .source = @embedFile("std/json.dsk"), .bindings = &@import("std/json.zig").bindings },
    .{ .path = "serialization", .source = @embedFile("std/serialization.dsk"), .bindings = &@import("std/serialization.zig").bindings },
};

pub fn find(path: []const u8) ?*const Module {
    for (&modules) |*module| {
        if (std.mem.eql(u8, module.path, path)) return module;
    }
    return null;
}
