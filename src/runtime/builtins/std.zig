const std = @import("std");
const native = @import("native.zig");
const should_use_reflection_json = @import("build_options").json_backend == .reflection;

pub const Module = struct {
    path: []const u8,
    source: []const u8,
    bindings: *const native.BindingMap,
};

pub const modules = [_]Module{
    .{ .path = "reflect", .source = @embedFile("std/reflect.dsk"), .bindings = &@import("std/reflect.zig").bindings },
    .{ .path = "json", .source = if (should_use_reflection_json) @embedFile("std/json_reflected.dsk") else @embedFile("std/json.dsk"), .bindings = if (should_use_reflection_json) &@import("std/json_reflected.zig").bindings else &@import("std/json.zig").bindings },
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
};

pub fn find(path: []const u8) ?*const Module {
    for (&modules) |*module| {
        if (std.mem.eql(u8, module.path, path)) return module;
    }
    return null;
}
