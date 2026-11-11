const std = @import("std");
const native = @import("native.zig");

pub const Module = struct {
    path: []const u8,
    source: []const u8,
    bindings: *const native.BindingMap,
};

pub const modules = [_]Module{
    .{ .path = "task", .source = @embedFile("std/task.dsk"), .bindings = &@import("std/task.zig").bindings },
};

pub fn find(path: []const u8) ?*const Module {
    for (&modules) |*module| {
        if (std.mem.eql(u8, module.path, path)) return module;
    }
    return null;
}
