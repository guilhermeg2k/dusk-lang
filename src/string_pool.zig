const std = @import("std");
const v = @import("value.zig");

pub const StringPool = struct {
    const Self = @This();
    const alignment = std.mem.Alignment.fromByteUnits(@alignOf(v.Bytes));

    memory: []align(@alignOf(v.Bytes)) u8 = &.{},
    values: []const v.Value = &.{},

    pub fn init(allocator: std.mem.Allocator, texts: []const []const u8) !Self {
        var total: usize = 0;
        for (texts) |text| total += alignment.forward(v.Bytes.calc_size(text.len));

        const memory = try allocator.alignedAlloc(u8, alignment, total);
        const values = try allocator.alloc(v.Value, texts.len);
        var offset: usize = 0;
        for (texts, 0..) |text, i| {
            const bytes: *v.Bytes = @ptrCast(@alignCast(memory[offset..].ptr));
            bytes.obj = .{ .kind = .string };
            bytes.len = text.len;
            @memcpy(bytes.getDataPtr()[0..text.len], text);
            values[i] = .{ .heap_obj = &bytes.obj };
            offset += alignment.forward(v.Bytes.calc_size(text.len));
        }

        return .{ .memory = memory, .values = values };
    }

    pub fn contains(self: *const Self, obj: *const v.HeapValue) bool {
        const address = @intFromPtr(obj);
        const start = @intFromPtr(self.memory.ptr);
        return address >= start and address < start + self.memory.len;
    }

    pub fn textAt(self: *const Self, index: usize) []const u8 {
        return v.HeapValue.getParentPtr(v.Bytes, self.values[index].heap_obj).slice();
    }
};
