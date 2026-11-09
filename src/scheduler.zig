const std = @import("std");
const v = @import("value.zig");
const CallFrame = @import("vm.zig").CallFrame;

pub const INITIAL_STACK_SLOTS = 1024;
pub const INITIAL_FRAMES = 32;

pub const TaskState = struct {
    const Self = @This();

    pub const Status = enum { ready, running, parked, done };

    stack: []v.Value,
    frames: []CallFrame,
    frame_count: usize,
    status: Status = .ready,
    has_failed: bool = false,

    pub fn create(allocator: std.mem.Allocator, entry: *v.Closure) !*Self {
        const stack = try allocator.alloc(v.Value, INITIAL_STACK_SLOTS);
        errdefer allocator.free(stack);
        @memset(stack, v.NULL_VALUE);
        const frames = try allocator.alloc(CallFrame, INITIAL_FRAMES);
        errdefer allocator.free(frames);

        stack[0] = .{ .heap_obj = &entry.obj };
        frames[0] = .{ .function = entry.function, .cur_inst = 0, .stack_offset = 1 };

        const task = try allocator.create(Self);
        task.* = .{ .stack = stack, .frames = frames, .frame_count = 1 };
        return task;
    }

    pub fn destroy(self: *Self, allocator: std.mem.Allocator) void {
        allocator.free(self.stack);
        allocator.free(self.frames);
        allocator.destroy(self);
    }
};
