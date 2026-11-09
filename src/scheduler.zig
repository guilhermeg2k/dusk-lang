const std = @import("std");
const v = @import("value.zig");
const CallFrame = @import("vm.zig").CallFrame;

pub const INITIAL_STACK_SLOTS = 1024;
pub const INITIAL_FRAMES = 32;

pub const Scheduler = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    tasks: std.ArrayList(*TaskState) = .empty,

    pub fn spawn(self: *Self, entry: *v.Closure, result_kind: v.ValueType) !*TaskState {
        try self.tasks.ensureUnusedCapacity(self.allocator, 1);
        const task = try TaskState.create(self.allocator, entry, result_kind);
        self.tasks.appendAssumeCapacity(task);
        return task;
    }

    pub fn deinit(self: *Self) void {
        for (self.tasks.items) |task| task.destroy(self.allocator);
        self.tasks.deinit(self.allocator);
    }
};

pub const TaskState = struct {
    const Self = @This();

    pub const Status = enum { ready, running, parked, done };

    stack: []v.Value,
    frames: []CallFrame,
    frame_count: usize,
    status: Status = .ready,
    has_failed: bool = false,
    result_kind: v.ValueType,

    pub fn create(allocator: std.mem.Allocator, entry: *v.Closure, result_kind: v.ValueType) !*Self {
        const stack = try allocator.alloc(v.Value, INITIAL_STACK_SLOTS);
        errdefer allocator.free(stack);
        @memset(stack, v.NULL_VALUE);
        const frames = try allocator.alloc(CallFrame, INITIAL_FRAMES);
        errdefer allocator.free(frames);

        stack[0] = .{ .heap_obj = &entry.obj };
        frames[0] = .{ .function = entry.function, .cur_inst = 0, .stack_offset = 1 };

        const task = try allocator.create(Self);
        task.* = .{ .stack = stack, .frames = frames, .frame_count = 1, .result_kind = result_kind };
        return task;
    }

    pub fn destroy(self: *Self, allocator: std.mem.Allocator) void {
        allocator.free(self.stack);
        allocator.free(self.frames);
        allocator.destroy(self);
    }
};
