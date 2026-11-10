const std = @import("std");
const v = @import("../value.zig");
const bc = @import("../bytecode.zig");

pub const INITIAL_STACK_SLOTS = 1024;
pub const INITIAL_FRAMES = 32;

pub const Scheduler = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    tasks: std.ArrayList(*TaskState) = .empty,
    ready: std.ArrayList(*TaskState) = .empty,
    ready_head: usize = 0,

    pub fn deinit(self: *Self) void {
        for (self.tasks.items) |task| task.destroy(self.allocator);
        self.tasks.deinit(self.allocator);
        self.ready.deinit(self.allocator);
    }

    pub fn spawn(self: *Self, entry: *v.Closure, result_kind: v.ValueType) !*TaskState {
        try self.tasks.ensureUnusedCapacity(self.allocator, 1);
        const task = try TaskState.create(self.allocator, entry, result_kind);
        errdefer task.destroy(self.allocator);
        try self.makeReady(task);
        self.tasks.appendAssumeCapacity(task);
        return task;
    }

    pub fn makeReady(self: *Self, task: *TaskState) !void {
        const is_mostly_consumed = self.ready_head > 0 and self.ready_head * 2 >= self.ready.items.len;
        if (is_mostly_consumed) {
            const remaining = self.ready.items.len - self.ready_head;
            std.mem.copyForwards(*TaskState, self.ready.items[0..remaining], self.ready.items[self.ready_head..]);
            self.ready.shrinkRetainingCapacity(remaining);
            self.ready_head = 0;
        }
        try self.ready.append(self.allocator, task);
        task.status = .ready;
    }

    pub fn readyCount(self: *const Self) usize {
        return self.ready.items.len - self.ready_head;
    }

    pub fn popReady(self: *Self) ?*TaskState {
        if (self.ready_head == self.ready.items.len) return null;
        const task = self.ready.items[self.ready_head];
        self.ready_head += 1;
        return task;
    }

    pub fn wake(self: *Self, task: *TaskState, wake_result: WakeResult) !void {
        task.wake_result = wake_result;
        try self.makeReady(task);
    }

    pub fn finish(self: *Self, task: *TaskState) !void {
        task.result = task.stack[0];
        task.freeStack(self.allocator);
        task.status = .done;
        const wake_result = WakeResult{ .value = task.result, .kind = task.result_kind, .has_failed = task.has_failed };
        for (task.waiters.items) |waiter| {
            try self.wake(waiter, wake_result);
        }
        task.waiters.clearAndFree(self.allocator);
    }

    pub fn hasUnfinishedTasks(self: *const Self) bool {
        for (self.tasks.items) |task| {
            if (task.status != .done) return true;
        }
        return false;
    }

    pub fn clearReachable(self: *Self) void {
        for (self.tasks.items) |task| task.is_reachable = false;
    }

    pub fn freeUnreachableTasks(self: *Self) void {
        var index: usize = 0;
        while (index < self.tasks.items.len) {
            const task = self.tasks.items[index];
            if (!task.canBeFreed()) {
                index += 1;
                continue;
            }
            task.destroy(self.allocator);
            _ = self.tasks.swapRemove(index);
        }
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
    is_awaited: bool = false,
    is_reachable: bool = false,
    result: v.Value = v.NULL_VALUE,
    result_kind: v.ValueType,
    wake_result: ?WakeResult = null,
    waiters: std.ArrayList(*TaskState) = .empty,

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
        self.freeStack(allocator);
        self.waiters.deinit(allocator);
        allocator.destroy(self);
    }

    fn freeStack(self: *Self, allocator: std.mem.Allocator) void {
        allocator.free(self.stack);
        allocator.free(self.frames);
        self.stack = &.{};
        self.frames = &.{};
        self.frame_count = 0;
    }

    pub fn park(self: *Self) void {
        self.status = .parked;
    }

    pub fn hasHeapResult(self: *const Self) bool {
        return self.status == .done and !self.has_failed and self.result_kind.isHeapType();
    }

    pub fn hasUnhandledFailure(self: *const Self) bool {
        return self.status == .done and self.has_failed and !self.is_awaited;
    }

    fn canBeFreed(self: *const Self) bool {
        return self.status == .done and !self.is_reachable and !self.hasUnhandledFailure();
    }
};

pub const CallFrame = struct {
    function: *const bc.Function,
    cur_inst: usize,
    stack_offset: usize,
};

pub const WakeResult = struct {
    value: v.Value,
    kind: v.ValueType,
    has_failed: bool = false,

    pub fn isHeapValue(self: WakeResult) bool {
        return !self.has_failed and self.kind.isHeapType();
    }
};
