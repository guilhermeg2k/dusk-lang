const std = @import("std");
const v = @import("../../value.zig");
const compression = @import("compression.zig");
const EventLoop = @import("../../tasks/event_loop.zig").EventLoop;
const Scheduler = @import("../../tasks/scheduler.zig").Scheduler;
const TaskState = @import("../../tasks/scheduler.zig").TaskState;
const WakeResult = @import("../../tasks/scheduler.zig").WakeResult;

const request_alignment = std.mem.Alignment.@"16";

pub const CompressionJob = struct {
    const Self = @This();

    pub const Kind = enum { compress, decompress };

    pub const Settings = struct {
        kind: Kind,
        format: compression.Format,
        level: u4 = 6,
        max: ?usize = null,
    };

    allocator: std.mem.Allocator,
    settings: Settings,
    input: []u8,
    work_memory: []align(16) u8,
    event_loop: *EventLoop = undefined,
    scheduler: *Scheduler = undefined,
    task: *TaskState = undefined,
    output: ?[]u8 = null,
    failure: ?compression.CompressionError = null,

    pub fn create(allocator: std.mem.Allocator, settings: Settings, input: []const u8, work_size: usize) !*Self {
        const job = try allocator.create(Self);
        errdefer allocator.destroy(job);
        const input_copy = try allocator.dupe(u8, input);
        errdefer allocator.free(input_copy);
        const work_memory = try allocator.alignedAlloc(u8, request_alignment, work_size);
        job.* = .{ .allocator = allocator, .settings = settings, .input = input_copy, .work_memory = work_memory };
        return job;
    }

    pub fn destroy(self: *Self) void {
        const allocator = self.allocator;
        if (self.output) |output| {
            allocator.free(output);
        }
        allocator.free(self.work_memory);
        allocator.free(self.input);
        allocator.destroy(self);
    }

    pub fn runOnPoolThread(self: *Self) void {
        const settings = self.settings;
        const result = switch (settings.kind) {
            .compress => compression.compressAll(self.allocator, settings.format, settings.level, self.input),
            .decompress => compression.decompressAll(self.allocator, settings.format, self.input, settings.max),
        };
        self.output = result catch |err| {
            self.failure = switch (err) {
                error.InvalidData => .InvalidData,
                error.TooLarge => .TooLarge,
                error.OutOfMemory => .Other,
            };
            return;
        };
    }

    pub fn takeWakeResult(self: *Self) WakeResult {
        if (self.failure) |failure| {
            return .{ .value = .{ .int64 = @intFromEnum(failure) }, .kind = .int64, .has_failed = true, .error_is_relative = true };
        }
        const output = self.output.?;
        self.output = null;
        return .{ .value = v.NULL_VALUE, .kind = .buffer, .payload = .{ .bytes = output } };
    }

    pub fn workRequest(self: *Self) *anyopaque {
        return @ptrCast(self.work_memory.ptr);
    }
};
