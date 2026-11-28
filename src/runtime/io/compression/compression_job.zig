const std = @import("std");
const v = @import("../../value.zig");
const compression = @import("compression.zig");
const Scheduler = @import("../../tasks/scheduler.zig").Scheduler;
const TaskState = @import("../../tasks/scheduler.zig").TaskState;
const WakeResult = @import("../../tasks/scheduler.zig").WakeResult;
const WorkerJob = @import("../../tasks/loop/worker_pool.zig").Job;

pub const CompressionJob = struct {
    const Self = @This();

    pub const Kind = enum { compress, decompress };

    pub const Settings = struct {
        kind: Kind,
        format: compression.Format,
        level: u4 = 6,
        max: ?usize = null,
    };

    job: WorkerJob = .{ .execute = execute, .finish = finish },
    allocator: std.mem.Allocator,
    settings: Settings,
    input: []u8,
    scheduler: *Scheduler = undefined,
    task: *TaskState = undefined,
    output: ?[]u8 = null,
    failure: ?compression.CompressionError = null,

    pub fn create(allocator: std.mem.Allocator, settings: Settings, input: []const u8) !*Self {
        const entry = try allocator.create(Self);
        errdefer allocator.destroy(entry);
        const input_copy = try allocator.dupe(u8, input);
        entry.* = .{ .allocator = allocator, .settings = settings, .input = input_copy };
        return entry;
    }

    pub fn destroy(self: *Self) void {
        const allocator = self.allocator;
        if (self.output) |output| {
            allocator.free(output);
        }
        allocator.free(self.input);
        allocator.destroy(self);
    }

    fn execute(job: *WorkerJob) void {
        const self: *Self = @fieldParentPtr("job", job);
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

    fn finish(job: *WorkerJob, delivery: WorkerJob.Delivery) anyerror!void {
        const self: *Self = @fieldParentPtr("job", job);
        defer self.destroy();
        if (delivery == .discard) return;
        const wake_result = self.takeWakeResult();
        self.scheduler.wake(self.task, wake_result) catch |failure| {
            wake_result.payload.free(self.allocator);
            return failure;
        };
    }

    fn takeWakeResult(self: *Self) WakeResult {
        if (self.failure) |failure| {
            return .{ .value = .{ .int64 = @intFromEnum(failure) }, .kind = .int64, .has_failed = true, .error_is_relative = true };
        }
        const output = self.output.?;
        self.output = null;
        return .{ .value = v.NULL_VALUE, .kind = .buffer, .payload = .{ .bytes = output } };
    }
};
