const std = @import("std");
const uv = @import("uv.zig");
const v = @import("../value.zig");
const EventLoop = @import("event_loop.zig").EventLoop;
const Scheduler = @import("scheduler.zig").Scheduler;
const TaskState = @import("scheduler.zig").TaskState;
const WakeResult = @import("scheduler.zig").WakeResult;

const request_alignment = std.mem.Alignment.@"16";

const mode_type_mask = 0o170000;
const mode_directory = 0o040000;
const mode_regular_file = 0o100000;
const mode_symlink = 0o120000;
const mode_permission_mask = 0o777;

pub const FsJob = struct {
    const Self = @This();

    pub const Kind = enum { exists, stat };

    allocator: std.mem.Allocator,
    kind: Kind,
    path: [:0]u8,
    work_memory: []align(16) u8,
    fs_memory: []align(16) u8,
    event_loop: *EventLoop = undefined,
    scheduler: *Scheduler = undefined,
    task: *TaskState = undefined,
    loop: *uv.Loop = undefined,
    failure: ?FsError = null,
    is_found: bool = false,
    stat_numbers: ?[]i64 = null,

    pub fn create(allocator: std.mem.Allocator, kind: Kind, path: []const u8) !*Self {
        const job = try allocator.create(Self);
        errdefer allocator.destroy(job);
        const path_copy = try allocator.dupeZ(u8, path);
        errdefer allocator.free(path_copy);
        const work_memory = try allocator.alignedAlloc(u8, request_alignment, uv.uv_req_size(.work));
        errdefer allocator.free(work_memory);
        const fs_memory = try allocator.alignedAlloc(u8, request_alignment, uv.uv_req_size(.fs));
        job.* = .{ .allocator = allocator, .kind = kind, .path = path_copy, .work_memory = work_memory, .fs_memory = fs_memory };
        return job;
    }

    pub fn destroy(self: *Self) void {
        const allocator = self.allocator;
        if (self.stat_numbers) |numbers| {
            allocator.free(numbers);
        }
        allocator.free(self.fs_memory);
        allocator.free(self.work_memory);
        allocator.free(self.path);
        allocator.destroy(self);
    }

    pub fn runOnPoolThread(self: *Self) void {
        switch (self.kind) {
            .exists => self.checkPathExists(),
            .stat => self.statPath(),
        }
    }

    fn checkPathExists(self: *Self) void {
        const request = self.fsRequest();
        self.is_found = uv.uv_fs_stat(self.loop, request, self.path.ptr, null) >= 0;
        uv.uv_fs_req_cleanup(request);
    }

    fn statPath(self: *Self) void {
        const request = self.fsRequest();
        const link_status = uv.uv_fs_lstat(self.loop, request, self.path.ptr, null);
        if (link_status < 0) {
            uv.uv_fs_req_cleanup(request);
            self.failure = failureFromStatus(link_status);
            return;
        }
        var info = uv.uv_fs_get_statbuf(request).*;
        uv.uv_fs_req_cleanup(request);

        const is_symlink = info.st_mode & mode_type_mask == mode_symlink;
        if (is_symlink) {
            if (uv.uv_fs_stat(self.loop, request, self.path.ptr, null) >= 0) {
                info = uv.uv_fs_get_statbuf(request).*;
            }
            uv.uv_fs_req_cleanup(request);
        }

        const numbers = self.allocator.alloc(i64, 7) catch {
            self.failure = .Other;
            return;
        };
        const file_type = info.st_mode & mode_type_mask;
        const is_dir = file_type == mode_directory;
        numbers[0] = if (is_dir) 0 else @intCast(info.st_size);
        numbers[1] = @intFromBool(is_dir);
        numbers[2] = @intFromBool(file_type == mode_regular_file);
        numbers[3] = @intFromBool(is_symlink);
        numbers[4] = millisecondsOf(info.st_mtim);
        numbers[5] = millisecondsOf(info.st_birthtim);
        numbers[6] = @intCast(info.st_mode & mode_permission_mask);
        self.stat_numbers = numbers;
    }

    fn fsRequest(self: *Self) *uv.Request {
        return @ptrCast(self.fs_memory.ptr);
    }

    pub fn takeWakeResult(self: *Self) WakeResult {
        if (self.failure) |failure| {
            return .{ .value = .{ .int64 = @intFromEnum(failure) }, .kind = .int64, .has_failed = true, .error_is_relative = true };
        }
        return switch (self.kind) {
            .exists => .{ .value = .{ .bool = self.is_found }, .kind = .bool },
            .stat => result: {
                const numbers = self.stat_numbers.?;
                self.stat_numbers = null;
                break :result .{ .value = v.NULL_VALUE, .kind = .array, .payload = .{ .ints = numbers } };
            },
        };
    }

    pub fn workRequest(self: *Self) *uv.Request {
        return @ptrCast(self.work_memory.ptr);
    }
};

pub const FsError = enum {
    NotFound,
    PermissionDenied,
    AlreadyExists,
    IsDirectory,
    NotDirectory,
    NotEmpty,
    InvalidUtf8,
    Other,
};

fn millisecondsOf(time: uv.Timespec) i64 {
    return @as(i64, time.tv_sec) * 1000 + @divTrunc(@as(i64, time.tv_nsec), 1_000_000);
}

fn failureFromStatus(status: c_int) FsError {
    const name = std.mem.span(uv.uv_err_name(status));
    const failures = [_]struct { []const u8, FsError }{
        .{ "ENOENT", .NotFound },
        .{ "EACCES", .PermissionDenied },
        .{ "EPERM", .PermissionDenied },
        .{ "EEXIST", .AlreadyExists },
        .{ "EISDIR", .IsDirectory },
        .{ "ENOTDIR", .NotDirectory },
        .{ "ENOTEMPTY", .NotEmpty },
    };
    for (failures) |entry| {
        if (std.mem.eql(u8, name, entry[0])) {
            return entry[1];
        }
    }
    return .Other;
}
