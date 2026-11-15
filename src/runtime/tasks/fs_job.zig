const std = @import("std");
const builtin = @import("builtin");
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
const open_read_only = 0;
const open_to_replace = writeOpenFlags(.{ .should_truncate = true });
const open_to_append = writeOpenFlags(.{ .should_append = true });
const new_file_mode = 0o666;
const min_read_capacity = 4096;

pub const FsJob = struct {
    const Self = @This();

    pub const Kind = enum { exists, stat, read_text, read_bytes, write, append };

    allocator: std.mem.Allocator,
    kind: Kind,
    path: [:0]u8,
    input: []u8,
    work_memory: []align(16) u8,
    fs_memory: []align(16) u8,
    event_loop: *EventLoop = undefined,
    scheduler: *Scheduler = undefined,
    task: *TaskState = undefined,
    loop: *uv.Loop = undefined,
    failure: ?FsError = null,
    is_found: bool = false,
    stat_numbers: ?[]i64 = null,
    contents: ?[]u8 = null,

    pub fn create(allocator: std.mem.Allocator, kind: Kind, path: []const u8, input: []const u8) !*Self {
        const job = try allocator.create(Self);
        errdefer allocator.destroy(job);
        const path_copy = try allocator.dupeZ(u8, path);
        errdefer allocator.free(path_copy);
        const input_copy = try allocator.dupe(u8, input);
        errdefer allocator.free(input_copy);
        const work_memory = try allocator.alignedAlloc(u8, request_alignment, uv.uv_req_size(.work));
        errdefer allocator.free(work_memory);
        const fs_memory = try allocator.alignedAlloc(u8, request_alignment, uv.uv_req_size(.fs));
        job.* = .{ .allocator = allocator, .kind = kind, .path = path_copy, .input = input_copy, .work_memory = work_memory, .fs_memory = fs_memory };
        return job;
    }

    pub fn destroy(self: *Self) void {
        const allocator = self.allocator;
        if (self.stat_numbers) |numbers| {
            allocator.free(numbers);
        }
        if (self.contents) |contents| {
            allocator.free(contents);
        }
        allocator.free(self.fs_memory);
        allocator.free(self.work_memory);
        allocator.free(self.input);
        allocator.free(self.path);
        allocator.destroy(self);
    }

    pub fn runOnPoolThread(self: *Self) void {
        switch (self.kind) {
            .exists => self.checkPathExists(),
            .stat => self.statPath(),
            .read_text => self.readWholeFile(true),
            .read_bytes => self.readWholeFile(false),
            .write => self.writeWholeFile(open_to_replace),
            .append => self.writeWholeFile(open_to_append),
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

    fn readWholeFile(self: *Self, needs_utf8: bool) void {
        const request = self.fsRequest();
        const file = uv.uv_fs_open(self.loop, request, self.path.ptr, open_read_only, 0, null);
        uv.uv_fs_req_cleanup(request);
        if (file < 0) {
            self.failure = failureFromStatus(file);
            return;
        }
        defer self.closeFile(file);

        const contents = self.readUntilEnd(file) orelse return;
        if (needs_utf8 and !std.unicode.utf8ValidateSlice(contents)) {
            self.allocator.free(contents);
            self.failure = .InvalidUtf8;
            return;
        }
        self.contents = contents;
    }

    fn readUntilEnd(self: *Self, file: c_int) ?[]u8 {
        const request = self.fsRequest();
        var data = self.allocator.alloc(u8, self.fileSizeHint(file) + 1) catch return self.failWith(.Other);
        var len: usize = 0;
        while (true) {
            if (len == data.len) {
                data = self.allocator.realloc(data, data.len * 2) catch {
                    self.allocator.free(data);
                    return self.failWith(.Other);
                };
            }
            const free_space = data[len..];
            const buffer = uv.Buffer{ .base = free_space.ptr, .len = @intCast(free_space.len) };
            const status = uv.uv_fs_read(self.loop, request, file, @ptrCast(&buffer), 1, -1, null);
            uv.uv_fs_req_cleanup(request);
            if (status < 0) {
                self.allocator.free(data);
                return self.failWith(failureFromStatus(status));
            }
            if (status == 0) break;
            len += @intCast(status);
        }
        return self.allocator.realloc(data, len) catch {
            self.allocator.free(data);
            return self.failWith(.Other);
        };
    }

    fn writeWholeFile(self: *Self, open_flags: c_int) void {
        const request = self.fsRequest();
        const file = uv.uv_fs_open(self.loop, request, self.path.ptr, open_flags, new_file_mode, null);
        uv.uv_fs_req_cleanup(request);
        if (file < 0) {
            self.failure = failureFromStatus(file);
            return;
        }
        defer self.closeFile(file);

        var written: usize = 0;
        while (written < self.input.len) {
            const remaining = self.input[written..];
            const buffer = uv.Buffer{ .base = remaining.ptr, .len = @intCast(remaining.len) };
            const status = uv.uv_fs_write(self.loop, request, file, @ptrCast(&buffer), 1, -1, null);
            uv.uv_fs_req_cleanup(request);
            if (status < 0) {
                self.failure = failureFromStatus(status);
                return;
            }
            written += @intCast(status);
        }
    }

    fn fileSizeHint(self: *Self, file: c_int) usize {
        const request = self.fsRequest();
        defer uv.uv_fs_req_cleanup(request);
        if (uv.uv_fs_fstat(self.loop, request, file, null) < 0) return min_read_capacity;
        const size: usize = @intCast(uv.uv_fs_get_statbuf(request).st_size);
        return @max(size, min_read_capacity);
    }

    fn failWith(self: *Self, failure: FsError) ?[]u8 {
        self.failure = failure;
        return null;
    }

    fn closeFile(self: *Self, file: c_int) void {
        const request = self.fsRequest();
        _ = uv.uv_fs_close(self.loop, request, file, null);
        uv.uv_fs_req_cleanup(request);
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
            .write, .append => .{ .value = v.NULL_VALUE, .kind = .null },
            .stat => result: {
                const numbers = self.stat_numbers.?;
                self.stat_numbers = null;
                break :result .{ .value = v.NULL_VALUE, .kind = .array, .payload = .{ .ints = numbers } };
            },
            .read_text, .read_bytes => result: {
                const contents = self.contents.?;
                self.contents = null;
                const kind: v.ValueType = if (self.kind == .read_text) .string else .buffer;
                break :result .{ .value = v.NULL_VALUE, .kind = kind, .payload = .{ .bytes = contents } };
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

const WriteMode = struct {
    should_truncate: bool = false,
    should_append: bool = false,
};

fn writeOpenFlags(comptime mode: WriteMode) c_int {
    if (builtin.os.tag == .windows) {
        const write_only = 0x0001;
        const append = 0x0008;
        const create = 0x0100;
        const truncate = 0x0200;
        return write_only | create | (if (mode.should_truncate) truncate else 0) | (if (mode.should_append) append else 0);
    } else {
        const flags: std.posix.O = .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = mode.should_truncate, .APPEND = mode.should_append };
        return @bitCast(@as(u32, @bitCast(flags)));
    }
}

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
