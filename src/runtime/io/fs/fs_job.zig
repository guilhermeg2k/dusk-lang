const std = @import("std");
const builtin = @import("builtin");
const uv = @import("../../tasks/loop/uv.zig");
const v = @import("../../value.zig");
const EventLoop = @import("../../tasks/event_loop.zig").EventLoop;
const Scheduler = @import("../../tasks/scheduler.zig").Scheduler;
const TaskState = @import("../../tasks/scheduler.zig").TaskState;
const WakeResult = @import("../../tasks/scheduler.zig").WakeResult;
const WakePayload = @import("../../tasks/scheduler.zig").WakePayload;
const Stream = @import("../stream/stream.zig").Stream;
const IoError = @import("../stream/stream.zig").IoError;

const request_alignment = std.mem.Alignment.@"16";

const mode_type_mask = 0o170000;
const mode_directory = 0o040000;
const mode_regular_file = 0o100000;
const mode_symlink = 0o120000;
const mode_permission_mask = 0o7777;
const open_read_only = 0;
const open_to_replace = writeOpenFlags(.{ .should_truncate = true });
const open_to_append = writeOpenFlags(.{ .should_append = true });
const new_file_mode = 0o666;
const min_read_capacity = 4096;
const max_initial_read_capacity = 16 * 1024 * 1024;
const new_dir_mode = 0o777;
const dir_batch_size = 64;
const chunk_size = 64 * 1024;

pub const FsJob = struct {
    const Self = @This();

    pub const Kind = enum {
        exists,
        stat,
        read_text,
        read_bytes,
        write,
        append,
        read_dir,
        make_dir,
        make_dir_recursive,
        delete,
        delete_recursive,
        rename,
        read_chunk,
        write_chunk,
        open_file,
        file_size,
    };

    pub const OpenMode = enum(i64) { read, write, append };

    allocator: std.mem.Allocator,
    kind: Kind,
    path: [:0]u8,
    input: [:0]u8,
    work_memory: []align(16) u8,
    fs_memory: []align(16) u8,
    event_loop: *EventLoop = undefined,
    scheduler: *Scheduler = undefined,
    task: ?*TaskState = null,
    stream: ?*Stream = null,
    file: c_int = -1,
    offset: i64 = -1,
    file_size: i64 = 0,
    open_mode: OpenMode = .read,
    loop: *uv.Loop = undefined,
    failure: ?FsError = null,
    is_found: bool = false,
    stat_numbers: ?[]i64 = null,
    contents: ?[]u8 = null,
    output: ?[]u8 = null,
    written_bytes: usize = 0,
    names: ?[][]u8 = null,

    pub fn create(allocator: std.mem.Allocator, kind: Kind, path: []const u8, input: []const u8) !*Self {
        const job = try allocator.create(Self);
        errdefer allocator.destroy(job);
        const path_copy = try allocator.dupeZ(u8, path);
        errdefer allocator.free(path_copy);
        const input_copy = try allocator.dupeZ(u8, input);
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
        if (self.output) |output| {
            allocator.free(output);
        }
        if (self.names) |names| {
            WakePayload.free(.{ .names = names }, allocator);
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
            .read_dir => self.readDir(),
            .make_dir => self.makeDir(),
            .make_dir_recursive => self.makeDirWithParents(),
            .delete => self.deletePath(),
            .delete_recursive => _ = self.deleteTree(self.path, .unknown),
            .rename => self.renamePath(),
            .read_chunk => self.readChunk(),
            .write_chunk => self.writeChunk(),
            .open_file => self.openFile(),
            .file_size => self.readFileSize(),
        }
    }

    fn readFileSize(self: *Self) void {
        const request = self.fsRequest();
        const status = uv.uv_fs_fstat(self.loop, request, self.file, null);
        if (status < 0) {
            uv.uv_fs_req_cleanup(request);
            self.failure = failureFromStatus(status);
            return;
        }
        self.file_size = clampFileSize(uv.uv_fs_get_statbuf(request).st_size);
        uv.uv_fs_req_cleanup(request);
    }

    fn writeChunk(self: *Self) void {
        const output = self.output orelse return;
        var written: usize = 0;
        const request = self.fsRequest();
        while (written < output.len) {
            const chunk_len = @min(output.len - written, 1 << 30);
            const buffer = uv.Buffer{ .base = output.ptr + written, .len = @intCast(chunk_len) };
            const offset: i64 = if (self.offset < 0) -1 else blk: {
                if (written > std.math.maxInt(i64)) {
                    self.written_bytes = written;
                    self.failure = .Other;
                    return;
                }
                break :blk std.math.add(i64, self.offset, @intCast(written)) catch {
                    self.written_bytes = written;
                    self.failure = .Other;
                    return;
                };
            };
            const status = uv.uv_fs_write(self.loop, request, self.file, @ptrCast(&buffer), 1, offset, null);
            uv.uv_fs_req_cleanup(request);
            if (status <= 0) {
                self.written_bytes = written;
                self.failure = if (status < 0) failureFromStatus(status) else .Other;
                return;
            }
            written += @intCast(status);
        }
        self.written_bytes = written;
    }

    fn openFile(self: *Self) void {
        const flags: c_int = switch (self.open_mode) {
            .read => open_read_only,
            .write => open_to_replace,
            .append => open_to_append,
        };
        const request = self.fsRequest();
        const status = uv.uv_fs_open(self.loop, request, self.path.ptr, flags, new_file_mode, null);
        uv.uv_fs_req_cleanup(request);
        if (status < 0) {
            self.failure = failureFromStatus(status);
            return;
        }
        const stat_status = uv.uv_fs_fstat(self.loop, request, status, null);
        const is_directory = stat_status >= 0 and uv.uv_fs_get_statbuf(request).st_mode & mode_type_mask == mode_directory;
        if (stat_status >= 0) {
            self.file_size = clampFileSize(uv.uv_fs_get_statbuf(request).st_size);
        }
        uv.uv_fs_req_cleanup(request);
        if (is_directory) {
            _ = uv.uv_fs_close(self.loop, request, status, null);
            uv.uv_fs_req_cleanup(request);
            self.failure = .IsDirectory;
            return;
        }
        self.file = status;
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

        const numbers = self.allocator.alloc(i64, 8) catch {
            self.failure = .Other;
            return;
        };
        const file_type = info.st_mode & mode_type_mask;
        const is_dir = file_type == mode_directory;
        numbers[0] = if (is_dir) 0 else clampFileSize(info.st_size);
        numbers[1] = @intFromBool(is_dir);
        numbers[2] = @intFromBool(file_type == mode_regular_file);
        numbers[3] = @intFromBool(is_symlink);
        numbers[4] = millisecondsOf(info.st_mtim);
        numbers[5] = millisecondsOf(info.st_birthtim);
        numbers[6] = @intCast(info.st_mode & mode_permission_mask);
        numbers[7] = @intFromBool(info.st_birthtim.tv_sec != 0 or info.st_birthtim.tv_nsec != 0);
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
            const chunk_len = @min(free_space.len, 1 << 30);
            const buffer = uv.Buffer{ .base = free_space.ptr, .len = @intCast(chunk_len) };
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
            const chunk_len = @min(remaining.len, 1 << 30);
            const buffer = uv.Buffer{ .base = remaining.ptr, .len = @intCast(chunk_len) };
            const status = uv.uv_fs_write(self.loop, request, file, @ptrCast(&buffer), 1, -1, null);
            uv.uv_fs_req_cleanup(request);
            if (status <= 0) {
                self.failure = if (status < 0) failureFromStatus(status) else .Other;
                return;
            }
            written += @intCast(status);
        }
    }

    fn readDir(self: *Self) void {
        const entries = self.readDirEntries(self.path) orelse return;
        defer self.allocator.free(entries);
        for (entries) |entry| {
            if (!std.unicode.utf8ValidateSlice(entry.name)) {
                for (entries) |item| {
                    self.allocator.free(item.name);
                }
                self.failure = .InvalidUtf8;
                return;
            }
        }
        const names = self.allocator.alloc([]u8, entries.len) catch {
            for (entries) |entry| {
                self.allocator.free(entry.name);
            }
            self.failure = .Other;
            return;
        };
        for (entries, names) |entry, *name| {
            name.* = entry.name;
        }
        self.names = names;
    }

    fn readDirEntries(self: *Self, path: [*:0]const u8) ?[]DirEntry {
        const request = self.fsRequest();
        const open_status = uv.uv_fs_opendir(self.loop, request, path, null);
        if (open_status < 0) {
            uv.uv_fs_req_cleanup(request);
            self.failure = failureFromStatus(open_status);
            return null;
        }
        const dir: *uv.DirEntries = @ptrCast(@alignCast(uv.uv_fs_get_ptr(request).?));
        uv.uv_fs_req_cleanup(request);
        defer self.closeDir(dir);

        var batch: [dir_batch_size]uv.DirEntry = undefined;
        dir.entries = &batch;
        dir.entry_count = batch.len;
        var entries: std.ArrayList(DirEntry) = .empty;
        while (true) {
            const count = uv.uv_fs_readdir(self.loop, request, dir, null);
            if (count <= 0) {
                uv.uv_fs_req_cleanup(request);
                if (count == 0) break;
                freeDirEntries(self.allocator, &entries);
                self.failure = failureFromStatus(count);
                return null;
            }
            for (batch[0..@intCast(count)]) |found| {
                const name = self.allocator.dupe(u8, std.mem.span(found.name)) catch null;
                const is_added = if (name) |copy| addDirEntry(self.allocator, &entries, copy, found.entry_type) else false;
                if (!is_added) {
                    uv.uv_fs_req_cleanup(request);
                    freeDirEntries(self.allocator, &entries);
                    self.failure = .Other;
                    return null;
                }
            }
            uv.uv_fs_req_cleanup(request);
        }
        return entries.toOwnedSlice(self.allocator) catch {
            freeDirEntries(self.allocator, &entries);
            self.failure = .Other;
            return null;
        };
    }

    fn closeDir(self: *Self, dir: *uv.DirEntries) void {
        const request = self.fsRequest();
        _ = uv.uv_fs_closedir(self.loop, request, dir, null);
        uv.uv_fs_req_cleanup(request);
    }

    fn makeDir(self: *Self) void {
        const status = self.createDir(self.path.ptr);
        if (status < 0) {
            self.failure = failureFromStatus(status);
        }
    }

    fn makeDirWithParents(self: *Self) void {
        const path = self.path;
        for (path, 0..) |char, i| {
            const ends_parent = std.fs.path.isSep(char) and i > 0 and !std.fs.path.isSep(path[i - 1]);
            if (!ends_parent) continue;
            path[i] = 0;
            const is_ready = self.makeDirIfMissing(path.ptr);
            path[i] = char;
            if (!is_ready) return;
        }
        _ = self.makeDirIfMissing(path.ptr);
    }

    fn makeDirIfMissing(self: *Self, path: [*:0]const u8) bool {
        const status = self.createDir(path);
        if (status >= 0) return true;
        if (self.isDirectory(path)) return true;
        const failure = failureFromStatus(status);
        self.failure = if (failure == .AlreadyExists) .NotDirectory else failure;
        return false;
    }

    fn createDir(self: *Self, path: [*:0]const u8) c_int {
        const request = self.fsRequest();
        defer uv.uv_fs_req_cleanup(request);
        return uv.uv_fs_mkdir(self.loop, request, path, new_dir_mode, null);
    }

    fn isDirectory(self: *Self, path: [*:0]const u8) bool {
        const request = self.fsRequest();
        defer uv.uv_fs_req_cleanup(request);
        if (uv.uv_fs_stat(self.loop, request, path, null) < 0) return false;
        return uv.uv_fs_get_statbuf(request).st_mode & mode_type_mask == mode_directory;
    }

    fn deletePath(self: *Self) void {
        const request = self.fsRequest();
        const status = uv.uv_fs_lstat(self.loop, request, self.path.ptr, null);
        const is_dir = status >= 0 and uv.uv_fs_get_statbuf(request).st_mode & mode_type_mask == mode_directory;
        uv.uv_fs_req_cleanup(request);
        if (status < 0) {
            self.failure = failureFromStatus(status);
            return;
        }
        _ = if (is_dir) self.deleteEmptyDir(self.path.ptr) else self.deleteFile(self.path.ptr);
    }

    fn deleteTree(self: *Self, path: [*:0]const u8, entry_type: uv.DirEntryType) bool {
        const is_dir = switch (entry_type) {
            .dir => true,
            .unknown => self.lstatIsDirectory(path) orelse return false,
            else => false,
        };
        if (!is_dir) return self.deleteFile(path);

        const entries = self.readDirEntries(path) orelse return false;
        defer {
            for (entries) |entry| {
                self.allocator.free(entry.name);
            }
            self.allocator.free(entries);
        }
        for (entries) |entry| {
            const child = std.fmt.allocPrintSentinel(self.allocator, "{s}/{s}", .{ std.mem.span(path), entry.name }, 0) catch {
                self.failure = .Other;
                return false;
            };
            defer self.allocator.free(child);
            if (!self.deleteTree(child.ptr, entry.entry_type)) return false;
        }
        return self.deleteEmptyDir(path);
    }

    fn lstatIsDirectory(self: *Self, path: [*:0]const u8) ?bool {
        const request = self.fsRequest();
        defer uv.uv_fs_req_cleanup(request);
        const status = uv.uv_fs_lstat(self.loop, request, path, null);
        if (status < 0) {
            self.failure = failureFromStatus(status);
            return null;
        }
        return uv.uv_fs_get_statbuf(request).st_mode & mode_type_mask == mode_directory;
    }

    fn deleteEmptyDir(self: *Self, path: [*:0]const u8) bool {
        const request = self.fsRequest();
        defer uv.uv_fs_req_cleanup(request);
        const status = uv.uv_fs_rmdir(self.loop, request, path, null);
        if (status >= 0) return true;
        const failure = failureFromStatus(status);
        self.failure = if (failure == .AlreadyExists) .NotEmpty else failure;
        return false;
    }

    fn deleteFile(self: *Self, path: [*:0]const u8) bool {
        const request = self.fsRequest();
        defer uv.uv_fs_req_cleanup(request);
        const status = uv.uv_fs_unlink(self.loop, request, path, null);
        if (status >= 0) return true;
        self.failure = failureFromStatus(status);
        return false;
    }

    fn renamePath(self: *Self) void {
        const request = self.fsRequest();
        defer uv.uv_fs_req_cleanup(request);
        const status = uv.uv_fs_rename(self.loop, request, self.path.ptr, self.input.ptr, null);
        if (status < 0) {
            self.failure = failureFromStatus(status);
        }
    }

    fn readChunk(self: *Self) void {
        const data = self.allocator.alloc(u8, chunk_size) catch {
            self.failure = .Other;
            return;
        };
        const request = self.fsRequest();
        const buffer = uv.Buffer{ .base = data.ptr, .len = @intCast(data.len) };
        const status = uv.uv_fs_read(self.loop, request, self.file, @ptrCast(&buffer), 1, self.offset, null);
        uv.uv_fs_req_cleanup(request);
        if (status < 0) {
            self.allocator.free(data);
            self.failure = failureFromStatus(status);
            return;
        }
        self.contents = self.allocator.realloc(data, @intCast(status)) catch {
            self.allocator.free(data);
            self.failure = .Other;
            return;
        };
    }

    pub fn takeContents(self: *Self) ?[]u8 {
        const contents = self.contents;
        self.contents = null;
        return contents;
    }

    fn fileSizeHint(self: *Self, file: c_int) usize {
        const request = self.fsRequest();
        defer uv.uv_fs_req_cleanup(request);
        if (uv.uv_fs_fstat(self.loop, request, file, null) < 0) return min_read_capacity;
        const clamped = @min(uv.uv_fs_get_statbuf(request).st_size, max_initial_read_capacity);
        const size: usize = @intCast(clamped);
        return @max(size, min_read_capacity);
    }

    fn failWith(self: *Self, failure: FsError) ?[]u8 {
        self.failure = failure;
        return null;
    }

    pub fn closeFile(self: *Self, file: c_int) void {
        const request = self.fsRequest();
        _ = uv.uv_fs_close(self.loop, request, file, null);
        uv.uv_fs_req_cleanup(request);
    }

    fn fsRequest(self: *Self) *uv.Request {
        return @ptrCast(self.fs_memory.ptr);
    }

    pub fn takeWakeResult(self: *Self) WakeResult {
        const is_stream_failure = self.failure != null and self.kind == .file_size;
        if (is_stream_failure) {
            return .{ .value = .{ .int64 = @intFromEnum(IoError.Other) }, .kind = .int64, .has_failed = true, .error_is_relative = true };
        }
        if (self.failure) |failure| {
            return .{ .value = .{ .int64 = @intFromEnum(failure) }, .kind = .int64, .has_failed = true, .error_is_relative = true };
        }
        return switch (self.kind) {
            .exists => .{ .value = .{ .bool = self.is_found }, .kind = .bool },
            .write, .append, .make_dir, .make_dir_recursive, .delete, .delete_recursive, .rename => .{ .value = v.NULL_VALUE, .kind = .null },
            .read_chunk, .write_chunk => @panic("takeWakeResult: chunk jobs go to their stream"),
            .open_file => result: {
                const numbers = self.allocator.alloc(i64, 2) catch {
                    self.closeFile(self.file);
                    break :result .{ .value = .{ .int64 = @intFromEnum(FsError.Other) }, .kind = .int64, .has_failed = true, .error_is_relative = true };
                };
                numbers[0] = self.file;
                numbers[1] = self.file_size;
                break :result .{ .value = v.NULL_VALUE, .kind = .array, .payload = .{ .ints = numbers } };
            },
            .file_size => .{ .value = .{ .int64 = self.file_size }, .kind = .int64 },
            .read_dir => result: {
                const names = self.names.?;
                self.names = null;
                break :result .{ .value = v.NULL_VALUE, .kind = .array, .payload = .{ .names = names } };
            },
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

const DirEntry = struct {
    name: []u8,
    entry_type: uv.DirEntryType,
};

fn addDirEntry(allocator: std.mem.Allocator, entries: *std.ArrayList(DirEntry), name: []u8, entry_type: uv.DirEntryType) bool {
    entries.append(allocator, .{ .name = name, .entry_type = entry_type }) catch {
        allocator.free(name);
        return false;
    };
    return true;
}

fn freeDirEntries(allocator: std.mem.Allocator, entries: *std.ArrayList(DirEntry)) void {
    for (entries.items) |entry| {
        allocator.free(entry.name);
    }
    entries.deinit(allocator);
}

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

fn clampFileSize(size: u64) i64 {
    return @intCast(@min(size, @as(u64, std.math.maxInt(i64))));
}

fn millisecondsOf(time: uv.Timespec) i64 {
    const sec: i64 = @intCast(time.tv_sec);
    const nsec_ms: i64 = @divTrunc(@as(i64, @intCast(time.tv_nsec)), 1_000_000);
    return (sec *| 1000) +| nsec_ms;
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
