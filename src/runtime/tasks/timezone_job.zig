const std = @import("std");
const builtin = @import("builtin");
const bundle = @import("timezone_data");
const uv = @import("uv.zig");
const EventLoop = @import("event_loop.zig").EventLoop;
const Scheduler = @import("scheduler.zig").Scheduler;
const TaskState = @import("scheduler.zig").TaskState;
const WakeResult = @import("scheduler.zig").WakeResult;
const timezone = @import("../timezone.zig");
const Recurrence = @import("../timezone_recurrence.zig").Recurrence;
const TimeError = @import("../time_error.zig").TimeError;
const maximum_file_size = 4 * 1024 * 1024;

pub const LocalJob = struct {
    allocator: std.mem.Allocator,
    loop: ?*uv.Loop,
    event_loop: *EventLoop,
    scheduler: ?*Scheduler = null,
    task: ?*TaskState = null,
    work_memory: []align(16) u8,
    fs_memory: []align(16) u8,
    timezone_override: ?[]u8,
    system_path: [:0]u8,
    result: ?timezone.LocalResult = null,

    fn create(event_loop: *EventLoop, timezone_override: ?[]const u8, system_path: []const u8) !*LocalJob {
        const allocator = event_loop.allocator;
        const job = try allocator.create(LocalJob);
        errdefer allocator.destroy(job);
        const work_memory = try allocator.alignedAlloc(u8, .@"16", uv.uv_req_size(.work));
        errdefer allocator.free(work_memory);
        const fs_memory = try allocator.alignedAlloc(u8, .@"16", uv.uv_req_size(.fs));
        errdefer allocator.free(fs_memory);
        const override_copy = if (timezone_override) |text| try allocator.dupe(u8, text) else null;
        errdefer if (override_copy) |text| {
            allocator.free(text);
        };
        const path_copy = try allocator.dupeZ(u8, system_path);
        job.* = .{ .allocator = allocator, .loop = event_loop.loop, .event_loop = event_loop, .work_memory = work_memory, .fs_memory = fs_memory, .timezone_override = override_copy, .system_path = path_copy };
        return job;
    }

    fn destroy(self: *LocalJob) void {
        const allocator = self.allocator;
        if (self.result) |result| {
            result.free(allocator);
        }
        if (self.timezone_override) |text| {
            allocator.free(text);
        }
        allocator.free(self.system_path);
        allocator.free(self.fs_memory);
        allocator.free(self.work_memory);
        allocator.destroy(self);
    }

    pub fn start(event_loop: *EventLoop, scheduler: *Scheduler, task: *TaskState, environment: ?*const std.process.Environ.Map) !void {
        var buffer: [4096]u8 = undefined;
        const timezone_override = if (environment) |env| env.get("TZ") else try readEnvironment(&buffer);
        _ = try event_loop.ensureLoopStarted();
        const job = try create(event_loop, timezone_override, "/etc/localtime");
        errdefer job.destroy();
        job.scheduler = scheduler;
        job.task = task;
        const request = job.workRequest();
        uv.uv_req_set_data(request, job);
        if (uv.uv_queue_work(job.loop.?, request, onLocalWork, onLocalDone) != 0) {
            return error.LocalTimeUnavailable;
        }
        event_loop.pending_operations += 1;
    }

    fn readEnvironment(buffer: *[4096]u8) !?[]const u8 {
        var length: usize = buffer.len;
        const status = uv.uv_os_getenv("TZ", buffer, &length);
        if (status == 0) {
            return buffer[0..length];
        }
        const failure = std.mem.span(uv.uv_err_name(status));
        if (std.mem.eql(u8, failure, "ENOENT")) {
            return null;
        }
        if (std.mem.eql(u8, failure, "ENOMEM")) {
            return error.OutOfMemory;
        }
        return error.LocalTimeUnavailable;
    }

    fn workRequest(self: *LocalJob) *uv.Request {
        return @ptrCast(self.work_memory.ptr);
    }

    fn discoverOnPoolThread(self: *LocalJob) void {
        const zone = self.discover() catch |failure| {
            if (failure == error.OutOfMemory) {
                self.result = .out_of_memory;
            }
            return;
        };
        self.result = .{ .zone = zone };
    }

    fn discover(self: *LocalJob) !timezone.LocalZone {
        if (self.timezone_override) |text| {
            return self.discoverOverride(text);
        }
        if (builtin.os.tag == .windows) {
            var name: [512]u8 = undefined;
            if (dusk_local_windows_timezone(&name, name.len) == 0) {
                return error.LocalTimeUnavailable;
            }
            const end = std.mem.indexOfScalar(u8, &name, 0) orelse return error.LocalTimeUnavailable;
            return .{ .named = try windowsZoneIndex(self.allocator, name[0..end]) };
        }
        return self.discoverUnixSystem();
    }

    fn discoverOverride(self: *LocalJob, text: []const u8) !timezone.LocalZone {
        if (text.len == 0) {
            return .utc;
        }
        const has_colon = text[0] == ':';
        const zone = if (has_colon) text[1..] else text;
        if (timezone.Registry.findZone(zone)) |index| {
            return .{ .named = index };
        }
        if (std.fs.path.isAbsolute(zone)) {
            return self.readSnapshot(zone);
        }
        if (has_colon) {
            return error.LocalTimeUnavailable;
        }
        const recurrence = Recurrence.parse(zone) catch return error.LocalTimeUnavailable;
        if (!timezone.canRepresentLocalRecurrence(recurrence)) {
            return error.LocalTimeUnavailable;
        }
        return .{ .recurrence = recurrence };
    }

    fn readSnapshot(self: *LocalJob, path: []const u8) !timezone.LocalZone {
        if (std.mem.indexOfScalar(u8, path, 0) != null) {
            return error.LocalTimeUnavailable;
        }
        const allocator = self.allocator;
        const terminated = try allocator.dupeZ(u8, path);
        defer allocator.free(terminated);
        const request: *uv.Request = @ptrCast(self.fs_memory.ptr);
        const loop = self.loop.?;
        const flags: c_int = if (builtin.os.tag == .windows) 0 else @bitCast(@as(u32, @bitCast(@as(std.posix.O, .{ .ACCMODE = .RDONLY, .NONBLOCK = true }))));
        const file = uv.uv_fs_open(loop, request, terminated, flags, 0, null);
        uv.uv_fs_req_cleanup(request);
        if (file < 0) {
            return statusFailure(file);
        }
        defer {
            _ = uv.uv_fs_close(loop, request, file, null);
            uv.uv_fs_req_cleanup(request);
        }
        const stat_status = uv.uv_fs_fstat(loop, request, file, null);
        if (stat_status < 0) {
            uv.uv_fs_req_cleanup(request);
            return statusFailure(stat_status);
        }
        const stat = uv.uv_fs_get_statbuf(request).*;
        uv.uv_fs_req_cleanup(request);
        const is_regular_file = stat.st_mode & 0o170000 == 0o100000;
        const has_supported_size = stat.st_size >= 44 and stat.st_size <= maximum_file_size;
        const can_read_snapshot = is_regular_file and has_supported_size;
        if (!can_read_snapshot) {
            return error.LocalTimeUnavailable;
        }
        var bytes = try allocator.alloc(u8, @max(4096, @as(usize, @intCast(stat.st_size)) + 1));
        errdefer allocator.free(bytes);
        var length: usize = 0;
        while (true) {
            if (length == bytes.len) {
                const capacity = @min(bytes.len * 2, maximum_file_size + 1);
                if (capacity <= length) {
                    return error.LocalTimeUnavailable;
                }
                bytes = try allocator.realloc(bytes, capacity);
            }
            const buffer = uv.Buffer{ .base = bytes.ptr + length, .len = @intCast(bytes.len - length) };
            const status = uv.uv_fs_read(loop, request, file, @ptrCast(&buffer), 1, @intCast(length), null);
            uv.uv_fs_req_cleanup(request);
            if (status < 0) {
                return statusFailure(status);
            }
            if (status == 0) break;
            length += @intCast(status);
            if (length > maximum_file_size) {
                return error.LocalTimeUnavailable;
            }
        }
        bytes = try allocator.realloc(bytes, length);
        const rules = timezone.Rules.parse(bytes) catch return error.LocalTimeUnavailable;
        if (!rules.canRepresentLocal()) {
            return error.LocalTimeUnavailable;
        }
        return .{ .file = .{ .bytes = bytes, .rules = rules } };
    }

    fn discoverUnixSystem(self: *LocalJob) !timezone.LocalZone {
        const request: *uv.Request = @ptrCast(self.fs_memory.ptr);
        const status = uv.uv_fs_readlink(self.loop.?, request, self.system_path, null);
        if (status >= 0) {
            defer uv.uv_fs_req_cleanup(request);
            const pointer: [*:0]const u8 = @ptrCast(uv.uv_fs_get_ptr(request) orelse return error.LocalTimeUnavailable);
            const target = std.mem.span(pointer);
            const parent = std.fs.path.dirname(self.system_path) orelse ".";
            const resolved = try std.fs.path.resolve(self.allocator, &.{ parent, target });
            defer self.allocator.free(resolved);
            if (systemLinkZoneIndex(resolved)) |index| {
                return .{ .named = index };
            }
        } else {
            uv.uv_fs_req_cleanup(request);
            if (std.mem.eql(u8, std.mem.span(uv.uv_err_name(status)), "ENOMEM")) {
                return error.OutOfMemory;
            }
        }
        return self.readSnapshot(self.system_path);
    }

    fn takeWakeResult(self: *LocalJob) WakeResult {
        const result = self.result orelse return .{ .value = .{ .int64 = @intFromEnum(TimeError.LocalTimeUnavailable) }, .kind = .int64, .has_failed = true, .error_is_relative = true };
        self.result = null;
        return .{ .value = .{ .int64 = 0 }, .kind = .int64, .payload = .{ .local_timezone = result } };
    }
};

fn statusFailure(status: c_int) anyerror {
    const name = std.mem.span(uv.uv_err_name(status));
    return if (std.mem.eql(u8, name, "ENOMEM")) error.OutOfMemory else error.LocalTimeUnavailable;
}

pub fn windowsZoneIndex(allocator: std.mem.Allocator, name: []const u8) !usize {
    const Manifest = struct { windows: std.json.ArrayHashMap([]const u8) };
    const parsed = try std.json.parseFromSlice(Manifest, allocator, bundle.manifest, .{ .ignore_unknown_fields = true });
    defer parsed.deinit();
    const zone = parsed.value.windows.map.get(name) orelse return error.LocalTimeUnavailable;
    return timezone.Registry.findZone(zone) orelse error.LocalTimeUnavailable;
}

fn systemLinkZoneIndex(path: []const u8) ?usize {
    for ([_][]const u8{ "/usr/share/zoneinfo/", "/usr/lib/zoneinfo/", "/var/db/timezone/zoneinfo/" }) |prefix| {
        if (std.mem.startsWith(u8, path, prefix)) {
            return timezone.Registry.findZone(path[prefix.len..]);
        }
    }
    return null;
}

extern fn dusk_local_windows_timezone(name: [*]u8, capacity: usize) c_int;

fn localJobOf(request: *uv.Request) *LocalJob {
    return @ptrCast(@alignCast(uv.uv_req_get_data(request).?));
}

fn onLocalWork(request: *uv.Request) callconv(.c) void {
    localJobOf(request).discoverOnPoolThread();
}

fn onLocalDone(request: *uv.Request, status: c_int) callconv(.c) void {
    const job = localJobOf(request);
    defer job.destroy();
    const event_loop = job.event_loop;
    event_loop.pending_operations -= 1;
    if (event_loop.is_shutting_down) return;
    if (status != 0) {
        if (job.result) |result| {
            result.free(event_loop.allocator);
        }
        job.result = null;
    }
    const wake_result = job.takeWakeResult();
    job.scheduler.?.wake(job.task.?, wake_result) catch |failure| {
        wake_result.payload.free(event_loop.allocator);
        event_loop.callback_failure = failure;
    };
}

test "local override forms are authoritative and Windows mappings are pinned" {
    var event_loop: EventLoop = .{ .allocator = std.testing.allocator };
    defer event_loop.deinit();
    const Case = struct { text: []const u8, name: []const u8, winter: i32, summer: i32 };
    for ([_]Case{
        .{ .text = "", .name = "UTC", .winter = 0, .summer = 0 },
        .{ .text = "UTC", .name = "UTC", .winter = 0, .summer = 0 },
        .{ .text = ":US/Eastern", .name = "US/Eastern", .winter = -18000, .summer = -14400 },
        .{ .text = "America/New_York", .name = "America/New_York", .winter = -18000, .summer = -14400 },
        .{ .text = "<+0530>-5:30", .name = "Local", .winter = 19800, .summer = 19800 },
        .{ .text = "STD0DST,M3.2.0,M11.1.0", .name = "Local", .winter = 0, .summer = 3600 },
    }) |case| {
        const job = try LocalJob.create(&event_loop, case.text, "/missing-localtime");
        defer job.destroy();
        var snapshot = try job.discover();
        defer snapshot.free(std.testing.allocator);
        var registry: timezone.Registry = .{};
        defer registry.deinit(std.testing.allocator);
        const identifier = try registry.installLocal(std.testing.allocator, &snapshot);
        var name_buffer: [32]u8 = undefined;
        try std.testing.expectEqualStrings(case.name, registry.zoneName(identifier, &name_buffer));
        try std.testing.expectEqual(case.winter, registry.offsetSeconds(identifier, 1705276800000));
        try std.testing.expectEqual(case.summer, registry.offsetSeconds(identifier, 1721001600000));
    }
    for ([_][]const u8{ "NoSuch/Zone", ":", ":UTC0", ":STD0DST,M3.2.0,M11.1.0", "../UTC", "UTC24", "STD0DST", "UTC0trailing", "America//New_York", "UTC\x00" }) |text| {
        const job = try LocalJob.create(&event_loop, text, "/etc/localtime");
        defer job.destroy();
        try std.testing.expectError(error.LocalTimeUnavailable, job.discover());
    }
    try std.testing.expectEqual(timezone.Registry.findZone("America/Los_Angeles").?, try windowsZoneIndex(std.testing.allocator, "Pacific Standard Time"));
    try std.testing.expectEqual(timezone.Registry.findZone("Etc/UTC").?, try windowsZoneIndex(std.testing.allocator, "UTC"));
    try std.testing.expectError(error.LocalTimeUnavailable, windowsZoneIndex(std.testing.allocator, "Missing Windows Zone"));
    try std.testing.expectEqual(timezone.Registry.findZone("Europe/Paris"), systemLinkZoneIndex("/usr/share/zoneinfo/Europe/Paris"));
    try std.testing.expectEqual(@as(?usize, null), systemLinkZoneIndex("/tmp/zoneinfo/Europe/Paris"));
    try std.testing.expectEqual(@as(?usize, null), systemLinkZoneIndex("/usr/share/zoneinfo/right/UTC"));
}

test "local discovery job allocation failures release native memory" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, createTestJob, .{});
}

fn createTestJob(allocator: std.mem.Allocator) !void {
    var event_loop: EventLoop = .{ .allocator = allocator };
    defer event_loop.deinit();
    const job = try LocalJob.create(&event_loop, "America/New_York", "/etc/localtime");
    defer job.destroy();
    job.discoverOnPoolThread();
    const result = job.result orelse return error.TestUnexpectedResult;
    if (result == .out_of_memory) {
        return error.OutOfMemory;
    }
}

test "Unix local discovery uses safe link names or bounded owned file snapshots" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;
    var temporary = std.testing.tmpDir(.{});
    defer temporary.cleanup();
    const header = "TZif" ++ "\x00" ** 16 ++ "\x00" ** 16 ++ "\x00\x00\x00\x01\x00\x00\x00\x06";
    const payload = header ++ "\x00\x00\x4d\x58\x00\x00LOCAL\x00";
    try temporary.dir.writeFile(std.testing.io, .{ .sub_path = "zone", .data = payload });
    const path = try temporary.dir.realPathFileAlloc(std.testing.io, "zone", std.testing.allocator);
    defer std.testing.allocator.free(path);
    var event_loop: EventLoop = .{ .allocator = std.testing.allocator };
    defer event_loop.deinit();
    _ = try event_loop.ensureLoopStarted();
    const job = try LocalJob.create(&event_loop, path, "/missing-localtime");
    defer job.destroy();
    var snapshot = try job.discover();
    defer snapshot.free(std.testing.allocator);
    try std.testing.expect(snapshot == .file);
    var registry: timezone.Registry = .{};
    defer registry.deinit(std.testing.allocator);
    const identifier = try registry.installLocal(std.testing.allocator, &snapshot);
    try temporary.dir.writeFile(std.testing.io, .{ .sub_path = "zone", .data = "bad" });
    try std.testing.expectEqual(@as(i32, 19800), registry.offsetSeconds(identifier, std.math.minInt(i64)));
    try std.testing.expectEqual(@as(i32, 19800), registry.offsetSeconds(identifier, std.math.maxInt(i64)));
    try std.testing.expectError(error.LocalTimeUnavailable, job.discover());
    try temporary.dir.writeFile(std.testing.io, .{ .sub_path = "zone", .data = payload });
    var retry = try job.discover();
    defer retry.free(std.testing.allocator);
    try std.testing.expect(retry == .file);
    const system_job = try LocalJob.create(&event_loop, null, path);
    defer system_job.destroy();
    var system_snapshot = try system_job.discover();
    defer system_snapshot.free(std.testing.allocator);
    try std.testing.expect(system_snapshot == .file);
    try temporary.dir.symLink(std.testing.io, "/usr/share/zoneinfo/Europe/Paris", "localtime", .{});
    const directory = try temporary.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
    defer std.testing.allocator.free(directory);
    const link_path = try std.fs.path.join(std.testing.allocator, &.{ directory, "localtime" });
    defer std.testing.allocator.free(link_path);
    const link_job = try LocalJob.create(&event_loop, null, link_path);
    defer link_job.destroy();
    const link_snapshot = try link_job.discover();
    try std.testing.expectEqual(timezone.Registry.findZone("Europe/Paris").?, link_snapshot.named);
    try std.testing.checkAllAllocationFailures(std.testing.allocator, readTestSnapshot, .{path});
}

fn readTestSnapshot(allocator: std.mem.Allocator, path: []const u8) !void {
    var event_loop: EventLoop = .{ .allocator = allocator };
    defer event_loop.deinit();
    _ = try event_loop.ensureLoopStarted();
    const job = try LocalJob.create(&event_loop, path, "/missing-localtime");
    defer job.destroy();
    job.discoverOnPoolThread();
    const result = job.result orelse return error.TestUnexpectedResult;
    if (result == .out_of_memory) {
        return error.OutOfMemory;
    }
}

test "local completion releases owned snapshots during shutdown and failed wake" {
    const entry = bundle.zones[timezone.Registry.findZone("America/New_York").?];
    const source = bundle.bytes[entry.offset..][0..entry.length];
    var event_loop: EventLoop = .{ .allocator = std.testing.allocator };
    defer event_loop.deinit();
    const job = try LocalJob.create(&event_loop, null, "/etc/localtime");
    const bytes = try std.testing.allocator.dupe(u8, source);
    job.result = .{ .zone = .{ .file = .{ .bytes = bytes, .rules = try timezone.Rules.parse(bytes) } } };
    uv.uv_req_set_data(job.workRequest(), job);
    event_loop.pending_operations = 1;
    event_loop.is_shutting_down = true;
    onLocalDone(job.workRequest(), 0);
    try std.testing.expectEqual(@as(usize, 0), event_loop.pending_operations);
    event_loop.is_shutting_down = false;
    const failed_job = try LocalJob.create(&event_loop, null, "/etc/localtime");
    const other = try std.testing.allocator.dupe(u8, source);
    failed_job.result = .{ .zone = .{ .file = .{ .bytes = other, .rules = try timezone.Rules.parse(other) } } };
    var failing_allocator = std.testing.FailingAllocator.init(std.testing.allocator, .{ .fail_index = 0 });
    var scheduler: Scheduler = .{ .allocator = failing_allocator.allocator() };
    defer scheduler.deinit();
    var task: TaskState = undefined;
    task.wake_result = null;
    failed_job.task = &task;
    failed_job.scheduler = &scheduler;
    uv.uv_req_set_data(failed_job.workRequest(), failed_job);
    event_loop.pending_operations = 1;
    onLocalDone(failed_job.workRequest(), 0);
    try std.testing.expectEqual(@as(usize, 0), event_loop.pending_operations);
    try std.testing.expectEqual(@as(?anyerror, error.OutOfMemory), event_loop.callback_failure);
    try std.testing.expect(task.wake_result == null);
}
