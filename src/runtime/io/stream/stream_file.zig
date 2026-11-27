const std = @import("std");
const uv = @import("../../tasks/loop/uv.zig");
const TaskState = @import("../../tasks/scheduler.zig").TaskState;
const WakeResult = @import("../../tasks/scheduler.zig").WakeResult;
const FsJob = @import("../fs/fs_job.zig").FsJob;
const Stream = @import("stream.zig").Stream;
const WriteOutcome = @import("stream.zig").WriteOutcome;
const handle_alignment = @import("stream.zig").handle_alignment;
const max_uv_chunk = @import("stream.zig").max_uv_chunk;
const stream_read = @import("stream_read.zig");
const IoError = stream_read.IoError;

const file_output_limit = 64 * 1024;

pub const OutputWaiter = struct {
    task: *TaskState,
    is_flush: bool,
};

pub fn startChunk(stream: *Stream) !void {
    if (stream.has_chunk_in_flight) return;
    const job = try FsJob.create(stream.allocator, .read_chunk, "", "");
    errdefer job.destroy();
    job.file = stream.file;
    job.offset = if (stream.owns_file) stream.file_offset else -1;
    job.stream = stream;
    try stream.event_loop.startFsJob(stream.scheduler, null, job);
    stream.has_chunk_in_flight = true;
}

pub fn finishChunk(stream: *Stream, contents: ?[]u8, has_failed: bool) void {
    stream.has_chunk_in_flight = false;
    const allocator = stream.allocator;
    defer {
        if (contents) |data| {
            allocator.free(data);
        }
    }
    if (stream.is_closed) {
        stream.releaseIfDone();
        return;
    }
    if (has_failed) {
        stream.failure = .Other;
    } else if (contents) |data| {
        if (data.len == 0) {
            stream.has_ended = true;
        } else {
            const received: i64 = @intCast(@min(data.len, @as(usize, std.math.maxInt(i64))));
            stream.file_offset = stream.file_offset +| received;
            stream.buffered.appendSlice(stream.allocator, data) catch {
                stream.failure = .Other;
            };
        }
    }
    stream.serveWaiter();
    const needs_more = stream.waiter != null and !stream.has_ended and stream.failure == null;
    if (needs_more) {
        startChunk(stream) catch {
            stream.failure = .Other;
            stream.serveWaiter();
        };
    }
}

pub fn write(stream: *Stream, task: *TaskState, data: []const u8) !WriteOutcome {
    if (!stream.can_write_file) {
        return .{ .failure = .Other };
    }
    if (takeOutputFailure(stream)) |failure| {
        return .{ .failure = failure };
    }
    if (!stream.is_append) {
        const total_pending = std.math.add(usize, stream.pending_output.items.len, data.len) catch return .{ .failure = .Other };
        if (total_pending > std.math.maxInt(i64)) {
            return .{ .failure = .Other };
        }
        _ = std.math.add(i64, stream.file_offset, @intCast(total_pending)) catch return .{ .failure = .Other };
    }
    try stream.pending_output.appendSlice(stream.allocator, data);
    if (stream.pending_output.items.len < file_output_limit) {
        return .done;
    }
    try startOutputJob(stream);
    if (stream.pending_output.items.len < file_output_limit) {
        return .done;
    }
    try stream.output_waiters.append(stream.allocator, .{ .task = task, .is_flush = false });
    stream.event_loop.pending_operations += 1;
    return .parked;
}

pub fn flush(stream: *Stream, task: *TaskState) !WriteOutcome {
    if (stream.kind != .file) {
        return .done;
    }
    if (takeOutputFailure(stream)) |failure| {
        return .{ .failure = failure };
    }
    const has_output = stream.pending_output.items.len > 0 or stream.has_output_in_flight;
    if (!has_output) {
        return .done;
    }
    try startOutputJob(stream);
    try stream.output_waiters.append(stream.allocator, .{ .task = task, .is_flush = true });
    stream.event_loop.pending_operations += 1;
    return .parked;
}

pub fn filePosition(stream: *const Stream) i64 {
    if (stream.can_write_file) {
        const pending: i64 = @intCast(@min(stream.pending_output.items.len, @as(usize, std.math.maxInt(i64))));
        return stream.file_offset +| pending;
    }
    const unread: i64 = @intCast(@min(stream.buffered.items.len - stream.read_offset, @as(usize, std.math.maxInt(i64))));
    return stream.file_offset -| unread;
}

pub fn seekFile(stream: *Stream, target: i64) ?IoError {
    const can_seek = stream.kind == .file and stream.owns_file and !stream.is_append and target >= 0;
    if (!can_seek) {
        return .Other;
    }
    if (stream.waiter != null or stream.has_chunk_in_flight) {
        return .Busy;
    }
    const has_output = stream.pending_output.items.len > 0 or stream.has_output_in_flight;
    if (has_output) {
        return .Other;
    }
    stream.file_offset = target;
    stream.buffered.clearRetainingCapacity();
    stream.read_offset = 0;
    stream.has_ended = false;
    return null;
}

pub fn flushOutputBeforeClose(stream: *Stream) void {
    startOutputJob(stream) catch writeOutputNow(stream);
}

pub fn finishOutput(stream: *Stream, written_bytes: usize, has_failed: bool) void {
    stream.has_output_in_flight = false;
    const written: i64 = @intCast(@min(written_bytes, @as(usize, std.math.maxInt(i64))));
    stream.file_offset = stream.file_offset +| written;
    if (has_failed) {
        stream.output_failure = .Other;
    }
    if (stream.is_closed) {
        failOutputWaiters(stream, .Closed);
        const has_remaining_output = stream.output_failure == null and stream.pending_output.items.len > 0;
        if (has_remaining_output) {
            if (stream.event_loop.is_shutting_down) {
                writeOutputNow(stream);
            } else {
                startOutputJob(stream) catch writeOutputNow(stream);
            }
        }
        stream.releaseIfDone();
        return;
    }
    if (stream.output_failure == null) {
        startOutputJob(stream) catch {
            stream.output_failure = .Other;
        };
    }
    wakeOutputWaiters(stream);
}

pub fn failOutputWaiters(stream: *Stream, failure: IoError) void {
    for (stream.output_waiters.items) |waiter| {
        stream.event_loop.pending_operations -= 1;
        stream.wakeTask(waiter.task, stream_read.failureResult(failure));
    }
    stream.output_waiters.clearRetainingCapacity();
}

pub fn writeOutputNow(stream: *Stream) void {
    const has_output = stream.pending_output.items.len > 0 and stream.output_failure == null and !stream.has_output_in_flight;
    if (!has_output) return;
    const request_memory = stream.allocator.alignedAlloc(u8, handle_alignment, uv.uv_req_size(.fs)) catch return;
    defer stream.allocator.free(request_memory);
    const request: *uv.Request = @ptrCast(request_memory.ptr);
    var written: usize = 0;
    const output = stream.pending_output.items;
    while (written < output.len) {
        const chunk_len = @min(output.len - written, max_uv_chunk);
        const buffer = uv.Buffer{ .base = output.ptr + written, .len = @intCast(chunk_len) };
        const offset: i64 = if (stream.is_append) -1 else blk: {
            if (written > std.math.maxInt(i64)) break;
            break :blk std.math.add(i64, stream.file_offset, @intCast(written)) catch break;
        };
        const status = uv.uv_fs_write(stream.loop, request, stream.file, @ptrCast(&buffer), 1, offset, null);
        uv.uv_fs_req_cleanup(request);
        if (status <= 0) break;
        written += @intCast(status);
    }
    const written_i64: i64 = @intCast(@min(written, @as(usize, std.math.maxInt(i64))));
    stream.file_offset = stream.file_offset +| written_i64;
    stream.pending_output.clearRetainingCapacity();
}

pub fn closeFileNow(allocator: std.mem.Allocator, loop: *uv.Loop, file: c_int) void {
    const request_memory = allocator.alignedAlloc(u8, handle_alignment, uv.uv_req_size(.fs)) catch return;
    defer allocator.free(request_memory);
    const request: *uv.Request = @ptrCast(request_memory.ptr);
    _ = uv.uv_fs_close(loop, request, file, null);
    uv.uv_fs_req_cleanup(request);
}

fn takeOutputFailure(stream: *Stream) ?IoError {
    const failure = stream.output_failure orelse return null;
    stream.output_failure = null;
    return failure;
}

fn startOutputJob(stream: *Stream) !void {
    const can_start = !stream.has_output_in_flight and stream.pending_output.items.len > 0;
    if (!can_start) return;
    const job = try FsJob.create(stream.allocator, .write_chunk, "", "");
    errdefer job.destroy();
    const owned_output = try stream.pending_output.toOwnedSlice(stream.allocator);
    job.output = owned_output;
    job.file = stream.file;
    job.offset = if (stream.is_append) -1 else stream.file_offset;
    job.stream = stream;
    stream.event_loop.startFsJob(stream.scheduler, null, job) catch |failure| {
        stream.pending_output = std.ArrayList(u8).fromOwnedSlice(owned_output);
        job.output = null;
        return failure;
    };
    stream.has_output_in_flight = true;
}

fn wakeOutputWaiters(stream: *Stream) void {
    const is_drained = !stream.has_output_in_flight and stream.pending_output.items.len == 0;
    const failure = stream.output_failure;
    var kept: usize = 0;
    var has_woken = false;
    for (stream.output_waiters.items) |waiter| {
        const has_room = stream.pending_output.items.len < file_output_limit;
        const can_continue = failure != null or (if (waiter.is_flush) is_drained else has_room);
        if (!can_continue) {
            stream.output_waiters.items[kept] = waiter;
            kept += 1;
            continue;
        }
        stream.event_loop.pending_operations -= 1;
        const wake_result: WakeResult = if (failure) |found| stream_read.failureResult(found) else stream_read.doneResult();
        stream.wakeTask(waiter.task, wake_result);
        has_woken = true;
    }
    stream.output_waiters.shrinkRetainingCapacity(kept);
    if (has_woken) {
        stream.output_failure = null;
    }
}
