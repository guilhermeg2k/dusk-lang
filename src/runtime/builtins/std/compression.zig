const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const native = @import("../native.zig");
const compression = @import("../../io/compression/compression.zig");
const CompressionJob = @import("../../io/compression/compression_job.zig").CompressionJob;
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const ok = native.ok;
const fail = native.fail;
const bytesArg = native.bytesArg;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "own", Binding{ .native = &native.ownResource } },
    .{ "release", Binding{ .native = &native.releaseResource } },
    .{ "compress", Binding{ .native = &compress } },
    .{ "decompress", Binding{ .native = &decompress } },
    .{ "compressor_new", Binding{ .native = &compressorNew } },
    .{ "decompressor_new", Binding{ .native = &decompressorNew } },
    .{ "compress_part", Binding{ .native = &compressPart } },
    .{ "decompress_part", Binding{ .native = &decompressPart } },
    .{ "decompress_finish", Binding{ .native = &decompressFinish } },
    .{ "is_stream_at_end", Binding{ .native = &isStreamAtEnd } },
    .{ "close_stream", Binding{ .native = &closeStream } },
});

pub const CompressionError = compression.CompressionError;
pub const error_sets = .{CompressionError};

const pool_threshold = 256 * 1024;

fn compress(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const level = args[2].int64;
    if (!isValidLevel(level)) {
        return fail(CompressionError.InvalidLevel);
    }
    const format: compression.Format = @enumFromInt(args[1].int64);
    const input = bytesArg(args, 0);
    if (input.len > pool_threshold) {
        return startJob(vm, .{ .kind = .compress, .format = format, .level = @intCast(level) }, input);
    }
    const allocator = std.heap.smp_allocator;
    const output = try compression.compressAll(allocator, format, @intCast(level), input);
    defer allocator.free(output);
    return ok(try vm.allocBytesValue(output, .buffer));
}

fn decompress(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const format: compression.Format = @enumFromInt(args[1].int64);
    const input = bytesArg(args, 0);
    if (input.len > pool_threshold) {
        return startJob(vm, .{ .kind = .decompress, .format = format, .max = maxArg(args[2]) }, input);
    }
    const allocator = std.heap.smp_allocator;
    const output = compression.decompressAll(allocator, format, input, maxArg(args[2])) catch |err| switch (err) {
        error.InvalidData => return fail(CompressionError.InvalidData),
        error.TooLarge => return fail(CompressionError.TooLarge),
        error.OutOfMemory => return err,
    };
    defer allocator.free(output);
    return ok(try vm.allocBytesValue(output, .buffer));
}

fn isValidLevel(level: i64) bool {
    return level >= 0 and level <= compression.max_level;
}

fn maxArg(value: v.Value) ?usize {
    return if (value.int64 < 0) null else @intCast(value.int64);
}

fn startJob(vm: *VM, settings: CompressionJob.Settings, input: []const u8) anyerror!NativeResult {
    const current = vm.running_task.?;
    try vm.event_loop.startCompressionJob(&vm.scheduler, current, settings, input);
    current.park();
    return native.parked();
}

fn compressorNew(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const level = args[1].int64;
    if (!isValidLevel(level)) {
        return fail(CompressionError.InvalidLevel);
    }
    const format: compression.Format = @enumFromInt(args[0].int64);
    const zip = compression.dusk_zip_compressor_new(format, @intCast(level)) orelse return error.OutOfMemory;
    errdefer compression.dusk_zip_free(zip);
    const handle = try vm.event_loop.compression_streams.add(vm.event_loop.allocator, zip, format, null);
    return ok(.{ .int64 = handle });
}

fn decompressorNew(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const format: compression.Format = @enumFromInt(args[0].int64);
    const zip = compression.dusk_zip_decompressor_new(format) orelse return error.OutOfMemory;
    errdefer compression.dusk_zip_free(zip);
    const handle = try vm.event_loop.compression_streams.add(vm.event_loop.allocator, zip, format, maxArg(args[1]));
    return ok(.{ .int64 = handle });
}

fn compressPart(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const streams = &vm.event_loop.compression_streams;
    const handle = args[0].int64;
    const slot = streams.find(handle) orelse return fail(CompressionError.Finished);
    const mode: compression.FlushMode = @enumFromInt(args[2].int64);
    const allocator = std.heap.smp_allocator;
    const output = compression.compressPart(allocator, slot.zip.?, bytesArg(args, 1), mode) catch |err| return streamFailure(err);
    defer allocator.free(output);
    if (mode == .finish) {
        streams.remove(handle);
    }
    return ok(try vm.allocBytesValue(output, .buffer));
}

fn decompressPart(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const slot = vm.event_loop.compression_streams.find(args[0].int64) orelse return fail(CompressionError.Finished);
    const allocator = std.heap.smp_allocator;
    const output = compression.decompressPart(allocator, slot, bytesArg(args, 1)) catch |err| return streamFailure(err);
    defer allocator.free(output);
    return ok(try vm.allocBytesValue(output, .buffer));
}

fn decompressFinish(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const streams = &vm.event_loop.compression_streams;
    const handle = args[0].int64;
    const slot = streams.find(handle) orelse return fail(CompressionError.Finished);
    const is_complete = slot.is_at_end;
    streams.remove(handle);
    if (!is_complete) {
        return fail(CompressionError.InvalidData);
    }
    return ok(.{ .null = {} });
}

fn isStreamAtEnd(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const slot = vm.event_loop.compression_streams.find(args[0].int64) orelse return ok(.{ .bool = false });
    return ok(.{ .bool = slot.is_at_end });
}

fn closeStream(vm: *VM, args: []v.Value) anyerror!NativeResult {
    vm.event_loop.compression_streams.remove(args[0].int64);
    return ok(.{ .null = {} });
}

fn streamFailure(err: compression.StreamError) anyerror!NativeResult {
    return switch (err) {
        error.Finished => fail(CompressionError.Finished),
        error.InvalidData => fail(CompressionError.InvalidData),
        error.TooLarge => fail(CompressionError.TooLarge),
        error.OutOfMemory => err,
    };
}
