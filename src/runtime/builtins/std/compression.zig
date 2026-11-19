const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const native = @import("../native.zig");
const compression = @import("../../compression.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const ok = native.ok;
const fail = native.fail;
const bytesArg = native.bytesArg;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "compress", Binding{ .native = &compress } },
    .{ "decompress", Binding{ .native = &decompress } },
});

pub const CompressionError = enum { InvalidData, TooLarge, InvalidLevel, Other };
pub const error_sets = .{CompressionError};

fn compress(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const level = args[2].int64;
    const is_valid_level = level >= 0 and level <= compression.max_level;
    if (!is_valid_level) {
        return fail(CompressionError.InvalidLevel);
    }
    const allocator = std.heap.smp_allocator;
    const format: compression.Format = @enumFromInt(args[1].int64);
    const output = try compression.compressAll(allocator, format, @intCast(level), bytesArg(args, 0));
    defer allocator.free(output);
    return ok(try vm.allocBytesValue(output, .buffer));
}

fn decompress(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const allocator = std.heap.smp_allocator;
    const format: compression.Format = @enumFromInt(args[1].int64);
    const max: ?usize = if (args[2].int64 < 0) null else @intCast(args[2].int64);
    const output = compression.decompressAll(allocator, format, bytesArg(args, 0), max) catch |err| switch (err) {
        error.InvalidData => return fail(CompressionError.InvalidData),
        error.TooLarge => return fail(CompressionError.TooLarge),
        error.OutOfMemory => return err,
    };
    defer allocator.free(output);
    return ok(try vm.allocBytesValue(output, .buffer));
}
