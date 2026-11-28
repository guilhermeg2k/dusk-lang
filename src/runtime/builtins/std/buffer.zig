const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const native = @import("../native.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const ok = native.ok;
const bytesArg = native.bytesArg;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "len", Binding{ .intrinsic = .bytes_len } },
    .{ "unchecked_new", Binding{ .intrinsic = .new_buffer } },
    .{ "from_string", Binding{ .native = &bufferFrom } },
    .{ "put", Binding{ .native = &put } },
    .{ "put_bytes", Binding{ .native = &put } },
    .{ "slice", Binding{ .native = &slice } },
    .{ "read_u8", Binding{ .native = &readNumber(u8) } },
    .{ "read_i8", Binding{ .native = &readNumber(i8) } },
    .{ "read_u16", Binding{ .native = &readNumber(u16) } },
    .{ "read_i16", Binding{ .native = &readNumber(i16) } },
    .{ "read_u32", Binding{ .native = &readNumber(u32) } },
    .{ "read_i32", Binding{ .native = &readNumber(i32) } },
    .{ "read_i64", Binding{ .native = &readNumber(i64) } },
    .{ "read_f32", Binding{ .native = &readNumber(f32) } },
    .{ "read_f64", Binding{ .native = &readNumber(f64) } },
    .{ "write_u8", Binding{ .native = &writeNumber(u8) } },
    .{ "write_i8", Binding{ .native = &writeNumber(i8) } },
    .{ "write_u16", Binding{ .native = &writeNumber(u16) } },
    .{ "write_i16", Binding{ .native = &writeNumber(i16) } },
    .{ "write_u32", Binding{ .native = &writeNumber(u32) } },
    .{ "write_i32", Binding{ .native = &writeNumber(i32) } },
    .{ "write_i64", Binding{ .native = &writeNumber(i64) } },
    .{ "write_f32", Binding{ .native = &writeNumber(f32) } },
    .{ "write_f64", Binding{ .native = &writeNumber(f64) } },
    .{ "equal", Binding{ .native = &equal } },
    .{ "starts_with", Binding{ .native = &startsWith } },
    .{ "index_of", Binding{ .native = &indexOf } },
    .{ "fill", Binding{ .native = &fill } },
    .{ "copy_within", Binding{ .native = &copyWithin } },
    .{ "parse_int", Binding{ .native = &parseInt } },
});

pub const BufferError = enum { NegativeSize, OutOfRange, ValueOutOfRange, InvalidNumber };

const NativeFn = fn (*VM, []v.Value) anyerror!NativeResult;
pub const error_sets = .{BufferError};

fn bufferFrom(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const size = v.Bytes.calc_size(bytesArg(args, 0).len);
    try vm.maybeCollectGarbage(size);
    const bytes = try v.Bytes.init(vm.getCurrentHeapAllocator(), bytesArg(args, 0), .buffer);
    vm.increaseHeapAllocationCount(size);
    return ok(.{ .heap_obj = &bytes.obj });
}

fn put(_: *VM, args: []v.Value) anyerror!NativeResult {
    return putBytes(args, bytesArg(args, 2));
}

fn putBytes(args: []v.Value, source: []const u8) NativeResult {
    const target = v.HeapValue.getParentPtr(v.Bytes, args[0].heap_obj).mutableSlice();
    const at = args[1].int64;
    const fits = at >= 0 and at <= target.len and source.len <= target.len - @as(usize, @intCast(at));
    if (!fits) {
        return native.fail(BufferError.OutOfRange);
    }
    const start: usize = @intCast(at);
    std.mem.copyForwards(u8, target[start .. start + source.len], source);
    return ok(.{ .int64 = @intCast(start + source.len) });
}

fn slice(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const length = bytesArg(args, 0).len;
    const start = args[1].int64;
    const stop = args[2].int64;
    const is_in_range = start >= 0 and start <= stop and stop <= length;
    if (!is_in_range) {
        return native.fail(BufferError.OutOfRange);
    }
    const size = v.Bytes.calc_size(@intCast(stop - start));
    try vm.maybeCollectGarbage(size);
    const bytes = try v.Bytes.init(vm.getCurrentHeapAllocator(), bytesArg(args, 0)[@intCast(start)..@intCast(stop)], .buffer);
    vm.increaseHeapAllocationCount(size);
    return ok(.{ .heap_obj = &bytes.obj });
}

fn readNumber(comptime T: type) NativeFn {
    return struct {
        fn read(_: *VM, args: []v.Value) anyerror!NativeResult {
            const data = bytesArg(args, 0);
            const start = numberStart(data.len, args[1].int64, @sizeOf(T)) orelse return native.fail(BufferError.OutOfRange);
            const Bits = std.meta.Int(.unsigned, @bitSizeOf(T));
            const bits = std.mem.readInt(Bits, data[start..][0..@sizeOf(T)], byteOrder(T, args));
            const number: T = @bitCast(bits);
            if (@typeInfo(T) == .float) {
                return ok(.{ .float64 = number });
            }
            return ok(.{ .int64 = number });
        }
    }.read;
}

fn writeNumber(comptime T: type) NativeFn {
    return struct {
        fn write(_: *VM, args: []v.Value) anyerror!NativeResult {
            const target = v.HeapValue.getParentPtr(v.Bytes, args[0].heap_obj).mutableSlice();
            const start = numberStart(target.len, args[1].int64, @sizeOf(T)) orelse return native.fail(BufferError.OutOfRange);
            const number = numberToWrite(T, args[2]) orelse return native.fail(BufferError.ValueOutOfRange);
            const Bits = std.meta.Int(.unsigned, @bitSizeOf(T));
            std.mem.writeInt(Bits, target[start..][0..@sizeOf(T)], @bitCast(number), byteOrder(T, args));
            return ok(.{ .int64 = @intCast(start + @sizeOf(T)) });
        }
    }.write;
}

fn numberStart(length: usize, at: i64, size: usize) ?usize {
    const fits = at >= 0 and size <= length and at <= length - size;
    if (!fits) {
        return null;
    }
    return @intCast(at);
}

fn byteOrder(comptime T: type, args: []v.Value) std.builtin.Endian {
    if (@sizeOf(T) == 1) {
        return .little;
    }
    const order = args[args.len - 1].int64;
    return if (order == 0) .little else .big;
}

fn numberToWrite(comptime T: type, value: v.Value) ?T {
    if (@typeInfo(T) == .int) {
        return std.math.cast(T, value.int64);
    }
    const number = value.float64;
    if (T == f64) {
        return number;
    }
    const is_too_large = std.math.isFinite(number) and @abs(number) > std.math.floatMax(f32);
    if (is_too_large) {
        return null;
    }
    return @floatCast(number);
}

fn equal(_: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .bool = std.mem.eql(u8, bytesArg(args, 0), bytesArg(args, 1)) });
}

fn startsWith(_: *VM, args: []v.Value) anyerror!NativeResult {
    return ok(.{ .bool = std.mem.startsWith(u8, bytesArg(args, 0), bytesArg(args, 1)) });
}

fn indexOf(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const data = bytesArg(args, 0);
    const needle = bytesArg(args, 1);
    const from = args[2].int64;
    const has_no_search_range = needle.len == 0 or from < 0 or from >= data.len;
    if (has_no_search_range) {
        return ok(v.NULL_VALUE);
    }
    const index = std.mem.indexOfPos(u8, data, @intCast(from), needle) orelse return ok(v.NULL_VALUE);
    return ok(try vm.allocNullBox(.{ .int64 = @intCast(index) }, .int64));
}

fn fill(_: *VM, args: []v.Value) anyerror!NativeResult {
    const target = v.HeapValue.getParentPtr(v.Bytes, args[0].heap_obj).mutableSlice();
    const start = args[2].int64;
    const stop = args[3].int64;
    const is_in_range = start >= 0 and start <= stop and stop <= target.len;
    if (!is_in_range) {
        return native.fail(BufferError.OutOfRange);
    }
    @memset(target[@intCast(start)..@intCast(stop)], @intCast(args[1].int64));
    return ok(.{ .null = {} });
}

fn copyWithin(_: *VM, args: []v.Value) anyerror!NativeResult {
    const target = v.HeapValue.getParentPtr(v.Bytes, args[0].heap_obj).mutableSlice();
    const from = args[1].int64;
    const to = args[2].int64;
    const count = args[3].int64;
    const length: i64 = @intCast(target.len);
    const is_in_range = from >= 0 and to >= 0 and count >= 0 and from <= length - count and to <= length - count;
    if (!is_in_range) {
        return native.fail(BufferError.OutOfRange);
    }
    const source = target[@intCast(from)..@intCast(from + count)];
    const destination = target[@intCast(to)..@intCast(to + count)];
    if (to < from) {
        std.mem.copyForwards(u8, destination, source);
    } else {
        std.mem.copyBackwards(u8, destination, source);
    }
    return ok(.{ .null = {} });
}

fn parseInt(_: *VM, args: []v.Value) anyerror!NativeResult {
    const data = bytesArg(args, 0);
    const start = args[1].int64;
    const stop = args[2].int64;
    const is_in_range = start >= 0 and start <= stop and stop <= data.len;
    if (!is_in_range) {
        return native.fail(BufferError.OutOfRange);
    }
    const text = data[@intCast(start)..@intCast(stop)];
    if (!isDecimalInteger(text)) {
        return native.fail(BufferError.InvalidNumber);
    }
    const number = std.fmt.parseInt(i64, text, 10) catch return native.fail(BufferError.ValueOutOfRange);
    return ok(.{ .int64 = number });
}

fn isDecimalInteger(text: []const u8) bool {
    const digits = if (text.len > 0 and text[0] == '-') text[1..] else text;
    if (digits.len == 0) {
        return false;
    }
    for (digits) |digit| {
        if (!std.ascii.isDigit(digit)) {
            return false;
        }
    }
    return true;
}
