const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const native = @import("../native.zig");
const json_codec = @import("../../json_codec.zig");
const serialization_plan = @import("../../serialization_plan.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;

pub const JsonError = json_codec.JsonError;
pub const error_sets = .{JsonError};

pub const bindings = native.BindingMap.initComptime(.{
    .{ "encode_value", Binding{ .native = &json_codec.encode } },
    .{ "decode_value", Binding{ .native = &json_codec.decode } },
    .{ "string_end", Binding{ .native = &stringEnd } },
    .{ "number_end", Binding{ .native = &numberEnd } },
    .{ "string_value", Binding{ .native = &stringValue } },
    .{ "quote", Binding{ .native = &quote } },
    .{ "key_index", Binding{ .native = &keyIndex } },
    .{ "parse_int", Binding{ .native = &parseInt } },
    .{ "parse_float", Binding{ .native = &parseFloat } },
    .{ "put_quoted", Binding{ .native = &putQuoted } },
    .{ "put_float", Binding{ .native = &putFloat } },
    .{ "buffer_text", Binding{ .native = &bufferText } },
});

fn stringEnd(_: *VM, args: []v.Value) anyerror!NativeResult {
    const text = native.bytesArg(args, 0);
    const start: usize = @intCast(@max(args[1].int64, 0));
    const is_string = start < text.len and text[start] == '"';
    if (!is_string) {
        return native.ok(.{ .int64 = -1 });
    }
    var position = start + 1;
    while (position < text.len) : (position += 1) {
        const c = text[position];
        if (c < 0x20) break;
        if (c == '\\') {
            position += 1;
            continue;
        }
        if (c == '"') {
            return native.ok(.{ .int64 = @intCast(position + 1) });
        }
    }
    return native.ok(.{ .int64 = -1 });
}

fn numberEnd(_: *VM, args: []v.Value) anyerror!NativeResult {
    const text = native.bytesArg(args, 0);
    var position: usize = @intCast(@max(args[1].int64, 0));
    if (position < text.len and text[position] == '-') {
        position += 1;
    }
    if (position >= text.len) {
        return native.ok(.{ .int64 = -1 });
    }
    if (text[position] == '0') {
        position += 1;
    } else {
        const digits = countDigits(text, position);
        if (digits == 0) {
            return native.ok(.{ .int64 = -1 });
        }
        position += digits;
    }
    if (position < text.len and text[position] == '.') {
        const digits = countDigits(text, position + 1);
        if (digits == 0) {
            return native.ok(.{ .int64 = -1 });
        }
        position += 1 + digits;
    }
    const has_exponent = position < text.len and (text[position] == 'e' or text[position] == 'E');
    if (has_exponent) {
        position += 1;
        if (position < text.len and (text[position] == '+' or text[position] == '-')) {
            position += 1;
        }
        const digits = countDigits(text, position);
        if (digits == 0) {
            return native.ok(.{ .int64 = -1 });
        }
        position += digits;
    }
    return native.ok(.{ .int64 = @intCast(position) });
}

fn countDigits(text: []const u8, start: usize) usize {
    var count: usize = 0;
    while (start + count < text.len and std.ascii.isDigit(text[start + count])) {
        count += 1;
    }
    return count;
}

fn stringValue(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const text = native.bytesArg(args, 0);
    const start = args[1].int64;
    const stop = args[2].int64;
    const is_in_bounds = start >= 0 and start + 2 <= stop and stop <= text.len;
    if (!is_in_bounds) {
        return native.fail(JsonError.InvalidJson);
    }
    const inner = text[@intCast(start + 1)..@intCast(stop - 1)];
    const is_plain = std.mem.indexOfScalar(u8, inner, '\\') == null;
    if (is_plain) {
        if (!std.unicode.utf8ValidateSlice(inner)) {
            return native.fail(JsonError.InvalidJson);
        }
        const length = inner.len;
        const result = try vm.allocString(length);
        @memcpy(result.mutableSlice(), native.bytesArg(args, 0)[@intCast(start + 1)..@intCast(stop - 1)]);
        return native.stringResult(result);
    }
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const token = try arena.allocator().dupe(u8, text[@intCast(start)..@intCast(stop)]);
    var scanner = std.json.Scanner.initCompleteInput(arena.allocator(), token);
    const value = scanner.nextAlloc(arena.allocator(), .alloc_always) catch return native.fail(JsonError.InvalidJson);
    const decoded = switch (value) {
        .string, .allocated_string => |decoded| decoded,
        else => return native.fail(JsonError.InvalidJson),
    };
    return native.ok(try vm.allocBytesValue(decoded, .string));
}

fn quote(vm: *VM, args: []v.Value) anyerror!NativeResult {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const text = native.bytesArg(args, 0);
    if (!std.unicode.utf8ValidateSlice(text)) {
        return native.fail(JsonError.InvalidJson);
    }
    const quoted = try std.json.Stringify.valueAlloc(arena.allocator(), text, .{});
    return native.ok(try vm.allocBytesValue(quoted, .string));
}

fn keyIndex(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const fields = fieldsOf(vm, args[0].int64) orelse return native.ok(.{ .int64 = -1 });
    const text = native.bytesArg(args, 1);
    const start = args[2].int64;
    const stop = args[3].int64;
    const is_in_bounds = start >= 0 and start + 2 <= stop and stop <= text.len;
    if (!is_in_bounds) {
        return native.fail(JsonError.InvalidJson);
    }
    const inner = text[@intCast(start + 1)..@intCast(stop - 1)];
    const is_plain = std.mem.indexOfScalar(u8, inner, '\\') == null;
    if (is_plain) {
        return native.ok(.{ .int64 = indexOfField(fields, inner) });
    }
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const token = try arena.allocator().dupe(u8, text[@intCast(start)..@intCast(stop)]);
    var scanner = std.json.Scanner.initCompleteInput(arena.allocator(), token);
    const value = scanner.nextAlloc(arena.allocator(), .alloc_always) catch return native.fail(JsonError.InvalidJson);
    const decoded = switch (value) {
        .string, .allocated_string => |decoded| decoded,
        else => return native.fail(JsonError.InvalidJson),
    };
    return native.ok(.{ .int64 = indexOfField(fields, decoded) });
}

fn fieldsOf(vm: *VM, id: i64) ?[]const serialization_plan.Field {
    const plans = vm.program.serialization_plans;
    const is_out_of_range = id < 0 or id >= plans.len;
    if (is_out_of_range) {
        return null;
    }
    return switch (plans[@intCast(id)].shape) {
        .structure => |structure| structure.fields,
        else => null,
    };
}

fn indexOfField(fields: []const serialization_plan.Field, name: []const u8) i64 {
    for (fields, 0..) |field, i| {
        if (field.isNamed(name)) {
            return @intCast(i);
        }
    }
    return -1;
}

fn parseInt(_: *VM, args: []v.Value) anyerror!NativeResult {
    const token = numberToken(native.bytesArg(args, 0), args[1].int64, args[2].int64) orelse return native.fail(JsonError.InvalidJson);
    if (std.mem.indexOfAny(u8, token, ".eE") != null) {
        return native.fail(JsonError.TypeMismatch);
    }
    const number = std.fmt.parseInt(i64, token, 10) catch return native.fail(JsonError.NumberOutOfRange);
    return native.ok(.{ .int64 = number });
}

fn parseFloat(_: *VM, args: []v.Value) anyerror!NativeResult {
    const token = numberToken(native.bytesArg(args, 0), args[1].int64, args[2].int64) orelse return native.fail(JsonError.InvalidJson);
    const number = std.fmt.parseFloat(f64, token) catch return native.fail(JsonError.NumberOutOfRange);
    if (!std.math.isFinite(number)) {
        return native.fail(JsonError.NumberOutOfRange);
    }
    return native.ok(.{ .float64 = number });
}

fn numberToken(text: []const u8, start: i64, stop: i64) ?[]const u8 {
    const is_in_bounds = start >= 0 and start < stop and stop <= text.len;
    if (!is_in_bounds) {
        return null;
    }
    return text[@intCast(start)..@intCast(stop)];
}

fn putQuoted(_: *VM, args: []v.Value) anyerror!NativeResult {
    const text = native.bytesArg(args, 2);
    if (!std.unicode.utf8ValidateSlice(text)) {
        return native.fail(JsonError.InvalidJson);
    }
    const target = roomAt(args) orelse return native.ok(.{ .int64 = -1 });
    var writer = std.Io.Writer.fixed(target);
    std.json.Stringify.encodeJsonString(text, .{}, &writer) catch return native.ok(.{ .int64 = -1 });
    return native.ok(.{ .int64 = args[1].int64 + @as(i64, @intCast(writer.end)) });
}

fn putFloat(_: *VM, args: []v.Value) anyerror!NativeResult {
    const number = args[2].float64;
    if (!std.math.isFinite(number)) {
        return native.fail(JsonError.NonFiniteNumber);
    }
    const target = roomAt(args) orelse return native.ok(.{ .int64 = -1 });
    var writer = std.Io.Writer.fixed(target);
    std.json.Stringify.value(number, .{}, &writer) catch return native.ok(.{ .int64 = -1 });
    return native.ok(.{ .int64 = args[1].int64 + @as(i64, @intCast(writer.end)) });
}

fn roomAt(args: []v.Value) ?[]u8 {
    const data = v.HeapValue.getParentPtr(v.Bytes, args[0].heap_obj).mutableSlice();
    const at = args[1].int64;
    const is_in_range = at >= 0 and at <= data.len;
    if (!is_in_range) {
        return null;
    }
    return data[@intCast(at)..];
}

fn bufferText(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const used: usize = @intCast(@max(args[1].int64, 0));
    const length = @min(used, native.bytesArg(args, 0).len);
    const result = try vm.allocString(length);
    @memcpy(result.mutableSlice(), native.bytesArg(args, 0)[0..length]);
    return native.stringResult(result);
}
