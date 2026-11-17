const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const native = @import("../native.zig");
const json_codec = @import("../../json_codec.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;

pub const JsonError = json_codec.JsonError;
pub const error_sets = .{JsonError};

pub const bindings = native.BindingMap.initComptime(.{
    .{ "encode_value", Binding{ .native = &json_codec.encode } },
    .{ "decode_value", Binding{ .native = &json_codec.decode } },
    .{ "skip_space", Binding{ .native = &skipSpace } },
    .{ "string_end", Binding{ .native = &stringEnd } },
    .{ "number_end", Binding{ .native = &numberEnd } },
    .{ "string_value", Binding{ .native = &stringValue } },
    .{ "quote", Binding{ .native = &quote } },
    .{ "format_float", Binding{ .native = &formatFloat } },
});

fn skipSpace(_: *VM, args: []v.Value) anyerror!NativeResult {
    const text = native.bytesArg(args, 0);
    var position: usize = @intCast(@max(args[1].int64, 0));
    while (position < text.len and std.mem.indexOfScalar(u8, " \t\r\n", text[position]) != null) {
        position += 1;
    }
    return native.ok(.{ .int64 = @intCast(position) });
}

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
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const text = native.bytesArg(args, 0);
    const start = args[1].int64;
    const stop = args[2].int64;
    const is_in_bounds = start >= 0 and start < stop and stop <= text.len;
    if (!is_in_bounds) {
        return native.fail(JsonError.InvalidJson);
    }
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

fn formatFloat(vm: *VM, args: []v.Value) anyerror!NativeResult {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const number = args[0].float64;
    if (!std.math.isFinite(number)) {
        return native.fail(JsonError.NonFiniteNumber);
    }
    const text = try std.json.Stringify.valueAlloc(arena.allocator(), number, .{});
    return native.ok(try vm.allocBytesValue(text, .string));
}
