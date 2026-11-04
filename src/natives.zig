const std = @import("std");
const bc = @import("bytecode.zig");
const v = @import("value.zig");
const format = @import("format.zig");
const VM = @import("vm.zig").VM;

const NativeResult = bc.NativeResult;

pub const conversion_invalid_id: i64 = 1;
pub const conversion_overflow_id: i64 = 2;
pub const string_out_of_range_id: i64 = 3;
pub const string_negative_count_id: i64 = 4;
pub const string_invalid_utf8_id: i64 = 5;

fn ok(value: v.Value) NativeResult {
    return .{ .value = value };
}

fn fail(id: i64) NativeResult {
    return .{ .value = .{ .int64 = id }, .has_failed = true };
}

fn stringResult(result: *v.Bytes) NativeResult {
    return ok(.{ .heap_obj = &result.obj });
}

fn bytesArg(args: []const v.Value, index: usize) []const u8 {
    return v.HeapValue.getParentPtr(v.Bytes, args[index].heap_obj).slice();
}

pub fn echo(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const writer = vm.stdout orelse return ok(.{ .null = {} });
    const ty: v.ValueType = @enumFromInt(@as(u8, @intCast(args[1].int64)));
    format.printValue(writer, args[0], ty);
    writer.print("\n", .{}) catch {};
    //todo: not treating error
    writer.flush() catch {};
    return ok(.{ .null = {} });
}

pub fn assert(_: *VM, args: []v.Value) anyerror!NativeResult {
    if (!args[0].bool) {
        @panic("ASSERTION_FAILED");
    }
    return ok(.{ .null = {} });
}

pub fn strCharCount(_: *VM, args: []v.Value) anyerror!NativeResult {
    var count: i64 = 0;
    for (bytesArg(args, 0)) |byte| {
        if (byte & 0xC0 != 0x80) count += 1;
    }
    return ok(.{ .int64 = count });
}

pub fn strConcat(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const left_len = bytesArg(args, 0).len;
    const right_len = bytesArg(args, 1).len;
    const result = try vm.allocString(left_len + right_len);
    const data = result.mutableSlice();
    @memcpy(data[0..left_len], bytesArg(args, 0));
    @memcpy(data[left_len..], bytesArg(args, 1));
    return stringResult(result);
}

fn joinParts(parts_value: *v.Value) []const v.Value {
    if (v.HeapValue.followRelocated(parts_value.heap_obj)) |relocated| parts_value.heap_obj = relocated;
    const array = v.HeapValue.getParentPtr(v.Array, parts_value.heap_obj);
    return array.getDataPtr()[0..array.len];
}

pub fn strJoin(vm: *VM, args: []v.Value) anyerror!NativeResult {
    var parts = joinParts(&args[0]);
    const separator_len = bytesArg(args, 1).len;
    var total: usize = 0;
    for (parts, 0..) |part, i| {
        if (i > 0) total += separator_len;
        total += v.HeapValue.getParentPtr(v.Bytes, part.heap_obj).len;
    }
    const result = try vm.allocString(total);
    parts = joinParts(&args[0]);
    const separator_bytes = bytesArg(args, 1);
    const data = result.mutableSlice();
    var offset: usize = 0;
    for (parts, 0..) |part, i| {
        if (i > 0) {
            @memcpy(data[offset..][0..separator_bytes.len], separator_bytes);
            offset += separator_bytes.len;
        }
        const part_bytes = v.HeapValue.getParentPtr(v.Bytes, part.heap_obj).slice();
        @memcpy(data[offset..][0..part_bytes.len], part_bytes);
        offset += part_bytes.len;
    }
    return stringResult(result);
}

pub fn bufferFrom(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const size = v.Bytes.calc_size(bytesArg(args, 0).len);
    try vm.maybeCollectGarbage(size);
    const bytes = try v.Bytes.init(vm.getCurrentHeapAllocator(), bytesArg(args, 0), .buffer);
    vm.increaseHeapAllocationCount(size);
    return ok(.{ .heap_obj = &bytes.obj });
}

pub fn stringFrom(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const text = bytesArg(args, 0);
    if (!std.unicode.utf8ValidateSlice(text)) return fail(string_invalid_utf8_id);
    const result = try vm.allocString(text.len);
    @memcpy(result.mutableSlice(), bytesArg(args, 0));
    return stringResult(result);
}

pub fn strEqual(_: *VM, args: []v.Value) anyerror!NativeResult {
    const left = v.HeapValue.getParentPtr(v.Bytes, args[0].heap_obj);
    const right = v.HeapValue.getParentPtr(v.Bytes, args[1].heap_obj);
    if (!args[2].bool) return ok(.{ .bool = left.eql(right) });
    return ok(.{ .bool = isEqualIgnoringCase(left.slice(), right.slice()) });
}

pub fn strFind(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const needle = bytesArg(args, 1);
    if (needle.len == 0) return ok(v.NULL_VALUE);
    const index = std.mem.indexOf(u8, bytesArg(args, 0), needle) orelse return ok(v.NULL_VALUE);
    return ok(try vm.boxInt(@intCast(index)));
}

pub fn strContains(_: *VM, args: []v.Value) anyerror!NativeResult {
    const needle = bytesArg(args, 1);
    return ok(.{ .bool = needle.len > 0 and std.mem.indexOf(u8, bytesArg(args, 0), needle) != null });
}

pub fn strStartsWith(_: *VM, args: []v.Value) anyerror!NativeResult {
    const prefix = bytesArg(args, 1);
    return ok(.{ .bool = prefix.len > 0 and std.mem.startsWith(u8, bytesArg(args, 0), prefix) });
}

pub fn strEndsWith(_: *VM, args: []v.Value) anyerror!NativeResult {
    const suffix = bytesArg(args, 1);
    return ok(.{ .bool = suffix.len > 0 and std.mem.endsWith(u8, bytesArg(args, 0), suffix) });
}

pub fn strCount(_: *VM, args: []v.Value) anyerror!NativeResult {
    const needle = bytesArg(args, 1);
    if (needle.len == 0) return ok(.{ .int64 = 0 });
    return ok(.{ .int64 = @intCast(std.mem.count(u8, bytesArg(args, 0), needle)) });
}

const ascii_whitespace = " \t\n\r\x0b\x0c";

pub fn strTrim(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const text = bytesArg(args, 0);
    const trimmed = std.mem.trim(u8, text, ascii_whitespace);
    const start = @intFromPtr(trimmed.ptr) - @intFromPtr(text.ptr);
    const result = try vm.allocString(trimmed.len);
    @memcpy(result.mutableSlice(), bytesArg(args, 0)[start..][0..trimmed.len]);
    return stringResult(result);
}

pub fn strUpper(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return changedCase(vm, args, true);
}

pub fn strLower(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return changedCase(vm, args, false);
}

fn changedCase(vm: *VM, args: []v.Value, is_upper: bool) anyerror!NativeResult {
    const result = try vm.allocString(bytesArg(args, 0).len);
    const data = result.mutableSlice();
    @memcpy(data, bytesArg(args, 0));
    changeCase(data, is_upper);
    return stringResult(result);
}

pub fn strRepeat(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const count = args[1].int64;
    if (count < 0) return fail(string_negative_count_id);
    const total = std.math.mul(usize, bytesArg(args, 0).len, @intCast(count)) catch return error.OutOfMemory;
    const result = try vm.allocString(total);
    const data = result.mutableSlice();
    const source = bytesArg(args, 0);
    var offset: usize = 0;
    while (offset < total) : (offset += source.len) @memcpy(data[offset..][0..source.len], source);
    return stringResult(result);
}

pub fn strReplace(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const text = bytesArg(args, 0);
    const old = bytesArg(args, 1);
    if (old.len == 0) return ok(args[0]);
    const matches = std.mem.count(u8, text, old);
    const total = text.len - matches * old.len + matches * bytesArg(args, 2).len;
    const result = try vm.allocString(total);
    _ = std.mem.replace(u8, bytesArg(args, 0), bytesArg(args, 1), bytesArg(args, 2), result.mutableSlice());
    return stringResult(result);
}

pub fn strSlice(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const text = bytesArg(args, 0);
    const start = args[1].int64;
    const end = args[2].int64;
    const is_in_range = start >= 0 and start <= end and end <= text.len;
    if (!is_in_range) return fail(string_out_of_range_id);
    const start_index: usize = @intCast(start);
    const end_index: usize = @intCast(end);
    if (!isCharBoundary(text, start_index) or !isCharBoundary(text, end_index)) return fail(string_out_of_range_id);
    const result = try vm.allocString(end_index - start_index);
    @memcpy(result.mutableSlice(), bytesArg(args, 0)[start_index..end_index]);
    return stringResult(result);
}

pub fn strByteAt(_: *VM, args: []v.Value) anyerror!NativeResult {
    const text = bytesArg(args, 0);
    const index = args[1].int64;
    if (index < 0 or index >= text.len) return fail(string_out_of_range_id);
    return ok(.{ .int64 = text[@intCast(index)] });
}

pub fn strCharAt(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const text = bytesArg(args, 0);
    const wanted = args[1].int64;
    if (wanted < 0) return fail(string_out_of_range_id);
    var characters = Pieces{ .text = text, .separator = "" };
    var position: i64 = 0;
    while (characters.next()) |character| : (position += 1) {
        if (position != wanted) continue;
        const start = @intFromPtr(character.ptr) - @intFromPtr(text.ptr);
        const result = try vm.allocString(character.len);
        @memcpy(result.mutableSlice(), bytesArg(args, 0)[start..][0..character.len]);
        return stringResult(result);
    }
    return fail(string_out_of_range_id);
}

pub fn strSplit(vm: *VM, args: []v.Value) anyerror!NativeResult {
    var piece_count: usize = 0;
    var total: usize = 0;
    var pieces = Pieces{ .text = bytesArg(args, 0), .separator = bytesArg(args, 1) };
    while (pieces.next()) |piece| {
        piece_count += 1;
        total += std.mem.Alignment.of(v.Bytes).forward(v.Bytes.calc_size(piece.len));
    }
    const capacity = @max(piece_count, 1);
    total += v.Array.calc_size(capacity);
    try vm.maybeCollectGarbage(total);

    const allocator = vm.getCurrentHeapAllocator();
    const array = try v.Array.init(allocator, .string, capacity);
    vm.increaseHeapAllocationCount(v.Array.calc_size(capacity));
    pieces = Pieces{ .text = bytesArg(args, 0), .separator = bytesArg(args, 1) };
    while (pieces.next()) |piece| {
        const bytes = try v.Bytes.init(allocator, piece, .string);
        vm.increaseHeapAllocationCount(v.Bytes.calc_size(piece.len));
        try array.append(.{ .heap_obj = &bytes.obj });
    }
    return ok(.{ .heap_obj = &array.obj });
}

pub fn strToInt(_: *VM, args: []v.Value) anyerror!NativeResult {
    const number = parseIntText(bytesArg(args, 0)) catch |failure| return conversionFailure(failure);
    return ok(.{ .int64 = number });
}

pub fn strToFloat(_: *VM, args: []v.Value) anyerror!NativeResult {
    const number = parseFloatText(bytesArg(args, 0)) catch |failure| return conversionFailure(failure);
    return ok(.{ .float64 = number });
}

const ConversionError = error{ Invalid, Overflow };

fn conversionFailure(failure: ConversionError) NativeResult {
    return fail(switch (failure) {
        error.Invalid => conversion_invalid_id,
        error.Overflow => conversion_overflow_id,
    });
}

fn skipDigits(text: []const u8, start: usize) usize {
    var index = start;
    while (index < text.len and std.ascii.isDigit(text[index])) index += 1;
    return index;
}

fn parseIntText(text: []const u8) ConversionError!i64 {
    const digits_start: usize = if (text.len > 0 and text[0] == '-') 1 else 0;
    const digits_end = skipDigits(text, digits_start);
    if (digits_end == digits_start or digits_end != text.len) return error.Invalid;
    return std.fmt.parseInt(i64, text, 10) catch |failure| switch (failure) {
        error.Overflow => error.Overflow,
        error.InvalidCharacter => error.Invalid,
    };
}

fn parseFloatText(text: []const u8) ConversionError!f64 {
    const digits_start: usize = if (text.len > 0 and text[0] == '-') 1 else 0;
    var end = skipDigits(text, digits_start);
    if (end == digits_start) return error.Invalid;
    if (end < text.len and text[end] == '.') {
        const fraction_end = skipDigits(text, end + 1);
        if (fraction_end == end + 1) return error.Invalid;
        end = fraction_end;
    }
    if (end != text.len) return error.Invalid;
    const number = std.fmt.parseFloat(f64, text) catch return error.Invalid;
    if (std.math.isInf(number)) return error.Overflow;
    return number;
}

fn isCharBoundary(text: []const u8, index: usize) bool {
    if (index == text.len) return true;
    return text[index] & 0xC0 != 0x80;
}

fn isEqualIgnoringCase(left: []const u8, right: []const u8) bool {
    if (left.len != right.len) return false;
    var index: usize = 0;
    while (index < left.len) : (index += 1) {
        const is_latin_pair = left[index] == 0xC3 and right[index] == 0xC3 and index + 1 < left.len;
        if (is_latin_pair) {
            index += 1;
            if (lowerLatinByte(left[index]) != lowerLatinByte(right[index])) return false;
            continue;
        }
        if (std.ascii.toLower(left[index]) != std.ascii.toLower(right[index])) return false;
    }
    return true;
}

fn lowerLatinByte(second: u8) u8 {
    const is_upper_latin = second >= 0x80 and second <= 0x9E and second != 0x97;
    return if (is_upper_latin) second + 0x20 else second;
}

fn changeCase(data: []u8, is_upper: bool) void {
    var index: usize = 0;
    while (index < data.len) : (index += 1) {
        const byte = data[index];
        if (byte == 0xC3 and index + 1 < data.len) {
            index += 1;
            const second = data[index];
            const is_lower_latin = second >= 0xA0 and second <= 0xBE and second != 0xB7;
            const is_upper_latin = second >= 0x80 and second <= 0x9E and second != 0x97;
            if (is_upper and is_lower_latin) data[index] = second - 0x20;
            if (!is_upper and is_upper_latin) data[index] = second + 0x20;
            continue;
        }
        if (is_upper and std.ascii.isLower(byte)) data[index] = std.ascii.toUpper(byte);
        if (!is_upper and std.ascii.isUpper(byte)) data[index] = std.ascii.toLower(byte);
    }
}

const Pieces = struct {
    text: []const u8,
    separator: []const u8,
    index: ?usize = 0,

    fn next(self: *Pieces) ?[]const u8 {
        const start = self.index orelse return null;
        if (self.separator.len == 0) {
            if (start >= self.text.len) {
                self.index = null;
                return null;
            }
            var end = start + 1;
            while (!isCharBoundary(self.text, end)) end += 1;
            self.index = end;
            return self.text[start..end];
        }
        if (std.mem.indexOfPos(u8, self.text, start, self.separator)) |position| {
            self.index = position + self.separator.len;
            return self.text[start..position];
        }
        self.index = null;
        return self.text[start..];
    }
};

pub const Binding = union(enum) {
    native: bc.NativeFnPtr,
    @"inline": @FieldType(bc.InlineFn, "gen"),
    sema: void,
};

pub const bindings = std.StaticStringMap(Binding).initComptime(.{
    .{ "echo", Binding{ .native = &echo } },
    .{ "assert", Binding{ .native = &assert } },
    .{ "len", Binding{ .sema = {} } },
    .{ "append", Binding{ .@"inline" = &appendGen } },
    .{ "copy", Binding{ .@"inline" = &copyGen } },
    .{ "buffer_from", Binding{ .native = &bufferFrom } },
    .{ "string_from", Binding{ .native = &stringFrom } },
    .{ "str_equal", Binding{ .native = &strEqual } },
    .{ "str_char_count", Binding{ .native = &strCharCount } },
    .{ "str_concat", Binding{ .native = &strConcat } },
    .{ "str_join", Binding{ .native = &strJoin } },
    .{ "str_to_int", Binding{ .native = &strToInt } },
    .{ "str_to_float", Binding{ .native = &strToFloat } },
    .{ "str_find", Binding{ .native = &strFind } },
    .{ "str_contains", Binding{ .native = &strContains } },
    .{ "str_starts_with", Binding{ .native = &strStartsWith } },
    .{ "str_ends_with", Binding{ .native = &strEndsWith } },
    .{ "str_count", Binding{ .native = &strCount } },
    .{ "str_byte_at", Binding{ .native = &strByteAt } },
    .{ "str_char_at", Binding{ .native = &strCharAt } },
    .{ "str_trim", Binding{ .native = &strTrim } },
    .{ "str_split", Binding{ .native = &strSplit } },
    .{ "str_replace", Binding{ .native = &strReplace } },
    .{ "str_upper", Binding{ .native = &strUpper } },
    .{ "str_lower", Binding{ .native = &strLower } },
    .{ "str_slice", Binding{ .native = &strSlice } },
    .{ "str_repeat", Binding{ .native = &strRepeat } },
});

fn appendGen(_: u8, arg_start_reg: u8, alloc: std.mem.Allocator) ![]const bc.Instruction {
    var list: std.ArrayList(bc.Instruction) = .empty;
    try list.append(alloc, .{ .op = .ARRAY_APPEND, .a = arg_start_reg, .b = arg_start_reg + 1 });
    return try list.toOwnedSlice(alloc);
}

fn copyGen(target_reg: u8, arg_start_reg: u8, alloc: std.mem.Allocator) ![]const bc.Instruction {
    var list: std.ArrayList(bc.Instruction) = .empty;
    try list.append(alloc, .{ .op = .DEEP_COPY, .a = target_reg, .b = arg_start_reg });
    return try list.toOwnedSlice(alloc);
}
