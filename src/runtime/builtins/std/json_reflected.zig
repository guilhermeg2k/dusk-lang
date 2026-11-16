const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const native = @import("../native.zig");

pub const JsonError = @import("../../json_codec.zig").JsonError;
pub const error_sets = .{JsonError};

pub const bindings = native.BindingMap.initComptime(.{
    .{ "scan", native.Binding{ .reflection = .{ .operation = .result_type, .func = &scan } } },
    .{ "format_string", native.Binding{ .native = &formatString } },
    .{ "format_int", native.Binding{ .native = &formatInt } },
    .{ "parse_float", native.Binding{ .native = &parseFloat } },
    .{ "format_float", native.Binding{ .native = &formatFloat } },
});

const TokenKind = enum { ObjectBegin, ObjectEnd, ArrayBegin, ArrayEnd, String, Number, True, False, Null, End, Invalid };

fn scan(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const source = try allocator.dupe(u8, native.bytesArg(args, 0));
    var scanner = std.json.Scanner.initCompleteInput(allocator, source);
    defer scanner.deinit();
    const result_type = vm.program.reflection_types[@intCast(args[1].int64)];
    const token_type = vm.program.reflection_types[result_type.child.?];
    const root_count = vm.heap.temporary_roots.items.len;
    defer vm.heap.restoreRootCount(root_count);
    const size = v.Array.calc_size(8);
    try vm.maybeCollectGarbage(size);
    const array = try v.Array.init(vm.getCurrentHeapAllocator(), .@"struct", 8);
    vm.increaseHeapAllocationCount(size);
    const root = try vm.heap.protectValue(.{ .heap_obj = &array.obj }, .array);
    while (true) {
        _ = scanner.peekNextTokenType() catch |failure| {
            if (failure == error.OutOfMemory) {
                return failure;
            }
            try appendToken(vm, root, token_type.object_descriptor.?, .Invalid, "", scanner.cursor);
            break;
        };
        const offset = scanner.cursor;
        const token = scanner.nextAllocMax(allocator, .alloc_always, source.len) catch |failure| {
            if (failure == error.OutOfMemory) {
                return failure;
            }
            try appendToken(vm, root, token_type.object_descriptor.?, .Invalid, "", scanner.cursor);
            break;
        };
        const kind: TokenKind = switch (token) {
            .object_begin => .ObjectBegin,
            .object_end => .ObjectEnd,
            .array_begin => .ArrayBegin,
            .array_end => .ArrayEnd,
            .string, .allocated_string => .String,
            .number, .allocated_number => .Number,
            .true => .True,
            .false => .False,
            .null => .Null,
            .end_of_document => .End,
            else => unreachable,
        };
        const text = switch (token) {
            .string, .allocated_string, .number, .allocated_number => |text| text,
            else => "",
        };
        try appendToken(vm, root, token_type.object_descriptor.?, kind, text, offset);
        if (kind == .End) break;
    }
    return native.ok(vm.heap.protectedValue(root));
}

fn appendToken(vm: *VM, list: usize, descriptor: u16, kind: TokenKind, text: []const u8, offset: usize) !void {
    const root_count = vm.heap.temporary_roots.items.len;
    defer vm.heap.restoreRootCount(root_count);
    const string = try vm.allocBytesValue(text, .string);
    const string_root = try vm.heap.protectValue(string, .string);
    const size = v.Struct.calc_size(3);
    try vm.maybeCollectGarbage(size);
    const token = try v.Struct.init(vm.getCurrentHeapAllocator(), 3, descriptor);
    vm.increaseHeapAllocationCount(size);
    token.set(0, .{ .int64 = @intFromEnum(kind) });
    token.set(1, vm.heap.protectedValue(string_root));
    token.set(2, .{ .int64 = @intCast(offset) });
    const token_root = try vm.heap.protectValue(.{ .heap_obj = &token.obj }, .@"struct");
    var array = v.HeapValue.getParentPtr(v.Array, vm.heap.protectedValue(list).heap_obj);
    if (array.len == array.capacity) {
        array = try vm.growArray(&vm.heap.temporary_roots.items[list].value);
    }
    try array.append(vm.heap.protectedValue(token_root));
}

fn formatString(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const text = native.bytesArg(args, 0);
    if (!std.unicode.utf8ValidateSlice(text)) {
        return native.fail(JsonError.InvalidJson);
    }
    return native.ok(try formatScalar(vm, text));
}

fn formatScalar(vm: *VM, value: anytype) !v.Value {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const text = try std.json.Stringify.valueAlloc(arena.allocator(), value, .{});
    return vm.allocBytesValue(text, .string);
}

fn formatInt(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    return native.ok(try formatScalar(vm, args[0].int64));
}

fn formatFloat(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    if (!std.math.isFinite(args[0].float64)) {
        return native.fail(JsonError.NonFiniteNumber);
    }
    return native.ok(try formatScalar(vm, args[0].float64));
}

fn parseFloat(_: *VM, args: []v.Value) anyerror!native.NativeResult {
    const number = std.fmt.parseFloat(f64, native.bytesArg(args, 0)) catch return native.fail(JsonError.NumberOutOfRange);
    return native.ok(.{ .float64 = number });
}
