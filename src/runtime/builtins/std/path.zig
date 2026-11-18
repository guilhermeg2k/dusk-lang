const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const paths = @import("../../path.zig").NativePaths;
const native = @import("../native.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const PathError = @import("../../path.zig").PathError;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "join", Binding{ .native = &join } },
    .{ "normalize", Binding{ .native = &normalize } },
    .{ "basename", Binding{ .native = &basename } },
    .{ "dirname", Binding{ .native = &dirname } },
    .{ "extension", Binding{ .native = &extension } },
    .{ "stem", Binding{ .native = &stem } },
    .{ "is_absolute", Binding{ .native = &isAbsolute } },
    .{ "resolve", Binding{ .native = &resolve } },
    .{ "relative", Binding{ .native = &relative } },
});

pub const error_sets = .{PathError};

fn join(vm: *VM, args: []v.Value) anyerror!NativeResult {
    args[0].heap_obj = args[0].heap_obj.followRelocated();
    const array = v.HeapValue.getParentPtr(v.Array, args[0].heap_obj);
    const parts = try vm.allocator.alloc([]const u8, array.len);
    defer vm.allocator.free(parts);
    for (parts, array.getDataPtr()[0..array.len]) |*part, item| {
        part.* = v.HeapValue.getParentPtr(v.Bytes, item.heap_obj).slice();
    }
    const result = try std.fs.path.join(vm.allocator, parts);
    defer vm.allocator.free(result);
    return native.ok(try vm.allocBytesValue(result, .string));
}

fn normalize(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const result = try paths.normalize(vm.allocator, native.bytesArg(args, 0));
    defer vm.allocator.free(result);
    return native.ok(try vm.allocBytesValue(result, .string));
}

fn basename(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return copyPathComponent(vm, args, paths.basename(native.bytesArg(args, 0)));
}

fn copyPathComponent(vm: *VM, args: []v.Value, text: []const u8) !NativeResult {
    const offset = if (text.len == 0) 0 else @intFromPtr(text.ptr) - @intFromPtr(native.bytesArg(args, 0).ptr);
    const length = text.len;
    const result = try vm.allocString(length);
    @memcpy(result.mutableSlice(), native.bytesArg(args, 0)[offset..][0..length]);
    return native.stringResult(result);
}

fn dirname(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const result = paths.dirname(native.bytesArg(args, 0)) orelse return native.ok(v.NULL_VALUE);
    return copyPathComponent(vm, args, result);
}

fn extension(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return copyPathComponent(vm, args, paths.extension(native.bytesArg(args, 0)));
}

fn stem(vm: *VM, args: []v.Value) anyerror!NativeResult {
    return copyPathComponent(vm, args, paths.stem(native.bytesArg(args, 0)));
}

fn isAbsolute(_: *VM, args: []v.Value) anyerror!NativeResult {
    return native.ok(.{ .bool = paths.isAbsolute(native.bytesArg(args, 0)) });
}

fn resolve(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const result = paths.resolve(vm.allocator, native.bytesArg(args, 0), native.bytesArg(args, 1)) catch |failure| {
        if (failure == error.NotAbsolute) {
            return native.fail(PathError.NotAbsolute);
        }
        return failure;
    };
    defer vm.allocator.free(result);
    return native.ok(try vm.allocBytesValue(result, .string));
}

fn relative(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const result = paths.relative(vm.allocator, native.bytesArg(args, 0), native.bytesArg(args, 1)) catch |failure| {
        if (failure == error.NotAbsolute) {
            return native.fail(PathError.NotAbsolute);
        }
        return failure;
    };
    defer vm.allocator.free(result);
    return native.ok(try vm.allocBytesValue(result, .string));
}
