const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const random = @import("../../random.zig");
const native = @import("../native.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const RandomError = random.RandomError;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "integer", Binding{ .native = &nextInt } },
    .{ "decimal", Binding{ .native = &nextFloat } },
    .{ "bytes", Binding{ .native = &bytes } },
    .{ "secure_bytes", Binding{ .native = &secureBytes } },
    .{ "seed_words", Binding{ .native = &seedWords } },
    .{ "generator_int", Binding{ .native = &generatorInt } },
    .{ "generator_float", Binding{ .native = &generatorFloat } },
    .{ "generator_bytes", Binding{ .native = &generatorBytes } },
});

pub const error_sets = .{RandomError};

fn nextInt(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const min = args[0].int64;
    const max = args[1].int64;
    if (min > max) {
        return native.fail(RandomError.InvalidBounds);
    }
    if (min == max) {
        return native.ok(.{ .int64 = min });
    }
    const engine = defaultEngine(vm) catch return native.fail(RandomError.Unsupported);
    return native.ok(.{ .int64 = try random.nextInt(engine, min, max) });
}

fn defaultEngine(vm: *VM) error{Unsupported}!*random.Engine {
    if (vm.random_engine) |*engine| {
        return engine;
    }
    const io = vm.io orelse return error.Unsupported;
    var seed: [8]u8 = undefined;
    io.random(&seed);
    vm.random_engine = .init(std.mem.readInt(u64, &seed, .little));
    return &vm.random_engine.?;
}

fn nextFloat(vm: *VM, _: []v.Value) anyerror!NativeResult {
    const engine = defaultEngine(vm) catch return native.fail(RandomError.Unsupported);
    return native.ok(.{ .float64 = random.nextFloat(engine) });
}

fn bytes(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const length = byteCount(args[0].int64) catch |failure| return sizeFailure(failure);
    if (length == 0) {
        return bufferResult(try allocBuffer(vm, 0));
    }
    const engine = defaultEngine(vm) catch return native.fail(RandomError.Unsupported);
    const result = try allocBuffer(vm, length);
    random.fillBytes(engine, result.mutableSlice());
    return bufferResult(result);
}

fn byteCount(size: i64) error{ NegativeSize, OutOfMemory }!usize {
    if (size < 0) {
        return error.NegativeSize;
    }
    const length = std.math.cast(usize, size) orelse return error.OutOfMemory;
    const maximum_length = std.math.maxInt(isize) - @sizeOf(v.Bytes) - @alignOf(v.Bytes);
    if (length > maximum_length) {
        return error.OutOfMemory;
    }
    return length;
}

fn sizeFailure(failure: error{ NegativeSize, OutOfMemory }) !NativeResult {
    if (failure == error.NegativeSize) {
        return native.fail(RandomError.NegativeSize);
    }
    return error.OutOfMemory;
}

fn allocBuffer(vm: *VM, length: usize) !*v.Bytes {
    const size = v.Bytes.calc_size(length);
    _ = std.math.add(usize, vm.heap.allocated, size) catch return error.OutOfMemory;
    try vm.maybeCollectGarbage(size);
    const result = try v.Bytes.initUndefined(vm.getCurrentHeapAllocator(), length, .buffer);
    vm.increaseHeapAllocationCount(size);
    return result;
}

fn bufferResult(buffer: *v.Bytes) NativeResult {
    return native.ok(.{ .heap_obj = &buffer.obj });
}

fn secureBytes(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const length = byteCount(args[0].int64) catch |failure| return sizeFailure(failure);
    if (length == 0) {
        return bufferResult(try allocBuffer(vm, 0));
    }
    const io = vm.io orelse return native.fail(RandomError.Unsupported);
    const result = try allocBuffer(vm, length);
    io.randomSecure(result.mutableSlice()) catch return native.fail(RandomError.EntropyUnavailable);
    return bufferResult(result);
}

fn seedWords(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const engine = random.seeded(args[0].int64);
    var words: [4]i64 = undefined;
    for (&words, engine.s) |*word, bits| {
        word.* = @bitCast(bits);
    }
    return native.ok(try vm.allocIntArray(&words));
}

fn generatorInt(_: *VM, args: []v.Value) anyerror!NativeResult {
    var engine = loadGenerator(args);
    const result = random.nextInt(&engine, args[1].int64, args[2].int64) catch return native.fail(RandomError.InvalidBounds);
    storeGenerator(args, engine);
    return native.ok(.{ .int64 = result });
}

fn loadGenerator(args: []v.Value) random.Engine {
    const generator = v.HeapValue.getParentPtr(v.Struct, args[0].heap_obj);
    var engine: random.Engine = undefined;
    for (&engine.s, generator.getDataPtr()[0..4]) |*word, value| {
        word.* = @bitCast(value.int64);
    }
    return engine;
}

fn storeGenerator(args: []v.Value, engine: random.Engine) void {
    const generator = v.HeapValue.getParentPtr(v.Struct, args[0].heap_obj);
    for (generator.getDataPtr()[0..4], engine.s) |*value, word| {
        value.* = .{ .int64 = @bitCast(word) };
    }
}

fn generatorFloat(_: *VM, args: []v.Value) anyerror!NativeResult {
    var engine = loadGenerator(args);
    const result = random.nextFloat(&engine);
    storeGenerator(args, engine);
    return native.ok(.{ .float64 = result });
}

fn generatorBytes(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const length = byteCount(args[1].int64) catch |failure| return sizeFailure(failure);
    const result = try allocBuffer(vm, length);
    var engine = loadGenerator(args);
    random.fillBytes(&engine, result.mutableSlice());
    storeGenerator(args, engine);
    return bufferResult(result);
}
