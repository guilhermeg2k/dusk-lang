const std = @import("std");
const v = @import("value.zig");
const VM = @import("vm.zig").VM;
const format = @import("reflection_format.zig");
const native = @import("builtins/native.zig");

pub const ReflectError = enum {
    TypeMismatch,
    UnknownField,
    OutOfRange,
    MissingField,
    DuplicateAssignment,
    InvalidBuilderState,
    UnknownVariant,
    UnsupportedType,
};

pub fn requireValue(_: *VM, _: []v.Value) anyerror!native.NativeResult {
    return native.ok(v.NULL_VALUE);
}

pub fn captureValue(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const type_index: u32 = @intCast(args[1].int64);
    const wrapper: u16 = @intCast(args[2].int64);
    const kind = vm.program.reflection_types[type_index].storage;
    const copied = try vm.copyValue(args[0], kind);
    return native.ok(try wrapValue(vm, type_index, copied, v.NULL_VALUE, wrapper));
}

pub fn wrapValue(vm: *VM, type_index: u32, value: v.Value, session: v.Value, wrapper: u16) !v.Value {
    const root_count = vm.heap.temporary_roots.items.len;
    defer vm.heap.restoreRootCount(root_count);
    const kind = vm.program.reflection_types[type_index].storage;
    const value_root = try vm.heap.protectValue(value, kind);
    const session_root = try vm.heap.protectValue(session, .@"struct");
    const size = @sizeOf(v.ReflectedValue);
    try vm.maybeCollectGarbage(size);
    const result = try v.ReflectedValue.init(vm.getCurrentHeapAllocator(), wrapper, type_index, vm.heap.protectedValue(value_root), kind, vm.heap.protectedValue(session_root));
    vm.increaseHeapAllocationCount(size);
    return .{ .heap_obj = &result.header.obj };
}

pub fn castValue(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const reflected = reflectedValue(args[0]);
    if (!isReady(reflected)) {
        return native.fail(ReflectError.InvalidBuilderState);
    }
    if (reflected.type_index.int64 != args[1].int64) {
        return native.fail(ReflectError.TypeMismatch);
    }
    return native.ok(try vm.copyValue(reflected.value, reflected.value_kind));
}

pub fn reflectedValue(value: v.Value) *v.ReflectedValue {
    return v.ReflectedValue.fromHeap(value.heap_obj);
}

pub fn isReady(value: *const v.ReflectedValue) bool {
    if (@as(u64, @bitCast(value.session)) == 0) {
        return true;
    }
    return v.HeapValue.getParentPtr(v.Struct, value.session.heap_obj).get(0).int64 == 2;
}

pub fn valueDescriptor(vm: *VM, value: *const v.ReflectedValue) format.Descriptor {
    return vm.program.reflection_types[@intCast(value.type_index.int64)];
}

pub fn valueTypeIndex(_: *VM, args: []v.Value) anyerror!native.NativeResult {
    return native.ok(reflectedValue(args[0]).type_index);
}

pub fn readField(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const reflected = reflectedValue(args[0]);
    if (!isReady(reflected)) {
        return native.fail(ReflectError.InvalidBuilderState);
    }
    const source = valueDescriptor(vm, reflected);
    if (source.kind != .Struct) {
        return native.fail(ReflectError.TypeMismatch);
    }
    const name = native.bytesArg(args, 1);
    for (source.fields) |field| {
        if (!std.mem.eql(u8, name, field.name)) continue;
        const object = v.HeapValue.getParentPtr(v.Struct, reflected.value.heap_obj);
        return native.ok(try wrapValue(vm, field.type_index, object.get(field.slot), reflected.session, reflected.header.descriptor_id));
    }
    return native.fail(ReflectError.UnknownField);
}

pub fn readElement(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const reflected = reflectedValue(args[0]);
    if (!isReady(reflected)) {
        return native.fail(ReflectError.InvalidBuilderState);
    }
    const source = valueDescriptor(vm, reflected);
    if (source.kind != .Array) {
        return native.fail(ReflectError.TypeMismatch);
    }
    const array = v.HeapValue.getParentPtr(v.Array, reflected.value.heap_obj.followRelocated());
    const index = args[1].int64;
    const is_in_range = index >= 0 and index < array.len;
    if (!is_in_range) {
        return native.fail(ReflectError.OutOfRange);
    }
    return native.ok(try wrapValue(vm, source.child.?, array.getDataPtr()[@intCast(index)], reflected.session, reflected.header.descriptor_id));
}

pub fn valueLength(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const reflected = reflectedValue(args[0]);
    if (!isReady(reflected)) {
        return native.fail(ReflectError.InvalidBuilderState);
    }
    const source = valueDescriptor(vm, reflected);
    const length = switch (source.kind) {
        .Array => v.HeapValue.getParentPtr(v.Array, reflected.value.heap_obj.followRelocated()).len,
        .Map => v.HeapValue.getParentPtr(v.Map, reflected.value.heap_obj).count(),
        else => return native.fail(ReflectError.TypeMismatch),
    };
    return native.ok(.{ .int64 = @intCast(length) });
}

pub fn areSameObjects(_: *VM, args: []v.Value) anyerror!native.NativeResult {
    const left = reflectedValue(args[0]);
    const right = reflectedValue(args[1]);
    const are_ready = isReady(left) and isReady(right);
    if (!are_ready) {
        return native.fail(ReflectError.InvalidBuilderState);
    }
    const can_compare_identity = left.type_index.int64 == right.type_index.int64 and left.value_kind.isHeapType() and @as(u64, @bitCast(left.value)) != 0 and @as(u64, @bitCast(right.value)) != 0;
    if (!can_compare_identity) {
        return native.ok(.{ .bool = false });
    }
    return native.ok(.{ .bool = left.value.heap_obj.followRelocated() == right.value.heap_obj.followRelocated() });
}

pub fn isNull(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const reflected = reflectedValue(args[0]);
    if (!isReady(reflected)) {
        return native.fail(ReflectError.InvalidBuilderState);
    }
    const has_null_value = valueDescriptor(vm, reflected).kind == .Nullable and @as(u64, @bitCast(reflected.value)) == 0;
    return native.ok(.{ .bool = has_null_value });
}

pub fn unwrapValue(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const reflected = reflectedValue(args[0]);
    if (!isReady(reflected)) {
        return native.fail(ReflectError.InvalidBuilderState);
    }
    const source = valueDescriptor(vm, reflected);
    const has_value = source.kind == .Nullable and @as(u64, @bitCast(reflected.value)) != 0;
    if (!has_value) {
        return native.fail(ReflectError.TypeMismatch);
    }
    const child = vm.program.reflection_types[source.child.?];
    const value = if (child.storage.isHeapType()) reflected.value else v.HeapValue.getParentPtr(v.NullBox, reflected.value.heap_obj).value;
    return native.ok(try wrapValue(vm, source.child.?, value, reflected.session, reflected.header.descriptor_id));
}

pub fn variantName(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const reflected = reflectedValue(args[0]);
    if (!isReady(reflected)) {
        return native.fail(ReflectError.InvalidBuilderState);
    }
    const source = valueDescriptor(vm, reflected);
    const variant_value = switch (source.kind) {
        .Enum => reflected.value.int64,
        .Union => v.HeapValue.getParentPtr(v.Union, reflected.value.heap_obj).active_tag,
        else => return native.fail(ReflectError.TypeMismatch),
    };
    for (source.variants) |variant| {
        if (variant.value != variant_value) continue;
        return native.ok(try vm.allocBytesValue(variant.name, .string));
    }
    return native.fail(ReflectError.UnknownVariant);
}

pub fn variantValue(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const reflected = reflectedValue(args[0]);
    if (!isReady(reflected)) {
        return native.fail(ReflectError.InvalidBuilderState);
    }
    if (valueDescriptor(vm, reflected).kind != .Enum) {
        return native.fail(ReflectError.TypeMismatch);
    }
    return native.ok(reflected.value);
}

pub fn unionPayload(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const reflected = reflectedValue(args[0]);
    if (!isReady(reflected)) {
        return native.fail(ReflectError.InvalidBuilderState);
    }
    const source = valueDescriptor(vm, reflected);
    if (source.kind != .Union) {
        return native.fail(ReflectError.TypeMismatch);
    }
    const object = v.HeapValue.getParentPtr(v.Union, reflected.value.heap_obj);
    for (source.variants) |variant| {
        if (variant.value != object.active_tag) continue;
        return native.ok(try wrapValue(vm, variant.type_index.?, object.get(object.active_tag), reflected.session, reflected.header.descriptor_id));
    }
    return native.fail(ReflectError.UnknownVariant);
}

pub fn mapKey(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    return mapEntry(vm, args, true);
}

pub fn mapValue(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    return mapEntry(vm, args, false);
}

fn mapEntry(vm: *VM, args: []v.Value, should_read_key: bool) !native.NativeResult {
    const reflected = reflectedValue(args[0]);
    if (!isReady(reflected)) {
        return native.fail(ReflectError.InvalidBuilderState);
    }
    const source = valueDescriptor(vm, reflected);
    if (source.kind != .Map) {
        return native.fail(ReflectError.TypeMismatch);
    }
    const map = v.HeapValue.getParentPtr(v.Map, reflected.value.heap_obj);
    const index = args[1].int64;
    const is_in_range = index >= 0 and index < map.count();
    if (!is_in_range) {
        return native.fail(ReflectError.OutOfRange);
    }
    var position: i64 = 0;
    for (map.getStorage().?.usedEntries()) |entry| {
        if (entry.isRemoved()) continue;
        if (position == index) {
            const child = if (should_read_key) source.key.? else source.child.?;
            const value = if (should_read_key) entry.key else entry.value;
            return native.ok(try wrapValue(vm, child, value, reflected.session, reflected.header.descriptor_id));
        }
        position += 1;
    }
    unreachable;
}

pub fn shareState(_: *VM, args: []v.Value) anyerror!native.NativeResult {
    args[0].heap_obj.kind = .reflection_state;
    return native.ok(v.NULL_VALUE);
}

pub fn canConstruct(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    var visited: std.AutoHashMapUnmanaged(u32, void) = .empty;
    defer visited.deinit(vm.allocator);
    const index: u32 = @intCast(v.HeapValue.getParentPtr(v.Struct, args[0].heap_obj).get(0).int64);
    return native.ok(.{ .bool = try canConstructType(vm, index, &visited) });
}

fn canConstructType(vm: *VM, index: u32, visited: *std.AutoHashMapUnmanaged(u32, void)) !bool {
    if (visited.contains(index)) {
        return true;
    }
    try visited.put(vm.allocator, index, {});
    const source = vm.program.reflection_types[index];
    switch (source.kind) {
        .Int, .Byte, .Float, .Bool, .String, .Buffer, .Enum => return true,
        .Array, .Nullable => return canConstructType(vm, source.child.?, visited),
        .Map => {
            const is_identity_key = vm.program.reflection_types[source.key.?].kind == .Struct;
            if (is_identity_key) {
                return false;
            }
            if (!try canConstructType(vm, source.key.?, visited)) {
                return false;
            }
            return canConstructType(vm, source.child.?, visited);
        },
        .Struct => {
            if (source.has_private_fields) {
                return false;
            }
            for (source.fields) |field| {
                if (!try canConstructType(vm, field.type_index, visited)) {
                    return false;
                }
            }
            return true;
        },
        .Union => {
            for (source.variants) |variant| {
                if (!try canConstructType(vm, variant.type_index.?, visited)) {
                    return false;
                }
            }
            return true;
        },
        else => return false,
    }
}

pub fn canAttach(_: *VM, args: []v.Value) anyerror!native.NativeResult {
    const value = reflectedValue(args[1]);
    if (isReady(value)) {
        return native.ok(.{ .bool = true });
    }
    return native.ok(.{ .bool = value.session.heap_obj == args[0].heap_obj });
}

pub fn construct(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const root_count = vm.heap.temporary_roots.items.len;
    defer vm.heap.restoreRootCount(root_count);
    const builder = v.HeapValue.getParentPtr(v.Struct, args[0].heap_obj);
    const type_index: u32 = @intCast(v.HeapValue.getParentPtr(v.Struct, builder.get(0).heap_obj).get(0).int64);
    const session = try vm.heap.protectValue(builder.get(1), .@"struct");
    const values = try vm.heap.protectValue(builder.get(2), .array);
    const names = try vm.heap.protectValue(builder.get(3), .array);
    const keys = try vm.heap.protectValue(builder.get(4), .array);
    var construction = Construction{ .vm = vm, .type_index = type_index, .session = session, .values = values, .names = names, .keys = keys };
    const result = construction.build() catch |failure| {
        if (failure == error.OutOfMemory) {
            return failure;
        }
        return native.fail(std.meta.stringToEnum(ReflectError, @errorName(failure)) orelse .InvalidBuilderState);
    };
    return native.ok(try wrapValue(vm, type_index, vm.heap.protectedValue(result), vm.heap.protectedValue(session), @intCast(args[1].int64)));
}

const Construction = struct {
    vm: *VM,
    type_index: u32,
    session: usize,
    values: usize,
    names: usize,
    keys: usize,

    fn build(self: *Construction) anyerror!usize {
        const source = self.vm.program.reflection_types[self.type_index];
        switch (source.kind) {
            .Struct => {
                const result = try self.allocateStruct(source.object_descriptor.?);
                for (source.fields) |field| {
                    if (self.namedValue(field.name)) |index| {
                        const item = try self.checkedValue(self.values, index, field.type_index);
                        self.rootedStruct(result).set(field.slot, item);
                    } else if (field.default_function) |function| {
                        try self.queueDefault(result, function);
                    } else {
                        return error.MissingField;
                    }
                }
                return result;
            },
            .Array => {
                const count = self.rootedArray(self.values).len;
                const capacity = if (count == 0) 8 else count;
                const size = v.Array.calc_size(capacity);
                try self.vm.maybeCollectGarbage(size);
                const array = try v.Array.init(self.vm.getCurrentHeapAllocator(), self.vm.program.reflection_types[source.child.?].storage, capacity);
                self.vm.increaseHeapAllocationCount(size);
                const result = try self.vm.heap.protectValue(.{ .heap_obj = &array.obj }, .array);
                for (0..count) |index| {
                    const value = try self.checkedValue(self.values, index, source.child.?);
                    try self.rootedArray(result).append(value);
                }
                return result;
            },
            .Map => {
                const map = try self.vm.newMap(self.vm.program.reflection_types[source.key.?].storage, self.vm.program.reflection_types[source.child.?].storage);
                const result = try self.vm.heap.protectValue(map, .map);
                const count = self.rootedArray(self.values).len;
                for (0..count) |index| {
                    const scope = self.vm.heap.temporary_roots.items.len;
                    const key = try self.vm.heap.protectValue(try self.checkedValue(self.keys, index, source.key.?), self.vm.program.reflection_types[source.key.?].storage);
                    const value = try self.vm.heap.protectValue(try self.checkedValue(self.values, index, source.child.?), self.vm.program.reflection_types[source.child.?].storage);
                    const current = v.HeapValue.getParentPtr(v.Map, self.vm.heap.protectedValue(result).heap_obj);
                    if (current.get(self.vm.heap.protectedValue(key)) != null) {
                        return error.DuplicateAssignment;
                    }
                    try self.vm.setMapEntry(&self.vm.heap.temporary_roots.items[result].value, &self.vm.heap.temporary_roots.items[key].value, &self.vm.heap.temporary_roots.items[value].value);
                    self.vm.heap.restoreRootCount(scope);
                }
                return result;
            },
            .Nullable => {
                if (self.rootedArray(self.names).len == 1) {
                    return self.vm.heap.protectValue(v.NULL_VALUE, .nullable);
                }
                if (self.rootedArray(self.values).len != 1) {
                    return error.MissingField;
                }
                const value = try self.checkedValue(self.values, 0, source.child.?);
                const child = self.vm.program.reflection_types[source.child.?];
                const boxed = if (child.storage.isHeapType()) value else try self.vm.allocNullBox(value, child.storage);
                return self.vm.heap.protectValue(boxed, .nullable);
            },
            .Enum, .Union => {
                if (self.rootedArray(self.names).len != 1) {
                    return error.MissingField;
                }
                const name = v.HeapValue.getParentPtr(v.Bytes, self.rootedArray(self.names).getDataPtr()[0].heap_obj).slice();
                for (source.variants) |variant| {
                    if (!std.mem.eql(u8, variant.name, name)) continue;
                    if (source.kind == .Enum) {
                        return self.vm.heap.protectValue(.{ .int64 = variant.value }, .int64);
                    }
                    const size = v.Union.calc_size();
                    try self.vm.maybeCollectGarbage(size);
                    const object = try v.Union.init(self.vm.getCurrentHeapAllocator(), source.object_descriptor.?);
                    self.vm.increaseHeapAllocationCount(size);
                    object.set(@intCast(variant.value), try self.checkedValue(self.values, 0, variant.type_index.?));
                    return self.vm.heap.protectValue(.{ .heap_obj = &object.obj }, .@"union");
                }
                return error.UnknownVariant;
            },
            else => return error.UnsupportedType,
        }
    }

    fn allocateStruct(self: *Construction, descriptor: u16) !usize {
        const count = self.vm.program.struct_descriptors[descriptor].field_count;
        const size = v.Struct.calc_size(count);
        try self.vm.maybeCollectGarbage(size);
        const result = try v.Struct.init(self.vm.getCurrentHeapAllocator(), count, descriptor);
        self.vm.increaseHeapAllocationCount(size);
        return self.vm.heap.protectValue(.{ .heap_obj = &result.obj }, .@"struct");
    }

    fn rootedStruct(self: *Construction, root: usize) *v.Struct {
        return v.HeapValue.getParentPtr(v.Struct, self.vm.heap.protectedValue(root).heap_obj);
    }

    fn rootedArray(self: *Construction, root: usize) *v.Array {
        return v.HeapValue.getParentPtr(v.Array, self.vm.heap.protectedValue(root).heap_obj.followRelocated());
    }

    fn namedValue(self: *Construction, name: []const u8) ?usize {
        for (self.rootedArray(self.names).getDataPtr()[0..self.rootedArray(self.names).len], 0..) |entry, index| {
            const text = v.HeapValue.getParentPtr(v.Bytes, entry.heap_obj).slice();
            if (std.mem.eql(u8, name, text)) {
                return index;
            }
        }
        return null;
    }

    fn checkedValue(self: *Construction, root: usize, index: usize, expected: u32) !v.Value {
        const items = self.rootedArray(root);
        if (index >= items.len) {
            return error.MissingField;
        }
        const value = reflectedValue(items.getDataPtr()[index]);
        if (value.type_index.int64 != expected) {
            return error.TypeMismatch;
        }
        if (!isReady(value)) {
            const is_same_session = value.session.heap_obj == self.vm.heap.protectedValue(self.session).heap_obj;
            if (!is_same_session) {
                return error.InvalidBuilderState;
            }
        }
        return value.value;
    }

    fn queueDefault(self: *Construction, object: usize, function: u16) !void {
        const scope = self.vm.heap.temporary_roots.items.len;
        defer self.vm.heap.restoreRootCount(scope);
        const size = v.Closure.calc_size(1);
        try self.vm.maybeCollectGarbage(size);
        const closure = try v.Closure.init(self.vm.getCurrentHeapAllocator(), &self.vm.program.functions[function], 1);
        self.vm.increaseHeapAllocationCount(size);
        closure.captures()[0] = self.vm.heap.protectedValue(object);
        const callback = try self.vm.heap.protectValue(.{ .heap_obj = &closure.obj }, .function);
        const list = try self.vm.heap.protectValue(self.rootedStruct(self.session).get(1), .array);
        var callbacks = self.rootedArray(list);
        if (callbacks.len == callbacks.capacity) {
            callbacks = try self.vm.growArray(&self.vm.heap.temporary_roots.items[list].value);
        }
        self.rootedStruct(self.session).set(1, self.vm.heap.protectedValue(list));
        try callbacks.append(self.vm.heap.protectedValue(callback));
    }
};
