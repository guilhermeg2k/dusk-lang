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

const type_slot = 0;
const bits_slot = 1;
const object_slot = 2;
const session_slot = 3;
const session_phase_slot = 0;
const session_handle_slot = 3;
const complete_phase = 2;
const builder_type_slot = 0;
const builder_session_slot = 1;
const builder_values_slot = 2;
const builder_names_slot = 3;

pub fn requireValue(_: *VM, _: []v.Value) anyerror!native.NativeResult {
    return native.ok(v.NULL_VALUE);
}

pub fn wrapTypedValue(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const type_index: u32 = @intCast(args[1].int64);
    const descriptor_id: u16 = @intCast(args[2].int64);
    return native.ok(try wrapValue(vm, type_index, args[0], v.NULL_VALUE, descriptor_id));
}

fn wrapValue(vm: *VM, type_index: u32, value: v.Value, session: v.Value, descriptor_id: u16) !v.Value {
    const root_count = vm.heap.temporary_roots.items.len;
    defer vm.heap.restoreRootCount(root_count);
    const kind = vm.program.reflection_types[type_index].storage;
    const value_root = try vm.heap.protectValue(value, kind);
    const session_root = try vm.heap.protectValue(session, .nullable);
    const object_root = try vm.heap.protectValue(v.NULL_VALUE, .nullable);
    const has_object = kind.isHeapType() and @as(u64, @bitCast(value)) != 0;
    if (has_object) {
        const cell_size = v.Cell.calc_size();
        try vm.maybeCollectGarbage(cell_size);
        const cell = try v.Cell.init(vm.getCurrentHeapAllocator(), vm.heap.protectedValue(value_root), kind);
        vm.increaseHeapAllocationCount(cell_size);
        vm.heap.temporary_roots.items[object_root].value = .{ .heap_obj = &cell.obj };
    }
    const field_count = vm.program.struct_descriptors[descriptor_id].field_count;
    const size = v.Struct.calc_size(field_count);
    try vm.maybeCollectGarbage(size);
    const handle = try v.Struct.init(vm.getCurrentHeapAllocator(), field_count, descriptor_id);
    vm.increaseHeapAllocationCount(size);
    const bits: v.Value = if (kind.isHeapType()) .{ .int64 = 0 } else vm.heap.protectedValue(value_root);
    handle.set(type_slot, .{ .int64 = type_index });
    handle.set(bits_slot, bits);
    handle.set(object_slot, vm.heap.protectedValue(object_root));
    handle.set(session_slot, vm.heap.protectedValue(session_root));
    return .{ .heap_obj = &handle.obj };
}

pub fn viewValue(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const handle = handleOf(args[0]);
    if (!isReady(handle)) {
        return native.fail(ReflectError.InvalidBuilderState);
    }
    if (handle.get(type_slot).int64 != args[1].int64) {
        return native.fail(ReflectError.TypeMismatch);
    }
    return native.ok(payloadOf(vm, handle));
}

pub fn nullOf(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const type_index = typeHandleIndex(args[0]);
    if (vm.program.reflection_types[type_index].kind != .Nullable) {
        return native.fail(ReflectError.TypeMismatch);
    }
    return native.ok(try wrapValue(vm, type_index, v.NULL_VALUE, v.NULL_VALUE, @intCast(args[1].int64)));
}

pub fn enumValue(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const type_index = typeHandleIndex(args[0]);
    const source = vm.program.reflection_types[type_index];
    if (source.kind != .Enum) {
        return native.fail(ReflectError.TypeMismatch);
    }
    const name = native.bytesArg(args, 1);
    for (source.variants) |variant| {
        if (!std.mem.eql(u8, variant.name, name)) continue;
        return native.ok(try wrapValue(vm, type_index, .{ .int64 = variant.value }, v.NULL_VALUE, @intCast(args[2].int64)));
    }
    return native.fail(ReflectError.UnknownVariant);
}

fn typeHandleIndex(type_handle: v.Value) u32 {
    return @intCast(v.HeapValue.getParentPtr(v.Struct, type_handle.heap_obj).get(0).int64);
}

fn handleOf(value: v.Value) *v.Struct {
    return v.HeapValue.getParentPtr(v.Struct, value.heap_obj);
}

fn isReady(handle: *const v.Struct) bool {
    const session = sessionStateOf(handle) orelse return true;
    return session.get(session_phase_slot).int64 == complete_phase;
}

fn sessionStateOf(handle: *const v.Struct) ?*v.Struct {
    const session = handle.get(session_slot);
    if (@as(u64, @bitCast(session)) == 0) {
        return null;
    }
    const cell = v.HeapValue.getParentPtr(v.Cell, session.heap_obj);
    return v.HeapValue.getParentPtr(v.Struct, cell.value.heap_obj);
}

fn payloadOf(vm: *VM, handle: *const v.Struct) v.Value {
    if (!handleDescriptor(vm, handle).storage.isHeapType()) {
        return handle.get(bits_slot);
    }
    const object = handle.get(object_slot);
    if (@as(u64, @bitCast(object)) == 0) {
        return v.NULL_VALUE;
    }
    return v.HeapValue.getParentPtr(v.Cell, object.heap_obj).value;
}

fn handleDescriptor(vm: *VM, handle: *const v.Struct) format.Descriptor {
    return vm.program.reflection_types[@intCast(handle.get(type_slot).int64)];
}

pub fn readField(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const handle = handleOf(args[0]);
    if (!isReady(handle)) {
        return native.fail(ReflectError.InvalidBuilderState);
    }
    const source = handleDescriptor(vm, handle);
    if (source.kind != .Struct) {
        return native.fail(ReflectError.TypeMismatch);
    }
    const name = native.bytesArg(args, 1);
    for (source.fields) |field| {
        if (!std.mem.eql(u8, name, field.name)) continue;
        const object = v.HeapValue.getParentPtr(v.Struct, payloadOf(vm, handle).heap_obj);
        return native.ok(try wrapValue(vm, field.type_index, object.get(field.slot), handle.get(session_slot), handle.descriptor_id));
    }
    return native.fail(ReflectError.UnknownField);
}

pub fn readElement(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const handle = handleOf(args[0]);
    if (!isReady(handle)) {
        return native.fail(ReflectError.InvalidBuilderState);
    }
    const source = handleDescriptor(vm, handle);
    if (source.kind != .Array) {
        return native.fail(ReflectError.TypeMismatch);
    }
    const array = v.HeapValue.getParentPtr(v.Array, payloadOf(vm, handle).heap_obj.followRelocated());
    const index = args[1].int64;
    const is_in_range = index >= 0 and index < array.len;
    if (!is_in_range) {
        return native.fail(ReflectError.OutOfRange);
    }
    return native.ok(try wrapValue(vm, source.child.?, array.getDataPtr()[@intCast(index)], handle.get(session_slot), handle.descriptor_id));
}

pub fn valueLength(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const handle = handleOf(args[0]);
    if (!isReady(handle)) {
        return native.fail(ReflectError.InvalidBuilderState);
    }
    const payload = payloadOf(vm, handle);
    const length = switch (handleDescriptor(vm, handle).kind) {
        .Array => v.HeapValue.getParentPtr(v.Array, payload.heap_obj.followRelocated()).len,
        .Map => v.HeapValue.getParentPtr(v.Map, payload.heap_obj).count(),
        else => return native.fail(ReflectError.TypeMismatch),
    };
    return native.ok(.{ .int64 = @intCast(length) });
}

pub fn areSameObjects(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const left = handleOf(args[0]);
    const right = handleOf(args[1]);
    const are_ready = isReady(left) and isReady(right);
    if (!are_ready) {
        return native.fail(ReflectError.InvalidBuilderState);
    }
    const left_payload = payloadOf(vm, left);
    const right_payload = payloadOf(vm, right);
    const can_compare_identity = left.get(type_slot).int64 == right.get(type_slot).int64 and handleDescriptor(vm, left).storage.isHeapType() and @as(u64, @bitCast(left_payload)) != 0 and @as(u64, @bitCast(right_payload)) != 0;
    if (!can_compare_identity) {
        return native.ok(.{ .bool = false });
    }
    return native.ok(.{ .bool = left_payload.heap_obj.followRelocated() == right_payload.heap_obj.followRelocated() });
}

pub fn isNull(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const handle = handleOf(args[0]);
    if (!isReady(handle)) {
        return native.fail(ReflectError.InvalidBuilderState);
    }
    const has_null_value = handleDescriptor(vm, handle).kind == .Nullable and @as(u64, @bitCast(payloadOf(vm, handle))) == 0;
    return native.ok(.{ .bool = has_null_value });
}

pub fn unwrapValue(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const handle = handleOf(args[0]);
    if (!isReady(handle)) {
        return native.fail(ReflectError.InvalidBuilderState);
    }
    const source = handleDescriptor(vm, handle);
    const payload = payloadOf(vm, handle);
    const has_value = source.kind == .Nullable and @as(u64, @bitCast(payload)) != 0;
    if (!has_value) {
        return native.fail(ReflectError.TypeMismatch);
    }
    const child = vm.program.reflection_types[source.child.?];
    const value = if (child.storage.isHeapType()) payload else v.HeapValue.getParentPtr(v.NullBox, payload.heap_obj).value;
    return native.ok(try wrapValue(vm, source.child.?, value, handle.get(session_slot), handle.descriptor_id));
}

pub fn variantName(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const handle = handleOf(args[0]);
    if (!isReady(handle)) {
        return native.fail(ReflectError.InvalidBuilderState);
    }
    const source = handleDescriptor(vm, handle);
    const payload = payloadOf(vm, handle);
    const variant_value = switch (source.kind) {
        .Enum => payload.int64,
        .Union => v.HeapValue.getParentPtr(v.Union, payload.heap_obj).active_tag,
        else => return native.fail(ReflectError.TypeMismatch),
    };
    for (source.variants) |variant| {
        if (variant.value != variant_value) continue;
        return native.ok(try vm.allocBytesValue(variant.name, .string));
    }
    return native.fail(ReflectError.UnknownVariant);
}

pub fn variantValue(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const handle = handleOf(args[0]);
    if (!isReady(handle)) {
        return native.fail(ReflectError.InvalidBuilderState);
    }
    if (handleDescriptor(vm, handle).kind != .Enum) {
        return native.fail(ReflectError.TypeMismatch);
    }
    return native.ok(payloadOf(vm, handle));
}

pub fn unionPayload(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const handle = handleOf(args[0]);
    if (!isReady(handle)) {
        return native.fail(ReflectError.InvalidBuilderState);
    }
    const source = handleDescriptor(vm, handle);
    if (source.kind != .Union) {
        return native.fail(ReflectError.TypeMismatch);
    }
    const object = v.HeapValue.getParentPtr(v.Union, payloadOf(vm, handle).heap_obj);
    for (source.variants) |variant| {
        if (variant.value != object.active_tag) continue;
        return native.ok(try wrapValue(vm, variant.type_index.?, object.get(object.active_tag), handle.get(session_slot), handle.descriptor_id));
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
    const handle = handleOf(args[0]);
    if (!isReady(handle)) {
        return native.fail(ReflectError.InvalidBuilderState);
    }
    const source = handleDescriptor(vm, handle);
    if (source.kind != .Map) {
        return native.fail(ReflectError.TypeMismatch);
    }
    const map = v.HeapValue.getParentPtr(v.Map, payloadOf(vm, handle).heap_obj);
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
            return native.ok(try wrapValue(vm, child, value, handle.get(session_slot), handle.descriptor_id));
        }
        position += 1;
    }
    unreachable;
}

pub fn canConstruct(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const index: u32 = @intCast(v.HeapValue.getParentPtr(v.Struct, args[0].heap_obj).get(0).int64);
    return native.ok(.{ .bool = vm.program.reflection_types[index].is_constructible });
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
    const session_handle = try sessionHandle(vm, session);
    return native.ok(try wrapValue(vm, type_index, vm.heap.protectedValue(result), session_handle, @intCast(args[1].int64)));
}

fn sessionHandle(vm: *VM, session_root: usize) !v.Value {
    const existing = v.HeapValue.getParentPtr(v.Struct, vm.heap.protectedValue(session_root).heap_obj).get(session_handle_slot);
    if (@as(u64, @bitCast(existing)) != 0) {
        return existing;
    }
    const size = v.Cell.calc_size();
    try vm.maybeCollectGarbage(size);
    const cell = try v.Cell.init(vm.getCurrentHeapAllocator(), vm.heap.protectedValue(session_root), .@"struct");
    vm.increaseHeapAllocationCount(size);
    const handle: v.Value = .{ .heap_obj = &cell.obj };
    v.HeapValue.getParentPtr(v.Struct, vm.heap.protectedValue(session_root).heap_obj).set(session_handle_slot, handle);
    return handle;
}

pub fn appendHandle(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    var array = v.HeapValue.getParentPtr(v.Array, args[0].heap_obj.followRelocated());
    if (array.len == array.capacity) {
        array = try vm.growArray(&args[0]);
    }
    try array.append(args[1]);
    return native.ok(v.NULL_VALUE);
}

pub fn setField(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const builder = v.HeapValue.getParentPtr(v.Struct, args[0].heap_obj);
    const target = vm.program.reflection_types[typeHandleIndex(builder.get(builder_type_slot))];
    if (target.kind != .Struct) {
        return native.fail(ReflectError.TypeMismatch);
    }
    const name = native.bytesArg(args, 1);
    const field = for (target.fields) |field| {
        if (std.mem.eql(u8, field.name, name)) {
            break field;
        }
    } else {
        return native.fail(ReflectError.UnknownField);
    };
    if (inputError(builder, handleOf(args[2]), field.type_index)) |failure| {
        return native.fail(failure);
    }
    const names = v.HeapValue.getParentPtr(v.Array, builder.get(builder_names_slot).heap_obj.followRelocated());
    for (names.getDataPtr()[0..names.len]) |existing| {
        if (std.mem.eql(u8, v.HeapValue.getParentPtr(v.Bytes, existing.heap_obj).slice(), name)) {
            return native.fail(ReflectError.DuplicateAssignment);
        }
    }
    const root_count = vm.heap.temporary_roots.items.len;
    defer vm.heap.restoreRootCount(root_count);
    const builder_root = try vm.heap.protectValue(args[0], .@"struct");
    const name_root = try vm.heap.protectValue(args[1], .string);
    const input_root = try vm.heap.protectValue(args[2], .@"struct");
    try appendBuilderItem(vm, builder_root, builder_names_slot, name_root);
    try appendBuilderItem(vm, builder_root, builder_values_slot, input_root);
    return native.ok(v.NULL_VALUE);
}

pub fn checkInput(_: *VM, args: []v.Value) anyerror!native.NativeResult {
    const builder = v.HeapValue.getParentPtr(v.Struct, args[0].heap_obj);
    if (inputError(builder, handleOf(args[1]), typeHandleIndex(args[2]))) |failure| {
        return native.fail(failure);
    }
    return native.ok(v.NULL_VALUE);
}

fn inputError(builder: *const v.Struct, input: *const v.Struct, expected: u32) ?ReflectError {
    if (input.get(type_slot).int64 != expected) {
        return ReflectError.TypeMismatch;
    }
    if (isReady(input)) {
        return null;
    }
    const is_same_session = &sessionStateOf(input).?.obj == builder.get(builder_session_slot).heap_obj;
    if (!is_same_session) {
        return ReflectError.InvalidBuilderState;
    }
    return null;
}

fn appendBuilderItem(vm: *VM, builder_root: usize, slot: usize, item_root: usize) !void {
    const scope = vm.heap.temporary_roots.items.len;
    defer vm.heap.restoreRootCount(scope);
    const list = try vm.heap.protectValue(rootedBuilder(vm, builder_root).get(slot), .array);
    var array = v.HeapValue.getParentPtr(v.Array, vm.heap.protectedValue(list).heap_obj.followRelocated());
    if (array.len == array.capacity) {
        array = try vm.growArray(&vm.heap.temporary_roots.items[list].value);
        rootedBuilder(vm, builder_root).set(slot, vm.heap.protectedValue(list));
    }
    try array.append(vm.heap.protectedValue(item_root));
}

fn rootedBuilder(vm: *VM, builder_root: usize) *v.Struct {
    return v.HeapValue.getParentPtr(v.Struct, vm.heap.protectedValue(builder_root).heap_obj);
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
        const handle = handleOf(items.getDataPtr()[index]);
        if (handle.get(type_slot).int64 != expected) {
            return error.TypeMismatch;
        }
        if (!isReady(handle)) {
            const is_same_session = &sessionStateOf(handle).?.obj == self.vm.heap.protectedValue(self.session).heap_obj;
            if (!is_same_session) {
                return error.InvalidBuilderState;
            }
        }
        return payloadOf(self.vm, handle);
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
