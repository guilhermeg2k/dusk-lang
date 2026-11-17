const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const format = @import("../../reflection_format.zig");
const reflection = @import("../../reflection.zig");
const native = @import("../native.zig");

pub const error_sets = .{reflection.ReflectError};

pub const bindings = native.BindingMap.initComptime(.{
    .{ "require_value", native.Binding{ .reflection = .{ .operation = .require_value, .func = &reflection.requireValue } } },
    .{ "can_construct", native.Binding{ .native = &reflection.canConstruct } },
    .{ "construct", native.Binding{ .reflection = .{ .operation = .result_descriptor, .func = &reflection.construct } } },
    .{ "append_handle", native.Binding{ .native = &reflection.appendHandle } },
    .{ "set_field", native.Binding{ .native = &reflection.setField } },
    .{ "check_input", native.Binding{ .native = &reflection.checkInput } },
    .{ "value", native.Binding{ .reflection = .{ .operation = .value, .func = &reflection.wrapTypedValue } } },
    .{ "view_as", native.Binding{ .reflection = .{ .operation = .cast, .func = &reflection.viewValue } } },
    .{ "null_of", native.Binding{ .reflection = .{ .operation = .result_descriptor, .func = &reflection.nullOf } } },
    .{ "enum_value", native.Binding{ .reflection = .{ .operation = .result_descriptor, .func = &reflection.enumValue } } },
    .{ "field", native.Binding{ .native = &reflection.readField } },
    .{ "element", native.Binding{ .native = &reflection.readElement } },
    .{ "length", native.Binding{ .native = &reflection.valueLength } },
    .{ "is_same_object", native.Binding{ .native = &reflection.areSameObjects } },
    .{ "is_null", native.Binding{ .native = &reflection.isNull } },
    .{ "unwrap", native.Binding{ .native = &reflection.unwrapValue } },
    .{ "variant_name", native.Binding{ .native = &reflection.variantName } },
    .{ "variant_value", native.Binding{ .native = &reflection.variantValue } },
    .{ "payload", native.Binding{ .native = &reflection.unionPayload } },
    .{ "map_key", native.Binding{ .native = &reflection.mapKey } },
    .{ "map_value", native.Binding{ .native = &reflection.mapValue } },
    .{ "type_of", native.Binding{ .reflection = .{ .operation = .type_of, .func = &typeOf } } },
    .{ "type_of_value", native.Binding{ .reflection = .{ .operation = .type_of, .func = &typeOf } } },
    .{ "name", native.Binding{ .native = &typeName } },
    .{ "kind", native.Binding{ .native = &typeKind } },
    .{ "is_same", native.Binding{ .native = &areSameTypes } },
    .{ "child_type", native.Binding{ .native = &childType } },
    .{ "member_count", native.Binding{ .native = &memberCount } },
    .{ "member_name", native.Binding{ .native = &memberName } },
    .{ "member_type", native.Binding{ .native = &memberType } },
    .{ "member_has_default", native.Binding{ .native = &memberHasDefault } },
    .{ "member_value", native.Binding{ .native = &memberValue } },
    .{ "member_index", native.Binding{ .native = &memberIndex } },
    .{ "has_private_fields", native.Binding{ .native = &hasPrivateFields } },
});

fn typeOf(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    return native.ok(try newType(vm, @intCast(args[args.len - 2].int64), @intCast(args[args.len - 1].int64)));
}

fn newType(vm: *VM, index: u32, wrapper_descriptor: u16) !v.Value {
    try vm.maybeCollectGarbage(v.Struct.calc_size(1));
    const result = try v.Struct.init(vm.getCurrentHeapAllocator(), 1, wrapper_descriptor);
    vm.increaseHeapAllocationCount(v.Struct.calc_size(1));
    result.set(0, .{ .int64 = index });
    return .{ .heap_obj = &result.obj };
}

fn typeIndex(value: v.Value) u32 {
    return @intCast(v.HeapValue.getParentPtr(v.Struct, value.heap_obj).get(0).int64);
}

fn descriptor(vm: *VM, value: v.Value) format.Descriptor {
    return vm.program.reflection_types[typeIndex(value)];
}

fn typeName(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    return native.ok(try vm.allocBytesValue(descriptor(vm, args[0]).name, .string));
}

fn typeKind(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    return native.ok(.{ .int64 = @intFromEnum(descriptor(vm, args[0]).kind) });
}

fn areSameTypes(_: *VM, args: []v.Value) anyerror!native.NativeResult {
    return native.ok(.{ .bool = typeIndex(args[0]) == typeIndex(args[1]) });
}

fn childType(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const source = descriptor(vm, args[0]);
    const index = if (args[1].bool) source.key.? else source.child.?;
    const wrapper = v.HeapValue.getParentPtr(v.Struct, args[0].heap_obj).descriptor_id;
    return native.ok(try newType(vm, index, wrapper));
}

fn memberCount(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const source = descriptor(vm, args[0]);
    return native.ok(.{ .int64 = @intCast(if (source.kind == .Struct) source.fields.len else source.variants.len) });
}

fn memberName(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const source = descriptor(vm, args[0]);
    const index: usize = @intCast(args[1].int64);
    const name = if (source.kind == .Struct) source.fields[index].name else source.variants[index].name;
    return native.ok(try vm.allocBytesValue(name, .string));
}

fn memberType(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const source = descriptor(vm, args[0]);
    const index: usize = @intCast(args[1].int64);
    const child = if (source.kind == .Struct) source.fields[index].type_index else source.variants[index].type_index.?;
    const wrapper = v.HeapValue.getParentPtr(v.Struct, args[0].heap_obj).descriptor_id;
    return native.ok(try newType(vm, child, wrapper));
}

fn memberHasDefault(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    return native.ok(.{ .bool = descriptor(vm, args[0]).fields[@intCast(args[1].int64)].has_default });
}

fn memberValue(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    return native.ok(.{ .int64 = descriptor(vm, args[0]).variants[@intCast(args[1].int64)].value });
}

fn memberIndex(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    const source = descriptor(vm, args[0]);
    const name = native.bytesArg(args, 1);
    if (source.kind == .Struct) {
        for (source.fields, 0..) |field, i| {
            if (std.mem.eql(u8, field.name, name)) {
                return native.ok(.{ .int64 = @intCast(i) });
            }
        }
        return native.ok(.{ .int64 = -1 });
    }
    for (source.variants, 0..) |variant, i| {
        if (std.mem.eql(u8, variant.name, name)) {
            return native.ok(.{ .int64 = @intCast(i) });
        }
    }
    return native.ok(.{ .int64 = -1 });
}

fn hasPrivateFields(vm: *VM, args: []v.Value) anyerror!native.NativeResult {
    return native.ok(.{ .bool = descriptor(vm, args[0]).has_private_fields });
}
