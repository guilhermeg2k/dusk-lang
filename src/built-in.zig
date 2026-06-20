const std = @import("std");
const bc = @import("bytecode.zig");
const v = @import("value.zig");

pub const BuiltIn = struct {
    const Self = @This();

    alloc: std.mem.Allocator,
    type_table: *TypeTable,

    void_type_id: TypeId,
    float_type_id: TypeId,
    int_type_id: TypeId,
    string_type_id: TypeId,
    boolean_type_id: TypeId,
    dynamic_type_id: TypeId,
    dynamic_array_type_id: TypeId,

    pub fn init(alloc: std.mem.Allocator, type_table: *TypeTable) !Self {
        const dynamic_type_id = type_table.primitives.get(.dynamic).?;
        return Self{
            .alloc = alloc,
            .type_table = type_table,
            .void_type_id = type_table.primitives.get(.void).?,
            .float_type_id = type_table.primitives.get(.float).?,
            .int_type_id = type_table.primitives.get(.int).?,
            .string_type_id = type_table.primitives.get(.string).?,
            .boolean_type_id = type_table.primitives.get(.boolean).?,
            .dynamic_type_id = dynamic_type_id,
            .dynamic_array_type_id = try type_table.getOrAddArray(dynamic_type_id),
        };
    }

    pub fn generate(self: *const Self) ![]BuiltInFn {
        const builtins = try self.alloc.alloc(BuiltInFn, builtin_factories.len);
        errdefer self.alloc.free(builtins);
        for (builtin_factories, 0..) |factory, i| {
            builtins[i] = try factory(self, i);
        }
        return builtins;
    }

    fn createFuncTypeId(self: *const Self, uid: usize, fn_identifier: []const u8, params: []const TypedIdentifier, return_type_id: TypeId) !TypeId {
        const params_copy = try self.alloc.alloc(TypedIdentifier, params.len);
        @memcpy(params_copy, params);
        const id = self.type_table.types.items.len;
        try self.type_table.types.append(self.alloc, .{
            .kind = .{
                .function = .{
                    .identifier = fn_identifier,
                    .uid = uid,
                    .params = params_copy,
                    .return_type_id = return_type_id,
                },
            },
            .nullable = false,
        });
        return id;
    }
};

fn echoFactory(self: *const BuiltIn, uid: usize) !BuiltInFn {
    var symbol = Symbol{
        .uid = uid,
        .identifier = "echo",
        .kind = .{ .function = {} },
        .type_id = undefined,
    };

    symbol.type_id = try self.createFuncTypeId(
        symbol.uid,
        symbol.identifier,
        &.{
            .{ .identifier = "msgs", .type_id = self.dynamic_type_id, .is_mut = false, .default_value = null },
        },
        self.void_type_id,
    );

    return BuiltInFn{ .symbol = symbol, .bc_fn = .{ .func = &echoImpl, .num_args = 1 } };
}

fn appendFactory(self: *const BuiltIn, uid: usize) !BuiltInFn {
    var symbol = Symbol{
        .uid = uid,
        .identifier = "append",
        .type_id = undefined,
        .kind = .{ .function = {} },
    };

    symbol.type_id = try self.createFuncTypeId(
        symbol.uid,
        symbol.identifier,
        &.{
            .{ .identifier = "array", .type_id = self.dynamic_array_type_id, .is_mut = true, .default_value = null },
            .{ .identifier = "value", .type_id = self.dynamic_type_id, .is_mut = false, .default_value = null },
        },
        self.void_type_id,
    );
    return BuiltInFn{ .symbol = symbol };
}

fn lenFactory(self: *const BuiltIn, uid: usize) !BuiltInFn {
    var symbol = Symbol{
        .uid = uid,
        .identifier = "len",
        .kind = .{ .function = {} },
        .type_id = undefined,
    };

    symbol.type_id = try self.createFuncTypeId(
        symbol.uid,
        symbol.identifier,
        &.{
            .{ .identifier = "array", .type_id = self.dynamic_array_type_id, .is_mut = false, .default_value = null },
        },
        self.int_type_id,
    );
    return BuiltInFn{ .symbol = symbol };
}

fn assertFactory(self: *const BuiltIn, uid: usize) !BuiltInFn {
    var symbol = Symbol{
        .uid = uid,
        .identifier = "assert",
        .kind = .{ .function = {} },
        .type_id = undefined,
    };
    symbol.type_id = try self.createFuncTypeId(symbol.uid, symbol.identifier, &.{
        .{ .identifier = "cond", .type_id = self.boolean_type_id, .is_mut = false, .default_value = null },
    }, self.void_type_id);
    return BuiltInFn{ .symbol = symbol, .bc_fn = .{ .func = &assertImpl, .num_args = 1 } };
}

const BuiltInFactory = *const fn (*const BuiltIn, usize) anyerror!BuiltInFn;

const builtin_factories = [_]BuiltInFactory{
    echoFactory,
    appendFactory,
    lenFactory,
    assertFactory,
};

const builtin_bytecode_registry = [_]struct {
    name: []const u8,
    kind: bc.FunctionKind,
}{
    .{ .name = "echo", .kind = .{ .host = .{ .func = &echoImpl, .num_args = 2 } } },
    .{ .name = "append", .kind = .{ .@"inline" = .{ .gen = &appendCodeGen, .num_args = 2 } } },
    .{ .name = "len", .kind = .{ .@"inline" = .{ .gen = &lenCodeGen, .num_args = 1 } } },
    .{ .name = "assert", .kind = .{ .host = .{ .func = &assertImpl, .num_args = 1 } } },
};

comptime {
    if (builtin_factories.len != builtin_bytecode_registry.len) {
        @compileError("builtin_factories and builtin_bytecode_registry must have same length");
    }
}

pub fn getBytecodeFunctions() [builtin_bytecode_registry.len]bc.Function {
    var funcs: [builtin_bytecode_registry.len]bc.Function = undefined;
    for (&funcs, builtin_bytecode_registry, 0..) |*f, entry, i| {
        f.* = .{
            .uid = i,
            .name = entry.name,
            .kind = entry.kind,
        };
    }
    return funcs;
}

fn printValue(value: v.Value, ty: v.ValueType) void {
    switch (ty) {
        .int64 => std.debug.print("{d}", .{value.int64}),
        .float64 => std.debug.print("{d}", .{value.float64}),
        .bool => std.debug.print("{}", .{value.bool}),
        .string => {
            const str = v.HeapValue.getParentPtr(v.String, value.heap_value);
            std.debug.print("{s}", .{str.slice()});
        },
        .null => std.debug.print("null", .{}),
        .array => {
            const array = v.HeapValue.getParentPtr(v.Array, value.heap_value);
            const data = array.getDataPtr();
            std.debug.print("[", .{});
            for (0..array.len) |i| {
                if (i > 0) std.debug.print(", ", .{});
                printValue(data[i], array.kind);
            }
            std.debug.print("]", .{});
        },
        .@"struct" => {
            const s = v.HeapValue.getParentPtr(v.Struct, value.heap_value);
            std.debug.print("{{ ", .{});
            for (0..s.field_count) |i| {
                if (i > 0) std.debug.print(", ", .{});
                std.debug.print("{d}", .{s.get(i).int64});
            }
            std.debug.print(" }}", .{});
        },
    }
}

fn echoImpl(args: []v.Value) v.Value {
    const ty: v.ValueType = @enumFromInt(@as(u8, @intCast(args[1].int64)));
    printValue(args[0], ty);
    std.debug.print("\n", .{});
    return .{ .null = {} };
}

fn appendCodeGen(_: u8, arg_start_reg: u8, alloc: std.mem.Allocator) ![]const bc.Instruction {
    var list: std.ArrayList(bc.Instruction) = .empty;
    try list.append(alloc, .{ .op = .ARRAY_APPEND, .a = arg_start_reg, .b = arg_start_reg + 1 });
    return try list.toOwnedSlice(alloc);
}

fn lenCodeGen(target_reg: u8, arg_start_reg: u8, alloc: std.mem.Allocator) ![]const bc.Instruction {
    var list: std.ArrayList(bc.Instruction) = .empty;
    try list.append(alloc, .{ .op = .ARRAY_LEN, .a = target_reg, .b = arg_start_reg });
    return try list.toOwnedSlice(alloc);
}

fn assertImpl(args: []v.Value) v.Value {
    if (!args[0].bool) {
        @panic("ASSERTION_FAILED");
    }
    return .{ .null = {} };
}

const BuiltInFn = struct {
    symbol: Symbol,
    bc_fn: ?bc.HostFn = null,
    code: []const u8 = "",
};

const sema = @import("sema.zig");
const Symbol = sema.Symbol;
const TypeId = sema.TypeId;
const TypedIdentifier = sema.TypedIdentifier;
const TypeTable = sema.TypeTable;
