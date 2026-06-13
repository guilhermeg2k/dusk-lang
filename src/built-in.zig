const std = @import("std");
const bc = @import("bytecode.zig");

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
    return BuiltInFn{ .symbol = symbol, .bc_fn = .{ .func = &appendImpl, .num_args = 2 } };
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
    return BuiltInFn{ .symbol = symbol, .bc_fn = .{ .func = &lenImpl, .num_args = 1 } };
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
    impl: *const fn (args: []bc.Value) bc.Value,
    num_args: u8,
}{
    .{ .name = "echo", .impl = &echoImpl, .num_args = 1 },
    .{ .name = "append", .impl = &appendImpl, .num_args = 2 },
    .{ .name = "len", .impl = &lenImpl, .num_args = 1 },
    .{ .name = "assert", .impl = &assertImpl, .num_args = 1 },
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
            .kind = .{ .builtin = .{ .func = entry.impl, .num_args = entry.num_args } },
        };
    }
    return funcs;
}

fn echoImpl(args: []bc.Value) bc.Value {
    switch (args[0]) {
        .i_string => {
            std.debug.print("{s}\n", .{args[0].i_string});
        },
        .i_int => std.debug.print("{d}\n", .{args[0].i_int}),
        .i_float => std.debug.print("{d}\n", .{args[0].i_float}),
        .i_bool => std.debug.print("{}\n", .{args[0].i_bool}),
        else => std.debug.print("{any}\n", .{args[0]}),
    }
    return .{ .i_null = {} };
}

fn appendImpl(args: []bc.Value) bc.Value {
    _ = args;
    @panic("not implemented: append");
}

fn lenImpl(args: []bc.Value) bc.Value {
    _ = args;
    @panic("not implemented: len");
}

fn assertImpl(args: []bc.Value) bc.Value {
    if (!args[0].i_bool) {
        @panic("ASSERTION_FAILED");
    }
    return .{ .i_null = {} };
}

const BuiltInFn = struct {
    symbol: Symbol,
    bc_fn: bc.BuiltinFn,
    code: []const u8 = "",
};

const sema = @import("sema.zig");
const Symbol = sema.Symbol;
const TypeId = sema.TypeId;
const TypedIdentifier = sema.TypedIdentifier;
const TypeTable = sema.TypeTable;
