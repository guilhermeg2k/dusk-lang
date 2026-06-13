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

    pub fn generate(self: *const Self) ![7]BuiltInFn {
        return [_]BuiltInFn{
            try self.echo(),
            try self.append(),
            try self.len(),
            try self.floor(),
            try self.concat(),
            try self.stringify(),
            try self.assert(),
        };
    }

    fn echo(self: *const Self) !BuiltInFn {
        var symbol = Symbol{
            .uid = 0,
            .identifier = "echo",
            .kind = .{ .function = {} },
            .type_id = undefined,
        };

        symbol.type_id = try self.createFuncTypeId(symbol.uid, symbol.identifier, &.{
            .{ .identifier = "msgs", .type_id = self.dynamic_type_id, .is_mut = false, .default_value = null },
        }, self.void_type_id);

        return BuiltInFn{
            .symbol = symbol,
            .bc_fn = .{ .func = &echoImpl, .num_args = 1 },
        };
    }

    fn append(self: *const Self) !BuiltInFn {
        var symbol = Symbol{
            .uid = 1,
            .identifier = "append",
            .type_id = undefined,
            .kind = .{ .function = {} },
        };

        symbol.type_id = try self.createFuncTypeId(symbol.uid, symbol.identifier, &.{
            .{ .identifier = "array", .type_id = self.dynamic_array_type_id, .is_mut = true, .default_value = null },
            .{ .identifier = "value", .type_id = self.dynamic_type_id, .is_mut = false, .default_value = null },
        }, self.void_type_id);

        return BuiltInFn{
            .symbol = symbol,
            .bc_fn = .{ .func = &appendImpl, .num_args = 2 },
        };
    }

    fn len(self: *const Self) !BuiltInFn {
        var symbol = Symbol{
            .uid = 2,
            .identifier = "len",
            .kind = .{ .function = {} },
            .type_id = undefined,
        };

        symbol.type_id = try self.createFuncTypeId(symbol.uid, symbol.identifier, &.{
            .{
                .identifier = "array",
                .type_id = self.dynamic_array_type_id,
                .is_mut = false,
                .default_value = null,
            },
        }, self.int_type_id);

        return BuiltInFn{
            .symbol = symbol,
            .bc_fn = .{
                .func = &lenImpl,
                .num_args = 1,
            },
        };
    }

    fn floor(self: *const Self) !BuiltInFn {
        var symbol = Symbol{
            .uid = 3,
            .identifier = "floor",
            .kind = .{ .function = {} },
            .type_id = undefined,
        };

        symbol.type_id = try self.createFuncTypeId(symbol.uid, symbol.identifier, &.{
            .{ .identifier = "n", .type_id = self.int_type_id, .is_mut = false, .default_value = null },
        }, self.int_type_id);

        return BuiltInFn{
            .symbol = symbol,
            .bc_fn = .{ .func = &floorImpl, .num_args = 1 },
        };
    }

    fn concat(self: *const Self) !BuiltInFn {
        var symbol = Symbol{
            .uid = 4,
            .identifier = "concat",
            .kind = .{ .function = {} },
            .type_id = undefined,
        };

        symbol.type_id = try self.createFuncTypeId(symbol.uid, symbol.identifier, &.{
            .{ .identifier = "str1", .type_id = self.string_type_id, .is_mut = false, .default_value = null },
            .{ .identifier = "str2", .type_id = self.string_type_id, .is_mut = false, .default_value = null },
        }, self.string_type_id);

        return BuiltInFn{
            .symbol = symbol,
            .bc_fn = .{ .func = &concatImpl, .num_args = 2 },
        };
    }

    fn stringify(self: *const Self) !BuiltInFn {
        var symbol = Symbol{
            .uid = 5,
            .identifier = "stringify",
            .kind = .{ .function = {} },
            .type_id = undefined,
        };

        symbol.type_id = try self.createFuncTypeId(symbol.uid, symbol.identifier, &.{
            .{ .identifier = "obj", .type_id = self.dynamic_type_id, .is_mut = false, .default_value = null },
        }, self.string_type_id);

        return BuiltInFn{
            .symbol = symbol,
            .bc_fn = .{ .func = &stringifyImpl, .num_args = 1 },
        };
    }

    fn assert(self: *const Self) !BuiltInFn {
        var symbol = Symbol{
            .uid = 6,
            .identifier = "assert",
            .kind = .{ .function = {} },
            .type_id = undefined,
        };

        symbol.type_id = try self.createFuncTypeId(symbol.uid, symbol.identifier, &.{
            .{ .identifier = "cond", .type_id = self.boolean_type_id, .is_mut = false, .default_value = null },
        }, self.void_type_id);

        return BuiltInFn{
            .symbol = symbol,
            .bc_fn = .{ .func = &assertImpl, .num_args = 1 },
        };
    }

    fn createFuncTypeId(self: *const Self, uid: usize, fn_identifier: []const u8, params: []const TypedIdentifier, return_type_id: TypeId) !TypeId {
        const params_copy = try self.alloc.alloc(TypedIdentifier, params.len);
        @memcpy(params_copy, params);
        const id = self.type_table.types.items.len;
        try self.type_table.types.append(self.alloc, .{
            .kind = .{ .function = .{
                .identifier = fn_identifier,
                .uid = uid,
                .params = params_copy,
                .return_type_id = return_type_id,
            } },
            .nullable = false,
        });
        return id;
    }
};

pub fn getBytecodeFunctions() [7]bc.Function {
    return [_]bc.Function{
        .{ .uid = 0, .name = "echo", .kind = .{ .builtin = .{ .func = &echoImpl, .num_args = 1 } } },
        .{ .uid = 1, .name = "append", .kind = .{ .builtin = .{ .func = &appendImpl, .num_args = 2 } } },
        .{ .uid = 2, .name = "len", .kind = .{ .builtin = .{ .func = &lenImpl, .num_args = 1 } } },
        .{ .uid = 3, .name = "floor", .kind = .{ .builtin = .{ .func = &floorImpl, .num_args = 1 } } },
        .{ .uid = 4, .name = "concat", .kind = .{ .builtin = .{ .func = &concatImpl, .num_args = 2 } } },
        .{ .uid = 5, .name = "stringify", .kind = .{ .builtin = .{ .func = &stringifyImpl, .num_args = 1 } } },
        .{ .uid = 6, .name = "assert", .kind = .{ .builtin = .{ .func = &assertImpl, .num_args = 1 } } },
    };
}

fn echoImpl(args: []bc.Value) bc.Value {
    switch (args[0]) {
        .i_string => {
            std.debug.print("{s}\n", .{args[0].i_string});
        },
        else => std.debug.print("{any}", .{args[0]}),
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

fn floorImpl(args: []bc.Value) bc.Value {
    return .{ .i_int = @intFromFloat(@floor(args[0].i_float)) };
}

fn concatImpl(args: []bc.Value) bc.Value {
    _ = args;
    @panic("not implemented: concat");
}

fn stringifyImpl(args: []bc.Value) bc.Value {
    _ = args;
    @panic("not implemented: stringify");
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
