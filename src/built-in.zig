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
        const symbol = try Symbol.init(self.alloc, .{
            .uid = 0,
            .identifier = "echo",
            .is_mut = false,
            .type_id = undefined,
        });

        symbol.type_id = try self.createFuncTypeId(symbol, &.{
            .{ .identifier = "msgs", .type_id = self.dynamic_type_id, .is_mut = false, .default_value = null },
        }, self.void_type_id);

        const code = try std.fmt.allocPrint(
            self.alloc,
            "function {s}_{d}(...msg) {{echo(...msg);}}\n",
            .{ symbol.identifier, symbol.uid },
        );

        return BuiltInFn{ .symbol = symbol, .code = code };
    }

    fn append(self: *const Self) !BuiltInFn {
        const symbol = try Symbol.init(self.alloc, .{
            .uid = 1,
            .identifier = "append",
            .type_id = undefined,
            .is_mut = false,
        });

        symbol.type_id = try self.createFuncTypeId(symbol, &.{
            .{ .identifier = "array", .type_id = self.dynamic_array_type_id, .is_mut = true, .default_value = null },
            .{ .identifier = "value", .type_id = self.dynamic_type_id, .is_mut = false, .default_value = null },
        }, self.void_type_id);

        const code = try std.fmt.allocPrint(
            self.alloc,
            "function {s}_{d}(arr, item) {{return arr.push(item);}}\n",
            .{ symbol.identifier, symbol.uid },
        );

        return BuiltInFn{ .symbol = symbol, .code = code };
    }

    fn len(self: *const Self) !BuiltInFn {
        const symbol = try Symbol.init(self.alloc, .{
            .uid = 2,
            .identifier = "len",
            .is_mut = false,
            .type_id = undefined,
        });

        symbol.type_id = try self.createFuncTypeId(symbol, &.{
            .{ .identifier = "array", .type_id = self.dynamic_array_type_id, .is_mut = false, .default_value = null },
        }, self.int_type_id);

        const code = try std.fmt.allocPrint(
            self.alloc,
            "function {s}_{d}(arr) {{return arr.length;}}\n",
            .{ symbol.identifier, symbol.uid },
        );

        return BuiltInFn{ .symbol = symbol, .code = code };
    }

    fn floor(self: *const Self) !BuiltInFn {
        const symbol = try Symbol.init(self.alloc, .{
            .uid = 3,
            .identifier = "floor",
            .is_mut = false,
            .type_id = undefined,
        });

        symbol.type_id = try self.createFuncTypeId(symbol, &.{
            .{ .identifier = "n", .type_id = self.int_type_id, .is_mut = false, .default_value = null },
        }, self.int_type_id);

        const code = try std.fmt.allocPrint(
            self.alloc,
            "function {s}_{d}(n) {{return Math.floor(n)}}\n",
            .{ symbol.identifier, symbol.uid },
        );

        return BuiltInFn{ .symbol = symbol, .code = code };
    }

    fn concat(self: *const Self) !BuiltInFn {
        const symbol = try Symbol.init(self.alloc, .{
            .uid = 4,
            .identifier = "concat",
            .is_mut = false,
            .type_id = undefined,
        });

        symbol.type_id = try self.createFuncTypeId(symbol, &.{
            .{ .identifier = "str1", .type_id = self.string_type_id, .is_mut = false, .default_value = null },
            .{ .identifier = "str2", .type_id = self.string_type_id, .is_mut = false, .default_value = null },
        }, self.string_type_id);

        const code = try std.fmt.allocPrint(
            self.alloc,
            "function {s}_{d}(s1, s2) {{return s1+s2;}}\n",
            .{ symbol.identifier, symbol.uid },
        );

        return BuiltInFn{ .symbol = symbol, .code = code };
    }

    fn stringify(self: *const Self) !BuiltInFn {
        const symbol = try Symbol.init(self.alloc, .{
            .uid = 5,
            .identifier = "stringify",
            .is_mut = false,
            .type_id = undefined,
        });

        symbol.type_id = try self.createFuncTypeId(symbol, &.{
            .{ .identifier = "obj", .type_id = self.dynamic_type_id, .is_mut = false, .default_value = null },
        }, self.string_type_id);

        const code = try std.fmt.allocPrint(
            self.alloc,
            "function {s}_{d}(obj) {{try {{return JSON.stringify(obj);}} catch {{return \"INVALID JSON\";}};}}\n",
            .{ symbol.identifier, symbol.uid },
        );

        return BuiltInFn{ .symbol = symbol, .code = code };
    }

    fn assert(self: *const Self) !BuiltInFn {
        const symbol = try Symbol.init(self.alloc, .{
            .uid = 6,
            .identifier = "assert",
            .is_mut = false,
            .type_id = undefined,
        });

        symbol.type_id = try self.createFuncTypeId(symbol, &.{
            .{ .identifier = "cond", .type_id = self.boolean_type_id, .is_mut = false, .default_value = null },
        }, self.void_type_id);

        const code = try std.fmt.allocPrint(
            self.alloc,
            "function {s}_{d}(cond) {{if(cond==false) throw 'ASSERTION_FAILED'}}\n",
            .{ symbol.identifier, symbol.uid },
        );

        return BuiltInFn{ .symbol = symbol, .code = code };
    }

    fn createFuncTypeId(self: *const Self, symbol: *Symbol, params: []const TypedIdentifier, return_type_id: TypeId) !TypeId {
        const params_copy = try self.alloc.alloc(TypedIdentifier, params.len);
        @memcpy(params_copy, params);
        const id = self.type_table.types.items.len;
        try self.type_table.types.append(self.alloc, .{
            .kind = .{ .function = .{
                .symbol = symbol,
                .params = params_copy,
                .return_type_id = return_type_id,
            } },
            .nullable = false,
        });
        return id;
    }
};

const BuiltInFn = struct {
    symbol: *Symbol,
    code: []const u8,
};

const sema = @import("sema.zig");
const Symbol = sema.Symbol;
const TypeId = sema.TypeId;
const TypedIdentifier = sema.TypedIdentifier;
const TypeTable = sema.TypeTable;
const std = @import("std");
