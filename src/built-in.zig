const std = @import("std");
const bc = @import("bytecode.zig");
const v = @import("value.zig");
const sema = @import("sema.zig");
const tt = @import("type_table.zig");

const Symbol = sema.Symbol;
const TypeId = tt.TypeId;
const TypedIdentifier = tt.TypedIdentifier;
const TypeTable = tt.TypeTable;

//note: temporary as we don't impl modules
pub const BuiltIn = struct {
    const Self = @This();

    const BuiltInFn = struct {
        symbol: Symbol,
        bc_fn: ?bc.HostFn = null,
    };

    const BuiltInFactory = *const fn (*const Self, usize) anyerror!BuiltInFn;

    const StrType = enum { string, int, boolean, nullable_int, string_array };

    const StringOpSpec = struct {
        name: []const u8,
        op: bc.StringOp,
        params: []const struct { []const u8, StrType },
        result: StrType,
        can_fail: bool = false,
    };

    const string_ops = [_]StringOpSpec{
        .{ .name = "str_find", .op = .find, .params = &.{ .{ "text", .string }, .{ "needle", .string } }, .result = .nullable_int },
        .{ .name = "str_contains", .op = .contains, .params = &.{ .{ "text", .string }, .{ "needle", .string } }, .result = .boolean },
        .{ .name = "str_starts_with", .op = .starts_with, .params = &.{ .{ "text", .string }, .{ "prefix", .string } }, .result = .boolean },
        .{ .name = "str_ends_with", .op = .ends_with, .params = &.{ .{ "text", .string }, .{ "suffix", .string } }, .result = .boolean },
        .{ .name = "str_count", .op = .count, .params = &.{ .{ "text", .string }, .{ "needle", .string } }, .result = .int, .can_fail = true },
        .{ .name = "str_trim", .op = .trim, .params = &.{.{ "text", .string }}, .result = .string },
        .{ .name = "str_split", .op = .split, .params = &.{ .{ "text", .string }, .{ "separator", .string } }, .result = .string_array, .can_fail = true },
        .{ .name = "str_replace", .op = .replace, .params = &.{ .{ "text", .string }, .{ "old", .string }, .{ "new", .string } }, .result = .string, .can_fail = true },
        .{ .name = "str_upper", .op = .upper, .params = &.{.{ "text", .string }}, .result = .string },
        .{ .name = "str_lower", .op = .lower, .params = &.{.{ "text", .string }}, .result = .string },
        .{ .name = "str_slice", .op = .slice, .params = &.{ .{ "text", .string }, .{ "start", .int }, .{ "stop", .int } }, .result = .string, .can_fail = true },
        .{ .name = "str_repeat", .op = .repeat, .params = &.{ .{ "text", .string }, .{ "count", .int } }, .result = .string, .can_fail = true },
    };

    const builtin_factories = [_]BuiltInFactory{
        echoFactory,
        appendFactory,
        lenFactory,
        assertFactory,
        bufferFromFactory,
        stringFromFactory,
        strEqualFactory,
        strCharCountFactory,
        strConcatFactory,
        strJoinFactory,
        strToIntFactory,
        strToFloatFactory,
    } ++ stringOpFactories();

    const RegistryEntry = struct {
        name: []const u8,
        kind: bc.FunctionKind,
    };

    const builtin_bytecode_registry = [_]RegistryEntry{
        .{ .name = "echo", .kind = .{ .host = .{ .func = &echoImpl, .num_args = 2 } } },
        .{ .name = "append", .kind = .{ .@"inline" = .{ .gen = &appendCodeGen, .num_args = 2 } } },
        .{ .name = "len", .kind = .{ .@"inline" = .{ .gen = &lenCodeGen, .num_args = 1 } } },
        .{ .name = "assert", .kind = .{ .host = .{ .func = &assertImpl, .num_args = 1 } } },
        .{ .name = "buffer_from", .kind = .{ .@"inline" = .{ .gen = &bufferFromCodeGen, .num_args = 1 } } },
        .{ .name = "string_from", .kind = .{ .@"inline" = .{ .gen = &stringFromCodeGen, .num_args = 1 } } },
        .{ .name = "str_equal", .kind = .{ .@"inline" = .{ .gen = &strEqualCodeGen, .num_args = 2 } } },
        .{ .name = "str_char_count", .kind = .{ .host = .{ .func = &strCharCountImpl, .num_args = 1 } } },
        .{ .name = "str_concat", .kind = .{ .@"inline" = .{ .gen = &strConcatCodeGen, .num_args = 2 } } },
        .{ .name = "str_join", .kind = .{ .@"inline" = .{ .gen = &strJoinCodeGen, .num_args = 2 } } },
        .{ .name = "str_to_int", .kind = .{ .@"inline" = .{ .gen = &strToIntCodeGen, .num_args = 1 } } },
        .{ .name = "str_to_float", .kind = .{ .@"inline" = .{ .gen = &strToFloatCodeGen, .num_args = 1 } } },
    } ++ stringOpRegistry();

    fn stringOpFactories() [string_ops.len]BuiltInFactory {
        var factories: [string_ops.len]BuiltInFactory = undefined;
        for (string_ops, 0..) |spec, i| factories[i] = stringOpFactory(spec);
        return factories;
    }

    fn stringOpRegistry() [string_ops.len]RegistryEntry {
        var entries: [string_ops.len]RegistryEntry = undefined;
        for (string_ops, 0..) |spec, i| {
            entries[i] = .{ .name = spec.name, .kind = .{ .@"inline" = .{ .gen = stringOpCodeGen(spec.op), .num_args = @intCast(spec.params.len) } } };
        }
        return entries;
    }

    fn stringOpFactory(comptime spec: StringOpSpec) BuiltInFactory {
        return struct {
            fn factory(self: *const Self, uid: usize) !BuiltInFn {
                var params: [spec.params.len]TypedIdentifier = undefined;
                for (spec.params, 0..) |param, i| {
                    params[i] = .{ .identifier = param[0], .type_id = try self.strTypeId(param[1]), .is_mut = false, .default_value = null };
                }
                const result = try self.strTypeId(spec.result);
                const return_type_id = if (spec.can_fail) try self.errorUnionTypeId(string_error_index, result) else result;
                var symbol = Symbol{ .uid = uid, .identifier = spec.name, .kind = .{ .function = {} }, .type_id = undefined };
                symbol.type_id = try self.createFuncTypeId(uid, spec.name, &params, return_type_id);
                return BuiltInFn{ .symbol = symbol };
            }
        }.factory;
    }

    fn stringOpCodeGen(comptime op: bc.StringOp) *const fn (u8, u8, std.mem.Allocator) anyerror![]const bc.Instruction {
        return struct {
            fn gen(target_reg: u8, arg_start_reg: u8, alloc: std.mem.Allocator) ![]const bc.Instruction {
                var list: std.ArrayList(bc.Instruction) = .empty;
                try list.append(alloc, .{ .op = .STR_OP, .a = target_reg, .b = arg_start_reg, .c = @intFromEnum(op) });
                return try list.toOwnedSlice(alloc);
            }
        }.gen;
    }

    fn strTypeId(self: *const Self, str_type: StrType) !TypeId {
        return switch (str_type) {
            .string => self.string_type_id,
            .int => self.int_type_id,
            .boolean => self.boolean_type_id,
            .nullable_int => try self.type_table.getOrAddNullable(self.int_type_id),
            .string_array => try self.type_table.getOrAddArray(self.string_type_id),
        };
    }

    comptime {
        if (builtin_factories.len != builtin_bytecode_registry.len) {
            @compileError("builtin_factories and builtin_bytecode_registry must have same length");
        }
    }

    var echo_writer: ?*std.Io.Writer = null;

    const ErrorSetSpec = struct { name: []const u8, variants: []const []const u8 };

    pub const error_sets = [_]ErrorSetSpec{
        .{ .name = "ConversionError", .variants = &.{ "Invalid", "Overflow" } },
        .{ .name = "StringError", .variants = &.{ "OutOfRange", "EmptyPattern", "NegativeCount" } },
    };
    const conversion_error_index = 0;
    const string_error_index = 1;

    pub const conversion_invalid_id: i64 = 1;
    pub const conversion_overflow_id: i64 = 2;
    pub const string_out_of_range_id: i64 = 3;
    pub const string_empty_pattern_id: i64 = 4;
    pub const string_negative_count_id: i64 = 5;

    alloc: std.mem.Allocator,
    type_table: *TypeTable,

    void_type_id: TypeId,
    float_type_id: TypeId,
    int_type_id: TypeId,
    string_type_id: TypeId,
    buffer_type_id: TypeId,
    boolean_type_id: TypeId,
    dynamic_type_id: TypeId,
    dynamic_array_type_id: TypeId,
    error_set_type_ids: [error_sets.len]TypeId,

    pub fn init(alloc: std.mem.Allocator, type_table: *TypeTable) !Self {
        const dynamic_type_id = type_table.primitives.get(.dynamic).?;
        return Self{
            .error_set_type_ids = try createErrorSets(alloc, type_table),
            .alloc = alloc,
            .type_table = type_table,
            .void_type_id = type_table.primitives.get(.void).?,
            .float_type_id = type_table.primitives.get(.float).?,
            .int_type_id = type_table.primitives.get(.int).?,
            .string_type_id = type_table.primitives.get(.string).?,
            .buffer_type_id = type_table.primitives.get(.buffer).?,
            .boolean_type_id = type_table.primitives.get(.boolean).?,
            .dynamic_type_id = dynamic_type_id,
            .dynamic_array_type_id = try type_table.getOrAddArray(dynamic_type_id),
        };
    }

    fn createErrorSets(alloc: std.mem.Allocator, type_table: *TypeTable) ![error_sets.len]TypeId {
        var type_ids: [error_sets.len]TypeId = undefined;
        for (error_sets, 0..) |spec, i| {
            const error_set = try alloc.create(tt.ErrorSet);
            error_set.* = .{ .is_inferred = false };
            var variants: std.StringHashMap(i64) = .init(alloc);
            for (spec.variants) |variant| {
                const id = try type_table.addErrorName(spec.name, variant);
                try variants.put(variant, id);
                try error_set.ids.put(alloc, id, {});
            }
            type_ids[i] = try type_table.addType(.{
                .kind = .{ .@"enum" = .{
                    .identifier = spec.name,
                    .variants = variants,
                    .variants_in_order = spec.variants,
                    .static_fields = .init(alloc),
                    .methods = .init(alloc),
                    .error_set = error_set,
                } },
                .is_nullable = false,
            });
        }
        std.debug.assert(type_table.error_names.items.len == string_negative_count_id + 1);
        return type_ids;
    }

    pub fn errorSetSymbol(self: *const Self, index: usize, uid: usize) Symbol {
        return .{
            .uid = uid,
            .identifier = error_sets[index].name,
            .type_id = self.type_table.getPrimitive(.meta),
            .kind = .{ .defined_type = .{ .blueprint_type_id = self.error_set_type_ids[index] } },
        };
    }

    fn errorUnionTypeId(self: *const Self, error_set_index: usize, payload: TypeId) !TypeId {
        return self.type_table.addType(.{
            .kind = .{ .error_union = .{ .error_set = self.error_set_type_ids[error_set_index], .payload = payload } },
            .is_nullable = false,
        });
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
            .is_nullable = false,
        });
        return id;
    }

    fn echoFactory(self: *const Self, uid: usize) !BuiltInFn {
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

    fn appendFactory(self: *const Self, uid: usize) !BuiltInFn {
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

    fn lenFactory(self: *const Self, uid: usize) !BuiltInFn {
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

    fn assertFactory(self: *const Self, uid: usize) !BuiltInFn {
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

    fn bufferFromFactory(self: *const Self, uid: usize) !BuiltInFn {
        var symbol = Symbol{ .uid = uid, .identifier = "buffer_from", .kind = .{ .function = {} }, .type_id = undefined };
        symbol.type_id = try self.createFuncTypeId(symbol.uid, symbol.identifier, &.{
            .{ .identifier = "text", .type_id = self.string_type_id, .is_mut = false, .default_value = null },
        }, self.buffer_type_id);
        return BuiltInFn{ .symbol = symbol };
    }

    fn stringFromFactory(self: *const Self, uid: usize) !BuiltInFn {
        var symbol = Symbol{ .uid = uid, .identifier = "string_from", .kind = .{ .function = {} }, .type_id = undefined };
        symbol.type_id = try self.createFuncTypeId(symbol.uid, symbol.identifier, &.{
            .{ .identifier = "data", .type_id = self.buffer_type_id, .is_mut = false, .default_value = null },
        }, try self.type_table.getOrAddNullable(self.string_type_id));
        return BuiltInFn{ .symbol = symbol };
    }

    fn strEqualFactory(self: *const Self, uid: usize) !BuiltInFn {
        var symbol = Symbol{ .uid = uid, .identifier = "str_equal", .kind = .{ .function = {} }, .type_id = undefined };
        symbol.type_id = try self.createFuncTypeId(symbol.uid, symbol.identifier, &.{
            .{ .identifier = "a", .type_id = self.string_type_id, .is_mut = false, .default_value = null },
            .{ .identifier = "b", .type_id = self.string_type_id, .is_mut = false, .default_value = null },
        }, self.boolean_type_id);
        return BuiltInFn{ .symbol = symbol };
    }

    fn strCharCountFactory(self: *const Self, uid: usize) !BuiltInFn {
        var symbol = Symbol{ .uid = uid, .identifier = "str_char_count", .kind = .{ .function = {} }, .type_id = undefined };
        symbol.type_id = try self.createFuncTypeId(symbol.uid, symbol.identifier, &.{
            .{ .identifier = "text", .type_id = self.string_type_id, .is_mut = false, .default_value = null },
        }, self.int_type_id);
        return BuiltInFn{ .symbol = symbol, .bc_fn = .{ .func = &strCharCountImpl, .num_args = 1 } };
    }

    fn strConcatFactory(self: *const Self, uid: usize) !BuiltInFn {
        var symbol = Symbol{ .uid = uid, .identifier = "str_concat", .kind = .{ .function = {} }, .type_id = undefined };
        symbol.type_id = try self.createFuncTypeId(symbol.uid, symbol.identifier, &.{
            .{ .identifier = "a", .type_id = self.string_type_id, .is_mut = false, .default_value = null },
            .{ .identifier = "b", .type_id = self.string_type_id, .is_mut = false, .default_value = null },
        }, self.string_type_id);
        return BuiltInFn{ .symbol = symbol };
    }

    fn strJoinFactory(self: *const Self, uid: usize) !BuiltInFn {
        var symbol = Symbol{ .uid = uid, .identifier = "str_join", .kind = .{ .function = {} }, .type_id = undefined };
        symbol.type_id = try self.createFuncTypeId(symbol.uid, symbol.identifier, &.{
            .{ .identifier = "parts", .type_id = try self.type_table.getOrAddArray(self.string_type_id), .is_mut = false, .default_value = null },
            .{ .identifier = "sep", .type_id = self.string_type_id, .is_mut = false, .default_value = null },
        }, self.string_type_id);
        return BuiltInFn{ .symbol = symbol };
    }

    fn strToIntFactory(self: *const Self, uid: usize) !BuiltInFn {
        return self.conversionFactory(uid, "str_to_int", self.int_type_id);
    }

    fn strToFloatFactory(self: *const Self, uid: usize) !BuiltInFn {
        return self.conversionFactory(uid, "str_to_float", self.float_type_id);
    }

    fn conversionFactory(self: *const Self, uid: usize, identifier: []const u8, payload: TypeId) !BuiltInFn {
        const return_type_id = try self.errorUnionTypeId(conversion_error_index, payload);
        var symbol = Symbol{ .uid = uid, .identifier = identifier, .kind = .{ .function = {} }, .type_id = undefined };
        symbol.type_id = try self.createFuncTypeId(symbol.uid, symbol.identifier, &.{
            .{ .identifier = "text", .type_id = self.string_type_id, .is_mut = false, .default_value = null },
        }, return_type_id);
        return BuiltInFn{ .symbol = symbol };
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

    pub fn setEchoWriter(writer: ?*std.Io.Writer) void {
        echo_writer = writer;
    }

    pub fn printValue(writer: *std.Io.Writer, value: v.Value, ty: v.ValueType) void {
        switch (ty) {
            .int64 => writer.print("{d}", .{value.int64}) catch @panic("failed to print int64"),
            .float64 => writer.print("{d}", .{value.float64}) catch @panic("failed to print float64"),
            .bool => writer.print("{}", .{value.bool}) catch @panic("failed to print bool"),
            .string => {
                const raw: u64 = @bitCast(value);
                if (raw == 0) {
                    writer.print("null", .{}) catch @panic("failed to print null");
                } else {
                    const str = v.HeapValue.getParentPtr(v.Bytes, value.heap_obj);
                    writer.print("{s}", .{str.slice()}) catch @panic("failed to print string");
                }
            },
            .buffer => {
                const raw: u64 = @bitCast(value);
                if (raw == 0) {
                    writer.print("null", .{}) catch @panic("failed to print null");
                    return;
                }
                const bytes = v.HeapValue.getParentPtr(v.Bytes, value.heap_obj).slice();
                writer.print("[", .{}) catch @panic("failed to print buffer bracket");
                for (bytes, 0..) |byte, i| {
                    if (i > 0) writer.print(", ", .{}) catch @panic("failed to print buffer separator");
                    writer.print("{d}", .{byte}) catch @panic("failed to print buffer byte");
                }
                writer.print("]", .{}) catch @panic("failed to print buffer bracket");
            },
            .null => writer.print("null", .{}) catch @panic("failed to print null"),
            .array => {
                const raw: u64 = @bitCast(value);
                if (raw == 0) {
                    writer.print("null", .{}) catch @panic("failed to print null");
                    return;
                }
                const array = v.HeapValue.getParentPtr(v.Array, value.heap_obj);
                const data = array.getDataPtr();
                writer.print("[", .{}) catch @panic("failed to print array bracket");
                for (0..array.len) |i| {
                    if (i > 0) writer.print(", ", .{}) catch @panic("failed to print array separator");
                    printValue(writer, data[i], array.kind);
                }
                writer.print("]", .{}) catch @panic("failed to print array bracket");
            },
            .nullable => {
                const raw: u64 = @bitCast(value);
                if (raw == 0) {
                    writer.print("null", .{}) catch @panic("failed to print null");
                    return;
                }
                // only primitives are boxed, nullable heap values are the object itself
                switch (value.heap_obj.kind) {
                    .null_box => {
                        const nullable = v.HeapValue.getParentPtr(v.NullBox, value.heap_obj);
                        printValue(writer, nullable.value, nullable.kind);
                    },
                    .string => printValue(writer, value, .string),
                    .buffer => printValue(writer, value, .buffer),
                    .array => printValue(writer, value, .array),
                    .@"struct" => printValue(writer, value, .@"struct"),
                    .@"union" => printValue(writer, value, .@"union"),
                }
            },
            .@"struct" => {
                const raw: u64 = @bitCast(value);
                if (raw == 0) {
                    writer.print("null", .{}) catch @panic("failed to print null");
                } else {
                    const s = v.HeapValue.getParentPtr(v.Struct, value.heap_obj);
                    writer.print("{{ ", .{}) catch @panic("failed to print struct brace");
                    for (0..s.field_count) |i| {
                        if (i > 0) writer.print(", ", .{}) catch @panic("failed to print struct separator");
                        writer.print("{d}", .{s.get(i).int64}) catch @panic("failed to print struct field");
                    }
                    writer.print(" }}", .{}) catch @panic("failed to print struct brace");
                }
            },
            .@"union" => {
                const raw: u64 = @bitCast(value);
                if (raw == 0) {
                    writer.print("null", .{}) catch @panic("failed to print null");
                } else {
                    writer.print("<union>", .{}) catch @panic("failed to print union");
                }
            },
        }
    }

    fn echoImpl(args: []v.Value) v.Value {
        if (echo_writer) |writer| {
            const ty: v.ValueType = @enumFromInt(@as(u8, @intCast(args[1].int64)));
            printValue(writer, args[0], ty);
            writer.print("\n", .{}) catch {};
            //todo: not treating error
            writer.flush() catch {};
        }
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

    fn bufferFromCodeGen(target_reg: u8, arg_start_reg: u8, alloc: std.mem.Allocator) ![]const bc.Instruction {
        var list: std.ArrayList(bc.Instruction) = .empty;
        try list.append(alloc, .{ .op = .BYTES_COPY, .a = target_reg, .b = arg_start_reg, .c = @intFromEnum(v.ValueType.buffer) });
        return try list.toOwnedSlice(alloc);
    }

    fn stringFromCodeGen(target_reg: u8, arg_start_reg: u8, alloc: std.mem.Allocator) ![]const bc.Instruction {
        var list: std.ArrayList(bc.Instruction) = .empty;
        try list.append(alloc, .{ .op = .BYTES_COPY, .a = target_reg, .b = arg_start_reg, .c = @intFromEnum(v.ValueType.string) });
        return try list.toOwnedSlice(alloc);
    }

    fn strEqualCodeGen(target_reg: u8, arg_start_reg: u8, alloc: std.mem.Allocator) ![]const bc.Instruction {
        var list: std.ArrayList(bc.Instruction) = .empty;
        try list.append(alloc, .{ .op = .BYTES_EQ, .a = target_reg, .b = arg_start_reg, .c = arg_start_reg + 1 });
        return try list.toOwnedSlice(alloc);
    }

    fn strCharCountImpl(args: []v.Value) v.Value {
        const text = v.HeapValue.getParentPtr(v.Bytes, args[0].heap_obj).slice();
        var count: i64 = 0;
        for (text) |byte| {
            if (byte & 0xC0 != 0x80) count += 1;
        }
        return .{ .int64 = count };
    }

    fn strConcatCodeGen(target_reg: u8, arg_start_reg: u8, alloc: std.mem.Allocator) ![]const bc.Instruction {
        var list: std.ArrayList(bc.Instruction) = .empty;
        try list.append(alloc, .{ .op = .BYTES_CONCAT, .a = target_reg, .b = arg_start_reg, .c = arg_start_reg + 1 });
        return try list.toOwnedSlice(alloc);
    }

    fn strJoinCodeGen(target_reg: u8, arg_start_reg: u8, alloc: std.mem.Allocator) ![]const bc.Instruction {
        var list: std.ArrayList(bc.Instruction) = .empty;
        try list.append(alloc, .{ .op = .STR_JOIN, .a = target_reg, .b = arg_start_reg, .c = arg_start_reg + 1 });
        return try list.toOwnedSlice(alloc);
    }

    fn strToIntCodeGen(target_reg: u8, arg_start_reg: u8, alloc: std.mem.Allocator) ![]const bc.Instruction {
        var list: std.ArrayList(bc.Instruction) = .empty;
        try list.append(alloc, .{ .op = .STR_TO_NUMBER, .a = target_reg, .b = arg_start_reg, .c = @intFromEnum(v.ValueType.int64) });
        return try list.toOwnedSlice(alloc);
    }

    fn strToFloatCodeGen(target_reg: u8, arg_start_reg: u8, alloc: std.mem.Allocator) ![]const bc.Instruction {
        var list: std.ArrayList(bc.Instruction) = .empty;
        try list.append(alloc, .{ .op = .STR_TO_NUMBER, .a = target_reg, .b = arg_start_reg, .c = @intFromEnum(v.ValueType.float64) });
        return try list.toOwnedSlice(alloc);
    }

    fn assertImpl(args: []v.Value) v.Value {
        if (!args[0].bool) {
            @panic("ASSERTION_FAILED");
        }
        return .{ .null = {} };
    }
};
