const std = @import("std");
const bc = @import("bytecode.zig");
const v = @import("value.zig");
const sema = @import("sema.zig");
const tt = @import("type_table.zig");
const ir = @import("ir.zig");
const natives = @import("natives.zig");

const Symbol = sema.Symbol;
const TypeId = tt.TypeId;
const TypedIdentifier = tt.TypedIdentifier;
const TypeTable = tt.TypeTable;

//note: temporary as we don't impl modules
pub const BuiltIn = struct {
    const Self = @This();

    const BuiltInFn = struct {
        symbol: Symbol,
    };

    const BuiltInFactory = *const fn (*const Self, usize) anyerror!BuiltInFn;

    const StrType = enum { string, int, byte, boolean, nullable_int, string_array };

    const StringOpSpec = struct {
        name: []const u8,
        func: bc.NativeFnPtr,
        params: []const struct { []const u8, StrType },
        result: StrType,
        can_fail: bool = false,
    };

    const string_ops = [_]StringOpSpec{
        .{ .name = "str_find", .func = &natives.strFind, .params = &.{ .{ "text", .string }, .{ "needle", .string } }, .result = .nullable_int },
        .{ .name = "str_contains", .func = &natives.strContains, .params = &.{ .{ "text", .string }, .{ "needle", .string } }, .result = .boolean },
        .{ .name = "str_starts_with", .func = &natives.strStartsWith, .params = &.{ .{ "text", .string }, .{ "prefix", .string } }, .result = .boolean },
        .{ .name = "str_ends_with", .func = &natives.strEndsWith, .params = &.{ .{ "text", .string }, .{ "suffix", .string } }, .result = .boolean },
        .{ .name = "str_count", .func = &natives.strCount, .params = &.{ .{ "text", .string }, .{ "needle", .string } }, .result = .int },
        .{ .name = "str_byte_at", .func = &natives.strByteAt, .params = &.{ .{ "text", .string }, .{ "index", .int } }, .result = .byte, .can_fail = true },
        .{ .name = "str_char_at", .func = &natives.strCharAt, .params = &.{ .{ "text", .string }, .{ "index", .int } }, .result = .string, .can_fail = true },
        .{ .name = "str_trim", .func = &natives.strTrim, .params = &.{.{ "text", .string }}, .result = .string },
        .{ .name = "str_split", .func = &natives.strSplit, .params = &.{ .{ "text", .string }, .{ "separator", .string } }, .result = .string_array },
        .{ .name = "str_replace", .func = &natives.strReplace, .params = &.{ .{ "text", .string }, .{ "old", .string }, .{ "new", .string } }, .result = .string },
        .{ .name = "str_upper", .func = &natives.strUpper, .params = &.{.{ "text", .string }}, .result = .string },
        .{ .name = "str_lower", .func = &natives.strLower, .params = &.{.{ "text", .string }}, .result = .string },
        .{ .name = "str_slice", .func = &natives.strSlice, .params = &.{ .{ "text", .string }, .{ "start", .int }, .{ "stop", .int } }, .result = .string, .can_fail = true },
        .{ .name = "str_repeat", .func = &natives.strRepeat, .params = &.{ .{ "text", .string }, .{ "count", .int } }, .result = .string, .can_fail = true },
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
        copyFactory,
    } ++ stringOpFactories();

    const RegistryEntry = struct {
        name: []const u8,
        kind: bc.FunctionKind,
    };

    const builtin_bytecode_registry = [_]RegistryEntry{
        .{ .name = "echo", .kind = native(&natives.echo, 2) },
        .{ .name = "append", .kind = .{ .@"inline" = .{ .gen = &appendCodeGen, .num_args = 2 } } },
        .{ .name = "len", .kind = .{ .@"inline" = .{ .gen = &lenCodeGen, .num_args = 1 } } },
        .{ .name = "assert", .kind = native(&natives.assert, 1) },
        .{ .name = "buffer_from", .kind = native(&natives.bufferFrom, 1) },
        .{ .name = "string_from", .kind = native(&natives.stringFrom, 1) },
        .{ .name = "str_equal", .kind = native(&natives.strEqual, 3) },
        .{ .name = "str_char_count", .kind = native(&natives.strCharCount, 1) },
        .{ .name = "str_concat", .kind = native(&natives.strConcat, 2) },
        .{ .name = "str_join", .kind = native(&natives.strJoin, 2) },
        .{ .name = "str_to_int", .kind = native(&natives.strToInt, 1) },
        .{ .name = "str_to_float", .kind = native(&natives.strToFloat, 1) },
        .{ .name = "copy", .kind = .{ .@"inline" = .{ .gen = &copyCodeGen, .num_args = 1 } } },
    } ++ stringOpRegistry();

    fn native(func: bc.NativeFnPtr, num_args: u8) bc.FunctionKind {
        return .{ .native = .{ .func = func, .num_args = num_args } };
    }

    fn stringOpFactories() [string_ops.len]BuiltInFactory {
        var factories: [string_ops.len]BuiltInFactory = undefined;
        for (string_ops, 0..) |spec, i| factories[i] = stringOpFactory(spec);
        return factories;
    }

    fn stringOpRegistry() [string_ops.len]RegistryEntry {
        var entries: [string_ops.len]RegistryEntry = undefined;
        for (string_ops, 0..) |spec, i| {
            entries[i] = .{
                .name = spec.name,
                .kind = native(spec.func, @intCast(spec.params.len)),
            };
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

    fn strTypeId(self: *const Self, str_type: StrType) !TypeId {
        return switch (str_type) {
            .string => self.string_type_id,
            .int => self.int_type_id,
            .byte => self.type_table.getPrimitive(.byte),
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

    const ErrorSetSpec = struct { name: []const u8, variants: []const []const u8 };

    pub const error_sets = [_]ErrorSetSpec{
        .{ .name = "ConversionError", .variants = &.{ "Invalid", "Overflow" } },
        .{ .name = "StringError", .variants = &.{ "OutOfRange", "NegativeCount", "InvalidUtf8" } },
    };
    const conversion_error_index = 0;
    const string_error_index = 1;


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
        std.debug.assert(type_table.error_names.items.len == natives.string_invalid_utf8_id + 1);
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

        return BuiltInFn{ .symbol = symbol };
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

    fn copyFactory(self: *const Self, uid: usize) !BuiltInFn {
        var symbol = Symbol{ .uid = uid, .identifier = "copy", .kind = .{ .function = {} }, .type_id = undefined };
        symbol.type_id = try self.createFuncTypeId(symbol.uid, symbol.identifier, &.{
            .{ .identifier = "value", .type_id = self.dynamic_type_id, .is_mut = false, .default_value = null },
        }, self.dynamic_type_id);
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
        return BuiltInFn{ .symbol = symbol };
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
        }, try self.errorUnionTypeId(string_error_index, self.string_type_id));
        return BuiltInFn{ .symbol = symbol };
    }

    fn strEqualFactory(self: *const Self, uid: usize) !BuiltInFn {
        var symbol = Symbol{ .uid = uid, .identifier = "str_equal", .kind = .{ .function = {} }, .type_id = undefined };
        symbol.type_id = try self.createFuncTypeId(symbol.uid, symbol.identifier, &.{
            .{ .identifier = "a", .type_id = self.string_type_id, .is_mut = false, .default_value = null },
            .{ .identifier = "b", .type_id = self.string_type_id, .is_mut = false, .default_value = null },
            .{ .identifier = "ignore_case", .type_id = self.boolean_type_id, .is_mut = false, .default_value = try ir.Value.init(self.alloc, .{ .data = .{ .i_bool = false }, .type_id = self.boolean_type_id }) },
        }, self.boolean_type_id);
        return BuiltInFn{ .symbol = symbol };
    }

    fn strCharCountFactory(self: *const Self, uid: usize) !BuiltInFn {
        var symbol = Symbol{ .uid = uid, .identifier = "str_char_count", .kind = .{ .function = {} }, .type_id = undefined };
        symbol.type_id = try self.createFuncTypeId(symbol.uid, symbol.identifier, &.{
            .{ .identifier = "text", .type_id = self.string_type_id, .is_mut = false, .default_value = null },
        }, self.int_type_id);
        return BuiltInFn{ .symbol = symbol };
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
            .{ .identifier = "sep", .type_id = self.string_type_id, .is_mut = false, .default_value = try ir.Value.init(self.alloc, .{ .data = .{ .i_string = "" }, .type_id = self.string_type_id }) },
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

    fn copyCodeGen(target_reg: u8, arg_start_reg: u8, alloc: std.mem.Allocator) ![]const bc.Instruction {
        var list: std.ArrayList(bc.Instruction) = .empty;
        try list.append(alloc, .{ .op = .DEEP_COPY, .a = target_reg, .b = arg_start_reg });
        return try list.toOwnedSlice(alloc);
    }

};
