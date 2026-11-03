const std = @import("std");
const util = @import("util.zig");
const ir = @import("ir.zig");

pub const TypeId = u64;

pub const TypeTable = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    types: std.ArrayList(Type),

    primitives: std.AutoHashMap(PrimitiveType, TypeId),
    arrays: std.AutoHashMap(TypeId, TypeId),
    nullableTypeIdByTypeId: std.AutoHashMap(TypeId, TypeId),
    typeIdByNullableTypeId: std.AutoHashMap(TypeId, TypeId),
    anom_structs: std.StringHashMap(TypeId),
    error_names: std.ArrayList([]const u8),

    pub fn init(alloc: std.mem.Allocator) !Self {
        var primitives = std.AutoHashMap(PrimitiveType, TypeId).init(alloc);
        var types: std.ArrayList(Type) = .empty;

        for (std.enums.values(PrimitiveType), 0..) |primitive_type, i| {
            try types.append(alloc, Type{
                .kind = switch (primitive_type) {
                    .float => .{ .float = {} },
                    .int => .{ .int = {} },
                    .string => .{ .string = {} },
                    .boolean => .{ .boolean = {} },
                    .void => .{ .void = {} },
                    .null => .{ .null = {} },
                    .dynamic => .{ .dynamic = {} },
                    .meta => .{ .meta = {} },
                },
                .is_nullable = false,
            });

            try primitives.put(primitive_type, @intCast(i));
        }

        return Self{
            .allocator = alloc,
            .types = types,
            .primitives = primitives,
            .arrays = std.AutoHashMap(TypeId, TypeId).init(alloc),
            .nullableTypeIdByTypeId = std.AutoHashMap(TypeId, TypeId).init(alloc),
            .typeIdByNullableTypeId = std.AutoHashMap(TypeId, TypeId).init(alloc),
            .anom_structs = std.StringHashMap(TypeId).init(alloc),
            .error_names = .empty,
        };
    }

    pub fn getPrimitive(self: *const Self, p: PrimitiveType) TypeId {
        return self.primitives.get(p).?;
    }

    pub fn getTypePtrById(self: *Self, id: TypeId) *Type {
        return &self.types.items[@intCast(id)];
    }

    pub fn eql(_: *Self, a: TypeId, b: TypeId) bool {
        return a == b;
    }

    pub fn canCoerce(self: *Self, from: TypeId, to: TypeId) bool {
        if (from == to) return true;

        const type_from = self.getTypePtrById(from);
        const type_to = self.getTypePtrById(to);

        if (type_from.kind == .dynamic or type_to.kind == .dynamic) return true;
        if (type_from.is_nullable and !type_to.is_nullable) return false;

        if (!type_from.is_nullable and type_to.is_nullable) {
            if (type_from.kind == .null) return true;
            if (self.typeIdByNullableTypeId.get(to)) |unwrapped| {
                return self.canCoerce(from, unwrapped);
            }
        }

        if (self.isErrorSet(from) and self.isErrorSet(to)) {
            const from_set = type_from.kind.@"enum".error_set.?;
            const to_set = type_to.kind.@"enum".error_set.?;
            if (from_set.is_inferred or to_set.is_inferred) return false;
            return from_set.isSubsetOf(to_set);
        }

        if (type_from.kind == .array and type_to.kind == .array) {
            return self.canCoerce(type_from.kind.array, type_to.kind.array);
        }

        if (type_from.kind == .@"struct" and type_to.kind == .@"struct") {
            const from_s = type_from.kind.@"struct";
            const to_s = type_to.kind.@"struct";
            const from_is_anon = std.mem.eql(u8, from_s.identifier, "@");
            const to_is_anon = std.mem.eql(u8, to_s.identifier, "@");

            if (from_is_anon and !to_is_anon) {
                for (to_s.fields_in_order) |field| {
                    if (from_s.fields.get(field.identifier) == null) return false;
                }
                return true;
            }
        }

        //note: wrong?
        if (type_from.kind == .function and type_to.kind == .function) return false;

        return false;
    }

    pub fn name(self: *Self, id: TypeId, allocator: std.mem.Allocator) ![]const u8 {
        const t = self.getTypePtrById(id);
        const prefix = if (t.is_nullable) "nullable " else "";

        return switch (t.kind) {
            .float => if (t.is_nullable) "nullable float" else "float",
            .int => if (t.is_nullable) "nullable int" else "int",
            .string => if (t.is_nullable) "nullable string" else "string",
            .boolean => if (t.is_nullable) "nullable boolean" else "boolean",
            .void => if (t.is_nullable) "nullable void" else "void",
            .null => if (t.is_nullable) "nullable null" else "null",
            .dynamic => if (t.is_nullable) "nullable dynamic" else "dynamic",
            .meta => "Meta Symbol",
            .function => |metadata| {
                return std.fmt.allocPrint(allocator, "function {s}", .{metadata.identifier});
            },
            .@"struct" => |s| {
                if (std.mem.eql(u8, s.identifier, "@")) {
                    return std.fmt.allocPrint(allocator, "{s}anonymous struct", .{prefix});
                }
                return std.fmt.allocPrint(allocator, "{s}struct {s}", .{ prefix, s.identifier });
            },
            .@"enum" => |e| {
                if (e.error_set) |set| {
                    if (set.is_inferred) {
                        return std.fmt.allocPrint(allocator, "{s}error set of '{s}'", .{ prefix, e.identifier });
                    }
                    return std.fmt.allocPrint(allocator, "{s}error {s}", .{ prefix, e.identifier });
                }
                return std.fmt.allocPrint(allocator, "{s}enum {s}", .{ prefix, e.identifier });
            },
            .error_union => |eu| {
                const set = self.getTypePtrById(eu.error_set).kind.@"enum";
                const set_name = if (set.error_set.?.is_inferred) "" else set.identifier;
                return std.fmt.allocPrint(allocator, "{s}!{s}", .{ set_name, try self.name(eu.payload, allocator) });
            },
            .@"union" => |u| {
                return std.fmt.allocPrint(allocator, "{s}union {s}", .{ prefix, u.identifier });
            },
            .array => |inner_id| {
                return std.fmt.allocPrint(allocator, "{s}[]{s}", .{ prefix, try self.name(inner_id, allocator) });
            },
        };
    }

    pub fn isErrorSet(self: *Self, id: TypeId) bool {
        const t = self.getTypePtrById(id);
        return t.kind == .@"enum" and t.kind.@"enum".error_set != null;
    }

    pub fn isErrorUnion(self: *Self, id: TypeId) bool {
        return self.getTypePtrById(id).kind == .error_union;
    }

    pub fn addErrorName(self: *Self, set_name: []const u8, variant: []const u8) !i64 {
        if (self.error_names.items.len == 0) {
            try self.error_names.append(self.allocator, "");
        }
        try self.error_names.append(self.allocator, try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ set_name, variant }));
        return @intCast(self.error_names.items.len - 1);
    }

    pub fn addType(self: *Self, typ: Type) !TypeId {
        try self.types.append(self.allocator, typ);
        return @intCast(self.types.items.len - 1);
    }

    pub fn getOrAddNullable(self: *Self, typeId: TypeId) !TypeId {
        if (self.getTypePtrById(typeId).is_nullable) return typeId;

        const nullable_type = self.nullableTypeIdByTypeId.get(typeId);

        if (nullable_type) |t| {
            return t;
        }

        const @"type" = self.getTypePtrById(typeId);

        try self.types.append(self.allocator, .{
            .kind = @"type".kind,
            .is_nullable = true,
        });

        const id: TypeId = @intCast(self.types.items.len - 1);
        try self.nullableTypeIdByTypeId.put(typeId, id);
        try self.typeIdByNullableTypeId.put(id, typeId);

        return id;
    }

    pub fn getOrAddArray(self: *Self, inner: TypeId) !TypeId {
        const cached_array = self.arrays.get(inner);
        if (cached_array) |type_id| {
            return type_id;
        }

        try self.types.append(self.allocator, .{
            .kind = .{
                .array = inner,
            },
            .is_nullable = false,
        });

        const new_type_id: TypeId = @intCast(self.types.items.len - 1);
        try self.arrays.put(inner, new_type_id);
        return new_type_id;
    }

    pub fn getAnonymoustStructSignature(self: *Self, anom_struct: *const Struct) ![]const u8 {
        var fields_in_order: std.ArrayList([]const u8) = .empty;

        var keys_it = anom_struct.fields.keyIterator();
        while (keys_it.next()) |key_ptr| {
            try fields_in_order.append(self.allocator, key_ptr.*);
        }

        std.mem.sort([]const u8, fields_in_order.items, {}, util.stringCompare);

        var buf: std.ArrayList(u8) = .empty;
        for (fields_in_order.items) |field| {
            const typed_id = anom_struct.fields.get(field);
            if (typed_id) |t_id| {
                const formatted = try std.fmt.allocPrint(self.allocator, "{s}:{d}/", .{
                    field,
                    t_id.type_id,
                });
                try buf.appendSlice(self.allocator, formatted);
            }
        }

        return buf.toOwnedSlice(self.allocator);
    }

    pub fn getOrAddAnonymousStruct(self: *Self, anom_struct: Struct) !TypeId {
        const anom_struct_signature = try self.getAnonymoustStructSignature(&anom_struct);
        const cached_anom_struct = self.anom_structs.get(anom_struct_signature);

        if (cached_anom_struct) |id| {
            return id;
        }

        try self.types.append(self.allocator, .{
            .kind = .{
                .@"struct" = anom_struct,
            },
            .is_nullable = false,
        });

        const new_id: TypeId = @intCast(self.types.items.len - 1);
        try self.anom_structs.put(anom_struct_signature, new_id);

        return new_id;
    }
};

pub const PrimitiveType = enum {
    float,
    int,
    string,
    boolean,
    void,
    null,
    dynamic,
    meta,
};

pub const Struct = struct {
    identifier: []const u8,
    fields: std.StringHashMap(TypedIdentifier),
    static_fields: std.StringHashMap(TypedIdentifier),
    fields_in_order: []const TypedIdentifier,
    field_index_by_name: std.StringHashMap(u8),
    methods: std.StringHashMap(TypeId),
};

pub const Func = struct {
    identifier: []const u8,
    uid: usize,
    params: []const TypedIdentifier,
    return_type_id: TypeId,
};

pub const Enum = struct {
    identifier: []const u8,
    variants: std.StringHashMap(i64),
    variants_in_order: []const []const u8 = &.{},
    static_fields: std.StringHashMap(TypedIdentifier),
    methods: std.StringHashMap(TypeId),
    error_set: ?*ErrorSet = null,
};

pub const ErrorSet = struct {
    is_inferred: bool,
    ids: std.AutoArrayHashMapUnmanaged(i64, void) = .empty,

    pub fn has(self: *const ErrorSet, id: i64) bool {
        return self.ids.contains(id);
    }

    pub fn isSubsetOf(self: *const ErrorSet, other: *const ErrorSet) bool {
        for (self.ids.keys()) |id| {
            if (!other.has(id)) return false;
        }
        return true;
    }
};

pub const ErrorUnion = struct {
    error_set: TypeId,
    payload: TypeId,
};

pub const Union = struct {
    pub const MAX_VARIANTS = 256;

    identifier: []const u8,
    variants: std.StringHashMap(TypeId),
    variants_in_order: []const []const u8 = &.{},
    static_fields: std.StringHashMap(TypedIdentifier),
    methods: std.StringHashMap(TypeId),
};

//note: idk about this
pub const TypedIdentifier = struct {
    const Self = @This();

    identifier: []const u8,
    type_id: TypeId,
    is_mut: bool,
    default_value: ?*ir.Value,

    pub fn init(allocator: std.mem.Allocator, exp: Self) !*Self {
        const ptr = try allocator.create(Self);
        ptr.* = exp;
        return ptr;
    }
};

pub const Type = struct {
    const Self = @This();

    kind: union(enum) {
        float,
        int,
        string,
        boolean,
        void,
        null,
        dynamic,

        meta,

        function: Func,
        array: TypeId,
        @"struct": Struct,
        @"enum": Enum,
        @"union": Union,
        error_union: ErrorUnion,
    },

    is_nullable: bool,

    pub fn init(allocator: std.mem.Allocator, exp: Self) !*Self {
        const ptr = try allocator.create(Self);
        ptr.* = exp;
        return ptr;
    }
};
