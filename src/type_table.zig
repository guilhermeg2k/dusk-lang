const std = @import("std");
const util = @import("util.zig");
const ir = @import("ir.zig");

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
    static_fields: std.StringHashMap(TypedIdentifier),
    methods: std.StringHashMap(TypeId),
};

pub const Union = struct {
    identifier: []const u8,
    variants: std.StringHashMap(TypeId),
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
    },

    nullable: bool,

    pub fn init(allocator: std.mem.Allocator, exp: Self) !*Self {
        const ptr = try allocator.create(Self);
        ptr.* = exp;
        return ptr;
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
                .nullable = false,
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

    pub fn coerce(self: *Self, from: TypeId, to: TypeId) bool {
        if (from == to) return true;

        const type_from = self.getTypePtrById(from);
        const type_to = self.getTypePtrById(to);

        if (type_from.kind == .dynamic or type_to.kind == .dynamic) return true;

        if (!type_from.nullable and type_to.nullable) {
            if (type_from.kind == .null) return true;
            if (self.typeIdByNullableTypeId.get(to)) |unwrapped| {
                return self.coerce(from, unwrapped);
            }
        }

        if (type_from.kind == .array and type_to.kind == .array) {
            return self.coerce(type_from.kind.array, type_to.kind.array);
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
        const prefix = if (t.nullable) "nullable " else "";

        return switch (t.kind) {
            .float => if (t.nullable) "nullable float" else "float",
            .int => if (t.nullable) "nullable int" else "int",
            .string => if (t.nullable) "nullable string" else "string",
            .boolean => if (t.nullable) "nullable boolean" else "boolean",
            .void => if (t.nullable) "nullable void" else "void",
            .null => if (t.nullable) "nullable null" else "null",
            .dynamic => if (t.nullable) "nullable dynamic" else "dynamic",
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
                return std.fmt.allocPrint(allocator, "{s}enum {s}", .{ prefix, e.identifier });
            },
            .array => |inner_id| {
                return std.fmt.allocPrint(allocator, "{s}[]{s}", .{ prefix, try self.name(inner_id, allocator) });
            },
        };
    }

    pub fn addType(self: *Self, typ: Type) !TypeId {
        try self.types.append(self.allocator, typ);
        return @intCast(self.types.items.len - 1);
    }

    pub fn getOrAddNullable(self: *Self, typeId: TypeId) !TypeId {
        if (self.getTypePtrById(typeId).nullable) return typeId;

        const nullable_type = self.nullableTypeIdByTypeId.get(typeId);

        if (nullable_type) |t| {
            return t;
        }

        const @"type" = self.getTypePtrById(typeId);

        try self.types.append(self.allocator, .{
            .kind = @"type".kind,
            .nullable = true,
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
            .nullable = false,
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
            .nullable = false,
        });

        const new_id: TypeId = @intCast(self.types.items.len - 1);
        try self.anom_structs.put(anom_struct_signature, new_id);

        return new_id;
    }
};
