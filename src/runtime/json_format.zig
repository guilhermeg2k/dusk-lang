const v = @import("value.zig");

pub const Codec = struct {
    kind: v.ValueType,
    shape: union(enum) {
        int,
        byte,
        float,
        boolean,
        string,
        nullable: u32,
        array: u32,
        map: u32,
        structure: Structure,
        enumeration: []const EnumVariant,
        tagged_union: TaggedUnion,
    },
};

pub const Structure = struct {
    descriptor: u16,
    fields: []const Field,
};

pub const Field = struct {
    name: []const u8,
    codec: u32,
    default_function: ?u16,
};

pub const EnumVariant = struct {
    name: []const u8,
    value: i64,
};

pub const TaggedUnion = struct {
    descriptor: u16,
    variants: []const Variant,
};

pub const Variant = struct {
    name: []const u8,
    codec: u32,
    tag: u16,
};
