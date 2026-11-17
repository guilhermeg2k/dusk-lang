const v = @import("value.zig");

pub const Plan = struct {
    kind: v.ValueType,
    shape: Shape,
};

pub const Shape = union(enum) {
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
};

pub const Structure = struct {
    descriptor: u16,
    fields: []const Field,
};

pub const Field = struct {
    name: []const u8,
    slot: u8,
    plan: u32,
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
    plan: u32,
    tag: u16,
};
