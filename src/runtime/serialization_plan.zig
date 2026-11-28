const std = @import("std");
const v = @import("value.zig");

pub const Plan = struct {
    kind: v.ValueType,
    shape: Shape,
    root_site: ?ProxySite = null,
};

pub const Shape = union(enum) {
    int,
    byte,
    float,
    boolean,
    string,
    nullable: u32,
    array: Items,
    map: Items,
    structure: Structure,
    enumeration: []const EnumVariant,
    tagged_union: TaggedUnion,
    proxy: ProxySite,
};

pub const Items = struct {
    item: u32,
    proxy: ?ProxySite = null,
};

pub const ProxySite = struct {
    plan: u32,
    decode_function: u16,
    encode_function: u16,
    is_nullable_target: bool,
};

pub const Structure = struct {
    descriptor: u16,
    fields: []const Field,
    skipped: []const SkippedField,
    lookup: []const NameEntry = &.{},
};

pub const NameEntry = struct {
    name: []const u8,
    index: usize,
};

pub const Field = struct {
    name: []const u8,
    aliases: []const []const u8,
    omits_null: bool,
    slot: u8,
    plan: u32,
    default: ?Default,
    proxy: ?ProxySite = null,

    pub fn isNamed(self: Field, text: []const u8) bool {
        return isAnyName(self.name, self.aliases, text);
    }
};

pub const SkippedField = struct {
    slot: u8,
    default: Default,
};

pub const Default = union(enum) {
    function: u16,
    constant: Constant,
};

pub const Constant = struct {
    value: ConstantValue,
    box_kind: ?v.ValueType,
};

pub const ConstantValue = union(enum) {
    int: i64,
    float: f64,
    boolean: bool,
    null,
    string: u16,
    array: ConstantArray,
};

pub const ConstantArray = struct {
    kind: v.ValueType,
    items: []const ConstantValue,
};

pub const EnumVariant = struct {
    name: []const u8,
    aliases: []const []const u8,
    value: i64,

    pub fn isNamed(self: EnumVariant, text: []const u8) bool {
        return isAnyName(self.name, self.aliases, text);
    }
};

pub const TaggedUnion = struct {
    descriptor: u16,
    variants: []const Variant,
};

pub const Variant = struct {
    name: []const u8,
    aliases: []const []const u8,
    plan: u32,
    tag: u16,
    proxy: ?ProxySite = null,

    pub fn isNamed(self: Variant, text: []const u8) bool {
        return isAnyName(self.name, self.aliases, text);
    }
};

fn isAnyName(name: []const u8, aliases: []const []const u8, text: []const u8) bool {
    if (std.mem.eql(u8, name, text)) {
        return true;
    }
    for (aliases) |alias| {
        if (std.mem.eql(u8, alias, text)) {
            return true;
        }
    }
    return false;
}
