const std = @import("std");
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
    proxy: u32,
};

pub const Structure = struct {
    descriptor: u16,
    fields: []const Field,
    skipped: []const SkippedField,
};

pub const Field = struct {
    name: []const u8,
    aliases: []const []const u8,
    omits_null: bool,
    slot: u8,
    plan: u32,
    default_function: ?u16,

    pub fn isNamed(self: Field, text: []const u8) bool {
        return isAnyName(self.name, self.aliases, text);
    }
};

pub const SkippedField = struct {
    slot: u8,
    default_function: u16,
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
