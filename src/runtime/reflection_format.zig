const v = @import("value.zig");

pub const Descriptor = struct {
    name: []const u8,
    kind: Kind,
    storage: v.ValueType,
    child: ?u32 = null,
    key: ?u32 = null,
    fields: []const Field = &.{},
    variants: []const Variant = &.{},
    object_descriptor: ?u16 = null,
    has_private_fields: bool = false,
};

pub const Kind = enum {
    Int,
    Byte,
    Float,
    Bool,
    String,
    Buffer,
    Nullable,
    Array,
    Map,
    Struct,
    Enum,
    Union,
    Function,
    Task,
    Error,
    Void,
    Null,
};

pub const Field = struct {
    name: []const u8,
    type_index: u32,
    slot: u8,
    has_default: bool,
};

pub const Variant = struct {
    name: []const u8,
    value: i64,
    type_index: ?u32 = null,
};
