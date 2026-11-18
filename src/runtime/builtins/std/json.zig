const native = @import("../native.zig");
const json_codec = @import("../../json_codec.zig");
const Binding = native.Binding;

pub const JsonError = json_codec.JsonError;
pub const error_sets = .{JsonError};

pub const bindings = native.BindingMap.initComptime(.{
    .{ "encode_value", Binding{ .native = &json_codec.encode } },
    .{ "encode_with_sites", Binding{ .native = &json_codec.encode } },
    .{ "collect_sites", Binding{ .native = &json_codec.collectSites } },
    .{ "collect_nested", Binding{ .native = &json_codec.collectNested } },
    .{ "decode_value", Binding{ .native = &json_codec.decode } },
    .{ "decode_proxied", Binding{ .native = &json_codec.decodeProxied } },
    .{ "describe_step", Binding{ .native = &json_codec.describeStep } },
});
