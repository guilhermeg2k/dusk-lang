const native = @import("../native.zig");
const json_codec = @import("../../json_codec.zig");

pub const JsonError = json_codec.JsonError;
pub const error_sets = .{JsonError};

pub const bindings = native.BindingMap.initComptime(.{
    .{ "encode_value", native.Binding{ .native = &json_codec.encode } },
    .{ "decode_value", native.Binding{ .native = &json_codec.decode } },
});
