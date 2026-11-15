const native = @import("../native.zig");

pub const JsonError = @import("../../json_codec.zig").JsonError;
pub const error_sets = .{JsonError};

pub const bindings = native.BindingMap.initComptime(.{
    .{ "encode", native.Binding{ .json = .encode } },
    .{ "decode_value", native.Binding{ .json = .decode } },
});
