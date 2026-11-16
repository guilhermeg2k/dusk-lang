const native = @import("../native.zig");
const codec = @import("../../json_codec.zig");

pub const JsonError = codec.JsonError;
pub const error_sets = .{JsonError};

const requirements = native.ValueRequirements.init(&.{ .Int, .Byte, .Float, .Bool, .String, .Nullable, .Array, .Map, .Struct, .Enum, .Union }, true);

pub const bindings = native.BindingMap.initComptime(.{
    .{ "encode", native.Binding{ .reflection = .{ .operation = .type_metadata, .func = &codec.encode, .requirements = requirements } } },
    .{ "decode_value", native.Binding{ .reflection = .{ .operation = .type_and_result_descriptor, .func = &codec.decode, .requirements = requirements } } },
});
