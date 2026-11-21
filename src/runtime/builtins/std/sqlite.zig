const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const native = @import("../native.zig");
const api = @import("../../tasks/sqlite_api.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "version", Binding{ .native = &version } },
});

fn version(vm: *VM, _: []v.Value) anyerror!NativeResult {
    return native.ok(try vm.allocBytesValue(std.mem.span(api.sqlite3_libversion()), .string));
}
