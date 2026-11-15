const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const native = @import("../native.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const ok = native.ok;
const bytesArg = native.bytesArg;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "print", Binding{ .native = &print } },
    .{ "println", Binding{ .native = &println } },
    .{ "eprint", Binding{ .native = &eprint } },
    .{ "eprintln", Binding{ .native = &eprintln } },
});

fn print(vm: *VM, args: []v.Value) anyerror!NativeResult {
    try vm.stdio.writeBytes(.stdout, bytesArg(args, 0));
    return ok(.{ .null = {} });
}

fn println(vm: *VM, args: []v.Value) anyerror!NativeResult {
    try vm.stdio.writeLine(.stdout, bytesArg(args, 0));
    return ok(.{ .null = {} });
}

fn eprint(vm: *VM, args: []v.Value) anyerror!NativeResult {
    try vm.stdio.writeBytes(.stderr, bytesArg(args, 0));
    return ok(.{ .null = {} });
}

fn eprintln(vm: *VM, args: []v.Value) anyerror!NativeResult {
    try vm.stdio.writeLine(.stderr, bytesArg(args, 0));
    return ok(.{ .null = {} });
}
