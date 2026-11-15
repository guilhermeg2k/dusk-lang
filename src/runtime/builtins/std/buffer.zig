const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const native = @import("../native.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const ok = native.ok;
const bytesArg = native.bytesArg;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "len", Binding{ .intrinsic = .bytes_len } },
    .{ "unchecked_new", Binding{ .intrinsic = .new_buffer } },
    .{ "from_string", Binding{ .native = &bufferFrom } },
});

fn bufferFrom(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const size = v.Bytes.calc_size(bytesArg(args, 0).len);
    try vm.maybeCollectGarbage(size);
    const bytes = try v.Bytes.init(vm.getCurrentHeapAllocator(), bytesArg(args, 0), .buffer);
    vm.increaseHeapAllocationCount(size);
    return ok(.{ .heap_obj = &bytes.obj });
}
