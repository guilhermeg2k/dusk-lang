const std = @import("std");
const bc = @import("bytecode.zig");
const v = @import("value.zig");
const util = @import("util.zig");
const RuntimeError = @import("error.zig").RuntimeError;
const RuntimeErrorReporter = @import("error.zig").RuntimeErrorReporter;
const format = @import("format.zig");

pub const VM = struct {
    const Self = @This();
    const MIN_GC_THRESHOLD = 1000000; //1MB
    const MAX_CALL_DEPTH = 100_000;
    const STACK_SLOTS = 4_000_000;
    const MAX_FRAME_REGISTERS = 256;

    allocator: std.mem.Allocator,
    program: *const bc.Program,
    frames: []CallFrame,
    frame_count: usize,
    stack: []v.Value,
    heap: Heap,
    scratch: std.Io.Writer.Allocating,
    stdout: ?*std.Io.Writer,

    pub fn init(program: *const bc.Program, stdout: ?*std.Io.Writer) VM {
        return .{
            .allocator = std.heap.page_allocator,
            .frames = &.{},
            .frame_count = 0,
            .program = program,
            .stack = &.{},
            .heap = .{
                .arenas = .{
                    std.heap.ArenaAllocator.init(std.heap.page_allocator),
                    std.heap.ArenaAllocator.init(std.heap.page_allocator),
                },
                .threshold = MIN_GC_THRESHOLD,
            },
            .scratch = .init(std.heap.page_allocator),
            .stdout = stdout,
        };
    }

    pub fn deinit(self: *Self) void {
        self.scratch.deinit();
        for (&self.heap.arenas) |*arena| arena.deinit();
    }

    noinline fn toString(self: *Self, value: v.Value, kind: v.ValueType) !v.Value {
        self.scratch.clearRetainingCapacity();
        format.printValue(&self.scratch.writer, value, kind);
        const bytes = self.scratch.written();
        const result = try self.allocString(bytes.len);
        @memcpy(result.mutableSlice(), bytes);
        return .{ .heap_obj = &result.obj };
    }

    noinline fn buildString(self: *Self, parts: []const v.Value, kinds: []const v.ValueType) !v.Value {
        self.scratch.clearRetainingCapacity();
        for (parts, kinds) |part, kind| {
            if (kind == .string) {
                try self.scratch.writer.writeAll(v.HeapValue.getParentPtr(v.Bytes, part.heap_obj).slice());
                continue;
            }
            format.printValue(&self.scratch.writer, part, kind);
        }
        const bytes = self.scratch.written();
        const result = try self.allocString(bytes.len);
        @memcpy(result.mutableSlice(), bytes);
        return .{ .heap_obj = &result.obj };
    }

    pub noinline fn run(self: *Self) !void {
        self.frames = try self.allocUntouched(CallFrame, MAX_CALL_DEPTH);
        defer self.freeUntouched(CallFrame, self.frames);
        self.stack = try self.allocUntouched(v.Value, STACK_SLOTS);
        defer self.freeUntouched(v.Value, self.stack);
        self.initMainFrame();

        const stack = self.stack[0..];
        var current_frame = &self.frames[self.frame_count - 1];
        var cur_instruction: usize = 0;
        var instructions = current_frame.function.kind.dusk.instructions;
        var stack_base: usize = 0;

        if (instructions.len == 0) {
            return;
        }

        while (true) {
            const inst = instructions[cur_instruction];
            cur_instruction += 1;

            switch (inst.op) {
                .LOAD_CONST => {
                    stack[stack_base + inst.a] = current_frame.function.kind.dusk.constants[inst.bEx()];
                },

                .LOAD_STRING => {
                    stack[stack_base + inst.a] = self.program.string_pool.values[inst.bEx()];
                },

                .TO_STRING => {
                    current_frame.cur_inst = cur_instruction;
                    stack[stack_base + inst.a] = try self.toString(stack[stack_base + inst.b], @enumFromInt(inst.c));
                },

                .STR_BUILD => {
                    current_frame.cur_inst = cur_instruction;
                    const kinds = current_frame.function.kind.dusk.build_recipes[inst.bEx()];
                    stack[stack_base + inst.a] = try self.buildString(stack[stack_base + inst.a + 1 ..][0..kinds.len], kinds);
                },

                .LOAD => {
                    stack[stack_base + inst.a] = stack[stack_base + inst.b];
                },

                .ARRAY_INIT => {
                    current_frame.cur_inst = cur_instruction;
                    try self.maybeCollectGarbage(v.Array.calc_size(inst.c));
                    var new_array = try v.Array.init(self.getCurrentHeapAllocator(), @enumFromInt(inst.b), inst.c);
                    self.increaseHeapAllocationCount(v.Array.calc_size(new_array.capacity));
                    stack[stack_base + inst.a] = .{ .heap_obj = &new_array.obj };
                },

                .ARRAY_LOAD => {
                    const array = getArrayFromHeapValuePtr(stack, stack_base, inst.b);
                    const idx = stack[stack_base + inst.c];
                    const value = try array.get(idx.int64);
                    stack[stack_base + inst.a] = value;
                },

                .ARRAY_APPEND => {
                    var array = getArrayFromHeapValuePtr(stack, stack_base, inst.a);

                    if (array.needsResize()) {
                        current_frame.cur_inst = cur_instruction;
                        try self.maybeCollectGarbage(v.Array.calc_size(array.calcNewCapacity()));
                        array = getArrayFromHeapValuePtr(stack, stack_base, inst.a);
                        array = try array.resize(self.getCurrentHeapAllocator());
                        stack[stack_base + inst.a].heap_obj = &array.obj;

                        const new_size = v.Array.calc_size(array.capacity);
                        self.heap.allocated += new_size;
                    }

                    const value = stack[stack_base + inst.b];
                    try array.append(value);
                },

                .ARRAY_STORE => {
                    var array = getArrayFromHeapValuePtr(stack, stack_base, inst.a);
                    const idx = stack[stack_base + inst.b];
                    const value = stack[stack_base + inst.c];
                    try array.set(idx.int64, value);
                },

                .ARRAY_LEN => {
                    const array = getArrayFromHeapValuePtr(stack, stack_base, inst.b);
                    stack[stack_base + inst.a] = .{ .int64 = @intCast(array.len) };
                },

                .BYTES_NEW => {
                    current_frame.cur_inst = cur_instruction;
                    stack[stack_base + inst.a] = try self.newBuffer(stack[stack_base + inst.b].int64);
                },

                .BYTES_LEN => {
                    const bytes = v.HeapValue.getParentPtr(v.Bytes, stack[stack_base + inst.b].heap_obj);
                    stack[stack_base + inst.a] = .{ .int64 = @intCast(bytes.len) };
                },

                .BYTES_GET => {
                    const bytes = v.HeapValue.getParentPtr(v.Bytes, stack[stack_base + inst.b].heap_obj);
                    const index = stack[stack_base + inst.c].int64;
                    if (index < 0 or index >= bytes.len) return RuntimeErrorReporter.indexOutOfBounds(index, bytes.len);
                    stack[stack_base + inst.a] = .{ .int64 = bytes.getDataPtr()[@intCast(index)] };
                },

                .BYTES_SET => {
                    const bytes = v.HeapValue.getParentPtr(v.Bytes, stack[stack_base + inst.a].heap_obj);
                    const index = stack[stack_base + inst.b].int64;
                    if (index < 0 or index >= bytes.len) return RuntimeErrorReporter.indexOutOfBounds(index, bytes.len);
                    bytes.getDataPtr()[@intCast(index)] = @intCast(stack[stack_base + inst.c].int64);
                },

                .DEEP_COPY => {
                    current_frame.cur_inst = cur_instruction;
                    stack[stack_base + inst.a] = try self.deepCopy(stack_base + inst.b);
                },

                .ARRAY_POP => {
                    var array = getArrayFromHeapValuePtr(stack, stack_base, inst.b);
                    const value = try array.pop();
                    stack[stack_base + inst.a] = value;
                },

                .STRUCT_INIT => {
                    const desc_id = inst.bEx();
                    const desc = self.program.struct_descriptors[desc_id];
                    current_frame.cur_inst = cur_instruction;
                    try self.maybeCollectGarbage(v.Struct.calc_size(desc.field_count));
                    var new_struct = try v.Struct.init(self.getCurrentHeapAllocator(), desc.field_count, desc_id);
                    self.increaseHeapAllocationCount(v.Struct.calc_size(new_struct.field_count));
                    stack[stack_base + inst.a] = .{ .heap_obj = &new_struct.obj };
                },

                .STRUCT_LOAD => {
                    const struct_ptr = stack[stack_base + inst.b].heap_obj;
                    const struct_vl = v.HeapValue.getParentPtr(v.Struct, struct_ptr);
                    stack[stack_base + inst.a] = struct_vl.get(inst.c);
                },

                .STRUCT_STORE => {
                    const struct_ptr = stack[stack_base + inst.a].heap_obj;
                    const struct_vl = v.HeapValue.getParentPtr(v.Struct, struct_ptr);
                    const vl = stack[stack_base + inst.c];
                    struct_vl.set(inst.b, vl);
                },

                .UNION_INIT => {
                    const desc_id = inst.bEx();
                    current_frame.cur_inst = cur_instruction;
                    try self.maybeCollectGarbage(v.Union.calc_size());
                    var new_union = try v.Union.init(self.getCurrentHeapAllocator(), desc_id);
                    self.increaseHeapAllocationCount(v.Union.calc_size());
                    stack[stack_base + inst.a] = .{ .heap_obj = &new_union.obj };
                },

                .UNION_LOAD => {
                    current_frame.cur_inst = cur_instruction;
                    try self.loadUnionVariant(stack, stack_base, inst.a, inst.b, inst.c);
                },

                .UNION_STORE => {
                    const union_ptr = stack[stack_base + inst.a].heap_obj;
                    const union_vl = v.HeapValue.getParentPtr(v.Union, union_ptr);
                    const vl = stack[stack_base + inst.c];
                    union_vl.set(@intCast(inst.b), vl);
                },

                .UNION_TAG => {
                    const union_vl = v.HeapValue.getParentPtr(v.Union, stack[stack_base + inst.b].heap_obj);
                    stack[stack_base + inst.a] = .{ .int64 = union_vl.active_tag };
                },

                .UNION_PAYLOAD => {
                    const union_vl = v.HeapValue.getParentPtr(v.Union, stack[stack_base + inst.b].heap_obj);
                    stack[stack_base + inst.a] = union_vl.getDataPtr()[0];
                },

                .NULL_BOX_INIT => {
                    const size = v.NullBox.calc_size();
                    current_frame.cur_inst = cur_instruction;
                    try self.maybeCollectGarbage(size);

                    const value = self.stack[stack_base + inst.c];
                    var new_box = try v.NullBox.init(self.getCurrentHeapAllocator(), value, @enumFromInt(inst.b));

                    self.increaseHeapAllocationCount(size);
                    stack[stack_base + inst.a] = .{ .heap_obj = &new_box.obj };
                },

                .NULL_BOX_UNWRAP => {
                    const raw: u64 = @bitCast(stack[stack_base + inst.b]);
                    if (raw == 0) {
                        stack[stack_base + inst.a] = v.NULL_VALUE;
                    } else {
                        const nullable_ptr = stack[stack_base + inst.b].heap_obj;
                        const nullable_vl = v.HeapValue.getParentPtr(v.NullBox, nullable_ptr);
                        stack[stack_base + inst.a] = nullable_vl.value;
                    }
                },

                .NULL_BOX_NNULL => {
                    stack[stack_base + inst.a] = if (@as(u64, @bitCast(stack[stack_base + inst.b])) != 0) v.TRUE_VALUE else v.FALSE_VALUE;
                },

                .I_ADD => {
                    stack[stack_base + inst.a] = .{
                        .int64 = stack[stack_base + inst.b].int64 + stack[stack_base + inst.c].int64,
                    };
                },

                .I_SUB => {
                    stack[stack_base + inst.a] = .{
                        .int64 = stack[stack_base + inst.b].int64 - stack[stack_base + inst.c].int64,
                    };
                },

                .I_MULT => {
                    stack[stack_base + inst.a] = .{
                        .int64 = stack[stack_base + inst.b].int64 * stack[stack_base + inst.c].int64,
                    };
                },

                .TRUNC_DIV => {
                    stack[stack_base + inst.a] = .{ .int64 = @divTrunc(
                        stack[stack_base + inst.b].int64,
                        stack[stack_base + inst.c].int64,
                    ) };
                },

                .I_MOD => {
                    stack[stack_base + inst.a] = .{ .int64 = @mod(
                        stack[stack_base + inst.b].int64,
                        stack[stack_base + inst.c].int64,
                    ) };
                },

                .I_EQ => {
                    stack[stack_base + inst.a] = .{
                        .bool = stack[stack_base + inst.b].int64 == stack[stack_base + inst.c].int64,
                    };
                },

                .I_NEQ => {
                    stack[stack_base + inst.a] = .{
                        .bool = stack[stack_base + inst.b].int64 != stack[stack_base + inst.c].int64,
                    };
                },

                .I_LT => {
                    stack[stack_base + inst.a] = .{
                        .bool = stack[stack_base + inst.b].int64 < stack[stack_base + inst.c].int64,
                    };
                },

                .I_LE => {
                    stack[stack_base + inst.a] = .{
                        .bool = stack[stack_base + inst.b].int64 <= stack[stack_base + inst.c].int64,
                    };
                },

                .I_GT => {
                    stack[stack_base + inst.a] = .{
                        .bool = stack[stack_base + inst.b].int64 > stack[stack_base + inst.c].int64,
                    };
                },

                .I_GE => {
                    stack[stack_base + inst.a] = .{
                        .bool = stack[stack_base + inst.b].int64 >= stack[stack_base + inst.c].int64,
                    };
                },

                .I_NEG => {
                    stack[stack_base + inst.a] = .{
                        .int64 = -stack[stack_base + inst.b].int64,
                    };
                },

                .F_ADD => {
                    stack[stack_base + inst.a] = .{
                        .float64 = stack[stack_base + inst.b].float64 + stack[stack_base + inst.c].float64,
                    };
                },

                .F_SUB => {
                    stack[stack_base + inst.a] = .{
                        .float64 = stack[stack_base + inst.b].float64 - stack[stack_base + inst.c].float64,
                    };
                },

                .F_MULT => {
                    stack[stack_base + inst.a] = .{
                        .float64 = stack[stack_base + inst.b].float64 * stack[stack_base + inst.c].float64,
                    };
                },

                .F_DIV => {
                    stack[stack_base + inst.a] = .{
                        .float64 = stack[stack_base + inst.b].float64 / stack[stack_base + inst.c].float64,
                    };
                },

                .F_MOD => {
                    stack[stack_base + inst.a] = .{ .float64 = @mod(
                        stack[stack_base + inst.b].float64,
                        stack[stack_base + inst.c].float64,
                    ) };
                },

                .F_EQ => {
                    stack[stack_base + inst.a] = .{
                        .bool = stack[stack_base + inst.b].float64 == stack[stack_base + inst.c].float64,
                    };
                },

                .F_NEQ => {
                    stack[stack_base + inst.a] = .{
                        .bool = stack[stack_base + inst.b].float64 != stack[stack_base + inst.c].float64,
                    };
                },

                .F_LT => {
                    stack[stack_base + inst.a] = .{
                        .bool = stack[stack_base + inst.b].float64 < stack[stack_base + inst.c].float64,
                    };
                },

                .F_LE => {
                    stack[stack_base + inst.a] = .{
                        .bool = stack[stack_base + inst.b].float64 <= stack[stack_base + inst.c].float64,
                    };
                },

                .F_GT => {
                    stack[stack_base + inst.a] = .{
                        .bool = stack[stack_base + inst.b].float64 > stack[stack_base + inst.c].float64,
                    };
                },

                .F_GE => {
                    stack[stack_base + inst.a] = .{
                        .bool = stack[stack_base + inst.b].float64 >= stack[stack_base + inst.c].float64,
                    };
                },

                .F_NEG => {
                    stack[stack_base + inst.a] = .{
                        .float64 = -stack[stack_base + inst.b].float64,
                    };
                },

                .B_AND => {
                    stack[stack_base + inst.a] = .{
                        .bool = stack[stack_base + inst.b].bool and stack[stack_base + inst.c].bool,
                    };
                },

                .B_OR => {
                    stack[stack_base + inst.a] = .{
                        .bool = stack[stack_base + inst.b].bool or stack[stack_base + inst.c].bool,
                    };
                },

                .B_EQ => {
                    stack[stack_base + inst.a] = .{
                        .bool = stack[stack_base + inst.b].bool == stack[stack_base + inst.c].bool,
                    };
                },

                .B_NEQ => {
                    stack[stack_base + inst.a] = .{
                        .bool = stack[stack_base + inst.b].bool != stack[stack_base + inst.c].bool,
                    };
                },

                .B_NOT => {
                    stack[stack_base + inst.a] = .{
                        .bool = !stack[stack_base + inst.b].bool,
                    };
                },

                .I_TO_BYTE => {
                    const number = stack[stack_base + inst.b].int64;
                    if (number < 0 or number > 255) return RuntimeErrorReporter.byteOutOfRange(number);
                    stack[stack_base + inst.a] = .{ .int64 = number };
                },

                .I_TO_F => {
                    stack[stack_base + inst.a] = .{
                        .float64 = @floatFromInt(stack[stack_base + inst.a].int64),
                    };
                },

                .F_TO_I => {
                    stack[stack_base + inst.a] = .{
                        .int64 = @trunc(stack[stack_base + inst.a].float64),
                    };
                },

                .CALL => {
                    const func = &self.program.functions[inst.bEx()];
                    switch (func.kind) {
                        .dusk => {
                            current_frame.cur_inst = cur_instruction;
                            const new_offset = stack_base + inst.a + 1;
                            if (self.frame_count == self.frames.len or new_offset + MAX_FRAME_REGISTERS > stack.len) return RuntimeErrorReporter.callDepthExceeded(MAX_CALL_DEPTH);
                            self.frames[self.frame_count] = .{
                                .function = func,
                                .cur_inst = 0,
                                .stack_offset = new_offset,
                            };
                            self.frame_count += 1;
                            current_frame = &self.frames[self.frame_count - 1];
                            cur_instruction = 0;
                            instructions = func.kind.dusk.instructions;
                            stack_base = new_offset;
                        },
                        .native => |n| {
                            current_frame.cur_inst = cur_instruction;
                            const args = stack[stack_base + inst.a + 1 ..][0..n.num_args];
                            const result = try n.func(self, args);
                            stack[stack_base + inst.a] = result.value;
                            if (result.has_failed) cur_instruction += 1;
                        },
                        .@"inline" => unreachable,
                    }
                },

                .TAIL_CALL => {
                    const func = &self.program.functions[inst.bEx()];
                    const chunk = &func.kind.dusk;
                    const args_start = stack_base + inst.a + 1;
                    std.mem.copyForwards(v.Value, stack[stack_base..][0..chunk.num_params], stack[args_start..][0..chunk.num_params]);
                    current_frame.function = func;
                    cur_instruction = 0;
                    instructions = chunk.instructions;
                },

                .JUMP => {
                    cur_instruction = inst.aEx();
                },

                .JUMP_IF_FALSE => {
                    if (!stack[stack_base + inst.a].bool) {
                        cur_instruction = inst.bEx();
                    }
                },

                .RETURN => {
                    self.frame_count -= 1;
                    if (self.frame_count == 0) break;
                    stack[stack_base - 1] = stack[stack_base + inst.a];
                    current_frame = &self.frames[self.frame_count - 1];
                    cur_instruction = current_frame.cur_inst;
                    instructions = current_frame.function.kind.dusk.instructions;
                    stack_base = current_frame.stack_offset;
                },

                .RETURN_ERR => {
                    self.frame_count -= 1;
                    stack[stack_base - 1] = stack[stack_base + inst.a];
                    current_frame = &self.frames[self.frame_count - 1];
                    cur_instruction = current_frame.cur_inst + 1;
                    instructions = current_frame.function.kind.dusk.instructions;
                    stack_base = current_frame.stack_offset;
                },

                .PANIC_ERR => {
                    return RuntimeErrorReporter.unhandledError(self.program.error_names[@intCast(stack[stack_base + inst.a].int64)]);
                },
                else => unreachable,
            }
        }
    }

    noinline fn newBuffer(self: *Self, len: i64) !v.Value {
        if (len < 0) return RuntimeErrorReporter.negativeBufferSize(len);
        const size = v.Bytes.calc_size(@intCast(len));
        try self.maybeCollectGarbage(size);
        const bytes = try v.Bytes.initZeroed(self.getCurrentHeapAllocator(), @intCast(len), .buffer);
        self.increaseHeapAllocationCount(size);
        return .{ .heap_obj = &bytes.obj };
    }

    noinline fn deepCopy(self: *Self, source_index: usize) !v.Value {
        if (@as(u64, @bitCast(self.stack[source_index])) == 0) return self.stack[source_index];
        try self.maybeCollectGarbage(heapObjectSize(self.stack[source_index].heap_obj));

        var copies: std.AutoHashMapUnmanaged(*v.HeapValue, *v.HeapValue) = .empty;
        defer copies.deinit(self.allocator);
        var pending: std.ArrayList(*v.HeapValue) = .empty;
        defer pending.deinit(self.allocator);

        var result = self.stack[source_index];
        try self.deepCopySlot(&result, &copies, &pending);
        while (pending.pop()) |copy| {
            switch (copy.kind) {
                .array => {
                    const array = v.HeapValue.getParentPtr(v.Array, copy);
                    switch (array.kind) {
                        .buffer, .@"struct", .array, .@"union", .nullable => {
                            for (array.getDataPtr()[0..array.len]) |*item| try self.deepCopySlot(item, &copies, &pending);
                        },
                        else => {},
                    }
                },
                .@"struct" => {
                    const s = v.HeapValue.getParentPtr(v.Struct, copy);
                    const desc = self.program.struct_descriptors[s.descriptor_id];
                    const fields = s.getDataPtr()[0..s.field_count];
                    for (0..s.field_count) |i| {
                        if (desc.heap_bitmap & (@as(u256, 1) << @intCast(i)) != 0) try self.deepCopySlot(&fields[i], &copies, &pending);
                    }
                },
                .@"union" => {
                    const u = v.HeapValue.getParentPtr(v.Union, copy);
                    if (u.active_tag == v.Union.NO_TAG) continue;
                    const desc = self.program.union_descriptors[u.descriptor_id];
                    if (desc.heap_bitmap & (@as(u256, 1) << @intCast(u.active_tag)) != 0) try self.deepCopySlot(&u.getDataPtr()[0], &copies, &pending);
                },
                else => {},
            }
        }
        return result;
    }

    fn deepCopySlot(self: *Self, slot: *v.Value, copies: *std.AutoHashMapUnmanaged(*v.HeapValue, *v.HeapValue), pending: *std.ArrayList(*v.HeapValue)) !void {
        if (@as(u64, @bitCast(slot.*)) == 0) return;
        const current = slot.heap_obj.followRelocated();
        if (current.kind == .string or current.kind == .null_box) {
            slot.heap_obj = current;
            return;
        }
        if (copies.get(current)) |copy| {
            slot.heap_obj = copy;
            return;
        }
        const copy = try cloneHeapObj(self.getCurrentHeapAllocator(), current);
        self.increaseHeapAllocationCount(heapObjectSize(copy));
        try copies.put(self.allocator, current, copy);
        if (copy.kind != .buffer) try pending.append(self.allocator, copy);
        slot.heap_obj = copy;
    }

    pub fn allocString(self: *Self, len: usize) !*v.Bytes {
        const size = v.Bytes.calc_size(len);
        try self.maybeCollectGarbage(size);
        const result = try v.Bytes.initUndefined(self.getCurrentHeapAllocator(), len, .string);
        self.increaseHeapAllocationCount(size);
        return result;
    }

    pub fn boxInt(self: *Self, number: i64) !v.Value {
        const size = v.NullBox.calc_size();
        try self.maybeCollectGarbage(size);
        const box = try v.NullBox.init(self.getCurrentHeapAllocator(), .{ .int64 = number }, .int64);
        self.increaseHeapAllocationCount(size);
        return .{ .heap_obj = &box.obj };
    }

    fn allocUntouched(self: *Self, comptime T: type, count: usize) ![]T {
        const bytes = self.allocator.rawAlloc(@sizeOf(T) * count, .of(T), @returnAddress()) orelse return error.OutOfMemory;
        const items: [*]T = @ptrCast(@alignCast(bytes));
        return items[0..count];
    }

    fn freeUntouched(self: *Self, comptime T: type, items: []T) void {
        const bytes: [*]u8 = @ptrCast(items.ptr);
        self.allocator.rawFree(bytes[0 .. @sizeOf(T) * items.len], .of(T), @returnAddress());
    }

    fn initMainFrame(self: *Self) void {
        self.frames[0] = CallFrame{
            .function = &self.program.functions[self.program.main_func_index],
            .cur_inst = 0,
            .stack_offset = 0,
        };
        self.frame_count = 1;
    }

    noinline fn loadUnionVariant(self: *Self, stack: []v.Value, stack_base: usize, target_reg: u8, union_reg: u8, tag: u16) !void {
        var union_vl = v.HeapValue.getParentPtr(v.Union, stack[stack_base + union_reg].heap_obj);
        const desc = self.program.union_descriptors[union_vl.descriptor_id];
        const needs_box = desc.box_bitmap & (@as(u256, 1) << @intCast(tag)) != 0;

        if (!needs_box or union_vl.active_tag != tag) {
            stack[stack_base + target_reg] = union_vl.get(tag);
            return;
        }

        const size = v.NullBox.calc_size();
        try self.maybeCollectGarbage(size);
        union_vl = v.HeapValue.getParentPtr(v.Union, stack[stack_base + union_reg].heap_obj);

        var new_box = try v.NullBox.init(self.getCurrentHeapAllocator(), union_vl.get(tag), desc.variant_kinds[tag]);
        self.increaseHeapAllocationCount(size);
        stack[stack_base + target_reg] = .{ .heap_obj = &new_box.obj };
    }

    fn getArrayFromHeapValuePtr(stack: []v.Value, stack_base: usize, reg_idx: u8) *v.Array {
        const ptr = stack[stack_base + reg_idx].heap_obj;
        const current = ptr.followRelocated();
        if (current != ptr) stack[stack_base + reg_idx].heap_obj = current;
        return v.HeapValue.getParentPtr(v.Array, current);
    }

    pub fn increaseHeapAllocationCount(self: *Self, bytes: usize) void {
        self.heap.allocated += bytes;
        self.heap.object_count += 1;
    }

    pub fn maybeCollectGarbage(self: *Self, bytes: usize) !void {
        if (self.heap.allocated + bytes > self.heap.threshold) {
            try self.collectGarbage();
        }
    }

    noinline fn collectGarbage(self: *Self) !void {
        const inactive: u1 = self.heap.active ^ 1;
        _ = self.heap.arenas[inactive].reset(.retain_capacity);
        const new_alloc = self.heap.arenas[inactive].allocator();

        self.heap.allocated = 0;
        self.heap.object_count = 0;
        self.heap.gc_queue.clearRetainingCapacity();

        const active_frames = self.frames[0..self.frame_count];
        for (active_frames) |frame| {
            const chunk = frame.function.kind.dusk;
            const cur_pc: u16 = @intCast(frame.cur_inst -| 1);
            const heap_map = util.binarySearch(bc.HeapMap, "pc", chunk.heap_maps, cur_pc);
            const bitmap: u256 = if (heap_map) |hm| hm.bitmap else 0;
            const base = frame.stack_offset;
            for (0..chunk.num_registers) |i| {
                const is_heap_value = bitmap & (@as(u256, 1) << @intCast(i)) != 0;
                if (is_heap_value) {
                    try self.copyHeapValue(new_alloc, &self.stack[base + i]);
                }
            }
        }

        while (self.heap.gc_queue.items.len > 0) {
            const obj = self.heap.gc_queue.pop().?;
            switch (obj.kind) {
                .array => {
                    const array = v.HeapValue.getParentPtr(v.Array, obj);
                    switch (array.kind) {
                        .string, .buffer, .@"struct", .array, .@"union", .nullable => {
                            const items = array.getDataPtr()[0..array.len];
                            for (items) |*item| {
                                try self.copyHeapValue(new_alloc, item);
                            }
                        },
                        else => {},
                    }
                },
                .@"struct" => {
                    const s = v.HeapValue.getParentPtr(v.Struct, obj);
                    const desc = self.program.struct_descriptors[s.descriptor_id];
                    const fields = s.getDataPtr()[0..s.field_count];
                    for (0..s.field_count) |i| {
                        if (desc.heap_bitmap & (@as(u256, 1) << @intCast(i)) != 0) {
                            try self.copyHeapValue(new_alloc, &fields[i]);
                        }
                    }
                },
                .@"union" => {
                    const u = v.HeapValue.getParentPtr(v.Union, obj);
                    if (u.active_tag != v.Union.NO_TAG) {
                        const desc = self.program.union_descriptors[u.descriptor_id];
                        if (desc.heap_bitmap & (@as(u256, 1) << @intCast(u.active_tag)) != 0) {
                            const slot = u.getDataPtr();
                            try self.copyHeapValue(new_alloc, &slot[0]);
                        }
                    }
                },
                else => {},
            }
        }

        self.heap.active = inactive;
        self.heap.threshold = @max(self.heap.allocated * 2, MIN_GC_THRESHOLD);

        const old_idx: u1 = self.heap.active ^ 1;
        if (self.heap.arenas[old_idx].queryCapacity() > self.heap.threshold) {
            _ = self.heap.arenas[old_idx].reset(.{ .retain_with_limit = self.heap.threshold });
            self.heap.gc_queue.clearAndFree(self.allocator);
            try self.heap.gc_queue.ensureTotalCapacity(self.allocator, self.heap.object_count);
        }
    }

    fn copyHeapValue(self: *Self, new_alloc: std.mem.Allocator, value: *v.Value) !void {
        const raw: u64 = @bitCast(value.*);
        if (raw == 0) return;
        if (self.program.string_pool.contains(value.heap_obj)) return;
        const current = value.heap_obj.followRelocated();

        if (current.gc_forward) |fwd| {
            value.heap_obj = fwd;
        } else {
            const new_ptr = try cloneHeapObj(new_alloc, current);
            current.gc_forward = new_ptr;
            value.heap_obj = new_ptr;
            if (new_ptr.kind != .string and new_ptr.kind != .buffer) {
                try self.heap.gc_queue.append(self.allocator, new_ptr);
            }
            self.heap.object_count += 1;
            self.heap.allocated += heapObjectSize(new_ptr);
        }
    }

    fn cloneHeapObj(allocator: std.mem.Allocator, obj_ptr: *v.HeapValue) !*v.HeapValue {
        return switch (obj_ptr.kind) {
            .array => &(try v.HeapValue.getParentPtr(v.Array, obj_ptr).clone(allocator)).obj,
            .@"struct" => &(try v.HeapValue.getParentPtr(v.Struct, obj_ptr).clone(allocator)).obj,
            .@"union" => &(try v.HeapValue.getParentPtr(v.Union, obj_ptr).clone(allocator)).obj,
            .null_box => &(try v.HeapValue.getParentPtr(v.NullBox, obj_ptr).clone(allocator)).obj,
            .string, .buffer => &(try v.HeapValue.getParentPtr(v.Bytes, obj_ptr).clone(allocator)).obj,
        };
    }

    fn heapObjectSize(ptr: *v.HeapValue) usize {
        return switch (ptr.kind) {
            .array => v.Array.calc_size(v.HeapValue.getParentPtr(v.Array, ptr).capacity),
            .@"struct" => v.Struct.calc_size(v.HeapValue.getParentPtr(v.Struct, ptr).field_count),
            .@"union" => v.Union.calc_size(),
            .null_box => v.NullBox.calc_size(),
            .string, .buffer => v.Bytes.calc_size(v.HeapValue.getParentPtr(v.Bytes, ptr).len),
        };
    }

    pub fn getCurrentHeapAllocator(self: *Self) std.mem.Allocator {
        return self.heap.arenas[self.heap.active].allocator();
    }
};

const Heap = struct {
    arenas: [2]std.heap.ArenaAllocator,
    active: u1 = 0,
    gc_queue: std.ArrayList(*v.HeapValue) = .empty,
    object_count: usize = 0,
    allocated: usize = 0,
    threshold: usize,
};

const CallFrame = struct {
    function: *const bc.Function,
    cur_inst: usize,
    stack_offset: usize,
};
