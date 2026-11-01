const std = @import("std");
const bc = @import("bytecode.zig");
const v = @import("value.zig");
const util = @import("util.zig");
const RunTimeError = @import("error.zig").RunTimeError;
const BuiltIn = @import("built-in.zig").BuiltIn;

pub const VM = struct {
    const Self = @This();
    const MIN_GC_THRESHOLD = 1000000; //1MB

    allocator: std.mem.Allocator,
    program: *const bc.Program,
    frames: [256]CallFrame,
    frame_count: usize,
    stack: [16383]v.Value,
    heap: Heap,
    static_store: []v.Value,

    pub fn init(program: *const bc.Program) VM {
        return .{
            .allocator = std.heap.page_allocator,
            .frames = undefined,
            .frame_count = 0,
            .program = program,
            .stack = undefined,
            .heap = .{
                .arenas = .{
                    std.heap.ArenaAllocator.init(std.heap.page_allocator),
                    std.heap.ArenaAllocator.init(std.heap.page_allocator),
                },
                .threshold = MIN_GC_THRESHOLD,
            },
            .static_store = &.{},
        };
    }

    fn appendStringValue(self: *Self, target: *v.Value, value: v.Value, kind: v.ValueType) !void {
        var buffer: std.Io.Writer.Allocating = .init(self.allocator);
        defer buffer.deinit();
        const prefix = v.HeapValue.getParentPtr(v.Bytes, target.heap_obj);
        try buffer.writer.writeAll(prefix.slice());
        @import("built-in.zig").BuiltIn.printValue(&buffer.writer, value, kind);
        const bytes = buffer.written();
        const size = v.Bytes.calc_size(bytes.len);
        try self.maybeCollectGarbage(size);
        const string = try v.Bytes.init(self.getCurrentHeapAllocator(), bytes, .string);
        self.increaseHeapAllocationCount(size);
        target.* = .{ .heap_obj = &string.obj };
    }

    pub noinline fn run(self: *Self) !void {
        try self.initStaticStore();
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

                .STRING_APPEND => {
                    current_frame.cur_inst = cur_instruction;
                    try self.appendStringValue(&stack[stack_base + inst.a], stack[stack_base + inst.b], @enumFromInt(inst.c));
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
                    const arr_ptr = stack[stack_base + inst.b].heap_obj;
                    const array = getArrayFromHeapValuePtr(stack, stack_base, inst.b, arr_ptr);
                    const idx = stack[stack_base + inst.c];
                    const value = try array.get(@intCast(idx.int64));
                    stack[stack_base + inst.a] = value;
                },

                .ARRAY_APPEND => {
                    const arr_ptr = stack[stack_base + inst.a].heap_obj;
                    var array = getArrayFromHeapValuePtr(stack, stack_base, inst.a, arr_ptr);

                    if (array.needsResize()) {
                        current_frame.cur_inst = cur_instruction;
                        try self.maybeCollectGarbage(v.Array.calc_size(array.calcNewCapacity()));
                        array = getArrayFromHeapValuePtr(stack, stack_base, inst.a, stack[stack_base + inst.a].heap_obj);
                        array = try array.resize(self.getCurrentHeapAllocator());
                        stack[stack_base + inst.a].heap_obj = &array.obj;

                        const new_size = v.Array.calc_size(array.capacity);
                        self.heap.allocated += new_size;
                    }

                    const value = stack[stack_base + inst.b];
                    try array.append(value);
                },

                .ARRAY_STORE => {
                    const arr_ptr = stack[stack_base + inst.a].heap_obj;
                    var array = getArrayFromHeapValuePtr(stack, stack_base, inst.a, arr_ptr);
                    const idx = stack[stack_base + inst.b];
                    const value = stack[stack_base + inst.c];
                    try array.set(@intCast(idx.int64), value);
                },

                .ARRAY_LEN => {
                    const arr_ptr = stack[stack_base + inst.b].heap_obj;
                    const array = getArrayFromHeapValuePtr(stack, stack_base, inst.b, arr_ptr);
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
                    if (index < 0 or index >= bytes.len) return indexOutOfBounds(index, bytes.len);
                    stack[stack_base + inst.a] = .{ .int64 = bytes.getDataPtr()[@intCast(index)] };
                },

                .BYTES_SET => {
                    const bytes = v.HeapValue.getParentPtr(v.Bytes, stack[stack_base + inst.a].heap_obj);
                    const index = stack[stack_base + inst.b].int64;
                    if (index < 0 or index >= bytes.len) return indexOutOfBounds(index, bytes.len);
                    bytes.getDataPtr()[@intCast(index)] = @intCast(stack[stack_base + inst.c].int64);
                },

                .BYTES_CONCAT => {
                    current_frame.cur_inst = cur_instruction;
                    stack[stack_base + inst.a] = try self.concatBytes(&stack[stack_base + inst.b], &stack[stack_base + inst.c]);
                },

                .STR_JOIN => {
                    current_frame.cur_inst = cur_instruction;
                    stack[stack_base + inst.a] = try self.joinStrings(&stack[stack_base + inst.b], &stack[stack_base + inst.c]);
                },

                .STR_TO_NUMBER => {
                    const result = convertText(stack[stack_base + inst.b], @enumFromInt(inst.c));
                    stack[stack_base + inst.a] = result.value;
                    if (result.has_failed) cur_instruction += 1;
                },

                .STR_OP => {
                    current_frame.cur_inst = cur_instruction;
                    const result = try self.runStringOp(@enumFromInt(inst.c), stack[stack_base + inst.b ..]);
                    stack[stack_base + inst.a] = result.value;
                    if (result.has_failed) cur_instruction += 1;
                },

                .BYTES_COPY => {
                    current_frame.cur_inst = cur_instruction;
                    stack[stack_base + inst.a] = try self.copyToBuffer(&stack[stack_base + inst.b]);
                },

                .ARRAY_POP => {
                    const arr_ptr = stack[stack_base + inst.b].heap_obj;
                    var array = getArrayFromHeapValuePtr(stack, stack_base, inst.b, arr_ptr);
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

                .STATIC_LOAD => {
                    stack[stack_base + inst.a] = self.static_store[inst.bEx()];
                },

                .STATIC_STORE => {
                    self.static_store[inst.bEx()] = stack[stack_base + inst.a];
                },

                .GLOBAL_LOAD => {
                    stack[stack_base + inst.a] = stack[inst.bEx()];
                },

                .GLOBAL_STORE => {
                    stack[inst.bEx()] = stack[stack_base + inst.a];
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

                .NULL_BOX_STORE => {
                    const is_null = inst.b == 0;
                    if (is_null) {
                        stack[stack_base + inst.a] = v.NULL_VALUE;
                        continue;
                    }

                    const new_value = stack[stack_base + inst.c];
                    const current_raw: u64 = @bitCast(stack[stack_base + inst.a]);
                    if (current_raw == 0) {
                        const size = v.NullBox.calc_size();
                        current_frame.cur_inst = cur_instruction;
                        try self.maybeCollectGarbage(size);
                        var new_box = try v.NullBox.init(self.getCurrentHeapAllocator(), new_value, @enumFromInt(inst.b - 1));
                        self.increaseHeapAllocationCount(size);
                        stack[stack_base + inst.a] = .{ .heap_obj = &new_box.obj };
                    } else {
                        const nullable_vl = v.HeapValue.getParentPtr(v.NullBox, stack[stack_base + inst.a].heap_obj);
                        nullable_vl.value = new_value;
                    }
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
                    if (number < 0 or number > 255) return byteOutOfRange(number);
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
                        .host => |b| {
                            const args = stack[stack_base + inst.a + 1 ..][0..b.num_args];
                            stack[stack_base + inst.a] = b.func(args);
                        },
                        .@"inline" => unreachable,
                    }
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
                    return self.panicUnhandledError(stack[stack_base + inst.a]);
                },
                else => unreachable,
            }
        }
    }

    noinline fn newBuffer(self: *Self, len: i64) !v.Value {
        if (len < 0) return negativeBufferSize(len);
        const size = v.Bytes.calc_size(@intCast(len));
        try self.maybeCollectGarbage(size);
        const bytes = try v.Bytes.initZeroed(self.getCurrentHeapAllocator(), @intCast(len), .buffer);
        self.increaseHeapAllocationCount(size);
        return .{ .heap_obj = &bytes.obj };
    }

    noinline fn copyToBuffer(self: *Self, source: *const v.Value) !v.Value {
        const source_len = v.HeapValue.getParentPtr(v.Bytes, source.heap_obj).len;
        const size = v.Bytes.calc_size(source_len);
        try self.maybeCollectGarbage(size);
        const source_bytes = v.HeapValue.getParentPtr(v.Bytes, source.heap_obj).slice();
        const bytes = try v.Bytes.init(self.getCurrentHeapAllocator(), source_bytes, .buffer);
        self.increaseHeapAllocationCount(size);
        return .{ .heap_obj = &bytes.obj };
    }

    noinline fn concatBytes(self: *Self, left: *const v.Value, right: *const v.Value) !v.Value {
        const left_len = v.HeapValue.getParentPtr(v.Bytes, left.heap_obj).len;
        const right_len = v.HeapValue.getParentPtr(v.Bytes, right.heap_obj).len;
        const size = v.Bytes.calc_size(left_len + right_len);
        try self.maybeCollectGarbage(size);
        const result = try v.Bytes.initUndefined(self.getCurrentHeapAllocator(), left_len + right_len, .string);
        const data = result.mutableSlice();
        @memcpy(data[0..left_len], v.HeapValue.getParentPtr(v.Bytes, left.heap_obj).slice());
        @memcpy(data[left_len..], v.HeapValue.getParentPtr(v.Bytes, right.heap_obj).slice());
        self.increaseHeapAllocationCount(size);
        return .{ .heap_obj = &result.obj };
    }

    noinline fn joinStrings(self: *Self, parts_value: *v.Value, separator: *const v.Value) !v.Value {
        var parts = joinParts(parts_value);
        const separator_len = v.HeapValue.getParentPtr(v.Bytes, separator.heap_obj).len;
        var total: usize = 0;
        for (parts, 0..) |part, i| {
            if (i > 0) total += separator_len;
            total += v.HeapValue.getParentPtr(v.Bytes, part.heap_obj).len;
        }
        const size = v.Bytes.calc_size(total);
        try self.maybeCollectGarbage(size);
        parts = joinParts(parts_value);
        const separator_bytes = v.HeapValue.getParentPtr(v.Bytes, separator.heap_obj).slice();
        const result = try v.Bytes.initUndefined(self.getCurrentHeapAllocator(), total, .string);
        const data = result.mutableSlice();
        var offset: usize = 0;
        for (parts, 0..) |part, i| {
            if (i > 0) {
                @memcpy(data[offset..][0..separator_bytes.len], separator_bytes);
                offset += separator_bytes.len;
            }
            const part_bytes = v.HeapValue.getParentPtr(v.Bytes, part.heap_obj).slice();
            @memcpy(data[offset..][0..part_bytes.len], part_bytes);
            offset += part_bytes.len;
        }
        self.increaseHeapAllocationCount(size);
        return .{ .heap_obj = &result.obj };
    }

    fn joinParts(parts_value: *v.Value) []const v.Value {
        if (v.HeapValue.followRelocated(parts_value.heap_obj)) |relocated| parts_value.heap_obj = relocated;
        const array = v.HeapValue.getParentPtr(v.Array, parts_value.heap_obj);
        return array.getDataPtr()[0..array.len];
    }

    const OpResult = struct { value: v.Value, has_failed: bool };

    fn bytesArg(args: []const v.Value, index: usize) []const u8 {
        return v.HeapValue.getParentPtr(v.Bytes, args[index].heap_obj).slice();
    }

    fn stringFailure(id: i64) OpResult {
        return .{ .value = .{ .int64 = id }, .has_failed = true };
    }

    fn succeeded(value: v.Value) OpResult {
        return .{ .value = value, .has_failed = false };
    }

    noinline fn runStringOp(self: *Self, op: bc.StringOp, args: []v.Value) !OpResult {
        const text = bytesArg(args, 0);
        return switch (op) {
            .equal => {
                const left = v.HeapValue.getParentPtr(v.Bytes, args[0].heap_obj);
                const right = v.HeapValue.getParentPtr(v.Bytes, args[1].heap_obj);
                if (!args[2].bool) return succeeded(.{ .bool = left.eql(right) });
                return succeeded(.{ .bool = isEqualIgnoringCase(left.slice(), right.slice()) });
            },
            .find => {
                const index = std.mem.indexOf(u8, text, bytesArg(args, 1)) orelse return succeeded(v.NULL_VALUE);
                return succeeded(try self.boxInt(@intCast(index)));
            },
            .contains => succeeded(.{ .bool = std.mem.indexOf(u8, text, bytesArg(args, 1)) != null }),
            .starts_with => succeeded(.{ .bool = std.mem.startsWith(u8, text, bytesArg(args, 1)) }),
            .ends_with => succeeded(.{ .bool = std.mem.endsWith(u8, text, bytesArg(args, 1)) }),
            .count => {
                const needle = bytesArg(args, 1);
                if (needle.len == 0) return succeeded(.{ .int64 = 0 });
                return succeeded(.{ .int64 = @intCast(std.mem.count(u8, text, needle)) });
            },
            .trim => {
                const trimmed = std.mem.trim(u8, text, ascii_whitespace);
                const start = @intFromPtr(trimmed.ptr) - @intFromPtr(text.ptr);
                const result = try self.allocString(trimmed.len);
                @memcpy(result.mutableSlice(), bytesArg(args, 0)[start..][0..trimmed.len]);
                return succeeded(.{ .heap_obj = &result.obj });
            },
            .upper, .lower => {
                const result = try self.allocString(text.len);
                const data = result.mutableSlice();
                @memcpy(data, bytesArg(args, 0));
                changeCase(data, op == .upper);
                return succeeded(.{ .heap_obj = &result.obj });
            },
            .repeat => {
                const count = args[1].int64;
                if (count < 0) return stringFailure(BuiltIn.string_negative_count_id);
                const total = std.math.mul(usize, text.len, @intCast(count)) catch return error.OutOfMemory;
                const result = try self.allocString(total);
                const data = result.mutableSlice();
                const source = bytesArg(args, 0);
                var offset: usize = 0;
                while (offset < total) : (offset += source.len) @memcpy(data[offset..][0..source.len], source);
                return succeeded(.{ .heap_obj = &result.obj });
            },
            .replace => {
                const old = bytesArg(args, 1);
                if (old.len == 0) return succeeded(args[0]);
                const matches = std.mem.count(u8, text, old);
                const total = text.len - matches * old.len + matches * bytesArg(args, 2).len;
                const result = try self.allocString(total);
                _ = std.mem.replace(u8, bytesArg(args, 0), bytesArg(args, 1), bytesArg(args, 2), result.mutableSlice());
                return succeeded(.{ .heap_obj = &result.obj });
            },
            .slice => {
                const start = args[1].int64;
                const end = args[2].int64;
                const is_in_range = start >= 0 and start <= end and end <= text.len;
                if (!is_in_range) return stringFailure(BuiltIn.string_out_of_range_id);
                const start_index: usize = @intCast(start);
                const end_index: usize = @intCast(end);
                if (!isCharBoundary(text, start_index) or !isCharBoundary(text, end_index)) return stringFailure(BuiltIn.string_out_of_range_id);
                const result = try self.allocString(end_index - start_index);
                @memcpy(result.mutableSlice(), bytesArg(args, 0)[start_index..end_index]);
                return succeeded(.{ .heap_obj = &result.obj });
            },
            .split => succeeded(try self.splitString(args)),
            .byte_at => {
                const index = args[1].int64;
                if (index < 0 or index >= text.len) return stringFailure(BuiltIn.string_out_of_range_id);
                return succeeded(.{ .int64 = text[@intCast(index)] });
            },
            .char_at => {
                const wanted = args[1].int64;
                if (wanted < 0) return stringFailure(BuiltIn.string_out_of_range_id);
                var characters = Pieces{ .text = text, .separator = "" };
                var position: i64 = 0;
                while (characters.next()) |character| : (position += 1) {
                    if (position != wanted) continue;
                    const start = @intFromPtr(character.ptr) - @intFromPtr(text.ptr);
                    const result = try self.allocString(character.len);
                    @memcpy(result.mutableSlice(), bytesArg(args, 0)[start..][0..character.len]);
                    return succeeded(.{ .heap_obj = &result.obj });
                }
                return stringFailure(BuiltIn.string_out_of_range_id);
            },
            .from_buffer => {
                if (!std.unicode.utf8ValidateSlice(text)) return stringFailure(BuiltIn.string_invalid_utf8_id);
                const result = try self.allocString(text.len);
                @memcpy(result.mutableSlice(), bytesArg(args, 0));
                return succeeded(.{ .heap_obj = &result.obj });
            },
        };
    }

    fn isCharBoundary(text: []const u8, index: usize) bool {
        if (index == text.len) return true;
        return text[index] & 0xC0 != 0x80;
    }

    const ascii_whitespace = " \t\n\r\x0b\x0c";

    fn isEqualIgnoringCase(left: []const u8, right: []const u8) bool {
        if (left.len != right.len) return false;
        var index: usize = 0;
        while (index < left.len) : (index += 1) {
            const is_latin_pair = left[index] == 0xC3 and right[index] == 0xC3 and index + 1 < left.len;
            if (is_latin_pair) {
                index += 1;
                if (lowerLatinByte(left[index]) != lowerLatinByte(right[index])) return false;
                continue;
            }
            if (std.ascii.toLower(left[index]) != std.ascii.toLower(right[index])) return false;
        }
        return true;
    }

    fn lowerLatinByte(second: u8) u8 {
        const is_upper_latin = second >= 0x80 and second <= 0x9E and second != 0x97;
        return if (is_upper_latin) second + 0x20 else second;
    }

    fn changeCase(data: []u8, is_upper: bool) void {
        var index: usize = 0;
        while (index < data.len) : (index += 1) {
            const byte = data[index];
            if (byte == 0xC3 and index + 1 < data.len) {
                index += 1;
                const second = data[index];
                const is_lower_latin = second >= 0xA0 and second <= 0xBE and second != 0xB7;
                const is_upper_latin = second >= 0x80 and second <= 0x9E and second != 0x97;
                if (is_upper and is_lower_latin) data[index] = second - 0x20;
                if (!is_upper and is_upper_latin) data[index] = second + 0x20;
                continue;
            }
            if (is_upper and std.ascii.isLower(byte)) data[index] = std.ascii.toUpper(byte);
            if (!is_upper and std.ascii.isUpper(byte)) data[index] = std.ascii.toLower(byte);
        }
    }

    fn allocString(self: *Self, len: usize) !*v.Bytes {
        const size = v.Bytes.calc_size(len);
        try self.maybeCollectGarbage(size);
        const result = try v.Bytes.initUndefined(self.getCurrentHeapAllocator(), len, .string);
        self.increaseHeapAllocationCount(size);
        return result;
    }

    const Pieces = struct {
        text: []const u8,
        separator: []const u8,
        index: ?usize = 0,

        fn next(self: *Pieces) ?[]const u8 {
            const start = self.index orelse return null;
            if (self.separator.len == 0) {
                if (start >= self.text.len) {
                    self.index = null;
                    return null;
                }
                var end = start + 1;
                while (!isCharBoundary(self.text, end)) end += 1;
                self.index = end;
                return self.text[start..end];
            }
            if (std.mem.indexOfPos(u8, self.text, start, self.separator)) |position| {
                self.index = position + self.separator.len;
                return self.text[start..position];
            }
            self.index = null;
            return self.text[start..];
        }
    };

    fn splitString(self: *Self, args: []v.Value) !v.Value {
        var piece_count: usize = 0;
        var total: usize = 0;
        var pieces = Pieces{ .text = bytesArg(args, 0), .separator = bytesArg(args, 1) };
        while (pieces.next()) |piece| {
            piece_count += 1;
            total += std.mem.Alignment.of(v.Bytes).forward(v.Bytes.calc_size(piece.len));
        }
        const capacity = @max(piece_count, 1);
        total += v.Array.calc_size(capacity);
        try self.maybeCollectGarbage(total);

        const allocator = self.getCurrentHeapAllocator();
        const array = try v.Array.init(allocator, .string, capacity);
        self.increaseHeapAllocationCount(v.Array.calc_size(capacity));
        pieces = Pieces{ .text = bytesArg(args, 0), .separator = bytesArg(args, 1) };
        while (pieces.next()) |piece| {
            const bytes = try v.Bytes.init(allocator, piece, .string);
            self.increaseHeapAllocationCount(v.Bytes.calc_size(piece.len));
            try array.append(.{ .heap_obj = &bytes.obj });
        }
        return .{ .heap_obj = &array.obj };
    }

    fn boxInt(self: *Self, number: i64) !v.Value {
        const size = v.NullBox.calc_size();
        try self.maybeCollectGarbage(size);
        const box = try v.NullBox.init(self.getCurrentHeapAllocator(), .{ .int64 = number }, .int64);
        self.increaseHeapAllocationCount(size);
        return .{ .heap_obj = &box.obj };
    }

    noinline fn convertText(text_value: v.Value, kind: v.ValueType) OpResult {
        const text = v.HeapValue.getParentPtr(v.Bytes, text_value.heap_obj).slice();
        const value: v.Value = switch (kind) {
            .int64 => .{ .int64 = parseIntText(text) catch |failure| return conversionFailure(failure) },
            .float64 => .{ .float64 = parseFloatText(text) catch |failure| return conversionFailure(failure) },
            else => unreachable,
        };
        return .{ .value = value, .has_failed = false };
    }

    fn conversionFailure(failure: ConversionError) OpResult {
        const id = switch (failure) {
            error.Invalid => BuiltIn.conversion_invalid_id,
            error.Overflow => BuiltIn.conversion_overflow_id,
        };
        return .{ .value = .{ .int64 = id }, .has_failed = true };
    }

    const ConversionError = error{ Invalid, Overflow };

    fn skipDigits(text: []const u8, start: usize) usize {
        var index = start;
        while (index < text.len and std.ascii.isDigit(text[index])) index += 1;
        return index;
    }

    fn parseIntText(text: []const u8) ConversionError!i64 {
        const digits_start: usize = if (text.len > 0 and text[0] == '-') 1 else 0;
        const digits_end = skipDigits(text, digits_start);
        if (digits_end == digits_start or digits_end != text.len) return error.Invalid;
        return std.fmt.parseInt(i64, text, 10) catch |failure| switch (failure) {
            error.Overflow => error.Overflow,
            error.InvalidCharacter => error.Invalid,
        };
    }

    fn parseFloatText(text: []const u8) ConversionError!f64 {
        const digits_start: usize = if (text.len > 0 and text[0] == '-') 1 else 0;
        var end = skipDigits(text, digits_start);
        if (end == digits_start) return error.Invalid;
        if (end < text.len and text[end] == '.') {
            const fraction_end = skipDigits(text, end + 1);
            if (fraction_end == end + 1) return error.Invalid;
            end = fraction_end;
        }
        if (end != text.len) return error.Invalid;
        const number = std.fmt.parseFloat(f64, text) catch return error.Invalid;
        if (std.math.isInf(number)) return error.Overflow;
        return number;
    }

    noinline fn indexOutOfBounds(index: i64, len: usize) RunTimeError {
        @branchHint(.cold);
        if (@import("builtin").is_test) return RunTimeError.IndexOutOfBounds;
        std.debug.print("error: index {d} is out of bounds for length {d}\n", .{ index, len });
        return RunTimeError.IndexOutOfBounds;
    }

    noinline fn negativeBufferSize(len: i64) RunTimeError {
        @branchHint(.cold);
        if (@import("builtin").is_test) return RunTimeError.NegativeBufferSize;
        std.debug.print("error: buffer size can't be negative: {d}\n", .{len});
        return RunTimeError.NegativeBufferSize;
    }

    noinline fn byteOutOfRange(number: i64) RunTimeError {
        @branchHint(.cold);
        if (@import("builtin").is_test) return RunTimeError.ByteOutOfRange;
        std.debug.print("error: value {d} doesn't fit in byte (0..255)\n", .{number});
        return RunTimeError.ByteOutOfRange;
    }

    noinline fn panicUnhandledError(self: *Self, err_value: v.Value) RunTimeError {
        @branchHint(.cold);
        if (@import("builtin").is_test) return RunTimeError.UnhandledError;
        std.debug.print("error: unhandled error {s}\n", .{self.program.error_names[@intCast(err_value.int64)]});
        return RunTimeError.UnhandledError;
    }

    fn initStaticStore(self: *Self) !void {
        if (self.program.static_count > 0) {
            self.static_store = try self.allocator.alloc(v.Value, self.program.static_count);
            @memset(self.static_store, v.NULL_VALUE);
        }
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

    fn getArrayFromHeapValuePtr(stack: []v.Value, stack_base: usize, reg_idx: u8, ptr: *v.HeapValue) *v.Array {
        const refreshed_pointer = refreshStackHeapPtr(stack, stack_base, reg_idx, ptr);
        const array = v.HeapValue.getParentPtr(v.Array, refreshed_pointer);
        return array;
    }

    fn refreshStackHeapPtr(stack: []v.Value, stack_base: usize, reg_idx: u8, ptr: *v.HeapValue) *v.HeapValue {
        const follow = v.HeapValue.followRelocated(ptr);

        if (follow) |relocated| {
            stack[stack_base + reg_idx].heap_obj = relocated;
        }

        return follow orelse ptr;
    }

    fn increaseHeapAllocationCount(self: *Self, bytes: usize) void {
        self.heap.allocated += bytes;
        self.heap.object_count += 1;
    }

    fn maybeCollectGarbage(self: *Self, bytes: usize) !void {
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

        for (0..self.program.static_count) |i| {
            const word_idx = i >> 6;
            const bit_idx = i & 63;
            if (word_idx < self.program.static_heap_bitmap.len and
                self.program.static_heap_bitmap[word_idx] & (@as(u64, 1) << @intCast(bit_idx)) != 0)
            {
                try self.copyHeapValue(new_alloc, &self.static_store[i]);
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
        var current = value.heap_obj;
        while (current.relocated) |rel| {
            current = rel;
        }

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

    fn getCurrentHeapAllocator(self: *Self) std.mem.Allocator {
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
