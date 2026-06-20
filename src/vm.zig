const std = @import("std");
const bc = @import("bytecode.zig");
const v = @import("value.zig");

pub const VM = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    program: *const bc.Program,
    frames: [256]CallFrame,
    frame_count: usize,
    stack: [16383]v.Value,
    heap: Heap,
    static_store: []v.Value,

    pub fn init(allocator: std.mem.Allocator, program: *const bc.Program) VM {
        return .{
            .allocator = allocator,
            .frames = undefined,
            .frame_count = 0,
            .program = program,
            .stack = undefined,
            .heap = .{
                .allocator = allocator,
                .head = null,
            },
            .static_store = &.{},
        };
    }

    pub fn run(self: *Self) !void {
        if (self.program.static_count > 0) {
            self.static_store = try self.allocator.alloc(v.Value, self.program.static_count);
            @memset(self.static_store, v.Value{ .null = {} });
        }

        const main_fn = self.program.functions[self.program.main_func_index];

        self.frames[0] = CallFrame{
            .function = &main_fn,
            .cur_inst = 0,
            .stack_offset = 0,
        };
        self.frame_count = 1;

        var current_frame = &self.frames[self.frame_count - 1];

        const stack = self.stack[0..];

        while (self.frame_count > 0) {
            current_frame = &self.frames[self.frame_count - 1];

            if (current_frame.cur_inst >= current_frame.function.kind.dusk.instructions.len) {
                self.frame_count -= 1;
                continue;
            }

            const inst = current_frame.function.kind.dusk.instructions[current_frame.cur_inst];
            current_frame.cur_inst += 1;

            switch (inst.op) {
                .LOAD_CONST => {
                    current_frame.setVar(stack, inst.a, current_frame.function.kind.dusk.constants[inst.bEx()]);
                },

                .LOAD_STRING => {
                    const str_data = current_frame.function.kind.dusk.string_constants[inst.bEx()];
                    var string_obj = try v.String.init(self.allocator, str_data);
                    try self.heap.allocate(&string_obj.obj);
                    current_frame.setVar(stack, inst.a, .{ .heap_value = &string_obj.obj });
                },

                .LOAD => {
                    current_frame.setVar(stack, inst.a, current_frame.getVar(stack, inst.b));
                },

                .ARRAY_INIT => {
                    var new_array = try v.Array.init(self.allocator, @enumFromInt(inst.b), inst.c);
                    try self.heap.allocate(&new_array.obj);

                    current_frame.setVar(
                        stack,
                        inst.a,
                        .{
                            .heap_value = &new_array.obj,
                        },
                    );
                },

                .ARRAY_LOAD => {
                    const arr_ptr = current_frame.getVar(stack, inst.b).heap_value;
                    const array = self.getArrayFromHeapValuePtr(current_frame, stack, inst.b, arr_ptr);

                    const idx = current_frame.getVar(stack, inst.c);
                    const value = try array.get(@intCast(idx.int64));
                    current_frame.setVar(stack, inst.a, value);
                },

                .ARRAY_APPEND => {
                    const value = current_frame.getVar(stack, inst.b);
                    const arr_ptr = current_frame.getVar(stack, inst.a).heap_value;
                    var array = self.getArrayFromHeapValuePtr(current_frame, stack, inst.b, arr_ptr);

                    if (array.needsResize()) {
                        const new_array_ptr = try array.resize(self.allocator);
                        current_frame.setVar(stack, inst.a, .{ .heap_value = &new_array_ptr.obj });
                    }

                    try array.append(value);
                },

                .ARRAY_STORE => {
                    const arr_ptr = current_frame.getVar(stack, inst.a).heap_value;
                    var array = self.getArrayFromHeapValuePtr(current_frame, stack, inst.a, arr_ptr);
                    const idx = current_frame.getVar(stack, inst.b);
                    const value = current_frame.getVar(stack, inst.c);

                    try array.set(@intCast(idx.int64), value);
                },

                .ARRAY_LEN => {
                    const arr_ptr = current_frame.getVar(stack, inst.b).heap_value;
                    const array = self.getArrayFromHeapValuePtr(current_frame, stack, inst.b, arr_ptr);
                    current_frame.setVar(stack, inst.a, .{ .int64 = @intCast(array.len) });
                },

                .ARRAY_POP => {
                    const arr_ptr = current_frame.getVar(stack, inst.b).heap_value;
                    var array = self.getArrayFromHeapValuePtr(current_frame, stack, inst.b, arr_ptr);
                    const value = try array.pop();
                    current_frame.setVar(stack, inst.a, value);
                },

                .STRUCT_INIT => {
                    var new_struct = try v.Struct.init(self.allocator, inst.b);
                    try self.heap.allocate(&new_struct.obj);

                    current_frame.setVar(
                        stack,
                        inst.a,
                        .{
                            .heap_value = &new_struct.obj,
                        },
                    );
                },

                .STRUCT_LOAD => {
                    const struct_ptr = current_frame.getVar(stack, inst.b).heap_value;
                    const struct_vl = v.HeapValue.getParentPtr(v.Struct, struct_ptr);

                    current_frame.setVar(
                        stack,
                        inst.a,
                        struct_vl.get(inst.c),
                    );
                },

                .STRUCT_STORE => {
                    const struct_ptr = current_frame.getVar(stack, inst.a).heap_value;
                    const struct_vl = v.HeapValue.getParentPtr(v.Struct, struct_ptr);
                    const vl = current_frame.getVar(stack, inst.c);
                    struct_vl.set(inst.b, vl);
                },

                .STATIC_LOAD => {
                    current_frame.setVar(stack, inst.a, self.static_store[inst.bEx()]);
                },

                .STATIC_STORE => {
                    self.static_store[inst.bEx()] = current_frame.getVar(stack, inst.a);
                },

                .I_ADD => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .int64 = current_frame.getVar(stack, inst.b).int64 + current_frame.getVar(stack, inst.c).int64,
                    });
                },

                .I_SUB => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .int64 = current_frame.getVar(stack, inst.b).int64 - current_frame.getVar(stack, inst.c).int64,
                    });
                },

                .I_MULT => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .int64 = current_frame.getVar(stack, inst.b).int64 * current_frame.getVar(stack, inst.c).int64,
                    });
                },

                .TRUNC_DIV => {
                    current_frame.setVar(stack, inst.a, v.Value{ .int64 = @divTrunc(
                        current_frame.getVar(stack, inst.b).int64,
                        current_frame.getVar(stack, inst.c).int64,
                    ) });
                },

                .I_MOD => {
                    current_frame.setVar(stack, inst.a, v.Value{ .int64 = @mod(
                        current_frame.getVar(stack, inst.b).int64,
                        current_frame.getVar(stack, inst.c).int64,
                    ) });
                },

                .I_EQ => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .bool = current_frame.getVar(stack, inst.b).int64 == current_frame.getVar(stack, inst.c).int64,
                    });
                },

                .I_NEQ => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .bool = current_frame.getVar(stack, inst.b).int64 != current_frame.getVar(stack, inst.c).int64,
                    });
                },

                .I_LT => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .bool = current_frame.getVar(stack, inst.b).int64 < current_frame.getVar(stack, inst.c).int64,
                    });
                },

                .I_LE => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .bool = current_frame.getVar(stack, inst.b).int64 <= current_frame.getVar(stack, inst.c).int64,
                    });
                },

                .I_GT => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .bool = current_frame.getVar(stack, inst.b).int64 > current_frame.getVar(stack, inst.c).int64,
                    });
                },

                .I_GE => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .bool = current_frame.getVar(stack, inst.b).int64 >= current_frame.getVar(stack, inst.c).int64,
                    });
                },

                .I_NEG => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .int64 = -current_frame.getVar(stack, inst.b).int64,
                    });
                },

                .F_ADD => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .float64 = current_frame.getVar(stack, inst.b).float64 + current_frame.getVar(stack, inst.c).float64,
                    });
                },

                .F_SUB => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .float64 = current_frame.getVar(stack, inst.b).float64 - current_frame.getVar(stack, inst.c).float64,
                    });
                },

                .F_MULT => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .float64 = current_frame.getVar(stack, inst.b).float64 * current_frame.getVar(stack, inst.c).float64,
                    });
                },

                .F_DIV => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .float64 = current_frame.getVar(stack, inst.b).float64 / current_frame.getVar(stack, inst.c).float64,
                    });
                },

                .F_MOD => {
                    current_frame.setVar(stack, inst.a, v.Value{ .float64 = @mod(
                        current_frame.getVar(stack, inst.b).float64,
                        current_frame.getVar(stack, inst.c).float64,
                    ) });
                },

                .F_EQ => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .bool = current_frame.getVar(stack, inst.b).float64 == current_frame.getVar(stack, inst.c).float64,
                    });
                },

                .F_NEQ => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .bool = current_frame.getVar(stack, inst.b).float64 != current_frame.getVar(stack, inst.c).float64,
                    });
                },

                .F_LT => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .bool = current_frame.getVar(stack, inst.b).float64 < current_frame.getVar(stack, inst.c).float64,
                    });
                },

                .F_LE => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .bool = current_frame.getVar(stack, inst.b).float64 <= current_frame.getVar(stack, inst.c).float64,
                    });
                },

                .F_GT => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .bool = current_frame.getVar(stack, inst.b).float64 > current_frame.getVar(stack, inst.c).float64,
                    });
                },

                .F_GE => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .bool = current_frame.getVar(stack, inst.b).float64 >= current_frame.getVar(stack, inst.c).float64,
                    });
                },

                .F_NEG => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .float64 = -current_frame.getVar(stack, inst.b).float64,
                    });
                },

                .B_AND => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .bool = current_frame.getVar(stack, inst.b).bool and current_frame.getVar(stack, inst.c).bool,
                    });
                },

                .B_OR => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .bool = current_frame.getVar(stack, inst.b).bool or current_frame.getVar(stack, inst.c).bool,
                    });
                },

                .B_EQ => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .bool = current_frame.getVar(stack, inst.b).bool == current_frame.getVar(stack, inst.c).bool,
                    });
                },

                .B_NEQ => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .bool = current_frame.getVar(stack, inst.b).bool != current_frame.getVar(stack, inst.c).bool,
                    });
                },

                .B_NOT => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .bool = !current_frame.getVar(stack, inst.b).bool,
                    });
                },

                .I_TO_F => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .float64 = @floatFromInt(current_frame.getVar(stack, inst.a).int64),
                    });
                },

                .F_TO_I => {
                    current_frame.setVar(stack, inst.a, v.Value{
                        .int64 = @trunc(current_frame.getVar(stack, inst.a).float64),
                    });
                },

                .CALL => {
                    const func = &self.program.functions[inst.bEx()];
                    switch (func.kind) {
                        .dusk => self.callFunction(func, current_frame.stack_offset, inst.a),
                        .host => |b| {
                            const args = stack[current_frame.stack_offset + inst.a + 1 ..][0..b.num_args];
                            stack[current_frame.stack_offset + inst.a] = b.func(args);
                        },
                        .@"inline" => unreachable,
                    }
                },

                .JUMP => {
                    current_frame.cur_inst = inst.aEx();
                },

                .JUMP_IF_FALSE => {
                    const condition = current_frame.getVar(stack, inst.a);
                    if (!condition.bool) {
                        current_frame.cur_inst = inst.bEx();
                    }
                },

                .RETURN => {
                    self.stack[current_frame.stack_offset - 1] = current_frame.getVar(stack, inst.a);
                    self.frame_count -= 1;
                },
                else => {},
            }
        }
    }

    fn getArrayFromHeapValuePtr(self: *Self, frame: *CallFrame, stack: []v.Value, reg_idx: u8, ptr: *v.HeapValue) *v.Array {
        const refreshed_pointer = self.refreshStackHeapPtr(frame, stack, reg_idx, ptr);
        const array = v.HeapValue.getParentPtr(v.Array, refreshed_pointer);
        return array;
    }

    fn refreshStackHeapPtr(_: *Self, frame: *CallFrame, stack: []v.Value, reg_idx: u8, ptr: *v.HeapValue) *v.HeapValue {
        const follow_forward = v.HeapValue.followForward(ptr);

        if (follow_forward) |forward| {
            frame.setVar(stack, reg_idx, .{ .heap_value = forward });
        }

        return follow_forward orelse ptr;
    }

    fn callFunction(self: *Self, func: *const bc.Function, current_stack_offset: usize, target_reg: u8) void {
        self.frames[self.frame_count] = CallFrame{
            .function = func,
            .cur_inst = 0,
            .stack_offset = current_stack_offset + target_reg + 1,
        };
        self.frame_count += 1;
    }
};

const CallFrame = struct {
    const Self = @This();

    function: *const bc.Function,
    cur_inst: usize,
    stack_offset: usize,

    pub fn getStackIndex(self: *Self, index: u8) usize {
        return self.stack_offset + index;
    }

    pub fn getVar(self: *Self, stack: []v.Value, index: u8) v.Value {
        return stack[self.getStackIndex(index)];
    }

    pub fn setVar(self: *Self, stack: []v.Value, index: u8, value: v.Value) void {
        stack[self.getStackIndex(index)] = value;
    }
};

const Heap = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    head: ?*v.HeapValue,

    pub fn allocate(self: *Self, heap_value: *v.HeapValue) !void {
        const current_head = self.head;
        if (current_head) |head| {
            head.next = heap_value;
        }
        self.head = heap_value;
    }
};
