const std = @import("std");
const bc = @import("bytecode.zig");
const v = @import("value.zig");

pub const VM = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    program: *const bc.Program,
    frames: std.ArrayList(CallFrame),
    stack: [16383]v.Value,
    heap: Heap,

    pub fn init(allocator: std.mem.Allocator, program: *const bc.Program) VM {
        return .{
            .allocator = allocator,
            .frames = .empty,
            .program = program,
            .stack = undefined,
            .heap = .{
                .allocator = allocator,
                .head = null,
            },
        };
    }

    pub fn run(self: *Self) !void {
        const main_fn = self.program.functions[self.program.main_func_index];

        try self.frames.append(self.allocator, CallFrame{
            .function = &main_fn,
            .cur_inst = 0,
            .stack_offset = 0,
        });

        var current_frame = &self.frames.items[self.frames.items.len - 1];

        const stack = self.stack[0..];

        while (current_frame.cur_inst < current_frame.function.kind.dusk.instructions.len) : (current_frame.cur_inst += 1) {
            current_frame = &self.frames.items[self.frames.items.len - 1];

            const inst = current_frame.function.kind.dusk.instructions[current_frame.cur_inst];

            switch (inst.op) {
                .LOAD_CONST => {
                    current_frame.setVar(stack, inst.a, current_frame.function.kind.dusk.constants[inst.bEx()]);
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
                    const func = self.program.functions[inst.bEx()];
                    switch (func.kind) {
                        .dusk => try self.callFunction(&func, current_frame.stack_offset, inst.a),
                        .host => |b| {
                            const args = stack[current_frame.stack_offset + inst.a + 1 ..][0..b.num_args];
                            stack[current_frame.stack_offset + inst.a] = b.func(args);
                        },
                        .@"inline" => unreachable,
                    }
                },

                .JUMP => {
                    current_frame.cur_inst = inst.aEx() - 1;
                },

                .JUMP_IF_FALSE => {
                    const condition = current_frame.getVar(stack, inst.a);
                    if (!condition.bool) {
                        current_frame.cur_inst = inst.bEx() - 1;
                    }
                },

                .TYPE => {
                    current_frame.setVar(stack, inst.a, v.Value{ .int64 = inst.b });
                },

                .RETURN => {
                    self.stack[current_frame.stack_offset - 1] = current_frame.getVar(stack, inst.a);
                    _ = self.frames.pop();
                    current_frame = &self.frames.items[self.frames.items.len - 1];
                    current_frame.cur_inst -= 1;
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

    fn callFunction(self: *Self, func: *const bc.Function, current_stack_offset: usize, target_reg: u8) !void {
        try self.frames.append(self.allocator, CallFrame{
            .function = func,
            .cur_inst = 0,
            .stack_offset = current_stack_offset + target_reg + 1,
        });
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
