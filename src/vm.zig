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
        try self.initStaticStore();
        self.initMainFrame();

        const stack = self.stack[0..];
        var current_frame = &self.frames[self.frame_count - 1];
        var ip: usize = 0;
        var instructions = current_frame.function.kind.dusk.instructions;
        var stack_base: usize = 0;

        while (true) {
            const inst = instructions[ip];
            ip += 1;

            switch (inst.op) {
                .LOAD_CONST => {
                    stack[stack_base + inst.a] = current_frame.function.kind.dusk.constants[inst.bEx()];
                },

                .LOAD_STRING => {
                    const str_data = current_frame.function.kind.dusk.string_constants[inst.bEx()];
                    var string_obj = try v.String.init(self.allocator, str_data);
                    try self.heap.allocate(&string_obj.obj);
                    stack[stack_base + inst.a] = .{ .heap_value = &string_obj.obj };
                },

                .LOAD => {
                    stack[stack_base + inst.a] = stack[stack_base + inst.b];
                },

                .ARRAY_INIT => {
                    var new_array = try v.Array.init(self.allocator, @enumFromInt(inst.b), inst.c);
                    try self.heap.allocate(&new_array.obj);
                    stack[stack_base + inst.a] = .{ .heap_value = &new_array.obj };
                },

                .ARRAY_LOAD => {
                    const arr_ptr = stack[stack_base + inst.b].heap_value;
                    const array = self.getArrayFromHeapValuePtr(stack, stack_base, inst.b, arr_ptr);
                    const idx = stack[stack_base + inst.c];
                    const value = try array.get(@intCast(idx.int64));
                    stack[stack_base + inst.a] = value;
                },

                .ARRAY_APPEND => {
                    const value = stack[stack_base + inst.b];
                    const arr_ptr = stack[stack_base + inst.a].heap_value;
                    var array = self.getArrayFromHeapValuePtr(stack, stack_base, inst.a, arr_ptr);

                    if (array.needsResize()) {
                        array = try array.resize(self.allocator);
                        stack[stack_base + inst.a] = .{ .heap_value = &array.obj };
                    }

                    try array.append(value);
                },

                .ARRAY_STORE => {
                    const arr_ptr = stack[stack_base + inst.a].heap_value;
                    var array = self.getArrayFromHeapValuePtr(stack, stack_base, inst.a, arr_ptr);
                    const idx = stack[stack_base + inst.b];
                    const value = stack[stack_base + inst.c];
                    try array.set(@intCast(idx.int64), value);
                },

                .ARRAY_LEN => {
                    const arr_ptr = stack[stack_base + inst.b].heap_value;
                    const array = self.getArrayFromHeapValuePtr(stack, stack_base, inst.b, arr_ptr);
                    stack[stack_base + inst.a] = .{ .int64 = @intCast(array.len) };
                },

                .ARRAY_POP => {
                    const arr_ptr = stack[stack_base + inst.b].heap_value;
                    var array = self.getArrayFromHeapValuePtr(stack, stack_base, inst.b, arr_ptr);
                    const value = try array.pop();
                    stack[stack_base + inst.a] = value;
                },

                .STRUCT_INIT => {
                    var new_struct = try v.Struct.init(self.allocator, inst.b);
                    try self.heap.allocate(&new_struct.obj);
                    stack[stack_base + inst.a] = .{ .heap_value = &new_struct.obj };
                },

                .STRUCT_LOAD => {
                    const struct_ptr = stack[stack_base + inst.b].heap_value;
                    const struct_vl = v.HeapValue.getParentPtr(v.Struct, struct_ptr);
                    stack[stack_base + inst.a] = struct_vl.get(inst.c);
                },

                .STRUCT_STORE => {
                    const struct_ptr = stack[stack_base + inst.a].heap_value;
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
                            current_frame.cur_inst = ip;
                            const new_offset = stack_base + inst.a + 1;
                            self.frames[self.frame_count] = .{
                                .function = func,
                                .cur_inst = 0,
                                .stack_offset = new_offset,
                            };
                            self.frame_count += 1;
                            current_frame = &self.frames[self.frame_count - 1];
                            ip = 0;
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
                    ip = inst.aEx();
                },

                .JUMP_IF_FALSE => {
                    if (!stack[stack_base + inst.a].bool) {
                        ip = inst.bEx();
                    }
                },

                .RETURN => {
                    self.frame_count -= 1;
                    if (self.frame_count == 0) break;
                    stack[stack_base - 1] = stack[stack_base + inst.a];
                    current_frame = &self.frames[self.frame_count - 1];
                    ip = current_frame.cur_inst;
                    instructions = current_frame.function.kind.dusk.instructions;
                    stack_base = current_frame.stack_offset;
                },
                else => unreachable,
            }
        }
    }

    fn initStaticStore(self: *Self) !void {
        if (self.program.static_count > 0) {
            self.static_store = try self.allocator.alloc(v.Value, self.program.static_count);
            @memset(self.static_store, v.Value{ .null = {} });
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

    fn getArrayFromHeapValuePtr(self: *Self, stack: []v.Value, stack_base: usize, reg_idx: u8, ptr: *v.HeapValue) *v.Array {
        const refreshed_pointer = self.refreshStackHeapPtr(stack, stack_base, reg_idx, ptr);
        const array = v.HeapValue.getParentPtr(v.Array, refreshed_pointer);
        return array;
    }

    fn refreshStackHeapPtr(_: *Self, stack: []v.Value, stack_base: usize, reg_idx: u8, ptr: *v.HeapValue) *v.HeapValue {
        const follow_forward = v.HeapValue.followForward(ptr);

        if (follow_forward) |forward| {
            stack[stack_base + reg_idx] = .{ .heap_value = forward };
        }

        return follow_forward orelse ptr;
    }
};

const CallFrame = struct {
    function: *const bc.Function,
    cur_inst: usize,
    stack_offset: usize,
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
