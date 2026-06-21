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

    pub fn init(program: *const bc.Program) VM {
        return .{
            .allocator = std.heap.page_allocator,
            .frames = undefined,
            .frame_count = 0,
            .program = program,
            .stack = undefined,
            .heap = .{
                .arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
                .ptr_store = std.AutoHashMap(*v.HeapValue, void).init(std.heap.page_allocator),
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
                    try self.maybeCollectGarbage(v.String.calc_size(str_data.len));
                    var string_obj = try v.String.init(self.heap.arena.allocator(), str_data);
                    try self.allocate(&string_obj.obj, v.String.calc_size(str_data.len));
                    stack[stack_base + inst.a] = .{ .heap_obj = &string_obj.obj };
                },

                .LOAD => {
                    stack[stack_base + inst.a] = stack[stack_base + inst.b];
                },

                .ARRAY_INIT => {
                    try self.maybeCollectGarbage(v.Array.calc_size(inst.c));
                    var new_array = try v.Array.init(self.heap.arena.allocator(), @enumFromInt(inst.b), inst.c);
                    try self.allocate(&new_array.obj, v.Array.calc_size(new_array.capacity));
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
                        try self.maybeCollectGarbage(v.Array.calc_size(array.calcNewCapacity()));
                        array = getArrayFromHeapValuePtr(stack, stack_base, inst.a, stack[stack_base + inst.a].heap_obj);
                        const old_size = v.Array.calc_size(array.capacity);
                        array = try array.resize(self.heap.arena.allocator());
                        stack[stack_base + inst.a] = .{ .heap_obj = &array.obj };
                        const new_size = v.Array.calc_size(array.capacity);

                        self.heap.allocated -= old_size;
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

                .ARRAY_POP => {
                    const arr_ptr = stack[stack_base + inst.b].heap_obj;
                    var array = getArrayFromHeapValuePtr(stack, stack_base, inst.b, arr_ptr);
                    const value = try array.pop();
                    stack[stack_base + inst.a] = value;
                },

                .STRUCT_INIT => {
                    try self.maybeCollectGarbage(v.Struct.calc_size(inst.b));
                    var new_struct = try v.Struct.init(self.heap.arena.allocator(), inst.b);
                    try self.allocate(&new_struct.obj, v.Struct.calc_size(new_struct.field_count));
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

    fn getArrayFromHeapValuePtr(stack: []v.Value, stack_base: usize, reg_idx: u8, ptr: *v.HeapValue) *v.Array {
        const refreshed_pointer = refreshStackHeapPtr(stack, stack_base, reg_idx, ptr);
        const array = v.HeapValue.getParentPtr(v.Array, refreshed_pointer);
        return array;
    }

    fn refreshStackHeapPtr(stack: []v.Value, stack_base: usize, reg_idx: u8, ptr: *v.HeapValue) *v.HeapValue {
        const follow_forward = v.HeapValue.followForward(ptr);

        if (follow_forward) |forward| {
            stack[stack_base + reg_idx] = .{ .heap_obj = forward };
        }

        return follow_forward orelse ptr;
    }

    fn allocate(self: *Self, heap_obj: *v.HeapValue, bytes: usize) !void {
        try self.heap.ptr_store.put(heap_obj, {});
        self.heap.allocated += bytes;
        self.heap.object_count += 1;
    }

    fn maybeCollectGarbage(self: *Self, bytes: usize) !void {
        if (self.heap.allocated + bytes > self.heap.threshold) {
            std.log.debug("clearing {d} {d}", .{ self.heap.allocated + bytes, self.heap.threshold });
            try self.collectGarbage();
        }
    }

    fn collectGarbage(self: *Self) !void {
        const active_frames = self.frames[0..self.frame_count];
        var stack_top: usize = 0;
        for (active_frames) |frame| {
            stack_top = @max(stack_top, frame.stack_offset + frame.function.kind.dusk.num_registers);
        }

        var new_heap = Heap{
            .arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
            .ptr_store = std.AutoHashMap(*v.HeapValue, void).init(self.allocator),
        };

        const new_alloc = new_heap.arena.allocator();

        var copied_objects_queue = try std.ArrayList(*v.HeapValue).initCapacity(self.allocator, self.heap.object_count);
        defer copied_objects_queue.deinit(self.allocator);

        const active_stack = self.stack[0..stack_top];
        for (active_stack) |*value| {
            try self.copyValue(new_alloc, value, &copied_objects_queue, &new_heap);
        }

        for (self.static_store) |*value| {
            try self.copyValue(new_alloc, value, &copied_objects_queue, &new_heap);
        }

        while (copied_objects_queue.pop()) |obj| {
            switch (obj.kind) {
                .array => {
                    const array = v.HeapValue.getParentPtr(v.Array, obj);
                    switch (array.kind) {
                        .string, .@"struct", .array => {
                            const items = array.getDataPtr()[0..array.len];
                            for (items) |*item| {
                                try self.copyValue(new_alloc, item, &copied_objects_queue, &new_heap);
                            }
                        },
                        else => {},
                    }
                },
                .@"struct" => {
                    const s = v.HeapValue.getParentPtr(v.Struct, obj);
                    const fields = s.getDataPtr()[0..s.field_count];
                    for (fields) |*field| {
                        try self.copyValue(new_alloc, field, &copied_objects_queue, &new_heap);
                    }
                },
                else => {},
            }
        }

        new_heap.threshold = @max(new_heap.allocated * 2, 1000000);
        self.heap.arena.deinit();
        self.heap.ptr_store.deinit();
        self.heap = new_heap;
    }

    fn copyValue(self: *Self, new_alloc: std.mem.Allocator, value: *v.Value, alive_queue: *std.ArrayList(*v.HeapValue), heap: *Heap) !void {
        if (self.heap.ptr_store.contains(value.heap_obj)) {
            if (value.heap_obj.forward) |fwd| {
                value.heap_obj = fwd;
            } else {
                const new_ptr = try cloneHeapObj(new_alloc, value.heap_obj);
                value.heap_obj.forward = new_ptr;
                value.heap_obj = new_ptr;
                if (new_ptr.kind != .string) {
                    try alive_queue.append(self.allocator, new_ptr);
                }
                try heap.ptr_store.put(new_ptr, {});
                heap.object_count += 1;
                heap.allocated += heapObjectSize(new_ptr);
            }
        }
    }

    fn cloneHeapObj(allocator: std.mem.Allocator, obj_ptr: *v.HeapValue) !*v.HeapValue {
        return switch (obj_ptr.kind) {
            .array => &(try v.HeapValue.getParentPtr(v.Array, obj_ptr).clone(allocator)).obj,
            .@"struct" => &(try v.HeapValue.getParentPtr(v.Struct, obj_ptr).clone(allocator)).obj,
            .string => &(try v.HeapValue.getParentPtr(v.String, obj_ptr).clone(allocator)).obj,
        };
    }

    fn heapObjectSize(ptr: *v.HeapValue) usize {
        return switch (ptr.kind) {
            .array => v.Array.calc_size(v.HeapValue.getParentPtr(v.Array, ptr).capacity),
            .@"struct" => v.Struct.calc_size(v.HeapValue.getParentPtr(v.Struct, ptr).field_count),
            .string => v.String.calc_size(v.HeapValue.getParentPtr(v.String, ptr).len),
        };
    }
};

const Heap = struct {
    arena: std.heap.ArenaAllocator,
    ptr_store: std.AutoHashMap(*v.HeapValue, void),
    object_count: usize = 0,
    allocated: usize = 0,
    threshold: usize = 1000000,
};

const CallFrame = struct {
    function: *const bc.Function,
    cur_inst: usize,
    stack_offset: usize,
};
