const std = @import("std");
const bc = @import("bytecode.zig");
const v = @import("value.zig");
const util = @import("util.zig");
const RuntimeError = @import("error.zig").RuntimeError;
const RuntimeErrorReporter = @import("error.zig").RuntimeErrorReporter;
const format = @import("format.zig");
const TaskState = @import("scheduler.zig").TaskState;
const Scheduler = @import("scheduler.zig").Scheduler;
const EventLoop = @import("event_loop.zig").EventLoop;

pub const VM = struct {
    const Self = @This();
    const MIN_GC_THRESHOLD = 1000000; //1MB
    const MAX_CALL_DEPTH = 100_000;
    const STACK_SLOTS = 4_000_000;
    const MAX_FRAME_REGISTERS = 256;

    allocator: std.mem.Allocator,
    task_allocator: std.mem.Allocator,
    program: *const bc.Program,
    current_task: ?*TaskState = null,
    scheduler: Scheduler,
    event_loop: EventLoop,
    frames: []CallFrame,
    frame_count: usize,
    failed_builtin: ?[]const u8 = null,
    stack: []v.Value,
    heap: Heap,
    scratch: std.Io.Writer.Allocating,
    stdout: ?*std.Io.Writer,

    pub fn init(program: *const bc.Program, stdout: ?*std.Io.Writer) VM {
        return .{
            .allocator = std.heap.page_allocator,
            .task_allocator = std.heap.c_allocator,
            .scheduler = .{ .allocator = std.heap.c_allocator },
            .event_loop = .{ .allocator = std.heap.c_allocator },
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
        self.event_loop.deinit();
        self.scheduler.deinit();
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
        const main_closure: *v.Closure = @constCast(&self.program.static_closures[self.program.main_func_index]);
        _ = try self.scheduler.spawn(main_closure, .null);
        self.runScheduler() catch |failure| {
            self.reportWhere(failure);
            self.saveCurrentTask();
            return failure;
        };
    }

    fn runScheduler(self: *Self) !void {
        while (true) {
            const batch = self.scheduler.readyCount();
            if (batch > 0) {
                for (0..batch) |_| {
                    try self.runTask(self.scheduler.popReady().?);
                }
                if (self.event_loop.hasPending()) {
                    try self.event_loop.poll();
                }
                continue;
            }
            if (self.event_loop.hasPending()) {
                try self.event_loop.waitOnce();
                continue;
            }
            if (self.scheduler.hasUnfinished()) {
                return RuntimeErrorReporter.allTasksWaiting();
            }
            break;
        }
        try self.reportUnhandledTaskErrors();
    }

    fn runTask(self: *Self, task: *TaskState) !void {
        self.enterTask(task);
        try self.execute();
        self.leaveTask();
        if (task.frame_count == 0) {
            try self.scheduler.finish(task);
        }
    }

    fn enterTask(self: *Self, task: *TaskState) void {
        self.current_task = task;
        self.stack = task.stack;
        self.frames = task.frames;
        self.frame_count = task.frame_count;
        task.status = .running;
        const delivery = task.pending orelse return;
        task.pending = null;
        const frame = &task.frames[task.frame_count - 1];
        const call = frame.function.kind.dusk.instructions[frame.cur_inst - 1];
        task.stack[frame.stack_offset + call.a] = delivery.value;
        if (delivery.has_failed) {
            frame.cur_inst += 1;
        }
    }

    fn leaveTask(self: *Self) void {
        self.saveCurrentTask();
        self.current_task = null;
        self.frame_count = 0;
    }

    fn saveCurrentTask(self: *Self) void {
        const task = self.current_task orelse return;
        task.stack = self.stack;
        task.frames = self.frames;
        task.frame_count = self.frame_count;
    }

    fn reportUnhandledTaskErrors(self: *Self) !void {
        var failure: ?anyerror = null;
        for (self.scheduler.tasks.items) |task| {
            if (!task.hasUnhandledFailure()) continue;
            failure = RuntimeErrorReporter.unhandledTaskError(self.program.error_names[@intCast(task.result.int64)]);
        }
        if (failure) |found| return found;
    }

    fn reportWhere(self: *Self, failure: anyerror) void {
        if (failure == error.OutOfMemory) {
            RuntimeErrorReporter.outOfMemory();
        }
        if (self.frame_count == 0) return;
        const name = self.frames[self.frame_count - 1].function.name;
        const is_top_level = name.len > 0 and name[0] == '$';
        RuntimeErrorReporter.where(if (is_top_level) null else name, self.failed_builtin);
    }

    noinline fn execute(self: *Self) !void {
        var stack = self.stack;
        var current_frame = &self.frames[self.frame_count - 1];
        var cur_instruction: usize = current_frame.cur_inst;
        var instructions = current_frame.function.kind.dusk.instructions;
        var stack_base: usize = current_frame.stack_offset;

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
                        array = try self.growArray(&stack[stack_base + inst.a]);
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
                    const is_out_of_bounds = index < 0 or index >= bytes.len;
                    if (is_out_of_bounds) {
                        return RuntimeErrorReporter.indexOutOfBounds(index, bytes.len);
                    }
                    stack[stack_base + inst.a] = .{ .int64 = bytes.getDataPtr()[@intCast(index)] };
                },

                .BYTES_SET => {
                    const bytes = v.HeapValue.getParentPtr(v.Bytes, stack[stack_base + inst.a].heap_obj);
                    const index = stack[stack_base + inst.b].int64;
                    const is_out_of_bounds = index < 0 or index >= bytes.len;
                    if (is_out_of_bounds) {
                        return RuntimeErrorReporter.indexOutOfBounds(index, bytes.len);
                    }
                    bytes.getDataPtr()[@intCast(index)] = @intCast(stack[stack_base + inst.c].int64);
                },

                .DEEP_COPY => {
                    current_frame.cur_inst = cur_instruction;
                    stack[stack_base + inst.a] = try self.deepCopy(stack_base + inst.b);
                },

                .TASK_SPAWN => {
                    current_frame.cur_inst = cur_instruction;
                    try self.spawnTask(stack_base + inst.a, stack_base + inst.b, @enumFromInt(inst.c));
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
                    const result = @addWithOverflow(stack[stack_base + inst.b].int64, stack[stack_base + inst.c].int64);
                    if (result[1] != 0) {
                        return RuntimeErrorReporter.integerOverflow("+", stack[stack_base + inst.b].int64, stack[stack_base + inst.c].int64);
                    }
                    stack[stack_base + inst.a] = .{ .int64 = result[0] };
                },

                .I_SUB => {
                    const result = @subWithOverflow(stack[stack_base + inst.b].int64, stack[stack_base + inst.c].int64);
                    if (result[1] != 0) {
                        return RuntimeErrorReporter.integerOverflow("-", stack[stack_base + inst.b].int64, stack[stack_base + inst.c].int64);
                    }
                    stack[stack_base + inst.a] = .{ .int64 = result[0] };
                },

                .I_MULT => {
                    const result = @mulWithOverflow(stack[stack_base + inst.b].int64, stack[stack_base + inst.c].int64);
                    if (result[1] != 0) {
                        return RuntimeErrorReporter.integerOverflow("*", stack[stack_base + inst.b].int64, stack[stack_base + inst.c].int64);
                    }
                    stack[stack_base + inst.a] = .{ .int64 = result[0] };
                },

                .TRUNC_DIV => {
                    const dividend = stack[stack_base + inst.b].int64;
                    const divisor = stack[stack_base + inst.c].int64;
                    if (divisor == 0) {
                        return RuntimeErrorReporter.divisionByZero("//");
                    }
                    const does_overflow = divisor == -1 and dividend == std.math.minInt(i64);
                    if (does_overflow) {
                        return RuntimeErrorReporter.divisionOverflow(dividend);
                    }
                    stack[stack_base + inst.a] = .{ .int64 = @divTrunc(dividend, divisor) };
                },

                .I_MOD => {
                    const divisor = stack[stack_base + inst.c].int64;
                    if (divisor == 0) {
                        return RuntimeErrorReporter.divisionByZero("%");
                    }
                    if (divisor == -1) {
                        stack[stack_base + inst.a] = .{ .int64 = 0 };
                        continue;
                    }
                    stack[stack_base + inst.a] = .{ .int64 = @mod(stack[stack_base + inst.b].int64, divisor) };
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
                    const operand = stack[stack_base + inst.b].int64;
                    if (operand == std.math.minInt(i64)) {
                        return RuntimeErrorReporter.negationOverflow(operand);
                    }
                    stack[stack_base + inst.a] = .{ .int64 = -operand };
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
                    const is_out_of_byte_range = number < 0 or number > 255;
                    if (is_out_of_byte_range) {
                        return RuntimeErrorReporter.byteOutOfRange(number);
                    }
                    stack[stack_base + inst.a] = .{ .int64 = number };
                },

                .I_TO_F => {
                    stack[stack_base + inst.a] = .{
                        .float64 = @floatFromInt(stack[stack_base + inst.a].int64),
                    };
                },

                .F_TO_I => {
                    const truncated = @trunc(stack[stack_base + inst.a].float64);
                    const is_in_range = truncated >= -0x1p63 and truncated < 0x1p63;
                    if (!is_in_range) {
                        return RuntimeErrorReporter.floatOutOfIntRange(stack[stack_base + inst.a].float64);
                    }
                    stack[stack_base + inst.a] = .{ .int64 = @intFromFloat(truncated) };
                },

                .CALL => {
                    const func = &self.program.functions[inst.bEx()];
                    switch (func.kind) {
                        .dusk => {
                            current_frame.cur_inst = cur_instruction;
                            const new_offset = stack_base + inst.a + 1;
                            const is_stack_full = self.frame_count == self.frames.len or new_offset + MAX_FRAME_REGISTERS > stack.len;
                            if (is_stack_full) {
                                stack = try self.growForCall(new_offset);
                            }
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
                            const result = n.func(self, args) catch |failure| {
                                self.failed_builtin = func.name;
                                return failure;
                            };
                            if (result.is_parked) return;
                            stack[stack_base + inst.a] = result.value;
                            if (result.has_failed) {
                                cur_instruction += 1;
                            }
                        },
                        .intrinsic => unreachable,
                    }
                },

                .LOAD_FUNC => {
                    stack[stack_base + inst.a] = .{ .heap_obj = @constCast(&self.program.static_closures[inst.bEx()].obj) };
                },

                .MAKE_CLOSURE => {
                    current_frame.cur_inst = cur_instruction;
                    try self.makeClosure(stack_base + inst.a, inst.bEx());
                },

                .CAPTURE_GET => {
                    const closure = v.HeapValue.getParentPtr(v.Closure, stack[stack_base - 1].heap_obj);
                    stack[stack_base + inst.a] = closure.captures()[inst.b];
                },

                .CELL_NEW => {
                    current_frame.cur_inst = cur_instruction;
                    try self.newCell(stack_base + inst.a, stack_base + inst.c, @enumFromInt(inst.b));
                },

                .CELL_GET => {
                    stack[stack_base + inst.a] = v.HeapValue.getParentPtr(v.Cell, stack[stack_base + inst.b].heap_obj).value;
                },

                .CELL_SET => {
                    v.HeapValue.getParentPtr(v.Cell, stack[stack_base + inst.a].heap_obj).value = stack[stack_base + inst.b];
                },

                .CALL_VALUE => {
                    const closure = v.HeapValue.getParentPtr(v.Closure, stack[stack_base + inst.a].heap_obj);
                    const func = closure.function;
                    current_frame.cur_inst = cur_instruction;
                    const new_offset = stack_base + inst.a + 1;
                    const is_stack_full = self.frame_count == self.frames.len or new_offset + MAX_FRAME_REGISTERS > stack.len;
                    if (is_stack_full) {
                        stack = try self.growForCall(new_offset);
                    }
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

                .JUMP_TABLE => {
                    const table = current_frame.function.kind.dusk.jump_tables[inst.bEx()];
                    const value = stack[stack_base + inst.a].int64;
                    cur_instruction = table.default;
                    if (value < table.min) continue;
                    const offset = @as(u64, @bitCast(value)) -% @as(u64, @bitCast(table.min));
                    if (offset < table.target_offsets.len) {
                        cur_instruction = table.target_offsets[@intCast(offset)];
                    }
                },

                .RETURN => {
                    self.frame_count -= 1;
                    stack[stack_base - 1] = stack[stack_base + inst.a];
                    if (self.frame_count == 0) break;
                    current_frame = &self.frames[self.frame_count - 1];
                    cur_instruction = current_frame.cur_inst;
                    instructions = current_frame.function.kind.dusk.instructions;
                    stack_base = current_frame.stack_offset;
                },

                .RETURN_ERR => {
                    self.frame_count -= 1;
                    stack[stack_base - 1] = stack[stack_base + inst.a];
                    if (self.frame_count == 0) {
                        self.current_task.?.has_failed = true;
                        break;
                    }
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

    noinline fn makeClosure(self: *Self, target_index: usize, fn_index: u16) !void {
        const function = &self.program.functions[fn_index];
        const capture_count = function.kind.dusk.capture_kinds.len;
        const size = v.Closure.calc_size(capture_count);
        try self.maybeCollectGarbage(size);
        const closure = try v.Closure.init(self.getCurrentHeapAllocator(), function, capture_count);
        self.increaseHeapAllocationCount(size);
        @memcpy(closure.captures(), self.stack[target_index + 1 ..][0..capture_count]);
        self.stack[target_index] = .{ .heap_obj = &closure.obj };
    }

    noinline fn spawnTask(self: *Self, target_index: usize, closure_index: usize, result_kind: v.ValueType) !void {
        const size = v.Task.calc_size();
        try self.maybeCollectGarbage(size);
        const closure = v.HeapValue.getParentPtr(v.Closure, self.stack[closure_index].heap_obj);
        const state = try self.scheduler.spawn(closure, result_kind);
        const handle = try v.Task.init(self.getCurrentHeapAllocator(), state);
        self.increaseHeapAllocationCount(size);
        self.stack[target_index] = .{ .heap_obj = &handle.obj };
    }

    noinline fn newCell(self: *Self, target_index: usize, source_index: usize, kind: v.ValueType) !void {
        const size = v.Cell.calc_size();
        try self.maybeCollectGarbage(size);
        const cell = try v.Cell.init(self.getCurrentHeapAllocator(), self.stack[source_index], kind);
        self.increaseHeapAllocationCount(size);
        self.stack[target_index] = .{ .heap_obj = &cell.obj };
    }

    noinline fn newBuffer(self: *Self, len: i64) !v.Value {
        if (len < 0) {
            return RuntimeErrorReporter.negativeBufferSize(len);
        }
        const size = v.Bytes.calc_size(@intCast(len));
        try self.maybeCollectGarbage(size);
        const bytes = try v.Bytes.initZeroed(self.getCurrentHeapAllocator(), @intCast(len), .buffer);
        self.increaseHeapAllocationCount(size);
        return .{ .heap_obj = &bytes.obj };
    }

    noinline fn deepCopy(self: *Self, source_index: usize) !v.Value {
        if (@as(u64, @bitCast(self.stack[source_index])) == 0) {
            return self.stack[source_index];
        }
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
                        .buffer, .@"struct", .array, .@"union", .nullable, .function, .task => {
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
                        if (desc.heap_bitmap & (@as(u256, 1) << @intCast(i)) != 0) {
                            try self.deepCopySlot(&fields[i], &copies, &pending);
                        }
                    }
                },
                .@"union" => {
                    const u = v.HeapValue.getParentPtr(v.Union, copy);
                    if (u.active_tag == v.Union.NO_TAG) continue;
                    const desc = self.program.union_descriptors[u.descriptor_id];
                    if (desc.heap_bitmap & (@as(u256, 1) << @intCast(u.active_tag)) != 0) {
                        try self.deepCopySlot(&u.getDataPtr()[0], &copies, &pending);
                    }
                },
                else => {},
            }
        }
        return result;
    }

    fn deepCopySlot(self: *Self, slot: *v.Value, copies: *std.AutoHashMapUnmanaged(*v.HeapValue, *v.HeapValue), pending: *std.ArrayList(*v.HeapValue)) !void {
        if (@as(u64, @bitCast(slot.*)) == 0) return;
        const current = slot.heap_obj.followRelocated();
        const is_immutable = switch (current.kind) {
            .string, .null_box, .closure, .cell, .task => true,
            else => false,
        };
        if (is_immutable) {
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
        if (copy.kind != .buffer) {
            try pending.append(self.allocator, copy);
        }
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

    noinline fn growForCall(self: *Self, new_offset: usize) ![]v.Value {
        if (self.frame_count == self.frames.len) {
            if (self.frames.len == MAX_CALL_DEPTH) {
                return RuntimeErrorReporter.callDepthExceeded(MAX_CALL_DEPTH);
            }
            const new_len = @min(self.frames.len * 2, MAX_CALL_DEPTH);
            self.frames = try self.task_allocator.realloc(self.frames, new_len);
        }
        const needed_slots = new_offset + MAX_FRAME_REGISTERS;
        if (needed_slots > self.stack.len) {
            if (needed_slots > STACK_SLOTS) {
                return RuntimeErrorReporter.callDepthExceeded(MAX_CALL_DEPTH);
            }
            const old_len = self.stack.len;
            const new_len = @min(@max(old_len * 2, needed_slots), STACK_SLOTS);
            self.stack = try self.task_allocator.realloc(self.stack, new_len);
            @memset(self.stack[old_len..], v.NULL_VALUE);
        }
        return self.stack;
    }

    noinline fn loadUnionVariant(self: *Self, stack: []v.Value, stack_base: usize, target_reg: u8, union_reg: u8, tag: u16) !void {
        var union_vl = v.HeapValue.getParentPtr(v.Union, stack[stack_base + union_reg].heap_obj);
        const desc = self.program.union_descriptors[union_vl.descriptor_id];
        const needs_box = desc.box_bitmap & (@as(u256, 1) << @intCast(tag)) != 0;

        const can_load_unboxed = !needs_box or union_vl.active_tag != tag;
        if (can_load_unboxed) {
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
        if (current != ptr) {
            stack[stack_base + reg_idx].heap_obj = current;
        }
        return v.HeapValue.getParentPtr(v.Array, current);
    }

    pub fn growArray(self: *Self, slot: *v.Value) !*v.Array {
        try self.maybeCollectGarbage(v.Array.calc_size(arrayInSlot(slot).calcNewCapacity()));
        const array = try arrayInSlot(slot).resize(self.getCurrentHeapAllocator());
        slot.heap_obj = &array.obj;
        self.increaseHeapAllocationCount(v.Array.calc_size(array.capacity));
        return array;
    }

    fn arrayInSlot(slot: *v.Value) *v.Array {
        slot.heap_obj = slot.heap_obj.followRelocated();
        return v.HeapValue.getParentPtr(v.Array, slot.heap_obj);
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

        self.saveCurrentTask();
        self.scheduler.unmarkAll();
        for (self.scheduler.tasks.items) |task| {
            try self.copyTaskRoots(new_alloc, task);
        }

        while (self.heap.gc_queue.items.len > 0) {
            const obj = self.heap.gc_queue.pop().?;
            switch (obj.kind) {
                .array => {
                    const array = v.HeapValue.getParentPtr(v.Array, obj);
                    switch (array.kind) {
                        .string, .buffer, .@"struct", .array, .@"union", .nullable, .function, .task => {
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
                .closure => {
                    const closure = v.HeapValue.getParentPtr(v.Closure, obj);
                    const kinds = closure.function.kind.dusk.capture_kinds;
                    for (closure.captures(), kinds) |*capture, kind| {
                        if (!kind.isHeapType()) continue;
                        try self.copyHeapValue(new_alloc, capture);
                    }
                },
                .cell => {
                    const cell = v.HeapValue.getParentPtr(v.Cell, obj);
                    if (cell.kind.isHeapType()) {
                        try self.copyHeapValue(new_alloc, &cell.value);
                    }
                },
                .task => {
                    const state = v.HeapValue.getParentPtr(v.Task, obj).state;
                    state.is_marked = true;
                    if (state.hasResultHeapValue()) {
                        try self.copyHeapValue(new_alloc, &state.result);
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

        self.scheduler.sweep();
        self.heap.active = inactive;
        self.heap.threshold = @max(self.heap.allocated * 2, MIN_GC_THRESHOLD);

        const old_idx: u1 = self.heap.active ^ 1;
        if (self.heap.arenas[old_idx].queryCapacity() > self.heap.threshold) {
            _ = self.heap.arenas[old_idx].reset(.{ .retain_with_limit = self.heap.threshold });
            self.heap.gc_queue.clearAndFree(self.allocator);
            try self.heap.gc_queue.ensureTotalCapacity(self.allocator, self.heap.object_count);
        }
    }

    fn copyTaskRoots(self: *Self, new_alloc: std.mem.Allocator, task: *TaskState) !void {
        if (task.pending) |*delivery| {
            if (delivery.isHeapValue()) {
                try self.copyHeapValue(new_alloc, &delivery.value);
            }
        }
        if (task.status == .done) return;
        try self.copyHeapValue(new_alloc, &task.stack[0]);
        const active_frames = task.frames[0..task.frame_count];
        for (active_frames, 0..) |frame, frame_index| {
            if (frame.cur_inst == 0) continue;
            const chunk = frame.function.kind.dusk;
            const cur_pc: u16 = @intCast(frame.cur_inst - 1);
            const heap_map = util.binarySearch(bc.HeapMap, "pc", chunk.heap_maps, cur_pc);
            const bitmap: u256 = if (heap_map) |hm| hm.bitmap else 0;
            const base = frame.stack_offset;
            const is_top_frame = frame_index + 1 == active_frames.len;
            const owned_registers = if (is_top_frame) chunk.num_registers else active_frames[frame_index + 1].stack_offset - base;
            for (0..owned_registers) |i| {
                const is_heap_value = bitmap & (@as(u256, 1) << @intCast(i)) != 0;
                if (is_heap_value) {
                    try self.copyHeapValue(new_alloc, &task.stack[base + i]);
                }
            }
        }
    }

    fn copyHeapValue(self: *Self, new_alloc: std.mem.Allocator, value: *v.Value) !void {
        const raw: u64 = @bitCast(value.*);
        if (raw == 0) return;
        if (self.program.string_pool.contains(value.heap_obj)) return;
        if (v.Closure.isStatic(value.heap_obj)) return;
        const current = value.heap_obj.followRelocated();

        if (current.gc_forward) |fwd| {
            value.heap_obj = fwd;
        } else {
            const new_ptr = try cloneHeapObj(new_alloc, current);
            current.gc_forward = new_ptr;
            value.heap_obj = new_ptr;
            const has_references = new_ptr.kind != .string and new_ptr.kind != .buffer;
            if (has_references) {
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
            .closure => &(try v.HeapValue.getParentPtr(v.Closure, obj_ptr).clone(allocator)).obj,
            .cell => &(try v.HeapValue.getParentPtr(v.Cell, obj_ptr).clone(allocator)).obj,
            .task => &(try v.HeapValue.getParentPtr(v.Task, obj_ptr).clone(allocator)).obj,
            .string, .buffer => &(try v.HeapValue.getParentPtr(v.Bytes, obj_ptr).clone(allocator)).obj,
        };
    }

    fn heapObjectSize(ptr: *v.HeapValue) usize {
        return switch (ptr.kind) {
            .array => v.Array.calc_size(v.HeapValue.getParentPtr(v.Array, ptr).capacity),
            .@"struct" => v.Struct.calc_size(v.HeapValue.getParentPtr(v.Struct, ptr).field_count),
            .@"union" => v.Union.calc_size(),
            .null_box => v.NullBox.calc_size(),
            .closure => v.Closure.calc_size(v.HeapValue.getParentPtr(v.Closure, ptr).capture_count),
            .cell => v.Cell.calc_size(),
            .task => v.Task.calc_size(),
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

pub const CallFrame = struct {
    function: *const bc.Function,
    cur_inst: usize,
    stack_offset: usize,
};
