const std = @import("std");
const bc = @import("bytecode.zig");
const v = @import("value.zig");
const RuntimeErrorReporter = @import("runtime_error.zig").RuntimeErrorReporter;
const format = @import("format.zig");
const TaskState = @import("tasks/scheduler.zig").TaskState;
const CallFrame = @import("tasks/scheduler.zig").CallFrame;
const WakeResult = @import("tasks/scheduler.zig").WakeResult;
const Scheduler = @import("tasks/scheduler.zig").Scheduler;
const EventLoop = @import("tasks/event_loop.zig").EventLoop;
const Stdio = @import("io/stdio.zig").Stdio;
const gc = @import("gc.zig");
const Heap = gc.Heap;

pub const VM = struct {
    const Self = @This();
    const MAX_CALL_DEPTH = 100_000;
    const STACK_SLOTS = 4_000_000;
    const MAX_FRAME_REGISTERS = 256;

    program: *const bc.Program,
    running_task: ?*TaskState = null,
    allocator: std.mem.Allocator,
    scratch: std.Io.Writer.Allocating,
    stdio: Stdio,
    io: std.Io,
    stdin_fd: c_int = 0,
    stack: []v.Value,
    heap: Heap,
    scheduler: Scheduler,
    event_loop: EventLoop,
    frames: []CallFrame,
    frame_count: usize,
    failed_builtin: ?[]const u8 = null,

    pub fn init(program: *const bc.Program, stdio: Stdio, io: std.Io) VM {
        return .{
            .allocator = std.heap.c_allocator,
            .scheduler = .{ .allocator = std.heap.c_allocator },
            .event_loop = .{ .allocator = std.heap.c_allocator },
            .frames = &.{},
            .frame_count = 0,
            .program = program,
            .stack = &.{},
            .heap = Heap.init(std.heap.page_allocator),
            .scratch = .init(std.heap.page_allocator),
            .stdio = stdio,
            .io = io,
        };
    }

    pub fn deinit(self: *Self) void {
        self.scratch.deinit();
        self.event_loop.deinit();
        self.scheduler.deinit();
        self.heap.deinit();
    }

    pub noinline fn run(self: *Self) !void {
        const main_closure: *v.Closure = @constCast(&self.program.static_closures[self.program.main_func_index]);
        _ = try self.scheduler.spawn(main_closure, .null);
        RuntimeErrorReporter.stdout_to_flush_first = self.stdio.stdout.writer();
        defer RuntimeErrorReporter.stdout_to_flush_first = null;
        self.runScheduler() catch |failure| {
            self.saveCurrentTask();
            self.stdio.tryFlush(.stdout) catch {};
            if (failure == error.BrokenPipe) return failure;
            self.reportErrorLocation(failure);
            return failure;
        };
        try self.stdio.flushStdout();
    }

    fn runScheduler(self: *Self) !void {
        while (true) {
            const ready_task_count = self.scheduler.countReadyTasks();

            if (ready_task_count > 0) {
                for (0..ready_task_count) |_| {
                    try self.runTask(self.scheduler.popReady().?);
                }

                if (self.event_loop.hasPendingOperations()) {
                    try self.event_loop.poll();
                }

                continue;
            }

            if (self.event_loop.hasPendingOperations()) {
                try self.stdio.flushStdout();
                try self.event_loop.waitForEvent();
                continue;
            }

            if (self.scheduler.hasUnfinishedTasks()) {
                return RuntimeErrorReporter.allTasksWaiting();
            }

            break;
        }
        try self.reportUnhandledTaskErrors();
    }

    fn runTask(self: *Self, task: *TaskState) !void {
        try self.enterTask(task);
        try self.execute();
        self.leaveTask();
        if (task.frame_count == 0) {
            try self.scheduler.finish(task);
        }
    }

    fn enterTask(self: *Self, task: *TaskState) !void {
        self.running_task = task;
        self.stack = task.stack;
        self.frames = task.frames;
        self.frame_count = task.frame_count;
        task.status = .running;
        if (task.wake_result) |wake_result| {
            try self.finishParkedCall(task, wake_result);
        }
    }

    fn finishParkedCall(self: *Self, task: *TaskState, wake_result: WakeResult) !void {
        task.wake_result = null;
        var result = try self.buildWakeValue(wake_result);
        const frame = &task.frames[task.frame_count - 1];
        const call = frame.function.kind.dusk.instructions[frame.cur_inst - 1];
        if (wake_result.has_failed) {
            if (wake_result.error_is_relative) {
                result.int64 += self.program.functions[call.bEx()].kind.native.error_base;
            }
            frame.cur_inst += 1;
        }
        task.stack[frame.stack_offset + call.a] = result;
    }

    fn buildWakeValue(self: *Self, wake_result: WakeResult) !v.Value {
        defer wake_result.payload.free(self.scheduler.allocator);
        return switch (wake_result.payload) {
            .none => wake_result.value,
            .ints => |ints| try self.allocIntArray(ints),
            .bytes => |bytes| try self.allocBytesValue(bytes, wake_result.kind),
            .names => |names| try self.allocStringArray(names),
        };
    }

    pub fn allocBytesValue(self: *Self, bytes: []const u8, kind: v.ValueType) !v.Value {
        const size = v.Bytes.calc_size(bytes.len);
        try self.maybeCollectGarbage(size);
        const allocator = self.getCurrentHeapAllocator();
        const result = if (kind == .buffer) try v.Bytes.init(allocator, bytes, .buffer) else try v.Bytes.init(allocator, bytes, .string);
        self.increaseHeapAllocationCount(size);
        return .{ .heap_obj = &result.obj };
    }

    pub fn allocStringArray(self: *Self, strings: []const []const u8) !v.Value {
        const capacity = @max(strings.len, 1);
        var total = v.Array.calc_size(capacity);
        for (strings) |text| {
            total += std.mem.Alignment.of(v.Bytes).forward(v.Bytes.calc_size(text.len));
        }
        try self.maybeCollectGarbage(total);
        const allocator = self.getCurrentHeapAllocator();
        const array = try v.Array.init(allocator, .string, capacity);
        self.increaseHeapAllocationCount(v.Array.calc_size(capacity));
        for (strings) |text| {
            const bytes = try v.Bytes.init(allocator, text, .string);
            self.increaseHeapAllocationCount(v.Bytes.calc_size(text.len));
            try array.append(.{ .heap_obj = &bytes.obj });
        }
        return .{ .heap_obj = &array.obj };
    }

    pub fn allocIntArray(self: *Self, ints: []const i64) !v.Value {
        const capacity = @max(ints.len, 1);
        const size = v.Array.calc_size(capacity);
        try self.maybeCollectGarbage(size);
        const array = try v.Array.init(self.getCurrentHeapAllocator(), .int64, capacity);
        self.increaseHeapAllocationCount(size);
        for (ints) |item| {
            try array.append(.{ .int64 = item });
        }
        return .{ .heap_obj = &array.obj };
    }

    noinline fn execute(self: *Self) align(64) !void {
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

                .MAP_INIT => {
                    current_frame.cur_inst = cur_instruction;
                    stack[stack_base + inst.a] = try self.newMap(@enumFromInt(inst.b), @enumFromInt(inst.c));
                },

                .MAP_GET => {
                    current_frame.cur_inst = cur_instruction;
                    stack[stack_base + inst.a] = try self.readMapEntry(&stack[stack_base + inst.b], &stack[stack_base + inst.c]);
                },

                .MAP_SET => {
                    current_frame.cur_inst = cur_instruction;
                    try self.setMapEntry(&stack[stack_base + inst.a], &stack[stack_base + inst.b], &stack[stack_base + inst.c]);
                },

                .MAP_GET_UNBOXED => {
                    const value = mapInSlot(&stack[stack_base + inst.b]).get(stack[stack_base + inst.c]);
                    stack[stack_base + inst.a] = if (value != null) v.TRUE_VALUE else v.FALSE_VALUE;
                    stack[stack_base + inst.a + 1] = value orelse v.NULL_VALUE;
                },

                .MAP_NEXT => {
                    var cursor = MapCursor.unpack(stack[stack_base + inst.c]);
                    const has_next = cursor.advance(mapInSlot(&stack[stack_base + inst.b]));
                    stack[stack_base + inst.c] = cursor.pack();
                    stack[stack_base + inst.a] = if (has_next) v.TRUE_VALUE else v.FALSE_VALUE;
                },

                .MAP_KEY_AT => {
                    const cursor = MapCursor.unpack(stack[stack_base + inst.c]);
                    stack[stack_base + inst.a] = cursor.entryIn(mapInSlot(&stack[stack_base + inst.b])).key;
                },

                .MAP_VALUE_AT => {
                    const cursor = MapCursor.unpack(stack[stack_base + inst.c]);
                    stack[stack_base + inst.a] = cursor.entryIn(mapInSlot(&stack[stack_base + inst.b])).value;
                },

                .BYTES_NEW => {
                    current_frame.cur_inst = cur_instruction;
                    stack[stack_base + inst.a] = try self.newBuffer(stack[stack_base + inst.b].int64);
                },

                .BYTES_LEN => {
                    const bytes = v.HeapValue.getParentPtr(v.Bytes, stack[stack_base + inst.b].heap_obj);
                    stack[stack_base + inst.a] = .{ .int64 = @intCast(bytes.len) };
                },

                .STR_EQ => {
                    stack[stack_base + inst.a] = .{ .bool = areStringsEqual(stack[stack_base + inst.b], stack[stack_base + inst.c]) };
                },

                .ARR_EQ => {
                    stack[stack_base + inst.a] = .{ .bool = areArraysEqual(stack[stack_base + inst.b], stack[stack_base + inst.c]) };
                },

                .ARR_NEQ => {
                    stack[stack_base + inst.a] = .{ .bool = !areArraysEqual(stack[stack_base + inst.b], stack[stack_base + inst.c]) };
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
                    current_frame.cur_inst = cur_instruction;
                    stack[stack_base + inst.a] = try self.allocNullBox(stack[stack_base + inst.c], @enumFromInt(inst.b));
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
                            current_frame = try self.pushFrame(func, new_offset, &stack);
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
                                stack[stack_base + inst.a].int64 += n.error_base;
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

                .CALL_CLOSURE => {
                    const closure = v.HeapValue.getParentPtr(v.Closure, stack[stack_base + inst.a].heap_obj);
                    const func = closure.function;
                    current_frame.cur_inst = cur_instruction;
                    const new_offset = stack_base + inst.a + 1;
                    current_frame = try self.pushFrame(func, new_offset, &stack);
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
                        self.running_task.?.has_failed = true;
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
            }
        }
    }

    noinline fn toString(self: *Self, value: v.Value, kind: v.ValueType) !v.Value {
        self.scratch.clearRetainingCapacity();
        format.printValue(&self.scratch.writer, value, kind) catch return error.OutOfMemory;
        const bytes = self.scratch.written();
        const result = try self.allocString(bytes.len);
        @memcpy(result.mutableSlice(), bytes);
        return .{ .heap_obj = &result.obj };
    }

    pub fn allocString(self: *Self, len: usize) !*v.Bytes {
        const size = v.Bytes.calc_size(len);
        try self.maybeCollectGarbage(size);
        const result = try v.Bytes.initUndefined(self.getCurrentHeapAllocator(), len, .string);
        self.increaseHeapAllocationCount(size);
        return result;
    }

    pub fn allocNullBox(self: *Self, value: v.Value, kind: v.ValueType) !v.Value {
        const size = v.NullBox.calc_size();
        try self.maybeCollectGarbage(size);
        const box = try v.NullBox.init(self.getCurrentHeapAllocator(), value, kind);
        self.increaseHeapAllocationCount(size);
        return .{ .heap_obj = &box.obj };
    }

    pub fn maybeCollectGarbage(self: *Self, bytes: usize) !void {
        if (!self.heap.needsCollection(bytes)) return;
        self.saveCurrentTask();
        try self.heap.collect(&self.scheduler, self.program);
        self.closeDeadResources();
    }

    fn closeDeadResources(self: *Self) void {
        for (self.heap.dead_resource_ids.items) |id| {
            self.event_loop.closeResource(id);
        }
        self.heap.dead_resource_ids.clearRetainingCapacity();
    }

    fn saveCurrentTask(self: *Self) void {
        const task = self.running_task orelse return;
        task.stack = self.stack;
        task.frames = self.frames;
        task.frame_count = self.frame_count;
    }

    pub fn getCurrentHeapAllocator(self: *Self) std.mem.Allocator {
        return self.heap.allocator();
    }

    pub fn increaseHeapAllocationCount(self: *Self, bytes: usize) void {
        self.heap.countAllocation(bytes);
    }

    noinline fn buildString(self: *Self, parts: []const v.Value, kinds: []const v.ValueType) !v.Value {
        self.scratch.clearRetainingCapacity();
        for (parts, kinds) |part, kind| {
            if (kind == .string) {
                self.scratch.writer.writeAll(v.HeapValue.getParentPtr(v.Bytes, part.heap_obj).slice()) catch return error.OutOfMemory;
                continue;
            }
            format.printValue(&self.scratch.writer, part, kind) catch return error.OutOfMemory;
        }
        const bytes = self.scratch.written();
        const result = try self.allocString(bytes.len);
        @memcpy(result.mutableSlice(), bytes);
        return .{ .heap_obj = &result.obj };
    }

    fn getArrayFromHeapValuePtr(stack: []v.Value, stack_base: usize, reg_idx: u8) *v.Array {
        const ptr = stack[stack_base + reg_idx].heap_obj;
        const current = v.HeapValue.getParentPtr(v.Array, ptr).newestCopy();
        if (&current.obj != ptr) {
            stack[stack_base + reg_idx].heap_obj = &current.obj;
        }
        return current;
    }

    pub fn growArray(self: *Self, slot: *v.Value) !*v.Array {
        try self.maybeCollectGarbage(v.Array.calc_size(arrayInSlot(slot).calcNewCapacity()));
        const array = try arrayInSlot(slot).resize(self.getCurrentHeapAllocator());
        slot.heap_obj = &array.obj;
        self.increaseHeapAllocationCount(v.Array.calc_size(array.capacity));
        return array;
    }

    fn arrayInSlot(slot: *v.Value) *v.Array {
        const array = v.HeapValue.getParentPtr(v.Array, slot.heap_obj).newestCopy();
        slot.heap_obj = &array.obj;
        return array;
    }

    fn newMap(self: *Self, key_type: v.ValueType, value_type: v.ValueType) !v.Value {
        const size = v.Map.calc_size();
        try self.maybeCollectGarbage(size);
        const map = try v.Map.init(self.getCurrentHeapAllocator(), key_type, value_type);
        self.increaseHeapAllocationCount(size);
        return .{ .heap_obj = &map.obj };
    }

    fn readMapEntry(self: *Self, map_slot: *const v.Value, key_slot: *const v.Value) !v.Value {
        const map = mapInSlot(map_slot);
        const value = map.get(key_slot.*) orelse return v.NULL_VALUE;
        if (map.value_type.isHeapType() or map.value_type == .null) return value;
        return self.allocNullBox(value, map.value_type);
    }

    fn setMapEntry(self: *Self, map_slot: *const v.Value, key_slot: *const v.Value, value_slot: *const v.Value) !void {
        const hash = mapInSlot(map_slot).hashKey(key_slot.*);
        if (mapInSlot(map_slot).find(key_slot.*, hash)) |entry| {
            entry.value = value_slot.*;
            return;
        }
        try self.ensureRoomForMapEntry(map_slot);
        mapInSlot(map_slot).append(key_slot.*, value_slot.*, hash);
    }

    fn ensureRoomForMapEntry(self: *Self, map_slot: *const v.Value) !void {
        const old_storage = mapInSlot(map_slot).getStorage();
        if (old_storage) |storage| {
            if (!storage.isFull()) return;
        }
        const capacity = if (old_storage) |storage| storage.nextCapacity() else v.MapStorage.min_capacity;
        const size = v.MapStorage.calc_size(capacity);
        try self.maybeCollectGarbage(size);
        const map = mapInSlot(map_slot);
        const storage = try v.MapStorage.init(self.getCurrentHeapAllocator(), map.key_type, map.value_type, capacity);
        self.increaseHeapAllocationCount(size);
        if (map.getStorage()) |previous| {
            storage.copyLiveEntriesFrom(previous);
        }
        map.storage = .{ .heap_obj = &storage.obj };
    }

    fn mapInSlot(slot: *const v.Value) *v.Map {
        return v.HeapValue.getParentPtr(v.Map, slot.heap_obj);
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

    fn areStringsEqual(left: v.Value, right: v.Value) bool {
        const left_bits: u64 = @bitCast(left);
        const right_bits: u64 = @bitCast(right);
        if (left_bits == right_bits) return true;
        const is_either_null = left_bits == 0 or right_bits == 0;
        if (is_either_null) return false;
        const left_bytes = v.HeapValue.getParentPtr(v.Bytes, left.heap_obj).slice();
        const right_bytes = v.HeapValue.getParentPtr(v.Bytes, right.heap_obj).slice();
        return std.mem.eql(u8, left_bytes, right_bytes);
    }

    fn areArraysEqual(left: v.Value, right: v.Value) bool {
        const left_bits: u64 = @bitCast(left);
        const right_bits: u64 = @bitCast(right);
        if (left_bits == right_bits) {
            return true;
        }
        const is_either_null = left_bits == 0 or right_bits == 0;
        if (is_either_null) {
            return false;
        }
        const left_array = v.HeapValue.getParentPtr(v.Array, left.heap_obj).newestCopy();
        const right_array = v.HeapValue.getParentPtr(v.Array, right.heap_obj).newestCopy();
        return left_array == right_array;
    }

    noinline fn deepCopy(self: *Self, source_index: usize) !v.Value {
        if (@as(u64, @bitCast(self.stack[source_index])) == 0) {
            return self.stack[source_index];
        }
        try self.maybeCollectGarbage(gc.heapObjectSize(self.stack[source_index].heap_obj.followRelocated()));

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
                    if (!array.kind.isHeapType()) continue;
                    for (array.getDataPtr()[0..array.len]) |*item| try self.deepCopySlot(item, &copies, &pending);
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
                .map => try self.deepCopySlot(&v.HeapValue.getParentPtr(v.Map, copy).storage, &copies, &pending),
                .map_storage => {
                    const storage = v.HeapValue.getParentPtr(v.MapStorage, copy);
                    if (!storage.value_type.isHeapType()) continue;
                    for (storage.usedEntries()) |*entry| try self.deepCopySlot(&entry.value, &copies, &pending);
                },
                .string, .buffer, .null_box, .closure, .cell, .task, .resource => {},
            }
        }
        return result;
    }

    fn deepCopySlot(self: *Self, slot: *v.Value, copies: *std.AutoHashMapUnmanaged(*v.HeapValue, *v.HeapValue), pending: *std.ArrayList(*v.HeapValue)) !void {
        if (@as(u64, @bitCast(slot.*)) == 0) return;
        const current = slot.heap_obj.followRelocated();
        const is_immutable = switch (current.kind) {
            .string, .null_box, .closure, .cell, .task, .resource => true,
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
        const copy = try gc.cloneHeapObj(self.getCurrentHeapAllocator(), current);
        self.increaseHeapAllocationCount(gc.heapObjectSize(copy));
        if (copy.kind == .@"struct") {
            v.HeapValue.getParentPtr(v.Struct, copy).identity = 0;
        }
        try copies.put(self.allocator, current, copy);
        if (copy.kind != .buffer) {
            try pending.append(self.allocator, copy);
        }
        slot.heap_obj = copy;
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

    noinline fn loadUnionVariant(self: *Self, stack: []v.Value, stack_base: usize, target_reg: u8, union_reg: u8, tag: u16) !void {
        var union_vl = v.HeapValue.getParentPtr(v.Union, stack[stack_base + union_reg].heap_obj);
        const desc = self.program.union_descriptors[union_vl.descriptor_id];
        const needs_box = desc.box_bitmap & (@as(u256, 1) << @intCast(tag)) != 0;

        const can_load_unboxed = !needs_box or union_vl.active_tag != tag;
        if (can_load_unboxed) {
            stack[stack_base + target_reg] = union_vl.get(tag);
            return;
        }

        stack[stack_base + target_reg] = try self.allocNullBox(union_vl.get(tag), desc.variant_kinds[tag]);
    }

    inline fn pushFrame(self: *Self, func: *const bc.Function, new_offset: usize, stack: *[]v.Value) !*CallFrame {
        const is_stack_full = self.frame_count == self.frames.len or new_offset + MAX_FRAME_REGISTERS > stack.len;
        if (is_stack_full) {
            stack.* = try self.growCallStack(new_offset);
        }
        self.frames[self.frame_count] = .{
            .function = func,
            .cur_inst = 0,
            .stack_offset = new_offset,
        };
        self.frame_count += 1;
        return &self.frames[self.frame_count - 1];
    }

    noinline fn growCallStack(self: *Self, new_offset: usize) ![]v.Value {
        if (self.frame_count == self.frames.len) {
            if (self.frames.len == MAX_CALL_DEPTH) {
                return RuntimeErrorReporter.callDepthExceeded(MAX_CALL_DEPTH);
            }
            const new_len = @min(self.frames.len * 2, MAX_CALL_DEPTH);
            self.frames = try self.scheduler.allocator.realloc(self.frames, new_len);
        }
        const needed_slots = new_offset + MAX_FRAME_REGISTERS;
        if (needed_slots > self.stack.len) {
            if (needed_slots > STACK_SLOTS) {
                return RuntimeErrorReporter.callDepthExceeded(MAX_CALL_DEPTH);
            }
            const old_len = self.stack.len;
            const new_len = @min(@max(old_len * 2, needed_slots), STACK_SLOTS);
            self.stack = try self.scheduler.allocator.realloc(self.stack, new_len);
            @memset(self.stack[old_len..], v.NULL_VALUE);
        }
        return self.stack;
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

    noinline fn newCell(self: *Self, target_index: usize, source_index: usize, kind: v.ValueType) !void {
        const size = v.Cell.calc_size();
        try self.maybeCollectGarbage(size);
        const cell = try v.Cell.init(self.getCurrentHeapAllocator(), self.stack[source_index], kind);
        self.increaseHeapAllocationCount(size);
        self.stack[target_index] = .{ .heap_obj = &cell.obj };
    }

    fn leaveTask(self: *Self) void {
        self.saveCurrentTask();
        self.running_task = null;
        self.frame_count = 0;
    }

    fn reportUnhandledTaskErrors(self: *Self) !void {
        var failure: ?anyerror = null;
        for (self.scheduler.tasks.items) |task| {
            if (!task.hasUnhandledFailure()) continue;
            failure = RuntimeErrorReporter.unhandledTaskError(self.program.error_names[@intCast(task.result.int64)]);
        }
        if (failure) |found| return found;
    }

    fn reportErrorLocation(self: *Self, failure: anyerror) void {
        if (failure == error.OutOfMemory) {
            RuntimeErrorReporter.outOfMemory();
        }
        if (self.frame_count == 0) return;
        const name = self.frames[self.frame_count - 1].function.name;
        const is_top_level = name.len > 0 and name[0] == '$';
        RuntimeErrorReporter.where(if (is_top_level) null else name, self.failed_builtin);
    }
};

const MapCursor = struct {
    next_position: u32,
    seq: u32,

    fn unpack(value: v.Value) MapCursor {
        const raw: u64 = @bitCast(value.int64);
        return .{ .next_position = @truncate(raw), .seq = @truncate(raw >> 32) };
    }

    fn pack(self: MapCursor) v.Value {
        const raw = @as(u64, self.next_position) | (@as(u64, self.seq) << 32);
        return .{ .int64 = @bitCast(raw) };
    }

    fn advance(self: *MapCursor, map: *const v.Map) bool {
        const storage = map.getStorage() orelse return false;
        const used = storage.usedEntries();
        var position = self.resumePosition(used);
        while (position < used.len) : (position += 1) {
            if (used[position].isRemoved()) continue;
            self.* = .{ .next_position = position + 1, .seq = used[position].seq };
            return true;
        }
        return false;
    }

    fn entryIn(self: MapCursor, map: *const v.Map) *v.MapEntry {
        return &map.getStorage().?.entries()[self.next_position - 1];
    }

    fn resumePosition(self: MapCursor, entries: []const v.MapEntry) u32 {
        if (self.next_position == 0) return 0;
        const visited = self.next_position - 1;
        const is_visited_in_place = visited < entries.len and entries[visited].seq == self.seq;
        if (is_visited_in_place) return self.next_position;
        return firstEntryAfter(entries, self.seq);
    }

    fn firstEntryAfter(entries: []const v.MapEntry, seq: u32) u32 {
        var low: usize = 0;
        var high: usize = entries.len;
        while (low < high) {
            const middle = low + (high - low) / 2;
            if (entries[middle].seq <= seq) {
                low = middle + 1;
            } else {
                high = middle;
            }
        }
        return @intCast(low);
    }
};
