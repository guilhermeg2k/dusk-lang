const std = @import("std");
const bc = @import("bytecode.zig");
const v = @import("value.zig");
const Scheduler = @import("tasks/scheduler.zig").Scheduler;
const TaskState = @import("tasks/scheduler.zig").TaskState;

const MIN_GC_THRESHOLD = 1000000; //1MB

pub const Heap = struct {
    const Self = @This();

    arenas: [2]std.heap.ArenaAllocator,
    active: u1 = 0,
    gc_queue: std.ArrayList(*v.HeapValue) = .empty,
    queue_allocator: std.mem.Allocator,
    object_count: usize = 0,
    allocated: usize = 0,
    threshold: usize = MIN_GC_THRESHOLD,

    pub fn init(backing_allocator: std.mem.Allocator) Self {
        return .{
            .arenas = .{
                std.heap.ArenaAllocator.init(backing_allocator),
                std.heap.ArenaAllocator.init(backing_allocator),
            },
            .queue_allocator = backing_allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.gc_queue.deinit(self.queue_allocator);
        for (&self.arenas) |*arena| arena.deinit();
    }

    pub noinline fn collect(self: *Self, scheduler: *Scheduler, program: *const bc.Program) !void {
        const inactive: u1 = self.active ^ 1;
        _ = self.arenas[inactive].reset(.retain_capacity);

        self.allocated = 0;
        self.object_count = 0;
        self.gc_queue.clearRetainingCapacity();

        var collector = Collector{ .heap = self, .program = program, .to_space = self.arenas[inactive].allocator() };

        scheduler.clearReachable();
        for (scheduler.tasks.items) |task| {
            try collector.copyTaskRoots(task);
        }
        while (self.gc_queue.pop()) |obj| {
            try collector.copyChildren(obj);
        }

        scheduler.freeUnreachableTasks();
        self.active = inactive;
        self.threshold = @max(self.allocated * 2, MIN_GC_THRESHOLD);
        try self.shrinkOldArena();
    }

    fn shrinkOldArena(self: *Self) !void {
        const old_idx: u1 = self.active ^ 1;
        if (self.arenas[old_idx].queryCapacity() <= self.threshold) return;
        _ = self.arenas[old_idx].reset(.{ .retain_with_limit = self.threshold });
        self.gc_queue.clearAndFree(self.queue_allocator);
        try self.gc_queue.ensureTotalCapacity(self.queue_allocator, self.object_count);
    }

    pub fn needsCollection(self: *const Self, bytes: usize) bool {
        return self.allocated + bytes > self.threshold;
    }

    pub fn allocator(self: *Self) std.mem.Allocator {
        return self.arenas[self.active].allocator();
    }

    pub fn countAllocation(self: *Self, bytes: usize) void {
        self.allocated += bytes;
        self.object_count += 1;
    }
};

const Collector = struct {
    const Self = @This();

    heap: *Heap,
    program: *const bc.Program,
    to_space: std.mem.Allocator,

    fn copyTaskRoots(self: *Self, task: *TaskState) !void {
        if (task.wake_result) |*wake_result| {
            if (wake_result.isHeapValue()) {
                try self.copyHeapValue(&wake_result.value);
            }
        }
        if (task.status == .done) return;
        try self.copyHeapValue(&task.stack[0]);
        const active_frames = task.frames[0..task.frame_count];
        for (active_frames, 0..) |frame, frame_index| {
            if (frame.cur_inst == 0) continue;
            const chunk = frame.function.kind.dusk;
            const cur_pc: u16 = @intCast(frame.cur_inst - 1);
            const heap_map = binarySearch(bc.HeapMap, "pc", chunk.heap_maps, cur_pc);
            const bitmap: u256 = if (heap_map) |hm| hm.bitmap else 0;
            const base = frame.stack_offset;
            const is_top_frame = frame_index + 1 == active_frames.len;
            const owned_registers = if (is_top_frame) chunk.num_registers else active_frames[frame_index + 1].stack_offset - base;
            for (0..owned_registers) |i| {
                const is_heap_value = bitmap & (@as(u256, 1) << @intCast(i)) != 0;
                if (is_heap_value) {
                    try self.copyHeapValue(&task.stack[base + i]);
                }
            }
        }
    }

    fn copyHeapValue(self: *Self, value: *v.Value) !void {
        const raw: u64 = @bitCast(value.*);
        if (raw == 0) return;
        if (self.program.string_pool.contains(value.heap_obj)) return;
        if (v.Closure.isStatic(value.heap_obj)) return;
        const current = value.heap_obj.followRelocated();

        if (current.gc_forward) |fwd| {
            value.heap_obj = fwd;
            return;
        }
        const new_ptr = try cloneHeapObj(self.to_space, current);
        current.gc_forward = new_ptr;
        value.heap_obj = new_ptr;
        const has_references = new_ptr.kind != .string and new_ptr.kind != .buffer;
        if (has_references) {
            try self.heap.gc_queue.append(self.heap.queue_allocator, new_ptr);
        }
        self.heap.object_count += 1;
        self.heap.allocated += heapObjectSize(new_ptr);
    }

    fn copyChildren(self: *Self, obj: *v.HeapValue) !void {
        switch (obj.kind) {
            .array => {
                const array = v.HeapValue.getParentPtr(v.Array, obj);
                switch (array.kind) {
                    .string, .buffer, .@"struct", .array, .@"union", .nullable, .function, .task => {
                        const items = array.getDataPtr()[0..array.len];
                        for (items) |*item| {
                            try self.copyHeapValue(item);
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
                        try self.copyHeapValue(&fields[i]);
                    }
                }
            },
            .closure => {
                const closure = v.HeapValue.getParentPtr(v.Closure, obj);
                const kinds = closure.function.kind.dusk.capture_kinds;
                for (closure.captures(), kinds) |*capture, kind| {
                    if (!kind.isHeapType()) continue;
                    try self.copyHeapValue(capture);
                }
            },
            .cell => {
                const cell = v.HeapValue.getParentPtr(v.Cell, obj);
                if (cell.kind.isHeapType()) {
                    try self.copyHeapValue(&cell.value);
                }
            },
            .task => {
                const state = v.HeapValue.getParentPtr(v.Task, obj).state;
                state.is_reachable = true;
                if (state.hasHeapResult()) {
                    try self.copyHeapValue(&state.result);
                }
            },
            .map => {
                const map = v.HeapValue.getParentPtr(v.Map, obj);
                try self.copyHeapValue(&map.storage);
            },
            .map_storage => {
                const storage = v.HeapValue.getParentPtr(v.MapStorage, obj);
                const has_heap_keys = storage.key_type.isHeapType();
                const has_heap_values = storage.value_type.isHeapType();
                for (storage.usedEntries()) |*entry| {
                    if (has_heap_keys) try self.copyHeapValue(&entry.key);
                    if (has_heap_values) try self.copyHeapValue(&entry.value);
                }
            },
            .@"union" => {
                const u = v.HeapValue.getParentPtr(v.Union, obj);
                if (u.active_tag == v.Union.NO_TAG) return;
                const desc = self.program.union_descriptors[u.descriptor_id];
                if (desc.heap_bitmap & (@as(u256, 1) << @intCast(u.active_tag)) != 0) {
                    const slot = u.getDataPtr();
                    try self.copyHeapValue(&slot[0]);
                }
            },
            else => {},
        }
    }
};

fn binarySearch(comptime T: type, comptime field: []const u8, items: []const T, key: anytype) ?T {
    const K = @TypeOf(key);
    const idx = std.sort.binarySearch(T, items, key, struct {
        fn cmp(k: K, item: T) std.math.Order {
            return std.math.order(k, @field(item, field));
        }
    }.cmp);
    return if (idx) |i| items[i] else null;
}

pub fn cloneHeapObj(allocator: std.mem.Allocator, obj_ptr: *v.HeapValue) !*v.HeapValue {
    return switch (obj_ptr.kind) {
        .array => &(try v.HeapValue.getParentPtr(v.Array, obj_ptr).clone(allocator)).obj,
        .@"struct" => &(try v.HeapValue.getParentPtr(v.Struct, obj_ptr).clone(allocator)).obj,
        .@"union" => &(try v.HeapValue.getParentPtr(v.Union, obj_ptr).clone(allocator)).obj,
        .null_box => &(try v.HeapValue.getParentPtr(v.NullBox, obj_ptr).clone(allocator)).obj,
        .closure => &(try v.HeapValue.getParentPtr(v.Closure, obj_ptr).clone(allocator)).obj,
        .cell => &(try v.HeapValue.getParentPtr(v.Cell, obj_ptr).clone(allocator)).obj,
        .task => &(try v.HeapValue.getParentPtr(v.Task, obj_ptr).clone(allocator)).obj,
        .map => &(try v.HeapValue.getParentPtr(v.Map, obj_ptr).clone(allocator)).obj,
        .map_storage => &(try v.HeapValue.getParentPtr(v.MapStorage, obj_ptr).clone(allocator)).obj,
        .string, .buffer => &(try v.HeapValue.getParentPtr(v.Bytes, obj_ptr).clone(allocator)).obj,
    };
}

pub fn heapObjectSize(ptr: *v.HeapValue) usize {
    return switch (ptr.kind) {
        .array => v.Array.calc_size(v.HeapValue.getParentPtr(v.Array, ptr).capacity),
        .@"struct" => v.Struct.calc_size(v.HeapValue.getParentPtr(v.Struct, ptr).field_count),
        .@"union" => v.Union.calc_size(),
        .null_box => v.NullBox.calc_size(),
        .closure => v.Closure.calc_size(v.HeapValue.getParentPtr(v.Closure, ptr).capture_count),
        .cell => v.Cell.calc_size(),
        .task => v.Task.calc_size(),
        .map => v.Map.calc_size(),
        .map_storage => v.MapStorage.calc_size(v.HeapValue.getParentPtr(v.MapStorage, ptr).capacity),
        .string, .buffer => v.Bytes.calc_size(v.HeapValue.getParentPtr(v.Bytes, ptr).len),
    };
}
