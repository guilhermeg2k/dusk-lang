const std = @import("std");
const v = @import("value.zig");
const VM = @import("vm.zig").VM;
const natives = @import("builtins/natives.zig");

pub const UnionDescriptor = struct {
    heap_bitmap: u256,
    box_bitmap: u256,
    variant_kinds: []const v.ValueType,
};

pub const StructDescriptor = struct {
    field_count: u8,
    heap_bitmap: u256,
};

pub const HeapMap = struct {
    pc: u16,
    bitmap: u256,
};

pub const StringPool = struct {
    const Self = @This();
    const alignment = std.mem.Alignment.fromByteUnits(@alignOf(v.Bytes));

    memory: []align(@alignOf(v.Bytes)) u8 = &.{},
    values: []const v.Value = &.{},

    pub fn init(allocator: std.mem.Allocator, texts: []const []const u8) !Self {
        var total: usize = 0;
        for (texts) |text| total += alignment.forward(v.Bytes.calc_size(text.len));

        const memory = try allocator.alignedAlloc(u8, alignment, total);
        const values = try allocator.alloc(v.Value, texts.len);
        var offset: usize = 0;
        for (texts, 0..) |text, i| {
            const bytes: *v.Bytes = @ptrCast(@alignCast(memory[offset..].ptr));
            bytes.obj = .{ .kind = .string };
            bytes.len = text.len;
            @memcpy(bytes.getDataPtr()[0..text.len], text);
            values[i] = .{ .heap_obj = &bytes.obj };
            offset += alignment.forward(v.Bytes.calc_size(text.len));
        }

        return .{ .memory = memory, .values = values };
    }

    pub fn contains(self: *const Self, obj: *const v.HeapValue) bool {
        const address = @intFromPtr(obj);
        const start = @intFromPtr(self.memory.ptr);
        return address >= start and address < start + self.memory.len;
    }

    pub fn textAt(self: *const Self, index: usize) []const u8 {
        return v.HeapValue.getParentPtr(v.Bytes, self.values[index].heap_obj).slice();
    }
};

pub const Program = struct {
    string_pool: StringPool,
    main_func_index: usize,
    functions: []const Function,
    static_closures: []const v.Closure,
    struct_descriptors: []const StructDescriptor,
    union_descriptors: []const UnionDescriptor,
    error_names: []const []const u8,
};

pub const NativeResult = struct { value: v.Value, has_failed: bool = false, is_parked: bool = false };

pub const NativeFnPtr = *const fn (vm: *VM, args: []v.Value) anyerror!NativeResult;

pub const NativeFn = struct {
    func: NativeFnPtr,
    num_args: u8,
};

pub const FunctionKind = union(enum) {
    dusk: Chunk,
    native: NativeFn,
    intrinsic: natives.Intrinsic,
};

pub const Function = struct {
    uid: usize,
    name: []const u8,
    kind: FunctionKind,
};

pub const JumpTable = struct {
    min: i64,
    target_offsets: []const u16,
    default: u16,
};

pub const Chunk = struct {
    const Self = @This();

    num_registers: u8,
    num_params: u8,
    instructions: []Instruction,
    constants: []v.Value,
    shadow_types: []v.ValueType,
    heap_maps: []const HeapMap,
    build_recipes: []const []const v.ValueType,
    jump_tables: []const JumpTable = &.{},
    capture_kinds: []const v.ValueType = &.{},

    pub fn disasamble(self: *const Self) void {
        std.debug.print("\n== constants ({d}) ==\n", .{self.constants.len});
        for (self.constants, 0..) |constant, i| {
            const ty = self.shadow_types[i];
            std.debug.print("{d:0>4}  {s: <10} ", .{ i, @tagName(ty) });
            switch (ty) {
                .int64 => std.debug.print("{d}", .{constant.int64}),
                .float64 => std.debug.print("{d}", .{constant.float64}),
                .bool => std.debug.print("{}", .{constant.bool}),
                .null => std.debug.print("null", .{}),
                .array => std.debug.print("<array>", .{}),
                .@"struct" => std.debug.print("<struct>", .{}),
                .@"union" => std.debug.print("<union>", .{}),
                .nullable => std.debug.print("<nullable:", .{}),
                .function => std.debug.print("<func>", .{}),
                .cell => std.debug.print("<cell>", .{}),
                .task => std.debug.print("<task>", .{}),
                .string, .buffer => unreachable,
            }
            std.debug.print("\n", .{});
        }

        std.debug.print("\n== code ({d}) ==\n", .{self.instructions.len});
        for (self.instructions, 0..) |_, offset| {
            self.disassembleInstruction(offset);
        }
    }

    fn disassembleInstruction(self: *const Self, offset: usize) void {
        const inst = self.instructions[offset];
        const op = @tagName(inst.op);
        std.debug.print("{d:0>4} {s:<16}", .{ offset, op });

        switch (inst.op) {
            .LOAD_CONST => std.debug.print("R[{d}] C[{d}]", .{ inst.a, inst.bEx() }),
            .LOAD_STRING => std.debug.print("R[{d}] STR[{d}]", .{ inst.a, inst.bEx() }),
            .LOAD => std.debug.print("R[{d}] R[{d}]", .{ inst.a, inst.b }),
            .STORE_VAR => std.debug.print("R[{d}] R[{d}] R[{d}]", .{ inst.a, inst.b, inst.c }),
            .I_ADD,
            .I_SUB,
            .I_MULT,
            .TRUNC_DIV,
            .I_MOD,
            .I_EQ,
            .I_NEQ,
            .I_LT,
            .I_LE,
            .I_GT,
            .I_GE,
            .F_ADD,
            .F_SUB,
            .F_MULT,
            .F_DIV,
            .F_MOD,
            .F_EQ,
            .F_NEQ,
            .F_LT,
            .F_LE,
            .F_GT,
            .F_GE,
            .B_OR,
            .B_AND,
            .B_EQ,
            .B_NEQ,
            .I_TO_F,
            .F_TO_I,
            => std.debug.print("R[{d}] R[{d}] R[{d}]", .{ inst.a, inst.b, inst.c }),
            .B_NOT, .I_NEG, .F_NEG, .I_TO_BYTE => std.debug.print("R[{d}] R[{d}]", .{ inst.a, inst.b }),
            .CALL, .TAIL_CALL, .LOAD_FUNC, .MAKE_CLOSURE => std.debug.print("R[{d}] FN[{d}]", .{ inst.a, inst.bEx() }),
            .CAPTURE_GET => std.debug.print("R[{d}] CAP[{d}]", .{ inst.a, inst.b }),
            .CELL_NEW => std.debug.print("R[{d}] K[{d}] R[{d}]", .{ inst.a, inst.b, inst.c }),
            .CELL_GET, .CELL_SET => std.debug.print("R[{d}] R[{d}]", .{ inst.a, inst.b }),
            .CALL_VALUE => std.debug.print("R[{d}]", .{inst.a}),
            .RETURN, .RETURN_ERR, .PANIC_ERR => std.debug.print("R[{d}]", .{inst.a}),
            .JUMP => std.debug.print("I[{d}]", .{inst.aEx()}),
            .JUMP_IF_FALSE => std.debug.print("R[{d}] I[{d}]", .{ inst.a, inst.bEx() }),
            .JUMP_TABLE => std.debug.print("R[{d}] T[{d}]", .{ inst.a, inst.bEx() }),
            .ARRAY_LOAD,
            .ARRAY_STORE,
            .BYTES_GET,
            .BYTES_SET,
            => std.debug.print("R[{d}] R[{d}] R[{d}]", .{ inst.a, inst.b, inst.c }),
            .ARRAY_APPEND,
            .ARRAY_LEN,
            .ARRAY_POP,
            .BYTES_NEW,
            .BYTES_LEN,
            .DEEP_COPY,
            => std.debug.print("R[{d}] R[{d}]", .{ inst.a, inst.b }),
            .ARRAY_INIT => std.debug.print("R[{d}] T[{d}] C[{d}]", .{ inst.a, inst.b, inst.c }),
            .STR_BUILD => std.debug.print("R[{d}] RECIPE[{d}]", .{ inst.a, inst.bEx() }),
            .TO_STRING, .TASK_SPAWN => std.debug.print("R[{d}] R[{d}] T[{d}]", .{ inst.a, inst.b, inst.c }),
            .STRUCT_INIT => std.debug.print("R[{d}] N[{d}]", .{ inst.a, inst.b }),
            .STRUCT_STORE => std.debug.print("R[{d}] F[{d}] R[{d}]", .{ inst.a, inst.b, inst.c }),
            .STRUCT_LOAD => std.debug.print("R[{d}] R[{d}] F[{d}]", .{ inst.a, inst.b, inst.c }),
            .UNION_INIT => std.debug.print("R[{d}] D[{d}]", .{ inst.a, inst.bEx() }),
            .UNION_LOAD => std.debug.print("R[{d}] R[{d}] T[{d}]", .{ inst.a, inst.b, inst.c }),
            .UNION_STORE => std.debug.print("R[{d}] T[{d}] R[{d}]", .{ inst.a, inst.b, inst.c }),
            .UNION_TAG, .UNION_PAYLOAD => std.debug.print("R[{d}] R[{d}]", .{ inst.a, inst.b }),
            .NULL_BOX_INIT => std.debug.print("R[{d}] B[{d}] R[{d}]", .{ inst.a, inst.b, inst.c }),
            .NULL_BOX_UNWRAP,
            .NULL_BOX_NNULL,
            => std.debug.print("R[{d}] R[{d}]", .{ inst.a, inst.b }),
        }
        std.debug.print("\n", .{});
    }
};

pub const Instruction = packed struct {
    const Self = @This();

    op: OpCode,
    a: u8 = 0,
    b: u8 = 0,
    c: u8 = 0,

    pub fn putAEx(self: *Self, value: u24) void {
        self.a = @intCast(value >> 8);
        self.b = @intCast(value & 0xFF);
    }

    pub fn aEx(self: Self) u24 {
        return (@as(u16, self.a) << 8) | self.b;
    }

    pub fn putBEx(self: *Self, value: u16) void {
        self.b = @intCast(value >> 8);
        self.c = @intCast(value & 0xFF);
    }

    pub fn bEx(self: Self) u16 {
        return (@as(u16, self.b) << 8) | self.c;
    }
};

pub const OpCode = enum(u8) {
    const Self = @This();

    LOAD_CONST,
    LOAD,
    STORE_VAR,

    LOAD_STRING,
    STR_BUILD,
    TO_STRING,

    ARRAY_INIT,
    ARRAY_LOAD,
    ARRAY_STORE,
    ARRAY_LEN,
    ARRAY_APPEND,
    ARRAY_POP,

    BYTES_NEW,
    BYTES_LEN,
    BYTES_GET,
    BYTES_SET,
    DEEP_COPY,

    STRUCT_INIT,
    STRUCT_LOAD,
    STRUCT_STORE,

    UNION_INIT,
    UNION_LOAD,
    UNION_STORE,
    UNION_TAG,
    UNION_PAYLOAD,

    NULL_BOX_INIT,
    NULL_BOX_UNWRAP,

    NULL_BOX_NNULL,

    I_ADD,
    I_SUB,
    I_MULT,
    I_MOD,
    I_EQ,
    I_NEQ,
    I_LT,
    I_LE,
    I_GT,
    I_GE,
    I_NEG,

    TRUNC_DIV,
    I_TO_F,
    F_TO_I,
    I_TO_BYTE,

    F_ADD,
    F_SUB,
    F_MULT,
    F_DIV,
    F_MOD,
    F_EQ,
    F_NEQ,
    F_LT,
    F_LE,
    F_GT,
    F_GE,
    F_NEG,

    B_OR,
    B_AND,
    B_EQ,
    B_NEQ,
    B_NOT,

    LOAD_FUNC,
    MAKE_CLOSURE,
    CAPTURE_GET,
    CELL_NEW,
    CELL_GET,
    CELL_SET,
    CALL,
    CALL_VALUE,
    TAIL_CALL,
    RETURN,
    RETURN_ERR,
    PANIC_ERR,

    JUMP,
    JUMP_IF_FALSE,
    JUMP_TABLE,

    TASK_SPAWN,

    pub fn canFail(self: Self) bool {
        return self == .CALL or self == .CALL_VALUE;
    }

    pub fn canTriggerGc(self: Self) bool {
        return switch (self) {
            .ARRAY_INIT,
            .ARRAY_APPEND,
            .STRUCT_INIT,
            .UNION_INIT,
            .UNION_LOAD,
            .NULL_BOX_INIT,
            .STR_BUILD,
            .TO_STRING,
            .BYTES_NEW,
            .DEEP_COPY,
            .CALL,
            .CALL_VALUE,
            .MAKE_CLOSURE,
            .CELL_NEW,
            .TASK_SPAWN,
            => true,
            else => false,
        };
    }

};
