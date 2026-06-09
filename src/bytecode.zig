const std = @import("std");
const ir = @import("ir.zig");

pub const BytecodeGen = struct {
    const Self = @This();

    allocator: std.mem.Allocator,

    var_register_id_by_uid: std.AutoHashMap(u32, u8),
    next_free_register: u32 = 0,
    chunk_constants: std.ArrayList(Value),
    chunk_instructions: std.ArrayList(Instruction),

    pub fn generate(self: *Self, program: *ir.Program) Program {
        const main_fn = self.genFunction(
            ir.Func{
                .uid = 0,
                .identifier = "$main",
                .params = .{},
                .body = program.instructions,
                .return_type = 0,
            },
        );

        return Program{
            .main_func_index = 0,
            .functions = .{main_fn},
        };
    }

    fn genFunction(self: *Self, func: ir.Func) Function {
        self.next_free_register = func.params.len;
        self.var_register_id_by_uid.clearRetainingCapacity();

        return Function{
            .uid = func.uid,
            .identifier = func.name,
            .number_of_params = func.params.len,
            .chunk = self.genFunctionChunk(func),
        };
    }

    fn genFunctionChunk(self: *Self, func: ir.Func) Chunk {
        for (func.body) |instruction| {
            switch (instruction) {
                .store_var => |store_var| {
                    _ = self.genStoreVar(store_var);
                },
                else => {},
            }
        }

        return Chunk{
            .instructions = self.chunk_instructions.toOwnedSlice(self.allocator),
            .constants = self.chunk_constants.toOwnedSlice(self.allocator),
        };
    }

    fn genStoreVar(self: *Self, store_var: ir.StoreVar) Instruction {
        const var_register_id = self.consumeRegister();
        _ = self.genValue(store_var.value, var_register_id);
        self.var_register_id_by_uid.put(store_var.uid, var_register_id);
    }

    fn genValue(self: *Self, value: *ir.Value, target_reg: u8) !u8 {
        switch (value.data) {
            .i_int, .i_float, .i_bool => {
                self.genLoadConstFromIntermediateValue(value, target_reg);
            },

            .identifier => |id| {
                const symbol_reg = self.var_register_id_by_uid(id.uid);

                const inst = Instruction{
                    .op = .LOAD,
                    .a = target_reg,
                    .b = symbol_reg,
                };

                try self.chunk_instructions.append(inst);
            },

            .binary_op => |bo| {
                try self.genBinOp(bo, self.consumeRegister());
            },

            .unary_op => |uo| {
                try self.genUnaryOp(uo, self.consumeRegister());
            },
        }

        return target_reg;
    }

    fn genLoadConstFromIntermediateValue(self: *Self, value: *ir.Value, target_reg: u8) void {
        const const_id = self.chunk_constants.items.len;

        try self.chunk_constants.append(self.allocator, Value.from_ir_value(value));

        const load_const = Instruction{
            .op = .LOAD_CONST,
            .a = const_id,
            .b = target_reg,
        };

        try self.chunk_instructions.append(load_const);
    }

    fn genBinOp(self: *Self, bo: *ir.BinaryOp, target_reg: u8) !void {
        const left_reg = try self.genValue(bo.left, self.consumeRegister());
        const right_reg = try self.genValue(bo.right, self.consumeRegister());
        defer self.freeRegister(2);

        const op = OpCode.from_ir_binary_op(bo.kind);
        const bo_inst = Instruction{
            .op = op,
            .a = target_reg,
            .b = left_reg,
            .c = right_reg,
        };

        try self.chunk_instructions.append(self.allocator, bo_inst);
    }

    fn genUnaryOp(self: *Self, bo: *ir.UnaryOp, target_reg: u8) !void {
        const aux_reg = try self.genValue(bo.right, self.consumeRegister());
        defer self.freeRegister();

        const op = OpCode.from_unary_op(bo.kind);
        const op_inst = Instruction{
            .op = op,
            .a = target_reg,
            .b = aux_reg,
        };

        try self.chunk_instructions.append(self.allocator, op_inst);
    }

    fn freeRegister(self: *Self, n: ?u32) void {
        const v = @max(0, self.next_free_register - (n orelse 1));
        self.next_free_register = v;
    }

    fn consumeRegister(self: *Self) u8 {
        //note: check u8 limit and panic
        const id = self.next_free_register;
        self.next_free_register += 1;
        return id;
    }
};

pub const Program = struct {
    main_func_index: usize,
    functions: []Function,
};

pub const Function = struct {
    uid: usize,
    name: []const u8,
    number_of_params: u8,
    chunk: Chunk,
};

const Chunk = struct {
    instructions: []Instruction,
    constants: []Value,
};

const Instruction = packed struct {
    op: OpCode,
    a: u8,
    b: u8,
    c: u8 = 0,
};

pub const Value = union(enum) {
    const Self = @This();

    i_int: i64,
    i_float: f64,
    i_bool: bool,
    i_null: void,

    fn from_ir_value(value: *ir.Value) Self {
        return switch (value.data) {
            .i_int => |i| Self{ .i_int = i },
            .i_float => |f| Self{ .i_float = f },
            .i_bool => |b| Self{ .i_bool = b },
            .i_null => Self{ .i_null = {} },
            else => unreachable,
        };
    }
};

const OpCode = enum(u8) {
    const Self = @This();

    LOAD_CONST,
    LOAD,
    STORE_VAR,

    I_ADD,
    I_SUB,
    I_MULT,
    I_DIV,
    I_MOD,
    I_EQ,
    I_NEQ,
    I_LT,
    I_LE,
    I_GT,
    I_GE,

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

    B_OR,
    B_AND,
    B_EQ,
    B_NEQ,
    B_NOT,

    CALL,
    RETURN,

    JUMP,
    JUMP_IF_FALSE,

    pub fn from_unary_op(op: ir.UnaryOpKind) Self {
        return switch (op) {
            .not => Self.B_NOT,
            //note: i_neg f_neg
            else => unreachable,
        };
    }

    pub fn from_ir_binary_op(op: ir.BinaryOpKind) Self {
        switch (op) {
            .i_add => Self.I_ADD,
            .i_sub => Self.I_SUB,
            .i_mult => Self.I_MULT,
            .i_div => Self.I_DIV,
            .i_mod => Self.I_MOD,

            .i_cmp_eq => Self.I_EQ,
            .i_cmp_neq => Self.I_NEQ,
            .i_cmp_lt => Self.I_LT,
            .i_cmp_le => Self.I_LE,
            .i_cmp_ge => Self.I_GE,
            .i_cmp_gt => Self.I_GT,

            .f_add => Self.F_ADD,
            .f_sub => Self.F_SUB,
            .f_mult => Self.F_MULT,
            .f_div => Self.F_DIV,
            .f_mod => Self.F_MOD,

            .f_cmp_eq => Self.F_EQ,
            .f_cmp_neq => Self.F_NEQ,
            .f_cmp_lt => Self.F_LT,
            .f_cmp_le => Self.F_LE,
            .f_cmp_ge => Self.F_GE,
            .f_cmp_gt => Self.F_GT,

            .b_and => Self.B_AND,
            .b_or => Self.B_OR,
            .b_cmp_eq => Self.B_EQ,
            .b_cmp_neq => Self.B_NEQ,
        }
    }
};
