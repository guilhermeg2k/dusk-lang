const std = @import("std");
const ir = @import("ir.zig");

pub const BytecodeGen = struct {
    const Self = @This();

    allocator: std.mem.Allocator,

    var_register_id_by_uid: std.AutoHashMap(u32, u8),
    next_free_register: u32 = 0,
    chunk_constants: std.ArrayList(Value),
    chunk_instructions: std.ArrayList(Instruction),

    pub fn generate(program: *ir.Program) Program {}

    fn genFunction(self: *Self, func: ir.Func) Function {
        self.next_free_register = func.params.len;
        self.var_register_id_by_uid.clearRetainingCapacity();

        return Function{
            .uid = func.uid,
            .identifier = func.name,
            .number_of_params = func.params.len,
        };
    }

    fn genFunctionChunk(self: *Self, func: ir.Func) Chunk {
        var chunk = Chunk{ .instructions = &.empty, .constants = &.empty };

        for (func.body) |instruction| {
            switch (instruction) {
                .store_var => |store_var| {
                    _ = self.genStoreVar(store_var);
                },
            }
        }
    }

    fn genStoreVar(self: *Self, store_var: ir.StoreVar) Instruction {
        const var_register_id = self.consumeRegister();
        _ = self.genValue(store_var.value, var_register_id);
        self.var_register_id_by_uid.put(store_var.uid, var_register_id);
    }

    fn genValue(self: *Self, value: *ir.Value, target_reg: u8) !Value {
        switch (value) {
            .i_float => |v| {
                const const_id = self.chunk_constants.items.len;

                try self.chunk_constants.append(self.allocator, .{
                    .i_float = v,
                });

                const load_const = Instruction{
                    .op = .LOAD_CONST,
                    .a = const_id,
                    .b = target_reg,
                };

                try self.chunk_instructions.append(load_const);
            },
        }
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
    c: u8,
};

pub const Value = union(enum) {
    i_int: i64,
    i_float: f64,
    i_bool: bool,
    i_null: void,
};

const OpCode = enum(u8) {
    LOAD_CONST,
    LOAD_VAR,
    STORE_VAR,

    I_ADD,
    I_SUB,
    I_MULT,
    I_DIV,
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
};
