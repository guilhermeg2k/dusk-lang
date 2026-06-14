const std = @import("std");
const ir = @import("ir.zig");

pub const BytecodeGen = struct {
    const Self = @This();

    const VOID_VALUE = ir.Value{
        .type_id = 0, // in this ctx type does not matter
        .data = .{
            .i_void = {},
        },
    };

    const TRUE_VALUE = ir.Value{
        .type_id = 0, // in this ctx type does not matter
        .data = .{
            .i_bool = true,
        },
    };

    allocator: std.mem.Allocator,

    var_register_id_by_uid: std.AutoHashMap(usize, u8),
    next_free_register: u8 = 0,
    //perf: this should be a "cached" array map i think
    chunk_constants: std.ArrayList(Value),
    chunk_instructions: std.ArrayList(Instruction),

    pub fn init(alloc: std.mem.Allocator) Self {
        return Self{
            .allocator = alloc,
            .var_register_id_by_uid = std.AutoHashMap(usize, u8).init(alloc),
            .next_free_register = 0,
            .chunk_constants = .empty,
            .chunk_instructions = .empty,
        };
    }

    pub fn generate(self: *Self, program: *const ir.Program, builtins: []const Function) !Program {
        var funcs: std.ArrayList(Function) = .empty;

        for (builtins) |bf| {
            try funcs.append(self.allocator, bf);
        }

        for (program.functions) |func| {
            try funcs.append(self.allocator, try self.genFunction(func));
        }

        const main_fn = try self.genFunction(
            ir.Func{
                .uid = 0,
                .identifier = "$main",
                .params = &.{},
                .body = program.instructions,
                .return_type = 0,
            },
        );
        try funcs.append(self.allocator, main_fn);

        return Program{
            .main_func_index = funcs.items.len - 1,
            .functions = try funcs.toOwnedSlice(self.allocator),
        };
    }

    fn genFunction(self: *Self, func: ir.Func) !Function {
        self.next_free_register = @intCast(func.params.len);
        self.var_register_id_by_uid.clearRetainingCapacity();

        for (func.params, 0..) |param, i| {
            try self.var_register_id_by_uid.put(param.uid, @intCast(i));
        }

        return Function{
            .uid = func.uid,
            .name = func.identifier,
            .kind = .{ .native = try self.genFunctionChunk(func) },
        };
    }

    fn genInstructionBlock(self: *Self, block: []const ir.Instruction) BytecodeError!void {
        for (block) |instruction| {
            switch (instruction) {
                .store_var => |store_var| {
                    _ = try self.genStoreVar(store_var);
                },
                .expression_stmt => |stmt| {
                    _ = try self.genValue(stmt.value, self.consumeRegister());
                },
                .branch_if => |stmt| {
                    try self.genBranchIf(stmt);
                },
                .loop => |stmt| {
                    try self.genLoop(stmt);
                },
                .return_stmt => |r_stmt| {
                    try self.genReturn(r_stmt);
                },

                else => {},
            }
        }
    }

    fn genFunctionChunk(self: *Self, func: ir.Func) !Chunk {
        try self.genInstructionBlock(func.body);
        const chunk = Chunk{
            .instructions = try self.chunk_instructions.toOwnedSlice(self.allocator),
            .constants = try self.chunk_constants.toOwnedSlice(self.allocator),
        };

        self.chunk_instructions = .empty;
        self.chunk_constants = .empty;

        return chunk;
    }

    fn genStoreVar(self: *Self, store_var: ir.StoreVar) !void {
        const register_idx = self.var_register_id_by_uid.get(store_var.uid);
        if (register_idx) |idx| {
            _ = try self.genValue(store_var.value, idx);
        } else {
            const reg_idx = try self.genValue(store_var.value, self.consumeRegister());
            try self.var_register_id_by_uid.put(store_var.uid, reg_idx);
        }
    }

    fn genBranchIf(self: *Self, ifStmt: ir.BranchIf) !void {
        const condition_reg = try self.genValue(ifStmt.condition, self.consumeRegister());
        defer self.freeRegister();

        const jump_into_else = Instruction{
            .op = .JUMP_IF_FALSE,
            .a = condition_reg,
        };

        try self.chunk_instructions.append(self.allocator, jump_into_else);
        const jump_into_else_idx = self.chunk_instructions.items.len - 1;

        try self.genInstructionBlock(ifStmt.then_block);

        if (ifStmt.else_block.len > 0) {
            const jump_pass_else = Instruction{
                .op = .JUMP,
            };

            try self.chunk_instructions.append(self.allocator, jump_pass_else);
            const jump_pass_else_idx = self.chunk_instructions.items.len - 1;
            const jump_into_else_position = self.chunk_instructions.items.len;

            try self.genInstructionBlock(ifStmt.else_block);
            const jump_pass_else_position = self.chunk_instructions.items.len;

            self.chunk_instructions.items[jump_into_else_idx].putBEx(@intCast(jump_into_else_position));
            self.chunk_instructions.items[jump_pass_else_idx].putAEx(@intCast(jump_pass_else_position));
        } else {
            const jump_into_else_position = self.chunk_instructions.items.len;
            self.chunk_instructions.items[jump_into_else_idx].putBEx(@intCast(jump_into_else_position));
        }
    }

    fn genLoop(self: *Self, loopStmt: ir.Loop) !void {
        const loop_begin_idx = self.chunk_instructions.items.len;

        //perf: this orelse
        const condition_reg = try self.genValue(loopStmt.condition orelse &TRUE_VALUE, self.consumeRegister());
        defer self.freeRegister();

        const jump_pass_loop = Instruction{
            .op = .JUMP_IF_FALSE,
            .a = condition_reg,
        };

        try self.chunk_instructions.append(self.allocator, jump_pass_loop);
        const jump_if_false_idx = self.chunk_instructions.items.len - 1;

        try self.genInstructionBlock(loopStmt.do_block);

        var jump_to_begin = Instruction{
            .op = .JUMP,
        };

        jump_to_begin.putAEx(@intCast(loop_begin_idx));

        try self.chunk_instructions.append(self.allocator, jump_to_begin);
        const jump_pass_loop_position = self.chunk_instructions.items.len;

        self.chunk_instructions.items[jump_if_false_idx].putBEx(@intCast(jump_pass_loop_position));
    }

    fn genFnCall(self: *Self, fc: ir.FnCall, target_reg: u8) !void {
        for (fc.args) |arg| {
            _ = try self.genValue(arg, self.consumeRegister());
        }

        var fn_call = Instruction{
            .op = .CALL,
            .a = target_reg,
        };

        fn_call.putBEx(@intCast(fc.fn_uid));

        try self.chunk_instructions.append(self.allocator, fn_call);

        self.freeRegisterN(fc.args.len);
    }

    fn genReturn(self: *Self, return_stmt: ir.ReturnStmt) !void {
        const return_value_register = try self.genValue(return_stmt.value orelse &VOID_VALUE, self.consumeRegister());

        const inst = Instruction{
            .op = .RETURN,
            .a = return_value_register,
        };

        try self.chunk_instructions.append(self.allocator, inst);
    }

    fn genValue(self: *Self, value: *const ir.Value, target_reg: u8) BytecodeError!u8 {
        switch (value.data) {
            .i_int, .i_float, .i_bool, .i_string, .i_null, .i_void => {
                try self.genLoadConstFromIntermediateValue(value, target_reg);
            },

            .identifier => |id| {
                const symbol_reg = self.var_register_id_by_uid.get(id.uid) orelse
                    return BytecodeError.UndefinedVariable;

                const inst = Instruction{
                    .op = .LOAD,
                    .a = target_reg,
                    .b = symbol_reg,
                };

                try self.chunk_instructions.append(self.allocator, inst);
            },

            .binary_op => |bo| {
                try self.genBinOp(bo, target_reg);
            },

            .unary_op => |uo| {
                try self.genUnaryOp(uo, target_reg);
            },

            .fn_call => |fc| {
                try self.genFnCall(fc, target_reg);
            },
            else => {},
        }

        return target_reg;
    }

    fn genLoadConstFromIntermediateValue(self: *Self, value: *const ir.Value, target_reg: u8) !void {
        const const_id = self.chunk_constants.items.len;

        try self.chunk_constants.append(self.allocator, Value.from_ir_value(value));

        var load_const = Instruction{
            .op = .LOAD_CONST,
            .a = target_reg,
        };

        load_const.putBEx(@intCast(const_id));

        try self.chunk_instructions.append(self.allocator, load_const);
    }

    fn genBinOp(self: *Self, bo: ir.BinaryOp, target_reg: u8) !void {
        const left_reg = try self.genValue(bo.left, self.consumeRegister());
        const right_reg = try self.genValue(bo.right, self.consumeRegister());
        defer self.freeRegisterN(2);

        const op = OpCode.fromIrBinaryOp(bo.kind);

        switch (bo.kind) {
            .f_add,
            .f_sub,
            .f_mult,
            .f_div,
            .f_mod,
            .f_cmp_eq,
            .f_cmp_neq,
            .f_cmp_lt,
            .f_cmp_le,
            .f_cmp_gt,
            .f_cmp_ge,
            => {
                if (bo.left.data == .i_int) {
                    const left_to_float = Instruction{
                        .op = .I_TO_F,
                        .a = left_reg,
                    };
                    try self.chunk_instructions.append(self.allocator, left_to_float);
                }

                if (bo.right.data == .i_int) {
                    const right_to_float = Instruction{
                        .op = .I_TO_F,
                        .a = right_reg,
                    };
                    try self.chunk_instructions.append(self.allocator, right_to_float);
                }
            },
            .trunc_div => {
                if (bo.left.data == .i_float) {
                    const left_to_int = Instruction{
                        .op = .F_TO_I,
                        .a = left_reg,
                    };
                    try self.chunk_instructions.append(self.allocator, left_to_int);
                }

                if (bo.right.data == .i_float) {
                    const right_to_int = Instruction{
                        .op = .F_TO_I,
                        .a = right_reg,
                    };
                    try self.chunk_instructions.append(self.allocator, right_to_int);
                }
            },
            else => {},
        }

        const bo_inst = Instruction{
            .op = op,
            .a = target_reg,
            .b = left_reg,
            .c = right_reg,
        };

        try self.chunk_instructions.append(self.allocator, bo_inst);
    }

    fn genUnaryOp(self: *Self, bo: ir.UnaryOp, target_reg: u8) !void {
        const aux_reg = try self.genValue(bo.right, self.consumeRegister());
        defer self.freeRegister();

        const op = OpCode.fromUnaryOp(bo.kind);
        const op_inst = Instruction{
            .op = op,
            .a = target_reg,
            .b = aux_reg,
        };

        try self.chunk_instructions.append(self.allocator, op_inst);
    }

    fn freeRegister(self: *Self) void {
        self.freeRegisterN(1);
    }

    fn freeRegisterN(self: *Self, n: usize) void {
        const v = @max(0, self.next_free_register - n);
        self.next_free_register = @intCast(v);
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
    functions: []const Function,
};

pub const BuiltinFn = struct {
    func: *const fn (args: []Value) Value,
    num_args: u8,
};

pub const Function = struct {
    uid: usize,
    name: []const u8,
    kind: union(enum) {
        native: Chunk,
        builtin: BuiltinFn,
    },
};

const Chunk = struct {
    const Self = @This();

    instructions: []Instruction,
    constants: []Value,

    pub fn disasamble(self: *const Self) void {
        std.debug.print("== constants ({d}) ==\n", .{self.constants.len});
        for (self.constants, 0..) |constant, i| {
            std.debug.print("{d:0>4}   {any}\n", .{ i, constant });
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
            .B_NOT, .I_NEG, .F_NEG => std.debug.print("R[{d}] R[{d}]", .{ inst.a, inst.b }),
            .CALL => std.debug.print("R[{d}] FN[{d}]", .{ inst.a, inst.bEx() }),
            .RETURN => std.debug.print("R[{d}]", .{inst.a}),
            .JUMP => std.debug.print("I[{d}]", .{inst.aEx()}),
            .JUMP_IF_FALSE => std.debug.print("R[{d}] I[{d}]", .{ inst.a, inst.bEx() }),
        }
        std.debug.print("\n", .{});
    }
};

const Instruction = packed struct {
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

pub const Value = union(enum) {
    const Self = @This();

    i_int: i64,
    i_float: f64,
    i_bool: bool,
    i_null: void,
    i_string: []const u8,

    pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .i_int => |v| try writer.print("int {d}", .{v}),
            .i_float => |v| try writer.print("float {d}", .{v}),
            .i_bool => |v| try writer.print("bool {}", .{v}),
            .i_null => try writer.print("null", .{}),
        }
    }

    fn from_ir_value(value: *const ir.Value) Self {
        return switch (value.data) {
            .i_int => |i| Self{ .i_int = i },
            .i_float => |f| Self{ .i_float = f },
            .i_bool => |b| Self{ .i_bool = b },
            .i_string => |s| Self{ .i_string = s },
            .i_null, .i_void => Self{ .i_null = {} },
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

    CALL,
    RETURN,

    JUMP,
    JUMP_IF_FALSE,

    pub fn fromUnaryOp(op: ir.UnaryOpKind) Self {
        return switch (op) {
            .not => Self.B_NOT,
            .i_neg => Self.I_NEG,
            .f_neg => Self.F_NEG,
        };
    }

    pub fn fromIrBinaryOp(op: ir.BinaryOpKind) Self {
        return switch (op) {
            .i_add => Self.I_ADD,
            .i_sub => Self.I_SUB,
            .i_mult => Self.I_MULT,
            .i_mod => Self.I_MOD,

            .trunc_div => Self.TRUNC_DIV,

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
        };
    }
};

const BytecodeError = error{ OutOfMemory, UndefinedVariable };
