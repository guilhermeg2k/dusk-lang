const std = @import("std");
const ir = @import("ir.zig");
const v = @import("value.zig");
const sema = @import("sema.zig");

pub const BytecodeGen = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    type_table: *sema.TypeTable,

    var_register_id_by_uid: std.AutoHashMap(usize, u8),

    register_types: [256]sema.TypeId = [_]sema.TypeId{0} ** 256,
    next_free_register: u8 = 0,
    peak_register: u8 = 0,

    chunk_constants: std.ArrayList(v.Value),
    chunk_constants_types: std.ArrayList(v.ValueType),
    const_id_by_value: std.HashMap(v.TypedValue, u32, v.TypedValueHashContext, 80),

    chunk_string_constants: std.ArrayList([]const u8),
    string_const_id_by_value: std.StringHashMap(u16),

    chunk_instructions: std.ArrayList(Instruction),
    loop_stack: std.ArrayList(LoopContext),
    break_patches: std.ArrayList(usize),
    inline_functions_by_uid: std.AutoHashMap(usize, InlineFn),

    static_store: StaticStoreTable,

    chunk_heap_maps: std.ArrayList(HeapMap),
    struct_descriptors: std.ArrayList(StructDescriptor),
    type_id_to_struct_descriptor_idx: std.AutoHashMap(sema.TypeId, u16),
    static_heap_bitmap: std.ArrayList(u64) = .empty,

    pub fn init(alloc: std.mem.Allocator, type_table: *sema.TypeTable) Self {
        return Self{
            .allocator = alloc,
            .type_table = type_table,
            .register_types = [_]sema.TypeId{type_table.getPrimitive(.void)} ** 256,
            .var_register_id_by_uid = std.AutoHashMap(usize, u8).init(alloc),
            .next_free_register = 0,
            .peak_register = 0,
            .chunk_constants = .empty,
            .chunk_constants_types = .empty,
            .const_id_by_value = std.HashMap(v.TypedValue, u32, v.TypedValueHashContext, 80).init(alloc),
            .chunk_string_constants = .empty,
            .string_const_id_by_value = std.StringHashMap(u16).init(alloc),
            .chunk_instructions = .empty,
            .loop_stack = .empty,
            .break_patches = .empty,
            .inline_functions_by_uid = std.AutoHashMap(usize, InlineFn).init(alloc),
            .static_store = StaticStoreTable.init(alloc),
            .chunk_heap_maps = .empty,
            .struct_descriptors = .empty,
            .type_id_to_struct_descriptor_idx = std.AutoHashMap(sema.TypeId, u16).init(alloc),
        };
    }

    pub fn generate(self: *Self, program: *const ir.Program, builtins: []const Function) !Program {
        var funcs: std.ArrayList(Function) = .empty;
        self.next_free_register = 0;
        self.peak_register = 0;
        self.type_id_to_struct_descriptor_idx.clearRetainingCapacity();
        self.var_register_id_by_uid.clearRetainingCapacity();

        for (builtins) |bf| {
            switch (bf.kind) {
                .@"inline" => |inl| try self.inline_functions_by_uid.put(bf.uid, inl),
                else => {},
            }
            try funcs.append(self.allocator, bf);
        }

        for (program.structs) |@"struct"| {
            try self.genStructStaticFields(@"struct");
            for (@"struct".funcs) |func| {
                try funcs.append(self.allocator, try self.genFunction(func));
            }
        }

        for (program.functions) |func| {
            try funcs.append(self.allocator, try self.genFunction(func));
        }

        try funcs.append(self.allocator, try self.genFunction(.{
            .uid = 0,
            .identifier = "$main",
            .params = &.{},
            .body = program.instructions,
            .return_type = self.type_table.getPrimitive(.void),
        }));

        // for (funcs.items) |func| {
        //     switch (func.kind) {
        //         .dusk => {
        //             func.kind.dusk.disasamble();
        //         },
        //         else => {},
        //     }
        // }

        return Program{
            .main_func_index = funcs.items.len - 1,
            .functions = try funcs.toOwnedSlice(self.allocator),
            .static_count = self.static_store.count(),
            .static_heap_bitmap = try self.static_heap_bitmap.toOwnedSlice(self.allocator),
            .struct_descriptors = try self.struct_descriptors.toOwnedSlice(self.allocator),
        };
    }

    fn genStructStaticFields(self: *Self, @"struct": ir.Struct) !void {
        try self.static_store.registerStructFields(@"struct");

        for (@"struct".static_fields) |field| {
            const slot_id = try self.static_store.getSlot(@"struct".type_id, field.identifier);
            const vt = v.ValueType.fromTypeId(self.type_table, field.type_id);
            if (vt.isHeapType()) {
                try self.setStaticHeapBit(slot_id);
            }

            if (field.default_value) |default| {
                const reg = try self.genValue(default, self.consumeRegister());
                defer self.freeRegister();

                var store = Instruction{ .op = .STATIC_STORE, .a = reg };
                store.putBEx(slot_id);

                try self.chunk_instructions.append(self.allocator, store);
            }
        }
    }

    fn genFunction(self: *Self, func: ir.Func) !Function {
        self.next_free_register = @intCast(func.params.len);
        self.peak_register = @intCast(func.params.len);
        self.var_register_id_by_uid.clearRetainingCapacity();
        @memset(&self.register_types, self.type_table.getPrimitive(.void));

        for (func.params, 0..) |param, i| {
            try self.var_register_id_by_uid.put(param.uid, @intCast(i));
            self.register_types[i] = param.type_id;
        }

        return Function{
            .uid = func.uid,
            .name = func.identifier,
            .kind = .{ .dusk = try self.genFunctionChunk(func) },
        };
    }

    fn genFunctionChunk(self: *Self, func: ir.Func) !Chunk {
        defer {
            self.chunk_instructions = .empty;
            self.chunk_constants = .empty;
            self.chunk_constants_types = .empty;
            self.chunk_string_constants = .empty;
            self.chunk_heap_maps = .empty;
            self.const_id_by_value.clearRetainingCapacity();
            self.string_const_id_by_value.clearRetainingCapacity();
        }

        try self.genInstructionBlock(func.body);

        if (self.chunk_instructions.getLastOrNull()) |last| {
            if (last.op != .RETURN) {
                try self.chunk_instructions.append(self.allocator, Instruction{ .op = .RETURN });
            }
        }

        const chunk = Chunk{
            .num_registers = self.peak_register,
            .instructions = try self.chunk_instructions.toOwnedSlice(self.allocator),
            .constants = try self.chunk_constants.toOwnedSlice(self.allocator),
            .shadow_types = try self.chunk_constants_types.toOwnedSlice(self.allocator),
            .string_constants = try self.chunk_string_constants.toOwnedSlice(self.allocator),
            .heap_maps = try self.chunk_heap_maps.toOwnedSlice(self.allocator),
        };

        return chunk;
    }

    fn genInstructionBlock(self: *Self, block: []const ir.Instruction) anyerror!void {
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
                .update_indexed => |stmt| {
                    _ = try self.genUpdateArrayValue(stmt);
                },
                .break_stmt => try self.genBreak(),
                .continue_stmt => try self.genContinue(),
            }
        }
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
        const saved_next_free = self.next_free_register;

        const condition_reg = try self.genValue(ifStmt.condition, self.consumeRegister());

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

        self.freeRegisterN(self.next_free_register - saved_next_free);
    }

    fn genLoop(self: *Self, loopStmt: ir.Loop) !void {
        const saved_next_free = self.next_free_register;
        const loop_begin_idx = self.chunk_instructions.items.len;

        try self.pushLoopCtx(loop_begin_idx);

        const condition_reg = if (loopStmt.condition) |cond|
            try self.genValue(cond, self.consumeRegister())
        else
            try self.genTrueValue(self.consumeRegister());

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

        try self.popLoopCtx();

        self.freeRegisterN(self.next_free_register - saved_next_free);
    }

    fn genBreak(self: *Self) !void {
        try self.break_patches.append(self.allocator, self.chunk_instructions.items.len);
        try self.chunk_instructions.append(self.allocator, Instruction{ .op = .JUMP });
    }

    fn genContinue(self: *Self) !void {
        const loop_ctx = self.loop_stack.items[self.loop_stack.items.len - 1];
        var jump = Instruction{ .op = .JUMP };
        jump.putAEx(@intCast(loop_ctx.loop_begin));
        try self.chunk_instructions.append(self.allocator, jump);
    }

    fn pushLoopCtx(self: *Self, loop_begin: usize) !void {
        try self.loop_stack.append(self.allocator, LoopContext{
            .loop_begin = loop_begin,
            .first_break_patch_idx = self.break_patches.items.len,
        });
    }

    fn popLoopCtx(self: *Self) !void {
        const ctx = self.loop_stack.pop() orelse unreachable;
        const loop_end = self.chunk_instructions.items.len;
        for (self.break_patches.items[ctx.first_break_patch_idx..]) |patch_idx| {
            self.chunk_instructions.items[patch_idx].putAEx(@intCast(loop_end));
        }
        self.break_patches.shrinkRetainingCapacity(ctx.first_break_patch_idx);
    }

    fn genFnCall(self: *Self, fc: ir.FnCall, value: *const ir.Value, target_reg: u8) !void {
        for (fc.args) |arg| {
            _ = try self.genValue(arg, self.consumeRegister());
        }

        if (self.inline_functions_by_uid.get(fc.fn_uid)) |inl| {
            try self.genInlineCall(inl, fc, value, target_reg);
            return;
        }

        //note: temp hacky for echo
        if (fc.fn_uid == 0) {
            const arg_type_id = fc.args[0].type_id;
            const arg_value_type = v.ValueType.fromTypeId(self.type_table, arg_type_id);
            const type_reg = self.consumeRegister();
            const type_value = ir.Value{
                .type_id = self.type_table.getPrimitive(.int),
                .data = .{ .i_int = @intFromEnum(arg_value_type) },
            };
            try self.genLoadConst(v.TypedValue.from_ir_value(&type_value), type_reg);
            self.register_types[type_reg] = self.type_table.getPrimitive(.int);
        }

        var fn_call = Instruction{
            .op = .CALL,
            .a = target_reg,
        };

        fn_call.putBEx(@intCast(fc.fn_uid));

        try self.chunk_instructions.append(self.allocator, fn_call);
        try self.recordHeapMap();

        const num_args = fc.args.len + if (fc.fn_uid == 0) @as(usize, 1) else @as(usize, 0);
        self.freeRegisterN(num_args);
        self.register_types[target_reg] = value.type_id;
    }

    fn genInlineCall(self: *Self, inl: InlineFn, fc: ir.FnCall, value: *const ir.Value, target_reg: u8) !void {
        const instructions = try inl.gen(target_reg, target_reg + 1, self.allocator);
        defer self.allocator.free(instructions);
        try self.chunk_instructions.appendSlice(self.allocator, instructions);
        for (instructions, 0..) |inl_inst, j| {
            if (inl_inst.op.canTriggerGc()) {
                const pc: u16 = @intCast(self.chunk_instructions.items.len - instructions.len + j);
                try self.recordHeapMapAt(pc);
            }
        }
        self.freeRegisterN(fc.args.len);
        self.register_types[target_reg] = value.type_id;
    }

    fn genReturn(self: *Self, return_stmt: ir.ReturnStmt) !void {
        const return_value_register = if (return_stmt.value) |val|
            try self.genValue(val, self.consumeRegister())
        else
            try self.genVoidValue(self.consumeRegister());

        const inst = Instruction{
            .op = .RETURN,
            .a = return_value_register,
        };

        try self.chunk_instructions.append(self.allocator, inst);
    }

    fn genValue(self: *Self, value: *const ir.Value, target_reg: u8) anyerror!u8 {
        switch (value.data) {
            .i_int, .i_float, .i_bool, .i_null, .i_void => {
                try self.genLoadConst(v.TypedValue.from_ir_value(value), target_reg);
                self.register_types[target_reg] = value.type_id;
            },

            .i_string => |s| {
                try self.genLoadString(s, target_reg);
                self.register_types[target_reg] = value.type_id;
            },

            .indexed => |idx| {
                try self.genIndexedLoad(idx, value, target_reg);
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
                self.register_types[target_reg] = self.register_types[symbol_reg];
            },

            .i_array => {
                try self.genArrayInit(value, target_reg);
            },

            .struct_init => {
                try self.genStructInit(value, target_reg);
            },

            .binary_op => |bo| {
                try self.genBinOp(bo, target_reg);
            },

            .unary_op => |uo| {
                try self.genUnaryOp(uo, target_reg);
            },

            .fn_call => |fc| {
                try self.genFnCall(fc, value, target_reg);
            },
        }

        return target_reg;
    }

    fn genIndexedLoad(self: *Self, idx: ir.IndexedValue, value: *const ir.Value, target_reg: u8) !void {
        const target_type = self.type_table.getTypePtrById(idx.target.type_id);

        switch (target_type.kind) {
            .@"struct" => |@"struct"| {
                const field_name = idx.index.data.i_string;
                if (@"struct".field_index_by_name.get(field_name)) |field_index| {
                    const struct_reg = try self.genValue(idx.target, self.consumeRegister());
                    defer self.freeRegister();
                    try self.genStructLoad(target_reg, struct_reg, @intCast(field_index));
                    return;
                }

                var load_static = Instruction{
                    .op = .STATIC_LOAD,
                    .a = target_reg,
                };

                const slot_id = try self.static_store.getSlot(idx.target.type_id, field_name);
                load_static.putBEx(slot_id);

                try self.chunk_instructions.append(self.allocator, load_static);
                self.register_types[target_reg] = value.type_id;
            },
            .array => {
                const array_reg = try self.genValue(idx.target, self.consumeRegister());
                defer self.freeRegister();
                try self.genArrayLoad(target_reg, array_reg, idx.index);
                self.register_types[target_reg] = value.type_id;
            },
            else => unreachable,
        }
    }

    fn genArrayInit(self: *Self, value: *const ir.Value, target_reg: u8) !void {
        const array_type = self.type_table.getTypePtrById(value.type_id);
        const array_inner_type_id = array_type.kind.array;

        const init_inst = Instruction{
            .op = .ARRAY_INIT,
            .a = target_reg,
            .b = @intFromEnum(v.ValueType.fromTypeId(self.type_table, array_inner_type_id)),
            .c = @intCast(@min(@max(value.data.i_array.values.len, 8), 255)),
        };

        try self.chunk_instructions.append(self.allocator, init_inst);
        try self.recordHeapMap();
        self.register_types[target_reg] = value.type_id;

        for (value.data.i_array.values) |vl| {
            try self.genArrayAppend(vl, target_reg);
        }
    }

    fn genArrayLoad(self: *Self, target_reg: u8, array_reg: u8, index_value: *ir.Value) !void {
        const index_reg = try self.genValue(index_value, self.consumeRegister());
        defer self.freeRegister();

        const load_inst = Instruction{
            .op = .ARRAY_LOAD,
            .a = target_reg,
            .b = array_reg,
            .c = index_reg,
        };

        try self.chunk_instructions.append(self.allocator, load_inst);
    }

    fn genArrayStore(self: *Self, array_reg: u8, index_value: *ir.Value, value: *ir.Value) !void {
        const index_reg = try self.genValue(index_value, self.consumeRegister());
        defer self.freeRegister();

        const value_reg = try self.genValue(value, self.consumeRegister());
        defer self.freeRegister();

        const store_inst = Instruction{
            .op = .ARRAY_STORE,
            .a = array_reg,
            .b = index_reg,
            .c = value_reg,
        };

        try self.chunk_instructions.append(self.allocator, store_inst);
    }

    fn genUpdateArrayValue(self: *Self, stmt: ir.UpdateIndexed) !void {
        const indexed = stmt.target.data.indexed;
        const target_type = self.type_table.getTypePtrById(indexed.target.type_id);

        switch (target_type.kind) {
            .@"struct" => |@"struct"| {
                const field_name = indexed.index.data.i_string;
                if (@"struct".field_index_by_name.get(field_name)) |field_index| {
                    const struct_reg = try self.genValue(indexed.target, self.consumeRegister());
                    const value_reg = try self.genValue(stmt.value, self.consumeRegister());
                    defer self.freeRegisterN(2);

                    const store_inst = Instruction{
                        .op = .STRUCT_STORE,
                        .a = struct_reg,
                        .b = @intCast(field_index),
                        .c = value_reg,
                    };
                    try self.chunk_instructions.append(self.allocator, store_inst);
                    return;
                }

                const value_reg = try self.genValue(stmt.value, self.consumeRegister());
                defer self.freeRegister();
                var store = Instruction{ .op = .STATIC_STORE, .a = value_reg };
                const slot_id = try self.static_store.getSlot(indexed.target.type_id, field_name);
                store.putBEx(slot_id);
                try self.chunk_instructions.append(self.allocator, store);
            },
            .array => {
                const array_reg = try self.genValue(indexed.target, self.consumeRegister());
                const index_reg = try self.genValue(indexed.index, self.consumeRegister());
                const value_reg = try self.genValue(stmt.value, self.consumeRegister());
                defer self.freeRegisterN(3);

                const store_inst = Instruction{
                    .op = .ARRAY_STORE,
                    .a = array_reg,
                    .b = index_reg,
                    .c = value_reg,
                };
                try self.chunk_instructions.append(self.allocator, store_inst);
            },
            else => unreachable,
        }
    }

    fn genArrayAppend(self: *Self, value: *const ir.Value, array_reg: u8) !void {
        const v_reg = try self.genValue(value, self.consumeRegister());
        defer self.freeRegister();

        const append_inst = Instruction{
            .op = .ARRAY_APPEND,
            .a = array_reg,
            .b = v_reg,
        };
        try self.chunk_instructions.append(self.allocator, append_inst);
        try self.recordHeapMap();
    }

    fn genArrayPop(self: *Self, array_reg: u8, target_reg: u8) !void {
        const pop_inst = Instruction{
            .op = .ARRAY_POP,
            .a = target_reg,
            .b = array_reg,
        };

        try self.chunk_instructions.append(self.allocator, pop_inst);
    }

    fn genArrayLen(self: *Self, array_reg: u8, target_reg: u8) !void {
        const len_inst = Instruction{
            .op = .ARRAY_LEN,
            .a = target_reg,
            .b = array_reg,
        };

        try self.chunk_instructions.append(self.allocator, len_inst);
    }

    fn genStructInit(self: *Self, value: *const ir.Value, target_reg: u8) !void {
        const @"struct" = value.data.struct_init;
        const desc_id = try self.getOrCreateStructDescriptor(value);

        var struct_init = Instruction{
            .op = .STRUCT_INIT,
            .a = target_reg,
        };
        struct_init.putBEx(desc_id);

        try self.chunk_instructions.append(self.allocator, struct_init);
        try self.recordHeapMap();
        self.register_types[target_reg] = value.type_id;

        for (@"struct".values, 0..) |field_v, i| {
            try self.genStructStore(target_reg, @intCast(i), field_v);
        }
    }

    fn genStructStore(self: *Self, struct_reg: u8, field_index: u8, value: *const ir.Value) !void {
        const struct_store = Instruction{
            .op = .STRUCT_STORE,
            .a = struct_reg,
            .b = field_index,
            .c = try self.genValue(value, self.consumeRegister()),
        };
        defer self.freeRegister();
        try self.chunk_instructions.append(self.allocator, struct_store);
    }

    fn genStructLoad(self: *Self, target_reg: u8, struct_reg: u8, field_index: u8) !void {
        const struct_load = Instruction{
            .op = .STRUCT_LOAD,
            .a = target_reg,
            .b = struct_reg,
            .c = field_index,
        };
        try self.chunk_instructions.append(self.allocator, struct_load);
    }

    fn genLoadConst(self: *Self, const_val: v.TypedValue, target_reg: u8) !void {
        const result = try self.const_id_by_value.getOrPut(const_val);
        if (!result.found_existing) {
            result.value_ptr.* = @intCast(self.chunk_constants.items.len);
            try self.chunk_constants.append(self.allocator, const_val.value);
            try self.chunk_constants_types.append(self.allocator, const_val.type);
        }

        var load_const = Instruction{
            .op = .LOAD_CONST,
            .a = target_reg,
        };

        load_const.putBEx(@intCast(result.value_ptr.*));

        try self.chunk_instructions.append(self.allocator, load_const);
    }

    fn genVoidValue(self: *Self, target_reg: u8) !u8 {
        try self.genLoadConst(.{ .value = .{ .null = {} }, .type = .null }, target_reg);
        self.register_types[target_reg] = self.type_table.getPrimitive(.void);
        return target_reg;
    }

    fn genTrueValue(self: *Self, target_reg: u8) !u8 {
        try self.genLoadConst(.{ .value = .{ .bool = true }, .type = .bool }, target_reg);
        self.register_types[target_reg] = self.type_table.getPrimitive(.boolean);
        return target_reg;
    }

    fn genLoadString(self: *Self, s: []const u8, target_reg: u8) !void {
        const result = try self.string_const_id_by_value.getOrPut(s);
        if (!result.found_existing) {
            result.value_ptr.* = @intCast(self.chunk_string_constants.items.len);
            try self.chunk_string_constants.append(self.allocator, s);
        }

        const string_id = result.value_ptr.*;

        var load_string = Instruction{
            .op = .LOAD_STRING,
            .a = target_reg,
        };

        load_string.putBEx(string_id);
        try self.chunk_instructions.append(self.allocator, load_string);
        try self.recordHeapMap();
    }

    fn genBinOp(self: *Self, bo: ir.BinaryOp, target_reg: u8) !void {
        const left_reg = try self.genValue(bo.left, self.consumeRegister());
        const right_reg = try self.genValue(bo.right, self.consumeRegister());
        defer self.freeRegisterN(2);

        const op = OpCode.fromIrBinaryOp(bo.kind);

        const left_is_int = self.type_table.getTypePtrById(bo.left.type_id).kind == .int;
        const right_is_int = self.type_table.getTypePtrById(bo.right.type_id).kind == .int;

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
                if (left_is_int) {
                    const left_to_float = Instruction{
                        .op = .I_TO_F,
                        .a = left_reg,
                    };
                    try self.chunk_instructions.append(self.allocator, left_to_float);
                }

                if (right_is_int) {
                    const right_to_float = Instruction{
                        .op = .I_TO_F,
                        .a = right_reg,
                    };
                    try self.chunk_instructions.append(self.allocator, right_to_float);
                }
            },
            .trunc_div => {
                if (!left_is_int) {
                    const left_to_int = Instruction{
                        .op = .F_TO_I,
                        .a = left_reg,
                    };
                    try self.chunk_instructions.append(self.allocator, left_to_int);
                }

                if (!right_is_int) {
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
        self.register_types[target_reg] = switch (bo.kind) {
            .i_add, .i_sub, .i_mult, .i_mod, .trunc_div => self.type_table.getPrimitive(.int),
            .i_cmp_eq, .i_cmp_neq, .i_cmp_lt, .i_cmp_le, .i_cmp_ge, .i_cmp_gt => self.type_table.getPrimitive(.boolean),
            .f_add, .f_sub, .f_mult, .f_div, .f_mod => self.type_table.getPrimitive(.float),
            .f_cmp_eq, .f_cmp_neq, .f_cmp_lt, .f_cmp_le, .f_cmp_ge, .f_cmp_gt => self.type_table.getPrimitive(.boolean),
            .b_and, .b_or, .b_cmp_eq, .b_cmp_neq => self.type_table.getPrimitive(.boolean),
        };
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
        self.register_types[target_reg] = switch (bo.kind) {
            .not => self.type_table.getPrimitive(.boolean),
            .i_neg => self.type_table.getPrimitive(.int),
            .f_neg => self.type_table.getPrimitive(.float),
        };
    }

    fn freeRegister(self: *Self) void {
        self.freeRegisterN(1);
    }

    fn freeRegisterN(self: *Self, n: usize) void {
        const new_free: usize = @max(0, self.next_free_register - n);
        for (new_free..self.next_free_register) |i| {
            self.register_types[i] = self.type_table.getPrimitive(.void);
        }
        self.next_free_register = @intCast(new_free);
    }

    fn consumeRegister(self: *Self) u8 {
        if (self.next_free_register == std.math.maxInt(u8)) {
            @panic("Register limit exceeded");
        }
        const id = self.next_free_register;
        self.next_free_register += 1;
        self.peak_register = @max(self.peak_register, self.next_free_register);
        return id;
    }

    fn computeHeapBitmap(self: *Self) u256 {
        var bitmap: u256 = 0;
        for (0..self.peak_register) |i| {
            const vt = v.ValueType.fromTypeId(self.type_table, self.register_types[i]);
            if (vt.isHeapType()) {
                bitmap |= @as(u256, 1) << @intCast(i);
            }
        }
        return bitmap;
    }

    fn recordHeapMap(self: *Self) !void {
        try self.recordHeapMapAt(@intCast(self.chunk_instructions.items.len - 1));
    }

    fn recordHeapMapAt(self: *Self, pc: u16) !void {
        try self.chunk_heap_maps.append(self.allocator, .{
            .pc = pc,
            .bitmap = self.computeHeapBitmap(),
        });
    }

    fn setStaticHeapBit(self: *Self, slot_id: u16) !void {
        const word_idx = slot_id >> 6;
        const bit_idx = slot_id & 63;
        while (self.static_heap_bitmap.items.len <= word_idx) {
            try self.static_heap_bitmap.append(self.allocator, 0);
        }
        self.static_heap_bitmap.items[word_idx] |= @as(u64, 1) << @intCast(bit_idx);
    }

    fn getOrCreateStructDescriptor(self: *Self, value: *const ir.Value) !u16 {
        if (self.type_id_to_struct_descriptor_idx.get(value.type_id)) |id| return id;

        const struct_data = value.data.struct_init;
        var bitmap: u256 = 0;
        for (struct_data.values, 0..) |field_v, i| {
            const vt = v.ValueType.fromTypeId(self.type_table, field_v.type_id);
            if (vt.isHeapType()) {
                bitmap |= @as(u256, 1) << @intCast(i);
            }
        }

        const id: u16 = @intCast(self.struct_descriptors.items.len);
        try self.struct_descriptors.append(self.allocator, .{
            .field_count = @intCast(struct_data.keys.len),
            .heap_bitmap = bitmap,
        });
        try self.type_id_to_struct_descriptor_idx.put(value.type_id, id);
        return id;
    }
};

pub const StructDescriptor = struct {
    field_count: u8,
    heap_bitmap: u256,
};

pub const HeapMap = struct {
    pc: u16,
    bitmap: u256,
};

pub const Program = struct {
    main_func_index: usize,
    functions: []const Function,
    static_count: u16,
    static_heap_bitmap: []const u64,
    struct_descriptors: []const StructDescriptor,
};

pub const HostFn = struct {
    func: *const fn (args: []v.Value) v.Value,
    num_args: u8,
};

pub const InlineFn = struct {
    gen: *const fn (target_reg: u8, arg_start_reg: u8, std.mem.Allocator) anyerror![]const Instruction,
    num_args: u8,
};

pub const FunctionKind = union(enum) {
    dusk: Chunk,
    host: HostFn,
    @"inline": InlineFn,
};

pub const Function = struct {
    uid: usize,
    name: []const u8,
    kind: FunctionKind,
};

pub const Chunk = struct {
    const Self = @This();

    num_registers: u8,
    instructions: []Instruction,
    constants: []v.Value,
    shadow_types: []v.ValueType,
    string_constants: []const []const u8,
    heap_maps: []const HeapMap,

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
                .string => unreachable,
                .array => std.debug.print("<array>", .{}),
                .@"struct" => std.debug.print("<struct>", .{}),
            }
            std.debug.print("\n", .{});
        }

        if (self.string_constants.len > 0) {
            std.debug.print("\n== string constants ({d}) ==\n", .{self.string_constants.len});
            for (self.string_constants, 0..) |s, i| {
                std.debug.print("{d:0>4}  \"{s}\"\n", .{ i, s });
            }
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
            .B_NOT, .I_NEG, .F_NEG => std.debug.print("R[{d}] R[{d}]", .{ inst.a, inst.b }),
            .CALL => std.debug.print("R[{d}] FN[{d}]", .{ inst.a, inst.bEx() }),
            .RETURN => std.debug.print("R[{d}]", .{inst.a}),
            .JUMP => std.debug.print("I[{d}]", .{inst.aEx()}),
            .JUMP_IF_FALSE => std.debug.print("R[{d}] I[{d}]", .{ inst.a, inst.bEx() }),
            .ARRAY_LOAD,
            .ARRAY_STORE,
            => std.debug.print("R[{d}] R[{d}] R[{d}]", .{ inst.a, inst.b, inst.c }),
            .ARRAY_APPEND,
            .ARRAY_LEN,
            .ARRAY_POP,
            => std.debug.print("R[{d}] R[{d}]", .{ inst.a, inst.b }),
            .ARRAY_INIT => std.debug.print("R[{d}] T[{d}] C[{d}]", .{ inst.a, inst.b, inst.c }),
            .STRUCT_INIT => std.debug.print("R[{d}] N[{d}]", .{ inst.a, inst.b }),
            .STRUCT_STORE => std.debug.print("R[{d}] F[{d}] R[{d}]", .{ inst.a, inst.b, inst.c }),
            .STRUCT_LOAD => std.debug.print("R[{d}] R[{d}] F[{d}]", .{ inst.a, inst.b, inst.c }),
            .STATIC_LOAD => std.debug.print("R[{d}] S[{d}]", .{ inst.a, inst.bEx() }),
            .STATIC_STORE => std.debug.print("R[{d}] S[{d}]", .{ inst.a, inst.bEx() }),
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

    ARRAY_INIT,
    ARRAY_LOAD,
    ARRAY_STORE,
    ARRAY_LEN,
    ARRAY_APPEND,
    ARRAY_POP,

    STRUCT_INIT,
    STRUCT_LOAD,
    STRUCT_STORE,

    STATIC_LOAD,
    STATIC_STORE,

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

    pub fn canTriggerGc(self: Self) bool {
        return switch (self) {
            .ARRAY_INIT, .ARRAY_APPEND, .STRUCT_INIT, .LOAD_STRING, .CALL => true,
            else => false,
        };
    }

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

const LoopContext = struct {
    loop_begin: usize,
    first_break_patch_idx: usize,
};

pub const StaticStoreTable = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    next_slot: u16 = 0,
    slots_by_type_id: std.AutoHashMap(sema.TypeId, std.StringHashMap(u16)),

    pub fn init(alloc: std.mem.Allocator) Self {
        return Self{
            .allocator = alloc,
            .slots_by_type_id = std.AutoHashMap(sema.TypeId, std.StringHashMap(u16)).init(alloc),
        };
    }

    pub fn registerStructFields(self: *Self, @"struct": ir.Struct) !void {
        if (@"struct".static_fields.len == 0) return;

        const slot = try self.slots_by_type_id.getOrPut(@"struct".type_id);
        if (!slot.found_existing) {
            slot.value_ptr.* = std.StringHashMap(u16).init(self.allocator);
        }

        for (@"struct".static_fields) |field| {
            try slot.value_ptr.put(field.identifier, self.next_slot);
            self.next_slot += 1;
        }
    }

    pub fn getSlot(self: *const Self, type_id: sema.TypeId, field_name: []const u8) !u16 {
        const type_slots = self.slots_by_type_id.get(type_id) orelse return error.UnableToFindSlot;
        return type_slots.get(field_name) orelse return error.UnableToFindSlot;
    }

    pub fn count(self: *const Self) u16 {
        return self.next_slot;
    }
};
