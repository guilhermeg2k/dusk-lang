const std = @import("std");
const ir = @import("ir.zig");
const v = @import("value.zig");
const tt = @import("type_table.zig");

pub const BytecodeGen = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    type_table: *tt.TypeTable,

    var_register_id_by_uid: std.AutoHashMap(usize, u8),

    register_types: [256]tt.TypeId = [_]tt.TypeId{0} ** 256,
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
    continue_patches: std.ArrayList(usize),
    inline_functions_by_uid: std.AutoHashMap(usize, InlineFn),
    fn_index_by_uid: std.AutoHashMap(usize, u16),

    static_store: StaticStoreTable,

    chunk_heap_maps: std.ArrayList(HeapMap),
    struct_descriptors: std.ArrayList(StructDescriptor),
    type_id_to_struct_descriptor_idx: std.AutoHashMap(tt.TypeId, u16),
    union_descriptors: std.ArrayList(UnionDescriptor),
    type_id_to_union_descriptor_idx: std.AutoHashMap(tt.TypeId, u16),
    union_tag_by_type_id: std.AutoHashMap(tt.TypeId, std.StringHashMap(u16)),
    static_heap_bitmap: std.ArrayList(u64) = .empty,
    global_uids: std.AutoHashMapUnmanaged(usize, void) = .empty,
    main_register_by_uid: std.AutoHashMapUnmanaged(usize, u8) = .empty,
    is_generating_main: bool = false,
    current_function_name: []const u8 = "",

    pub fn init(alloc: std.mem.Allocator, type_table: *tt.TypeTable) Self {
        return Self{
            .allocator = alloc,
            .type_table = type_table,
            .register_types = [_]tt.TypeId{type_table.getPrimitive(.void)} ** 256,
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
            .continue_patches = .empty,
            .inline_functions_by_uid = std.AutoHashMap(usize, InlineFn).init(alloc),
            .fn_index_by_uid = std.AutoHashMap(usize, u16).init(alloc),
            .static_store = StaticStoreTable.init(alloc),
            .chunk_heap_maps = .empty,
            .struct_descriptors = .empty,
            .type_id_to_struct_descriptor_idx = std.AutoHashMap(tt.TypeId, u16).init(alloc),
            .union_descriptors = .empty,
            .type_id_to_union_descriptor_idx = std.AutoHashMap(tt.TypeId, u16).init(alloc),
            .union_tag_by_type_id = std.AutoHashMap(tt.TypeId, std.StringHashMap(u16)).init(alloc),
        };
    }

    pub fn generate(self: *Self, program: *const ir.Program, builtins: []const Function) !Program {
        var funcs: std.ArrayList(Function) = .empty;

        for (builtins) |bf| {
            switch (bf.kind) {
                .@"inline" => |inl| try self.inline_functions_by_uid.put(bf.uid, inl),
                else => {},
            }
            try self.fn_index_by_uid.put(bf.uid, @intCast(funcs.items.len));
            try funcs.append(self.allocator, bf);
        }

        for (program.defined_types) |dt| {
            switch (dt.kind) {
                .@"struct" => |s| try self.registerStaticFields(s.type_id, s.static_fields),
                .@"enum" => |e| try self.registerStaticFields(e.type_id, e.static_fields),
                .@"union" => |u| try self.registerStaticFields(u.type_id, u.static_fields),
            }
        }

        var next_fn_index: u16 = @intCast(funcs.items.len);
        for (program.defined_types) |dt| {
            const type_funcs = switch (dt.kind) {
                .@"struct" => |s| s.funcs,
                .@"enum" => |e| e.funcs,
                .@"union" => |u| u.funcs,
            };
            for (type_funcs) |func| {
                try self.fn_index_by_uid.put(func.uid, next_fn_index);
                next_fn_index += 1;
            }
        }
        for (program.functions) |func| {
            try self.fn_index_by_uid.put(func.uid, next_fn_index);
            next_fn_index += 1;
        }

        for (program.global_uids) |uid| {
            try self.global_uids.put(self.allocator, uid, {});
        }
        const main_func = try self.genMainFunc(program);

        for (program.defined_types) |dt| {
            switch (dt.kind) {
                .@"struct" => |s| {
                    for (s.funcs) |func| try self.addFunction(&funcs, func);
                },
                .@"enum" => |e| {
                    for (e.funcs) |func| try self.addFunction(&funcs, func);
                },
                .@"union" => |u| {
                    for (u.funcs) |func| try self.addFunction(&funcs, func);
                },
            }
        }

        for (program.functions) |func| {
            try self.addFunction(&funcs, func);
        }

        try funcs.append(self.allocator, main_func);

        return Program{
            .main_func_index = funcs.items.len - 1,
            .functions = try funcs.toOwnedSlice(self.allocator),
            .static_count = self.static_store.count(),
            .static_heap_bitmap = try self.static_heap_bitmap.toOwnedSlice(self.allocator),
            .struct_descriptors = try self.struct_descriptors.toOwnedSlice(self.allocator),
            .union_descriptors = try self.union_descriptors.toOwnedSlice(self.allocator),
            .error_names = try self.copyErrorNames(program.error_names),
        };
    }

    fn copyErrorNames(self: *Self, error_names: []const []const u8) ![]const []const u8 {
        const names = try self.allocator.alloc([]const u8, error_names.len);
        for (error_names, 0..) |name, i| {
            names[i] = try self.allocator.dupe(u8, name);
        }
        return names;
    }

    fn genMainFunc(self: *Self, program: *const ir.Program) !Function {
        self.is_generating_main = true;
        defer self.is_generating_main = false;
        self.next_free_register = 0;
        self.peak_register = 0;
        self.var_register_id_by_uid.clearRetainingCapacity();
        @memset(&self.register_types, self.type_table.getPrimitive(.void));

        for (program.defined_types) |dt| {
            switch (dt.kind) {
                .@"struct" => |s| try self.emitStaticInit(s.type_id, s.static_fields),
                .@"enum" => |e| try self.emitStaticInit(e.type_id, e.static_fields),
                .@"union" => |u| try self.emitStaticInit(u.type_id, u.static_fields),
            }
        }

        return Function{
            .uid = 0,
            .name = "$main",
            .kind = .{ .dusk = try self.genFunctionChunk(.{
                .uid = 0,
                .identifier = "$main",
                .params = &.{},
                .body = program.instructions,
                .return_type = self.type_table.getPrimitive(.void),
            }) },
        };
    }

    fn addFunction(self: *Self, funcs: *std.ArrayList(Function), func: ir.Func) !void {
        try self.fn_index_by_uid.put(func.uid, @intCast(funcs.items.len));
        try funcs.append(self.allocator, try self.genFunction(func));
    }

    fn registerStaticFields(self: *Self, type_id: tt.TypeId, static_fields: []const ir.Field) !void {
        try self.static_store.registerFields(type_id, static_fields);

        for (static_fields) |field| {
            const slot_id = try self.static_store.getSlot(type_id, field.identifier);
            const vt = v.ValueType.fromTypeId(self.type_table, field.type_id);
            if (vt.isHeapType()) {
                try self.setStaticHeapBit(slot_id);
            }
        }
    }

    fn emitStaticInit(self: *Self, type_id: tt.TypeId, static_fields: []const ir.Field) !void {
        for (static_fields) |field| {
            if (field.default_value) |default| {
                const slot_id = try self.static_store.getSlot(type_id, field.identifier);
                const reg = try self.genValue(default, try self.consumeRegister());
                defer self.freeRegister();

                var store = Instruction{ .op = .STATIC_STORE, .a = reg };
                store.putBEx(slot_id);
                try self.chunk_instructions.append(self.allocator, store);
            }
        }
    }

    fn genFunction(self: *Self, func: ir.Func) !Function {
        self.current_function_name = func.identifier;
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

        // chunk.disasamble();

        return chunk;
    }

    fn genInstructionBlock(self: *Self, block: []const ir.Instruction) anyerror!void {
        for (block) |instruction| {
            switch (instruction) {
                .store_var => |store_var| {
                    try self.genStoreVar(store_var);
                },
                .expression_stmt => |stmt| {
                    const saved_next_free = self.next_free_register;
                    //note: consuming a register
                    _ = try self.genValue(stmt.value, try self.consumeRegister());
                    self.freeRegisterN(self.next_free_register - saved_next_free);
                },
                .branch_if => |stmt| {
                    try self.genBranchIf(stmt);
                },
                .scope => |instructions| {
                    const saved_next_free = self.next_free_register;
                    try self.genInstructionBlock(instructions);
                    self.freeRegisterN(self.next_free_register - saved_next_free);
                },
                .loop => |stmt| {
                    try self.genLoop(stmt);
                },
                .return_stmt => |r_stmt| {
                    try self.genReturn(r_stmt);
                },
                .return_error => |r_err| {
                    const reg = try self.genValue(r_err.value, try self.consumeRegister());
                    try self.chunk_instructions.append(self.allocator, .{ .op = .RETURN_ERR, .a = reg });
                },
                .update_indexed => |stmt| {
                    _ = try self.genUpdateArrayValue(stmt);
                },
                .unwrap_null_box => |stmt| {
                    _ = try self.genUnwrapNullBox(stmt, try self.consumeRegister());
                },
                .update_null_box => |stmt| {
                    try self.genUpadateNullBox(stmt);
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
            self.register_types[idx] = store_var.type_id;
            return;
        }
        if (self.main_register_by_uid.get(store_var.uid)) |main_reg| {
            const value_reg = try self.genValue(store_var.value, try self.consumeRegister());
            defer self.freeRegister();
            var store = Instruction{ .op = .GLOBAL_STORE, .a = value_reg };
            store.putBEx(main_reg);
            try self.chunk_instructions.append(self.allocator, store);
            return;
        }
        const reg_idx = try self.genValue(store_var.value, try self.consumeRegister());
        self.register_types[reg_idx] = store_var.type_id;
        try self.bindVariable(store_var.uid, reg_idx);
    }

    fn bindVariable(self: *Self, uid: usize, reg: u8) !void {
        try self.var_register_id_by_uid.put(uid, reg);
        if (self.is_generating_main and self.global_uids.contains(uid)) {
            try self.main_register_by_uid.put(self.allocator, uid, reg);
        }
    }

    fn genGlobalLoad(self: *Self, uid: usize, type_id: tt.TypeId, target_reg: u8) !void {
        const main_reg = self.main_register_by_uid.get(uid) orelse return BytecodeError.UndefinedVariable;
        var load = Instruction{ .op = .GLOBAL_LOAD, .a = target_reg };
        load.putBEx(main_reg);
        try self.chunk_instructions.append(self.allocator, load);
        self.register_types[target_reg] = type_id;
    }

    fn genBranchIf(self: *Self, ifStmt: ir.BranchIf) !void {
        const saved_next_free = self.next_free_register;

        const condition_reg = try self.genValue(ifStmt.condition, try self.consumeRegister());

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

        try self.pushLoopCtx();

        const condition_reg = if (loopStmt.condition) |cond|
            try self.genValue(cond, try self.consumeRegister())
        else
            try self.genTrueValue(try self.consumeRegister());

        const jump_pass_loop = Instruction{
            .op = .JUMP_IF_FALSE,
            .a = condition_reg,
        };

        try self.chunk_instructions.append(self.allocator, jump_pass_loop);
        const jump_if_false_idx = self.chunk_instructions.items.len - 1;

        try self.genInstructionBlock(loopStmt.do_block);

        const advancement_begin = self.chunk_instructions.items.len;
        const context = self.loop_stack.items[self.loop_stack.items.len - 1];
        for (self.continue_patches.items[context.first_continue_patch_index..]) |patch_index| {
            self.chunk_instructions.items[patch_index].putAEx(@intCast(advancement_begin));
        }
        self.continue_patches.shrinkRetainingCapacity(context.first_continue_patch_index);
        try self.genInstructionBlock(loopStmt.advancement);

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
        try self.continue_patches.append(self.allocator, self.chunk_instructions.items.len);
        try self.chunk_instructions.append(self.allocator, .{ .op = .JUMP });
    }

    fn pushLoopCtx(self: *Self) !void {
        try self.loop_stack.append(self.allocator, LoopContext{
            .first_break_patch_idx = self.break_patches.items.len,
            .first_continue_patch_index = self.continue_patches.items.len,
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
        if (target_reg + 1 != self.next_free_register) {
            const call_reg = try self.consumeRegister();
            defer self.freeRegister();
            try self.genFnCall(fc, value, call_reg);
            try self.chunk_instructions.append(self.allocator, .{ .op = .LOAD, .a = target_reg, .b = call_reg });
            self.register_types[target_reg] = value.type_id;
            return;
        }

        for (fc.args) |arg| {
            _ = try self.genValue(arg, try self.consumeRegister());
        }

        if (self.inline_functions_by_uid.get(fc.fn_uid)) |inl| {
            try self.genInlineCall(inl, fc, value, target_reg);
            return;
        }

        //note: temp hacky for echo
        if (fc.fn_uid == 0) {
            const arg_type_id = fc.args[0].type_id;
            const arg_value_type = v.ValueType.fromTypeId(self.type_table, arg_type_id);
            const type_reg = try self.consumeRegister();
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

        fn_call.putBEx(self.fn_index_by_uid.get(fc.fn_uid) orelse unreachable);

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
            try self.genValue(val, try self.consumeRegister())
        else
            try self.genVoidValue(try self.consumeRegister());

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
                const symbol_reg = self.var_register_id_by_uid.get(id.uid) orelse {
                    try self.genGlobalLoad(id.uid, value.type_id, target_reg);
                    return target_reg;
                };

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

            .union_init => {
                try self.genUnionInit(value, target_reg);
            },

            .null_box_init => {
                try self.genNullBoxInit(value, target_reg);
            },

            .union_tag => |un| {
                const union_reg = try self.genValue(un, try self.consumeRegister());
                defer self.freeRegister();
                try self.chunk_instructions.append(self.allocator, .{ .op = .UNION_TAG, .a = target_reg, .b = union_reg });
                self.register_types[target_reg] = value.type_id;
            },

            .union_payload => |un| {
                const union_reg = try self.genValue(un, try self.consumeRegister());
                defer self.freeRegister();
                try self.chunk_instructions.append(self.allocator, .{ .op = .UNION_PAYLOAD, .a = target_reg, .b = union_reg });
                self.register_types[target_reg] = value.type_id;
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

            .try_call => |tc| {
                try self.genFailingCall(tc.call, value, target_reg, .{ .try_call = tc });
            },

            .catch_call => |cc| {
                try self.genFailingCall(cc.call, value, target_reg, .{ .catch_call = cc });
            },
            .block_value => |block| {
                const saved_variables = try self.var_register_id_by_uid.clone();
                defer {
                    self.var_register_id_by_uid.deinit();
                    self.var_register_id_by_uid = saved_variables;
                }
                const saved_next_free = self.next_free_register;
                const result_register = try self.consumeRegister();
                try self.genLoadConst(.{ .value = v.NULL_VALUE, .type = .null }, result_register);
                self.register_types[result_register] = value.type_id;
                try self.var_register_id_by_uid.put(block.result_uid, result_register);
                try self.genInstructionBlock(block.instructions);
                try self.chunk_instructions.append(self.allocator, .{ .op = .LOAD, .a = target_reg, .b = result_register });
                self.register_types[target_reg] = value.type_id;
                self.freeRegisterN(self.next_free_register - saved_next_free);
            },
            .int_to_float => |operand| try self.genIntToFloat(operand, value.type_id, target_reg),
            .array_length => |array| {
                const saved_next_free = self.next_free_register;
                const array_register = try self.genValue(array, try self.consumeRegister());
                try self.chunk_instructions.append(self.allocator, .{ .op = .ARRAY_LEN, .a = target_reg, .b = array_register });
                self.register_types[target_reg] = value.type_id;
                self.freeRegisterN(self.next_free_register - saved_next_free);
            },
        }

        return target_reg;
    }

    fn genIntToFloat(self: *Self, operand: *const ir.Value, result_type: tt.TypeId, target_register: u8) !void {
        if (!self.type_table.getTypePtrById(operand.type_id).is_nullable) {
            _ = try self.genValue(operand, target_register);
            try self.chunk_instructions.append(self.allocator, .{ .op = .I_TO_F, .a = target_register });
            self.register_types[target_register] = result_type;
            return;
        }
        const saved_next_free = self.next_free_register;
        const operand_register = try self.genValue(operand, try self.consumeRegister());
        const condition_register = try self.consumeRegister();
        try self.genLoadConst(.{ .value = v.NULL_VALUE, .type = .null }, target_register);
        self.register_types[target_register] = result_type;
        try self.chunk_instructions.append(self.allocator, .{ .op = .NULL_BOX_NNULL, .a = condition_register, .b = operand_register });
        self.register_types[condition_register] = self.type_table.getPrimitive(.boolean);
        const null_jump_index = self.chunk_instructions.items.len;
        try self.chunk_instructions.append(self.allocator, .{ .op = .JUMP_IF_FALSE, .a = condition_register });
        const number_register = try self.consumeRegister();
        try self.chunk_instructions.append(self.allocator, .{ .op = .NULL_BOX_UNWRAP, .a = number_register, .b = operand_register });
        try self.chunk_instructions.append(self.allocator, .{ .op = .I_TO_F, .a = number_register });
        self.register_types[number_register] = self.type_table.getPrimitive(.float);
        try self.chunk_instructions.append(self.allocator, .{
            .op = .NULL_BOX_INIT,
            .a = target_register,
            .b = @intFromEnum(v.ValueType.float64),
            .c = number_register,
        });
        try self.recordHeapMap();
        self.chunk_instructions.items[null_jump_index].putBEx(@intCast(self.chunk_instructions.items.len));
        self.freeRegisterN(self.next_free_register - saved_next_free);
    }

    fn genFailingCall(self: *Self, call: *const ir.Value, value: *const ir.Value, target_reg: u8, handler: FailingCallHandler) !void {
        const error_union = self.type_table.getTypePtrById(call.type_id).kind.error_union;
        const call_reg = try self.consumeRegister();
        defer self.freeRegister();

        try self.genFnCall(call.data.fn_call, call, call_reg);
        self.register_types[call_reg] = error_union.error_set;

        std.debug.assert(self.chunk_instructions.items[self.chunk_instructions.items.len - 1].op == .CALL);
        try self.chunk_instructions.append(self.allocator, .{ .op = .JUMP });
        const jump_ok_idx = self.chunk_instructions.items.len - 1;

        switch (handler) {
            .try_call => |tc| {
                const op: OpCode = if (tc.is_top_level) .PANIC_ERR else .RETURN_ERR;
                try self.chunk_instructions.append(self.allocator, .{ .op = op, .a = call_reg });
            },
            .catch_call => |cc| switch (cc.fallback) {
                .value => |fallback| {
                    _ = try self.genValue(fallback, call_reg);
                },
                .block => |block| {
                    const saved_next_free = self.next_free_register;
                    try self.bindVariable(block.capture_uid, call_reg);
                    try self.genInstructionBlock(block.instructions);
                    self.freeRegisterN(self.next_free_register - saved_next_free);
                    _ = try self.genVoidValue(call_reg);
                },
            },
        }

        self.chunk_instructions.items[jump_ok_idx].putAEx(@intCast(self.chunk_instructions.items.len));
        try self.chunk_instructions.append(self.allocator, .{ .op = .LOAD, .a = target_reg, .b = call_reg });
        self.register_types[target_reg] = value.type_id;
    }

    fn genIndexedLoad(self: *Self, idx: ir.IndexedValue, value: *const ir.Value, target_reg: u8) !void {
        const target_type = self.type_table.getTypePtrById(idx.target.type_id);

        // sema only allows a nullable target through `?.`
        if (target_type.is_nullable) {
            const is_instance_member = switch (target_type.kind) {
                .@"struct" => |s| s.field_index_by_name.contains(idx.index.data.i_string),
                .@"union" => |u| u.variants.contains(idx.index.data.i_string),
                else => false,
            };
            if (is_instance_member) {
                return self.genNullableIndexedLoad(idx, value, target_reg);
            }
        }

        switch (target_type.kind) {
            .@"struct" => |@"struct"| {
                const field_name = idx.index.data.i_string;
                if (@"struct".field_index_by_name.get(field_name)) |field_index| {
                    const struct_reg = try self.genValue(idx.target, try self.consumeRegister());
                    defer self.freeRegister();

                    try self.genStructLoad(target_reg, struct_reg, @intCast(field_index));
                    self.register_types[target_reg] = value.type_id;
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
            .@"enum" => {
                const field_name = idx.index.data.i_string;

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
                const array_reg = try self.genValue(idx.target, try self.consumeRegister());
                defer self.freeRegister();
                try self.genArrayLoad(target_reg, array_reg, idx.index);
                self.register_types[target_reg] = value.type_id;
            },
            .@"union" => |@"union"| {
                const field_name = idx.index.data.i_string;

                if (!@"union".variants.contains(field_name)) {
                    var load_static = Instruction{
                        .op = .STATIC_LOAD,
                        .a = target_reg,
                    };

                    const slot_id = try self.static_store.getSlot(idx.target.type_id, field_name);
                    load_static.putBEx(slot_id);

                    try self.chunk_instructions.append(self.allocator, load_static);
                    self.register_types[target_reg] = value.type_id;
                    return;
                }

                const union_reg = try self.genValue(idx.target, try self.consumeRegister());
                defer self.freeRegister();

                try self.genUnionLoad(target_reg, union_reg, idx.target.type_id, field_name);
                self.register_types[target_reg] = value.type_id;
            },
            else => unreachable,
        }
    }

    // Optional chaining (`target?.member`): the result is null when the target is null.
    //   cond   = target != null
    //   result = null
    //   JUMP_IF_FALSE cond -> end
    //   result = <member load>, boxed when the member is a non-nullable primitive
    // end:
    fn genNullableIndexedLoad(self: *Self, idx: ir.IndexedValue, value: *const ir.Value, target_reg: u8) !void {
        const target_type = self.type_table.getTypePtrById(idx.target.type_id);
        const member_name = idx.index.data.i_string;

        const obj_reg = try self.genValue(idx.target, try self.consumeRegister());
        const cond_reg = try self.consumeRegister();
        defer self.freeRegisterN(2);

        try self.chunk_instructions.append(self.allocator, .{
            .op = .NULL_BOX_NNULL,
            .a = cond_reg,
            .b = obj_reg,
        });
        self.register_types[cond_reg] = self.type_table.getPrimitive(.boolean);

        try self.genLoadConst(.{ .value = v.NULL_VALUE, .type = .null }, target_reg);
        self.register_types[target_reg] = value.type_id;

        try self.chunk_instructions.append(self.allocator, .{ .op = .JUMP_IF_FALSE, .a = cond_reg });
        const jump_idx = self.chunk_instructions.items.len - 1;

        switch (target_type.kind) {
            .@"struct" => |@"struct"| {
                const field_index = @"struct".field_index_by_name.get(member_name) orelse unreachable;
                const field_type_id = (@"struct".fields.get(member_name) orelse unreachable).type_id;
                const field_kind = v.ValueType.fromTypeId(self.type_table, field_type_id);

                if (field_kind.isHeapType()) {
                    try self.genStructLoad(target_reg, obj_reg, @intCast(field_index));
                } else {
                    const raw_reg = try self.consumeRegister();
                    defer self.freeRegister();

                    try self.genStructLoad(raw_reg, obj_reg, @intCast(field_index));
                    self.register_types[raw_reg] = field_type_id;

                    try self.chunk_instructions.append(self.allocator, .{
                        .op = .NULL_BOX_INIT,
                        .a = target_reg,
                        .b = @intFromEnum(field_kind),
                        .c = raw_reg,
                    });
                    try self.recordHeapMap();
                }
            },
            // UNION_LOAD already boxes primitive variants
            .@"union" => try self.genUnionLoad(target_reg, obj_reg, idx.target.type_id, member_name),
            else => unreachable,
        }

        self.chunk_instructions.items[jump_idx].putBEx(@intCast(self.chunk_instructions.items.len));
        self.register_types[target_reg] = value.type_id;
    }

    fn genUnionLoad(self: *Self, target_reg: u8, union_reg: u8, union_type_id: tt.TypeId, variant_name: []const u8) !void {
        const tag_index = try self.unionTagIndex(union_type_id, variant_name);

        try self.chunk_instructions.append(self.allocator, .{
            .op = .UNION_LOAD,
            .a = target_reg,
            .b = union_reg,
            .c = @intCast(tag_index),
        });
        try self.recordHeapMap();
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
        const index_reg = try self.genValue(index_value, try self.consumeRegister());
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
        const index_reg = try self.genValue(index_value, try self.consumeRegister());
        defer self.freeRegister();

        const value_reg = try self.genValue(value, try self.consumeRegister());
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
                    const struct_reg = try self.genValue(indexed.target, try self.consumeRegister());
                    const value_reg = try self.genValue(stmt.value, try self.consumeRegister());
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

                const value_reg = try self.genValue(stmt.value, try self.consumeRegister());
                defer self.freeRegister();
                var store = Instruction{ .op = .STATIC_STORE, .a = value_reg };
                const slot_id = try self.static_store.getSlot(indexed.target.type_id, field_name);
                store.putBEx(slot_id);
                try self.chunk_instructions.append(self.allocator, store);
            },
            .@"enum", .@"union" => {
                const field_name = indexed.index.data.i_string;

                const value_reg = try self.genValue(stmt.value, try self.consumeRegister());
                defer self.freeRegister();
                var store = Instruction{ .op = .STATIC_STORE, .a = value_reg };
                const slot_id = try self.static_store.getSlot(indexed.target.type_id, field_name);
                store.putBEx(slot_id);
                try self.chunk_instructions.append(self.allocator, store);
            },
            .array => {
                const array_reg = try self.genValue(indexed.target, try self.consumeRegister());
                const index_reg = try self.genValue(indexed.index, try self.consumeRegister());
                const value_reg = try self.genValue(stmt.value, try self.consumeRegister());
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
        const v_reg = try self.genValue(value, try self.consumeRegister());
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
            .c = try self.genValue(value, try self.consumeRegister()),
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

    fn genUnwrapNullBox(self: *Self, stmt: ir.UnwrapNullBox, target_reg: u8) !u8 {
        const box_reg = try self.genValue(stmt.null_box, try self.consumeRegister());
        defer self.freeRegisterN(1);

        const unwrap_isnt = Instruction{
            .op = .NULL_BOX_UNWRAP,
            .a = target_reg,
            .b = box_reg,
        };

        try self.chunk_instructions.append(self.allocator, unwrap_isnt);
        try self.bindVariable(stmt.unwrapped.uid, target_reg);
        self.register_types[target_reg] = stmt.unwrapped.type_id;

        return target_reg;
    }

    fn genUpadateNullBox(self: *Self, stmt: ir.UpdateNullBox) !void {
        if (self.var_register_id_by_uid.get(stmt.uid) == null) return self.genGlobalNullBoxUpdate(stmt);
        const box_reg = self.var_register_id_by_uid.get(stmt.uid).?;

        // b = 0 stores null, otherwise b = inner value kind + 1
        var store_inst = Instruction{
            .op = .NULL_BOX_STORE,
            .a = box_reg,
            .b = 0,
        };

        if (stmt.value) |value| {
            const value_reg = try self.genValue(value, try self.consumeRegister());
            defer self.freeRegister();
            store_inst.b = @intFromEnum(v.ValueType.fromTypeId(self.type_table, value.type_id)) + 1;
            store_inst.c = value_reg;
        }

        try self.chunk_instructions.append(self.allocator, store_inst);
        try self.recordHeapMap();
    }

    fn genGlobalNullBoxUpdate(self: *Self, stmt: ir.UpdateNullBox) !void {
        const box_reg = try self.consumeRegister();
        defer self.freeRegister();
        try self.genGlobalLoad(stmt.uid, stmt.type_id, box_reg);
        try self.var_register_id_by_uid.put(stmt.uid, box_reg);
        defer _ = self.var_register_id_by_uid.remove(stmt.uid);
        try self.genUpadateNullBox(stmt);
        var store = Instruction{ .op = .GLOBAL_STORE, .a = box_reg };
        store.putBEx(self.main_register_by_uid.get(stmt.uid).?);
        try self.chunk_instructions.append(self.allocator, store);
    }

    fn genVoidValue(self: *Self, target_reg: u8) !u8 {
        try self.genLoadConst(.{ .value = v.NULL_VALUE, .type = .null }, target_reg);
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
            const owned = try self.allocator.dupe(u8, s);
            result.key_ptr.* = owned;
            result.value_ptr.* = @intCast(self.chunk_string_constants.items.len);
            try self.chunk_string_constants.append(self.allocator, owned);
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
        const left_reg = try self.genValue(bo.left, try self.consumeRegister());
        const right_reg = try self.genValue(bo.right, try self.consumeRegister());
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

    fn genUnaryOp(self: *Self, uo: ir.UnaryOp, target_reg: u8) !void {
        const aux_reg = try self.genValue(uo.right, try self.consumeRegister());
        defer self.freeRegister();

        const op = OpCode.fromUnaryOp(uo.kind);
        const op_inst = Instruction{
            .op = op,
            .a = target_reg,
            .b = aux_reg,
        };

        try self.chunk_instructions.append(self.allocator, op_inst);

        self.register_types[target_reg] = switch (uo.kind) {
            .not => self.type_table.getPrimitive(.boolean),
            .box_not_null => self.type_table.getPrimitive(.boolean),
            .i_neg => self.type_table.getPrimitive(.int),
            .f_neg => self.type_table.getPrimitive(.float),
        };
    }

    fn tooManyRegisters(self: *Self) BytecodeError {
        if (@import("builtin").is_test) return BytecodeError.TooManyRegisters;
        const hint = "uses more than 255 registers at once (variables and temporaries); split it into smaller functions";
        if (self.is_generating_main) {
            std.debug.print("error: the top-level code {s}\n", .{hint});
        } else {
            std.debug.print("error: function '{s}' {s}\n", .{ self.current_function_name, hint });
        }
        return BytecodeError.TooManyRegisters;
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

    fn consumeRegister(self: *Self) !u8 {
        if (self.next_free_register == std.math.maxInt(u8)) return self.tooManyRegisters();
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
        const struct_def = self.type_table.getTypePtrById(value.type_id).kind.@"struct";
        var bitmap: u256 = 0;
        // use the declared field types, the init values may be null or not yet boxed
        for (struct_data.keys, 0..) |key, i| {
            const field = struct_def.fields.get(key) orelse unreachable;
            const vt = v.ValueType.fromTypeId(self.type_table, field.type_id);
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

    fn genUnionInit(self: *Self, value: *const ir.Value, target_reg: u8) !void {
        const union_init = value.data.union_init;
        const desc_id = try self.getOrCreateUnionDescriptor(value.type_id);

        var init_inst = Instruction{ .op = .UNION_INIT, .a = target_reg };
        init_inst.putBEx(desc_id);
        try self.chunk_instructions.append(self.allocator, init_inst);
        try self.recordHeapMap();
        self.register_types[target_reg] = value.type_id;

        if (union_init.tag) |tag_name| {
            if (union_init.value) |payload| {
                const tag_index = try self.unionTagIndex(value.type_id, tag_name);
                const value_reg = try self.genValue(payload, try self.consumeRegister());
                defer self.freeRegister();

                try self.chunk_instructions.append(self.allocator, .{
                    .op = .UNION_STORE,
                    .a = target_reg,
                    .b = @intCast(tag_index),
                    .c = value_reg,
                });
            }
        }
    }

    fn getOrCreateUnionDescriptor(self: *Self, type_id: tt.TypeId) !u16 {
        if (self.type_id_to_union_descriptor_idx.get(type_id)) |id| return id;

        const union_def = self.type_table.getTypePtrById(type_id).kind.@"union";

        var tag_map = std.StringHashMap(u16).init(self.allocator);
        var bitmap: u256 = 0;
        var box_bitmap: u256 = 0;
        const variant_kinds = try self.allocator.alloc(v.ValueType, union_def.variants.count());

        for (union_def.variants_in_order, 0..) |variant_name, i| {
            const tag: u16 = @intCast(i);
            try tag_map.put(variant_name, tag);
            const vt = v.ValueType.fromTypeId(self.type_table, union_def.variants.get(variant_name).?);
            variant_kinds[tag] = vt;
            if (vt.isHeapType()) {
                bitmap |= @as(u256, 1) << @intCast(tag);
            } else {
                // reading a primitive variant yields ?T, which needs a null box
                box_bitmap |= @as(u256, 1) << @intCast(tag);
            }
        }

        const id: u16 = @intCast(self.union_descriptors.items.len);
        try self.union_descriptors.append(self.allocator, .{
            .heap_bitmap = bitmap,
            .box_bitmap = box_bitmap,
            .variant_kinds = variant_kinds,
        });
        try self.type_id_to_union_descriptor_idx.put(type_id, id);
        try self.union_tag_by_type_id.put(type_id, tag_map);
        return id;
    }

    fn unionTagIndex(self: *Self, type_id: tt.TypeId, name: []const u8) !u16 {
        _ = try self.getOrCreateUnionDescriptor(type_id);
        const tag_map = self.union_tag_by_type_id.get(type_id).?;
        return tag_map.get(name) orelse error.UnableToFindSlot;
    }

    fn genNullBoxInit(self: *Self, value: *const ir.Value, target_reg: u8) !void {
        const null_box_init = value.data.null_box_init;

        const val = null_box_init.value orelse {
            try self.genLoadConst(.{ .value = v.NULL_VALUE, .type = .null }, target_reg);
            self.register_types[target_reg] = try self.type_table.getOrAddNullable(value.type_id);
            return;
        };

        const value_reg = try self.genValue(val, try self.consumeRegister());
        defer self.freeRegister();

        try self.chunk_instructions.append(self.allocator, .{
            .op = .NULL_BOX_INIT,
            .a = target_reg,
            .b = @intFromEnum(v.ValueType.fromTypeId(self.type_table, val.type_id)),
            .c = value_reg,
        });
        try self.recordHeapMap();

        // the ir value carries the inner type, but the register holds the box
        self.register_types[target_reg] = try self.type_table.getOrAddNullable(value.type_id);
    }
};

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

pub const Program = struct {
    main_func_index: usize,
    functions: []const Function,
    static_count: u16,
    static_heap_bitmap: []const u64,
    struct_descriptors: []const StructDescriptor,
    union_descriptors: []const UnionDescriptor,
    error_names: []const []const u8,
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
                .array => std.debug.print("<array>", .{}),
                .@"struct" => std.debug.print("<struct>", .{}),
                .@"union" => std.debug.print("<union>", .{}),
                .nullable => std.debug.print("<nullable:", .{}),
                .string => unreachable,
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
            .RETURN, .RETURN_ERR, .PANIC_ERR => std.debug.print("R[{d}]", .{inst.a}),
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
            .UNION_INIT => std.debug.print("R[{d}] D[{d}]", .{ inst.a, inst.bEx() }),
            .UNION_LOAD => std.debug.print("R[{d}] R[{d}] T[{d}]", .{ inst.a, inst.b, inst.c }),
            .UNION_STORE => std.debug.print("R[{d}] T[{d}] R[{d}]", .{ inst.a, inst.b, inst.c }),
            .UNION_TAG, .UNION_PAYLOAD => std.debug.print("R[{d}] R[{d}]", .{ inst.a, inst.b }),
            .STATIC_LOAD => std.debug.print("R[{d}] S[{d}]", .{ inst.a, inst.bEx() }),
            .STATIC_STORE => std.debug.print("R[{d}] S[{d}]", .{ inst.a, inst.bEx() }),
            .GLOBAL_LOAD, .GLOBAL_STORE => std.debug.print("R[{d}] G[{d}]", .{ inst.a, inst.bEx() }),
            .NULL_BOX_INIT,
            .NULL_BOX_STORE,
            => std.debug.print("R[{d}] B[{d}] R[{d}]", .{ inst.a, inst.b, inst.c }),
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

    ARRAY_INIT,
    ARRAY_LOAD,
    ARRAY_STORE,
    ARRAY_LEN,
    ARRAY_APPEND,
    ARRAY_POP,

    STRUCT_INIT,
    STRUCT_LOAD,
    STRUCT_STORE,

    UNION_INIT,
    UNION_LOAD,
    UNION_STORE,
    UNION_TAG,
    UNION_PAYLOAD,

    STATIC_LOAD,
    STATIC_STORE,
    GLOBAL_LOAD,
    GLOBAL_STORE,

    NULL_BOX_INIT,
    NULL_BOX_STORE,
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
    RETURN_ERR,
    PANIC_ERR,

    JUMP,
    JUMP_IF_FALSE,

    pub fn canTriggerGc(self: Self) bool {
        return switch (self) {
            .ARRAY_INIT,
            .ARRAY_APPEND,
            .STRUCT_INIT,
            .UNION_INIT,
            .UNION_LOAD,
            .NULL_BOX_INIT,
            .NULL_BOX_STORE,
            .LOAD_STRING,
            .CALL,
            => true,
            else => false,
        };
    }

    pub fn fromUnaryOp(op: ir.UnaryOpKind) Self {
        return switch (op) {
            .box_not_null => Self.NULL_BOX_NNULL,
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

const BytecodeError = error{ OutOfMemory, UndefinedVariable, TooManyRegisters };

const FailingCallHandler = union(enum) {
    try_call: ir.TryCall,
    catch_call: ir.CatchCall,
};

const LoopContext = struct {
    first_break_patch_idx: usize,
    first_continue_patch_index: usize,
};

pub const StaticStoreTable = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    next_slot: u16 = 0,
    slots_by_type_id: std.AutoHashMap(tt.TypeId, std.StringHashMap(u16)),

    pub fn init(alloc: std.mem.Allocator) Self {
        return Self{
            .allocator = alloc,
            .slots_by_type_id = std.AutoHashMap(tt.TypeId, std.StringHashMap(u16)).init(alloc),
        };
    }

    pub fn registerFields(self: *Self, type_id: tt.TypeId, static_fields: []const ir.Field) !void {
        if (static_fields.len == 0) return;

        const slot = try self.slots_by_type_id.getOrPut(type_id);
        if (!slot.found_existing) {
            slot.value_ptr.* = std.StringHashMap(u16).init(self.allocator);
        }

        for (static_fields) |field| {
            try slot.value_ptr.put(field.identifier, self.next_slot);
            self.next_slot += 1;
        }
    }

    pub fn getSlot(self: *const Self, type_id: tt.TypeId, field_name: []const u8) !u16 {
        const type_slots = self.slots_by_type_id.get(type_id) orelse return error.UnableToFindSlot;
        return type_slots.get(field_name) orelse return error.UnableToFindSlot;
    }

    pub fn count(self: *const Self) u16 {
        return self.next_slot;
    }
};
