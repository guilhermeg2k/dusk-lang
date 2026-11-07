const std = @import("std");
const ir = @import("ir.zig");
const v = @import("value.zig");
const tt = @import("type_table.zig");
const VM = @import("vm.zig").VM;
const natives = @import("natives.zig");

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

    string_texts: std.ArrayList([]const u8),
    chunk_build_recipes: std.ArrayList([]const v.ValueType) = .empty,
    chunk_jump_tables: std.ArrayList(JumpTable) = .empty,
    string_id_by_text: std.StringHashMap(u16),

    chunk_instructions: std.ArrayList(Instruction),
    loop_stack: std.ArrayList(LoopContext),
    break_patches: std.ArrayList(usize),
    continue_patches: std.ArrayList(usize),
    inline_functions_by_uid: std.AutoHashMap(usize, InlineFn),
    native_params_by_uid: std.AutoHashMap(usize, []const tt.TypedIdentifier),
    fn_index_by_uid: std.AutoHashMap(usize, u16),

    chunk_heap_maps: std.ArrayList(HeapMap),
    struct_descriptors: std.ArrayList(StructDescriptor),
    type_id_to_struct_descriptor_idx: std.AutoHashMap(tt.TypeId, u16),
    union_descriptors: std.ArrayList(UnionDescriptor),
    type_id_to_union_descriptor_idx: std.AutoHashMap(tt.TypeId, u16),
    union_tag_by_type_id: std.AutoHashMap(tt.TypeId, std.StringHashMap(u16)),
    current_function_name: []const u8 = "",
    extern_count: usize = 0,

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
            .string_texts = .empty,
            .string_id_by_text = std.StringHashMap(u16).init(alloc),
            .chunk_instructions = .empty,
            .loop_stack = .empty,
            .break_patches = .empty,
            .continue_patches = .empty,
            .inline_functions_by_uid = std.AutoHashMap(usize, InlineFn).init(alloc),
            .native_params_by_uid = std.AutoHashMap(usize, []const tt.TypedIdentifier).init(alloc),
            .fn_index_by_uid = std.AutoHashMap(usize, u16).init(alloc),
            .chunk_heap_maps = .empty,
            .struct_descriptors = .empty,
            .type_id_to_struct_descriptor_idx = std.AutoHashMap(tt.TypeId, u16).init(alloc),
            .union_descriptors = .empty,
            .type_id_to_union_descriptor_idx = std.AutoHashMap(tt.TypeId, u16).init(alloc),
            .union_tag_by_type_id = std.AutoHashMap(tt.TypeId, std.StringHashMap(u16)).init(alloc),
        };
    }

    pub fn generate(self: *Self, program: *const ir.Program) !Program {
        var funcs: std.ArrayList(Function) = .empty;
        self.extern_count = program.externs.len;

        for (program.externs) |ext| {
            try self.fn_index_by_uid.put(ext.uid, @intCast(funcs.items.len));
            try funcs.append(self.allocator, try self.bindExtern(ext));
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

        const string_pool = try StringPool.init(self.allocator, self.string_texts.items);
        self.string_texts.clearAndFree(self.allocator);
        self.string_id_by_text.clearAndFree();

        return Program{
            .string_pool = string_pool,
            .main_func_index = funcs.items.len - 1,
            .functions = try funcs.toOwnedSlice(self.allocator),
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
        self.next_free_register = 0;
        self.peak_register = 0;
        self.var_register_id_by_uid.clearRetainingCapacity();
        @memset(&self.register_types, self.type_table.getPrimitive(.void));

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
            self.chunk_heap_maps = .empty;
            self.chunk_build_recipes = .empty;
            self.chunk_jump_tables = .empty;
            self.const_id_by_value.clearRetainingCapacity();
        }

        try self.genInstructionBlock(func.body);

        if (self.chunk_instructions.getLastOrNull()) |last| {
            if (last.op != .RETURN) {
                try self.chunk_instructions.append(self.allocator, Instruction{ .op = .RETURN });
            }
        }

        const chunk = Chunk{
            .num_registers = self.peak_register,
            .num_params = @intCast(func.params.len),
            .instructions = try self.chunk_instructions.toOwnedSlice(self.allocator),
            .constants = try self.chunk_constants.toOwnedSlice(self.allocator),
            .shadow_types = try self.chunk_constants_types.toOwnedSlice(self.allocator),
            .heap_maps = try self.chunk_heap_maps.toOwnedSlice(self.allocator),
            .build_recipes = try self.chunk_build_recipes.toOwnedSlice(self.allocator),
            .jump_tables = try self.chunk_jump_tables.toOwnedSlice(self.allocator),
        };

        // chunk.disasamble();

        return chunk;
    }

    fn genInstructionBlock(self: *Self, block: ir.Block) anyerror!void {
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
                .match => |match| {
                    try self.genMatch(match);
                },
                .block => |instructions| {
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
                .break_stmt => try self.genBreak(),
                .continue_stmt => try self.genContinue(),
            }
        }
    }

    fn genStoreVar(self: *Self, store_var: ir.StoreVar) !void {
        const register_idx = self.var_register_id_by_uid.get(store_var.uid);
        if (register_idx) |idx| {
            if (isBuiltInSteps(store_var.value) and readsVariable(store_var.value, store_var.uid)) {
                const temp_reg = try self.genValue(store_var.value, try self.consumeRegister());
                defer self.freeRegister();
                try self.chunk_instructions.append(self.allocator, .{ .op = .LOAD, .a = idx, .b = temp_reg });
                self.register_types[idx] = store_var.type_id;
                return;
            }
            _ = try self.genValue(store_var.value, idx);
            self.register_types[idx] = store_var.type_id;
            return;
        }

        const reg_idx = try self.genValue(store_var.value, try self.consumeRegister());
        self.register_types[reg_idx] = store_var.type_id;
        try self.bindVariable(store_var.uid, reg_idx);
    }

    fn isBuiltInSteps(value: *const ir.Value) bool {
        return switch (value.data) {
            .struct_init, .i_array, .union_init, .block_value, .try_call, .catch_call => true,
            else => false,
        };
    }

    fn readsVariable(value: *const ir.Value, uid: usize) bool {
        return switch (value.data) {
            .identifier => |id| id.uid == uid,
            .indexed => |indexed| readsVariable(indexed.target, uid) or readsVariable(indexed.index, uid),
            .binary_op => |op| readsVariable(op.left, uid) or readsVariable(op.right, uid),
            .unary_op => |op| readsVariable(op.right, uid),
            .fn_call => |call| for (call.args) |arg| {
                if (readsVariable(arg, uid)) break true;
            } else false,
            .struct_init => |struct_init| for (struct_init.values) |field| {
                if (readsVariable(field, uid)) break true;
            } else false,
            .i_array => |array| for (array.values) |item| {
                if (readsVariable(item, uid)) break true;
            } else false,
            .string_interpolation => |parts| for (parts) |part| {
                if (readsVariable(part, uid)) break true;
            } else false,
            .union_init => |union_init| if (union_init.value) |payload| readsVariable(payload, uid) else false,
            .null_box_init => |box| if (box.value) |inner| readsVariable(inner, uid) else false,
            .union_tag, .union_payload, .int_to_float, .array_length, .buffer_new, .bytes_length => |inner| readsVariable(inner, uid),
            .try_call, .catch_call, .block_value => true,
            .i_float, .i_int, .i_bool, .i_string, .i_void, .i_null => false,
        };
    }

    fn bindVariable(self: *Self, uid: usize, reg: u8) !void {
        try self.var_register_id_by_uid.put(uid, reg);
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

    fn genMatch(self: *Self, match: ir.Match) !void {
        const saved_next_free = self.next_free_register;
        var end_jumps: std.ArrayList(usize) = .empty;
        defer end_jumps.deinit(self.allocator);

        var null_jump: ?usize = null;
        if (match.null_check) |condition| {
            const condition_reg = try self.genValue(condition, try self.consumeRegister());
            try self.chunk_instructions.append(self.allocator, .{ .op = .JUMP_IF_FALSE, .a = condition_reg });
            null_jump = self.chunk_instructions.items.len - 1;
        }

        var default_position: usize = undefined;
        if (match.table) |table| {
            const subject_reg = try self.genValue(table.subject, try self.consumeRegister());
            var table_inst = Instruction{ .op = .JUMP_TABLE, .a = subject_reg };
            table_inst.putBEx(@intCast(self.chunk_jump_tables.items.len));
            try self.chunk_instructions.append(self.allocator, table_inst);

            const case_positions = try self.allocator.alloc(u16, match.cases.len);
            for (match.cases, 0..) |case, i| {
                case_positions[i] = @intCast(self.chunk_instructions.items.len);
                try self.genInstructionBlock(case.body);
                try self.chunk_instructions.append(self.allocator, .{ .op = .JUMP });
                try end_jumps.append(self.allocator, self.chunk_instructions.items.len - 1);
            }
            default_position = self.chunk_instructions.items.len;
            try self.genInstructionBlock(match.default_block);

            const targets = try self.allocator.alloc(u16, table.case_by_offset.len);
            for (table.case_by_offset, targets) |case_index, *target| {
                target.* = if (case_index) |index| case_positions[index] else @intCast(default_position);
            }
            try self.chunk_jump_tables.append(self.allocator, .{ .min = table.min, .targets = targets, .default = @intCast(default_position) });
        } else {
            for (match.cases) |case| {
                const condition_reg = try self.genValue(case.condition, try self.consumeRegister());
                try self.chunk_instructions.append(self.allocator, .{ .op = .JUMP_IF_FALSE, .a = condition_reg });
                const next_case_jump = self.chunk_instructions.items.len - 1;
                try self.genInstructionBlock(case.body);
                try self.chunk_instructions.append(self.allocator, .{ .op = .JUMP });
                try end_jumps.append(self.allocator, self.chunk_instructions.items.len - 1);
                self.chunk_instructions.items[next_case_jump].putBEx(@intCast(self.chunk_instructions.items.len));
            }
            default_position = self.chunk_instructions.items.len;
            try self.genInstructionBlock(match.default_block);
        }

        if (null_jump) |jump| {
            var null_position = default_position;
            if (match.null_body) |body| {
                try self.chunk_instructions.append(self.allocator, .{ .op = .JUMP });
                try end_jumps.append(self.allocator, self.chunk_instructions.items.len - 1);
                null_position = self.chunk_instructions.items.len;
                try self.genInstructionBlock(body);
            }
            self.chunk_instructions.items[jump].putBEx(@intCast(null_position));
        }

        const end_position: u16 = @intCast(self.chunk_instructions.items.len);
        for (end_jumps.items) |jump| self.chunk_instructions.items[jump].putAEx(end_position);
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
            .first_break_patch_index = self.break_patches.items.len,
            .first_continue_patch_index = self.continue_patches.items.len,
        });
    }

    fn popLoopCtx(self: *Self) !void {
        const ctx = self.loop_stack.pop() orelse unreachable;
        const loop_end = self.chunk_instructions.items.len;
        for (self.break_patches.items[ctx.first_break_patch_index..]) |patch_index| {
            self.chunk_instructions.items[patch_index].putAEx(@intCast(loop_end));
        }
        self.break_patches.shrinkRetainingCapacity(ctx.first_break_patch_index);
    }

    fn bindExtern(self: *Self, ext: ir.Extern) !Function {
        const binding = natives.bindings.get(ext.identifier) orelse @panic("extern without binding");
        const num_args: u8 = @intCast(ext.params.len);
        const kind: FunctionKind = switch (binding) {
            .native => |func| blk: {
                try self.native_params_by_uid.put(ext.uid, ext.params);
                break :blk .{ .native = .{ .func = func, .num_args = num_args + self.dynamicParamCount(ext.params) } };
            },
            .@"inline" => |gen| blk: {
                const inl = InlineFn{ .gen = gen, .num_args = num_args };
                try self.inline_functions_by_uid.put(ext.uid, inl);
                break :blk .{ .@"inline" = inl };
            },
        };
        return .{ .uid = ext.uid, .name = ext.identifier, .kind = kind };
    }

    fn isDynamicParam(self: *Self, param: tt.TypedIdentifier) bool {
        return param.type_id == self.type_table.getPrimitive(.dynamic);
    }

    fn dynamicParamCount(self: *Self, params: []const tt.TypedIdentifier) u8 {
        var count: u8 = 0;
        for (params) |param| {
            if (self.isDynamicParam(param)) count += 1;
        }
        return count;
    }

    fn genArgKinds(self: *Self, fc: ir.FnCall) !usize {
        const params = self.native_params_by_uid.get(fc.fn_uid) orelse return 0;
        var count: usize = 0;
        for (params, fc.args) |param, arg| {
            if (!self.isDynamicParam(param)) continue;
            const kind_reg = try self.consumeRegister();
            const kind_value = ir.Value{
                .type_id = self.type_table.getPrimitive(.int),
                .data = .{ .i_int = @intFromEnum(v.ValueType.fromTypeId(self.type_table, arg.type_id)) },
            };
            try self.genLoadConst(v.TypedValue.from_ir_value(&kind_value), kind_reg);
            self.register_types[kind_reg] = self.type_table.getPrimitive(.int);
            count += 1;
        }
        return count;
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

        const kind_count = try self.genArgKinds(fc);

        var fn_call = Instruction{
            .op = .CALL,
            .a = target_reg,
        };

        fn_call.putBEx(self.fn_index_by_uid.get(fc.fn_uid) orelse unreachable);

        try self.chunk_instructions.append(self.allocator, fn_call);
        try self.recordHeapMap();

        self.freeRegisterN(fc.args.len + kind_count);
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
        if (return_stmt.value) |value| {
            if (self.tailCallOf(value)) |call| {
                if (self.tailCallIndex(call)) |fn_index| return self.genTailCall(call.data.fn_call, fn_index);
            }
        }

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

    fn tailCallOf(self: *Self, value: *const ir.Value) ?*const ir.Value {
        if (value.data == .try_call) return if (value.data.try_call.is_top_level) null else value.data.try_call.call;
        if (self.type_table.isErrorUnion(value.type_id)) return null;
        return value;
    }

    fn tailCallIndex(self: *Self, value: *const ir.Value) ?u16 {
        if (value.data != .fn_call) return null;
        const fn_index = self.fn_index_by_uid.get(value.data.fn_call.fn_uid) orelse return null;
        if (fn_index < self.extern_count) return null;
        return fn_index;
    }

    fn genTailCall(self: *Self, fc: ir.FnCall, fn_index: u16) !void {
        const call_reg = try self.consumeRegister();
        for (fc.args) |arg| {
            _ = try self.genValue(arg, try self.consumeRegister());
        }
        var tail_call = Instruction{ .op = .TAIL_CALL, .a = call_reg };
        tail_call.putBEx(fn_index);
        try self.chunk_instructions.append(self.allocator, tail_call);
        self.freeRegisterN(fc.args.len + 1);
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

            .string_interpolation => |parts| try self.genStringBuild(parts, value.type_id, target_reg),

            .indexed => |idx| {
                try self.genIndexedLoad(idx, value, target_reg);
            },

            .identifier => |id| {
                const symbol_reg = self.var_register_id_by_uid.get(id.uid) orelse return BytecodeError.UndefinedVariable;

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
            .array_length => |array| try self.genUnaryOpcode(.ARRAY_LEN, array, value.type_id, target_reg),
            .bytes_length => |bytes| try self.genUnaryOpcode(.BYTES_LEN, bytes, value.type_id, target_reg),
            .buffer_new => |size| {
                try self.genUnaryOpcode(.BYTES_NEW, size, value.type_id, target_reg);
                try self.recordHeapMap();
            },
        }

        return target_reg;
    }

    const max_build_parts = 32;

    fn isFormattedLate(kind: v.ValueType) bool {
        return switch (kind) {
            .int64, .float64, .bool, .null, .string => true,
            else => false,
        };
    }

    fn genStringBuild(self: *Self, parts: []const *ir.Value, result_type: tt.TypeId, target_reg: u8) !void {
        const build_reg = try self.consumeRegister();
        defer self.freeRegister();
        var kinds: std.ArrayList(v.ValueType) = .empty;
        defer kinds.deinit(self.allocator);
        var index: usize = 0;
        var has_previous = false;
        while (index < parts.len) {
            const saved_next_free = self.next_free_register;
            kinds.clearRetainingCapacity();
            if (has_previous) {
                const previous_reg = try self.consumeRegister();
                try self.chunk_instructions.append(self.allocator, .{ .op = .LOAD, .a = previous_reg, .b = build_reg });
                self.register_types[previous_reg] = result_type;
                try kinds.append(self.allocator, .string);
            }
            while (index < parts.len and kinds.items.len < max_build_parts) : (index += 1) {
                const part_reg = try self.genValue(parts[index], try self.consumeRegister());
                const kind = v.ValueType.fromTypeId(self.type_table, parts[index].type_id);
                if (!isFormattedLate(kind)) {
                    try self.chunk_instructions.append(self.allocator, .{ .op = .TO_STRING, .a = part_reg, .b = part_reg, .c = @intFromEnum(kind) });
                    try self.recordHeapMap();
                    self.register_types[part_reg] = result_type;
                    try kinds.append(self.allocator, .string);
                    continue;
                }
                try kinds.append(self.allocator, kind);
            }
            var build = Instruction{ .op = .STR_BUILD, .a = build_reg };
            build.putBEx(@intCast(self.chunk_build_recipes.items.len));
            try self.chunk_build_recipes.append(self.allocator, try self.allocator.dupe(v.ValueType, kinds.items));
            try self.chunk_instructions.append(self.allocator, build);
            try self.recordHeapMap();
            self.register_types[build_reg] = result_type;
            self.freeRegisterN(self.next_free_register - saved_next_free);
            has_previous = true;
        }
        try self.chunk_instructions.append(self.allocator, .{ .op = .LOAD, .a = target_reg, .b = build_reg });
        self.register_types[target_reg] = result_type;
    }

    fn genUnaryOpcode(self: *Self, op: OpCode, operand: *const ir.Value, result_type: tt.TypeId, target_reg: u8) !void {
        const saved_next_free = self.next_free_register;
        const operand_register = try self.genValue(operand, try self.consumeRegister());
        try self.chunk_instructions.append(self.allocator, .{ .op = op, .a = target_reg, .b = operand_register });
        self.register_types[target_reg] = result_type;
        self.freeRegisterN(self.next_free_register - saved_next_free);
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

        std.debug.assert(self.chunk_instructions.items[self.chunk_instructions.items.len - 1].op.canFail());
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
                return BytecodeError.UndefinedVariable;
            },
            .@"enum" => return BytecodeError.UndefinedVariable,
            .array => {
                const array_reg = try self.genValue(idx.target, try self.consumeRegister());
                defer self.freeRegister();
                try self.genArrayLoad(target_reg, array_reg, idx.index);
                self.register_types[target_reg] = value.type_id;
            },
            .buffer, .string => {
                const buffer_reg = try self.genValue(idx.target, try self.consumeRegister());
                const index_reg = try self.genValue(idx.index, try self.consumeRegister());
                defer self.freeRegisterN(2);
                try self.chunk_instructions.append(self.allocator, .{ .op = .BYTES_GET, .a = target_reg, .b = buffer_reg, .c = index_reg });
                self.register_types[target_reg] = value.type_id;
            },
            .@"union" => |@"union"| {
                const field_name = idx.index.data.i_string;

                if (!@"union".variants.contains(field_name)) return BytecodeError.UndefinedVariable;

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
                return BytecodeError.UndefinedVariable;
            },
            .@"enum", .@"union" => return BytecodeError.UndefinedVariable,
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
            .buffer => {
                const buffer_reg = try self.genValue(indexed.target, try self.consumeRegister());
                const index_reg = try self.genValue(indexed.index, try self.consumeRegister());
                const value_reg = try self.genValue(stmt.value, try self.consumeRegister());
                defer self.freeRegisterN(3);
                try self.chunk_instructions.append(self.allocator, .{ .op = .BYTES_SET, .a = buffer_reg, .b = index_reg, .c = value_reg });
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
        const result = try self.string_id_by_text.getOrPut(s);
        if (!result.found_existing) {
            if (self.string_texts.items.len > std.math.maxInt(u16)) {
                _ = self.string_id_by_text.remove(s);
                return self.tooManyStrings();
            }
            result.value_ptr.* = @intCast(self.string_texts.items.len);
            try self.string_texts.append(self.allocator, s);
        }

        const string_id = result.value_ptr.*;

        var load_string = Instruction{
            .op = .LOAD_STRING,
            .a = target_reg,
        };

        load_string.putBEx(string_id);
        try self.chunk_instructions.append(self.allocator, load_string);
    }

    fn tooManyStrings(self: *Self) BytecodeError {
        _ = self;
        if (@import("builtin").is_test) return BytecodeError.TooManyStrings;
        std.debug.print("error: the program has more than 65536 different string literals\n", .{});
        return BytecodeError.TooManyStrings;
    }

    fn genBinOp(self: *Self, bo: ir.BinaryOp, target_reg: u8) !void {
        const left_reg = try self.genValue(bo.left, try self.consumeRegister());
        const right_reg = try self.genValue(bo.right, try self.consumeRegister());
        defer self.freeRegisterN(2);

        const op = OpCode.fromIrBinaryOp(bo.kind);

        const left_is_int = self.isIntegerType(bo.left.type_id);
        const right_is_int = self.isIntegerType(bo.right.type_id);

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

    fn isIntegerType(self: *Self, type_id: tt.TypeId) bool {
        const kind = self.type_table.getTypePtrById(type_id).kind;
        return kind == .int or kind == .byte;
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
            .to_byte => self.type_table.getPrimitive(.byte),
        };
    }

    fn tooManyRegisters(self: *Self) BytecodeError {
        if (@import("builtin").is_test) return BytecodeError.TooManyRegisters;
        const hint = "uses more than 255 registers at once (variables and temporaries); split it into smaller functions";
        std.debug.print("error: function '{s}' {s}\n", .{ self.current_function_name, hint });
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
    struct_descriptors: []const StructDescriptor,
    union_descriptors: []const UnionDescriptor,
    error_names: []const []const u8,
};

pub const NativeResult = struct { value: v.Value, has_failed: bool = false };

pub const NativeFnPtr = *const fn (vm: *VM, args: []v.Value) anyerror!NativeResult;

pub const NativeFn = struct {
    func: NativeFnPtr,
    num_args: u8,
};

pub const InlineFn = struct {
    gen: *const fn (target_reg: u8, arg_start_reg: u8, std.mem.Allocator) anyerror![]const Instruction,
    num_args: u8,
};

pub const FunctionKind = union(enum) {
    dusk: Chunk,
    native: NativeFn,
    @"inline": InlineFn,
};

pub const Function = struct {
    uid: usize,
    name: []const u8,
    kind: FunctionKind,
};

pub const JumpTable = struct {
    min: i64,
    targets: []const u16,
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
            .CALL, .TAIL_CALL => std.debug.print("R[{d}] FN[{d}]", .{ inst.a, inst.bEx() }),
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
            .TO_STRING => std.debug.print("R[{d}] R[{d}] T[{d}]", .{ inst.a, inst.b, inst.c }),
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

    CALL,
    TAIL_CALL,
    RETURN,
    RETURN_ERR,
    PANIC_ERR,

    JUMP,
    JUMP_IF_FALSE,
    JUMP_TABLE,

    pub fn canFail(self: Self) bool {
        return self == .CALL;
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
            .to_byte => Self.I_TO_BYTE,
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

const BytecodeError = error{ OutOfMemory, UndefinedVariable, TooManyRegisters, TooManyStrings };

const FailingCallHandler = union(enum) {
    try_call: ir.TryCall,
    catch_call: ir.CatchCall,
};

const LoopContext = struct {
    first_break_patch_index: usize,
    first_continue_patch_index: usize,
};
