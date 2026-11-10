const std = @import("std");
const ir = @import("ir.zig");
const v = @import("../runtime/value.zig");
const tt = @import("type_table.zig");
const bc = @import("../runtime/bytecode.zig");
const natives = @import("../runtime/builtins/natives.zig");
const err = @import("compile_error.zig");

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
    chunk_jump_tables: std.ArrayList(bc.JumpTable) = .empty,
    string_id_by_text: std.StringHashMap(u16),

    chunk_instructions: std.ArrayList(bc.Instruction),
    loop_stack: std.ArrayList(LoopContext),
    break_patches: std.ArrayList(usize),
    continue_patches: std.ArrayList(usize),
    intrinsic_by_uid: std.AutoHashMap(usize, natives.Intrinsic),
    native_params_by_uid: std.AutoHashMap(usize, []const tt.TypedIdentifier),
    fn_index_by_uid: std.AutoHashMap(usize, u16),
    boxed_uids: std.AutoHashMapUnmanaged(usize, void) = .empty,
    boxed_type_by_uid: std.AutoHashMapUnmanaged(usize, tt.TypeId) = .empty,

    chunk_heap_maps: std.ArrayList(bc.HeapMap),
    struct_descriptors: std.ArrayList(bc.StructDescriptor),
    type_id_to_struct_descriptor_idx: std.AutoHashMap(tt.TypeId, u16),
    union_descriptors: std.ArrayList(bc.UnionDescriptor),
    type_id_to_union_descriptor_idx: std.AutoHashMap(tt.TypeId, u16),
    union_tag_by_type_id: std.AutoHashMap(tt.TypeId, std.StringHashMap(u16)),
    current_function_name: []const u8 = "",
    current_function_loc: ?err.Loc = null,
    err_reporter: err.CompileErrorReporter,
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
            .intrinsic_by_uid = std.AutoHashMap(usize, natives.Intrinsic).init(alloc),
            .native_params_by_uid = std.AutoHashMap(usize, []const tt.TypedIdentifier).init(alloc),
            .fn_index_by_uid = std.AutoHashMap(usize, u16).init(alloc),
            .chunk_heap_maps = .empty,
            .struct_descriptors = .empty,
            .type_id_to_struct_descriptor_idx = std.AutoHashMap(tt.TypeId, u16).init(alloc),
            .union_descriptors = .empty,
            .type_id_to_union_descriptor_idx = std.AutoHashMap(tt.TypeId, u16).init(alloc),
            .union_tag_by_type_id = std.AutoHashMap(tt.TypeId, std.StringHashMap(u16)).init(alloc),
            .err_reporter = .{ .src = "", .allocator = alloc },
        };
    }

    pub fn generate(self: *Self, program: *const ir.Program) !bc.Program {
        var funcs: std.ArrayList(bc.Function) = .empty;
        self.extern_count = program.externs.len;
        self.boxed_uids = program.boxed_uids;

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

        const functions = try funcs.toOwnedSlice(self.allocator);
        const static_closures = try self.allocator.alloc(v.Closure, functions.len);
        for (functions, static_closures) |*function, *closure| {
            closure.* = v.Closure.initStatic(function);
        }

        const string_pool = try bc.StringPool.init(self.allocator, self.string_texts.items);
        self.string_texts.clearAndFree(self.allocator);
        self.string_id_by_text.clearAndFree();

        return bc.Program{
            .string_pool = string_pool,
            .main_func_index = functions.len - 1,
            .functions = functions,
            .static_closures = static_closures,
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

    fn genMainFunc(self: *Self, program: *const ir.Program) !bc.Function {
        self.enterFunction("$main", null);
        self.next_free_register = 0;
        self.peak_register = 0;
        self.var_register_id_by_uid.clearRetainingCapacity();
        @memset(&self.register_types, self.type_table.getPrimitive(.void));

        return bc.Function{
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

    fn addFunction(self: *Self, funcs: *std.ArrayList(bc.Function), func: ir.Func) !void {
        try self.fn_index_by_uid.put(func.uid, @intCast(funcs.items.len));
        try funcs.append(self.allocator, try self.genFunction(func));
    }

    fn genFunction(self: *Self, func: ir.Func) !bc.Function {
        self.enterFunction(func.identifier, func.origin);
        self.next_free_register = @intCast(func.params.len);
        self.peak_register = @intCast(func.params.len);
        self.var_register_id_by_uid.clearRetainingCapacity();
        @memset(&self.register_types, self.type_table.getPrimitive(.void));

        for (func.params, 0..) |param, i| {
            try self.var_register_id_by_uid.put(param.uid, @intCast(i));
            self.register_types[i] = param.type_id;
        }

        return bc.Function{
            .uid = func.uid,
            .name = func.identifier,
            .kind = .{ .dusk = try self.genFunctionChunk(func) },
        };
    }

    fn enterFunction(self: *Self, name: []const u8, origin: ?ir.FuncOrigin) void {
        self.current_function_name = name;
        self.current_function_loc = if (origin) |known| known.loc else null;
        self.err_reporter.src = if (origin) |known| known.src else "";
        self.err_reporter.copy_context = if (origin) |known| known.copy_context else null;
    }

    fn genFunctionChunk(self: *Self, func: ir.Func) !bc.Chunk {
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
                try self.chunk_instructions.append(self.allocator, bc.Instruction{ .op = .RETURN });
            }
        }
        _ = try self.currentPosition();

        const chunk = bc.Chunk{
            .num_registers = self.peak_register,
            .num_params = @intCast(func.params.len),
            .instructions = try self.chunk_instructions.toOwnedSlice(self.allocator),
            .constants = try self.chunk_constants.toOwnedSlice(self.allocator),
            .shadow_types = try self.chunk_constants_types.toOwnedSlice(self.allocator),
            .heap_maps = try self.chunk_heap_maps.toOwnedSlice(self.allocator),
            .build_recipes = try self.chunk_build_recipes.toOwnedSlice(self.allocator),
            .jump_tables = try self.chunk_jump_tables.toOwnedSlice(self.allocator),
            .capture_kinds = try self.captureKinds(func.captures),
        };

        // chunk.disassemble();

        return chunk;
    }

    fn captureKinds(self: *Self, captures: []const ir.Capture) ![]const v.ValueType {
        const kinds = try self.allocator.alloc(v.ValueType, captures.len);
        for (captures, kinds) |capture, *kind| {
            kind.* = if (capture.is_boxed) .cell else valueTypeFromTypeId(self.type_table, capture.type_id);
        }
        return kinds;
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
                .when => |when| {
                    try self.genWhen(when);
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
                .update_capture => |stmt| {
                    try self.genUpdateCapture(stmt);
                },
                .break_stmt => try self.genBreak(),
                .continue_stmt => try self.genContinue(),
            }
        }
    }

    fn genStoreVar(self: *Self, store_var: ir.StoreVar) !void {
        if (self.boxed_uids.contains(store_var.uid)) {
            return self.genStoreBoxedVar(store_var);
        }
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

    fn genStoreBoxedVar(self: *Self, store_var: ir.StoreVar) !void {
        if (self.var_register_id_by_uid.get(store_var.uid)) |cell_reg| {
            const value_reg = try self.genValue(store_var.value, try self.consumeRegister());
            defer self.freeRegister();
            try self.chunk_instructions.append(self.allocator, .{ .op = .CELL_SET, .a = cell_reg, .b = value_reg });
            return;
        }

        const reg = try self.genValue(store_var.value, try self.consumeRegister());
        self.register_types[reg] = store_var.type_id;
        try self.chunk_instructions.append(self.allocator, .{
            .op = .CELL_NEW,
            .a = reg,
            .b = @intFromEnum(valueTypeFromTypeId(self.type_table, store_var.type_id)),
            .c = reg,
        });
        try self.recordHeapMap();
        self.register_types[reg] = self.type_table.getPrimitive(.cell);
        try self.boxed_type_by_uid.put(self.allocator, store_var.uid, store_var.type_id);
        try self.bindVariable(store_var.uid, reg);
    }

    fn genUpdateCapture(self: *Self, stmt: ir.UpdateCapture) !void {
        const cell_reg = try self.consumeRegister();
        defer self.freeRegister();
        try self.chunk_instructions.append(self.allocator, .{ .op = .CAPTURE_GET, .a = cell_reg, .b = stmt.index });
        self.register_types[cell_reg] = self.type_table.getPrimitive(.cell);
        const value_reg = try self.genValue(stmt.value, try self.consumeRegister());
        defer self.freeRegister();
        try self.chunk_instructions.append(self.allocator, .{ .op = .CELL_SET, .a = cell_reg, .b = value_reg });
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
                if (readsVariable(arg, uid)) {
                    break true;
                }
            } else false,
            .struct_init => |struct_init| for (struct_init.values) |field| {
                if (readsVariable(field, uid)) {
                    break true;
                }
            } else false,
            .i_array => |array| for (array.values) |item| {
                if (readsVariable(item, uid)) {
                    break true;
                }
            } else false,
            .string_interpolation => |parts| for (parts) |part| {
                if (readsVariable(part, uid)) {
                    break true;
                }
            } else false,
            .call_value => |call| readsVariable(call.callee, uid) or for (call.args) |arg| {
                if (readsVariable(arg, uid)) {
                    break true;
                }
            } else false,
            .make_closure => |closure| for (closure.sources) |source| {
                if (readsVariable(source, uid)) {
                    break true;
                }
            } else false,
            .var_box => |box_uid| box_uid == uid,
            .capture_ref, .capture_raw => false,
            .union_init => |union_init| if (union_init.value) |payload| readsVariable(payload, uid) else false,
            .null_box_init => |box| if (box.value) |inner| readsVariable(inner, uid) else false,
            .union_tag, .union_payload, .int_to_float, .array_length, .bytes_length => |inner| readsVariable(inner, uid),
            .try_call, .catch_call, .block_value => true,
            .i_float, .i_int, .i_bool, .i_string, .i_void, .i_null, .func_ref => false,
        };
    }

    fn bindVariable(self: *Self, uid: usize, reg: u8) !void {
        try self.var_register_id_by_uid.put(uid, reg);
    }

    fn genBranchIf(self: *Self, ifStmt: ir.BranchIf) !void {
        const saved_next_free = self.next_free_register;

        const condition_reg = try self.genValue(ifStmt.condition, try self.consumeRegister());

        const jump_into_else = bc.Instruction{
            .op = .JUMP_IF_FALSE,
            .a = condition_reg,
        };

        try self.chunk_instructions.append(self.allocator, jump_into_else);
        const jump_into_else_idx = self.chunk_instructions.items.len - 1;

        try self.genInstructionBlock(ifStmt.then_block);

        if (ifStmt.else_block.len > 0) {
            const jump_pass_else = bc.Instruction{
                .op = .JUMP,
            };

            try self.chunk_instructions.append(self.allocator, jump_pass_else);
            const jump_pass_else_idx = self.chunk_instructions.items.len - 1;
            const jump_into_else_position = try self.currentPosition();

            try self.genInstructionBlock(ifStmt.else_block);
            const jump_pass_else_position = try self.currentPosition();

            self.chunk_instructions.items[jump_into_else_idx].putBEx(jump_into_else_position);
            self.chunk_instructions.items[jump_pass_else_idx].putAEx(jump_pass_else_position);
        } else {
            const jump_into_else_position = try self.currentPosition();
            self.chunk_instructions.items[jump_into_else_idx].putBEx(jump_into_else_position);
        }

        self.freeRegisterN(self.next_free_register - saved_next_free);
    }

    fn genWhen(self: *Self, when: ir.When) !void {
        const saved_next_free = self.next_free_register;
        var end_jumps: std.ArrayList(usize) = .empty;
        defer end_jumps.deinit(self.allocator);

        var null_jump: ?usize = null;
        if (when.null_check) |condition| {
            const condition_reg = try self.genValue(condition, try self.consumeRegister());
            try self.chunk_instructions.append(self.allocator, .{ .op = .JUMP_IF_FALSE, .a = condition_reg });
            null_jump = self.chunk_instructions.items.len - 1;
        }

        var case_positions: []u16 = &.{};
        defer self.allocator.free(case_positions);
        var table_index: usize = undefined;
        if (when.table) |table| {
            case_positions = try self.allocator.alloc(u16, when.cases.len);
            const subject_reg = try self.genValue(table.subject, try self.consumeRegister());
            table_index = self.chunk_jump_tables.items.len;
            try self.chunk_jump_tables.append(self.allocator, undefined);
            var table_inst = bc.Instruction{ .op = .JUMP_TABLE, .a = subject_reg };
            table_inst.putBEx(@intCast(table_index));
            try self.chunk_instructions.append(self.allocator, table_inst);

            for (when.cases, 0..) |case, i| {
                case_positions[i] = try self.currentPosition();
                const case_next_free = self.next_free_register;
                try self.genInstructionBlock(case.body);
                self.freeRegisterN(self.next_free_register - case_next_free);
                try self.chunk_instructions.append(self.allocator, .{ .op = .JUMP });
                try end_jumps.append(self.allocator, self.chunk_instructions.items.len - 1);
            }
        } else {
            for (when.cases) |case| {
                const case_next_free = self.next_free_register;
                const condition_reg = try self.genValue(case.condition, try self.consumeRegister());
                try self.chunk_instructions.append(self.allocator, .{ .op = .JUMP_IF_FALSE, .a = condition_reg });
                const next_case_jump = self.chunk_instructions.items.len - 1;
                self.freeRegisterN(self.next_free_register - case_next_free);
                try self.genInstructionBlock(case.body);
                self.freeRegisterN(self.next_free_register - case_next_free);
                try self.chunk_instructions.append(self.allocator, .{ .op = .JUMP });
                try end_jumps.append(self.allocator, self.chunk_instructions.items.len - 1);
                self.chunk_instructions.items[next_case_jump].putBEx(try self.currentPosition());
            }
        }

        const default_position = try self.currentPosition();
        try self.genInstructionBlock(when.default_block);
        if (when.table) |table| {
            const target_offsets = try self.allocator.alloc(u16, table.case_by_offset.len);
            for (table.case_by_offset, target_offsets) |case_index, *target| {
                target.* = if (case_index) |index| case_positions[index] else default_position;
            }
            self.chunk_jump_tables.items[table_index] = .{ .min = table.min, .target_offsets = target_offsets, .default = default_position };
        }

        if (null_jump) |jump| {
            var null_position = default_position;
            if (when.null_body) |body| {
                try self.chunk_instructions.append(self.allocator, .{ .op = .JUMP });
                try end_jumps.append(self.allocator, self.chunk_instructions.items.len - 1);
                null_position = try self.currentPosition();
                try self.genInstructionBlock(body);
            }
            self.chunk_instructions.items[jump].putBEx(null_position);
        }

        const end_position = try self.currentPosition();
        for (end_jumps.items) |jump| self.chunk_instructions.items[jump].putAEx(end_position);
        self.freeRegisterN(self.next_free_register - saved_next_free);
    }

    fn genLoop(self: *Self, loopStmt: ir.Loop) !void {
        const saved_next_free = self.next_free_register;
        const loop_begin_position = try self.currentPosition();

        try self.pushLoopCtx();

        const condition_reg = if (loopStmt.condition) |cond|
            try self.genValue(cond, try self.consumeRegister())
        else
            try self.genTrueValue(try self.consumeRegister());

        const jump_pass_loop = bc.Instruction{
            .op = .JUMP_IF_FALSE,
            .a = condition_reg,
        };

        try self.chunk_instructions.append(self.allocator, jump_pass_loop);
        const jump_if_false_idx = self.chunk_instructions.items.len - 1;

        try self.genInstructionBlock(loopStmt.do_block);

        const advancement_begin = try self.currentPosition();
        const context = self.loop_stack.items[self.loop_stack.items.len - 1];
        for (self.continue_patches.items[context.first_continue_patch_index..]) |patch_index| {
            self.chunk_instructions.items[patch_index].putAEx(advancement_begin);
        }
        self.continue_patches.shrinkRetainingCapacity(context.first_continue_patch_index);
        try self.genInstructionBlock(loopStmt.advancement);

        var jump_to_begin = bc.Instruction{
            .op = .JUMP,
        };

        jump_to_begin.putAEx(loop_begin_position);

        try self.chunk_instructions.append(self.allocator, jump_to_begin);
        const jump_pass_loop_position = try self.currentPosition();

        self.chunk_instructions.items[jump_if_false_idx].putBEx(jump_pass_loop_position);

        try self.popLoopCtx();

        self.freeRegisterN(self.next_free_register - saved_next_free);
    }

    fn genBreak(self: *Self) !void {
        try self.break_patches.append(self.allocator, self.chunk_instructions.items.len);
        try self.chunk_instructions.append(self.allocator, bc.Instruction{ .op = .JUMP });
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
        const loop_end = try self.currentPosition();
        for (self.break_patches.items[ctx.first_break_patch_index..]) |patch_index| {
            self.chunk_instructions.items[patch_index].putAEx(loop_end);
        }
        self.break_patches.shrinkRetainingCapacity(ctx.first_break_patch_index);
    }

    fn bindExtern(self: *Self, ext: ir.Extern) !bc.Function {
        const binding = natives.bindings.get(ext.identifier) orelse @panic("extern without binding");
        const num_args: u8 = @intCast(ext.params.len);
        const kind: bc.FunctionKind = switch (binding) {
            .native => |func| blk: {
                try self.native_params_by_uid.put(ext.uid, ext.params);
                break :blk .{ .native = .{ .func = func, .num_args = num_args + self.dynamicParamCount(ext.params) } };
            },
            .intrinsic => |intrinsic| blk: {
                try self.intrinsic_by_uid.put(ext.uid, intrinsic);
                break :blk .{ .intrinsic = intrinsic };
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
                .data = .{ .i_int = @intFromEnum(valueTypeFromTypeId(self.type_table, arg.type_id)) },
            };
            try self.genLoadConst(typedValueFromIr(&kind_value), kind_reg);
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

        if (self.intrinsic_by_uid.get(fc.fn_uid)) |intrinsic| {
            try self.genIntrinsicCall(intrinsic, fc, value, target_reg);
            return;
        }

        const kind_count = try self.genArgKinds(fc);

        var fn_call = bc.Instruction{
            .op = .CALL,
            .a = target_reg,
        };

        fn_call.putBEx(self.fn_index_by_uid.get(fc.fn_uid) orelse unreachable);

        try self.chunk_instructions.append(self.allocator, fn_call);
        try self.recordHeapMap();

        self.freeRegisterN(fc.args.len + kind_count);
        self.register_types[target_reg] = value.type_id;
    }

    fn genCallValue(self: *Self, cv: ir.CallValue, value: *const ir.Value, target_reg: u8) !void {
        if (target_reg + 1 != self.next_free_register) {
            const call_reg = try self.consumeRegister();
            defer self.freeRegister();
            try self.genCallValue(cv, value, call_reg);
            try self.chunk_instructions.append(self.allocator, .{ .op = .LOAD, .a = target_reg, .b = call_reg });
            self.register_types[target_reg] = value.type_id;
            return;
        }

        _ = try self.genValue(cv.callee, target_reg);
        for (cv.args) |arg| {
            _ = try self.genValue(arg, try self.consumeRegister());
        }

        try self.chunk_instructions.append(self.allocator, .{ .op = .CALL_VALUE, .a = target_reg });
        try self.recordHeapMap();

        self.freeRegisterN(cv.args.len);
        self.register_types[target_reg] = value.type_id;
    }

    fn genMakeClosure(self: *Self, closure: ir.MakeClosure, value: *const ir.Value, target_reg: u8) !void {
        if (target_reg + 1 != self.next_free_register) {
            const closure_reg = try self.consumeRegister();
            defer self.freeRegister();
            try self.genMakeClosure(closure, value, closure_reg);
            try self.chunk_instructions.append(self.allocator, .{ .op = .LOAD, .a = target_reg, .b = closure_reg });
            self.register_types[target_reg] = value.type_id;
            return;
        }

        for (closure.sources) |source| {
            _ = try self.genValue(source, try self.consumeRegister());
        }
        var make = bc.Instruction{ .op = .MAKE_CLOSURE, .a = target_reg };
        make.putBEx(self.fn_index_by_uid.get(closure.fn_uid) orelse unreachable);
        try self.chunk_instructions.append(self.allocator, make);
        try self.recordHeapMap();

        self.freeRegisterN(closure.sources.len);
        self.register_types[target_reg] = value.type_id;
    }

    fn genCall(self: *Self, call: *const ir.Value, target_reg: u8) !void {
        switch (call.data) {
            .fn_call => |fc| try self.genFnCall(fc, call, target_reg),
            .call_value => |cv| try self.genCallValue(cv, call, target_reg),
            else => unreachable,
        }
    }

    fn genIntrinsicCall(self: *Self, intrinsic: natives.Intrinsic, fc: ir.FnCall, value: *const ir.Value, target_reg: u8) !void {
        const arg = target_reg + 1;
        switch (intrinsic) {
            .array_len => try self.appendIntrinsicInstruction(.{ .op = .ARRAY_LEN, .a = target_reg, .b = arg }),
            .array_pop => try self.appendIntrinsicInstruction(.{ .op = .ARRAY_POP, .a = target_reg, .b = arg }),
            .bytes_len => try self.appendIntrinsicInstruction(.{ .op = .BYTES_LEN, .a = target_reg, .b = arg }),
            .new_buffer => try self.appendIntrinsicInstruction(.{ .op = .BYTES_NEW, .a = target_reg, .b = arg }),
            .to_byte => try self.appendIntrinsicInstruction(.{ .op = .I_TO_BYTE, .a = target_reg, .b = arg }),
            .to_int => {
                try self.appendIntrinsicInstruction(.{ .op = .LOAD, .a = target_reg, .b = arg });
                try self.appendIntrinsicInstruction(.{ .op = .F_TO_I, .a = target_reg });
            },
            .to_float => {
                try self.appendIntrinsicInstruction(.{ .op = .LOAD, .a = target_reg, .b = arg });
                const is_float_arg = fc.args[0].type_id == self.type_table.getPrimitive(.float);
                if (!is_float_arg) try self.appendIntrinsicInstruction(.{ .op = .I_TO_F, .a = target_reg });
            },
            .append => try self.appendIntrinsicInstruction(.{ .op = .ARRAY_APPEND, .a = arg, .b = arg + 1 }),
            .copy => try self.appendIntrinsicInstruction(.{ .op = .DEEP_COPY, .a = target_reg, .b = arg }),
            .task_spawn => {
                const payload = self.type_table.getTypePtrById(value.type_id).kind.task.payload;
                const payload_kind = valueTypeFromTypeId(self.type_table, payload);
                try self.appendIntrinsicInstruction(.{ .op = .TASK_SPAWN, .a = target_reg, .b = arg, .c = @intFromEnum(payload_kind) });
            },
        }
        self.freeRegisterN(fc.args.len);
        self.register_types[target_reg] = value.type_id;
    }

    fn appendIntrinsicInstruction(self: *Self, instruction: bc.Instruction) !void {
        try self.chunk_instructions.append(self.allocator, instruction);
        if (instruction.op.canTriggerGc()) try self.recordHeapMap();
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

        const inst = bc.Instruction{
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
        var tail_call = bc.Instruction{ .op = .TAIL_CALL, .a = call_reg };
        tail_call.putBEx(fn_index);
        try self.chunk_instructions.append(self.allocator, tail_call);
        self.freeRegisterN(fc.args.len + 1);
    }

    fn genValue(self: *Self, value: *const ir.Value, target_reg: u8) anyerror!u8 {
        switch (value.data) {
            .i_int, .i_float, .i_bool, .i_null, .i_void => {
                try self.genLoadConst(typedValueFromIr(value), target_reg);
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
                if (self.boxed_type_by_uid.get(id.uid)) |boxed_type| {
                    try self.chunk_instructions.append(self.allocator, .{ .op = .CELL_GET, .a = target_reg, .b = symbol_reg });
                    self.register_types[target_reg] = boxed_type;
                    return target_reg;
                }

                const inst = bc.Instruction{
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

            .func_ref => |func_ref| {
                var load = bc.Instruction{ .op = .LOAD_FUNC, .a = target_reg };
                load.putBEx(self.fn_index_by_uid.get(func_ref.fn_uid) orelse unreachable);
                try self.chunk_instructions.append(self.allocator, load);
                self.register_types[target_reg] = value.type_id;
            },

            .call_value => |cv| {
                try self.genCallValue(cv, value, target_reg);
            },

            .make_closure => |closure| {
                try self.genMakeClosure(closure, value, target_reg);
            },

            .capture_ref => |capture| {
                try self.chunk_instructions.append(self.allocator, .{ .op = .CAPTURE_GET, .a = target_reg, .b = capture.index });
                if (capture.is_boxed) {
                    try self.chunk_instructions.append(self.allocator, .{ .op = .CELL_GET, .a = target_reg, .b = target_reg });
                }
                self.register_types[target_reg] = value.type_id;
            },

            .capture_raw => |index| {
                try self.chunk_instructions.append(self.allocator, .{ .op = .CAPTURE_GET, .a = target_reg, .b = index });
                self.register_types[target_reg] = value.type_id;
            },

            .var_box => |uid| {
                const cell_reg = self.var_register_id_by_uid.get(uid) orelse return BytecodeError.UndefinedVariable;
                try self.chunk_instructions.append(self.allocator, .{ .op = .LOAD, .a = target_reg, .b = cell_reg });
                self.register_types[target_reg] = value.type_id;
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
                const kind = valueTypeFromTypeId(self.type_table, parts[index].type_id);
                if (!isFormattedLate(kind)) {
                    try self.chunk_instructions.append(self.allocator, .{ .op = .TO_STRING, .a = part_reg, .b = part_reg, .c = @intFromEnum(kind) });
                    try self.recordHeapMap();
                    self.register_types[part_reg] = result_type;
                    try kinds.append(self.allocator, .string);
                    continue;
                }
                try kinds.append(self.allocator, kind);
            }
            var build = bc.Instruction{ .op = .STR_BUILD, .a = build_reg };
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

    fn genUnaryOpcode(self: *Self, op: bc.OpCode, operand: *const ir.Value, result_type: tt.TypeId, target_reg: u8) !void {
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
        self.chunk_instructions.items[null_jump_index].putBEx(try self.currentPosition());
        self.freeRegisterN(self.next_free_register - saved_next_free);
    }

    fn genFailingCall(self: *Self, call: *const ir.Value, value: *const ir.Value, target_reg: u8, handler: FailingCallHandler) !void {
        const error_union = self.type_table.getTypePtrById(call.type_id).kind.error_union;
        const call_reg = try self.consumeRegister();
        defer self.freeRegister();

        try self.genCall(call, call_reg);
        self.register_types[call_reg] = error_union.error_set;

        std.debug.assert(self.chunk_instructions.items[self.chunk_instructions.items.len - 1].op.canFail());
        try self.chunk_instructions.append(self.allocator, .{ .op = .JUMP });
        const jump_ok_idx = self.chunk_instructions.items.len - 1;

        switch (handler) {
            .try_call => |tc| {
                const op: bc.OpCode = if (tc.is_top_level) .PANIC_ERR else .RETURN_ERR;
                try self.chunk_instructions.append(self.allocator, .{ .op = op, .a = call_reg });
            },
            .catch_call => |cc| switch (cc.fallback) {
                .value => |fallback| {
                    _ = try self.genValue(fallback, call_reg);
                    if (cc.is_success_boxed) {
                        return self.genBoxedCatchJoin(error_union.payload, value, call_reg, target_reg, jump_ok_idx);
                    }
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

        self.chunk_instructions.items[jump_ok_idx].putAEx(try self.currentPosition());
        try self.chunk_instructions.append(self.allocator, .{ .op = .LOAD, .a = target_reg, .b = call_reg });
        self.register_types[target_reg] = value.type_id;
    }

    fn genBoxedCatchJoin(self: *Self, payload: tt.TypeId, value: *const ir.Value, call_reg: u8, target_reg: u8, jump_ok_idx: usize) !void {
        try self.chunk_instructions.append(self.allocator, .{ .op = .JUMP });
        const jump_join_idx = self.chunk_instructions.items.len - 1;

        self.chunk_instructions.items[jump_ok_idx].putAEx(try self.currentPosition());
        self.register_types[call_reg] = payload;
        try self.chunk_instructions.append(self.allocator, .{
            .op = .NULL_BOX_INIT,
            .a = call_reg,
            .b = @intFromEnum(valueTypeFromTypeId(self.type_table, payload)),
            .c = call_reg,
        });
        try self.recordHeapMap();

        self.chunk_instructions.items[jump_join_idx].putAEx(try self.currentPosition());
        self.register_types[call_reg] = value.type_id;
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
                const field_kind = valueTypeFromTypeId(self.type_table, field_type_id);

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

        self.chunk_instructions.items[jump_idx].putBEx(try self.currentPosition());
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

        const init_inst = bc.Instruction{
            .op = .ARRAY_INIT,
            .a = target_reg,
            .b = @intFromEnum(valueTypeFromTypeId(self.type_table, array_inner_type_id)),
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

        const load_inst = bc.Instruction{
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

        const store_inst = bc.Instruction{
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

                    const store_inst = bc.Instruction{
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

                const store_inst = bc.Instruction{
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

        const append_inst = bc.Instruction{
            .op = .ARRAY_APPEND,
            .a = array_reg,
            .b = v_reg,
        };
        try self.chunk_instructions.append(self.allocator, append_inst);
        try self.recordHeapMap();
    }

    fn genStructInit(self: *Self, value: *const ir.Value, target_reg: u8) !void {
        const @"struct" = value.data.struct_init;
        const desc_id = try self.getOrCreateStructDescriptor(value);

        var struct_init = bc.Instruction{
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
        const struct_store = bc.Instruction{
            .op = .STRUCT_STORE,
            .a = struct_reg,
            .b = field_index,
            .c = try self.genValue(value, try self.consumeRegister()),
        };
        defer self.freeRegister();
        try self.chunk_instructions.append(self.allocator, struct_store);
    }

    fn genStructLoad(self: *Self, target_reg: u8, struct_reg: u8, field_index: u8) !void {
        const struct_load = bc.Instruction{
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

        var load_const = bc.Instruction{
            .op = .LOAD_CONST,
            .a = target_reg,
        };

        load_const.putBEx(@intCast(result.value_ptr.*));

        try self.chunk_instructions.append(self.allocator, load_const);
    }

    fn genUnwrapNullBox(self: *Self, stmt: ir.UnwrapNullBox, target_reg: u8) !u8 {
        const box_reg = try self.genValue(stmt.null_box, try self.consumeRegister());
        defer self.freeRegisterN(1);

        const unwrap_isnt = bc.Instruction{
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
                return self.err_reporter.tooManyStrings();
            }
            result.value_ptr.* = @intCast(self.string_texts.items.len);
            try self.string_texts.append(self.allocator, s);
        }

        const string_id = result.value_ptr.*;

        var load_string = bc.Instruction{
            .op = .LOAD_STRING,
            .a = target_reg,
        };

        load_string.putBEx(string_id);
        try self.chunk_instructions.append(self.allocator, load_string);
    }

    fn genBinOp(self: *Self, bo: ir.BinaryOp, target_reg: u8) !void {
        if (bo.kind == .b_and or bo.kind == .b_or) {
            return self.genShortCircuit(bo, target_reg);
        }

        const left_reg = try self.genValue(bo.left, try self.consumeRegister());
        const right_reg = try self.genValue(bo.right, try self.consumeRegister());
        defer self.freeRegisterN(2);

        const op = opCodeFromBinaryOp(bo.kind);

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
                    const left_to_float = bc.Instruction{
                        .op = .I_TO_F,
                        .a = left_reg,
                    };
                    try self.chunk_instructions.append(self.allocator, left_to_float);
                }

                if (right_is_int) {
                    const right_to_float = bc.Instruction{
                        .op = .I_TO_F,
                        .a = right_reg,
                    };
                    try self.chunk_instructions.append(self.allocator, right_to_float);
                }
            },
            .trunc_div => {
                if (!left_is_int) {
                    const left_to_int = bc.Instruction{
                        .op = .F_TO_I,
                        .a = left_reg,
                    };
                    try self.chunk_instructions.append(self.allocator, left_to_int);
                }

                if (!right_is_int) {
                    const right_to_int = bc.Instruction{
                        .op = .F_TO_I,
                        .a = right_reg,
                    };
                    try self.chunk_instructions.append(self.allocator, right_to_int);
                }
            },
            else => {},
        }

        const bo_inst = bc.Instruction{
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

    fn genShortCircuit(self: *Self, bo: ir.BinaryOp, target_reg: u8) !void {
        const saved_next_free = self.next_free_register;
        const result_reg = try self.consumeRegister();
        const boolean = self.type_table.getPrimitive(.boolean);

        _ = try self.genValue(bo.left, result_reg);
        self.register_types[result_reg] = boolean;
        const skip_right_idx = self.chunk_instructions.items.len;
        try self.chunk_instructions.append(self.allocator, .{ .op = .JUMP_IF_FALSE, .a = result_reg });

        var jump_to_end_idx: ?usize = null;
        if (bo.kind == .b_or) {
            jump_to_end_idx = self.chunk_instructions.items.len;
            try self.chunk_instructions.append(self.allocator, .{ .op = .JUMP });
            self.chunk_instructions.items[skip_right_idx].putBEx(try self.currentPosition());
        }

        _ = try self.genValue(bo.right, result_reg);
        self.register_types[result_reg] = boolean;

        const end_position = try self.currentPosition();
        if (jump_to_end_idx) |jump| {
            self.chunk_instructions.items[jump].putAEx(end_position);
        } else {
            self.chunk_instructions.items[skip_right_idx].putBEx(end_position);
        }

        try self.chunk_instructions.append(self.allocator, .{ .op = .LOAD, .a = target_reg, .b = result_reg });
        self.register_types[target_reg] = boolean;
        self.freeRegisterN(self.next_free_register - saved_next_free);
    }

    fn isIntegerType(self: *Self, type_id: tt.TypeId) bool {
        const kind = self.type_table.getTypePtrById(type_id).kind;
        return kind == .int or kind == .byte;
    }

    fn genUnaryOp(self: *Self, uo: ir.UnaryOp, target_reg: u8) !void {
        const aux_reg = try self.genValue(uo.right, try self.consumeRegister());
        defer self.freeRegister();

        const op = opCodeFromUnaryOp(uo.kind);
        const op_inst = bc.Instruction{
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

    fn currentPosition(self: *Self) BytecodeError!u16 {
        const position = self.chunk_instructions.items.len;
        if (position <= std.math.maxInt(u16)) return @intCast(position);
        return self.err_reporter.functionTooLong(self.current_function_name, self.current_function_loc);
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
        if (self.next_free_register == std.math.maxInt(u8)) return self.err_reporter.tooManyRegisters(self.current_function_name, self.current_function_loc);
        const id = self.next_free_register;
        self.next_free_register += 1;
        self.peak_register = @max(self.peak_register, self.next_free_register);
        return id;
    }

    fn computeHeapBitmap(self: *Self) u256 {
        var bitmap: u256 = 0;
        for (0..self.peak_register) |i| {
            const vt = valueTypeFromTypeId(self.type_table, self.register_types[i]);
            if (vt.isHeapType()) {
                bitmap |= @as(u256, 1) << @intCast(i);
            }
        }
        return bitmap;
    }

    fn recordHeapMap(self: *Self) !void {
        try self.recordHeapMapAt(try self.currentPosition() - 1);
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
            const vt = valueTypeFromTypeId(self.type_table, field.type_id);
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

        var init_inst = bc.Instruction{ .op = .UNION_INIT, .a = target_reg };
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
            const vt = valueTypeFromTypeId(self.type_table, union_def.variants.get(variant_name).?);
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
            .b = @intFromEnum(valueTypeFromTypeId(self.type_table, val.type_id)),
            .c = value_reg,
        });
        try self.recordHeapMap();

        // the ir value carries the inner type, but the register holds the box
        self.register_types[target_reg] = try self.type_table.getOrAddNullable(value.type_id);
    }
};

const BytecodeError = error{ OutOfMemory, UndefinedVariable, TooManyRegisters, TooManyStrings, FunctionTooLong };

const FailingCallHandler = union(enum) {
    try_call: ir.TryCall,
    catch_call: ir.CatchCall,
};

const LoopContext = struct {
    first_break_patch_index: usize,
    first_continue_patch_index: usize,
};

fn opCodeFromUnaryOp(op: ir.UnaryOpKind) bc.OpCode {
    return switch (op) {
        .box_not_null => .NULL_BOX_NNULL,
        .not => .B_NOT,
        .i_neg => .I_NEG,
        .f_neg => .F_NEG,
        .to_byte => .I_TO_BYTE,
    };
}

fn opCodeFromBinaryOp(op: ir.BinaryOpKind) bc.OpCode {
    return switch (op) {
        .i_add => .I_ADD,
        .i_sub => .I_SUB,
        .i_mult => .I_MULT,
        .i_mod => .I_MOD,

        .trunc_div => .TRUNC_DIV,

        .i_cmp_eq => .I_EQ,
        .i_cmp_neq => .I_NEQ,
        .i_cmp_lt => .I_LT,
        .i_cmp_le => .I_LE,
        .i_cmp_ge => .I_GE,
        .i_cmp_gt => .I_GT,

        .f_add => .F_ADD,
        .f_sub => .F_SUB,
        .f_mult => .F_MULT,
        .f_div => .F_DIV,
        .f_mod => .F_MOD,

        .f_cmp_eq => .F_EQ,
        .f_cmp_neq => .F_NEQ,
        .f_cmp_lt => .F_LT,
        .f_cmp_le => .F_LE,
        .f_cmp_ge => .F_GE,
        .f_cmp_gt => .F_GT,

        .b_and => .B_AND,
        .b_or => .B_OR,
        .b_cmp_eq => .B_EQ,
        .b_cmp_neq => .B_NEQ,
    };
}

fn typedValueFromIr(ir_value: *const ir.Value) v.TypedValue {
    return switch (ir_value.data) {
        .i_int => |i| v.TypedValue{ .value = .{ .int64 = i }, .type = .int64 },
        .i_float => |f| v.TypedValue{ .value = .{ .float64 = f }, .type = .float64 },
        .i_bool => |b| v.TypedValue{ .value = .{ .bool = b }, .type = .bool },
        .i_string => unreachable,
        .i_null, .i_void => v.TypedValue{ .value = v.NULL_VALUE, .type = .null },
        .i_array => v.TypedValue{ .value = undefined, .type = .array },
        else => unreachable,
    };
}

fn valueTypeFromTypeId(type_table: *tt.TypeTable, type_id: tt.TypeId) v.ValueType {
    const ty = type_table.getTypePtrById(type_id);

    if (ty.is_nullable) {
        return .nullable;
    }

    return switch (ty.kind) {
        .int, .byte => .int64,
        .float => .float64,
        .boolean => .bool,
        .string => .string,
        .buffer => .buffer,
        .array => .array,
        .@"struct" => .@"struct",
        .@"union" => .@"union",
        .func_type => .function,
        .task => .task,
        .cell => .cell,
        .@"enum" => .int64,
        //note: void -> null
        .void => .null,
        .null => .null,
        else => {
            std.debug.print("{}", .{ty});
            unreachable;
        },
    };
}
