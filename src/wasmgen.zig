const binaryen = @import("binaryen.zig");
const B = binaryen.binaryen;

const std = @import("std");
const ir = @import("ir.zig");
const sema = @import("sema.zig");
const wasm_builtins = @import("wasm_builtins.zig");
const TypeTable = sema.TypeTable;
const TypeId = sema.TypeId;
const WasmGenError = error{OutOfMemory};

pub const WasmGenerator = struct {
    const Self = @This();

    const LocalVar = struct { index: u32, wasm_type: B.BinaryenType };
    const LoopLabels = struct { exit: []const u8, @"continue": []const u8 };

    allocator: std.mem.Allocator,
    type_table: *TypeTable,
    module: B.BinaryenModuleRef,
    wasm_builtins: wasm_builtins.WasmBuiltins,

    local_vars_by_symbol_uid: std.AutoHashMap(usize, LocalVar),
    next_local_index: u32,

    loop_labels: std.ArrayList(LoopLabels),
    next_label_id: u32,

    array_types: std.AutoHashMap(TypeId, B.BinaryenHeapType),

    pub fn generate(allocator: std.mem.Allocator, type_table: *TypeTable, program: ir.Program) ![]const u8 {
        const module = B.BinaryenModuleCreate();
        errdefer B.BinaryenModuleDispose(module);

        B.BinaryenModuleSetFeatures(module, B.BinaryenFeatureReferenceTypes() | B.BinaryenFeatureGC() | B.BinaryenFeatureStrings());
        B.BinaryenSetMemory(module, 1, 1, "memory", null, null, null, null, null, 0, false, false, "memory");

        var wasm_builtins_instance = try wasm_builtins.WasmBuiltins.init(allocator, null);
        defer wasm_builtins_instance.deinit();

        var self = Self{
            .allocator = allocator,
            .type_table = type_table,
            .module = module,
            .wasm_builtins = wasm_builtins_instance,
            .local_vars_by_symbol_uid = std.AutoHashMap(usize, LocalVar).init(allocator),
            .next_local_index = 0,
            .loop_labels = .empty,
            .next_label_id = 0,
            .array_types = std.AutoHashMap(TypeId, B.BinaryenHeapType).init(allocator),
        };

        try self.setupBuiltIns();

        try self.genFunctions(program.functions);

        if (program.instructions.len > 0) {
            const main_fn = try self.genFunction(.{
                .uid = 0,
                .identifier = "$main",
                .body = program.instructions,
                .params = &.{},
                .return_type = type_table.getPrimitive(.void),
            });
            B.BinaryenSetStart(module, main_fn);
        }

        if (!B.BinaryenModuleValidate(module)) {
            std.log.err("WASM validation failed", .{});
            return error.WasmValidationFailed;
        }

        const result = B.BinaryenModuleAllocateAndWrite(module, null);
        defer std.c.free(result.binary);
        errdefer if (result.sourceMap) |sm| std.c.free(sm);

        const output = try allocator.alloc(u8, result.binaryBytes);
        @memcpy(output, @as([*]u8, @ptrCast(result.binary))[0..result.binaryBytes]);

        return output;
    }

    fn genFunctions(self: *Self, funcs: []const ir.Func) !void {
        for (funcs) |func| {
            _ = try self.genFunction(func);
        }
    }

    fn setupBuiltIns(self: *Self) !void {
        for (self.wasm_builtins.builtins) |def| {
            switch (def.kind) {
                .host_import => |host| {
                    const params_type = self.typeIdsToWasmType(host.params);
                    const results_type = self.typeIdsToWasmType(host.results);
                    const name = try self.genName(def.uid, def.identifier);
                    _ = B.BinaryenAddFunctionImport(self.module, name.ptr, "env", name.ptr, params_type, results_type);
                },
                .host_import_raw => |raw| {
                    _ = B.BinaryenAddFunctionImport(self.module, def.identifier.ptr, "env", def.identifier.ptr, raw.params, raw.results);
                },
                .inline_expr => {},
            }
        }
    }

    fn genFunction(self: *Self, func: ir.Func) !B.BinaryenFunctionRef {
        self.local_vars_by_symbol_uid.clearRetainingCapacity();
        self.next_local_index = 0;

        for (func.params) |param| {
            try self.local_vars_by_symbol_uid.put(param.uid, .{
                .index = self.next_local_index,
                .wasm_type = self.duskTypeToWasmType(param.type_id),
            });
            self.next_local_index += 1;
        }

        try self.collectAllLocalVarsFromInstructions(func.body);

        const num_vars = self.next_local_index - func.params.len;

        var var_types = try std.ArrayList(B.BinaryenType).initCapacity(self.allocator, num_vars);
        if (num_vars > 0) {
            try var_types.resize(self.allocator, num_vars);
            for (var_types.items) |*t| t.* = B.BinaryenTypeNone();
            var iter = self.local_vars_by_symbol_uid.iterator();
            while (iter.next()) |entry| {
                if (entry.value_ptr.index >= func.params.len) {
                    const var_idx = entry.value_ptr.index - func.params.len;
                    var_types.items[var_idx] = entry.value_ptr.wasm_type;
                }
            }
        }

        const body = try self.genInstructions(func.body);

        const fn_name = try self.genName(func.uid, func.identifier);
        const param_types = try self.funcParamsToWasmType(func.params);
        const result_type = self.duskTypeToWasmType(func.return_type);
        const var_types_ptr = if (var_types.items.len > 0) var_types.items.ptr else null;

        return B.BinaryenAddFunction(
            self.module,
            fn_name.ptr,
            param_types,
            result_type,
            var_types_ptr,
            @intCast(var_types.items.len),
            body,
        );
    }

    fn collectAllLocalVarsFromInstructions(self: *Self, instructions: []const ir.Instruction) !void {
        for (instructions) |inst| {
            switch (inst) {
                .store_var => |sv| {
                    try self.local_vars_by_symbol_uid.put(sv.uid, .{
                        .index = self.next_local_index,
                        .wasm_type = self.duskTypeToWasmType(sv.type_id),
                    });
                    self.next_local_index += 1;
                },
                .branch_if => |bi| {
                    try self.collectAllLocalVarsFromInstructions(bi.then_block);
                    try self.collectAllLocalVarsFromInstructions(bi.else_block);
                },
                .loop => |lp| {
                    try self.collectAllLocalVarsFromInstructions(lp.do_block);
                },
                else => {},
            }
        }
    }

    fn genInstructions(self: *Self, instructions: []const ir.Instruction) !B.BinaryenExpressionRef {
        if (instructions.len == 0) {
            return B.BinaryenNop(self.module);
        }

        if (instructions.len == 1) {
            return self.genInstruction(instructions[0]);
        }

        const children = try self.allocator.alloc(B.BinaryenExpressionRef, instructions.len);

        for (instructions, 0..) |inst, i| {
            children[i] = try self.genInstruction(inst);
        }
        for (children[0 .. children.len - 1]) |*child| {
            const child_type = B.BinaryenExpressionGetType(child.*);
            if (child_type != B.BinaryenTypeNone() and child_type != B.BinaryenTypeUnreachable()) {
                child.* = B.BinaryenDrop(self.module, child.*);
            }
        }

        return B.BinaryenBlock(self.module, null, children.ptr, @intCast(children.len), B.BinaryenTypeAuto());
    }

    fn genInstruction(self: *Self, inst: ir.Instruction) WasmGenError!B.BinaryenExpressionRef {
        switch (inst) {
            .store_var => |sv| {
                const value = try self.genValue(sv.value);
                const idx = self.local_vars_by_symbol_uid.get(sv.uid).?.index;
                return B.BinaryenLocalSet(self.module, idx, value);
            },
            .update_var => |uv| {
                const value = try self.genValue(uv.value);
                const idx = self.local_vars_by_symbol_uid.get(uv.var_uid).?.index;
                return B.BinaryenLocalSet(self.module, idx, value);
            },
            .expression_stmt => |es| {
                return self.genValue(es.value);
            },
            .branch_if => |bi| {
                const cond = try self.genValue(bi.condition);
                const then_body = try self.genInstructions(bi.then_block);
                const else_body = try self.genInstructions(bi.else_block);
                if (bi.else_block.len == 0) {
                    const then_type = B.BinaryenExpressionGetType(then_body);
                    if (then_type != B.BinaryenTypeNone() and then_type != B.BinaryenTypeUnreachable()) {
                        return B.BinaryenIf(self.module, cond, B.BinaryenDrop(self.module, then_body), else_body);
                    }
                }
                return B.BinaryenIf(self.module, cond, then_body, else_body);
            },
            .loop => |lp| {
                return self.genLoop(lp);
            },
            .break_stmt => {
                const labels = self.getCurrentLoopLabels();
                return B.BinaryenBreak(self.module, @ptrCast(labels.exit.ptr), null, null);
            },
            .continue_stmt => {
                const labels = self.getCurrentLoopLabels();
                return B.BinaryenBreak(self.module, @ptrCast(labels.@"continue".ptr), null, null);
            },
            .return_stmt => |rs| {
                const return_value = if (rs.value) |val| try self.genValue(val) else null;
                return B.BinaryenReturn(self.module, return_value);
            },
            .update_indexed => |ui| {
                return self.genIndexedSet(ui);
            },
        }
    }

    fn genLoop(self: *Self, lp: ir.Loop) !B.BinaryenExpressionRef {
        const labels = try self.pushLoopLabels();
        defer self.popLoopLabels();

        const loop_body = try self.genLoopBody(lp);
        const loop_expr = B.BinaryenLoop(self.module, labels.@"continue".ptr, loop_body);

        var exit = [_]B.BinaryenExpressionRef{loop_expr};
        return B.BinaryenBlock(self.module, labels.exit.ptr, &exit, 1, B.BinaryenTypeAuto());
    }

    fn genLoopBody(self: *Self, lp: ir.Loop) !B.BinaryenExpressionRef {
        const labels = self.getCurrentLoopLabels();
        const body = try self.genInstructions(lp.do_block);
        const continue_br = B.BinaryenBreak(self.module, labels.@"continue".ptr, null, null);

        if (lp.condition) |cond| {
            const cond_val = try self.genValue(cond);
            const not_cond = B.BinaryenUnary(self.module, B.BinaryenEqZInt32(), cond_val);
            const cond_break = B.BinaryenBreak(self.module, labels.exit.ptr, not_cond, null);

            var children = try self.allocator.alloc(B.BinaryenExpressionRef, 3);
            children[0] = cond_break;
            children[1] = body;
            children[2] = continue_br;
            return B.BinaryenBlock(self.module, null, children.ptr, 3, B.BinaryenTypeAuto());
        }

        var children = try self.allocator.alloc(B.BinaryenExpressionRef, 2);
        children[0] = body;
        children[1] = continue_br;
        return B.BinaryenBlock(self.module, null, children.ptr, 2, B.BinaryenTypeAuto());
    }

    fn genValue(self: *Self, value: *ir.Value) WasmGenError!B.BinaryenExpressionRef {
        switch (value.data) {
            .i_int => |i| {
                const lit = B.BinaryenLiteralInt64(i);
                return B.BinaryenConst(self.module, lit);
            },
            .i_float => |f| {
                const lit = B.BinaryenLiteralFloat64(f);
                return B.BinaryenConst(self.module, lit);
            },
            .i_bool => |b| {
                const lit = B.BinaryenLiteralInt32(if (b) @as(i32, 1) else 0);
                return B.BinaryenConst(self.module, lit);
            },
            .identifier => |id| {
                const local = self.local_vars_by_symbol_uid.get(id.uid).?;
                return B.BinaryenLocalGet(self.module, local.index, local.wasm_type);
            },
            .binary_op => |bo| {
                return self.genBinaryOp(bo);
            },
            .unary_op => |uo| {
                return self.genUnaryOp(uo);
            },
            .fn_call => |fc| {
                return self.genFnCall(fc, value.type_id);
            },
            .i_void => return B.BinaryenNop(self.module),
            .i_null => {
                const type_ptr = self.type_table.getTypePtrById(value.type_id);
                return switch (type_ptr.kind) {
                    .string, .array => B.BinaryenRefNull(self.module, self.duskTypeToWasmType(value.type_id)),
                    else => B.BinaryenNop(self.module),
                };
            },
            .i_string => |s| {
                const null_terminated = try self.allocator.allocSentinel(u8, s.len, 0);
                @memcpy(null_terminated[0..s.len], s);
                return B.BinaryenStringConst(self.module, null_terminated.ptr);
            },
            .i_array => |a| {
                return self.genArrayLiteral(value.type_id, a);
            },
            .indexed => |idx| {
                return self.genIndexedGet(idx);
            },
            else => unreachable,
        }
    }

    fn genBinaryOp(self: *Self, bo: ir.BinaryOp) !B.BinaryenExpressionRef {
        var left = try self.genValue(bo.left);
        var right = try self.genValue(bo.right);

        if (self.isFloatOpKind(bo.kind)) {
            const int_type = self.type_table.getPrimitive(.int);
            if (bo.left.type_id == int_type) {
                left = B.BinaryenUnary(self.module, B.BinaryenConvertSInt64ToFloat64(), left);
            }
            if (bo.right.type_id == int_type) {
                right = B.BinaryenUnary(self.module, B.BinaryenConvertSInt64ToFloat64(), right);
            }
        }

        if (bo.kind == .f_mod) {
            return self.genFloatMod(left, right);
        }

        const op = self.genBinaryOpSymbol(bo.kind);
        return B.BinaryenBinary(self.module, op, left, right);
    }

    fn isFloatOpKind(_: *Self, op: ir.BinaryOpKind) bool {
        return switch (op) {
            .f_add, .f_sub, .f_mult, .f_div, .f_mod, .f_cmp_eq, .f_cmp_neq, .f_cmp_lt, .f_cmp_le, .f_cmp_ge, .f_cmp_gt => true,
            else => false,
        };
    }

    fn genFloatMod(self: *Self, left: B.BinaryenExpressionRef, right: B.BinaryenExpressionRef) !B.BinaryenExpressionRef {
        const div = B.BinaryenBinary(self.module, B.BinaryenDivFloat64(), left, right);
        const zero = B.BinaryenConst(self.module, B.BinaryenLiteralFloat64(0.0));
        const non_negative = B.BinaryenBinary(self.module, B.BinaryenGtFloat64(), div, zero);
        const floor_x = B.BinaryenUnary(self.module, B.BinaryenFloorFloat64(), div);
        const ceil_x = B.BinaryenUnary(self.module, B.BinaryenCeilFloat64(), div);
        const truncated = B.BinaryenSelect(self.module, non_negative, floor_x, ceil_x);
        const mul = B.BinaryenBinary(self.module, B.BinaryenMulFloat64(), right, truncated);
        return B.BinaryenBinary(self.module, B.BinaryenSubFloat64(), left, mul);
    }

    fn genBinaryOpSymbol(_: *Self, op: ir.BinaryOpKind) B.BinaryenOp {
        return switch (op) {
            .i_add => B.BinaryenAddInt64(),
            .i_sub => B.BinaryenSubInt64(),
            .i_mult => B.BinaryenMulInt64(),
            .i_div => B.BinaryenDivSInt64(),
            .i_mod => B.BinaryenRemSInt64(),
            .i_cmp_eq => B.BinaryenEqInt64(),
            .i_cmp_neq => B.BinaryenNeInt64(),
            .i_cmp_lt => B.BinaryenLtSInt64(),
            .i_cmp_le => B.BinaryenLeSInt64(),
            .i_cmp_ge => B.BinaryenGeSInt64(),
            .i_cmp_gt => B.BinaryenGtSInt64(),
            .f_add => B.BinaryenAddFloat64(),
            .f_sub => B.BinaryenSubFloat64(),
            .f_mult => B.BinaryenMulFloat64(),
            .f_div => B.BinaryenDivFloat64(),
            .f_mod => unreachable,
            .f_cmp_eq => B.BinaryenEqFloat64(),
            .f_cmp_neq => B.BinaryenNeFloat64(),
            .f_cmp_lt => B.BinaryenLtFloat64(),
            .f_cmp_le => B.BinaryenLeFloat64(),
            .f_cmp_ge => B.BinaryenGeFloat64(),
            .f_cmp_gt => B.BinaryenGtFloat64(),
            .b_and => B.BinaryenAndInt32(),
            .b_or => B.BinaryenOrInt32(),
            .b_cmp_eq => B.BinaryenEqInt32(),
            .b_cmp_neq => B.BinaryenNeInt32(),
        };
    }

    fn genUnaryOp(self: *Self, uo: ir.UnaryOp) !B.BinaryenExpressionRef {
        const right = try self.genValue(uo.right);
        return switch (uo.kind) {
            .i_neg => {
                const zero = B.BinaryenLiteralInt64(0);
                const zero_expr = B.BinaryenConst(self.module, zero);
                return B.BinaryenBinary(self.module, B.BinaryenSubInt64(), zero_expr, right);
            },
            .f_neg => B.BinaryenUnary(self.module, B.BinaryenNegFloat64(), right),
            .not => B.BinaryenUnary(self.module, B.BinaryenEqZInt32(), right),
        };
    }

    fn genFnCall(self: *Self, fc: ir.FnCall, return_type_id: TypeId) !B.BinaryenExpressionRef {
        if (self.wasm_builtins.getBuiltin(fc.fn_uid)) |def| {
            if (def.kind == .inline_expr) return self.genBuiltInExpr(fc);
        }

        const args = try self.allocator.alloc(B.BinaryenExpressionRef, fc.args.len);
        for (fc.args, 0..) |arg, i| {
            args[i] = try self.genValue(arg);
        }
        const name = try self.genName(fc.fn_uid, fc.identifier);
        const return_type = self.duskTypeToWasmType(return_type_id);
        return B.BinaryenCall(self.module, name.ptr, args.ptr, @intCast(args.len), return_type);
    }

    fn genBuiltInExpr(self: *Self, fc: ir.FnCall) WasmGenError!B.BinaryenExpressionRef {
        const def = self.wasm_builtins.getBuiltin(fc.fn_uid).?;
        const gen = def.kind.inline_expr;
        const gen_value_fn = struct {
            fn call(ctx: *anyopaque, value: *ir.Value) anyerror!B.BinaryenExpressionRef {
                const s: *Self = @ptrCast(@alignCast(ctx));
                return s.genValue(value);
            }
        }.call;
        return gen(self.module, self.allocator, self.type_table, fc, self, gen_value_fn) catch return error.OutOfMemory;
    }

    fn funcParamsToWasmType(self: *Self, params: []const ir.FuncParam) !B.BinaryenType {
        return switch (params.len) {
            0 => B.BinaryenTypeNone(),
            1 => self.duskTypeToWasmType(params[0].type_id),
            else => {
                const types = try self.allocator.alloc(B.BinaryenType, params.len);
                for (params, 0..) |param, i| {
                    types[i] = self.duskTypeToWasmType(param.type_id);
                }
                return B.BinaryenTypeCreate(types.ptr, @intCast(types.len));
            },
        };
    }

    fn duskTypeToWasmType(self: *Self, type_id: TypeId) B.BinaryenType {
        const type_ptr = self.type_table.getTypePtrById(type_id);
        return switch (type_ptr.kind) {
            .int => B.BinaryenTypeInt64(),
            .float => B.BinaryenTypeFloat64(),
            .boolean => B.BinaryenTypeInt32(),
            .void => B.BinaryenTypeNone(),
            .string => B.BinaryenTypeStringref(),
            .array => |inner_id| {
                const heap_type = self.getOrPutArrayType(inner_id);
                return B.BinaryenTypeFromHeapType(heap_type, type_ptr.nullable);
            },
            else => B.BinaryenTypeInt64(),
        };
    }

    fn getOrPutArrayType(self: *Self, inner_type_id: TypeId) B.BinaryenHeapType {
        const cached = self.array_types.get(inner_type_id);
        if (cached) |h| return h;

        const inner_wasm = self.duskTypeToWasmType(inner_type_id);
        const builder = B.TypeBuilderCreate(1);
        B.TypeBuilderSetArrayType(builder, 0, inner_wasm, B.BinaryenPackedTypeNotPacked(), 1);
        _ = B.TypeBuilderGetTempHeapType(builder, 0);

        var error_index: u32 = 0;
        var error_reason: u32 = 0;
        var built_type: B.BinaryenHeapType = undefined;
        if (!B.TypeBuilderBuildAndDispose(builder, &built_type, &error_index, &error_reason)) {
            @panic("TypeBuilderBuildAndDispose failed");
        }

        self.array_types.put(inner_type_id, built_type) catch unreachable;
        return built_type;
    }

    fn genArrayLiteral(self: *Self, type_id: TypeId, a: ir.Array) WasmGenError!B.BinaryenExpressionRef {
        const inner_type = self.type_table.getTypePtrById(type_id).kind.array;
        const heap_type = self.getOrPutArrayType(inner_type);
        const values = try self.allocator.alloc(B.BinaryenExpressionRef, a.values.len);
        for (a.values, 0..) |v, i| {
            values[i] = try self.genValue(v);
        }
        return B.BinaryenArrayNewFixed(self.module, heap_type, values.ptr, @intCast(values.len));
    }

    fn genIndexedGet(self: *Self, indexed: ir.IndexedValue) WasmGenError!B.BinaryenExpressionRef {
        const target_type = self.type_table.getTypePtrById(indexed.target.type_id);
        return switch (target_type.kind) {
            .array => |inner_id| {
                const target = try self.genValue(indexed.target);
                const index_i64 = try self.genValue(indexed.index);
                const index_i32 = B.BinaryenUnary(self.module, B.BinaryenWrapInt64(), index_i64);
                const elem_wasm = self.duskTypeToWasmType(inner_id);
                return B.BinaryenArrayGet(self.module, target, index_i32, elem_wasm, false);
            },
            else => unreachable,
        };
    }

    fn genIndexedSet(self: *Self, updateIndexed: ir.UpdateIndexed) WasmGenError!B.BinaryenExpressionRef {
        const inner_indexed = updateIndexed.target.data.indexed;
        const array_ref = try self.genValue(inner_indexed.target);
        const value = try self.genValue(updateIndexed.value);
        const index_i64 = try self.genValue(inner_indexed.index);
        const index_i32 = B.BinaryenUnary(self.module, B.BinaryenWrapInt64(), index_i64);
        return B.BinaryenArraySet(self.module, array_ref, index_i32, value);
    }

    fn getCurrentLoopLabels(self: *Self) LoopLabels {
        return self.loop_labels.items[self.loop_labels.items.len - 1];
    }

    fn pushLoopLabels(self: *Self) !LoopLabels {
        const id = self.next_label_id;
        self.next_label_id += 1;

        const exit_name = try std.fmt.allocPrintSentinel(self.allocator, "exit_{d}", .{id}, 0);
        const cont_name = try std.fmt.allocPrintSentinel(self.allocator, "loop_{d}", .{id}, 0);

        const labels = LoopLabels{ .exit = exit_name, .@"continue" = cont_name };
        try self.loop_labels.append(self.allocator, labels);
        return labels;
    }

    fn popLoopLabels(self: *Self) void {
        _ = self.loop_labels.pop();
    }

    fn genName(self: *Self, uid: usize, identifier: []const u8) ![]const u8 {
        return std.fmt.allocPrintSentinel(
            self.allocator,
            "{s}_{d}",
            .{ identifier, uid },
            0,
        );
    }

    fn typeIdsToWasmType(self: *Self, type_ids: []const TypeId) B.BinaryenType {
        return switch (type_ids.len) {
            0 => B.BinaryenTypeNone(),
            1 => self.duskTypeToWasmType(type_ids[0]),
            else => {
                const types = self.allocator.alloc(B.BinaryenType, type_ids.len) catch unreachable;
                for (type_ids, 0..) |tid, i| {
                    types[i] = self.duskTypeToWasmType(tid);
                }
                return B.BinaryenTypeCreate(types.ptr, @intCast(types.len));
            },
        };
    }
};
