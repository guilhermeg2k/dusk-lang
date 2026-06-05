pub const Generator = struct {
    const Self = @This();
    allocator: std.mem.Allocator,
    type_table: *TypeTable,

    pub fn generate(self: *Self, program: ir.Program) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;

        const builtin_functions = try self.genBuiltInFunctions();
        const functions = try self.genFunctions(program.functions);
        const instructions = try self.genInstructions(program.instructions);
        const structs = try self.genStructs(program.structs);

        try buf.appendSlice(self.allocator, builtin_functions);
        try buf.appendSlice(self.allocator, "\n");
        try buf.appendSlice(self.allocator, structs);
        try buf.appendSlice(self.allocator, "\n");
        try buf.appendSlice(self.allocator, functions);
        try buf.appendSlice(self.allocator, "\n");
        try buf.appendSlice(self.allocator, instructions);

        return buf.toOwnedSliceSentinel(self.allocator, 0);
    }

    fn genBuiltInFunctions(self: *Self) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;
        const builtin = try BuiltIn.init(self.allocator, self.type_table);

        for (try builtin.generate()) |func| {
            try buf.appendSlice(self.allocator, func.code);
        }

        return buf.toOwnedSlice(self.allocator);
    }

    fn genStructs(self: *Self, structs: []const ir.Struct) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;

        for (structs) |struct_| {
            const struct_fns = try self.genFunctions(struct_.funcs);
            try buf.appendSlice(self.allocator, struct_fns);

            const signature = try self.genStructConstructorSignature(struct_);
            const body = try self.genStructConstructorBody(struct_);
            const static_members = try self.genStructStaticMembers(struct_);

            try buf.appendSlice(self.allocator, signature);
            try buf.appendSlice(self.allocator, "{\n");
            try buf.appendSlice(self.allocator, body);
            try buf.appendSlice(self.allocator, "}\n");
            try buf.appendSlice(self.allocator, static_members);
        }

        return buf.toOwnedSlice(self.allocator);
    }

    fn genStructConstructorSignature(self: *Self, struct_: ir.Struct) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;
        const fn_name = try self.genName(struct_.uid, struct_.identifier);
        try buf.print(self.allocator, "function {s}(", .{fn_name});

        for (struct_.fields, 0..) |field, i| {
            if (i > 0) try buf.appendSlice(self.allocator, ", ");
            if (field.default_value) |default_value| {
                const value = try self.genValue(default_value);
                try buf.print(self.allocator, "{s} = {s}", .{ field.identifier, value });
            } else {
                try buf.appendSlice(self.allocator, field.identifier);
            }
        }

        try buf.appendSlice(self.allocator, ")");
        return buf.toOwnedSlice(self.allocator);
    }

    fn genStructConstructorBody(self: *Self, struct_: ir.Struct) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;
        try buf.appendSlice(self.allocator, "const obj = {");

        for (struct_.fields) |field| {
            try buf.print(self.allocator, "{s}: {s},\n", .{ field.identifier, field.identifier });
        }

        for (struct_.funcs) |func| {
            const fn_name = try self.genName(func.uid, func.identifier);
            try buf.print(self.allocator, "{s}: {s},\n", .{ func.identifier, fn_name });
        }

        try buf.appendSlice(self.allocator, "};\n");

        try buf.appendSlice(self.allocator, "return obj;\n");

        return buf.toOwnedSlice(self.allocator);
    }

    fn genStructStaticMembers(self: *Self, struct_: ir.Struct) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;
        const struct_fn_name = try self.genName(struct_.uid, struct_.identifier);

        for (struct_.static_fields) |field| {
            try buf.print(self.allocator, "{s}['{s}']", .{ struct_fn_name, field.identifier });
            if (field.default_value) |default_value| {
                const value = try self.genValue(default_value);
                try buf.print(self.allocator, "= {s}", .{value});
            }
            try buf.appendSlice(self.allocator, "\n");
        }

        for (struct_.funcs) |func| {
            const fn_name = try self.genName(func.uid, func.identifier);
            try buf.print(self.allocator, "{s}['{s}'] = {s}\n", .{ struct_fn_name, func.identifier, fn_name });
        }

        return buf.toOwnedSlice(self.allocator);
    }

    fn genFunctions(self: *Self, functions: []const ir.Func) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;

        for (functions) |func| {
            const fn_code = try self.genFunction(func);
            try buf.appendSlice(self.allocator, fn_code);
        }

        return buf.toOwnedSlice(self.allocator);
    }

    fn genFunction(self: *Self, func: ir.Func) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;
        const signature = try self.genFuncSignature(func);
        const body = try self.genInstructions(func.body);

        try buf.appendSlice(self.allocator, signature);
        try buf.appendSlice(self.allocator, "{\n");
        try buf.appendSlice(self.allocator, body);
        try buf.appendSlice(self.allocator, "}\n");

        return buf.toOwnedSlice(self.allocator);
    }

    fn genFuncSignature(self: *Self, func: ir.Func) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;

        const fn_name = try self.genName(func.uid, func.identifier);
        try buf.print(self.allocator, "function {s}(", .{fn_name});

        for (func.params, 0..) |arg, i| {
            if (i > 0) try buf.appendSlice(self.allocator, ", ");

            const arg_name = try self.genName(arg.uid, arg.identifier);
            try buf.appendSlice(self.allocator, arg_name);
            if (arg.default_value) |default_value| {
                const value = try self.genValue(default_value);
                try buf.print(self.allocator, "= {s}", .{value});
            }
        }

        try buf.appendSlice(self.allocator, ")");
        return buf.toOwnedSlice(self.allocator);
    }

    fn genInstructions(self: *Self, instructions: []const ir.Instruction) GeneratorError![]const u8 {
        var buf: std.ArrayList(u8) = .empty;

        for (instructions) |instruction| {
            switch (instruction) {
                .store_var => {
                    const str = try self.genStoreVar(instruction.store_var);
                    try buf.appendSlice(self.allocator, str);
                },
                .update_var => {
                    const str = try self.genUpdateVar(instruction.update_var);
                    try buf.appendSlice(self.allocator, str);
                },
                .update_indexed => {
                    const str = try self.genUpdateIndexed(instruction.update_indexed);
                    try buf.appendSlice(self.allocator, str);
                },
                .branch_if => {
                    const str = try self.genBranchIf(instruction.branch_if);
                    try buf.appendSlice(self.allocator, str);
                },
                .loop => {
                    const str = try self.genLoop(instruction.loop);
                    try buf.appendSlice(self.allocator, str);
                },
                .break_stmt => {
                    try buf.appendSlice(self.allocator, "break;");
                },
                .continue_stmt => {
                    try buf.appendSlice(self.allocator, "continue;");
                },
                .return_stmt => {
                    const str = try self.genReturn(instruction.return_stmt);
                    try buf.appendSlice(self.allocator, str);
                },
                .expression_stmt => {
                    const str = try self.genExpression(instruction.expression_stmt);
                    try buf.appendSlice(self.allocator, str);
                },
            }
        }

        return buf.toOwnedSlice(self.allocator);
    }

    fn genStoreVar(self: *Self, store_var: ir.StoreVar) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;

        const var_name = try self.genName(store_var.uid, store_var.identifier);
        const value = try self.genValue(store_var.value);

        try buf.print(self.allocator, "let {s} = {s};\n", .{ var_name, value });
        return buf.toOwnedSlice(self.allocator);
    }

    fn genUpdateVar(self: *Self, update_var: ir.UpdateVar) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;

        const var_name = try self.genName(update_var.var_uid, update_var.identifier);
        const value = try self.genValue(update_var.value);

        try buf.print(self.allocator, "{s} = {s};\n", .{ var_name, value });
        return buf.toOwnedSlice(self.allocator);
    }

    fn genUpdateIndexed(self: *Self, update_indexed: ir.UpdateIndexed) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;

        const target = try self.genValue(update_indexed.target);
        const value = try self.genValue(update_indexed.value);

        try buf.print(self.allocator, "{s} = {s};\n", .{ target, value });
        return buf.toOwnedSlice(self.allocator);
    }

    fn genBranchIf(self: *Self, branch_if: ir.BranchIf) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;

        const value = try self.genValue(branch_if.condition);
        const then_block = try self.genInstructions(branch_if.then_block);
        const else_block = try self.genInstructions(branch_if.else_block);

        try buf.print(self.allocator, "if ({s}){{\n", .{value});
        try buf.appendSlice(self.allocator, then_block);
        try buf.appendSlice(self.allocator, "}");

        if (else_block.len > 0) {
            try buf.appendSlice(self.allocator, "else {\n");
            try buf.appendSlice(self.allocator, else_block);
            try buf.appendSlice(self.allocator, "}\n");
        }

        return buf.toOwnedSlice(self.allocator);
    }

    fn genLoop(self: *Self, loop: ir.Loop) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;

        try buf.appendSlice(self.allocator, "while (");

        if (loop.condition) |condition| {
            const cond_str = try self.genValue(condition);
            try buf.appendSlice(self.allocator, cond_str);
        } else {
            try buf.appendSlice(self.allocator, "true");
        }

        try buf.appendSlice(self.allocator, "){\n");

        const block = try self.genInstructions(loop.do_block);
        try buf.appendSlice(self.allocator, block);
        try buf.appendSlice(self.allocator, "}\n");

        return buf.toOwnedSlice(self.allocator);
    }

    fn genReturn(self: *Self, return_stmt: ir.ReturnStmt) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;
        try buf.appendSlice(self.allocator, "return");

        if (return_stmt.value) |value| {
            try buf.appendSlice(self.allocator, " ");
            const val_str = try self.genValue(value);
            try buf.appendSlice(self.allocator, val_str);
        }

        try buf.appendSlice(self.allocator, ";\n");
        return buf.toOwnedSlice(self.allocator);
    }

    fn genExpression(self: *Self, exp_stmt: ir.ExpressionStmt) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;
        const val_str = try self.genValue(exp_stmt.value);

        try buf.appendSlice(self.allocator, val_str);
        try buf.appendSlice(self.allocator, ";\n");

        return buf.toOwnedSlice(self.allocator);
    }

    fn genValue(self: *Self, value: *ir.Value) GeneratorError![]const u8 {
        var buf: std.ArrayList(u8) = .empty;

        switch (value.data) {
            .i_float => |f| try buf.print(self.allocator, "{d}", .{f}),
            .i_int => |i| try buf.print(self.allocator, "{d}", .{i}),
            .i_bool => |b| try buf.print(self.allocator, "{s}", .{if (b) "true" else "false"}),
            .i_string => |s| try buf.print(self.allocator, "{s}", .{s}),
            .i_void => {},
            .i_null => try buf.appendSlice(self.allocator, "null"),
            .indexed => |idx| {
                const target = try self.genValue(idx.target);
                const is_nullable = self.type_table.getTypePtrById(idx.target.type_id).nullable;
                const optional_prefix = if (is_nullable) "?." else "";
                switch (idx.index.data) {
                    .i_string => |member_name| {
                        try buf.print(self.allocator, "{s}{s}[\"{s}\"]", .{ target, optional_prefix, member_name });
                    },
                    else => {
                        const index_js = try self.genValue(idx.index);
                        try buf.print(self.allocator, "{s}{s}[{s}]", .{ target, optional_prefix, index_js });
                    },
                }
            },
            .i_array => |a| {
                const i_array = try self.genImmediateArray(a);
                try buf.appendSlice(self.allocator, i_array);
            },
            .identifier => |id| {
                const name = try self.genName(id.uid, id.identifier);
                try buf.appendSlice(self.allocator, name);
            },
            .fn_call => |fc| {
                const call = try self.genFnCall(fc);
                try buf.appendSlice(self.allocator, call);
            },
            .struct_init => |si| {
                const struct_obj = try self.genStructInit(si);
                try buf.appendSlice(self.allocator, struct_obj);
            },
            .struct_fn_call => |sfc| {
                const call = try self.genStructFnCall(sfc);
                try buf.appendSlice(self.allocator, call);
            },
            .binary_op => |bo| {
                const op = try self.genBinaryOp(bo);
                try buf.appendSlice(self.allocator, op);
            },
            .unary_op => |uo| {
                const op = try self.genUnaryOp(uo);
                try buf.appendSlice(self.allocator, op);
            },
        }

        return buf.toOwnedSlice(self.allocator);
    }

    fn genBinaryOp(self: *Self, binaryOp: ir.BinaryOp) ![]const u8 {
        const left = try self.genValue(binaryOp.left);
        const right = try self.genValue(binaryOp.right);
        const op = self.genBinaryOpSymbol(binaryOp.kind);

        if (binaryOp.kind == .i_div) {
            return std.fmt.allocPrint(self.allocator, "(Math.trunc({s} / {s}))", .{ left, right });
        }

        return std.fmt.allocPrint(self.allocator, "({s} {s} {s})", .{ left, op, right });
    }

    fn genBinaryOpSymbol(_: *Self, op: ir.BinaryOpKind) []const u8 {
        return switch (op) {
            .i_add, .f_add => "+",
            .i_sub, .f_sub => "-",
            .i_mult, .f_mult => "*",
            .i_div, .f_div => "/",
            .i_mod, .f_mod => "%",

            .i_cmp_eq, .f_cmp_eq, .b_cmp_eq => "===",
            .i_cmp_neq, .f_cmp_neq, .b_cmp_neq => "!==",
            .i_cmp_lt, .f_cmp_lt => "<",
            .i_cmp_le, .f_cmp_le => "<=",
            .i_cmp_ge, .f_cmp_ge => ">=",
            .i_cmp_gt, .f_cmp_gt => ">",

            .b_and => "&&",
            .b_or => "||",
        };
    }

    fn genImmediateArray(self: *Self, array: ir.Array) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;

        try buf.append(self.allocator, '[');
        for (array.values, 0..) |v, i| {
            if (i > 0) try buf.append(self.allocator, ',');
            try buf.appendSlice(self.allocator, try self.genValue(v));
        }

        try buf.append(self.allocator, ']');
        return buf.toOwnedSlice(self.allocator);
    }

    fn genUnaryOp(self: *Self, unaryOp: ir.UnaryOp) ![]const u8 {
        const right = try self.genValue(unaryOp.right);
        const op = self.genUnaryOpSymbol(unaryOp.kind);

        return std.fmt.allocPrint(self.allocator, "({s}{s})", .{ op, right });
    }

    fn genUnaryOpSymbol(_: *Self, op: ir.UnaryOpKind) []const u8 {
        return switch (op) {
            .f_neg, .i_neg => "-",
            .not => "!",
        };
    }

    fn genFnCall(self: *Self, fnCall: ir.FnCall) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;
        const fn_name = try self.genName(fnCall.fn_uid, fnCall.identifier);

        try buf.print(self.allocator, "{s}(", .{fn_name});

        for (fnCall.args, 0..) |arg, i| {
            if (i > 0) try buf.append(self.allocator, ',');
            const arg_value = try self.genValue(arg);
            try buf.appendSlice(self.allocator, arg_value);
        }

        try buf.appendSlice(self.allocator, ")");

        return buf.toOwnedSlice(self.allocator);
    }

    fn genStructInit(self: *Self, struct_init: ir.StructInit) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;
        try buf.appendSlice(self.allocator, "{");

        for (struct_init.keys, 0..) |key, index| {
            try buf.print(self.allocator, "{s}: {s},\n", .{ key, try self.genValue(struct_init.values[index]) });
        }

        try buf.appendSlice(self.allocator, "}\n");
        return buf.toOwnedSlice(self.allocator);
    }

    fn genStructFnCall(self: *Self, fnCall: ir.StructFnCall) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;
        const target = try self.genValue(fnCall.target);

        try buf.print(self.allocator, "{s}.{s}(", .{ target, fnCall.identifier });

        for (fnCall.args, 0..) |arg, i| {
            if (i > 0) try buf.append(self.allocator, ',');
            const arg_value = try self.genValue(arg);
            try buf.appendSlice(self.allocator, arg_value);
        }

        try buf.appendSlice(self.allocator, ")");

        return buf.toOwnedSlice(self.allocator);
    }

    pub fn genName(self: *Self, uid: usize, identifier: []const u8) ![]const u8 {
        return std.fmt.allocPrint(self.allocator, "{s}_{d}", .{ identifier, uid });
    }
};

const GeneratorError = error{OutOfMemory};

const ir = @import("ir.zig");
const BuiltIn = @import("built-in.zig").BuiltIn;
const TypeTable = @import("sema.zig").TypeTable;
const std = @import("std");
