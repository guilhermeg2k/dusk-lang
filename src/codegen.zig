pub const Generator = struct {
    const Self = @This();
    allocator: std.mem.Allocator,

    pub fn generate(self: *Self, program: ir.Program) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;

        const functions = try self.genFunctions(program.functions);
        const instructions = try self.genInstructions(program.instructions);
        const structs = try self.genStructs(program.structs);
        try buf.appendSlice(self.allocator, structs);
        try buf.appendSlice(self.allocator, "\n");
        try buf.appendSlice(self.allocator, functions);
        try buf.appendSlice(self.allocator, "\n");
        try buf.appendSlice(self.allocator, instructions);

        return buf.toOwnedSliceSentinel(self.allocator, 0);
    }

    fn genStructs(self: *Self, structs: []const ir.Struct) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;

        for (structs) |struct_| {
            const signature = try self.genStructConstructorSignature(struct_);
            const body = try self.genStructConstructorBody(struct_);
            try buf.appendSlice(self.allocator, signature);
            try buf.appendSlice(self.allocator, "{\n");
            try buf.appendSlice(self.allocator, body);
            try buf.appendSlice(self.allocator, "}\n");

            const struct_fns = try self.genFunctions(struct_.funcs);
            try buf.appendSlice(self.allocator, struct_fns);
        }

        return buf.toOwnedSlice(self.allocator);
    }

    fn genStructConstructorSignature(self: *Self, struct_: ir.Struct) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;
        const fn_name = try self.genName(struct_.uid, struct_.identifier);
        try buf.print(self.allocator, "function {s}(", .{fn_name});

        for (struct_.fields, 0..) |field, i| {
            if (i > 0) try buf.appendSlice(self.allocator, ", ");
            try buf.appendSlice(self.allocator, field.identifier);
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
        try buf.appendSlice(self.allocator, "};");

        try buf.appendSlice(self.allocator, "return obj;");

        return buf.toOwnedSlice(self.allocator);
    }

    fn genFunctions(self: *Self, functions: []const ir.Func) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;
        const builtin = BuiltIn{ .alloc = self.allocator };
        for (try builtin.generate()) |func| {
            try buf.appendSlice(self.allocator, func.code);
        }

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

        const id = update_indexed.target.identifier;
        const target_name = try self.genName(id.uid, id.identifier);

        const value = try self.genValue(update_indexed.value);
        const index = try self.genValue(update_indexed.index);

        try buf.print(self.allocator, "{s}[{s}] = {s};\n", .{ target_name, index, value });
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

        switch (value.*) {
            .i_float => try buf.print(self.allocator, "{d}", .{value.i_float}),
            .i_bool => try buf.print(self.allocator, "{s}", .{if (value.i_bool) "true" else "false"}),
            .i_string => try buf.print(self.allocator, "{s}", .{value.i_string}),
            .i_void => {},
            .indexed => {
                const target = try self.genValue(value.indexed.target);
                switch (value.indexed.index.*) {
                    .i_string => |member_name| {
                        try buf.print(self.allocator, "{s}[\"{s}\"]", .{ target, member_name });
                    },
                    else => {
                        const index_js = try self.genValue(value.indexed.index);
                        try buf.print(self.allocator, "{s}[{s}]", .{ target, index_js });
                    },
                }
            },
            .i_array => {
                const i_array = try self.genImmediateArray(value.i_array);
                try buf.appendSlice(self.allocator, i_array);
            },
            .identifier => {
                const name = try self.genName(value.identifier.uid, value.identifier.identifier);
                try buf.appendSlice(self.allocator, name);
            },

            .fn_call => {
                const call = try self.genFnCall(value.fn_call);
                try buf.appendSlice(self.allocator, call);
            },

            .binary_op => {
                const op = try self.genBinaryOp(value.binary_op);
                try buf.appendSlice(self.allocator, op);
            },

            .unary_op => {
                const op = try self.genUnaryOp(value.unary_op);
                try buf.appendSlice(self.allocator, op);
            },

            .struct_def => {},
            .fn_def => {},
        }

        return buf.toOwnedSlice(self.allocator);
    }

    fn genBinaryOp(self: *Self, binaryOp: ir.BinaryOp) ![]const u8 {
        const left = try self.genValue(binaryOp.left);
        const right = try self.genValue(binaryOp.right);
        const op = self.genBinaryOpSymbol(binaryOp.kind);

        return std.fmt.allocPrint(self.allocator, "({s} {s} {s})", .{ left, op, right });
    }

    fn genBinaryOpSymbol(_: *Self, op: ir.BinaryOpKind) []const u8 {
        return switch (op) {
            .add => "+",
            .sub => "-",
            .mult => "*",
            .div => "/",
            .mod => "%",

            .cmp_eq => "===",
            .cmp_neq => "!==",
            .cmp_lt => "<",
            .cmp_le => "<=",
            .cmp_ge => ">=",
            .cmp_gt => ">",

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
            .neg => "-",
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

    pub fn genName(self: *Self, uid: usize, identifier: []const u8) ![]const u8 {
        return std.fmt.allocPrint(self.allocator, "{s}_{d}", .{ identifier, uid });
    }
};

const GeneratorError = error{OutOfMemory};

const ir = @import("ir.zig");
const BuiltIn = @import("built-in.zig").BuiltIn;
const std = @import("std");
