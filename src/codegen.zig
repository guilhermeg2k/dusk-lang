pub const Generator = struct {
    const Self = @This();
    allocator: std.mem.Allocator,

    pub fn generate(self: *Self, program: ir.Program) ![]const u8 {
        var buf: std.ArrayList(u8) = .empty;
        const functions = try self.genFunctions(program.functions);
        const instructions = try self.genInstructions(program.instructions);
        try buf.print(self.allocator, "{s}", .{functions});
        try buf.print(self.allocator, "\n", .{});
        try buf.print(self.allocator, "{s}", .{instructions});

        return buf.toOwnedSlice(self.allocator);
    }

    fn genFunctions(self: *Self, functions: std.ArrayList(ir.Func)) ![]const u8 {
        var functions_buf: std.ArrayList(u8) = .empty;
        const writer = functions_buf.writer(self.allocator);

        for (functions.items) |func| {
            try writer.writeAll(try self.genFuncSignature(func));
            try writer.writeAll("{\n");
            const body = try self.genInstructions(func.body);
            try writer.writeAll(body);
            try writer.writeAll("}\n\n");
        }

        return functions_buf.toOwnedSlice(self.allocator);
    }

    fn genInstructions(self: *Self, instructions: std.ArrayList(ir.Instruction)) GeneratorError![]const u8 {
        var instructions_buf: std.ArrayList(u8) = .empty;
        const writer = instructions_buf.writer(self.allocator);

        for (instructions.items) |instruction| {
            switch (instruction) {
                .store_var => {
                    try writer.writeAll(try self.genStoreVar(instruction.store_var));
                },
                .update_var => {
                    try writer.writeAll(try self.genUpdateVar(instruction.update_var));
                },
                .branch_if => {
                    try writer.writeAll(try self.genBranchIf(instruction.branch_if));
                },
                .loop => {
                    try writer.writeAll(try self.genLoop(instruction.loop));
                },
                .return_stmt => {
                    try writer.writeAll(try self.genReturn(instruction.return_stmt));
                },
                .expression_stmt => {
                    try writer.writeAll(try self.genExpression(instruction.expression_stmt));
                },
            }
        }

        return instructions_buf.toOwnedSlice(self.allocator);
    }

    fn genStoreVar(self: *Self, store_var: ir.StoreVar) ![]const u8 {
        var store_var_buf: std.ArrayList(u8) = .empty;
        const writer = store_var_buf.writer(self.allocator);

        const var_name = try self.genName(store_var.uid, store_var.identifier);
        const value = try self.genValue(store_var.value);

        try writer.print("let {s} = {s};\n", .{ var_name, value });

        return store_var_buf.toOwnedSlice(self.allocator);
    }

    fn genUpdateVar(self: *Self, update_var: ir.UpdateVar) ![]const u8 {
        var update_var_buf: std.ArrayList(u8) = .empty;
        const writer = update_var_buf.writer(self.allocator);

        const var_name = try self.genName(update_var.var_uid, update_var.identifier);
        const value = try self.genValue(update_var.value);

        try writer.print("{s} = {s};\n", .{ var_name, value });

        return update_var_buf.toOwnedSlice(self.allocator);
    }

    fn genBranchIf(self: *Self, branch_if: ir.BranchIf) ![]const u8 {
        var branch_if_buf: std.ArrayList(u8) = .empty;
        const writer = branch_if_buf.writer(self.allocator);
        const value = try self.genValue(branch_if.condition);

        const then_block = try self.genInstructions(branch_if.then_block);
        const else_block = try self.genInstructions(branch_if.else_block);

        try writer.print("if ({s}){{\n", .{value});
        try writer.writeAll(then_block);
        try writer.writeAll("}");

        if (else_block.len > 0) {
            try writer.writeAll("else {\n");
            try writer.writeAll(else_block);
            try writer.writeAll("}\n");
        }

        return branch_if_buf.toOwnedSlice(self.allocator);
    }

    fn genLoop(self: *Self, loop: ir.Loop) ![]const u8 {
        var loop_buf: std.ArrayList(u8) = .empty;
        const writer = loop_buf.writer(self.allocator);

        try writer.writeAll("while (");

        if (loop.condition) |condition| {
            try writer.writeAll(try self.genValue(condition));
        } else {
            try writer.writeAll("true");
        }

        try writer.writeAll("){\n");

        const block = try self.genInstructions(loop.do_block);
        try writer.writeAll(block);
        try writer.writeAll("}\n");

        return loop_buf.toOwnedSlice(self.allocator);
    }

    fn genReturn(self: *Self, return_stmt: ir.ReturnStmt) ![]const u8 {
        var return_buf: std.ArrayList(u8) = .empty;
        const writer = return_buf.writer(self.allocator);
        try writer.writeAll("return");

        if (return_stmt.value) |value| {
            try writer.print(" {s}", .{try self.genValue(value)});
        }

        try writer.writeAll(";\n");
        return return_buf.toOwnedSlice(self.allocator);
    }

    fn genExpression(self: *Self, exp_stmt: ir.ExpressionStmt) ![]const u8 {
        var exp_buf: std.ArrayList(u8) = .empty;
        const writer = exp_buf.writer(self.allocator);
        try writer.print("{s};\n", .{try self.genValue(exp_stmt.value)});
        return exp_buf.toOwnedSlice(self.allocator);
    }

    fn genValue(self: *Self, value: *ir.Value) GeneratorError![]const u8 {
        var value_buf: std.ArrayList(u8) = .empty;
        const writer = value_buf.writer(self.allocator);

        switch (value.*) {
            .i_float => try writer.print("{d}", .{value.i_float}),
            .i_bool => try writer.print("{any}", .{value.i_bool}),
            .i_string => try writer.print("\"{s}\"", .{value.i_string}),
            .i_void => {},
            .identifier => try writer.writeAll(try self.genName(value.identifier.uid, value.identifier.identifier)),
            .fn_call => try writer.writeAll(try self.genFnCall(value.fn_call)),
            .binary_op => try writer.writeAll(try self.genBinaryOp(value.binary_op)),
            .unary_op => try writer.writeAll(try self.genUnaryOp(value.unary_op)),
            //todo: closures
            .fn_def => {},
        }

        return value_buf.toOwnedSlice(self.allocator);
    }

    fn genBinaryOp(self: *Self, binaryOp: ir.BinaryOp) ![]const u8 {
        const left = try self.genValue(binaryOp.left);
        const right = try self.genValue(binaryOp.right);
        const op = self.genBinaryOpSymbol(binaryOp.kind);
        return std.fmt.allocPrint(self.allocator, "{s} {s} {s}", .{ left, op, right });
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

    fn genUnaryOp(self: *Self, unaryOp: ir.UnaryOp) ![]const u8 {
        const right = try self.genValue(unaryOp.right);
        const op = self.genUnaryOpSymbol(unaryOp.kind);
        return std.fmt.allocPrint(self.allocator, "{s}{s}", .{ op, right });
    }

    fn genUnaryOpSymbol(_: *Self, op: ir.UnaryOpKind) []const u8 {
        return switch (op) {
            .neg => "-",
            .not => "!",
        };
    }

    fn genFnCall(self: *Self, fnCall: ir.FnCall) ![]const u8 {
        var fn_call_buf: std.ArrayList(u8) = .empty;
        const writer = fn_call_buf.writer(self.allocator);
        const fn_name = try self.genName(fnCall.fn_uid, fnCall.identifier);

        try writer.print("{s}(", .{fn_name});

        for (fnCall.args.items, 0..) |arg, i| {
            const arg_value = try self.genValue(arg);
            if (i > 0) try writer.writeAll(",");
            try writer.print("{s}", .{arg_value});
        }

        try writer.print(")", .{});

        return fn_call_buf.toOwnedSlice(self.allocator);
    }

    fn genFuncSignature(self: *Self, func: ir.Func) ![]const u8 {
        var function_signature: std.ArrayList(u8) = .empty;
        const writer = function_signature.writer(self.allocator);

        const fn_name = try self.genName(func.uid, func.identifier);
        try writer.print("function {s}(", .{fn_name});

        for (func.args.items, 0..) |arg, i| {
            const arg_name = try self.genName(arg.uid, arg.identifier);
            if (i > 0) try writer.writeAll(",");
            try writer.print("{s}", .{arg_name});
        }

        try writer.writeAll(")");
        return function_signature.toOwnedSlice(self.allocator);
    }

    fn genName(self: *Self, uid: usize, identifier: []const u8) ![]const u8 {
        return std.fmt.allocPrint(self.allocator, "{s}_{d}", .{ identifier, uid });
    }
};

const GeneratorError = error{OutOfMemory};

const std = @import("std");
const ir = @import("ir.zig");
