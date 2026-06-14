const std = @import("std");
const bc = @import("bytecode.zig");

pub const VM = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    program: *const bc.Program,
    frames: std.ArrayList(CallFrame),
    stack: [16383]bc.Value,
    heap: std.ArrayList(bc.Value),

    pub fn init(allocator: std.mem.Allocator, program: *const bc.Program) VM {
        return .{
            .allocator = allocator,
            .frames = .empty,
            .program = program,
            .stack = undefined,
            .heap = .empty,
        };
    }

    pub fn run(self: *Self) !void {
        const main_fn = self.program.functions[self.program.main_func_index];

        try self.frames.append(self.allocator, CallFrame{
            .function = &main_fn,
            .cur_inst = 0,
            .stack_offset = 0,
        });

        var current_frame = &self.frames.items[self.frames.items.len - 1];

        const stack = self.stack[0..];

        while (current_frame.cur_inst < current_frame.function.kind.native.instructions.len) : (current_frame.cur_inst += 1) {
            current_frame = &self.frames.items[self.frames.items.len - 1];

            const inst = current_frame.function.kind.native.instructions[current_frame.cur_inst];

            switch (inst.op) {
                .LOAD_CONST => {
                    current_frame.setVar(stack, inst.a, current_frame.function.kind.native.constants[inst.bEx()]);
                },
                .LOAD => {
                    current_frame.setVar(stack, inst.a, current_frame.getVar(stack, inst.b));
                },

                .I_ADD => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_int = current_frame.getVar(stack, inst.b).i_int + current_frame.getVar(stack, inst.c).i_int,
                    });
                },
                .I_SUB => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_int = current_frame.getVar(stack, inst.b).i_int - current_frame.getVar(stack, inst.c).i_int,
                    });
                },
                .I_MULT => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_int = current_frame.getVar(stack, inst.b).i_int * current_frame.getVar(stack, inst.c).i_int,
                    });
                },
                .TRUNC_DIV => {
                    current_frame.setVar(stack, inst.a, bc.Value{ .i_int = @divTrunc(
                        current_frame.getVar(stack, inst.b).i_int,
                        current_frame.getVar(stack, inst.c).i_int,
                    ) });
                },
                .I_MOD => {
                    current_frame.setVar(stack, inst.a, bc.Value{ .i_int = @mod(
                        current_frame.getVar(stack, inst.b).i_int,
                        current_frame.getVar(stack, inst.c).i_int,
                    ) });
                },
                .I_EQ => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = current_frame.getVar(stack, inst.b).i_int == current_frame.getVar(stack, inst.c).i_int,
                    });
                },
                .I_NEQ => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = current_frame.getVar(stack, inst.b).i_int != current_frame.getVar(stack, inst.c).i_int,
                    });
                },
                .I_LT => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = current_frame.getVar(stack, inst.b).i_int < current_frame.getVar(stack, inst.c).i_int,
                    });
                },
                .I_LE => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = current_frame.getVar(stack, inst.b).i_int <= current_frame.getVar(stack, inst.c).i_int,
                    });
                },
                .I_GT => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = current_frame.getVar(stack, inst.b).i_int > current_frame.getVar(stack, inst.c).i_int,
                    });
                },
                .I_GE => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = current_frame.getVar(stack, inst.b).i_int >= current_frame.getVar(stack, inst.c).i_int,
                    });
                },
                .I_NEG => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_int = -current_frame.getVar(stack, inst.b).i_int,
                    });
                },

                .F_ADD => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_float = current_frame.getVar(stack, inst.b).i_float + current_frame.getVar(stack, inst.c).i_float,
                    });
                },
                .F_SUB => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_float = current_frame.getVar(stack, inst.b).i_float - current_frame.getVar(stack, inst.c).i_float,
                    });
                },
                .F_MULT => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_float = current_frame.getVar(stack, inst.b).i_float * current_frame.getVar(stack, inst.c).i_float,
                    });
                },
                .F_DIV => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_float = current_frame.getVar(stack, inst.b).i_float / current_frame.getVar(stack, inst.c).i_float,
                    });
                },
                .F_MOD => {
                    current_frame.setVar(stack, inst.a, bc.Value{ .i_float = @mod(
                        current_frame.getVar(stack, inst.b).i_float,
                        current_frame.getVar(stack, inst.c).i_float,
                    ) });
                },
                .F_EQ => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = current_frame.getVar(stack, inst.b).i_float == current_frame.getVar(stack, inst.c).i_float,
                    });
                },
                .F_NEQ => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = current_frame.getVar(stack, inst.b).i_float != current_frame.getVar(stack, inst.c).i_float,
                    });
                },
                .F_LT => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = current_frame.getVar(stack, inst.b).i_float < current_frame.getVar(stack, inst.c).i_float,
                    });
                },
                .F_LE => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = current_frame.getVar(stack, inst.b).i_float <= current_frame.getVar(stack, inst.c).i_float,
                    });
                },
                .F_GT => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = current_frame.getVar(stack, inst.b).i_float > current_frame.getVar(stack, inst.c).i_float,
                    });
                },
                .F_GE => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = current_frame.getVar(stack, inst.b).i_float >= current_frame.getVar(stack, inst.c).i_float,
                    });
                },
                .F_NEG => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_float = -current_frame.getVar(stack, inst.b).i_float,
                    });
                },
                .B_AND => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = current_frame.getVar(stack, inst.b).i_bool and current_frame.getVar(stack, inst.c).i_bool,
                    });
                },
                .B_OR => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = current_frame.getVar(stack, inst.b).i_bool or current_frame.getVar(stack, inst.c).i_bool,
                    });
                },
                .B_EQ => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = current_frame.getVar(stack, inst.b).i_bool == current_frame.getVar(stack, inst.c).i_bool,
                    });
                },
                .B_NEQ => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = current_frame.getVar(stack, inst.b).i_bool != current_frame.getVar(stack, inst.c).i_bool,
                    });
                },
                .B_NOT => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = !current_frame.getVar(stack, inst.b).i_bool,
                    });
                },
                .I_TO_F => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_float = @floatFromInt(current_frame.getVar(stack, inst.a).i_int),
                    });
                },
                .F_TO_I => {
                    current_frame.setVar(stack, inst.a, bc.Value{
                        .i_int = @trunc(current_frame.getVar(stack, inst.a).i_float),
                    });
                },
                .CALL => {
                    const func = self.program.functions[inst.bEx()];
                    switch (func.kind) {
                        .native => try self.callFunction(&func, current_frame.stack_offset, inst.a),
                        .builtin => |b| {
                            const args = stack[current_frame.stack_offset + inst.a + 1 ..][0..b.num_args];
                            stack[current_frame.stack_offset + inst.a] = b.func(args);
                        },
                    }
                },
                .JUMP => {
                    current_frame.cur_inst = inst.aEx() - 1;
                },
                .JUMP_IF_FALSE => {
                    const condition = current_frame.getVar(stack, inst.a);
                    if (!condition.i_bool) {
                        current_frame.cur_inst = inst.bEx() - 1;
                    }
                },
                .TYPE => {
                    current_frame.setVar(stack, inst.a, bc.Value{ .i_int = inst.b });
                },
                .RETURN => {
                    self.stack[current_frame.stack_offset - 1] = current_frame.getVar(stack, inst.a);
                    _ = self.frames.pop();
                    current_frame = &self.frames.items[self.frames.items.len - 1];
                    current_frame.cur_inst -= 1;
                },
                else => {},
            }
        }
    }

    fn callFunction(self: *Self, func: *const bc.Function, current_stack_offset: usize, target_reg: u8) !void {
        try self.frames.append(self.allocator, CallFrame{
            .function = func,
            .cur_inst = 0,
            .stack_offset = current_stack_offset + target_reg + 1,
        });
    }
};

const CallFrame = struct {
    const Self = @This();

    function: *const bc.Function,
    cur_inst: usize,
    stack_offset: usize,

    pub fn getStackIndex(self: *Self, index: u8) usize {
        return self.stack_offset + index;
    }

    pub fn getVar(self: *Self, stack: []bc.Value, index: u8) bc.Value {
        return stack[self.getStackIndex(index)];
    }

    pub fn setVar(self: *Self, stack: []bc.Value, index: u8, value: bc.Value) void {
        stack[self.getStackIndex(index)] = value;
    }
};
