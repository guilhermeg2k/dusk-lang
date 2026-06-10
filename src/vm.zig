const std = @import("std");
const bc = @import("bytecode.zig");

pub const VM = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    program: *const bc.Program,
    frames: std.ArrayList(CallFrame),
    current_frame: *CallFrame,
    stack: [16383]bc.Value,

    pub fn init(allocator: std.mem.Allocator, program: *const bc.Program) VM {
        return .{
            .allocator = allocator,
            .frames = .empty,
            .program = program,
            .current_frame = undefined,
            .stack = undefined,
        };
    }

    pub fn run(self: *Self) void {
        const main_fn = self.program.functions[self.program.main_func_index];

        self.frames.append(self.allocator, CallFrame{
            .function = &main_fn,
            .cur_inst = 0,
            .stack_start_index = 0,
        }) catch @panic("OOM");
        self.current_frame = &self.frames.items[self.frames.items.len - 1];

        const instructions = self.current_frame.function.chunk.instructions;
        const constants = self.current_frame.function.chunk.constants;

        while (self.current_frame.cur_inst < instructions.len) : (self.current_frame.cur_inst += 1) {
            const inst = instructions[self.current_frame.cur_inst];
            const stack = self.stack[0..];
            switch (inst.op) {
                .LOAD_CONST => {
                    self.current_frame.setVar(stack, inst.a, constants[inst.b]);
                },
                .LOAD => {
                    self.current_frame.setVar(stack, inst.a, self.current_frame.getVar(stack, inst.b));
                },

                .I_ADD => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{
                        .i_int = self.current_frame.getVar(stack, inst.b).i_int + self.current_frame.getVar(stack, inst.c).i_int,
                    });
                },
                .I_SUB => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{
                        .i_int = self.current_frame.getVar(stack, inst.b).i_int - self.current_frame.getVar(stack, inst.c).i_int,
                    });
                },
                .I_MULT => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{
                        .i_int = self.current_frame.getVar(stack, inst.b).i_int * self.current_frame.getVar(stack, inst.c).i_int,
                    });
                },
                .I_DIV => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{ .i_int = @divTrunc(
                        self.current_frame.getVar(stack, inst.b).i_int,
                        self.current_frame.getVar(stack, inst.c).i_int,
                    ) });
                },
                .I_MOD => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{ .i_int = @mod(
                        self.current_frame.getVar(stack, inst.b).i_int,
                        self.current_frame.getVar(stack, inst.c).i_int,
                    ) });
                },
                .I_EQ => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = self.current_frame.getVar(stack, inst.b).i_int == self.current_frame.getVar(stack, inst.c).i_int,
                    });
                },
                .I_NEQ => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = self.current_frame.getVar(stack, inst.b).i_int != self.current_frame.getVar(stack, inst.c).i_int,
                    });
                },
                .I_LT => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = self.current_frame.getVar(stack, inst.b).i_int < self.current_frame.getVar(stack, inst.c).i_int,
                    });
                },
                .I_LE => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = self.current_frame.getVar(stack, inst.b).i_int <= self.current_frame.getVar(stack, inst.c).i_int,
                    });
                },
                .I_GT => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = self.current_frame.getVar(stack, inst.b).i_int > self.current_frame.getVar(stack, inst.c).i_int,
                    });
                },
                .I_GE => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = self.current_frame.getVar(stack, inst.b).i_int >= self.current_frame.getVar(stack, inst.c).i_int,
                    });
                },
                .I_NEG => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{
                        .i_int = -self.current_frame.getVar(stack, inst.b).i_int,
                    });
                },

                .F_ADD => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{
                        .i_float = self.current_frame.getVar(stack, inst.b).i_float + self.current_frame.getVar(stack, inst.c).i_float,
                    });
                },
                .F_SUB => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{
                        .i_float = self.current_frame.getVar(stack, inst.b).i_float - self.current_frame.getVar(stack, inst.c).i_float,
                    });
                },
                .F_MULT => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{
                        .i_float = self.current_frame.getVar(stack, inst.b).i_float * self.current_frame.getVar(stack, inst.c).i_float,
                    });
                },
                .F_DIV => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{
                        .i_float = self.current_frame.getVar(stack, inst.b).i_float / self.current_frame.getVar(stack, inst.c).i_float,
                    });
                },
                .F_MOD => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{ .i_float = @mod(
                        self.current_frame.getVar(stack, inst.b).i_float,
                        self.current_frame.getVar(stack, inst.c).i_float,
                    ) });
                },
                .F_EQ => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = self.current_frame.getVar(stack, inst.b).i_float == self.current_frame.getVar(stack, inst.c).i_float,
                    });
                },
                .F_NEQ => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = self.current_frame.getVar(stack, inst.b).i_float != self.current_frame.getVar(stack, inst.c).i_float,
                    });
                },
                .F_LT => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = self.current_frame.getVar(stack, inst.b).i_float < self.current_frame.getVar(stack, inst.c).i_float,
                    });
                },
                .F_LE => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = self.current_frame.getVar(stack, inst.b).i_float <= self.current_frame.getVar(stack, inst.c).i_float,
                    });
                },
                .F_GT => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = self.current_frame.getVar(stack, inst.b).i_float > self.current_frame.getVar(stack, inst.c).i_float,
                    });
                },
                .F_GE => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{
                        .i_bool = self.current_frame.getVar(stack, inst.b).i_float >= self.current_frame.getVar(stack, inst.c).i_float,
                    });
                },
                .F_NEG => {
                    self.current_frame.setVar(stack, inst.a, bc.Value{
                        .i_float = -self.current_frame.getVar(stack, inst.b).i_float,
                    });
                },
                else => {},
            }
        }
        std.debug.print("Result in R: {any}\n", .{self.stack[self.current_frame.stack_start_index .. self.current_frame.stack_start_index + 14]});
    }
};

const CallFrame = struct {
    const Self = @This();

    function: *const bc.Function,
    cur_inst: usize,
    stack_start_index: usize,

    pub fn getStackIndex(self: *Self, index: u8) usize {
        return self.stack_start_index + index;
    }

    pub fn getVar(self: *Self, stack: []bc.Value, index: u8) bc.Value {
        return stack[self.getStackIndex(index)];
    }

    pub fn setVar(self: *Self, stack: []bc.Value, index: u8, value: bc.Value) void {
        stack[self.getStackIndex(index)] = value;
    }
};
