const std = @import("std");
const bc = @import("bytecode.zig");

pub const VM = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    program: *const bc.Program,
    frames: std.ArrayList(CallFrame),
    current_frame: *CallFrame,

    pub fn init(allocator: std.mem.Allocator, program: *const bc.Program) VM {
        return .{
            .allocator = allocator,
            .frames = .empty,
            .program = program,
            .current_frame = undefined,
        };
    }

    pub fn run(self: *Self) void {
        const main_fn = self.program.functions[self.program.main_func_index];

        var frame = CallFrame{
            .function = &main_fn,
            .cur_inst = 0,
            .registers = undefined,
        };

        self.current_frame = &frame;

        const instructions = self.current_frame.function.chunk.instructions;
        const constants = self.current_frame.function.chunk.constants;

        while (self.current_frame.cur_inst < instructions.len) : (self.current_frame.cur_inst += 1) {
            const inst = instructions[self.current_frame.cur_inst];
            switch (inst.op) {
                .LOAD_CONST => {
                    frame.registers[inst.a] = constants[inst.b];
                },
                .LOAD => {
                    frame.registers[inst.a] = frame.registers[inst.b];
                },

                .I_ADD => {
                    frame.registers[inst.a] = bc.Value{
                        .i_int = frame.registers[inst.b].i_int + frame.registers[inst.c].i_int,
                    };
                },
                .I_SUB => {
                    frame.registers[inst.a] = bc.Value{
                        .i_int = frame.registers[inst.b].i_int - frame.registers[inst.c].i_int,
                    };
                },
                .I_MULT => {
                    frame.registers[inst.a] = bc.Value{
                        .i_int = frame.registers[inst.b].i_int * frame.registers[inst.c].i_int,
                    };
                },
                .I_DIV => {
                    frame.registers[inst.a] = bc.Value{ .i_int = @divTrunc(
                        frame.registers[inst.b].i_int,
                        frame.registers[inst.c].i_int,
                    ) };
                },
                .I_MOD => {
                    frame.registers[inst.a] = bc.Value{ .i_int = @mod(
                        frame.registers[inst.b].i_int,
                        frame.registers[inst.c].i_int,
                    ) };
                },
                .I_EQ => {
                    frame.registers[inst.a] = bc.Value{
                        .i_bool = frame.registers[inst.b].i_int == frame.registers[inst.c].i_int,
                    };
                },
                .I_NEQ => {
                    frame.registers[inst.a] = bc.Value{
                        .i_bool = frame.registers[inst.b].i_int != frame.registers[inst.c].i_int,
                    };
                },
                .I_LT => {
                    frame.registers[inst.a] = bc.Value{
                        .i_bool = frame.registers[inst.b].i_int < frame.registers[inst.c].i_int,
                    };
                },
                .I_LE => {
                    frame.registers[inst.a] = bc.Value{
                        .i_bool = frame.registers[inst.b].i_int <= frame.registers[inst.c].i_int,
                    };
                },
                .I_GT => {
                    frame.registers[inst.a] = bc.Value{
                        .i_bool = frame.registers[inst.b].i_int > frame.registers[inst.c].i_int,
                    };
                },
                .I_GE => {
                    frame.registers[inst.a] = bc.Value{
                        .i_bool = frame.registers[inst.b].i_int >= frame.registers[inst.c].i_int,
                    };
                },
                .I_NEG => {
                    frame.registers[inst.a] = bc.Value{
                        .i_int = -frame.registers[inst.b].i_int,
                    };
                },

                .F_ADD => {
                    frame.registers[inst.a] = bc.Value{
                        .i_float = frame.registers[inst.b].i_float + frame.registers[inst.c].i_float,
                    };
                },
                .F_SUB => {
                    frame.registers[inst.a] = bc.Value{
                        .i_float = frame.registers[inst.b].i_float - frame.registers[inst.c].i_float,
                    };
                },
                .F_MULT => {
                    frame.registers[inst.a] = bc.Value{
                        .i_float = frame.registers[inst.b].i_float * frame.registers[inst.c].i_float,
                    };
                },
                .F_DIV => {
                    frame.registers[inst.a] = bc.Value{
                        .i_float = frame.registers[inst.b].i_float / frame.registers[inst.c].i_float,
                    };
                },
                .F_MOD => {
                    frame.registers[inst.a] = bc.Value{ .i_float = @mod(
                        frame.registers[inst.b].i_float,
                        frame.registers[inst.c].i_float,
                    ) };
                },
                .F_EQ => {
                    frame.registers[inst.a] = bc.Value{
                        .i_bool = frame.registers[inst.b].i_float == frame.registers[inst.c].i_float,
                    };
                },
                .F_NEQ => {
                    frame.registers[inst.a] = bc.Value{
                        .i_bool = frame.registers[inst.b].i_float != frame.registers[inst.c].i_float,
                    };
                },
                .F_LT => {
                    frame.registers[inst.a] = bc.Value{
                        .i_bool = frame.registers[inst.b].i_float < frame.registers[inst.c].i_float,
                    };
                },
                .F_LE => {
                    frame.registers[inst.a] = bc.Value{
                        .i_bool = frame.registers[inst.b].i_float <= frame.registers[inst.c].i_float,
                    };
                },
                .F_GT => {
                    frame.registers[inst.a] = bc.Value{
                        .i_bool = frame.registers[inst.b].i_float > frame.registers[inst.c].i_float,
                    };
                },
                .F_GE => {
                    frame.registers[inst.a] = bc.Value{
                        .i_bool = frame.registers[inst.b].i_float >= frame.registers[inst.c].i_float,
                    };
                },
                .F_NEG => {
                    frame.registers[inst.a] = bc.Value{
                        .i_float = -frame.registers[inst.b].i_float,
                    };
                },
                else => {},
            }
        }
        std.debug.print("Result in R: {any}\n", .{self.current_frame.registers[0..14]});
    }
};

const CallFrame = struct {
    function: *const bc.Function,
    cur_inst: u32,
    registers: [256]bc.Value,
};
