const std = @import("std");
const bc = @import("bytecode.zig");

const VM = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    program: bc.Program,
    frames: std.ArrayList(CallFrame),

    fn init(allocator: std.mem.Allocator, program: bc.Program) VM {
        return .{
            .allocator = allocator,
            .frames = std.ArrayList(CallFrame).init(allocator),
            .program = program,
        };
    }

    pub fn run(self: *Self) void {
         
    }
};

const CallFrame = struct {
    function: *bc.Function,
    current_instruction: u32,
    registers: []bc.Value,
};
