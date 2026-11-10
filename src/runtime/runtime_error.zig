const std = @import("std");

pub const RuntimeError = error{
    OutOfMemory,
    ArrayOutOfBounds,
    ByteOutOfRange,
    IndexOutOfBounds,
    NegativeBufferSize,
    UnhandledError,
    CallDepthExceeded,
    DivisionByZero,
    DivisionOverflow,
    IntegerOverflow,
    FloatOutOfIntRange,
    AssertionFailed,
    TaskAwaitsItself,
    AllTasksWaiting,
    NegativeSleep,
    UnhandledTaskError,
};

pub const RuntimeErrorReporter = struct {
    pub noinline fn arrayOutOfBounds(index: i64, len: usize) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.ArrayOutOfBounds, "index {d} is out of bounds for array of length {d}", .{ index, len });
    }

    pub noinline fn indexOutOfBounds(index: i64, len: usize) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.IndexOutOfBounds, "index {d} is out of bounds for length {d}", .{ index, len });
    }

    pub noinline fn negativeBufferSize(len: i64) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.NegativeBufferSize, "buffer size can't be negative: {d}", .{len});
    }

    pub noinline fn byteOutOfRange(number: i64) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.ByteOutOfRange, "value {d} doesn't fit in byte (0..255)", .{number});
    }

    pub noinline fn callDepthExceeded(max_depth: usize) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.CallDepthExceeded, "call depth exceeded: more than {d} nested calls", .{max_depth});
    }

    pub noinline fn divisionByZero(operator: []const u8) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.DivisionByZero, "integer division by zero ('{s}' with a right side of 0)", .{operator});
    }

    pub noinline fn divisionOverflow(dividend: i64) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.DivisionOverflow, "integer overflow: {d} // -1 doesn't fit in an int", .{dividend});
    }

    pub noinline fn integerOverflow(operator: []const u8, left: i64, right: i64) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.IntegerOverflow, "integer overflow: {d} {s} {d} doesn't fit in an int", .{ left, operator, right });
    }

    pub noinline fn negationOverflow(operand: i64) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.IntegerOverflow, "integer overflow: -({d}) doesn't fit in an int", .{operand});
    }

    pub noinline fn floatOutOfIntRange(value: f64) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.FloatOutOfIntRange, "{d} doesn't fit in an int", .{value});
    }

    pub noinline fn unhandledError(error_name: []const u8) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.UnhandledError, "unhandled error {s}", .{error_name});
    }

    pub noinline fn taskAwaitsItself() RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.TaskAwaitsItself, "a task can't await itself", .{});
    }

    pub noinline fn allTasksWaiting() RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.AllTasksWaiting, "all tasks are waiting: nothing can wake them up", .{});
    }

    pub noinline fn negativeSleep(ms: i64) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.NegativeSleep, "sleep time can't be negative: {d}", .{ms});
    }

    pub noinline fn unhandledTaskError(error_name: []const u8) RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.UnhandledTaskError, "unhandled error in task: {s}", .{error_name});
    }

    pub noinline fn assertionFailed() RuntimeError {
        @branchHint(.cold);
        return report(RuntimeError.AssertionFailed, "assertion failed", .{});
    }

    pub fn outOfMemory() void {
        if (@import("builtin").is_test) return;
        std.debug.print("error: out of memory\n", .{});
    }

    pub fn where(function: ?[]const u8, builtin: ?[]const u8) void {
        if (@import("builtin").is_test) return;
        if (builtin) |name| {
            if (function) |caller| return std.debug.print("  in builtin '{s}', called from function '{s}'\n", .{ name, caller });
            return std.debug.print("  in builtin '{s}'\n", .{name});
        }
        if (function) |name| std.debug.print("  in function '{s}'\n", .{name});
    }

    fn report(err: RuntimeError, comptime fmt: []const u8, args: anytype) RuntimeError {
        if (@import("builtin").is_test) return err;
        std.debug.print("error: " ++ fmt ++ "\n", args);
        return err;
    }
};
