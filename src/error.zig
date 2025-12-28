pub const ErrorDispatcher = struct {
    const Self = @This();

    src: []const u8,
    allocator: std.mem.Allocator,

    const MAX_PADDING = 256;
    const padding = " " ** MAX_PADDING;

    pub fn unexpectedToken(self: *Self, token: Token) Errors {
        self.log(try allocPrint(self.allocator, "Unexpected token \"{s}\"", .{token.value(self.src)}));
        return Errors.LexerError;
    }

    pub fn invalidSyntax(self: *Self, expected: []const u8, token: Token) Errors {
        self.log(allocPrint(self.allocator, "Expected {s} found {s}", .{ expected, token.value(self.src) }));
        return Errors.ParserError;
    }

    pub fn invalidType(self: *Self, expected: []const u8, found: []const u8, loc: usize) Errors {
        self.log(allocPrint(self.allocator, "Invalid type, expected {s} found {s}", .{ expected, found }), loc);
        return Errors.SemaError;
    }

    pub fn typeNotDefined(self: *Self, found: []const u8, loc: usize) Errors {
        self.log(allocPrint(self.allocator, "Invalid type {s}", .{found}), loc);
        return Errors.SemaError;
    }

    pub fn invalidNumberOfArgs(self: *Self, expected: usize, found: usize, loc: usize) Errors {
        self.log(allocPrint(self.allocator, "Invalid number of arguments, expected {d} found {d}"), .{ expected, found }, loc);
        return Errors.SemaError;
    }

    pub fn notDefined(self: *Self, identifier: []const u8, loc: usize) Errors {
        self.log(allocPrint(self.allocator, "Cannot find name \"{s}\"", .{identifier}), loc);
        return Errors.SemaError;
    }

    pub fn notMutable(self: *Self, identifier: []const u8, loc: usize) Errors {
        self.log(allocPrint(self.allocator, "\"{s}\" is not mutable", .{identifier}), loc);
        return Errors.SemaError;
    }

    pub fn log(self: *Self, msg: []const u8, loc: usize) Errors {
        const exactLoc = try ErrorDispatcher.findExactLoc(self.src, loc);
        const spaces = 6 + loc - exactLoc.line.start;

        std.log.err(
            "{s}\n at line {d} col {d}\n--> {d}: {s} {s}^",
            .{
                msg,
                exactLoc.line.count,
                exactLoc.col,
                exactLoc.line.count,
                self.src[exactLoc.line.start .. exactLoc.line.end + 1],
                padding[0..@min(spaces, MAX_PADDING)],
            },
        );

        return Errors.SemaError;
    }

    fn findExactLoc(self: *Self, loc: usize) !ExactLoc {
        var line_count: usize = 1;
        var line_start: usize = 0;
        var line_end: usize = 0;

        var i: usize = 0;
        var should_break_in_next_line = false;
        while (true) {
            if (i == loc) should_break_in_next_line = true;
            if (i >= self.src.len) return Errors.UnableToFindLoc;
            if (self.src[i] == '\n') {
                if (should_break_in_next_line) {
                    line_end = i;
                    break;
                }

                line_start = i + 1;
                line_count += 1;
            }
            i += 1;
        }

        var col_count: usize = 1;
        i = line_start;
        while (true) {
            if (i == loc) break;
            if (i >= self.src.len) return Errors.UnableToFindLoc;
            col_count += 1;
            i += 1;
        }

        return .{ .line = .{
            .count = line_count,
            .start = line_start,
            .end = line_end,
        }, .col = col_count };
    }
};

const ExactLoc = struct { line: struct {
    count: usize,
    start: usize,
    end: usize,
}, col: usize };

pub const Errors = error{
    OutOfMemory,
    UnableToFindLoc,
    AstError,
    LexerError,
    ParserError,
    SemaError,
};

const std = @import("std");
const allocPrint = std.fmt.allocPrint;
const Token = @import("lexer.zig").Token;
