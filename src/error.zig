pub const Error = struct {
    const MAX_PADDING = 256;
    const padding = " " ** MAX_PADDING;

    pub fn lexer(token: Token, src: []const u8) AllError {
        const fineLoc = try token.fineLoc(src);
        const spaces = 6 + token.loc.start - fineLoc.line.start;

        std.log.err("Unexpected Token \"{s}\" at line {d} col {d}\n--> {d}: {s} {s}^", .{ token.value(src), fineLoc.line.count, fineLoc.col, fineLoc.line.count, src[fineLoc.line.start .. fineLoc.line.end + 1], padding[0..@min(spaces, MAX_PADDING)] });
        return AllError.UnexpectedToken;
    }

    pub fn parser(expected: []const u8, token: Token, src: []const u8) AllError {
        const fineLoc = try token.fineLoc(src);
        const spaces = 6 + token.loc.start - fineLoc.line.start;

        std.log.err("Expected {s}\nFound \"{s}\" at line {d} col {d}\n--> {d}: {s} {s}^", .{ expected, token.value(src), fineLoc.line.count, fineLoc.col, fineLoc.line.count, src[fineLoc.line.start .. fineLoc.line.end + 1], padding[0..@min(spaces, MAX_PADDING)] });

        return AllError.UnexpectedToken;
    }
};

pub const AllError = error{ UnexpectedToken, UnableToFindToken, OutOfMemory };

const std = @import("std");
const Token = @import("lexer.zig").Token;
