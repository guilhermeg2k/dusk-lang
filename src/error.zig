const std = @import("std");
const Token = @import("lexer.zig").Token;
const Loc = @import("lexer.zig").Loc;

const allocPrint = std.fmt.allocPrint;

pub const ErrorDispatcher = struct {
    const Self = @This();

    src: []const u8,
    allocator: std.mem.Allocator,

    const LINE_MAX_CHAR_COUNT = 256;
    const padding = " " ** LINE_MAX_CHAR_COUNT;

    pub fn unexpectedToken(self: *Self, token: Token) Errors {
        self.log(try allocPrint(self.allocator, "Unexpected token '{s}'", .{token.value(self.src)}), token.loc);
        return Errors.LexerError;
    }

    pub fn invalidSyntax(self: *Self, expected: []const u8, token: Token) Errors {
        self.log(
            try allocPrint(
                self.allocator,
                "Invalid Syntax: expected {s} found '{s}'",
                .{ expected, token.value(self.src) },
            ),
            token.loc,
        );
        return Errors.ParserError;
    }

    pub fn invalidType(self: *Self, expected: []const u8, found: []const u8, loc: Loc) Errors {
        self.log(
            try allocPrint(
                self.allocator,
                "Invalid type: expected '{s}' found '{s}'",
                .{ expected, found },
            ),
            loc,
        );
        return error.InvalidType;
    }

    pub fn invalidFunctionReturnType(self: *Self, expected: []const u8, found: []const u8, loc: Loc) Errors {
        self.log(
            try allocPrint(
                self.allocator,
                "Invalid function return type: expected '{s}' found '{s}'",
                .{ expected, found },
            ),
            loc,
        );
        return error.InvalidReturnType;
    }

    pub fn invalidExpression(self: *Self, expected: []const u8, found: []const u8, loc: Loc) Errors {
        self.log(
            try allocPrint(
                self.allocator,
                "Invalid expression: expected {s} found {s}",
                .{ expected, found },
            ),
            loc,
        );
        return error.InvalidExpression;
    }

    pub fn invalidIndexing(self: *Self, expected: []const u8, found: []const u8, loc: Loc) Errors {
        self.log(
            try allocPrint(
                self.allocator,
                "Invalid indexing: expected {s} found {s}",
                .{ expected, found },
            ),
            loc,
        );
        return error.InvalidIndexing;
    }

    pub fn invalidAssignment(self: *Self, expected: []const u8, found: []const u8, loc: Loc) Errors {
        self.log(
            try allocPrint(
                self.allocator,
                "Invalid assignment: expected {s} found {s}",
                .{ expected, found },
            ),
            loc,
        );
        return error.InvalidAssignment;
    }

    pub fn typeNotDefined(self: *Self, found: []const u8, loc: Loc) Errors {
        self.log(
            try allocPrint(self.allocator, "Invalid type '{s}'", .{found}),
            loc,
        );
        return error.UnknownType;
    }

    pub fn invalidNumberOfArgs(self: *Self, expected: usize, found: usize, loc: Loc) Errors {
        self.log(
            try allocPrint(
                self.allocator,
                "Invalid number of arguments: expected {d} found {d}",
                .{ expected, found },
            ),
            loc,
        );
        return error.InvalidNumberOfArgs;
    }

    pub fn missingArgument(self: *Self, expected: []const u8, loc: Loc) Errors {
        self.log(
            try allocPrint(
                self.allocator,
                "Missing argument '{s}'",
                .{expected},
            ),
            loc,
        );
        return error.MissingArgument;
    }

    pub fn alreadyDefined(self: *Self, identifier: []const u8, loc: Loc) Errors {
        self.log(try allocPrint(self.allocator, "'{s}' is already defined", .{identifier}), loc);
        return error.AlreadyDefined;
    }

    pub fn notDefined(self: *Self, identifier: []const u8, loc: Loc) Errors {
        self.log(try allocPrint(self.allocator, "Cannot find name '{s}'", .{identifier}), loc);
        return error.NotDefined;
    }

    pub fn notMutable(self: *Self, identifier: []const u8, loc: Loc) Errors {
        self.log(try allocPrint(self.allocator, "'{s}' is not mutable", .{identifier}), loc);
        return error.NotMutable;
    }

    pub fn unwrappedValueCantBeMutable(self: *Self, identifier: []const u8, loc: Loc) Errors {
        self.log(try allocPrint(self.allocator, "Unwrapped value '{s}' cannot be mutable", .{identifier}), loc);
        return error.UnwrappedCantBeMutable;
    }

    pub fn selfCantBeUsedOutsideOfAstruct(self: *Self, loc: Loc) Errors {
        self.log("@ cannot be used outside of a struct", loc);
        return error.SelfOutsideStruct;
    }

    pub fn invalidStaticStructField(self: *Self, struct_name: []const u8, field: []const u8, loc: Loc) Errors {
        self.log(
            try allocPrint(
                self.allocator,
                "'{s}' is not a static field of struct '{s}'",
                .{ field, struct_name },
            ),
            loc,
        );
        return error.InvalidStructField;
    }

    pub fn invalidStructField(self: *Self, struct_name: []const u8, member: []const u8, loc: Loc) Errors {
        self.log(
            try allocPrint(
                self.allocator,
                "'{s}' does not exists on struct '{s}'",
                .{ member, struct_name },
            ),
            loc,
        );
        return error.InvalidStructField;
    }

    pub fn invalidStructFunction(self: *Self, struct_name: []const u8, function: []const u8, loc: Loc) Errors {
        self.log(
            try allocPrint(
                self.allocator,
                "Function '{s}' does not exists on struct '{s}'",
                .{ function, struct_name },
            ),
            loc,
        );
        return error.InvalidStructField;
    }

    pub fn cantInferAnonymousStruct(self: *Self, loc: Loc) Errors {
        self.log(
            "Can't infer anonymous struct type",
            loc,
        );
        return error.CantInferAnonymousStruct;
    }

    pub fn primitiveParamsCantBeMutable(self: *Self, loc: Loc) Errors {
        self.log(
            "Primitive params can't be mutable",
            loc,
        );
        return error.PrimitiveParamsCantBeMutable;
    }

    pub fn cantInferArrayLiteralType(self: *Self, loc: Loc) Errors {
        self.log(
            "Can't infer array literal type",
            loc,
        );
        return error.CantInferArrayLiteralType;
    }

    pub fn unnecessaryOptionalChain(self: *Self, member: []const u8, loc: Loc) Errors {
        self.log(
            try allocPrint(
                self.allocator,
                "Unecessary optional chain for '{s}'",
                .{member},
            ),
            loc,
        );
        return error.UnecessaryOptionalChain;
    }

    pub fn nullableMustBeUnwraped(self: *Self, member: []const u8, loc: Loc) Errors {
        self.log(
            try allocPrint(
                self.allocator,
                "{s} must be unwrapped",
                .{member},
            ),
            loc,
        );
        return error.NullableMustBeUnwrapped;
    }

    pub fn invalidParameterType(self: *Self, parameter_name: []const u8, loc: Loc) Errors {
        self.log(
            try allocPrint(
                self.allocator,
                "Parameter '{s}' must have a type",
                .{parameter_name},
            ),
            loc,
        );
        return error.InvalidParameterType;
    }

    pub fn log(self: *Self, msg: []const u8, loc: Loc) void {
        if (@import("builtin").is_test) return;

        const exactLoc = self.findExactLoc(loc.start);
        const start_col = 6 + loc.start - exactLoc.line.start;
        const range_len = if (loc.end > loc.start) (loc.end - loc.start) else 0;

        var error_loc_marker: [LINE_MAX_CHAR_COUNT]u8 = undefined;
        var error_loc_marker_len: usize = 0;

        const white_space_count = @min(start_col, LINE_MAX_CHAR_COUNT);
        @memcpy(error_loc_marker[0..white_space_count], padding[0..white_space_count]);
        error_loc_marker_len += white_space_count;

        error_loc_marker[error_loc_marker_len] = '^';
        error_loc_marker_len += 1;

        if (range_len > 0) {
            const tilde_count = @min(range_len, LINE_MAX_CHAR_COUNT - error_loc_marker_len - 1);
            @memset(error_loc_marker[error_loc_marker_len .. error_loc_marker_len + tilde_count], '~');
            error_loc_marker_len += tilde_count;
            error_loc_marker[error_loc_marker_len] = '^';
            error_loc_marker_len += 1;
        }

        std.log.err(
            "{s} at line {d} col {d}\n--> {d}: {s} {s}",
            .{
                msg,
                exactLoc.line.count,
                exactLoc.col,
                exactLoc.line.count,
                self.src[exactLoc.line.start .. exactLoc.line.end + 1],
                error_loc_marker[0..error_loc_marker_len],
            },
        );
    }

    fn findExactLoc(self: *Self, loc: usize) ExactLoc {
        var line_count: usize = 1;
        var line_start: usize = 0;
        var line_end: usize = self.src.len -| 1;

        var i: usize = 0;
        var should_break_in_next_line = false;
        while (i < self.src.len) {
            if (i == loc) should_break_in_next_line = true;
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
            col_count += 1;
            i += 1;
        }

        return .{
            .line = .{
                .count = line_count,
                .start = line_start,
                .end = line_end,
            },
            .col = col_count,
        };
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

    InvalidType,
    InvalidReturnType,
    InvalidExpression,
    InvalidIndexing,
    InvalidAssignment,
    UnknownType,
    InvalidNumberOfArgs,
    MissingArgument,
    AlreadyDefined,
    NotDefined,
    NotMutable,
    UnwrappedCantBeMutable,
    SelfOutsideStruct,
    InvalidStructField,
    CantInferAnonymousStruct,
    PrimitiveParamsCantBeMutable,
    CantInferArrayLiteralType,
    UnecessaryOptionalChain,
    NullableMustBeUnwrapped,
    InvalidParameterType,
};
