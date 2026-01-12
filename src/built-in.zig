pub const BuiltIn = struct {
    const Self = @This();

    alloc: std.mem.Allocator,

    pub fn generate(self: *const Self) ![5]BuiltInFn {
        return [_]BuiltInFn{
            try self.echo(),
            try self.append(),
            try self.len(),
            try self.floor(),
            try self.concat(),
        };
    }

    fn echo(self: *const Self) !BuiltInFn {
        const symbol = try Symbol.init(self.alloc, .{
            .uid = 0,
            .identifier = "echo",
            .type = try Type.init(self.alloc, .{
                .function = .{
                    .identifier = "echo",
                    .params = echo_params,
                    .return_type = &void_type,
                },
            }),
            .is_mut = false,
        });

        const code = try std.fmt.allocPrint(
            self.alloc,
            "function {s}_{d}(...msg) {{echo(...msg);}}\n",
            .{ symbol.identifier, symbol.uid },
        );

        return BuiltInFn{ .symbol = symbol, .code = code };
    }

    fn append(self: *const Self) !BuiltInFn {
        const symbol = try Symbol.init(self.alloc, .{
            .uid = 1,
            .identifier = "append",
            .type = try Type.init(self.alloc, .{
                .function = .{
                    .identifier = "append",
                    .params = append_params,
                    .return_type = &void_type,
                },
            }),
            .is_mut = false,
        });

        const code = try std.fmt.allocPrint(
            self.alloc,
            "function {s}_{d}(arr, item) {{return arr.push(item);}}\n",
            .{ symbol.identifier, symbol.uid },
        );

        return BuiltInFn{ .symbol = symbol, .code = code };
    }

    fn len(self: *const Self) !BuiltInFn {
        const symbol = try Symbol.init(self.alloc, .{
            .uid = 2,
            .identifier = "len",
            .type = try Type.init(self.alloc, .{
                .function = .{
                    .identifier = "len",
                    .params = len_params,
                    .return_type = &number_type,
                },
            }),
            .is_mut = false,
        });

        const code = try std.fmt.allocPrint(
            self.alloc,
            "function {s}_{d}(arr) {{return arr.length;}}\n",
            .{ symbol.identifier, symbol.uid },
        );

        return BuiltInFn{ .symbol = symbol, .code = code };
    }

    fn floor(self: *const Self) !BuiltInFn {
        const symbol = try Symbol.init(self.alloc, .{
            .uid = 3,
            .identifier = "floor",
            .type = try Type.init(self.alloc, .{
                .function = .{
                    .identifier = "floor",
                    .params = floor_params,
                    .return_type = &number_type,
                },
            }),
            .is_mut = false,
        });

        const code = try std.fmt.allocPrint(
            self.alloc,
            "function {s}_{d}(n) {{return Math.floor(n)}}\n",
            .{ symbol.identifier, symbol.uid },
        );

        return BuiltInFn{ .symbol = symbol, .code = code };
    }

    fn concat(self: *const Self) !BuiltInFn {
        const symbol = try Symbol.init(self.alloc, .{
            .uid = 4,
            .identifier = "concat",
            .type = try Type.init(self.alloc, .{
                .function = .{
                    .identifier = "concat",
                    .params = concat_params,
                    .return_type = &string_type,
                },
            }),
            .is_mut = false,
        });

        const code = try std.fmt.allocPrint(
            self.alloc,
            "function {s}_{d}(s1, s2) {{return s1+s2;}}\n",
            .{ symbol.identifier, symbol.uid },
        );

        return BuiltInFn{ .symbol = symbol, .code = code };
    }

    var fn_type = Type{ .function_def = {} };
    var void_type = Type{ .void = {} };
    var number_type = Type{ .number = {} };
    var string_type = Type{ .string = {} };
    var dynamic = Type{ .dynamic = {} };
    var any_array_type = Type{ .array = &dynamic };

    const echo_params: []const sema.TypedIdentifier = &.{
        .{ .identifier = "msgs", .type = &dynamic },
    };

    const len_params: []const sema.TypedIdentifier = &.{
        .{ .identifier = "array", .type = &any_array_type },
    };

    const append_params: []const sema.TypedIdentifier = &.{
        .{ .identifier = "array", .type = &any_array_type },
        .{ .identifier = "value", .type = &dynamic },
    };

    const floor_params: []const sema.TypedIdentifier = &.{
        .{ .identifier = "n", .type = &number_type },
    };

    const concat_params: []const sema.TypedIdentifier = &.{
        .{ .identifier = "str1", .type = &string_type },
        .{ .identifier = "str2", .type = &string_type },
    };
};

const BuiltInFn = struct {
    symbol: *Symbol,
    code: []const u8,
};

const sema = @import("sema.zig");
const Symbol = sema.Symbol;
const Type = sema.Type;
const std = @import("std");
