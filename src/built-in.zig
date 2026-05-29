pub const BuiltIn = struct {
    const Self = @This();

    alloc: std.mem.Allocator,

    pub fn generate(self: *const Self) ![7]BuiltInFn {
        return [_]BuiltInFn{
            try self.echo(),
            try self.append(),
            try self.len(),
            try self.floor(),
            try self.concat(),
            try self.stringify(),
            try self.assert(),
        };
    }

    fn echo(self: *const Self) !BuiltInFn {
        const symbol = try Symbol.init(self.alloc, .{
            .uid = 0,
            .identifier = "echo",
            .is_mut = false,
            .type = undefined,
        });

        symbol.type = try Type.init(self.alloc, .{
            .kind = .{ .function = .{
                .symbol = symbol,
                .params = echo_params,
                .return_type = &void_type,
            } },
            .nullable = false,
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
            .type = undefined,
            .is_mut = false,
        });

        symbol.type = try Type.init(self.alloc, .{
            .kind = .{
                .function = .{
                    .symbol = symbol,
                    .params = append_params,
                    .return_type = &void_type,
                },
            },
            .nullable = false,
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
            .is_mut = false,
            .type = undefined,
        });

        symbol.type = try Type.init(self.alloc, .{
            .kind = .{
                .function = .{
                    .symbol = symbol,
                    .params = len_params,
                    .return_type = &int_type,
                },
            },
            .nullable = false,
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
            .is_mut = false,
            .type = undefined,
        });

        symbol.type = try Type.init(self.alloc, .{
            .kind = .{
                .function = .{
                    .symbol = symbol,
                    .params = floor_params,
                    .return_type = &float_type,
                },
            },
            .nullable = false,
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
            .is_mut = false,
            .type = undefined,
        });

        symbol.type = try Type.init(self.alloc, .{
            .kind = .{
                .function = .{
                    .symbol = symbol,
                    .params = concat_params,
                    .return_type = &string_type,
                },
            },
            .nullable = false,
        });

        const code = try std.fmt.allocPrint(
            self.alloc,
            "function {s}_{d}(s1, s2) {{return s1+s2;}}\n",
            .{ symbol.identifier, symbol.uid },
        );

        return BuiltInFn{ .symbol = symbol, .code = code };
    }

    fn stringify(self: *const Self) !BuiltInFn {
        const symbol = try Symbol.init(self.alloc, .{
            .uid = 5,
            .identifier = "stringify",
            .type = undefined,
            .is_mut = false,
        });

        symbol.type = try Type.init(self.alloc, .{
            .kind = .{
                .function = .{
                    .symbol = symbol,
                    .params = stringify_params,
                    .return_type = &string_type,
                },
            },
            .nullable = false,
        });

        const code = try std.fmt.allocPrint(
            self.alloc,
            "function {s}_{d}(obj) {{try {{return JSON.stringify(obj);}} catch {{return \"INVALID JSON\";}};}}\n",
            .{ symbol.identifier, symbol.uid },
        );

        return BuiltInFn{ .symbol = symbol, .code = code };
    }

    fn assert(self: *const Self) !BuiltInFn {
        const symbol = try Symbol.init(self.alloc, .{
            .uid = 6,
            .identifier = "assert",
            .type = undefined,
            .is_mut = false,
        });

        symbol.type = try Type.init(self.alloc, .{
            .kind = .{
                .function = .{
                    .symbol = symbol,
                    .params = assert_params,
                    .return_type = &void_type,
                },
            },
            .nullable = false,
        });

        const code = try std.fmt.allocPrint(
            self.alloc,
            "function {s}_{d}(cond) {{if(cond==false) throw 'ASSERTION_FAILED'}}\n",
            .{ symbol.identifier, symbol.uid },
        );

        return BuiltInFn{ .symbol = symbol, .code = code };
    }

    const echo_params: []const sema.TypedIdentifier = &.{
        .{
            .identifier = "msgs",
            .type = &dynamic,
            .is_mut = false,
            .default_value = null,
        },
    };

    const len_params: []const sema.TypedIdentifier = &.{
        .{
            .identifier = "array",
            .type = &dynamic_array_type,
            .is_mut = false,
            .default_value = null,
        },
    };

    const append_params: []const sema.TypedIdentifier = &.{
        .{
            .identifier = "array",
            .type = &dynamic_array_type,
            .is_mut = true,
            .default_value = null,
        },
        .{
            .identifier = "value",
            .type = &dynamic,
            .is_mut = false,
            .default_value = null,
        },
    };

    const floor_params: []const sema.TypedIdentifier = &.{
        .{
            .identifier = "n",
            .type = &float_type,
            .is_mut = false,
            .default_value = null,
        },
    };

    const concat_params: []const sema.TypedIdentifier = &.{
        .{
            .identifier = "str1",
            .type = &string_type,
            .is_mut = false,
            .default_value = null,
        },
        .{
            .identifier = "str2",
            .type = &string_type,
            .is_mut = false,
            .default_value = null,
        },
    };

    const stringify_params: []const sema.TypedIdentifier = &.{
        .{
            .identifier = "obj",
            .type = &dynamic,
            .is_mut = false,
            .default_value = null,
        },
    };

    const assert_params: []const sema.TypedIdentifier = &.{
        .{
            .identifier = "cond",
            .type = &boolean_type,
            .is_mut = false,
            .default_value = null,
        },
    };

    var fn_type = Type{ .kind = .{ .function_def = {} } };
    var void_type = Type{ .kind = .{ .void = {} }, .nullable = false };
    var float_type = Type{ .kind = .{ .float = {} }, .nullable = false };
    var int_type = Type{ .kind = .{ .int = {} }, .nullable = false };
    var string_type = Type{ .kind = .{ .string = {} }, .nullable = false };
    var boolean_type = Type{ .kind = .{ .boolean = {} }, .nullable = false };
    var dynamic = Type{ .kind = .{ .dynamic = {} }, .nullable = false };
    var dynamic_array_type = Type{ .kind = .{ .array = &dynamic }, .nullable = false };
};

const BuiltInFn = struct {
    symbol: *Symbol,
    code: []const u8,
};

const sema = @import("sema.zig");
const Symbol = sema.Symbol;
const Type = sema.Type;
const std = @import("std");
