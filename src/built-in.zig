const BuiltInFn = struct {
    symbol: Symbol,
    code: []const u8,
};

pub const built_in_functions = [_]BuiltInFn{
    echo(),
    append(),
    len(),
    floor(),
    concat(),
};

fn echo() BuiltInFn {
    const symbol = Symbol{
        .uid = 0,
        .identifier = "echo",
        .type = &fn_type,
        .is_mut = false,
        .metadata = .{ .func = .{
            .params = echo_params,
            .return_type = &void_type,
        } },
    };

    const code = std.fmt.comptimePrint(
        "function {s}_{d}(...msg) {{echo(...msg);}}\n",
        .{ symbol.identifier, symbol.uid },
    );

    return BuiltInFn{ .symbol = symbol, .code = code[0..] };
}

pub fn append() BuiltInFn {
    const symbol = Symbol{
        .uid = 1,
        .identifier = "append",
        .type = &fn_type,
        .is_mut = false,
        .metadata = .{
            .func = .{
                .params = append_params,
                .return_type = &void_type,
            },
        },
    };

    const code = std.fmt.comptimePrint(
        "function {s}_{d}(arr, item) {{return arr.push(item);}}\n",
        .{ symbol.identifier, symbol.uid },
    );

    return BuiltInFn{ .symbol = symbol, .code = code[0..] };
}

pub fn len() BuiltInFn {
    const symbol = Symbol{
        .uid = 2,
        .identifier = "len",
        .type = &fn_type,
        .is_mut = false,
        .metadata = .{
            .func = .{
                .params = len_params,
                .return_type = &number_type,
            },
        },
    };

    const code = std.fmt.comptimePrint(
        "function {s}_{d}(arr) {{return arr.length;}}\n",
        .{ symbol.identifier, symbol.uid },
    );

    return BuiltInFn{ .symbol = symbol, .code = code[0..] };
}

fn floor() BuiltInFn {
    const symbol = Symbol{
        .uid = 3,
        .identifier = "floor",
        .type = &fn_type,
        .is_mut = false,
        .metadata = .{
            .func = .{
                .params = floor_params,
                .return_type = &number_type,
            },
        },
    };

    const code = std.fmt.comptimePrint(
        "function {s}_{d}(n) {{return Math.floor(n)}}\n",
        .{ symbol.identifier, symbol.uid },
    );

    return BuiltInFn{ .symbol = symbol, .code = code[0..] };
}

fn concat() BuiltInFn {
    const symbol = Symbol{
        .uid = 4,
        .identifier = "concat",
        .type = &fn_type,
        .is_mut = false,
        .metadata = .{
            .func = .{
                .params = concat_params,
                .return_type = &string_type,
            },
        },
    };

    const code = std.fmt.comptimePrint(
        "function {s}_{d}(s1, s2) {{return s1+s2;}}\n",
        .{ symbol.identifier, symbol.uid },
    );

    return BuiltInFn{ .symbol = symbol, .code = code[0..] };
}

var fn_type = Type{ .function = {} };
var void_type = Type{ .void = {} };
var number_type = Type{ .number = {} };
var string_type = Type{ .string = {} };
var anytype_ = Type{ .dynamic = {} };
var any_array_type = Type{ .array = &anytype_ };

const echo_params: []const sema.FuncParamMetadata = &.{
    .{ .identifier = "msgs", .type = &anytype_ },
};

const len_params: []const sema.FuncParamMetadata = &.{
    .{ .identifier = "array", .type = &any_array_type },
};

const append_params: []const sema.FuncParamMetadata = &.{
    .{ .identifier = "array", .type = &any_array_type },
    .{ .identifier = "value", .type = &anytype_ },
};

const floor_params: []const sema.FuncParamMetadata = &.{
    .{ .identifier = "n", .type = &number_type },
};

const concat_params: []const sema.FuncParamMetadata = &.{
    .{ .identifier = "str1", .type = &string_type },
    .{ .identifier = "str2", .type = &string_type },
};

const sema = @import("sema.zig");
const Symbol = sema.Symbol;
const Type = sema.Type;
const std = @import("std");
