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
        .metadata = .{
            .params = echo_params,
            .return_type = &void_type,
        },
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
            .params = append_params[0..],
            .return_type = &void_type,
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
            .params = len_params[0..],
            .return_type = &number_type,
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
            .params = floor_params,
            .return_type = &number_type,
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
            .params = concat_params,
            .return_type = &string_type,
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
var _anytype = Type{ .dynamic = {} };
var any_array_type = Type{ .array = &_anytype };

const echo_params: []const *Type = &[_]*Type{
    &_anytype,
};

const len_params: []const *Type = &[_]*Type{
    &any_array_type,
};

const append_params: []const *Type = &[_]*Type{ &any_array_type, &_anytype };

const floor_params: []const *Type = &[_]*Type{
    &number_type,
};

const concat_params: []const *Type = &[_]*Type{
    &string_type,
    &string_type,
};

const sema = @import("sema.zig");
const Symbol = sema.Symbol;
const Type = sema.Type;
const std = @import("std");
