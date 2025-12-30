const BuiltInFn = struct {
    symbol: Symbol,
    code: []const u8,
};

pub const built_in_functions = [_]BuiltInFn{
    echo(),
    append(),
    len(),
};

fn echo() BuiltInFn {
    const symbol = Symbol{
        .uid = 0,
        .identifier = "echo",
        .type = &fn_type,
        .is_mut = false,
        .metadata = .{
            .params_types = echo_params,
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
            .params_types = append_params[0..],
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
            .params_types = len_params[0..],
            .return_type = &number_type,
        },
    };

    const code = std.fmt.comptimePrint(
        "function {s}_{d}(arr) {{return arr.length;}}\n",
        .{ symbol.identifier, symbol.uid },
    );

    return BuiltInFn{ .symbol = symbol, .code = code[0..] };
}

var fn_type = Type{ .function = {} };
var void_type = Type{ .void = {} };
var number_type = Type{ .number = {} };
var _anytype = Type{ .dynamic = {} };
var any_array_type = Type{ .array = &_anytype };

const echo_params: []const *Type = &[_]*Type{
    &_anytype,
};

const len_params: []const *Type = &[_]*Type{
    &any_array_type,
};

const append_params: []const *Type = &[_]*Type{ &any_array_type, &_anytype };

const sema = @import("sema.zig");
const Symbol = sema.Symbol;
const Type = sema.Type;
const std = @import("std");
