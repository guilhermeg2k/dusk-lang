const std = @import("std");
const v = @import("value.zig");

pub fn printValue(writer: *std.Io.Writer, value: v.Value, ty: v.ValueType) std.Io.Writer.Error!void {
    switch (ty) {
        .int64 => try writer.print("{d}", .{value.int64}),
        .float64 => try writer.print("{d}", .{value.float64}),
        .bool => try writer.print("{}", .{value.bool}),
        .string => {
            const raw: u64 = @bitCast(value);
            if (raw == 0) {
                try writer.print("null", .{});
            } else {
                const str = v.HeapValue.getParentPtr(v.Bytes, value.heap_obj);
                try writer.print("{s}", .{str.slice()});
            }
        },
        .buffer => {
            const raw: u64 = @bitCast(value);
            if (raw == 0) {
                try writer.print("null", .{});
                return;
            }
            const bytes = v.HeapValue.getParentPtr(v.Bytes, value.heap_obj).slice();
            try writer.print("[", .{});
            for (bytes, 0..) |byte, i| {
                if (i > 0) {
                    try writer.print(", ", .{});
                }
                try writer.print("{d}", .{byte});
            }
            try writer.print("]", .{});
        },
        .null => try writer.print("null", .{}),
        .array => {
            const raw: u64 = @bitCast(value);
            if (raw == 0) {
                try writer.print("null", .{});
                return;
            }
            const array = v.HeapValue.getParentPtr(v.Array, value.heap_obj).newestCopy();
            const data = array.getDataPtr();
            try writer.print("[", .{});
            for (0..array.len) |i| {
                if (i > 0) {
                    try writer.print(", ", .{});
                }
                try printValue(writer, data[i], array.kind);
            }
            try writer.print("]", .{});
        },
        .nullable => {
            const raw: u64 = @bitCast(value);
            if (raw == 0) {
                try writer.print("null", .{});
                return;
            }
            // only primitives are boxed, nullable heap values are the object itself
            switch (value.heap_obj.kind) {
                .null_box => {
                    const nullable = v.HeapValue.getParentPtr(v.NullBox, value.heap_obj);
                    try printValue(writer, nullable.value, nullable.kind);
                },
                .string => try printValue(writer, value, .string),
                .buffer => try printValue(writer, value, .buffer),
                .array => try printValue(writer, value, .array),
                .@"struct" => try printValue(writer, value, .@"struct"),
                .@"union" => try printValue(writer, value, .@"union"),
                .closure => try printValue(writer, value, .function),
                .task => try printValue(writer, value, .task),
                .map => try printValue(writer, value, .map),
                .cell, .map_storage => unreachable,
            }
        },
        .@"struct" => {
            const raw: u64 = @bitCast(value);
            if (raw == 0) {
                try writer.print("null", .{});
            } else {
                const s = v.HeapValue.getParentPtr(v.Struct, value.heap_obj);
                try writer.print("{{ ", .{});
                for (0..s.field_count) |i| {
                    if (i > 0) {
                        try writer.print(", ", .{});
                    }
                    try writer.print("{d}", .{s.get(i).int64});
                }
                try writer.print(" }}", .{});
            }
        },
        .@"union" => {
            const raw: u64 = @bitCast(value);
            if (raw == 0) {
                try writer.print("null", .{});
            } else {
                try writer.print("<union>", .{});
            }
        },
        .cell => unreachable,
        .map => {
            const raw: u64 = @bitCast(value);
            const text = if (raw == 0) "null" else "<map>";
            try writer.print("{s}", .{text});
        },
        .task => {
            const raw: u64 = @bitCast(value);
            if (raw == 0) {
                try writer.print("null", .{});
                return;
            }
            const task = v.HeapValue.getParentPtr(v.Task, value.heap_obj);
            const text = if (task.state.status == .done) "<task done>" else "<task>";
            try writer.print("{s}", .{text});
        },
        .function => {
            const raw: u64 = @bitCast(value);
            if (raw == 0) {
                try writer.print("null", .{});
                return;
            }
            const name = v.HeapValue.getParentPtr(v.Closure, value.heap_obj).function.name;
            if (name.len == 0 or name[0] == '$') {
                try writer.print("<func>", .{});
                return;
            }
            try writer.print("<func {s}>", .{name});
        },
    }
}
