const std = @import("std");
const v = @import("value.zig");

pub fn printValue(writer: *std.Io.Writer, value: v.Value, ty: v.ValueType) void {
    switch (ty) {
        .int64 => writer.print("{d}", .{value.int64}) catch @panic("failed to print int64"),
        .float64 => writer.print("{d}", .{value.float64}) catch @panic("failed to print float64"),
        .bool => writer.print("{}", .{value.bool}) catch @panic("failed to print bool"),
        .string => {
            const raw: u64 = @bitCast(value);
            if (raw == 0) {
                writer.print("null", .{}) catch @panic("failed to print null");
            } else {
                const str = v.HeapValue.getParentPtr(v.Bytes, value.heap_obj);
                writer.print("{s}", .{str.slice()}) catch @panic("failed to print string");
            }
        },
        .buffer => {
            const raw: u64 = @bitCast(value);
            if (raw == 0) {
                writer.print("null", .{}) catch @panic("failed to print null");
                return;
            }
            const bytes = v.HeapValue.getParentPtr(v.Bytes, value.heap_obj).slice();
            writer.print("[", .{}) catch @panic("failed to print buffer bracket");
            for (bytes, 0..) |byte, i| {
                if (i > 0) {
                    writer.print(", ", .{}) catch @panic("failed to print buffer separator");
                }
                writer.print("{d}", .{byte}) catch @panic("failed to print buffer byte");
            }
            writer.print("]", .{}) catch @panic("failed to print buffer bracket");
        },
        .null => writer.print("null", .{}) catch @panic("failed to print null"),
        .array => {
            const raw: u64 = @bitCast(value);
            if (raw == 0) {
                writer.print("null", .{}) catch @panic("failed to print null");
                return;
            }
            const array = v.HeapValue.getParentPtr(v.Array, value.heap_obj.followRelocated());
            const data = array.getDataPtr();
            writer.print("[", .{}) catch @panic("failed to print array bracket");
            for (0..array.len) |i| {
                if (i > 0) {
                    writer.print(", ", .{}) catch @panic("failed to print array separator");
                }
                printValue(writer, data[i], array.kind);
            }
            writer.print("]", .{}) catch @panic("failed to print array bracket");
        },
        .nullable => {
            const raw: u64 = @bitCast(value);
            if (raw == 0) {
                writer.print("null", .{}) catch @panic("failed to print null");
                return;
            }
            // only primitives are boxed, nullable heap values are the object itself
            switch (value.heap_obj.kind) {
                .null_box => {
                    const nullable = v.HeapValue.getParentPtr(v.NullBox, value.heap_obj);
                    printValue(writer, nullable.value, nullable.kind);
                },
                .string => printValue(writer, value, .string),
                .buffer => printValue(writer, value, .buffer),
                .array => printValue(writer, value, .array),
                .@"struct" => printValue(writer, value, .@"struct"),
                .@"union" => printValue(writer, value, .@"union"),
                .closure => printValue(writer, value, .function),
                .task => printValue(writer, value, .task),
                .cell => unreachable,
            }
        },
        .@"struct" => {
            const raw: u64 = @bitCast(value);
            if (raw == 0) {
                writer.print("null", .{}) catch @panic("failed to print null");
            } else {
                const s = v.HeapValue.getParentPtr(v.Struct, value.heap_obj);
                writer.print("{{ ", .{}) catch @panic("failed to print struct brace");
                for (0..s.field_count) |i| {
                    if (i > 0) {
                        writer.print(", ", .{}) catch @panic("failed to print struct separator");
                    }
                    writer.print("{d}", .{s.get(i).int64}) catch @panic("failed to print struct field");
                }
                writer.print(" }}", .{}) catch @panic("failed to print struct brace");
            }
        },
        .@"union" => {
            const raw: u64 = @bitCast(value);
            if (raw == 0) {
                writer.print("null", .{}) catch @panic("failed to print null");
            } else {
                writer.print("<union>", .{}) catch @panic("failed to print union");
            }
        },
        .cell => unreachable,
        .task => {
            const raw: u64 = @bitCast(value);
            if (raw == 0) {
                writer.print("null", .{}) catch @panic("failed to print null");
                return;
            }
            const task = v.HeapValue.getParentPtr(v.Task, value.heap_obj);
            const text = if (task.state.status == .done) "<task done>" else "<task>";
            writer.print("{s}", .{text}) catch @panic("failed to print task");
        },
        .function => {
            const raw: u64 = @bitCast(value);
            if (raw == 0) {
                writer.print("null", .{}) catch @panic("failed to print null");
                return;
            }
            const name = v.HeapValue.getParentPtr(v.Closure, value.heap_obj).function.name;
            if (name.len == 0 or name[0] == '$') {
                writer.print("<func>", .{}) catch @panic("failed to print function");
                return;
            }
            writer.print("<func {s}>", .{name}) catch @panic("failed to print function");
        },
    }
}
