const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const native = @import("../native.zig");
const api = @import("../../io/sqlite/sqlite_api.zig");
const sqlite = @import("../../io/sqlite/sqlite.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const bytesArg = native.bytesArg;
const SqliteError = sqlite.SqliteError;

pub const bindings = native.BindingMap.initComptime(.{
    .{ "version", Binding{ .native = &version } },
    .{ "open_handle", Binding{ .native = &openHandle } },
    .{ "execute_handle", Binding{ .native = &executeHandle } },
    .{ "execute_with_handle", Binding{ .native = &executeWithHandle } },
    .{ "execute_many_handle", Binding{ .native = &executeManyHandle } },
    .{ "query_handle", Binding{ .native = &queryHandle } },
    .{ "begin_handle", Binding{ .native = &beginHandle } },
    .{ "commit_handle", Binding{ .native = &commitHandle } },
    .{ "rollback_handle", Binding{ .native = &rollbackHandle } },
    .{ "result_rows", Binding{ .native = &resultRows } },
    .{ "result_columns", Binding{ .native = &resultColumns } },
    .{ "result_free", Binding{ .native = &resultFree } },
    .{ "cell_kind", Binding{ .native = &cellKind } },
    .{ "cell_int", Binding{ .native = &cellInt } },
    .{ "cell_float", Binding{ .native = &cellFloat } },
    .{ "cell_text", Binding{ .native = &cellText } },
    .{ "close_handle", Binding{ .native = &closeHandle } },
    .{ "has_failure", Binding{ .native = &hasFailure } },
    .{ "error_offset", Binding{ .native = &errorOffset } },
    .{ "take_error_message", Binding{ .native = &takeErrorMessage } },
    .{ "own", Binding{ .native = &native.ownResource } },
    .{ "release", Binding{ .native = &native.releaseResource } },
});

pub const error_sets = .{SqliteError};

fn version(vm: *VM, _: []v.Value) anyerror!NativeResult {
    return native.ok(try vm.allocBytesValue(std.mem.span(api.sqlite3_libversion()), .string));
}

fn openHandle(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const path = bytesArg(args, 0);
    if (std.mem.indexOfScalar(u8, path, 0) != null) {
        return native.fail(SqliteError.CantOpen);
    }
    const settings: sqlite.OpenSettings = .{
        .journal = @enumFromInt(args[1].int64),
        .has_foreign_keys = args[2].bool,
        .busy_timeout_ms = args[3].int64,
        .is_read_only = args[4].bool,
        .can_create = args[5].bool,
    };
    const task = vm.running_task.?;
    try vm.event_loop.sqlite.open(&vm.event_loop, &vm.scheduler, task, path, settings);
    task.park();
    return native.parked();
}

fn executeHandle(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const sql = bytesArg(args, 1);
    if (std.mem.indexOfScalar(u8, sql, 0) != null) {
        return native.fail(SqliteError.InvalidSql);
    }
    const task = vm.running_task.?;
    const is_queued = try vm.event_loop.sqlite.execute(&vm.event_loop, &vm.scheduler, task, args[0].int64, sql, args[2].bool, args[3].int64);
    if (!is_queued) {
        return native.fail(SqliteError.Closed);
    }
    task.park();
    return native.parked();
}

fn executeWithHandle(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const sql = bytesArg(args, 1);
    if (std.mem.indexOfScalar(u8, sql, 0) != null) {
        return native.fail(SqliteError.InvalidSql);
    }
    const allocator = vm.event_loop.allocator;
    const params = try copyParams(allocator, args[2..7], null);
    const task = vm.running_task.?;
    const is_queued = vm.event_loop.sqlite.executeWith(&vm.event_loop, &vm.scheduler, task, args[0].int64, sql, params, args[7].bool, args[8].int64) catch |failure| {
        params.deinit(allocator);
        return failure;
    };
    if (!is_queued) {
        params.deinit(allocator);
        return native.fail(SqliteError.Closed);
    }
    task.park();
    return native.parked();
}

fn executeManyHandle(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const sql = bytesArg(args, 1);
    if (std.mem.indexOfScalar(u8, sql, 0) != null) {
        return native.fail(SqliteError.InvalidSql);
    }
    const allocator = vm.event_loop.allocator;
    const params = try copyParams(allocator, args[2..7], args[7]);
    const task = vm.running_task.?;
    const is_queued = vm.event_loop.sqlite.executeMany(&vm.event_loop, &vm.scheduler, task, args[0].int64, sql, params, args[8].bool, args[9].int64) catch |failure| {
        params.deinit(allocator);
        return failure;
    };
    if (!is_queued) {
        params.deinit(allocator);
        return native.fail(SqliteError.Closed);
    }
    task.park();
    return native.parked();
}

fn queryHandle(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const sql = bytesArg(args, 1);
    if (std.mem.indexOfScalar(u8, sql, 0) != null) {
        return native.fail(SqliteError.InvalidSql);
    }
    const allocator = vm.event_loop.allocator;
    const params = try copyParams(allocator, args[2..7], null);
    const task = vm.running_task.?;
    const is_queued = vm.event_loop.sqlite.query(&vm.event_loop, &vm.scheduler, task, args[0].int64, sql, params, args[7].int64, args[8].bool, args[9].int64) catch |failure| {
        params.deinit(allocator);
        return failure;
    };
    if (!is_queued) {
        params.deinit(allocator);
        return native.fail(SqliteError.Closed);
    }
    task.park();
    return native.parked();
}

fn beginHandle(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const task = vm.running_task.?;
    const is_queued = try vm.event_loop.sqlite.begin(&vm.event_loop, &vm.scheduler, task, args[0].int64);
    if (!is_queued) {
        return native.fail(SqliteError.Closed);
    }
    task.park();
    return native.parked();
}

fn commitHandle(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const task = vm.running_task.?;
    const is_queued = try vm.event_loop.sqlite.commit(&vm.event_loop, &vm.scheduler, task, args[0].int64);
    if (!is_queued) {
        return native.fail(SqliteError.Closed);
    }
    task.park();
    return native.parked();
}

fn rollbackHandle(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const task = vm.running_task.?;
    const is_queued = try vm.event_loop.sqlite.rollback(&vm.event_loop, &vm.scheduler, task, args[0].int64);
    if (!is_queued) {
        return native.ok(v.NULL_VALUE);
    }
    task.park();
    return native.parked();
}

fn resultRows(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const result = vm.event_loop.sqlite.resultAt(args[0].int64) orelse return native.ok(.{ .int64 = 0 });
    return native.ok(.{ .int64 = @intCast(result.rowCount()) });
}

fn resultColumns(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const result = vm.event_loop.sqlite.resultAt(args[0].int64) orelse return native.ok(try vm.allocStringArray(&.{}));
    return native.ok(try vm.allocStringArray(result.column_names.items));
}

fn resultFree(vm: *VM, args: []v.Value) anyerror!NativeResult {
    vm.event_loop.sqlite.freeResult(args[0].int64);
    return native.ok(v.NULL_VALUE);
}

fn cellOf(vm: *VM, args: []v.Value) ?sqlite.ResultSet.Cell {
    const result = vm.event_loop.sqlite.resultAt(args[0].int64) orelse return null;
    return result.cellAt(args[1].int64, args[2].int64);
}

fn cellKind(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const cell = cellOf(vm, args) orelse return native.ok(.{ .int64 = @intFromEnum(sqlite.ResultSet.CellKind.null) });
    return native.ok(.{ .int64 = @intFromEnum(cell.kind) });
}

fn cellInt(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const cell = cellOf(vm, args) orelse return native.ok(.{ .int64 = 0 });
    return native.ok(.{ .int64 = cell.int });
}

fn cellFloat(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const cell = cellOf(vm, args) orelse return native.ok(.{ .float64 = 0 });
    const value: f64 = if (cell.kind == .int) @floatFromInt(cell.int) else cell.float;
    return native.ok(.{ .float64 = value });
}

fn cellText(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const cell = cellOf(vm, args) orelse return native.ok(v.NULL_VALUE);
    const result = vm.event_loop.sqlite.resultAt(args[0].int64).?;
    const text = result.bytesOf(cell);
    if (!std.unicode.utf8ValidateSlice(text)) {
        return native.ok(v.NULL_VALUE);
    }
    return native.ok(try vm.allocBytesValue(text, .string));
}

fn copyParams(allocator: std.mem.Allocator, arrays: []v.Value, row_counts_value: ?v.Value) !sqlite.ParamBatch {
    const names = itemsOf(arrays[0]);
    const kinds = itemsOf(arrays[1]);
    const ints = itemsOf(arrays[2]);
    const floats = itemsOf(arrays[3]);
    const texts = itemsOf(arrays[4]);
    var total_bytes: usize = 0;
    var text_count: usize = 0;
    for (kinds, names) |kind_value, name_value| {
        total_bytes += textOf(name_value).len + 2;
        if (@as(sqlite.Param.Kind, @enumFromInt(kind_value.int64)) == .text) {
            total_bytes += textOf(texts[text_count]).len;
            text_count += 1;
        }
    }
    const items = try allocator.alloc(sqlite.Param, kinds.len);
    errdefer allocator.free(items);
    const bytes = try allocator.alloc(u8, total_bytes);
    errdefer allocator.free(bytes);
    var row_counts: []usize = &.{};
    if (row_counts_value) |value| {
        const raw_counts = itemsOf(value);
        row_counts = try allocator.alloc(usize, raw_counts.len);
        for (raw_counts, row_counts) |item, *dest| {
            dest.* = @intCast(item.int64);
        }
    }
    errdefer if (row_counts.len > 0) allocator.free(row_counts);
    var cursor: usize = 0;
    var next_int: usize = 0;
    var next_float: usize = 0;
    var next_text: usize = 0;
    for (kinds, names, items) |kind_value, name_value, *param| {
        const raw_name = textOf(name_value);
        const name_start = cursor;
        bytes[cursor] = ':';
        cursor += 1;
        @memcpy(bytes[cursor..][0..raw_name.len], raw_name);
        cursor += raw_name.len;
        bytes[cursor] = 0;
        const name = bytes[name_start..cursor :0];
        cursor += 1;
        param.* = .{ .name = name, .kind = @enumFromInt(kind_value.int64) };
        switch (param.kind) {
            .null => {},
            .int => {
                param.int = ints[next_int].int64;
                next_int += 1;
            },
            .float => {
                param.float = floats[next_float].float64;
                next_float += 1;
            },
            .text => {
                const raw_text = textOf(texts[next_text]);
                const text_start = cursor;
                @memcpy(bytes[cursor..][0..raw_text.len], raw_text);
                cursor += raw_text.len;
                param.text = bytes[text_start..cursor];
                next_text += 1;
            },
        }
    }
    return .{ .items = items, .bytes = bytes, .row_counts = row_counts };
}

fn itemsOf(value: v.Value) []v.Value {
    const array = v.HeapValue.getParentPtr(v.Array, value.heap_obj.followRelocated());
    return array.getDataPtr()[0..array.len];
}

fn textOf(value: v.Value) []const u8 {
    return v.HeapValue.getParentPtr(v.Bytes, value.heap_obj.followRelocated()).slice();
}

fn closeHandle(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const task = vm.running_task.?;
    const is_queued = try vm.event_loop.sqlite.close(&vm.event_loop, &vm.scheduler, task, args[0].int64);
    if (!is_queued) {
        return native.ok(v.NULL_VALUE);
    }
    task.park();
    return native.parked();
}

fn hasFailure(vm: *VM, _: []v.Value) anyerror!NativeResult {
    return native.ok(.{ .bool = vm.event_loop.sqlite.hasFailure(vm.running_task.?) });
}

fn errorOffset(vm: *VM, _: []v.Value) anyerror!NativeResult {
    return native.ok(.{ .int64 = vm.event_loop.sqlite.failureOffset(vm.running_task.?) });
}

fn takeErrorMessage(vm: *VM, _: []v.Value) anyerror!NativeResult {
    const failure = vm.event_loop.sqlite.takeFailure(vm.running_task.?) orelse return native.ok(try vm.allocBytesValue("", .string));
    defer sqlite.freeMessage(failure.message);
    return native.ok(try vm.allocBytesValue(failure.message orelse "", .string));
}
