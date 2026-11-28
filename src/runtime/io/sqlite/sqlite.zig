const std = @import("std");
const api = @import("sqlite_api.zig");
const uv = @import("../../tasks/loop/uv.zig");
const v = @import("../../value.zig");
const RuntimeErrorReporter = @import("../../runtime_error.zig").RuntimeErrorReporter;
const EventLoop = @import("../../tasks/event_loop.zig").EventLoop;
const HandleOwner = @import("../../tasks/event_loop.zig").HandleOwner;
const onHandleClosed = @import("../../tasks/event_loop.zig").onHandleClosed;
const Scheduler = @import("../../tasks/scheduler.zig").Scheduler;
const TaskState = @import("../../tasks/scheduler.zig").TaskState;
const WakeResult = @import("../../tasks/scheduler.zig").WakeResult;
const SlotTable = @import("../../tasks/loop/slot_table.zig").SlotTable;
const WorkerDispatch = @import("../../tasks/loop/worker_dispatch.zig").WorkerDispatch;
const Job = @import("../../tasks/loop/worker_pool.zig").Job;
const WorkerPool = @import("../../tasks/loop/worker_pool.zig").WorkerPool;

const handle_alignment = std.mem.Alignment.@"16";

pub const SqliteError = enum { CantOpen, ReadOnly, Busy, Constraint, InvalidSql, MissingParameter, UnknownParameter, TypeMismatch, MissingColumn, TooLarge, TimedOut, Full, Corrupt, Closed, Other };

pub const Journal = enum { wal, delete };

pub const OpenSettings = struct {
    journal: Journal = .wal,
    has_foreign_keys: bool = true,
    busy_timeout_ms: i64 = 5000,
    is_read_only: bool = false,
    can_create: bool = true,
};

pub const Param = struct {
    pub const Kind = enum { null, int, float, text };

    name: [:0]const u8,
    kind: Kind,
    int: i64 = 0,
    float: f64 = 0,
    text: []const u8 = &.{},
};

pub const ParamBatch = struct {
    items: []Param = &.{},
    bytes: []u8 = &.{},
    row_counts: []usize = &.{},

    pub fn deinit(self: ParamBatch, allocator: std.mem.Allocator) void {
        if (self.items.len > 0) {
            allocator.free(self.items);
        }
        if (self.bytes.len > 0) {
            allocator.free(self.bytes);
        }
        if (self.row_counts.len > 0) {
            allocator.free(self.row_counts);
        }
    }
};

pub const ResultSet = struct {
    pub const CellKind = enum { null, int, float, text, blob };

    pub const Cell = struct {
        kind: CellKind,
        int: i64 = 0,
        float: f64 = 0,
        start: usize = 0,
        len: usize = 0,
    };

    column_names: std.ArrayList([]u8) = .empty,
    cells: std.ArrayList(Cell) = .empty,
    bytes: std.ArrayList(u8) = .empty,

    fn create() !*ResultSet {
        const result = try std.heap.c_allocator.create(ResultSet);
        result.* = .{};
        return result;
    }

    pub fn destroy(self: *ResultSet) void {
        const allocator = std.heap.c_allocator;
        for (self.column_names.items) |name| {
            allocator.free(name);
        }
        self.column_names.deinit(allocator);
        self.cells.deinit(allocator);
        self.bytes.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn rowCount(self: *const ResultSet) usize {
        const column_count = self.column_names.items.len;
        if (column_count == 0) {
            return 0;
        }
        return self.cells.items.len / column_count;
    }

    pub fn cellAt(self: *const ResultSet, row: i64, column: i64) ?Cell {
        const column_count = self.column_names.items.len;
        const is_inside = row >= 0 and column >= 0 and row < self.rowCount() and column < column_count;
        if (!is_inside) {
            return null;
        }
        return self.cells.items[@as(usize, @intCast(row)) * column_count + @as(usize, @intCast(column))];
    }

    pub fn bytesOf(self: *const ResultSet, cell: Cell) []const u8 {
        return self.bytes.items[cell.start..][0..cell.len];
    }
};

pub const Failure = struct {
    message: ?[]u8,
    offset: i64,
};

const pool_options: WorkerPool.Options = .{ .thread_count = 4, .max_outstanding = 64 };

pub const Databases = struct {
    const Self = @This();

    connections: SlotTable(Connection) = .{},
    results: SlotTable(ResultSet) = .{},
    dispatch: ?*WorkerDispatch = null,
    blocked: std.ArrayList(*Connection) = .empty,
    failures: std.AutoHashMapUnmanaged(*TaskState, Failure) = .empty,

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        for (self.connections.entries.items) |entry| {
            const connection = entry orelse continue;
            connection.closeNow(allocator);
        }
        self.connections.deinit(allocator);
        for (self.results.entries.items) |entry| {
            const result = entry orelse continue;
            result.destroy();
        }
        self.results.deinit(allocator);
        self.blocked.deinit(allocator);
        var failures = self.failures.valueIterator();
        while (failures.next()) |failure| {
            freeMessage(failure.message);
        }
        self.failures.deinit(allocator);
    }

    pub fn interruptAll(self: *Self) void {
        for (self.connections.entries.items) |entry| {
            const connection = entry orelse continue;
            const operation = connection.running_operation orelse continue;
            operation.is_interrupted.store(true, .release);
            operation.stopTimer();
            if (connection.database) |database| {
                api.sqlite3_interrupt(database);
            }
        }
    }

    pub fn open(self: *Self, event_loop: *EventLoop, scheduler: *Scheduler, task: *TaskState, path: []const u8, settings: OpenSettings) !void {
        try self.ensureDispatch(event_loop);
        const allocator = event_loop.allocator;
        try self.connections.reserve(allocator);
        try self.blocked.ensureTotalCapacity(allocator, self.connections.liveCount() + 1);
        const connection = try allocator.create(Connection);
        connection.* = .{};
        errdefer allocator.destroy(connection);
        const operation = try Operation.create(event_loop, scheduler, task, connection, .open, path);
        operation.settings = settings;
        connection.handle = self.connections.addAssumeCapacity(connection);
        self.enqueue(connection, operation);
    }

    pub fn execute(self: *Self, event_loop: *EventLoop, scheduler: *Scheduler, task: *TaskState, handle: i64, sql: []const u8, is_in_transaction: bool, timeout_ms: i64) !bool {
        const connection = try self.usableConnection(handle, task, is_in_transaction) orelse return false;
        const operation = try Operation.create(event_loop, scheduler, task, connection, .execute, sql);
        errdefer operation.destroy();
        try operation.startTimeout(timeout_ms);
        self.enqueue(connection, operation);
        return true;
    }

    pub fn executeWith(self: *Self, event_loop: *EventLoop, scheduler: *Scheduler, task: *TaskState, handle: i64, sql: []const u8, params: ParamBatch, is_in_transaction: bool, timeout_ms: i64) !bool {
        const connection = try self.usableConnection(handle, task, is_in_transaction) orelse return false;
        const operation = try Operation.create(event_loop, scheduler, task, connection, .execute_one, sql);
        errdefer operation.destroy();
        try operation.startTimeout(timeout_ms);
        operation.params = params;
        self.enqueue(connection, operation);
        return true;
    }

    pub fn executeMany(self: *Self, event_loop: *EventLoop, scheduler: *Scheduler, task: *TaskState, handle: i64, sql: []const u8, params: ParamBatch, is_in_transaction: bool, timeout_ms: i64) !bool {
        const connection = try self.usableConnection(handle, task, is_in_transaction) orelse return false;
        const operation = try Operation.create(event_loop, scheduler, task, connection, .execute_many, sql);
        errdefer operation.destroy();
        try operation.startTimeout(timeout_ms);
        operation.params = params;
        self.enqueue(connection, operation);
        return true;
    }

    pub fn query(self: *Self, event_loop: *EventLoop, scheduler: *Scheduler, task: *TaskState, handle: i64, sql: []const u8, params: ParamBatch, max_bytes: i64, is_in_transaction: bool, timeout_ms: i64) !bool {
        const connection = try self.usableConnection(handle, task, is_in_transaction) orelse return false;
        const operation = try Operation.create(event_loop, scheduler, task, connection, .query, sql);
        errdefer operation.destroy();
        try operation.startTimeout(timeout_ms);
        operation.params = params;
        operation.max_bytes = max_bytes;
        self.enqueue(connection, operation);
        return true;
    }

    pub fn begin(self: *Self, event_loop: *EventLoop, scheduler: *Scheduler, task: *TaskState, handle: i64) !bool {
        const connection = try self.usableConnection(handle, task, false) orelse return false;
        const operation = try Operation.create(event_loop, scheduler, task, connection, .begin, "BEGIN IMMEDIATE");
        self.enqueue(connection, operation);
        return true;
    }

    pub fn commit(self: *Self, event_loop: *EventLoop, scheduler: *Scheduler, task: *TaskState, handle: i64) !bool {
        const connection = try self.usableConnection(handle, task, true) orelse return false;
        const operation = try Operation.create(event_loop, scheduler, task, connection, .commit, "COMMIT");
        self.enqueue(connection, operation);
        return true;
    }

    pub fn rollback(self: *Self, event_loop: *EventLoop, scheduler: *Scheduler, task: *TaskState, handle: i64) !bool {
        const connection = self.connections.get(handle) orelse {
            return false;
        };
        if (connection.transaction_owner != task) {
            return false;
        }
        const operation = try Operation.create(event_loop, scheduler, task, connection, .rollback, "ROLLBACK");
        self.enqueue(connection, operation);
        return true;
    }

    pub fn resultAt(self: *Self, handle: i64) ?*ResultSet {
        return self.results.get(handle);
    }

    pub fn freeResult(self: *Self, handle: i64) void {
        const result = self.results.remove(handle) orelse return;
        result.destroy();
    }

    pub fn close(self: *Self, event_loop: *EventLoop, scheduler: ?*Scheduler, task: ?*TaskState, handle: i64) !bool {
        const connection = self.openConnection(handle) orelse return false;
        if (task != null and connection.transaction_owner == task) {
            return RuntimeErrorReporter.sqliteDatabaseInTransaction();
        }
        const operation = try Operation.create(event_loop, scheduler, task, connection, .close, "");
        connection.is_closing = true;
        self.enqueue(connection, operation);
        return true;
    }

    pub fn hasFailure(self: *Self, task: *TaskState) bool {
        return self.failures.contains(task);
    }

    pub fn takeFailure(self: *Self, task: *TaskState) ?Failure {
        const entry = self.failures.fetchRemove(task) orelse return null;
        return entry.value;
    }

    pub fn failureOffset(self: *Self, task: *TaskState) i64 {
        const failure = self.failures.get(task) orelse return -1;
        return failure.offset;
    }

    fn ensureDispatch(self: *Self, event_loop: *EventLoop) !void {
        if (self.dispatch != null) return;
        self.dispatch = try event_loop.startWorkerDispatch(pool_options);
    }

    fn openConnection(self: *Self, handle: i64) ?*Connection {
        const connection = self.connections.get(handle) orelse return null;
        if (connection.is_closing) {
            return null;
        }
        return connection;
    }

    fn usableConnection(self: *Self, handle: i64, task: *TaskState, is_in_transaction: bool) !?*Connection {
        if (!is_in_transaction) {
            const connection = self.openConnection(handle) orelse return null;
            if (connection.transaction_owner == task) {
                return RuntimeErrorReporter.sqliteDatabaseInTransaction();
            }
            return connection;
        }
        const connection = self.connections.get(handle) orelse return RuntimeErrorReporter.sqliteTransactionClosed();
        if (connection.transaction_owner != task) {
            return RuntimeErrorReporter.sqliteTransactionClosed();
        }
        return connection;
    }

    fn enqueue(self: *Self, connection: *Connection, operation: *Operation) void {
        connection.queued.push(operation);
        self.submitNext(connection);
    }

    fn submitNext(self: *Self, connection: *Connection) void {
        if (connection.running_operation != null) return;
        const operation = connection.queued.peekReady(connection.transaction_owner) orelse return;
        operation.database = connection.database;
        operation.cache = &connection.cache;
        self.dispatch.?.submit(&operation.job) catch {
            self.markBlocked(connection);
            return;
        };
        connection.queued.remove(operation);
        connection.running_operation = operation;
    }

    fn markBlocked(self: *Self, connection: *Connection) void {
        for (self.blocked.items) |blocked| {
            if (blocked == connection) return;
        }
        self.blocked.appendAssumeCapacity(connection);
    }

    fn submitBlocked(self: *Self) void {
        var index: usize = 0;
        while (index < self.blocked.items.len) {
            const connection = self.blocked.items[index];
            self.submitNext(connection);
            if (connection.running_operation != null) {
                _ = self.blocked.swapRemove(index);
                continue;
            }
            index += 1;
        }
    }

    fn forgetConnection(self: *Self, connection: *Connection) void {
        _ = self.connections.remove(connection.handle);
        for (self.blocked.items, 0..) |blocked, index| {
            if (blocked != connection) continue;
            _ = self.blocked.swapRemove(index);
            break;
        }
    }

    fn recordFailure(self: *Self, allocator: std.mem.Allocator, task: *TaskState, failure: Failure) void {
        const entry = self.failures.getOrPut(allocator, task) catch {
            freeMessage(failure.message);
            return;
        };
        if (entry.found_existing) {
            freeMessage(entry.value_ptr.message);
        }
        entry.value_ptr.* = failure;
    }

    pub fn clearFailure(self: *Self, task: *TaskState) void {
        const failure = self.takeFailure(task) orelse return;
        freeMessage(failure.message);
    }
};

pub const Connection = struct {
    database: ?*api.Database = null,
    handle: i64 = 0,
    queued: OperationQueue = .{},
    transaction_owner: ?*TaskState = null,
    running_operation: ?*Operation = null,
    is_closing: bool = false,
    cache: StatementCache = .{},

    fn closeNow(self: *Connection, allocator: std.mem.Allocator) void {
        while (self.queued.pop()) |operation| {
            operation.stopTimer();
            operation.destroy();
        }
        self.cache.finalizeAll();
        if (self.database) |database| {
            _ = api.sqlite3_close_v2(database);
        }
        allocator.destroy(self);
    }
};

pub const OperationTimer = struct {
    owner: HandleOwner = .{ .kind = .sqlite_timer },
    event_loop: *EventLoop,
    operation: ?*Operation,
    timer_memory: []align(16) u8,

    fn start(event_loop: *EventLoop, operation: *Operation, timeout_ms: i64) !*OperationTimer {
        const loop = try event_loop.ensureLoopStarted();
        const allocator = event_loop.allocator;
        const entry = try allocator.create(OperationTimer);
        const timer_memory = allocator.alignedAlloc(u8, handle_alignment, uv.uv_handle_size(.timer)) catch |failure| {
            allocator.destroy(entry);
            return failure;
        };
        entry.* = .{ .event_loop = event_loop, .operation = operation, .timer_memory = timer_memory };
        const handle = entry.timer();
        if (uv.uv_timer_init(loop, handle) != 0) {
            allocator.free(timer_memory);
            allocator.destroy(entry);
            return error.TimerFailed;
        }
        uv.uv_handle_set_data(handle, &entry.owner);
        uv.uv_update_time(loop);
        if (uv.uv_timer_start(handle, onOperationTimeout, @intCast(timeout_ms), 0) != 0) {
            uv.uv_close(handle, onHandleClosed);
            return error.TimerFailed;
        }
        event_loop.pending_operations += 1;
        return entry;
    }

    fn timer(self: *OperationTimer) *uv.Handle {
        return @ptrCast(self.timer_memory.ptr);
    }

    pub fn destroy(self: *OperationTimer) void {
        const allocator = self.event_loop.allocator;
        allocator.free(self.timer_memory);
        allocator.destroy(self);
    }
};

fn onOperationTimeout(handle: *uv.Handle) callconv(.c) void {
    const entry: *OperationTimer = @alignCast(@fieldParentPtr("owner", HandleOwner.of(handle)));
    const event_loop = entry.event_loop;
    event_loop.pending_operations -= 1;
    uv.uv_close(handle, onHandleClosed);
    const operation = entry.operation orelse return;
    entry.operation = null;
    operation.timer = null;
    if (event_loop.is_shutting_down) return;
    const connection = operation.connection;
    if (connection.running_operation == operation) {
        operation.is_interrupted.store(true, .release);
        if (connection.database) |database| {
            api.sqlite3_interrupt(database);
        }
        return;
    }
    connection.queued.remove(operation);
    operation.recordFixedFailure(.TimedOut, "operation timed out after {d} ms", .{operation.timeout_ms});
    defer operation.destroy();
    const task = operation.task orelse return;
    const databases = &event_loop.sqlite;
    databases.recordFailure(event_loop.allocator, task, .{ .message = operation.message, .offset = operation.offset });
    operation.message = null;
    operation.scheduler.?.wake(task, failed(.TimedOut)) catch |failure| {
        event_loop.callback_failure = failure;
    };
}

const Operation = struct {
    const Kind = enum { open, execute, execute_one, execute_many, query, begin, commit, rollback, close };

    job: Job = .{ .execute = runOnWorker, .finish = finishOnLoop },
    kind: Kind,
    event_loop: *EventLoop,
    scheduler: ?*Scheduler,
    task: ?*TaskState,
    connection: *Connection,
    text: [:0]u8,
    settings: OpenSettings = .{},
    params: ParamBatch = .{},
    database: ?*api.Database = null,
    cache: ?*StatementCache = null,
    fixed_error: ?SqliteError = null,
    max_bytes: i64 = 0,
    timeout_ms: i64 = -1,
    timer: ?*OperationTimer = null,
    is_interrupted: std.atomic.Value(bool) = .init(false),
    result: ?*ResultSet = null,
    next: ?*Operation = null,
    code: c_int = 0,
    rows: i64 = 0,
    last_id: i64 = 0,
    message: ?[]u8 = null,
    offset: i64 = -1,

    fn create(event_loop: *EventLoop, scheduler: ?*Scheduler, task: ?*TaskState, connection: *Connection, kind: Kind, text: []const u8) !*Operation {
        const allocator = event_loop.allocator;
        const operation = try allocator.create(Operation);
        errdefer allocator.destroy(operation);
        operation.* = .{ .kind = kind, .event_loop = event_loop, .scheduler = scheduler, .task = task, .connection = connection, .text = try allocator.dupeZ(u8, text) };
        return operation;
    }

    fn startTimeout(self: *Operation, timeout_ms: i64) !void {
        if (timeout_ms < 0) return;
        self.timeout_ms = timeout_ms;
        self.timer = try OperationTimer.start(self.event_loop, self, timeout_ms);
    }

    fn stopTimer(self: *Operation) void {
        const entry = self.timer orelse return;
        self.timer = null;
        entry.operation = null;
        _ = uv.uv_timer_stop(entry.timer());
        self.event_loop.pending_operations -= 1;
        uv.uv_close(entry.timer(), onHandleClosed);
    }

    fn destroy(self: *Operation) void {
        const allocator = self.event_loop.allocator;
        freeMessage(self.message);
        allocator.free(self.text);
        self.params.deinit(allocator);
        if (self.result) |result| {
            result.destroy();
        }
        allocator.destroy(self);
    }

    fn runOnWorker(job: *Job) void {
        const self: *Operation = @alignCast(@fieldParentPtr("job", job));
        switch (self.kind) {
            .open => self.openOnWorker(),
            .execute => self.executeOnWorker(),
            .execute_one, .begin, .commit, .rollback => self.executeOneOnWorker(),
            .execute_many => self.executeManyOnWorker(),
            .query => self.queryOnWorker(),
            .close => {
                self.cache.?.finalizeAll();
                _ = api.sqlite3_close_v2(self.database);
                self.database = null;
            },
        }
    }

    fn openOnWorker(self: *Operation) void {
        const settings = self.settings;
        var flags: c_int = api.open_extended_result_codes;
        if (settings.is_read_only) {
            flags |= api.open_read_only;
        } else {
            flags |= api.open_read_write;
            if (settings.can_create) {
                flags |= api.open_create;
            }
        }
        var database: ?*api.Database = null;
        const status = api.sqlite3_open_v2(self.text.ptr, &database, flags, null);
        if (status != 0) {
            self.recordWorkerFailure(database, status);
            _ = api.sqlite3_close_v2(database);
            return;
        }
        const opened = database.?;
        _ = api.sqlite3_busy_timeout(opened, @intCast(std.math.clamp(settings.busy_timeout_ms, 0, std.math.maxInt(c_int))));
        const should_use_wal = settings.journal == .wal and !settings.is_read_only;
        const pragmas: [:0]const u8 = if (should_use_wal and settings.has_foreign_keys)
            "PRAGMA journal_mode=WAL; PRAGMA foreign_keys=ON;"
        else if (should_use_wal)
            "PRAGMA journal_mode=WAL;"
        else if (settings.has_foreign_keys)
            "PRAGMA foreign_keys=ON;"
        else
            "";
        const pragma_status = api.sqlite3_exec(opened, pragmas.ptr, null, null, null);
        if (pragma_status != 0) {
            self.recordWorkerFailure(opened, pragma_status);
            _ = api.sqlite3_close_v2(opened);
            return;
        }
        self.database = opened;
    }

    fn executeOnWorker(self: *Operation) void {
        const database = self.database.?;
        const changes_before = api.sqlite3_total_changes64(database);
        const text = self.text;
        var position: usize = 0;
        while (position < text.len) {
            if (self.is_interrupted.load(.acquire)) {
                self.recordFixedFailure(.TimedOut, "operation timed out after {d} ms", .{self.timeout_ms});
                return;
            }
            var statement: ?*api.Statement = null;
            var tail: ?[*]const u8 = null;
            const remaining = text[position..];
            const prepare_status = api.sqlite3_prepare_v2(database, remaining.ptr, @intCast(remaining.len), &statement, &tail);
            if (prepare_status != 0) {
                self.recordWorkerFailure(database, prepare_status);
                self.shiftOffset(position);
                return;
            }
            const consumed = if (tail) |end| @intFromPtr(end) - @intFromPtr(remaining.ptr) else remaining.len;
            const prepared = statement orelse {
                position += consumed;
                continue;
            };
            const step_status = stepToEnd(prepared);
            if (step_status != @intFromEnum(api.ResultCode.done)) {
                self.recordWorkerFailure(database, step_status);
                self.shiftOffset(position);
                _ = api.sqlite3_finalize(prepared);
                return;
            }
            _ = api.sqlite3_finalize(prepared);
            position += consumed;
        }
        self.rows = api.sqlite3_total_changes64(database) - changes_before;
        self.last_id = api.sqlite3_last_insert_rowid(database);
    }

    fn executeOneOnWorker(self: *Operation) void {
        if (self.is_interrupted.load(.acquire)) {
            self.recordFixedFailure(.TimedOut, "operation timed out after {d} ms", .{self.timeout_ms});
            return;
        }
        const database = self.database.?;
        const statement = self.preparedWithParams(database, self.params.items) orelse return;
        defer resetStatement(statement);
        const status = stepToEnd(statement);
        if (status != @intFromEnum(api.ResultCode.done)) {
            self.recordWorkerFailure(database, status);
            return;
        }
        self.rows = api.sqlite3_changes64(database);
        self.last_id = api.sqlite3_last_insert_rowid(database);
    }

    fn executeManyOnWorker(self: *Operation) void {
        if (self.is_interrupted.load(.acquire)) {
            self.recordFixedFailure(.TimedOut, "operation timed out after {d} ms", .{self.timeout_ms});
            return;
        }
        const database = self.database.?;
        const statement = self.preparedStatement(database) orelse return;
        defer resetStatement(statement);
        const changes_before = api.sqlite3_total_changes64(database);
        var offset: usize = 0;
        for (self.params.row_counts) |count| {
            if (self.is_interrupted.load(.acquire)) {
                self.recordFixedFailure(.TimedOut, "operation timed out after {d} ms", .{self.timeout_ms});
                return;
            }
            const row_params = self.params.items[offset .. offset + count];
            offset += count;
            if (!self.bindParams(statement, row_params)) {
                return;
            }
            const status = stepToEnd(statement);
            if (status != @intFromEnum(api.ResultCode.done)) {
                self.recordWorkerFailure(database, status);
                return;
            }
            resetStatement(statement);
        }
        self.rows = api.sqlite3_total_changes64(database) - changes_before;
        self.last_id = api.sqlite3_last_insert_rowid(database);
    }

    fn queryOnWorker(self: *Operation) void {
        if (self.is_interrupted.load(.acquire)) {
            self.recordFixedFailure(.TimedOut, "operation timed out after {d} ms", .{self.timeout_ms});
            return;
        }
        const database = self.database.?;
        const statement = self.preparedWithParams(database, self.params.items) orelse return;
        defer resetStatement(statement);
        const result = ResultSet.create() catch {
            self.recordFixedFailure(.Other, "out of memory", .{});
            return;
        };
        self.result = result;
        self.collectRows(database, statement, result) catch {
            self.recordFixedFailure(.Other, "out of memory", .{});
        };
        const has_succeeded = self.code == 0 and self.fixed_error == null;
        if (has_succeeded) return;
        result.destroy();
        self.result = null;
    }

    fn collectRows(self: *Operation, database: *api.Database, statement: *api.Statement, result: *ResultSet) !void {
        const allocator = std.heap.c_allocator;
        const column_count = api.sqlite3_column_count(statement);
        var column: c_int = 0;
        while (column < column_count) : (column += 1) {
            const name = if (api.sqlite3_column_name(statement, column)) |found| std.mem.span(found) else "";
            try result.column_names.append(allocator, try allocator.dupe(u8, name));
        }
        var size: usize = 0;
        while (true) {
            const status = api.sqlite3_step(statement);
            if (status == @intFromEnum(api.ResultCode.done)) return;
            if (status != @intFromEnum(api.ResultCode.row)) {
                self.recordWorkerFailure(database, status);
                return;
            }
            column = 0;
            while (column < column_count) : (column += 1) {
                const cell = try copyCell(statement, column, result);
                size += @sizeOf(ResultSet.Cell) + cell.len;
                const is_too_large = self.max_bytes >= 0 and size > @as(usize, @intCast(self.max_bytes));
                if (is_too_large) {
                    self.recordFixedFailure(.TooLarge, "the result is larger than max_bytes ({d} bytes)", .{self.max_bytes});
                    return;
                }
                try result.cells.append(allocator, cell);
            }
        }
    }

    fn preparedStatement(self: *Operation, database: *api.Database) ?*api.Statement {
        const prepared = self.cache.?.take(database, self.text);
        return switch (prepared) {
            .statement => |found| found,
            .failure => |status| {
                self.recordWorkerFailure(database, status);
                return null;
            },
            .several_statements => {
                self.recordFixedFailure(.InvalidSql, "execute_with and queries take one statement; use execute for several", .{});
                return null;
            },
            .empty => {
                self.recordFixedFailure(.InvalidSql, "the SQL text holds no statement", .{});
                return null;
            },
            .out_of_memory => {
                self.recordFixedFailure(.Other, "out of memory", .{});
                return null;
            },
        };
    }

    fn preparedWithParams(self: *Operation, database: *api.Database, params: []const Param) ?*api.Statement {
        const statement = self.preparedStatement(database) orelse return null;
        if (!self.bindParams(statement, params)) {
            resetStatement(statement);
            return null;
        }
        return statement;
    }

    fn bindParams(self: *Operation, statement: *api.Statement, params: []const Param) bool {
        for (params) |param| {
            const index = api.sqlite3_bind_parameter_index(statement, param.name.ptr);
            if (index == 0) {
                self.recordFixedFailure(.UnknownParameter, "the statement has no parameter {s}", .{param.name});
                return false;
            }
            const status = switch (param.kind) {
                .null => api.sqlite3_bind_null(statement, index),
                .int => api.sqlite3_bind_int64(statement, index, param.int),
                .float => api.sqlite3_bind_double(statement, index, param.float),
                .text => api.sqlite3_bind_text64(statement, index, param.text.ptr, param.text.len, api.transient_destructor, api.utf8_encoding),
            };
            if (status != 0) {
                self.recordWorkerFailure(self.database, status);
                return false;
            }
        }
        const count = api.sqlite3_bind_parameter_count(statement);
        var index: c_int = 1;
        while (index <= count) : (index += 1) {
            const name = api.sqlite3_bind_parameter_name(statement, index) orelse {
                self.recordFixedFailure(.MissingParameter, "parameter {d} has no name; write :name and pass it in the parameters", .{index});
                return false;
            };
            const text = std.mem.span(name);
            if (!hasParam(params, text)) {
                self.recordFixedFailure(.MissingParameter, "no value for parameter {s}", .{text});
                return false;
            }
        }
        return true;
    }

    fn hasParam(params: []const Param, name: []const u8) bool {
        for (params) |param| {
            if (std.mem.eql(u8, param.name, name)) {
                return true;
            }
        }
        return false;
    }

    fn recordFixedFailure(self: *Operation, failure: SqliteError, comptime format: []const u8, args: anytype) void {
        self.fixed_error = failure;
        self.message = std.fmt.allocPrint(std.heap.c_allocator, format, args) catch null;
    }

    fn shiftOffset(self: *Operation, position: usize) void {
        if (self.offset < 0) return;
        self.offset += @intCast(position);
    }

    fn recordWorkerFailure(self: *Operation, database: ?*api.Database, status: c_int) void {
        self.code = status;
        const text = std.mem.span(api.sqlite3_errmsg(database));
        self.message = std.heap.c_allocator.dupe(u8, text) catch null;
        if (database) |found| {
            self.offset = api.sqlite3_error_offset(found);
        }
    }

    fn finishOnLoop(job: *Job, delivery: Job.Delivery) anyerror!void {
        const self: *Operation = @alignCast(@fieldParentPtr("job", job));
        defer self.destroy();
        self.stopTimer();
        const databases = &self.event_loop.sqlite;
        const connection = self.connection;
        connection.running_operation = null;
        const is_connection_gone = self.releaseConnection(databases, connection);
        if (delivery == .discard) return;
        defer databases.submitBlocked();
        if (!is_connection_gone) {
            defer databases.submitNext(connection);
        }
        const task = self.task orelse return;
        const result = try self.wakeResult(databases, task);
        self.scheduler.?.wake(task, result) catch |failure| {
            result.payload.free(self.event_loop.allocator);
            return failure;
        };
    }

    fn releaseConnection(self: *Operation, databases: *Databases, connection: *Connection) bool {
        const has_run = self.job.outcome == .executed;
        const has_failed = self.code != 0 or self.fixed_error != null;
        switch (self.kind) {
            .execute, .execute_one, .execute_many, .query => return false,
            .begin => {
                if (has_run and !has_failed) {
                    connection.transaction_owner = self.task;
                }
                return false;
            },
            .commit => {
                if (has_run and !has_failed) {
                    connection.transaction_owner = null;
                } else if (has_run) {
                    self.syncAutocommit(connection);
                }
                return false;
            },
            .rollback => {
                connection.transaction_owner = null;
                return false;
            },
            .open => {
                if (has_run and self.code == 0) {
                    connection.database = self.database;
                    return false;
                }
                databases.forgetConnection(connection);
                connection.closeNow(self.event_loop.allocator);
                return true;
            },
            .close => {
                if (has_run) {
                    connection.database = null;
                }
                databases.forgetConnection(connection);
                connection.closeNow(self.event_loop.allocator);
                return true;
            },
        }
    }

    fn syncAutocommit(self: *Operation, connection: *Connection) void {
        if (connection.transaction_owner != self.task) return;
        const database = connection.database orelse return;
        if (api.sqlite3_get_autocommit(database) != 0) {
            connection.transaction_owner = null;
        }
    }

    fn wakeResult(self: *Operation, databases: *Databases, task: *TaskState) !WakeResult {
        const has_failed = self.code != 0 or self.fixed_error != null;
        if (has_failed and self.kind != .rollback) {
            databases.recordFailure(self.event_loop.allocator, task, .{ .message = self.message, .offset = self.offset });
            self.message = null;
            return failed(self.fixed_error orelse errorOf(self.code));
        }
        return switch (self.kind) {
            .open => .{ .value = .{ .int64 = self.connection.handle }, .kind = .int64 },
            .execute, .execute_one, .execute_many => result: {
                const numbers = try self.event_loop.allocator.alloc(i64, 2);
                numbers[0] = self.rows;
                numbers[1] = self.last_id;
                break :result .{ .value = v.NULL_VALUE, .kind = .array, .payload = .{ .ints = numbers } };
            },
            .query => result: {
                const result = self.result.?;
                try databases.results.reserve(self.event_loop.allocator);
                self.result = null;
                break :result .{ .value = .{ .int64 = databases.results.addAssumeCapacity(result) }, .kind = .int64 };
            },
            .begin, .commit, .rollback, .close => .{ .value = v.NULL_VALUE, .kind = .null },
        };
    }
};

const statement_cache_size = 64;

const Prepared = union(enum) {
    statement: *api.Statement,
    failure: c_int,
    several_statements,
    empty,
    out_of_memory,
};

const StatementCache = struct {
    entries: [statement_cache_size]?Entry = @splat(null),
    clock: u64 = 0,

    const Entry = struct {
        sql: []u8,
        statement: *api.Statement,
        last_used: u64,
    };

    fn take(self: *StatementCache, database: *api.Database, sql: []const u8) Prepared {
        self.clock += 1;
        for (&self.entries) |*slot| {
            const entry = &(slot.* orelse continue);
            if (!std.mem.eql(u8, entry.sql, sql)) continue;
            entry.last_used = self.clock;
            return .{ .statement = entry.statement };
        }
        var statement: ?*api.Statement = null;
        var tail: ?[*]const u8 = null;
        const status = api.sqlite3_prepare_v3(database, sql.ptr, @intCast(sql.len), api.prepare_persistent, &statement, &tail);
        if (status != 0) {
            return .{ .failure = status };
        }
        const prepared = statement orelse return .empty;
        if (hasMoreStatements(database, sql, tail)) {
            _ = api.sqlite3_finalize(prepared);
            return .several_statements;
        }
        const owned_sql = std.heap.c_allocator.dupe(u8, sql) catch {
            _ = api.sqlite3_finalize(prepared);
            return .out_of_memory;
        };
        const slot = self.freeSlot();
        slot.* = .{ .sql = owned_sql, .statement = prepared, .last_used = self.clock };
        return .{ .statement = prepared };
    }

    fn freeSlot(self: *StatementCache) *?Entry {
        var oldest: *?Entry = &self.entries[0];
        for (&self.entries) |*slot| {
            const entry = slot.* orelse return slot;
            if (entry.last_used < oldest.*.?.last_used) {
                oldest = slot;
            }
        }
        freeEntry(oldest.*.?);
        oldest.* = null;
        return oldest;
    }

    fn finalizeAll(self: *StatementCache) void {
        for (&self.entries) |*slot| {
            const entry = slot.* orelse continue;
            freeEntry(entry);
            slot.* = null;
        }
    }

    fn freeEntry(entry: Entry) void {
        _ = api.sqlite3_finalize(entry.statement);
        std.heap.c_allocator.free(entry.sql);
    }
};

fn hasMoreStatements(database: *api.Database, sql: []const u8, tail: ?[*]const u8) bool {
    const end = tail orelse return false;
    const consumed = @intFromPtr(end) - @intFromPtr(sql.ptr);
    const rest = sql[consumed..];
    if (rest.len == 0) {
        return false;
    }
    var statement: ?*api.Statement = null;
    var rest_tail: ?[*]const u8 = null;
    const status = api.sqlite3_prepare_v2(database, rest.ptr, @intCast(rest.len), &statement, &rest_tail);
    if (status != 0) {
        return true;
    }
    const found = statement orelse return false;
    _ = api.sqlite3_finalize(found);
    return true;
}

fn copyCell(statement: *api.Statement, column: c_int, result: *ResultSet) !ResultSet.Cell {
    const column_type = api.sqlite3_column_type(statement, column);
    return switch (column_type) {
        api.integer_type => .{ .kind = .int, .int = api.sqlite3_column_int64(statement, column) },
        api.float_type => .{ .kind = .float, .float = api.sqlite3_column_double(statement, column) },
        api.text_type, api.blob_type => cell: {
            const pointer: ?[*]const u8 = if (column_type == api.text_type) api.sqlite3_column_text(statement, column) else @ptrCast(api.sqlite3_column_blob(statement, column));
            const len: usize = @intCast(api.sqlite3_column_bytes(statement, column));
            const start = result.bytes.items.len;
            if (pointer) |bytes| {
                try result.bytes.appendSlice(std.heap.c_allocator, bytes[0..len]);
            }
            break :cell .{ .kind = if (column_type == api.text_type) .text else .blob, .start = start, .len = len };
        },
        else => .{ .kind = .null },
    };
}

fn resetStatement(statement: *api.Statement) void {
    _ = api.sqlite3_reset(statement);
    _ = api.sqlite3_clear_bindings(statement);
}

fn stepToEnd(statement: *api.Statement) c_int {
    while (true) {
        const status = api.sqlite3_step(statement);
        if (status != @intFromEnum(api.ResultCode.row)) {
            return status;
        }
    }
}

const OperationQueue = struct {
    head: ?*Operation = null,
    tail: ?*Operation = null,

    fn push(self: *OperationQueue, operation: *Operation) void {
        operation.next = null;
        if (self.tail) |tail| {
            tail.next = operation;
        } else {
            self.head = operation;
        }
        self.tail = operation;
    }

    fn peekReady(self: *const OperationQueue, owner: ?*TaskState) ?*Operation {
        const required_task = owner orelse return self.head;
        var current = self.head;
        while (current) |operation| : (current = operation.next) {
            if (operation.task == required_task) {
                return operation;
            }
        }
        return null;
    }

    fn remove(self: *OperationQueue, target: *Operation) void {
        var previous: ?*Operation = null;
        var current = self.head;
        while (current) |operation| : (current = operation.next) {
            if (operation != target) {
                previous = operation;
                continue;
            }
            if (previous) |found| {
                found.next = operation.next;
            } else {
                self.head = operation.next;
            }
            if (self.tail == operation) {
                self.tail = previous;
            }
            operation.next = null;
            return;
        }
    }

    fn pop(self: *OperationQueue) ?*Operation {
        const operation = self.head orelse return null;
        self.head = operation.next;
        if (self.head == null) {
            self.tail = null;
        }
        operation.next = null;
        return operation;
    }
};

pub fn failed(failure: SqliteError) WakeResult {
    return .{ .value = .{ .int64 = @intFromEnum(failure) }, .kind = .int64, .has_failed = true, .error_is_relative = true };
}

fn errorOf(code: c_int) SqliteError {
    return switch (api.ResultCode.of(code)) {
        .cant_open => .CantOpen,
        .read_only, .permission => .ReadOnly,
        .busy, .locked => .Busy,
        .constraint => .Constraint,
        .sql_error => .InvalidSql,
        .interrupted => .TimedOut,
        .full => .Full,
        .corrupt, .not_a_database => .Corrupt,
        else => .Other,
    };
}

pub fn freeMessage(message: ?[]u8) void {
    const text = message orelse return;
    std.heap.c_allocator.free(text);
}
