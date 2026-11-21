const std = @import("std");
const api = @import("sqlite_api.zig");
const v = @import("../value.zig");
const EventLoop = @import("event_loop.zig").EventLoop;
const Scheduler = @import("scheduler.zig").Scheduler;
const TaskState = @import("scheduler.zig").TaskState;
const WakeResult = @import("scheduler.zig").WakeResult;
const HandleTable = @import("handle_table.zig").HandleTable;
const WorkerDispatch = @import("worker_dispatch.zig").WorkerDispatch;
const Job = @import("worker_pool.zig").Job;
const WorkerPool = @import("worker_pool.zig").WorkerPool;

pub const SqliteError = enum { CantOpen, ReadOnly, Busy, Constraint, InvalidSql, MissingParameter, UnknownParameter, TypeMismatch, MissingColumn, TooLarge, TimedOut, Full, Corrupt, Closed, Other };

pub const Journal = enum { wal, delete };

pub const OpenSettings = struct {
    journal: Journal = .wal,
    has_foreign_keys: bool = true,
    busy_timeout_ms: i64 = 5000,
    is_read_only: bool = false,
    can_create: bool = true,
};

pub const Failure = struct {
    message: ?[]u8,
    offset: i64,
};

const pool_options: WorkerPool.Options = .{ .thread_count = 4, .max_outstanding = 64 };

pub const Databases = struct {
    const Self = @This();

    connections: HandleTable(Connection) = .{},
    dispatch: ?*WorkerDispatch = null,
    blocked: std.ArrayList(*Connection) = .empty,
    failures: std.AutoHashMapUnmanaged(*TaskState, Failure) = .empty,

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        for (self.connections.entries.items) |entry| {
            const connection = entry orelse continue;
            connection.closeNow(allocator);
        }
        self.connections.deinit(allocator);
        self.blocked.deinit(allocator);
        var failures = self.failures.valueIterator();
        while (failures.next()) |failure| {
            freeMessage(failure.message);
        }
        self.failures.deinit(allocator);
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

    pub fn execute(self: *Self, event_loop: *EventLoop, scheduler: *Scheduler, task: *TaskState, handle: i64, sql: []const u8) !bool {
        const connection = self.openConnection(handle) orelse return false;
        const operation = try Operation.create(event_loop, scheduler, task, connection, .execute, sql);
        self.enqueue(connection, operation);
        return true;
    }

    pub fn close(self: *Self, event_loop: *EventLoop, scheduler: ?*Scheduler, task: ?*TaskState, handle: i64) !bool {
        const connection = self.openConnection(handle) orelse return false;
        const operation = try Operation.create(event_loop, scheduler, task, connection, .close, "");
        connection.is_closing = true;
        self.enqueue(connection, operation);
        return true;
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

    fn enqueue(self: *Self, connection: *Connection, operation: *Operation) void {
        connection.queued.push(operation);
        self.submitNext(connection);
    }

    fn submitNext(self: *Self, connection: *Connection) void {
        if (connection.is_running) return;
        const operation = connection.queued.peek() orelse return;
        operation.database = connection.database;
        self.dispatch.?.submit(&operation.job) catch {
            self.markBlocked(connection);
            return;
        };
        _ = connection.queued.pop();
        connection.is_running = true;
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
            if (connection.is_running) {
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
};

pub const Connection = struct {
    database: ?*api.Database = null,
    handle: i64 = 0,
    queued: OperationQueue = .{},
    is_running: bool = false,
    is_closing: bool = false,

    fn closeNow(self: *Connection, allocator: std.mem.Allocator) void {
        while (self.queued.pop()) |operation| {
            operation.destroy();
        }
        if (self.database) |database| {
            _ = api.sqlite3_close_v2(database);
        }
        allocator.destroy(self);
    }
};

const Operation = struct {
    const Kind = enum { open, execute, close };

    job: Job = .{ .execute = runOnWorker, .finish = finishOnLoop },
    kind: Kind,
    event_loop: *EventLoop,
    scheduler: ?*Scheduler,
    task: ?*TaskState,
    connection: *Connection,
    text: [:0]u8,
    settings: OpenSettings = .{},
    database: ?*api.Database = null,
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

    fn destroy(self: *Operation) void {
        const allocator = self.event_loop.allocator;
        freeMessage(self.message);
        allocator.free(self.text);
        allocator.destroy(self);
    }

    fn runOnWorker(job: *Job) void {
        const self: *Operation = @alignCast(@fieldParentPtr("job", job));
        switch (self.kind) {
            .open => self.openOnWorker(),
            .execute => self.executeOnWorker(),
            .close => {
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
        const databases = &self.event_loop.sqlite;
        const connection = self.connection;
        connection.is_running = false;
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
        switch (self.kind) {
            .execute => return false,
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

    fn wakeResult(self: *Operation, databases: *Databases, task: *TaskState) !WakeResult {
        if (self.code != 0) {
            databases.recordFailure(self.event_loop.allocator, task, .{ .message = self.message, .offset = self.offset });
            self.message = null;
            return failed(errorOf(self.code));
        }
        return switch (self.kind) {
            .open => .{ .value = .{ .int64 = self.connection.handle }, .kind = .int64 },
            .execute => result: {
                const numbers = try self.event_loop.allocator.alloc(i64, 2);
                numbers[0] = self.rows;
                numbers[1] = self.last_id;
                break :result .{ .value = v.NULL_VALUE, .kind = .array, .payload = .{ .ints = numbers } };
            },
            .close => .{ .value = v.NULL_VALUE, .kind = .null },
        };
    }
};

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

    fn peek(self: *const OperationQueue) ?*Operation {
        return self.head;
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
