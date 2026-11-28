const std = @import("std");

pub const WorkerPool = struct {
    const Self = @This();

    pub const Options = struct {
        thread_count: usize,
        max_outstanding: usize,
    };

    pub const Notifier = struct {
        context: *anyopaque,
        notify: *const fn (context: *anyopaque) void,
    };

    pub const CreateError = error{ InvalidOptions, OutOfMemory } || std.Thread.SpawnError;
    pub const SubmitError = error{ QueueFull, ShuttingDown };

    allocator: std.mem.Allocator,
    options: Options,
    notifier: Notifier,
    threaded: std.Io.Threaded = .init_single_threaded,
    mutex: std.Io.Mutex = .init,
    has_work: std.Io.Condition = .init,
    queued: JobQueue = .{},
    completed: JobQueue = .{},
    outstanding: usize = 0,
    is_stopping: bool = false,
    is_joined: bool = false,
    threads: []std.Thread,
    started_count: usize = 0,

    pub fn create(allocator: std.mem.Allocator, options: Options, notifier: Notifier) CreateError!*Self {
        const are_options_valid = options.thread_count > 0 and options.max_outstanding > 0;
        if (!are_options_valid) {
            return error.InvalidOptions;
        }
        const pool = try allocator.create(Self);
        errdefer allocator.destroy(pool);
        const threads = try allocator.alloc(std.Thread, options.thread_count);
        pool.* = .{ .allocator = allocator, .options = options, .notifier = notifier, .threads = threads };
        errdefer allocator.free(threads);
        for (threads) |*thread| {
            thread.* = std.Thread.spawn(.{}, runWorker, .{pool}) catch |failure| {
                pool.stopAndJoin();
                return failure;
            };
            pool.started_count += 1;
        }
        return pool;
    }

    pub fn destroy(self: *Self) void {
        std.debug.assert(self.is_joined);
        std.debug.assert(self.queued.isEmpty() and self.completed.isEmpty());
        const allocator = self.allocator;
        allocator.free(self.threads);
        allocator.destroy(self);
    }

    pub fn submit(self: *Self, job: *Job) SubmitError!void {
        const io = self.threaded.io();
        self.mutex.lockUncancelable(io);
        defer self.mutex.unlock(io);
        if (self.is_stopping) {
            return error.ShuttingDown;
        }
        if (self.outstanding >= self.options.max_outstanding) {
            return error.QueueFull;
        }
        job.outcome = .pending;
        self.queued.push(job);
        self.outstanding += 1;
        self.has_work.signal(io);
    }

    pub fn takeCompleted(self: *Self) ?*Job {
        const io = self.threaded.io();
        self.mutex.lockUncancelable(io);
        defer self.mutex.unlock(io);
        const job = self.completed.pop() orelse return null;
        self.outstanding -= 1;
        return job;
    }

    pub fn stopAndJoin(self: *Self) void {
        if (self.is_joined) return;
        self.abandonQueuedJobs();
        for (self.threads[0..self.started_count]) |thread| {
            thread.join();
        }
        self.is_joined = true;
    }

    fn abandonQueuedJobs(self: *Self) void {
        const io = self.threaded.io();
        self.mutex.lockUncancelable(io);
        defer self.mutex.unlock(io);
        self.is_stopping = true;
        while (self.queued.pop()) |job| {
            job.outcome = .abandoned;
            self.completed.push(job);
        }
        self.has_work.broadcast(io);
    }

    fn runWorker(self: *Self) void {
        while (self.takeQueued()) |job| {
            job.execute(job);
            self.publishCompleted(job);
        }
    }

    fn takeQueued(self: *Self) ?*Job {
        const io = self.threaded.io();
        self.mutex.lockUncancelable(io);
        defer self.mutex.unlock(io);
        while (true) {
            if (self.is_stopping) {
                return null;
            }
            if (self.queued.pop()) |job| {
                return job;
            }
            self.has_work.waitUncancelable(io, &self.mutex);
        }
    }

    fn publishCompleted(self: *Self, job: *Job) void {
        const io = self.threaded.io();
        self.mutex.lockUncancelable(io);
        job.outcome = .executed;
        self.completed.push(job);
        self.mutex.unlock(io);
        self.notifier.notify(self.notifier.context);
    }
};

pub const Job = struct {
    execute: *const fn (job: *Job) void,
    finish: *const fn (job: *Job, delivery: Delivery) anyerror!void,
    next: ?*Job = null,
    outcome: Outcome = .pending,

    pub const Outcome = enum { pending, executed, abandoned };
    pub const Delivery = enum { deliver, discard };
};

const JobQueue = struct {
    head: ?*Job = null,
    tail: ?*Job = null,

    fn isEmpty(self: *const JobQueue) bool {
        return self.head == null;
    }

    fn push(self: *JobQueue, job: *Job) void {
        job.next = null;
        if (self.tail) |tail| {
            tail.next = job;
        } else {
            self.head = job;
        }
        self.tail = job;
    }

    fn pop(self: *JobQueue) ?*Job {
        const job = self.head orelse return null;
        self.head = job.next;
        if (self.head == null) {
            self.tail = null;
        }
        job.next = null;
        return job;
    }
};
