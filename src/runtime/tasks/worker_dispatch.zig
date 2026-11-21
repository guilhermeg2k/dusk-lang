const std = @import("std");
const uv = @import("uv.zig");
const event_loop_module = @import("event_loop.zig");
const EventLoop = event_loop_module.EventLoop;
const HandleOwner = event_loop_module.HandleOwner;
const onHandleClosed = event_loop_module.onHandleClosed;
const WorkerPool = @import("worker_pool.zig").WorkerPool;
const Job = @import("worker_pool.zig").Job;

const handle_alignment = std.mem.Alignment.@"16";

pub const WorkerDispatch = struct {
    const Self = @This();

    owner: HandleOwner = .{ .kind = .worker_async },
    event_loop: *EventLoop,
    pool: ?*WorkerPool = null,
    async_memory: []align(16) u8,
    outstanding: usize = 0,
    is_shut_down: bool = false,

    pub fn create(event_loop: *EventLoop, options: WorkerPool.Options) !*Self {
        const loop = try event_loop.ensureLoopStarted();
        const allocator = event_loop.allocator;
        const dispatch = try allocator.create(Self);
        const async_memory = allocator.alignedAlloc(u8, handle_alignment, uv.uv_handle_size(.async)) catch |failure| {
            allocator.destroy(dispatch);
            return failure;
        };
        dispatch.* = .{ .event_loop = event_loop, .async_memory = async_memory };
        if (uv.uv_async_init(loop, dispatch.asyncHandle(), onAsync) != 0) {
            dispatch.destroy();
            return error.WorkerDispatchFailed;
        }
        uv.uv_handle_set_data(dispatch.asyncHandle(), &dispatch.owner);
        uv.uv_unref(dispatch.asyncHandle());
        dispatch.pool = WorkerPool.create(allocator, options, .{ .context = dispatch, .notify = notifyLoop }) catch |failure| {
            dispatch.is_shut_down = true;
            uv.uv_close(dispatch.asyncHandle(), onHandleClosed);
            return failure;
        };
        return dispatch;
    }

    pub fn destroy(self: *Self) void {
        std.debug.assert(self.outstanding == 0);
        const allocator = self.event_loop.allocator;
        if (self.pool) |pool| {
            pool.destroy();
        }
        allocator.free(self.async_memory);
        allocator.destroy(self);
    }

    pub fn submit(self: *Self, job: *Job) WorkerPool.SubmitError!void {
        try self.pool.?.submit(job);
        if (self.outstanding == 0) {
            uv.uv_ref(self.asyncHandle());
        }
        self.outstanding += 1;
        self.event_loop.pending_operations += 1;
    }

    pub fn shutDown(self: *Self) void {
        if (self.is_shut_down) return;
        self.is_shut_down = true;
        self.pool.?.stopAndJoin();
        self.finishCompleted(.discard);
        uv.uv_close(self.asyncHandle(), onHandleClosed);
    }

    fn onAsync(handle: *uv.Handle) callconv(.c) void {
        const dispatch: *Self = @alignCast(@fieldParentPtr("owner", HandleOwner.of(handle)));
        const delivery: Job.Delivery = if (dispatch.event_loop.is_shutting_down) .discard else .deliver;
        dispatch.finishCompleted(delivery);
    }

    fn finishCompleted(self: *Self, delivery: Job.Delivery) void {
        while (self.pool.?.takeCompleted()) |job| {
            self.outstanding -= 1;
            self.event_loop.pending_operations -= 1;
            const job_delivery: Job.Delivery = if (job.outcome == .abandoned) .discard else delivery;
            job.finish(job, job_delivery) catch |failure| {
                if (self.event_loop.callback_failure == null) {
                    self.event_loop.callback_failure = failure;
                }
            };
        }
        if (self.outstanding > 0) return;
        if (self.is_shut_down) return;
        uv.uv_unref(self.asyncHandle());
    }

    fn notifyLoop(context: *anyopaque) void {
        const dispatch: *Self = @ptrCast(@alignCast(context));
        const status = uv.uv_async_send(dispatch.asyncHandle());
        if (status != 0) {
            std.debug.panic("worker pool: uv_async_send failed: {s}", .{std.mem.span(uv.uv_err_name(status))});
        }
    }

    fn asyncHandle(self: *Self) *uv.Handle {
        return @ptrCast(self.async_memory.ptr);
    }
};
