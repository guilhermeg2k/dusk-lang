# Generic native worker pool: design and implementation plan

Planning requested 2026-11-18 on v017std. Status: proposed; no implementation authorized yet. SQLite will be the first production consumer, planned separately.

## Goal and scope

Provide reusable runtime infrastructure for blocking native work without occupying the VM thread or libuv's shared filesystem/DNS pool. Use Zig std for threads, synchronization and memory ownership; use uv_async_t only in an event-loop adapter that announces completed work.

This is an internal native facility, not a std/worker module or a mechanism for running Dusk closures concurrently. SQLite bindings, database objects, SQL values, connection queues and transactions are outside this task. Existing filesystem and trust-store jobs stay on uv_queue_work.

## Structure and proposed internal API

- tasks/worker_pool.zig: WorkerPool, Job, options, thread lifecycle, bounded admission, work/completion queues and an injected notifier. Imports neither libuv, VM, Scheduler nor SQLite.
- tasks/worker_dispatch.zig: stable runtime bridge owning a pool and its uv_async_t; tracks accepted operations and dispatches completions on the event loop thread.
- tasks/event_loop.zig and tasks/uv.zig: async bindings, handle ownership/close dispatch, pending-operation accounting and shutdown integration.
- src/test.zig: registered native tests with synthetic jobs; no public testing builtin.

WorkerPool options require an explicit positive thread_count and max_outstanding. No global singleton or implicit CPU-count sizing. Each consumer chooses its pool capacity; tests use small explicit values. SQLite's production defaults are decided with its own plan. Pools start lazily when a consumer first needs them and retain idle threads until teardown.

Proposed operations:

- create(allocator, options, notifier) -> owned stable pool or initialization error.
- submit(job) -> success, QueueFull or ShuttingDown. Called on the owner/event-loop thread only; never waits for capacity.
- takeCompleted() -> next completed Job, called on the owner thread. Each returned job releases one admission slot.
- stopAndJoin() -> reject new submissions, abandon queued work and join running workers; leaves terminal jobs available for cleanup.
- destroy() -> free queues/synchronization/storage after join and completion disposal.

Job embeds an intrusive header in consumer-owned native storage: execute callback, context, queue linkage and terminal outcome (executed or abandoned). execute(job) returns void and records the operation's result/error in that job's native storage. The event-loop bridge invokes the consumer's finish(job, delivery_mode) on the owner thread, with delivery_mode = deliver or discard. Consumer finish owns disposal or transfers result storage into a WakeResult. The pool itself never interprets results, wakes tasks or frees consumer contexts.

Notifier is a non-failing function pointer plus context, called after publishing a completion and outside queue locks. The runtime adapter implements it with uv_async_send; tests may use a separate synchronization primitive. The notifier context stays valid until every worker has joined.

All objects whose addresses escape to worker threads or libuv are allocated at stable addresses. No stack-local pool copied into EventLoop after threads start.

## Execution and capacity

1. Consumer validates and copies inputs, then submits an initialized job. Rejection leaves ownership with the caller and changes no pending count.
2. Acceptance transfers access to the pool. Adapter increments pending_operations before it yields to event-loop polling, then the native can park its task.
3. An idle worker dequeues one job under synchronization, releases the lock and executes it.
4. Worker publishes its result to the completion queue under synchronization, then notifies the owner thread. It makes no further access to that job after publication.
5. Async callback drains completions, updates accounting and invokes finish. It continues disposing/draining even if one finish fails, recording the first runtime failure through the existing callback_failure path.
6. On normal delivery, consumers wake a task with the existing WakeResult protocol. GC allocations and payload conversion happen at the established VM resume boundary; the pool introduces no new WakePayload variant.

The work queue is FIFO; completion order across workers is unspecified. max_outstanding counts queued, executing and completed-but-not-yet-taken jobs together, bounding native job count even if the event loop cannot drain promptly. Intrusive work/completion queues perform no allocation after admission. Consumer payload byte limits are separate; a job-count limit does not bound arbitrarily large query results.

Use std.Thread.spawn/join and the installed Zig 0.16 synchronization interfaces: std.Io.Mutex and std.Io.Condition, not removed std.Thread.Mutex/Condition APIs. Prefer a pool-owned std.Io.Threaded context for these synchronization calls, kept alive through thread join; it is not used to dispatch jobs or depend on the embedding host's custom I/O implementation. Verify this arrangement on all supported build targets in the first implementation slice. Use uncancelable lock/wait operations for infrastructure invariants. Sleep idle workers on a condition; do not spin. Keep callbacks and job execution outside locks. Use a thread-safe allocator for allocations reachable from workers (runtime C allocator initially), never a VM arena or GC allocator.

## Event-loop bridge

The bridge owns one async handle per pool instance, not one per job. It integrates through HandleOwner and onHandleClosed, with handle memory sized by uv_handle_size(.async).

The callback drains the completion queue rather than treating each notification as one job: notifications can coalesce. Queue synchronization establishes publication/visibility; notification is only a wakeup. No worker touches Scheduler, TaskState fields, VM stacks, heap objects or pending_operations.

An idle pool must not keep the loop/program alive. Initialize its async handle unreferenced, reference it while accepted jobs remain outstanding, then unreference it when the count reaches zero. Reference changes and pending_operations changes occur only on the event loop thread. Completed-but-undelivered jobs remain pending. This must work when the only active work is a native job and there are no timers, sockets or filesystem requests.

Treat uv_async_send failure on a live adapter as an infrastructure failure, not a lost notification to ignore. Implementation must establish a fail-fast diagnostic policy rather than leaving parked tasks waiting forever. Correct handle lifetime prevents sends after close.

## Ownership, errors and shutdown

Accepted job lifecycle: caller-owned -> queued -> executing -> completed -> owner delivery/disposal. Shutdown may instead move queued jobs directly to abandoned completion. Exactly one terminal finish invocation per accepted job; rejected jobs receive none. A result is published once and a task is woken at most once.

Operation errors live in consumer storage and do not kill a worker. Allocation failure in execute must likewise be represented as a consumer result. Initialization failure joins any threads already started and frees partial state. Worker panics are runtime failures, not catchable database errors.

On VM teardown:

1. Stop accepting new work and disable ordinary delivery/resubmission.
2. Detach queued jobs into abandoned completions and wake all idle workers to exit.
3. Allow already executing jobs to finish, then join every worker. Do not free their jobs, notifier, database/native owners, scheduler or loop first.
4. Drain all terminal jobs in discard mode without waking tasks. Release results, account for every pending operation and dispose native storage exactly once.
5. Close async handles and run close callbacks before freeing their bridge storage or closing the libuv loop. Preserve existing process kill/reap and stream teardown ordering.

Join occurs without holding queue locks. Workers cannot require an event-loop callback to finish execute; otherwise joining could deadlock. Shutdown is memory-safe but has no bounded duration for an uninterruptible native call. Generic forced thread termination, running-job cancellation, deadlines and graceful runtime pool resizing are deferred. A later SQLite layer must define interruption, bounded lock waits, connection cleanup and transaction shutdown; this infrastructure must not promise those behaviors.

## SQLite boundary

SQLite gets a dedicated WorkerPool instance, independent of libuv and future blocking consumers. The database layer submits only the next ready operation for a connection and queues later operations outside the worker pool, with its own admission limits. Workers do not wait for another job on the same connection. Operations on one connection never overlap, and transactions reserve their connection in the database layer. There is no thread-affinity guarantee: successive operations can execute on different workers, so SQLite configuration/ownership must permit that safely. A future library requiring thread affinity needs a separate design extension.

## Implementation slices

- [ ] 1. Generic pool core: stable ownership, explicit options, std synchronization, bounded admission, intrusive queues, notifier, partial-startup rollback and stop/join.
- [ ] 2. Runtime bridge: async bindings/section in uv.zig, owner kind, lazy creation, completion draining, pending/ref accounting and close lifetime.
- [ ] 3. Teardown/error paths: discard delivery, queued abandonment, active-job completion, callback failures and allocation-failure cleanup.
- [ ] 4. Integrated tests and internal documentation: synthetic parked jobs through the real scheduler/event loop, task tracking and vm-internals. No public API or changelog entry for internal infrastructure alone.

## Validation

Register tests in src/test.zig and use barriers/conditions to establish ordering; avoid correctness assertions based on sleeps or timing thresholds. Run potentially hanging teardown/liveness cases under an external timeout.

- Single job, worker reuse, FIFO dequeue with one worker, and concurrent execution with two workers.
- QueueFull leaves caller ownership intact; admission capacity includes completions not yet drained; accepting work again after drain.
- All results drained when many notifications coalesce, including publication racing with draining and the last pending job.
- Only worker jobs execute off-thread; finish/wake/free happen on the owner thread. Copied inputs and native results survive GC while the caller is parked.
- Pool-only pending work wakes an otherwise idle event loop; an idle pool does not keep the runtime alive or cause polling.
- Block every dedicated worker using test barriers and verify an actual filesystem job can finish before releasing those workers. Timer and ready-task progress while jobs wait.
- Two pool instances have independent capacity and shutdown; generic core tests work without a libuv notifier.
- Operation failure followed by successful work, initialization/allocation fault paths, finish failure with further completions, accepted-versus-rejected disposal counts and relative native error delivery.
- Shutdown with idle workers, queued jobs, executing jobs and undrained completions; no callback/send after close, double frees, stranded counters or access after VM teardown.
- Debug and ReleaseFast full suites; supported cross-builds, explicitly distinguishing build success from runtime coverage. No benchmarks unless requested.

Planning changes only: no runtime source, database dependency or public std module is introduced by this document.
