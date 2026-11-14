// Hand-written libuv bindings: only what event_loop.zig calls.
// translate-c of uv.h breaks on macOS, so libuv structs stay opaque here
// and their sizes come from uv_loop_size / uv_handle_size at runtime.

// ---- Types ----

pub const Loop = opaque {};
pub const Handle = opaque {};

pub const RunMode = enum(c_int) { default = 0, once = 1, nowait = 2 };
pub const HandleType = enum(c_int) { timer = 13 };

// ---- Callbacks ----

pub const TimerCallback = *const fn (timer: *Handle) callconv(.c) void;
pub const CloseCallback = *const fn (handle: *Handle) callconv(.c) void;
pub const WalkCallback = *const fn (handle: *Handle, arg: ?*anyopaque) callconv(.c) void;

// ---- Loop ----

pub extern fn uv_loop_size() usize;
pub extern fn uv_loop_init(loop: *Loop) c_int;
pub extern fn uv_loop_close(loop: *Loop) c_int;
pub extern fn uv_run(loop: *Loop, mode: RunMode) c_int;
pub extern fn uv_update_time(loop: *Loop) void;
pub extern fn uv_walk(loop: *Loop, callback: WalkCallback, arg: ?*anyopaque) void;

// ---- Handles (shared by every handle type) ----

pub extern fn uv_handle_size(handle_type: HandleType) usize;
pub extern fn uv_handle_get_data(handle: *const Handle) ?*anyopaque;
pub extern fn uv_handle_set_data(handle: *Handle, data: ?*anyopaque) void;
pub extern fn uv_is_closing(handle: *const Handle) c_int;
pub extern fn uv_close(handle: *Handle, callback: ?CloseCallback) void;

// ---- Timers ----

pub extern fn uv_timer_init(loop: *Loop, timer: *Handle) c_int;
pub extern fn uv_timer_start(timer: *Handle, callback: TimerCallback, timeout: u64, repeat: u64) c_int;
