pub const Loop = opaque {};
pub const Handle = opaque {};

pub const RunMode = enum(c_int) { default = 0, once = 1, nowait = 2 };
pub const HandleType = enum(c_int) { timer = 13 };

pub const TimerCallback = *const fn (timer: *Handle) callconv(.c) void;
pub const CloseCallback = *const fn (handle: *Handle) callconv(.c) void;
pub const WalkCallback = *const fn (handle: *Handle, arg: ?*anyopaque) callconv(.c) void;

pub extern fn uv_loop_size() usize;
pub extern fn uv_loop_init(loop: *Loop) c_int;
pub extern fn uv_loop_close(loop: *Loop) c_int;
pub extern fn uv_run(loop: *Loop, mode: RunMode) c_int;
pub extern fn uv_update_time(loop: *Loop) void;
pub extern fn uv_walk(loop: *Loop, callback: WalkCallback, arg: ?*anyopaque) void;

pub extern fn uv_handle_size(handle_type: HandleType) usize;
pub extern fn uv_handle_get_data(handle: *const Handle) ?*anyopaque;
pub extern fn uv_handle_set_data(handle: *Handle, data: ?*anyopaque) void;
pub extern fn uv_is_closing(handle: *const Handle) c_int;
pub extern fn uv_close(handle: *Handle, callback: ?CloseCallback) void;

pub extern fn uv_timer_init(loop: *Loop, timer: *Handle) c_int;
pub extern fn uv_timer_start(timer: *Handle, callback: TimerCallback, timeout: u64, repeat: u64) c_int;
