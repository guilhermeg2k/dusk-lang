// Hand-written libuv bindings: only what event_loop.zig calls.
// translate-c of uv.h breaks on macOS, so libuv structs stay opaque here
// and their sizes come from uv_loop_size / uv_handle_size at runtime.

// ---- Types ----

pub const Loop = opaque {};
pub const Handle = opaque {};
pub const Request = opaque {};

pub const RunMode = enum(c_int) { default = 0, once = 1, nowait = 2 };
pub const HandleType = enum(c_int) { timer = 13 };
pub const RequestType = enum(c_int) { fs = 6, work = 7 };

pub const Buffer = if (@import("builtin").os.tag == .windows) extern struct {
    len: c_ulong,
    base: [*]u8,
} else extern struct {
    base: [*]u8,
    len: usize,
};

pub const DirEntryType = enum(c_int) { unknown = 0, file = 1, dir = 2, link = 3, _ };

pub const DirEntry = extern struct {
    name: [*:0]const u8,
    entry_type: DirEntryType,
};

pub const DirEntries = extern struct {
    entries: [*]DirEntry,
    entry_count: usize,
};

pub const Timespec = extern struct {
    tv_sec: c_long,
    tv_nsec: c_long,
};

pub const Stat = extern struct {
    st_dev: u64,
    st_mode: u64,
    st_nlink: u64,
    st_uid: u64,
    st_gid: u64,
    st_rdev: u64,
    st_ino: u64,
    st_size: u64,
    st_blksize: u64,
    st_blocks: u64,
    st_flags: u64,
    st_gen: u64,
    st_atim: Timespec,
    st_mtim: Timespec,
    st_ctim: Timespec,
    st_birthtim: Timespec,
};

// ---- Callbacks ----

pub const TimerCallback = *const fn (timer: *Handle) callconv(.c) void;
pub const CloseCallback = *const fn (handle: *Handle) callconv(.c) void;
pub const WalkCallback = *const fn (handle: *Handle, arg: ?*anyopaque) callconv(.c) void;
pub const WorkCallback = *const fn (request: *Request) callconv(.c) void;
pub const AfterWorkCallback = *const fn (request: *Request, status: c_int) callconv(.c) void;
pub const FsCallback = *const fn (request: *Request) callconv(.c) void;

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

pub extern fn uv_req_size(request_type: RequestType) usize;
pub extern fn uv_req_get_data(request: *const Request) ?*anyopaque;
pub extern fn uv_req_set_data(request: *Request, data: ?*anyopaque) void;
pub extern fn uv_err_name(status: c_int) [*:0]const u8;

pub extern fn uv_queue_work(loop: *Loop, request: *Request, work: WorkCallback, after_work: AfterWorkCallback) c_int;

pub extern fn uv_fs_open(loop: *Loop, request: *Request, path: [*:0]const u8, flags: c_int, mode: c_int, callback: ?FsCallback) c_int;
pub extern fn uv_fs_read(loop: *Loop, request: *Request, file: c_int, buffers: [*]const Buffer, buffer_count: c_uint, offset: i64, callback: ?FsCallback) c_int;
pub extern fn uv_fs_write(loop: *Loop, request: *Request, file: c_int, buffers: [*]const Buffer, buffer_count: c_uint, offset: i64, callback: ?FsCallback) c_int;
pub extern fn uv_fs_close(loop: *Loop, request: *Request, file: c_int, callback: ?FsCallback) c_int;
pub extern fn uv_fs_fstat(loop: *Loop, request: *Request, file: c_int, callback: ?FsCallback) c_int;
pub extern fn uv_fs_mkdir(loop: *Loop, request: *Request, path: [*:0]const u8, mode: c_int, callback: ?FsCallback) c_int;
pub extern fn uv_fs_rmdir(loop: *Loop, request: *Request, path: [*:0]const u8, callback: ?FsCallback) c_int;
pub extern fn uv_fs_unlink(loop: *Loop, request: *Request, path: [*:0]const u8, callback: ?FsCallback) c_int;
pub extern fn uv_fs_rename(loop: *Loop, request: *Request, path: [*:0]const u8, new_path: [*:0]const u8, callback: ?FsCallback) c_int;
pub extern fn uv_fs_opendir(loop: *Loop, request: *Request, path: [*:0]const u8, callback: ?FsCallback) c_int;
pub extern fn uv_fs_readdir(loop: *Loop, request: *Request, dir: *DirEntries, callback: ?FsCallback) c_int;
pub extern fn uv_fs_closedir(loop: *Loop, request: *Request, dir: *DirEntries, callback: ?FsCallback) c_int;
pub extern fn uv_fs_get_ptr(request: *const Request) ?*anyopaque;
pub extern fn uv_fs_stat(loop: *Loop, request: *Request, path: [*:0]const u8, callback: ?FsCallback) c_int;
pub extern fn uv_fs_lstat(loop: *Loop, request: *Request, path: [*:0]const u8, callback: ?FsCallback) c_int;
pub extern fn uv_fs_get_statbuf(request: *Request) *Stat;
pub extern fn uv_fs_req_cleanup(request: *Request) void;
