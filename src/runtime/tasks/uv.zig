// Hand-written libuv bindings: only what event_loop.zig and fs_job.zig call.
// translate-c of uv.h breaks on macOS, so libuv structs stay opaque here
// and their sizes come from uv_loop_size / uv_handle_size / uv_req_size at runtime.

// ---- Types ----

pub const Loop = opaque {};
pub const Handle = opaque {};
pub const Request = opaque {};

pub const RunMode = enum(c_int) { default = 0, once = 1, nowait = 2 };
pub const HandleType = enum(c_int) { unknown = 0, named_pipe = 7, tcp = 12, timer = 13, tty = 14, file = 17, _ };
pub const RequestType = enum(c_int) { connect = 2, write = 3, fs = 6, work = 7, getaddrinfo = 8 };
pub const DirEntryType = enum(c_int) { unknown = 0, file = 1, dir = 2, link = 3, _ };
pub const OsFile = if (@import("builtin").os.tag == .windows) ?*anyopaque else c_int;
pub const OsSocket = if (@import("builtin").os.tag == .windows) usize else c_int;

// ---- Structs mirrored from uv.h (layouts must match) ----

pub const Buffer = if (@import("builtin").os.tag == .windows) extern struct {
    len: c_ulong,
    base: [*]u8,
} else extern struct {
    base: [*]u8,
    len: usize,
};

pub const DirEntry = extern struct {
    name: [*:0]const u8,
    entry_type: DirEntryType,
};

pub const DirEntries = extern struct {
    entries: [*]DirEntry,
    entry_count: usize,
};

pub const AddressInfo = if (@import("builtin").os.tag == .windows) extern struct {
    flags: c_int,
    family: c_int,
    socktype: c_int,
    protocol: c_int,
    addrlen: usize,
    canonname: ?[*:0]u8,
    addr: ?*@import("std").c.sockaddr,
    next: ?*@This(),
} else @import("std").c.addrinfo;

pub const SocketAddress = extern struct {
    bytes: [128]u8 align(8) = @splat(0),
};

pub const Timespec = extern struct {
    tv_sec: c_long,
    tv_nsec: c_long,
};

pub const Timeval64 = extern struct {
    tv_sec: i64,
    tv_usec: i32,
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
pub const AllocCallback = *const fn (handle: *Handle, suggested_size: usize, buffer: *Buffer) callconv(.c) void;
pub const ReadCallback = *const fn (stream: *Handle, read_count: isize, buffer: *const Buffer) callconv(.c) void;
pub const WriteCallback = *const fn (request: *Request, status: c_int) callconv(.c) void;
pub const ConnectCallback = *const fn (request: *Request, status: c_int) callconv(.c) void;
pub const LookupCallback = *const fn (request: *Request, status: c_int, result: ?*AddressInfo) callconv(.c) void;
pub const ConnectionCallback = *const fn (server: *Handle, status: c_int) callconv(.c) void;

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
pub extern fn uv_guess_handle(file: c_int) HandleType;
pub extern fn uv_fileno(handle: *const Handle, file: *OsFile) c_int;

// ---- Timers ----

pub extern fn uv_timer_init(loop: *Loop, timer: *Handle) c_int;
pub extern fn uv_timer_start(timer: *Handle, callback: TimerCallback, timeout: u64, repeat: u64) c_int;
pub extern fn uv_timer_stop(timer: *Handle) c_int;

// ---- Streams (terminals, pipes and sockets) ----

pub extern fn uv_tty_init(loop: *Loop, tty: *Handle, file: c_int, readable: c_int) c_int;
pub extern fn uv_pipe_init(loop: *Loop, pipe: *Handle, ipc: c_int) c_int;
pub extern fn uv_pipe_open(pipe: *Handle, file: c_int) c_int;
pub extern fn uv_read_start(stream: *Handle, alloc: AllocCallback, read: ReadCallback) c_int;
pub extern fn uv_read_stop(stream: *Handle) c_int;
pub extern fn uv_try_write(stream: *Handle, buffers: [*]const Buffer, buffer_count: c_uint) c_int;
pub extern fn uv_write(request: *Request, stream: *Handle, buffers: [*]const Buffer, buffer_count: c_uint, callback: WriteCallback) c_int;

// ---- TCP ----

pub extern fn uv_tcp_init(loop: *Loop, tcp: *Handle) c_int;
pub extern fn uv_tcp_open(tcp: *Handle, socket: OsSocket) c_int;
pub extern fn uv_tcp_nodelay(tcp: *Handle, enable: c_int) c_int;
pub extern fn uv_tcp_bind(tcp: *Handle, address: *const SocketAddress, flags: c_uint) c_int;
pub extern fn uv_tcp_connect(request: *Request, tcp: *Handle, address: *const SocketAddress, callback: ConnectCallback) c_int;
pub extern fn uv_tcp_getsockname(tcp: *const Handle, address: *SocketAddress, length: *c_int) c_int;
pub extern fn uv_tcp_getpeername(tcp: *const Handle, address: *SocketAddress, length: *c_int) c_int;
pub extern fn uv_listen(stream: *Handle, backlog: c_int, callback: ConnectionCallback) c_int;
pub extern fn uv_accept(server: *Handle, client: *Handle) c_int;

// ---- DNS ----

pub extern fn uv_getaddrinfo(loop: *Loop, request: *Request, callback: LookupCallback, node: [*:0]const u8, service: ?[*:0]const u8, hints: ?*const AddressInfo) c_int;
pub extern fn uv_freeaddrinfo(result: ?*AddressInfo) void;

// ---- IP addresses ----

pub extern fn uv_ip4_addr(ip: [*:0]const u8, port: c_int, address: *SocketAddress) c_int;
pub extern fn uv_ip6_addr(ip: [*:0]const u8, port: c_int, address: *SocketAddress) c_int;
pub extern fn uv_ip_name(address: *const SocketAddress, text: [*]u8, size: usize) c_int;

// ---- Clocks ----

pub extern fn uv_gettimeofday(time: *Timeval64) c_int;
pub extern fn uv_hrtime() u64;

// ---- Requests (shared by every request type) ----

pub extern fn uv_req_size(request_type: RequestType) usize;
pub extern fn uv_req_get_data(request: *const Request) ?*anyopaque;
pub extern fn uv_req_set_data(request: *Request, data: ?*anyopaque) void;
pub extern fn uv_cancel(request: *Request) c_int;

// ---- Errors ----

pub extern fn uv_err_name(status: c_int) [*:0]const u8;

// ---- Thread pool ----

pub extern fn uv_queue_work(loop: *Loop, request: *Request, work: WorkCallback, after_work: AfterWorkCallback) c_int;

// ---- File system: files (called synchronously from pool jobs, with no callback) ----

pub extern fn uv_fs_open(loop: *Loop, request: *Request, path: [*:0]const u8, flags: c_int, mode: c_int, callback: ?FsCallback) c_int;
pub extern fn uv_fs_read(loop: *Loop, request: *Request, file: c_int, buffers: [*]const Buffer, buffer_count: c_uint, offset: i64, callback: ?FsCallback) c_int;
pub extern fn uv_fs_write(loop: *Loop, request: *Request, file: c_int, buffers: [*]const Buffer, buffer_count: c_uint, offset: i64, callback: ?FsCallback) c_int;
pub extern fn uv_fs_close(loop: *Loop, request: *Request, file: c_int, callback: ?FsCallback) c_int;
pub extern fn uv_fs_fstat(loop: *Loop, request: *Request, file: c_int, callback: ?FsCallback) c_int;

// ---- File system: paths ----

pub extern fn uv_fs_stat(loop: *Loop, request: *Request, path: [*:0]const u8, callback: ?FsCallback) c_int;
pub extern fn uv_fs_lstat(loop: *Loop, request: *Request, path: [*:0]const u8, callback: ?FsCallback) c_int;
pub extern fn uv_fs_rename(loop: *Loop, request: *Request, path: [*:0]const u8, new_path: [*:0]const u8, callback: ?FsCallback) c_int;
pub extern fn uv_fs_unlink(loop: *Loop, request: *Request, path: [*:0]const u8, callback: ?FsCallback) c_int;
pub extern fn uv_fs_mkdir(loop: *Loop, request: *Request, path: [*:0]const u8, mode: c_int, callback: ?FsCallback) c_int;
pub extern fn uv_fs_rmdir(loop: *Loop, request: *Request, path: [*:0]const u8, callback: ?FsCallback) c_int;

// ---- File system: folders ----

pub extern fn uv_fs_opendir(loop: *Loop, request: *Request, path: [*:0]const u8, callback: ?FsCallback) c_int;
pub extern fn uv_fs_readdir(loop: *Loop, request: *Request, dir: *DirEntries, callback: ?FsCallback) c_int;
pub extern fn uv_fs_closedir(loop: *Loop, request: *Request, dir: *DirEntries, callback: ?FsCallback) c_int;

// ---- File system: request results ----

pub extern fn uv_fs_get_ptr(request: *const Request) ?*anyopaque;
pub extern fn uv_fs_get_statbuf(request: *Request) *Stat;
pub extern fn uv_fs_req_cleanup(request: *Request) void;
