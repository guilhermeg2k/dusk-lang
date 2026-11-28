const std = @import("std");
const uv = @import("../tasks/loop/uv.zig");
const HandleOwner = @import("../tasks/event_loop.zig").HandleOwner;
const onHandleClosed = @import("../tasks/event_loop.zig").onHandleClosed;

const handle_alignment = std.mem.Alignment.@"16";

pub const TerminalSize = struct {
    columns: u32,
    rows: u32,
};

pub const TerminalOutput = struct {
    const Self = @This();

    owner: HandleOwner = .{ .kind = .terminal_output },
    allocator: std.mem.Allocator,
    handle_memory: []align(16) u8,
    is_closing: bool = false,

    pub fn open(allocator: std.mem.Allocator, loop: *uv.Loop, file: c_int) !*Self {
        const self = try allocator.create(Self);
        errdefer allocator.destroy(self);
        const memory = try allocator.alignedAlloc(u8, handle_alignment, uv.uv_handle_size(.tty));
        errdefer allocator.free(memory);
        if (uv.uv_tty_init(loop, @ptrCast(memory.ptr), file, 0) != 0) {
            return error.TerminalInitFailed;
        }
        self.* = .{ .allocator = allocator, .handle_memory = memory };
        uv.uv_handle_set_data(self.uvHandle(), &self.owner);
        return self;
    }

    pub fn destroy(self: *Self) void {
        self.allocator.free(self.handle_memory);
        self.allocator.destroy(self);
    }

    pub fn size(self: *Self) ?TerminalSize {
        var columns: c_int = 0;
        var rows: c_int = 0;
        if (uv.uv_tty_get_winsize(self.uvHandle(), &columns, &rows) != 0) {
            return null;
        }
        const is_known = columns > 0 and rows > 0;
        if (!is_known) {
            return null;
        }
        return .{ .columns = @intCast(columns), .rows = @intCast(rows) };
    }

    pub fn close(self: *Self) void {
        if (self.is_closing) return;
        self.is_closing = true;
        uv.uv_close(self.uvHandle(), onHandleClosed);
    }

    fn uvHandle(self: *Self) *uv.Handle {
        return @ptrCast(self.handle_memory.ptr);
    }
};
