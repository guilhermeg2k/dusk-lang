const std = @import("std");

pub const Zip = opaque {};

pub const Status = enum(c_int) {
    ok = 0,
    done = 1,
    invalid = -1,
    out_of_memory = -2,
    _,
};

pub const Format = enum(c_int) { gzip, zlib, deflate };

pub const FlushMode = enum(c_int) { none, flush, finish };

pub const max_level = 9;

pub extern fn dusk_zip_compressor_new(format: Format, level: c_int) ?*Zip;
pub extern fn dusk_zip_decompressor_new(format: Format) ?*Zip;
pub extern fn dusk_zip_free(zip: ?*Zip) void;
pub extern fn dusk_zip_bound(zip: *Zip, length: usize) usize;
pub extern fn dusk_zip_restart(zip: *Zip) Status;
pub extern fn dusk_zip_compress(zip: *Zip, in: [*]const u8, in_len: usize, out: [*]u8, out_cap: usize, mode: FlushMode, consumed: *usize, produced: *usize) Status;
pub extern fn dusk_zip_decompress(zip: *Zip, in: [*]const u8, in_len: usize, out: [*]u8, out_cap: usize, consumed: *usize, produced: *usize) Status;

pub const DecompressError = error{ InvalidData, TooLarge, OutOfMemory };

const first_output_capacity = 64 * 1024;
const gzip_magic = [_]u8{ 0x1f, 0x8b };

pub fn compressAll(allocator: std.mem.Allocator, format: Format, level: u4, input: []const u8) error{OutOfMemory}![]u8 {
    const zip = dusk_zip_compressor_new(format, level) orelse return error.OutOfMemory;
    defer dusk_zip_free(zip);
    const output = try allocator.alloc(u8, dusk_zip_bound(zip, input.len));
    errdefer allocator.free(output);
    var consumed: usize = 0;
    var produced: usize = 0;
    const status = dusk_zip_compress(zip, input.ptr, input.len, output.ptr, output.len, .finish, &consumed, &produced);
    if (status != .done) {
        return error.OutOfMemory;
    }
    return allocator.realloc(output, produced);
}

pub fn decompressAll(allocator: std.mem.Allocator, format: Format, input: []const u8, max: ?usize) DecompressError![]u8 {
    const zip = dusk_zip_decompressor_new(format) orelse return error.OutOfMemory;
    defer dusk_zip_free(zip);
    var output: std.ArrayList(u8) = .empty;
    errdefer output.deinit(allocator);
    const limit = if (max) |bytes| bytes +| 1 else std.math.maxInt(usize);
    try output.ensureTotalCapacity(allocator, @min(@max(input.len *| 4, first_output_capacity), limit));
    var rest = input;
    while (true) {
        if (output.items.len == output.capacity) {
            try output.ensureTotalCapacity(allocator, @min(output.capacity *| 2, limit));
        }
        const room = output.allocatedSlice()[output.items.len..];
        var consumed: usize = 0;
        var produced: usize = 0;
        const status = dusk_zip_decompress(zip, rest.ptr, rest.len, room.ptr, room.len, &consumed, &produced);
        output.items.len += produced;
        rest = rest[consumed..];
        if (output.items.len >= limit) {
            return error.TooLarge;
        }
        switch (status) {
            .done => {
                if (rest.len == 0) {
                    return output.toOwnedSlice(allocator);
                }
                const starts_next_member = format == .gzip and std.mem.startsWith(u8, rest, &gzip_magic);
                if (!starts_next_member) {
                    return error.InvalidData;
                }
                if (dusk_zip_restart(zip) != .ok) {
                    return error.InvalidData;
                }
            },
            .ok => {
                const is_truncated = rest.len == 0 and produced < room.len;
                const is_stuck = consumed == 0 and produced == 0;
                if (is_truncated or is_stuck) {
                    return error.InvalidData;
                }
            },
            .out_of_memory => return error.OutOfMemory,
            else => return error.InvalidData,
        }
    }
}
