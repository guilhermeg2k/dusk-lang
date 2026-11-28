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

pub const CompressionError = enum { InvalidData, TooLarge, InvalidLevel, Finished, Other };

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
    const first_capacity = @min(@max(input.len *| 4, first_output_capacity), limit);
    if (max != null) {
        try output.ensureTotalCapacityPrecise(allocator, first_capacity);
    } else {
        try output.ensureTotalCapacity(allocator, first_capacity);
    }
    var rest = input;
    while (true) {
        if (output.items.len == output.capacity) {
            const next_capacity = @min(output.capacity *| 2, limit);
            if (max != null) {
                try output.ensureTotalCapacityPrecise(allocator, next_capacity);
            } else {
                try output.ensureTotalCapacity(allocator, next_capacity);
            }
        }
        const raw_room = output.allocatedSlice()[output.items.len..];
        const room = raw_room[0..@min(raw_room.len, limit - output.items.len)];
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

pub const StreamError = error{ Finished, InvalidData, TooLarge, OutOfMemory };

const first_part_capacity = 16 * 1024;

pub const CompressionStream = struct {
    zip: *Zip,
    format: Format,
    max: ?usize = null,
    produced: usize = 0,
    is_at_end: bool = false,

    pub fn destroy(self: *CompressionStream, allocator: std.mem.Allocator) void {
        dusk_zip_free(self.zip);
        allocator.destroy(self);
    }
};

pub fn compressPart(allocator: std.mem.Allocator, zip: *Zip, input: []const u8, mode: FlushMode) StreamError![]u8 {
    var output: std.ArrayList(u8) = .empty;
    errdefer output.deinit(allocator);
    var rest = input;
    while (true) {
        try output.ensureUnusedCapacity(allocator, @max(first_part_capacity, rest.len / 2));
        const room = output.unusedCapacitySlice();
        var consumed: usize = 0;
        var produced: usize = 0;
        const status = dusk_zip_compress(zip, rest.ptr, rest.len, room.ptr, room.len, mode, &consumed, &produced);
        output.items.len += produced;
        rest = rest[consumed..];
        switch (status) {
            .done => return output.toOwnedSlice(allocator),
            .ok => {
                const is_complete = rest.len == 0 and produced < room.len;
                if (is_complete) {
                    return output.toOwnedSlice(allocator);
                }
            },
            .out_of_memory => return error.OutOfMemory,
            else => return error.InvalidData,
        }
    }
}

pub fn decompressPart(allocator: std.mem.Allocator, stream: *CompressionStream, input: []const u8) StreamError![]u8 {
    var output: std.ArrayList(u8) = .empty;
    errdefer output.deinit(allocator);
    if (input.len == 0) {
        return output.toOwnedSlice(allocator);
    }
    if (stream.is_at_end) {
        if (stream.format != .gzip or dusk_zip_restart(stream.zip) != .ok) {
            return error.InvalidData;
        }
        stream.is_at_end = false;
    }
    const limit = if (stream.max) |max| (max -| stream.produced) +| 1 else std.math.maxInt(usize);
    const first_capacity = @min(@max(input.len *| 4, first_part_capacity), limit);
    if (stream.max != null) {
        try output.ensureTotalCapacityPrecise(allocator, first_capacity);
    } else {
        try output.ensureTotalCapacity(allocator, first_capacity);
    }
    var rest = input;
    while (true) {
        if (output.items.len == output.capacity) {
            const next_capacity = @min(output.capacity *| 2, limit);
            if (stream.max != null) {
                try output.ensureTotalCapacityPrecise(allocator, next_capacity);
            } else {
                try output.ensureTotalCapacity(allocator, next_capacity);
            }
        }
        const raw_room = output.unusedCapacitySlice();
        const room = raw_room[0..@min(raw_room.len, limit - output.items.len)];
        var consumed: usize = 0;
        var produced: usize = 0;
        const status = dusk_zip_decompress(stream.zip, rest.ptr, rest.len, room.ptr, room.len, &consumed, &produced);
        output.items.len += produced;
        rest = rest[consumed..];
        if (output.items.len >= limit) {
            return error.TooLarge;
        }
        switch (status) {
            .done => {
                if (rest.len == 0) {
                    stream.is_at_end = true;
                    break;
                }
                if (stream.format != .gzip or dusk_zip_restart(stream.zip) != .ok) {
                    return error.InvalidData;
                }
            },
            .ok => {
                const needs_more_input = rest.len == 0 and produced < room.len;
                if (needs_more_input) {
                    break;
                }
                if (consumed == 0 and produced == 0) {
                    return error.InvalidData;
                }
            },
            .out_of_memory => return error.OutOfMemory,
            else => return error.InvalidData,
        }
    }
    stream.produced += output.items.len;
    return output.toOwnedSlice(allocator);
}
