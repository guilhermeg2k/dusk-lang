const std = @import("std");
const Recurrence = @import("recurrence.zig").Recurrence;

pub fn main(init: std.process.Init) !void {
    const arena = init.arena.allocator();
    const args = try init.minimal.args.toSlice(arena);
    const has_queries = args.len == 3 and std.mem.eql(u8, args[2], "--verify");
    if (args.len != 2 and !has_queries) {
        return error.ExpectedCorpusPath;
    }
    const corpus = try std.Io.Dir.cwd().readFileAlloc(init.io, args[1], arena, .limited(16 * 1024 * 1024));
    var input = std.Io.Reader.fixed(corpus);
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    defer std.debug.assert(debug_allocator.deinit() == .ok);
    var zone_count: usize = 0;
    var transition_count: usize = 0;
    var footer_count: usize = 0;
    var query_count: usize = 0;
    while (input.seek < corpus.len) {
        const name_length = try input.takeInt(u32, .big);
        const payload_length = try input.takeInt(u32, .big);
        const name = try input.take(name_length);
        const payload = try input.take(payload_length);
        var reader = std.Io.Reader.fixed(payload);
        var zone = try std.tz.Tz.parse(debug_allocator.allocator(), &reader);
        defer zone.deinit();
        zone_count += 1;
        transition_count += zone.transitions.len;
        if (zone.footer != null) {
            footer_count += 1;
        }
        for (zone.transitions, 0..) |transition, i| {
            const is_out_of_order = i > 0 and zone.transitions[i - 1].ts >= transition.ts;
            if (is_out_of_order) {
                return error.UnorderedTransitions;
            }
        }
        const recurrence = if (zone.footer) |text| try Recurrence.parse(text) else null;
        if (!has_queries) continue;
        const queries = try input.takeInt(u32, .big);
        for (0..queries) |_| {
            const timestamp = try input.takeInt(i64, .big);
            const expected = try input.takeInt(i32, .big);
            const actual = offsetSeconds(&zone, recurrence, timestamp);
            if (actual != expected) {
                std.debug.print("{s} at {d}: {d} != {d}\n", .{ name, timestamp, actual, expected });
                return error.OffsetMismatch;
            }
            query_count += 1;
        }
    }
    var buffer: [512]u8 = undefined;
    var stdout = std.Io.File.stdout().writerStreaming(init.io, &buffer);
    try stdout.interface.print("{{\"zones\":{d},\"transitions\":{d},\"footers\":{d},\"offset_comparisons\":{d}}}\n", .{
        zone_count, transition_count, footer_count, query_count,
    });
    try stdout.interface.flush();
}

fn offsetSeconds(zone: *const std.tz.Tz, recurrence: ?Recurrence, timestamp: i64) i32 {
    const needs_recurrence = recurrence != null and (zone.transitions.len == 0 or timestamp > zone.transitions[zone.transitions.len - 1].ts);
    if (needs_recurrence) {
        return recurrence.?.offsetSeconds(timestamp);
    }
    var left: usize = 0;
    var right = zone.transitions.len;
    while (left < right) {
        const middle = left + (right - left) / 2;
        if (zone.transitions[middle].ts <= timestamp) {
            left = middle + 1;
        } else {
            right = middle;
        }
    }
    return if (left == 0) zone.timetypes[0].offset else zone.transitions[left - 1].timetype.offset;
}
