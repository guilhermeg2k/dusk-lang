const std = @import("std");

pub fn stringCompare(_: void, a: []const u8, b: []const u8) bool {
    return std.mem.lessThan(u8, a, b);
}

pub fn binarySearch(comptime T: type, comptime field: []const u8, items: []const T, key: anytype) ?T {
    const K = @TypeOf(key);
    const idx = std.sort.binarySearch(T, items, key, struct {
        fn cmp(k: K, item: T) std.math.Order {
            return std.math.order(k, @field(item, field));
        }
    }.cmp);
    return if (idx) |i| items[i] else null;
}
