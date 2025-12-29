const std = @import("std");
const Dusk = @import("dusk.zig").Dusk; // Import your main struct
const testing = std.testing;

const TestCase = struct {
    name: []const u8,
    src_file: []const u8,
    expected_output: []const u8,
};

test "Test: Compile and Run" {
    const allocator = testing.allocator;

    const cases = [_]TestCase{
        .{
            .name = "echo-2",
            .src_file = "test/echo-2.dsk",
            .expected_output = "Ecoing\n",
        },
        .{
            .name = "count",
            .src_file = "test/count.dsk",
            .expected_output =
            \\0
            \\1
            \\0
            \\1
            \\2
            \\3
            \\4
            \\5
            \\0
            \\1
            \\2
            \\3
            \\4
            \\5
            \\6
            \\7
            \\8
            \\9
            \\10
            \\11
            \\12
            \\13
            \\14
            \\15
            \\
            ,
        },
        .{
            .name = "fibonacci",
            .src_file = "test/fibonnaci.dsk",
            .expected_output = "1\n1\n2\n55\n610\n6765\n",
        },
        .{
            .name = "is-prime",
            .src_file = "test/is-prime.dsk",
            .expected_output = "false\ntrue\ntrue\ntrue\nfalse\nfalse\n",
        },
        .{
            .name = "power",
            .src_file = "test/power.dsk",
            .expected_output = "1\n8\n25\n81\n",
        },
        .{
            .name = "expressions",
            .src_file = "test/expressions.dsk",
            .expected_output = "7\n9\n11\n1\n5\n11\n-2\n-8\n5\ntrue\ntrue\nfalse\nfalse\n46\n8\n",
        },
        .{ .name = "mut array", .src_file = "test/mut-array.dsk", .expected_output = "[ 1, 3, 5, 7 ]" },
    };

    try std.fs.cwd().makePath("test_build");

    for (cases) |case| {
        std.debug.print("Testing: {s}\n", .{case.name});

        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        var dusk = Dusk{ .allocator = arena.allocator() };

        const output_filename = try std.fmt.allocPrint(arena.allocator(), "test_build/{s}.js", .{case.name});

        const compiled_path = dusk.compileFile(case.src_file, output_filename) catch |e| {
            std.debug.print("Test Failed: {s}\n", .{case.name});
            return e;
        };
        const output = try dusk.runCaptured(compiled_path);

        try testing.expectEqualStrings(std.mem.trim(u8, case.expected_output, "\n\r "), std.mem.trim(u8, output, "\n\r "));
    }
}
