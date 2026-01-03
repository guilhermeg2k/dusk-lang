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
            .expected_output = "0\n1\n0\n1\n2\n3\n4\n5\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n\n",
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
        .{
            .name = "mut array",
            .src_file = "test/mut-array.dsk",
            .expected_output = "1,3,5,7",
        },
        .{
            .name = "bubble sort",
            .src_file = "test/bubble-sort.dsk",
            .expected_output = "1,2,22,51,100,321,500,21312,21312",
        },
        .{
            .name = "filter evens",
            .src_file = "test/filter-evens.dsk",
            .expected_output = "10,34,100,20,10,40",
        },
        .{
            .name = "reverse array",
            .src_file = "test/reverse-array.dsk",
            .expected_output = "Não,Eu,Hoje,Doutor,General,Salve",
        },
        .{
            .name = "find-max",
            .src_file = "test/find-max.dsk",
            .expected_output = "21312312321\n0\n10000\n1",
        },
        .{
            .name = "bin search",
            .src_file = "test/bin-search.dsk",
            .expected_output = "9",
        },
        .{
            .name = "concat",
            .src_file = "test/concat.dsk",
            .expected_output = "Hello World",
        },
        .{
            .name = "inline return",
            .src_file = "test/inline-return.dsk",
            .expected_output = "20",
        },
        .{
            .name = "else if",
            .src_file = "test/else-if.dsk",
            .expected_output = "negative\n-23",
        },
        .{
            .name = "count odd",
            .src_file = "test/echo-odd.dsk",
            .expected_output = "1\n1\n1\n3\n1\n5\n1\n7\n1\n",
        },
        .{
            .name = "reverse count",
            .src_file = "test/reverse-count.dsk",
            .expected_output = "9\n8\n7\n6\n5\n4\n3\n2\n1\n",
        },
    };

    for (cases) |case| {
        std.debug.print("Testing: {s}\n", .{case.name});

        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();

        var buf: [65536]u8 = undefined;
        const stdout_writer_mock = std.io.Writer.fixed(&buf);

        var dusk = Dusk{ .allocator = arena.allocator(), .stdout_writer = stdout_writer_mock };
        try dusk.runFile(case.src_file);

        const output = buf[0..runtime.stdout_writer.end];
        try testing.expectEqualStrings(std.mem.trim(u8, case.expected_output, "\n\r "), std.mem.trim(u8, output, "\n\r "));
    }
}

const TestCase = struct {
    name: []const u8,
    src_file: []const u8,
    expected_output: []const u8,
};

const testing = std.testing;

const runtime = @import("runtime.zig").QjsRuntime;
const Dusk = @import("dusk.zig").Dusk;
const builtin = @import("builtin");
const std = @import("std");
