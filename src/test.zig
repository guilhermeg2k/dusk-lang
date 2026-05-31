const std = @import("std");
const testing = std.testing;
const runtime = @import("runtime.zig").QjsRuntime;
const Dusk = @import("dusk.zig").Dusk;

fn runCase(src_file: []const u8, expected_output: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    var buf: [65536]u8 = undefined;
    var stdout_writer_mock = std.Io.Writer.fixed(&buf);

    var dusk = Dusk{ .allocator = arena.allocator(), .stdout_writer = &stdout_writer_mock, .io = testing.io };
    try dusk.runFile(src_file);

    const output = buf[0..runtime.stdout_writer.end];
    try testing.expectEqualStrings(std.mem.trim(u8, expected_output, "\n\r "), std.mem.trim(u8, output, "\n\r "));
}

test "echo-2" {
    try runCase("test/echo-2.dsk", "Ecoing\n");
}

test "count" {
    try runCase("test/count.dsk", "0\n1\n0\n1\n2\n3\n4\n5\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n\n");
}

test "fibonacci" {
    try runCase("test/fibonnaci.dsk", "1\n1\n2\n55\n610\n6765\n");
}

test "is-prime" {
    try runCase("test/is-prime.dsk", "false\ntrue\ntrue\ntrue\nfalse\nfalse\n");
}

test "power" {
    try runCase("test/power.dsk", "1\n8\n25\n81\n");
}

test "expressions" {
    try runCase("test/expressions.dsk", "7\n9\n11\n1\n5\n11\n-2\n-8\n5\ntrue\ntrue\nfalse\nfalse\n46\n8\n");
}

test "mut array" {
    try runCase("test/mut-array.dsk", "1,3,5,7");
}

test "bubble sort" {
    try runCase("test/bubble-sort.dsk", "1,2,22,51,100,321,500,21312,21312");
}

test "filter evens" {
    try runCase("test/filter-evens.dsk", "10,34,100,20,10,40");
}

test "reverse array" {
    try runCase("test/reverse-array.dsk", "Não,Eu,Hoje,Doutor,General,Salve");
}

test "find-max" {
    try runCase("test/find-max.dsk", "21312312321\n0\n10000\n1");
}

test "bin search" {
    try runCase("test/bin-search.dsk", "9");
}

test "concat" {
    try runCase("test/concat.dsk", "Hello World");
}

test "inline return" {
    try runCase("test/inline-return.dsk", "20");
}

test "else if" {
    try runCase("test/else-if.dsk", "negative\n-23");
}

test "count odd" {
    try runCase("test/echo-odd.dsk", "1\n1\n1\n3\n1\n5\n1\n7\n1\n");
}

test "reverse count" {
    try runCase("test/reverse-count.dsk", "9\n8\n7\n6\n5\n4\n3\n2\n1\n");
}

test "struct" {
    try runCase("test/struct.dsk", "FSDF@!#W!@Z#!@Geromel");
}

test "multi-line-fn-def" {
    try runCase("test/multiline-fn.dsk", "1\n8\n25\n81\n");
}

test "anonymous structs" {
    try runCase("test/anom-struct.dsk",
        \\{"x":10,"y":10}
        \\{"x":20,"y":20}
        \\{"x":30,"y":30}
        \\{"x":50,"y":50}
        \\{"x":40,"y":40}
    );
}

test "static fields structs" {
    try runCase("test/static-struct.dsk",
        \\127.0.0.1:9090/me
        \\127.0.0.1:9090/me
        \\:9090
    );
}

test "simple tree" {
    try runCase("test/simple-tree.dsk",
        \\{"left":{"value":0,"left":null,"right":null},"right":{"value":100,"left":null,"right":null},"value":50}
        \\{"value":0,"left":null,"right":null}
        \\{"value":888,"left":null,"right":null}
    );
}

test "linked list" {
    try runCase("test/linked-list.dsk",
        \\60
        \\50
        \\10
        \\20
        \\30
        \\40
        \\true
        \\6
        \\60
        \\50
        \\10
        \\20
        \\4
        \\false
        \\10
        \\20
        \\2
    );
}

test "capture nullable" {
    try runCase("test/nullable-capture.dsk", "Geromel Dr.");
}

test "struct nullable chaining" {
    try runCase("test/struct-nullable.dsk",
        \\country name is Brasil
        \\state name is DF
        \\country code is BR
        \\phone is 40028922
        \\phone is 40028922
        \\DF
    );
}

test "default_values" {
    try runCase("test/default-values.dsk",
        \\52
        \\52
        \\100
        \\100
        \\20
        \\20
        \\47
        \\{"username":"Rogerinho","auth_method":"DUSK"}
        \\{"username":"Rogerinho","auth_method":"Y"}
        \\{"username":"Rogerinho","auth_method":"DUSK"}
        \\{"username":"Rogerinho","auth_method":"T"}
    );
}

test "nested structs" {
    try runCase("test/nested-struct.dsk",
        \\{"id":"1","username":"Rogerinho","auth_provider":"G","address":{"country":"Brasil","state":"DF","city":"Brasília","street":"Eixão Sul"},"default_address":{"country":"Brasil","state":"SP","city":"São Paulo","street":"Av. Paulista"}}
    );
}

test "simple pipe" {
    try runCase("test/simple-pipe.dsk", "dusk!");
}

test "float ints" {
    try runCase("test/float-ints.dsk", "dusk!");
}

test "division" {
    try runCase("test/div.dsk", "dusk!");
}
