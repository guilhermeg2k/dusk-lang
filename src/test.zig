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
    try dusk.runFile(try std.fmt.allocPrint(arena.allocator(), "test/success/{s}", .{src_file}));

    const output = buf[0..runtime.stdout_writer.end];
    try testing.expectEqualStrings(std.mem.trim(u8, expected_output, "\n\r "), std.mem.trim(u8, output, "\n\r "));
}

fn runCaseError(src_file: []const u8, expected: anytype) !void {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const cwd = std.Io.Dir.cwd();
    const file_path = try std.fmt.allocPrint(arena.allocator(), "test/error/{s}", .{src_file});
    const src = try cwd.readFileAlloc(testing.io, file_path, arena.allocator(), .unlimited);

    var buf: [65536]u8 = undefined;
    var stdout_writer_mock = std.Io.Writer.fixed(&buf);
    var dusk = Dusk{ .allocator = arena.allocator(), .stdout_writer = &stdout_writer_mock, .io = testing.io };

    try testing.expectError(expected, dusk.compile(src));
}

test "echo-2" {
    try runCase("echo-2.dsk", "Ecoing\n");
}

test "count" {
    try runCase("count.dsk", "0\n1\n0\n1\n2\n3\n4\n5\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n\n");
}

test "fibonacci" {
    try runCase("fibonnaci.dsk", "1\n1\n2\n55\n610\n6765\n");
}

test "is-prime" {
    try runCase("is-prime.dsk", "false\ntrue\ntrue\ntrue\nfalse\nfalse\n");
}

test "power" {
    try runCase("power.dsk", "1\n8\n25\n81\n");
}

test "expressions" {
    try runCase("expressions.dsk", "7\n9\n11\n1\n5\n11\n-2\n-8\n5\ntrue\ntrue\nfalse\nfalse\n46\n8\n");
}

test "mut array" {
    try runCase("mut-array.dsk", "1,3,5,7");
}

test "bubble sort" {
    try runCase("bubble-sort.dsk", "1,2,22,51,100,321,500,21312,21312");
}

test "filter evens" {
    try runCase("filter-evens.dsk", "10,34,100,20,10,40");
}

test "reverse array" {
    try runCase("reverse-array.dsk", "Não,Eu,Hoje,Doutor,General,Salve");
}

test "find-max" {
    try runCase("find-max.dsk", "21312312321\n0\n10000\n1");
}

test "bin search" {
    try runCase("bin-search.dsk", "9");
}

test "concat" {
    try runCase("concat.dsk", "Hello World");
}

test "inline return" {
    try runCase("inline-return.dsk", "20");
}

test "else if" {
    try runCase("else-if.dsk", "negative\n-23");
}

test "count odd" {
    try runCase("echo-odd.dsk", "1\n1\n1\n3\n1\n5\n1\n7\n1\n");
}

test "reverse count" {
    try runCase("reverse-count.dsk", "9\n8\n7\n6\n5\n4\n3\n2\n1\n");
}

test "struct" {
    try runCase("struct.dsk", "FSDF@!#W!@Z#!@Geromel");
}

test "multi-line-fn-def" {
    try runCase("multiline-fn.dsk", "1\n8\n25\n81\n");
}

test "anonymous structs" {
    try runCase("anom-struct.dsk",
        \\{"x":10,"y":10}
        \\{"x":20,"y":20}
        \\{"x":30,"y":30}
        \\{"x":50,"y":50}
        \\{"x":40,"y":40}
    );
}

test "static fields structs" {
    try runCase("static-struct.dsk",
        \\127.0.0.1:9090/me
        \\127.0.0.1:9090/me
        \\:9090
    );
}

test "simple tree" {
    try runCase("simple-tree.dsk",
        \\{"left":{"value":0,"left":null,"right":null},"right":{"value":100,"left":null,"right":null},"value":50}
        \\{"value":0,"left":null,"right":null}
        \\{"value":888,"left":null,"right":null}
    );
}

test "linked list" {
    try runCase("linked-list.dsk",
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
    try runCase("nullable-capture.dsk", "Geromel Dr.");
}

test "struct nullable chaining" {
    try runCase("struct-nullable.dsk",
        \\country name is Brasil
        \\state name is DF
        \\country code is BR
        \\phone is 40028922
        \\phone is 40028922
        \\DF
    );
}

test "default_values" {
    try runCase("default-values.dsk",
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
    try runCase("nested-struct.dsk",
        \\{"id":"1","username":"Rogerinho","auth_provider":"G","address":{"country":"Brasil","state":"DF","city":"Brasília","street":"Eixão Sul"},"default_address":{"country":"Brasil","state":"SP","city":"São Paulo","street":"Av. Paulista"}}
    );
}

test "simple pipe" {
    try runCase("simple-pipe.dsk", "dusk!");
}

test "float ints" {
    try runCase("float-ints.dsk", "dusk!");
}

test "division" {
    try runCase("div.dsk", "dusk!");
}

test "auto promotion" {
    try runCase("auto-promotion.dsk", "");
}

test "sema error: type mismatch" {
    try runCaseError("type-mismatch.dsk", error.InvalidType);
}

test "sema error: not defined" {
    try runCaseError("not-defined.dsk", error.NotDefined);
}

test "sema error: not mutable" {
    try runCaseError("not-mutable.dsk", error.NotMutable);
}

test "sema error: already defined" {
    try runCaseError("already-defined.dsk", error.AlreadyDefined);
}

test "sema error: invalid struct field" {
    try runCaseError("invalid-struct-field.dsk", error.InvalidStructField);
}

test "sema error: invalid number of args" {
    try runCaseError("invalid-number-of-args.dsk", error.InvalidNumberOfArgs);
}

test "sema error: nullable must be unwrapped" {
    try runCaseError("nullable-must-be-unwrapped.dsk", error.NullableMustBeUnwrapped);
}

test "sema error: unnecessary optional chain" {
    try runCaseError("unnecessary-optional-chain.dsk", error.UnecessaryOptionalChain);
}

test "sema error: invalid return type" {
    try runCaseError("invalid-return-type.dsk", error.InvalidReturnType);
}

test "sema error: invalid struct function" {
    try runCaseError("invalid-struct-function.dsk", error.InvalidStructField);
}

test "sema error: invalid static struct field" {
    try runCaseError("invalid-static-struct-field.dsk", error.InvalidStructField);
}

test "sema error: cant infer array literal type" {
    try runCaseError("cant-infer-array-literal-type.dsk", error.CantInferArrayLiteralType);
}

test "sema error: invalid parameter type" {
    try runCaseError("invalid-parameter-type.dsk", error.InvalidParameterType);
}

test "sema error: primitive params cant be mutable" {
    try runCaseError("primitive-params-cant-be-mutable.dsk", error.PrimitiveParamsCantBeMutable);
}

test "sema error: unwrapped cant be mutable" {
    try runCaseError("unwrapped-cant-be-mutable.dsk", error.UnwrappedCantBeMutable);
}

test "sema error: missing argument" {
    try runCaseError("missing-argument.dsk", error.MissingArgument);
}

test "sema error: invalid indexing" {
    try runCaseError("invalid-indexing.dsk", error.InvalidIndexing);
}

test "sema error: unknown type" {
    try runCaseError("unknown-type.dsk", error.UnknownType);
}

test "sema error: if condition not bool" {
    try runCaseError("if-condition-not-bool.dsk", error.InvalidType);
}

test "sema error: for condition not bool" {
    try runCaseError("for-condition-not-bool.dsk", error.InvalidType);
}

test "sema error: if capture on nonnullable" {
    try runCaseError("if-capture-on-nonnullable.dsk", error.InvalidType);
}

test "sema error: assign type mismatch" {
    try runCaseError("assign-type-mismatch.dsk", error.InvalidType);
}

test "sema error: array assign type mismatch" {
    try runCaseError("array-assign-type-mismatch.dsk", error.InvalidType);
}

test "sema error: struct field assign type mismatch" {
    try runCaseError("struct-field-assign-type-mismatch.dsk", error.InvalidType);
}

test "sema error: static field assign type mismatch" {
    try runCaseError("static-field-assign-type-mismatch.dsk", error.InvalidType);
}

test "sema error: call non function" {
    try runCaseError("call-non-function.dsk", error.InvalidType);
}

test "sema error: method call on nonstruct" {
    try runCaseError("method-call-on-nonstruct.dsk", error.InvalidType);
}

test "sema error: fn arg type mismatch" {
    try runCaseError("fn-arg-type-mismatch.dsk", error.InvalidType);
}

test "sema error: binary op type mismatch" {
    try runCaseError("binary-op-type-mismatch.dsk", error.InvalidType);
}

test "sema error: binary bool type mismatch" {
    try runCaseError("binary-bool-type-mismatch.dsk", error.InvalidType);
}

test "sema error: unary op type mismatch" {
    try runCaseError("unary-op-type-mismatch.dsk", error.InvalidType);
}

test "sema error: index into non array" {
    try runCaseError("index-into-non-array.dsk", error.InvalidType);
}

test "sema error: struct field default type mismatch" {
    try runCaseError("struct-field-default-type-mismatch.dsk", error.InvalidType);
}

test "sema error: param default type mismatch" {
    try runCaseError("param-default-type-mismatch.dsk", error.InvalidType);
}

test "sema error: indexed assign notmut root" {
    try runCaseError("indexed-assign-notmut-root.dsk", error.NotMutable);
}

test "sema error: static field not mutable" {
    try runCaseError("static-field-not-mutable.dsk", error.NotMutable);
}

test "sema error: self binding not mut" {
    try runCaseError("self-binding-not-mut.dsk", error.NotMutable);
}

test "sema error: assign undefined" {
    try runCaseError("assign-undefined.dsk", error.NotDefined);
}

test "sema error: indexed assign undefined" {
    try runCaseError("indexed-assign-undefined.dsk", error.NotDefined);
}

test "sema error: call undefined" {
    try runCaseError("call-undefined.dsk", error.NotDefined);
}

test "sema error: duplicate fn def" {
    try runCaseError("duplicate-fn-def.dsk", error.AlreadyDefined);
}

test "sema error: duplicate param name" {
    try runCaseError("duplicate-param-name.dsk", error.AlreadyDefined);
}

test "sema error: bare return in nonvoid fn" {
    try runCaseError("bare-return-in-nonvoid-fn.dsk", error.InvalidReturnType);
}
