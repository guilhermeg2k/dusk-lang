const std = @import("std");
const testing = std.testing;
const Dusk = @import("dusk.zig").Dusk;

fn runCase(src_file: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const cwd = std.Io.Dir.cwd();
    const file_path = try std.fmt.allocPrint(arena.allocator(), "test/success/{s}", .{src_file});
    const src = try cwd.readFileAlloc(testing.io, file_path, arena.allocator(), .unlimited);

    var dusk = Dusk{ .allocator = arena.allocator(), .stdout_writer = null, .io = testing.io };
    _ = try dusk.compile(src);
}

fn runCaseError(src_file: []const u8, expected: anytype) !void {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const cwd = std.Io.Dir.cwd();
    const file_path = try std.fmt.allocPrint(arena.allocator(), "test/error/{s}", .{src_file});
    const src = try cwd.readFileAlloc(testing.io, file_path, arena.allocator(), .unlimited);

    var dusk = Dusk{ .allocator = arena.allocator(), .stdout_writer = null, .io = testing.io };
    try testing.expectError(expected, dusk.compile(src));
}

test "echo-2" {
    try runCase("echo-2.dsk");
}

test "fibonacci" {
    try runCase("fibonnaci.dsk");
}

test "is-prime" {
    try runCase("is-prime.dsk");
}

test "power" {
    try runCase("power.dsk");
}

test "expressions" {
    try runCase("expressions.dsk");
}

test "mut array" {
    try runCase("mut-array.dsk");
}

test "bubble sort" {
    try runCase("bubble-sort.dsk");
}

test "filter evens" {
    try runCase("filter-evens.dsk");
}

test "reverse array" {
    try runCase("reverse-array.dsk");
}

test "find-max" {
    try runCase("find-max.dsk");
}

test "bin search" {
    try runCase("bin-search.dsk");
}

test "inline return" {
    try runCase("inline-return.dsk");
}

test "struct" {
    try runCase("struct.dsk");
}

test "multi-line-fn-def" {
    try runCase("multiline-fn.dsk");
}

test "anonymous structs" {
    try runCase("anom-struct.dsk");
}

test "static fields structs" {
    try runCase("static-struct.dsk");
}

test "simple tree" {
    try runCase("simple-tree.dsk");
}

test "linked list" {
    try runCase("linked-list.dsk");
}

test "capture nullable" {
    try runCase("nullable-capture.dsk");
}

test "struct nullable chaining" {
    try runCase("struct-nullable.dsk");
}

test "default_values" {
    try runCase("default-values.dsk");
}

test "nested structs" {
    try runCase("nested-struct.dsk");
}

test "simple pipe" {
    try runCase("simple-pipe.dsk");
}

test "float ints" {
    try runCase("float-ints.dsk");
}

test "division" {
    try runCase("div.dsk");
}

test "auto promotion" {
    try runCase("auto-promotion.dsk");
}

test "break continue" {
    try runCase("break-continue.dsk");
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
