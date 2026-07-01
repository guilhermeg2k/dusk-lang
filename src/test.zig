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
    try dusk.compileAndRun(src);
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
    try runCaseError("sema/type-mismatch.dsk", error.InvalidType);
}

test "sema error: not defined" {
    try runCaseError("sema/not-defined.dsk", error.NotDefined);
}

test "sema error: not mutable" {
    try runCaseError("sema/not-mutable.dsk", error.NotMutable);
}

test "sema error: already defined" {
    try runCaseError("sema/already-defined.dsk", error.AlreadyDefined);
}

test "sema error: invalid struct field" {
    try runCaseError("sema/invalid-struct-field.dsk", error.InvalidStructField);
}

test "sema error: invalid number of args" {
    try runCaseError("sema/invalid-number-of-args.dsk", error.InvalidNumberOfArgs);
}

test "sema error: nullable must be unwrapped" {
    try runCaseError("sema/nullable-must-be-unwrapped.dsk", error.NullableMustBeUnwrapped);
}

test "sema error: unnecessary optional chain" {
    try runCaseError("sema/unnecessary-optional-chain.dsk", error.UnecessaryOptionalChain);
}

test "sema error: invalid return type" {
    try runCaseError("sema/invalid-return-type.dsk", error.InvalidReturnType);
}

test "sema error: bare return in nonvoid fn" {
    try runCaseError("sema/bare-return-in-nonvoid-fn.dsk", error.InvalidReturnType);
}

test "sema error: invalid struct function" {
    try runCaseError("sema/invalid-struct-function.dsk", error.InvalidStructField);
}

test "sema error: invalid static struct field" {
    try runCaseError("sema/invalid-static-struct-field.dsk", error.InvalidStructField);
}

test "sema error: cant infer array literal type" {
    try runCaseError("sema/cant-infer-array-literal-type.dsk", error.CantInferArrayLiteralType);
}

test "sema error: invalid parameter type" {
    try runCaseError("sema/invalid-parameter-type.dsk", error.InvalidParameterType);
}

test "sema error: primitive params cant be mutable" {
    try runCaseError("sema/primitive-params-cant-be-mutable.dsk", error.PrimitiveParamsCantBeMutable);
}

test "sema error: unwrapped cant be mutable" {
    try runCaseError("sema/unwrapped-cant-be-mutable.dsk", error.UnwrappedCantBeMutable);
}

test "sema error: missing argument" {
    try runCaseError("sema/missing-argument.dsk", error.MissingArgument);
}

test "sema error: invalid indexing" {
    try runCaseError("sema/invalid-indexing.dsk", error.InvalidIndexing);
}

test "sema error: unknown type" {
    try runCaseError("sema/unknown-type.dsk", error.UnknownType);
}

test "sema error: if condition not bool" {
    try runCaseError("sema/if-condition-not-bool.dsk", error.InvalidType);
}

test "sema error: for condition not bool" {
    try runCaseError("sema/for-condition-not-bool.dsk", error.InvalidType);
}

test "sema error: if capture on nonnullable" {
    try runCaseError("sema/if-capture-on-nonnullable.dsk", error.InvalidType);
}

test "sema error: assign type mismatch" {
    try runCaseError("sema/assign-type-mismatch.dsk", error.InvalidType);
}

test "sema error: array assign type mismatch" {
    try runCaseError("sema/array-assign-type-mismatch.dsk", error.InvalidType);
}

test "sema error: struct field assign type mismatch" {
    try runCaseError("sema/struct-field-assign-type-mismatch.dsk", error.InvalidType);
}

test "sema error: static field assign type mismatch" {
    try runCaseError("sema/static-field-assign-type-mismatch.dsk", error.InvalidType);
}

test "sema error: call non function" {
    try runCaseError("sema/call-non-function.dsk", error.InvalidType);
}

test "sema error: method call on nonstruct" {
    try runCaseError("sema/method-call-on-nonstruct.dsk", error.InvalidType);
}

test "sema error: fn arg type mismatch" {
    try runCaseError("sema/fn-arg-type-mismatch.dsk", error.InvalidType);
}

test "sema error: binary op type mismatch" {
    try runCaseError("sema/binary-op-type-mismatch.dsk", error.InvalidType);
}

test "sema error: binary bool type mismatch" {
    try runCaseError("sema/binary-bool-type-mismatch.dsk", error.InvalidType);
}

test "sema error: unary op type mismatch" {
    try runCaseError("sema/unary-op-type-mismatch.dsk", error.InvalidType);
}

test "sema error: index into non array" {
    try runCaseError("sema/index-into-non-array.dsk", error.InvalidType);
}

test "sema error: struct field default type mismatch" {
    try runCaseError("sema/struct-field-default-type-mismatch.dsk", error.InvalidType);
}

test "sema error: param default type mismatch" {
    try runCaseError("sema/param-default-type-mismatch.dsk", error.InvalidType);
}

test "sema error: indexed assign notmut root" {
    try runCaseError("sema/indexed-assign-notmut-root.dsk", error.NotMutable);
}

test "sema error: static field not mutable" {
    try runCaseError("sema/static-field-not-mutable.dsk", error.NotMutable);
}

test "sema error: self binding not mut" {
    try runCaseError("sema/self-binding-not-mut.dsk", error.NotMutable);
}

test "sema error: assign undefined" {
    try runCaseError("sema/assign-undefined.dsk", error.NotDefined);
}

test "sema error: indexed assign undefined" {
    try runCaseError("sema/indexed-assign-undefined.dsk", error.NotDefined);
}

test "sema error: call undefined" {
    try runCaseError("sema/call-undefined.dsk", error.NotDefined);
}

test "sema error: duplicate fn def" {
    try runCaseError("sema/duplicate-fn-def.dsk", error.AlreadyDefined);
}

test "sema error: duplicate param name" {
    try runCaseError("sema/duplicate-param-name.dsk", error.AlreadyDefined);
}

test "sema error: unknown enum variant" {
    try runCaseError("sema/enum-unknown-variant.dsk", error.InvalidStructField);
}

test "sema error: assign to enum variant" {
    try runCaseError("sema/enum-assign-variant.dsk", error.NotMutable);
}

test "sema error: call enum as function" {
    try runCaseError("sema/enum-call-as-function.dsk", error.InvalidType);
}

test "sema error: enum type coercion" {
    try runCaseError("sema/enum-type-coercion.dsk", error.InvalidType);
}

test "sema error: mutable struct definition" {
    try runCaseError("sema/mut-struct-definition.dsk", error.InvalidDefinition);
}

test "sema error: mutable enum definition" {
    try runCaseError("sema/mut-enum-definition.dsk", error.InvalidDefinition);
}

test "syntax error: break outside loop" {
    try runCaseError("parser/break-outside-loop.dsk", error.ParserError);
}

test "syntax error: continue outside loop" {
    try runCaseError("parser/continue-outside-loop.dsk", error.ParserError);
}

test "syntax error: invalid statement" {
    try runCaseError("parser/invalid-statement.dsk", error.ParserError);
}

test "syntax error: struct static after field" {
    try runCaseError("parser/struct-static-after-field.dsk", error.ParserError);
}

test "syntax error: struct field after method" {
    try runCaseError("parser/struct-field-after-method.dsk", error.ParserError);
}

test "syntax error: struct unexpected token" {
    try runCaseError("parser/struct-unexpected-token.dsk", error.ParserError);
}

test "syntax error: struct missing colon fn" {
    try runCaseError("parser/struct-missing-colon-fn.dsk", error.ParserError);
}

test "syntax error: struct field default missing colon" {
    try runCaseError("parser/struct-field-default-missing-colon.dsk", error.ParserError);
}

test "syntax error: enum mixed explicit and implicit" {
    try runCaseError("parser/enum-mixed-mode.dsk", error.ParserError);
}

test "syntax error: enum mixed implicit and explicit" {
    try runCaseError("parser/enum-mixed-mode-2.dsk", error.ParserError);
}

test "syntax error: enum unexpected token" {
    try runCaseError("parser/enum-unexpected-token.dsk", error.ParserError);
}

test "syntax error: fn args mixed named positional" {
    try runCaseError("parser/fn-args-mixed-named.dsk", error.ParserError);
}

test "syntax error: fn args non identifier name" {
    try runCaseError("parser/fn-args-non-identifier.dsk", error.ParserError);
}

test "syntax error: pipe non fn call" {
    try runCaseError("parser/pipe-non-fn-call.dsk", error.ParserError);
}

test "syntax error: unknown type annotation" {
    try runCaseError("parser/type-annotation-unknown.dsk", error.ParserError);
}

test "gc: basic" {
    try runCase("gc-basic.dsk");
}

test "gc: nested structures" {
    try runCase("gc-nested.dsk");
}

test "gc: across function calls" {
    try runCase("gc-functions.dsk");
}

test "gc: array resize" {
    try runCase("gc-resize.dsk");
}

test "gc: stress cycles" {
    try runCase("gc-stress-cycles.dsk");
}

test "gc: stress mixed types" {
    try runCase("gc-stress-mixed.dsk");
}

test "gc: stress resize" {
    try runCase("gc-stress-resize.dsk");
}

test "gc: stress complex flow" {
    try runCase("gc-stress-complex-flow.dsk");
}

test "gc: static store survival" {
    try runCase("gc-static-store.dsk");
}

test "register: stress" {
    try runCase("register-stress.dsk");
}

test "recursion: deep frame stack" {
    try runCase("deep-recursion.dsk");
}
