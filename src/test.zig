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

fn runCaseRuntimeError(src_file: []const u8, expected: anytype) !void {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const cwd = std.Io.Dir.cwd();
    const file_path = try std.fmt.allocPrint(arena.allocator(), "test/runtime/{s}", .{src_file});
    const src = try cwd.readFileAlloc(testing.io, file_path, arena.allocator(), .unlimited);

    var dusk = Dusk{ .allocator = arena.allocator(), .stdout_writer = null, .io = testing.io };
    try testing.expectError(expected, dusk.compileAndRun(src));
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

test "enums" {
    try runCase("enums.dsk");
}

test "union" {
    try runCase("union.dsk");
}

test "union with the max number of variants" {
    try runCase("union-max-variants.dsk");
}

test "nullable echo" {
    try runCase("nullable-echo.dsk");
}

test "optional chaining" {
    try runCase("optional-chaining.dsk");
}

test "union anonymous syntax" {
    try runCase("union-anonymous.dsk");
}

test "when" {
    try runCase("when.dsk");
}

test "returns inside branches" {
    try runCase("return-flow.dsk");
}

test "call result assigned to an older variable" {
    try runCase("call-assign.dsk");
}

test "nullable primitive args and returns" {
    try runCase("nullable-primitive-args.dsk");
}

test "empty array literal" {
    try runCase("empty-array-literal.dsk");
}

test "struct" {
    try runCase("struct.dsk");
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

test "sema error: return only in if" {
    try runCaseError("sema/return-only-in-if.dsk", error.InvalidReturnType);
}

test "sema error: return missing in elif" {
    try runCaseError("sema/return-missing-in-elif.dsk", error.InvalidReturnType);
}

test "sema error: return missing in when case" {
    try runCaseError("sema/return-missing-in-when-case.dsk", error.InvalidReturnType);
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

test "sema error: unknown union variant" {
    try runCaseError("sema/union-unknown-variant.dsk", error.InvalidUnionVariant);
}

test "sema error: unknown union member" {
    try runCaseError("sema/union-unknown-member.dsk", error.InvalidUnionMember);
}

test "sema error: unknown union method" {
    try runCaseError("sema/union-unknown-method.dsk", error.InvalidUnionFunction);
}

test "sema error: union payload type mismatch" {
    try runCaseError("sema/union-payload-type-mismatch.dsk", error.InvalidType);
}

test "sema error: union init with positional argument" {
    try runCaseError("sema/union-init-positional.dsk", error.InvalidType);
}

test "sema error: union init with multiple arguments" {
    try runCaseError("sema/union-init-multiple-args.dsk", error.InvalidType);
}

test "sema error: assign to union variant" {
    try runCaseError("sema/union-assign-variant.dsk", error.NotMutable);
}

test "sema error: union static field not mutable" {
    try runCaseError("sema/union-static-field-not-mutable.dsk", error.NotMutable);
}

test "sema error: union static field type mismatch" {
    try runCaseError("sema/union-static-field-type-mismatch.dsk", error.InvalidType);
}

test "sema error: union with too many variants" {
    try runCaseError("sema/union-too-many-variants.dsk", error.TooManyUnionVariants);
}

test "sema error: mutable union param" {
    try runCaseError("sema/union-mut-param.dsk", error.UnionParamsCantBeMutable);
}

test "sema error: anonymous union unknown variant" {
    try runCaseError("sema/union-anonymous-unknown-variant.dsk", error.InvalidUnionVariant);
}

test "sema error: when not exhaustive enum" {
    try runCaseError("sema/when-not-exhaustive-enum.dsk", error.WhenNotExhaustive);
}

test "sema error: when not exhaustive union" {
    try runCaseError("sema/when-not-exhaustive-union.dsk", error.WhenNotExhaustive);
}

test "sema error: when int needs else" {
    try runCaseError("sema/when-int-needs-else.dsk", error.WhenNotExhaustive);
}

test "sema error: when nullable missing null" {
    try runCaseError("sema/when-nullable-missing-null.dsk", error.WhenNotExhaustive);
}

test "sema error: when useless else" {
    try runCaseError("sema/when-useless-else.dsk", error.WhenUselessElse);
}

test "sema error: when duplicate case" {
    try runCaseError("sema/when-duplicate-case.dsk", error.WhenDuplicateCase);
}

test "sema error: when duplicate variant" {
    try runCaseError("sema/when-duplicate-variant.dsk", error.WhenDuplicateCase);
}

test "sema error: when unknown variant" {
    try runCaseError("sema/when-unknown-variant.dsk", error.InvalidUnionVariant);
}

test "sema error: when case type mismatch" {
    try runCaseError("sema/when-case-type-mismatch.dsk", error.InvalidType);
}

test "sema error: when unsupported subject" {
    try runCaseError("sema/when-unsupported-subject.dsk", error.InvalidType);
}

test "sema error: when float subject" {
    try runCaseError("sema/when-float-subject.dsk", error.InvalidType);
}

test "sema error: when union case without capture" {
    try runCaseError("sema/when-union-case-without-capture.dsk", error.WhenInvalidCase);
}

test "sema error: when variant case on enum" {
    try runCaseError("sema/when-variant-case-on-enum.dsk", error.WhenInvalidCase);
}

test "sema error: when null case on non-nullable" {
    try runCaseError("sema/when-null-case-non-nullable.dsk", error.WhenInvalidCase);
}

test "sema error: when non-literal int case" {
    try runCaseError("sema/when-non-literal-int-case.dsk", error.WhenInvalidCase);
}

test "parser error: when case missing end" {
    try runCaseError("parser/when-case-missing-end.dsk", error.ParserError);
}

test "parser error: when case missing then" {
    try runCaseError("parser/when-case-missing-then.dsk", error.ParserError);
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


test "syntax error: struct unexpected token" {
    try runCaseError("parser/struct-unexpected-token.dsk", error.ParserError);
}

test "semantic error: struct field default ident not defined" {
    try runCaseError("parser/struct-field-default-missing-colon.dsk", error.NotDefined);
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

test "syntax error: struct missing end" {
    try runCaseError("parser/struct-missing-end.dsk", error.ParserError);
}

test "syntax error: enum missing end" {
    try runCaseError("parser/enum-missing-end.dsk", error.ParserError);
}

test "syntax error: union missing end" {
    try runCaseError("parser/union-missing-end.dsk", error.ParserError);
}

test "syntax error: anonymous struct missing end" {
    try runCaseError("parser/anon-struct-missing-end.dsk", error.ParserError);
}

test "syntax error: fn args mixed named positional" {
    try runCaseError("parser/fn-args-mixed-named.dsk", error.ParserError);
}

test "syntax error: fn args non identifier name" {
    try runCaseError("parser/fn-args-non-identifier.dsk", error.ParserError);
}

test "syntax error: anonymous init missing paren" {
    try runCaseError("parser/anon-init-missing-paren.dsk", error.ParserError);
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

test "gc: unions in arrays and structs" {
    try runCase("union-gc.dsk");
}

test "gc: nullables in arrays and structs" {
    try runCase("gc-nullable.dsk");
}

test "errors: try" {
    try runCase("errors-try.dsk");
}

test "errors: catch" {
    try runCase("errors-catch.dsk");
}

test "errors: inferred sets" {
    try runCase("errors-inferred.dsk");
}

test "errors: values" {
    try runCase("errors-values.dsk");
}

test "gc: errors" {
    try runCase("errors-gc.dsk");
}

test "sema error: unhandled call" {
    try runCaseError("sema/errors-unhandled-call.dsk", error.UnhandledError);
}

test "sema error: unhandled call statement" {
    try runCaseError("sema/errors-unhandled-call-stmt.dsk", error.UnhandledError);
}

test "sema error: unhandled call argument" {
    try runCaseError("sema/errors-unhandled-call-arg.dsk", error.UnhandledError);
}

test "sema error: try in non-failing function" {
    try runCaseError("sema/errors-try-in-non-failing-fn.dsk", error.TryOutsideFailingFunction);
}

test "sema error: try in inline return" {
    try runCaseError("sema/errors-try-inline-return.dsk", error.TryOutsideFailingFunction);
}

test "sema error: try on non-failing call" {
    try runCaseError("sema/errors-try-non-failing-call.dsk", error.NotAFailingCall);
}

test "sema error: catch on non-failing call" {
    try runCaseError("sema/errors-catch-non-failing-call.dsk", error.NotAFailingCall);
}

test "sema error: try on non-call" {
    try runCaseError("sema/errors-try-non-call.dsk", error.NotAFailingCall);
}

test "sema error: error not in set (return)" {
    try runCaseError("sema/errors-not-in-set-return.dsk", error.ErrorNotInSet);
}

test "sema error: error not in set (try)" {
    try runCaseError("sema/errors-not-in-set-try.dsk", error.ErrorNotInSet);
}

test "sema error: error not in set (inferred chain)" {
    try runCaseError("sema/errors-not-in-set-inferred-chain.dsk", error.ErrorNotInSet);
}

test "sema error: error not in set (captured)" {
    try runCaseError("sema/errors-not-in-set-captured.dsk", error.ErrorNotInSet);
}

test "sema error: catch block falls through" {
    try runCaseError("sema/errors-catch-block-falls-through.dsk", error.CatchBlockMustLeave);
}

test "sema error: catch default type mismatch" {
    try runCaseError("sema/errors-catch-default-type-mismatch.dsk", error.InvalidType);
}

test "sema error: duplicate error variant" {
    try runCaseError("sema/errors-duplicate-variant.dsk", error.AlreadyDefined);
}

test "sema error: failing return wrong type" {
    try runCaseError("sema/errors-return-wrong-type.dsk", error.InvalidReturnType);
}

test "sema error: not an error set" {
    try runCaseError("sema/errors-not-an-error-set.dsk", error.NotAnErrorSet);
}

test "sema error: inferred error passed where it does not fit" {
    try runCaseError("sema/errors-inferred-set-arg-not-in-set.dsk", error.ErrorNotInSet);
}

test "parser error: error set missing end" {
    try runCaseError("parser/errors-set-missing-end.dsk", error.ParserError);
}

test "parser error: error variant with payload" {
    try runCaseError("parser/errors-set-variant-payload.dsk", error.ParserError);
}

test "parser error: error union type in let" {
    try runCaseError("parser/errors-union-type-in-let.dsk", error.ParserError);
}

test "parser error: catch block missing end" {
    try runCaseError("parser/errors-catch-block-missing-end.dsk", error.ParserError);
}

test "runtime error: top-level try" {
    try runCaseRuntimeError("errors-top-level-try.dsk", error.UnhandledError);
}

test "errors: when on caught errors" {
    try runCase("errors-when.dsk");
}

test "sema error: when on error not exhaustive" {
    try runCaseError("sema/errors-when-not-exhaustive.dsk", error.WhenNotExhaustive);
}

test "sema error: when on inferred error not exhaustive" {
    try runCaseError("sema/errors-when-not-exhaustive-inferred.dsk", error.WhenNotExhaustive);
}

test "sema error: when error case not in set" {
    try runCaseError("sema/errors-when-error-not-in-set.dsk", error.ErrorNotInSet);
}

test "sema error: when error case not in inferred set" {
    try runCaseError("sema/errors-when-error-not-in-set-inferred.dsk", error.ErrorNotInSet);
}

test "sema error: when on inferred error useless else" {
    try runCaseError("sema/errors-when-useless-else-inferred.dsk", error.WhenUselessElse);
}

test "sema error: when on failing call" {
    try runCaseError("sema/errors-when-failing-call-subject.dsk", error.UnhandledError);
}

test "sema error: when duplicate error case" {
    try runCaseError("sema/errors-when-duplicate-case.dsk", error.WhenDuplicateCase);
}
