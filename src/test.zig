const std = @import("std");
const testing = std.testing;
const Dusk = @import("dusk.zig").Dusk;
const Lexer = @import("lexer.zig").Lexer;

fn runCaseOutput(src_file: []const u8, expected: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var output: std.Io.Writer.Allocating = .init(testing.allocator);
    defer output.deinit();
    const path = try std.fmt.allocPrint(arena.allocator(), "test/success/{s}", .{src_file});
    const source = try std.Io.Dir.cwd().readFileAlloc(testing.io, path, arena.allocator(), .unlimited);
    var dusk = Dusk{ .allocator = arena.allocator(), .stdout_writer = &output.writer, .io = testing.io };
    try dusk.compileAndRun(source);
    try testing.expectEqualStrings(expected, output.written());
}

test "string interpolation" {
    try runCaseOutput("string-interpolation.dsk", "Hello, Dusk! You are 13 years old.\n" ++
        "Sum: 5\nDusk135\nDusk.length 20 yes\nNested: Dusk 5\n" ++
        "Escaped: $name ${a + b} $name $ $5\n" ++
        "Backslash: \\Dusk; Unicode: 🌙 🌙\n#= } =#\n" ++
        "Controls: a\x00b\n\t\nold!\n1 2 3\n[1, 2] 7 [9, 2]\nHello, World!\ndefault\n" ++
        "true 1.5 null 7 1.5 true text null\n3 9 [1, 2] { 9 }\n<union>\n");
}

test "string interpolation in batches" {
    try runCaseOutput("string-interpolation-large.dsk", "0=0 1=1 2=2 3=3 4=4 5=5 6=6 7=7 8=8 9=9 10=10 11=11 12=12 13=13 14=14 15=15 16=16 17=17 18=18 19=19 20=20 21=21 22=22 23=23 24=24 25=25 26=26 27=27 28=28 29=29 30=30 31=31 32=32 33=33 34=34 35=35 36=36 37=37 38=38 39=39\ntrue 1.5 [1, 2] 5 null done\n");
    try runCaseOutput("string-interpolation-build-gc.dsk", "[item 5000: [5000, 5001] 5000 nested 5000 1.5 true, item 10000: [10000, 10001] 10000 nested 10000 1.5 true, " ++
        "item 15000: [15000, 15001] 15000 nested 15000 1.5 true, item 20000: [20000, 20001] 20000 nested 20000 1.5 true]\n1034474\n");
}

test "string interpolation compiles to one STR_BUILD" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var dusk = Dusk{ .allocator = arena.allocator(), .stdout_writer = null, .io = testing.io };
    const program = try dusk.compile("func main() -> void let a = 1 let b = [2] let s = \"a=$a b=$b\" end");
    const chunk = for (program.functions) |func| {
        if (std.mem.eql(u8, func.name, "main")) break func.kind.dusk;
    } else return error.MainNotFound;
    var builds: usize = 0;
    var snapshots: usize = 0;
    for (chunk.instructions) |inst| {
        if (inst.op == .STR_BUILD) builds += 1;
        if (inst.op == .TO_STRING) snapshots += 1;
    }
    try testing.expectEqual(@as(usize, 1), builds);
    try testing.expectEqual(@as(usize, 1), snapshots);
    try testing.expectEqual(@as(usize, 1), chunk.build_recipes.len);
    try testing.expectEqual(@as(usize, 4), chunk.build_recipes[0].len);
}

fn countOps(source: []const u8, function: []const u8, op: @import("bytecode.zig").OpCode) !usize {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var dusk = Dusk{ .allocator = arena.allocator(), .stdout_writer = null, .io = testing.io };
    const program = try dusk.compile(source);
    const chunk = for (program.functions) |func| {
        if (std.mem.eql(u8, func.name, function)) break func.kind.dusk;
    } else return error.FunctionNotFound;
    var count: usize = 0;
    for (chunk.instructions) |inst| {
        if (inst.op == op) count += 1;
    }
    return count;
}

test "when: dense cases compile to a jump table, sparse ones to compares" {
    try runCaseOutput("when-jump-tables.dsk", "[10, 4, 21, 2, 1]\n[6, 9, 9, 0]\n[A, B, C, F, ?, ?]\n[0, 3, -2]\n[10, 40, 99, 99]\n[-1, 1, 2]\n[1, 2, 3, 4, 5]\n[110, 113, 199, 2, 4, 9]\n");
    const dense = "func f(n: int) -> int let mut out = 0 when n 1 then out = 1 end 2 then out = 2 end 3 then out = 3 end 4 then out = 4 end else then out = 5 end end return out end func main() -> void end";
    const sparse = "func f(n: int) -> int let mut out = 0 when n 1 then out = 1 end 100 then out = 2 end 3 then out = 3 end 4 then out = 4 end else then out = 5 end end return out end func main() -> void end";
    const few = "func f(n: int) -> int let mut out = 0 when n 1 then out = 1 end 2 then out = 2 end else then out = 5 end end return out end func main() -> void end";
    try testing.expectEqual(@as(usize, 1), try countOps(dense, "f", .JUMP_TABLE));
    try testing.expectEqual(@as(usize, 0), try countOps(sparse, "f", .JUMP_TABLE));
    try testing.expectEqual(@as(usize, 0), try countOps(few, "f", .JUMP_TABLE));
}

test "when: a nullable subject without a null case lowers else once" {
    const source = "func f(n: ?int) -> void when n 1 then end 2 then end else then echo(7) end end end func main() -> void end";
    try testing.expectEqual(@as(usize, 1), try countOps(source, "f", .CALL));
}

test "arrays print their items after growing, and a one-item array grows" {
    try runCaseOutput("array-growth-printing.dsk", "[1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9]\n[1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9]\n[a, b, c]\n");
}

test "tail calls: return try f(x) reuses the frame when the success types match" {
    try runCase("tail-calls-failing.dsk");
    const same = "error E\n Bad\nend\nfunc f(n: int) -> E!int\n if n == 0 then return E.Bad end\n return try f(n - 1)\nend\nfunc main() -> void\nend";
    const boxed = "error E\n Bad\nend\nfunc g(n: int) -> E!int\n return n\nend\nfunc f(n: int) -> E!?int\n return try g(n)\nend\nfunc main() -> void\nend";
    try testing.expectEqual(@as(usize, 1), try countOps(same, "f", .TAIL_CALL));
    try testing.expectEqual(@as(usize, 0), try countOps(boxed, "f", .TAIL_CALL));
}

test "catch: a null fallback makes the result nullable when a nullable is expected" {
    try runCase("catch-nullable-fallback.dsk");
    try runCaseError("sema/catch-null-without-nullable.dsk", error.InvalidType);
}

test "string interpolation GC" {
    try runCaseOutput("string-interpolation-gc.dsk", "value=12000 / nested value=12001 / [first, second] / 42\n[first, second] 42\n");
}

test "string interpolation control flow" {
    try runCaseOutput("string-interpolation-control-flow.dsk", "number=5\nfallback\ncaught=8\nearly\nbefore 4 after\n1\n3\n4\nother\n");
}

test "string interpolation ignores braces in comments" {
    const sources = [_][]const u8{
        "func main() -> void\nlet value = \"${1 #= } #= nested =# =# + 2}\"\nend",
        "func main() -> void\nlet value = \"${1 + 2 # ignored }\n}\"\nend",
    };
    for (sources) |source| {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var dusk = Dusk{ .allocator = arena.allocator(), .stdout_writer = null, .io = testing.io };
        try dusk.compileAndRun(source);
    }
}

test "string interpolation empty" {
    try runCaseError("parser/string-interpolation-empty.dsk", error.ParserError);
}

test "string interpolation extra expression" {
    try runCaseError("parser/string-interpolation-extra-expression.dsk", error.ParserError);
}

test "string interpolation unclosed" {
    try runCaseError("parser/string-interpolation-unclosed.dsk", error.LexerError);
}

test "string interpolation unknown name" {
    try runCaseError("sema/string-interpolation-unknown-name.dsk", error.NotDefined);
}

test "string interpolation void" {
    try runCaseError("sema/string-interpolation-void.dsk", error.InvalidExpression);
}

test "string interpolation unhandled error" {
    try runCaseError("sema/string-interpolation-unhandled-error.dsk", error.UnhandledError);
}

test "multiline comments preserve tokens and locations" {
    const src = "#= header\n#= nested =#\n=#let#==#x = 1#= inline =#+2#= tail =#";
    var lexer = Lexer.init(testing.allocator, src);
    var tokens = try lexer.list();
    defer tokens.deinit(testing.allocator);
    const Tag = @import("lexer.zig").Tag;
    const expected = [_]Tag{ .let_kw, .identifier, .eq, .int_literal, .plus, .int_literal, .eof };
    try testing.expectEqual(expected.len, tokens.items.len);
    for (expected, tokens.items) |tag, token| {
        try testing.expectEqual(tag, token.tag);
    }
    try testing.expectEqualStrings("let", tokens.items[0].value(src));
    try testing.expectEqual(std.mem.indexOf(u8, src, "let").?, tokens.items[0].loc.start);
    try testing.expectEqual(src.len, tokens.items[6].loc.start);
}

test "comment delimiters inside strings and line comments" {
    const src = "\"#= text =#\" # ignored #=\n42 # EOF";
    var lexer = Lexer.init(testing.allocator, src);
    var tokens = try lexer.list();
    defer tokens.deinit(testing.allocator);
    try testing.expectEqual(@as(usize, 3), tokens.items.len);
    try testing.expectEqualStrings("#= text =#", tokens.items[0].value(src));
    try testing.expectEqualStrings("42", tokens.items[1].value(src));
}

test "comments and whitespace at EOF" {
    const sources = [_][]const u8{ "# line comment", "#", "   ", "#==#", "#=\n=#", "#= #= nested =# =#   " };
    for (sources) |src| {
        var lexer = Lexer.init(testing.allocator, src);
        var tokens = try lexer.list();
        defer tokens.deinit(testing.allocator);
        try testing.expectEqual(@as(usize, 1), tokens.items.len);
        try testing.expectEqual(@import("lexer.zig").Tag.eof, tokens.items[0].tag);
    }
}

test "unterminated multiline comments" {
    const sources = [_][]const u8{ "#=", "#= text", "#= #= nested =#", "#= =", "#= #" };
    for (sources) |src| {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var lexer = Lexer.init(arena.allocator(), src);
        try testing.expectError(error.LexerError, lexer.list());
    }
}

test "string escapes decode into UTF-8 literals" {
    const cases = [_]struct { source: []const u8, expected: []const u8 }{
        .{ .source = "\"\"", .expected = "" },
        .{ .source = "\"Olá 🌙\"", .expected = "Olá 🌙" },
        .{ .source = "\"a\\nb\\rc\\td\\0e\"", .expected = "a\nb\rc\td\x00e" },
        .{ .source = "\"say \\\"hi\\\"\\\\\"", .expected = "say \"hi\"\\" },
        .{ .source = "\"\\\\n\"", .expected = "\\n" },
        .{ .source = "\"\\u{0}\\u{7f}\\u{80}\\u{7ff}\\u{800}\"", .expected = "\x00\x7f\u{80}\u{7ff}\u{800}" },
        .{ .source = "\"\\u{D7FF}\\u{e000}\\u{ffff}\\u{10000}\\u{10FFFF}\"", .expected = "\u{d7ff}\u{e000}\u{ffff}\u{10000}\u{10ffff}" },
        .{ .source = "\"\\u{000041}\\u{1F319}\"", .expected = "A🌙" },
        .{ .source = "\"\\u{22}#= text =#\\u{5c}\"", .expected = "\"#= text =#\\" },
    };
    for (cases) |case| {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const source = try std.fmt.allocPrint(arena.allocator(), "let text = {s}", .{case.source});
        var lexer = Lexer.init(arena.allocator(), source);
        const tokens = try lexer.list();
        var parser = @import("parser.zig").Parser.init(arena.allocator(), source, tokens.items);
        const root = try parser.parse();
        const decoded = root.statements.items[0].data.let.value.data.string_literal;
        try testing.expectEqualStrings(case.expected, decoded);
        try testing.expect(std.unicode.utf8ValidateSlice(decoded));
    }
}

test "multiline strings strip the closing indentation" {
    const cases = [_]struct { source: []const u8, expected: []const u8 }{
        .{ .source = "\"\"\"\n\"\"\"", .expected = "" },
        .{ .source = "\"\"\"   \n  a\n  \"\"\"", .expected = "a" },
        .{ .source = "\"\"\"\n    a\n      b\n    \"\"\"", .expected = "a\n  b" },
        .{ .source = "\"\"\"\n        a\n        b\n    \"\"\"", .expected = "    a\n    b" },
        .{ .source = "\"\"\"\n  a\n\n \n      \n  b\n  \"\"\"", .expected = "a\n\n\n\nb" },
        .{ .source = "\"\"\"\n  a\n\n  \"\"\"", .expected = "a\n" },
        .{ .source = "\"\"\"\n\n  a\n  \"\"\"", .expected = "\na" },
        .{ .source = "\"\"\"\na\n\"\"\"", .expected = "a" },
        .{ .source = "\"\"\"\n  \"a\" \"\"b\"\" \\\"\"\"\n  \"\"\"", .expected = "\"a\" \"\"b\"\" \"\"\"" },
        .{ .source = "\"\"\"\n  \\t\\u{1F319} #= =# $\n  \"\"\"", .expected = "\t🌙 #= =# $" },
    };
    for (cases) |case| {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const source = try std.fmt.allocPrint(arena.allocator(), "let text = {s}", .{case.source});
        var lexer = Lexer.init(arena.allocator(), source);
        const tokens = try lexer.list();
        var parser = @import("parser.zig").Parser.init(arena.allocator(), source, tokens.items);
        const root = try parser.parse();
        try testing.expectEqualStrings(case.expected, root.statements.items[0].data.let.value.data.string_literal);
    }
}

test "multiline strings" {
    try runCaseOutput("multiline-strings.dsk", "Usage: dusk <file>\n  -h  show 2 \"help\"\n\nquote: \"\"\" done\n" ++
        "[]\n[a\n]\n[other tail]\n[    kept\n    indent]\n");
}

test "string constants outlive compilation and source storage" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const source = try testing.allocator.dupe(u8, "func main() -> void let mut text = \"a\\n\\u{1F319}\\0\" let mut same = \"a\\n🌙\\0\" let mut plain = \"plain\" end");
    var dusk = Dusk{ .allocator = arena.allocator(), .stdout_writer = null, .io = testing.io };
    const program = dusk.compile(source) catch |failure| {
        testing.allocator.free(source);
        return failure;
    };
    testing.allocator.free(source);
    const pool = program.string_pool;
    try testing.expectEqual(@as(usize, 2), pool.values.len);
    try testing.expectEqualStrings("a\n🌙\x00", pool.textAt(0));
    try testing.expectEqualStrings("plain", pool.textAt(1));
    var vm = @import("vm.zig").VM.init(&program, null);
    defer vm.deinit();
    try vm.run();
}

test "str_equal" {
    try runCaseOutput("str-equal.dsk", "true\nfalse\n");
    try runCaseError("sema/str-equal-nullable.dsk", error.InvalidType);
    try runCaseError("sema/str-equal-int.dsk", error.InvalidType);
}

test "string length and char count" {
    try runCaseOutput("string-length.dsk", "4\n3\n");
    try runCaseError("sema/len-nullable-string.dsk", error.InvalidType);
    try runCaseError("sema/len-string.dsk", error.InvalidType);
    try runCaseError("sema/len-buffer.dsk", error.InvalidType);
    try runCaseError("sema/str-char-count-int.dsk", error.InvalidType);
}

test "string concat and join" {
    try runCaseOutput("string-concat-join.dsk", "Hello, Dusk\na, b, c\nonly\n[]\n--\nxy\ngrow the array\nabc\nxy\np+q\n");
    try runCaseOutput("string-concat-join-gc.dsk", "item-5000-end! | item-10000-end! | item-15000-end! | item-20000-end!\n288894\n");
    try runCaseError("sema/str-concat-int.dsk", error.InvalidType);
    try runCaseError("sema/str-join-int-array.dsk", error.InvalidType);
    try runCaseError("sema/str-join-int-separator.dsk", error.InvalidType);
}

test "string conversion" {
    try runCaseOutput("string-conversion.dsk", "42\n-9223372036854775808\n3\n-0.25\n-1\n" ++
        "'1' -> ok 1\n'' -> invalid\n'-' -> invalid\n'+1' -> invalid\n' 1' -> invalid\n'1 ' -> invalid\n" ++
        "'1_000' -> invalid\n'0x10' -> invalid\n'1.5' -> invalid\n'9223372036854775808' -> overflow\n" ++
        "'1.' -> -1\n'.5' -> -1\n'1e3' -> -1\n'inf' -> -1\n'nan' -> -1\n'2.50' -> 2.5\n");
    try runCaseRuntimeError("str-to-int-unhandled.dsk", error.UnhandledError);
    try runCaseError("sema/str-to-int-unhandled.dsk", error.UnhandledError);
    try runCaseError("sema/str-to-int-int.dsk", error.InvalidType);
    try runCaseError("sema/conversion-error-redefined.dsk", error.AlreadyDefined);
}

test "string indexing and loops" {
    try runCaseOutput("string-index.dsk", "out of range\nout of range\n108\nout of range\nout of range\n\u{E1}\nout of range\n" ++
        "0: 111\n1: 108\n2: 195\n3: 161\n4: 33\n");
    try runCaseError("sema/string-index-read.dsk", error.InvalidIndexing);
    try runCaseError("sema/string-index-assign.dsk", error.InvalidIndexing);
    try runCaseError("sema/str-byte-at-unhandled.dsk", error.UnhandledError);
}

test "string search" {
    try runCaseOutput("string-search.dsk", "2\n0\n6\nnull\n");
    try runCaseError("sema/string-error-redefined.dsk", error.AlreadyDefined);
}

test "string transforms" {
    try runCaseOutput("string-transform.dsk", "[hello world]\n[]\n[none]\n[x]\n[\u{A0}x\u{A0}]\n[a, b, , c]\n[abc]\n1\n[a, b, c]\n[ol\u{E1}, mundo]\n" ++
        "a+b+c\nbbbbbb\nheo\nabc\nHELLO, A\u{C7}\u{C3}O! \u{DF} \u{FF} \u{D7} \u{F7} \u{3B1}\nhello, a\u{E7}\u{E3}o! \u{17D} \u{3A9}\n" ++
        "ababab\n[]\n[]\n[o, l, \u{E1}]\n0\n[a, b]\nabc\nnegative count\nabab\n[a,  b , c]\n");
    try runCaseOutput("string-transform-gc.dsk", "[KEY5000v5000,  extra, KEY10000v10000,  extra, KEY15000v15000,  extra, KEY20000v20000,  extra]\n317788\n");
    try runCaseError("sema/str-trim-int.dsk", error.InvalidType);
    try runCaseError("sema/str-repeat-string-count.dsk", error.InvalidType);
    try runCaseRuntimeError("str-repeat-unhandled.dsk", error.UnhandledError);
}

test "string slice" {
    try runCaseOutput("string-slice.dsk", "ol\nol\u{E1}\nmundo\n[]\n[]\nol\u{E1}\n" ++
        "out of range\nout of range\nout of range\nout of range\nout of range\n\u{E1}\n");
    try runCaseError("sema/str-slice-unhandled.dsk", error.UnhandledError);
    try runCaseRuntimeError("str-slice-unhandled.dsk", error.UnhandledError);
}

test "equality: only same types (or numbers) compare, strings and unions can't" {
    try runCase("equality-rules.dsk");
    try runCaseError("sema/compare-different-structs.dsk", error.CantCompare);
    try runCaseError("sema/compare-bool-int.dsk", error.CantCompare);
    try runCaseError("sema/compare-int-string.dsk", error.CantCompare);
    try runCaseError("sema/compare-strings.dsk", error.CantCompare);
    try runCaseError("sema/compare-unions.dsk", error.CantCompare);
    try runCaseError("sema/compare-not-nullable-with-null.dsk", error.CantCompare);
}

test "string literal pool" {
    try runCaseOutput("string-literal-pool.dsk", "hello\nfield text\nstatic text\n[kept, kept, kept]\n");
}

test "more than 65536 distinct string literals are rejected" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var source: std.ArrayList(u8) = .empty;
    try source.appendSlice(arena.allocator(), "func main() -> void\nend\n");
    for (0..65536) |i| {
        if (i % 1000 == 0) try source.print(arena.allocator(), "func f{d}() -> void\n", .{i});
        try source.print(arena.allocator(), "echo(\"s{d}\")\n", .{i});
        if (i % 1000 == 999 or i == 65535) try source.appendSlice(arena.allocator(), "end\n");
    }
    var dusk = Dusk{ .allocator = arena.allocator(), .stdout_writer = null, .io = testing.io };
    _ = try dusk.compile(source.items);
    try source.print(arena.allocator(), "func g() -> void\necho(\"one more\")\nend\n", .{});
    try testing.expectError(error.TooManyStrings, dusk.compile(source.items));
}

test "functions over 65535 instructions are rejected" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var source: std.ArrayList(u8) = .empty;
    try source.appendSlice(arena.allocator(), "func main() -> void\nend\nfunc big(x: int) -> int\nlet mut y = x\n");
    for (0..20000) |_| try source.appendSlice(arena.allocator(), "if y > 5 then y = y - 1 end\n");
    try source.appendSlice(arena.allocator(), "return y\nend\n");
    var dusk = Dusk{ .allocator = arena.allocator(), .stdout_writer = null, .io = testing.io };
    try testing.expectError(error.FunctionTooLong, dusk.compile(source.items));
}

test "invalid string escapes are rejected" {
    const sources = [_][]const u8{
        "\"\\q\"",       "\"\\x41\"",       "\"\\u\"",         "\"\\u0041\"",      "\"\\u{}\"",
        "\"\\u{g}\"",    "\"\\u{-1}\"",     "\"\\u{ 41}\"",    "\"\\u{41\"",       "\"\\u{D800}\"",
        "\"\\u{DFFF}\"", "\"\\u{110000}\"", "\"\\u{FFFFFF}\"", "\"\\u{0000000}\"", "\"\"\"\n\\q\n\"\"\"",
    };
    for (sources) |source| {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var dusk = Dusk{ .allocator = arena.allocator(), .stdout_writer = null, .io = testing.io };
        const program = try std.fmt.allocPrint(arena.allocator(), "let text = {s}", .{source});
        try testing.expectError(error.ParserError, dusk.compile(program));
    }
}

test "byte" {
    try runCaseOutput("byte.dsk", "119\nc=119 newline=10\nlower\ndigit\nnewline\nother\n[97, 98, 39, 92, 34, 65, 0, 9, 13, 36]\n");
}

test "byte errors" {
    try runCaseError("sema/byte-constant-out-of-range.dsk", error.ByteOutOfRange);
    try runCaseError("sema/byte-negative-constant.dsk", error.ByteOutOfRange);
    try runCaseError("sema/byte-from-int-variable.dsk", error.InvalidType);
    try runCaseError("sema/byte-cast-string.dsk", error.InvalidType);
    try runCaseError("sema/byte-arithmetic-assign.dsk", error.InvalidType);
    try runCaseError("sema/byte-when-case-out-of-range.dsk", error.ByteOutOfRange);
    try runCaseRuntimeError("byte-cast-out-of-range.dsk", error.UnhandledError);
    try runCaseRuntimeError("byte-cast-negative.dsk", error.UnhandledError);
    try runCase("to-byte-errors.dsk");
    try runCase("to-int.dsk");
}

test "buffer" {
    try runCaseOutput("buffer.dsk", "[0, 0, 0, 0]\n4\n[104, 105, 0, 255]\n104\n[111, 108, 195, 161]\n4\nol\xc3\xa1\ninvalid\n575\n" ++
        "buf=[104, 105, 0, 255]\nnull\n4\n[[0], [0, 0]]\n[]\n\n[0, 9]\n2\ninvalid utf-8\nfine\n");
}

test "buffer annotations and parameters" {
    try runCaseOutput("buffer-params.dsk", "[[120, 120, 120, 120]]\n");
}

test "buffer GC" {
    try runCaseOutput("buffer-gc.dsk", "388894\n");
}

test "buffer errors" {
    try runCaseError("sema/buffer-store-out-of-range.dsk", error.ByteOutOfRange);
    try runCaseError("sema/buffer-store-int-variable.dsk", error.InvalidType);
    try runCaseError("sema/buffer-immutable.dsk", error.NotMutable);
    try runCaseError("sema/len-invalid-argument.dsk", error.InvalidType);
    try runCaseError("sema/buffer-size-string.dsk", error.InvalidType);
    try runCaseError("sema/string-from-string.dsk", error.InvalidType);
    try runCaseError("sema/string-from-unhandled.dsk", error.UnhandledError);
    try runCaseError("sema/buffer-as-string.dsk", error.InvalidType);
    try runCaseError("sema/buffer-immutable-param.dsk", error.NotMutable);
    try runCaseError("sema/buffer-mut-param-immutable-arg.dsk", error.NotMutable);
    try runCaseRuntimeError("buffer-index-out-of-bounds.dsk", error.IndexOutOfBounds);
    try runCaseRuntimeError("buffer-store-negative-index.dsk", error.IndexOutOfBounds);
    try runCaseRuntimeError("buffer-negative-size.dsk", error.UnhandledError);
    try runCase("buffer-negative-size-error.dsk");
}

test "invalid byte literals are rejected" {
    const parser_sources = [_][]const u8{ "''", "'ab'", "'\xc3\xa9'", "'\\q'", "'\\u{80}'", "'\\n\\n'" };
    for (parser_sources) |source| {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var dusk = Dusk{ .allocator = arena.allocator(), .stdout_writer = null, .io = testing.io };
        const program = try std.fmt.allocPrint(arena.allocator(), "let b = {s}", .{source});
        try testing.expectError(error.ParserError, dusk.compile(program));
    }
    const lexer_sources = [_][]const u8{ "'", "'a", "'\\'", "'a\nb'", "'\\" };
    for (lexer_sources) |source| {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var dusk = Dusk{ .allocator = arena.allocator(), .stdout_writer = null, .io = testing.io };
        try testing.expectError(error.LexerError, dusk.compile(source));
    }
}

test "invalid UTF-8 and unterminated strings are rejected" {
    const sources = [_][]const u8{
        "\"",                "\"missing",                      "\"trailing\\",            "\"escaped\\\"",        "\"missing\n",
        "\"\xff\"",          "\"\xc0\x80\"",                   "\"\xed\xa0\x80\"",        "\"\xf4\x90\x80\x80\"", "\"\xe2\x82\"",
        "\"first\nsecond\"", "\"a\\\nb\"",                     "\"\\\n\"",                "\"\"\" x\n\"\"\"",     "\"\"\"\n  x",
        "\"\"\"\n  x\"\"\"", "\"\"\"\n    a\n  b\n    \"\"\"", "\"\"\"\n\t\ta\n  \"\"\"", "\"\"\"\n\xff\n\"\"\"",
    };
    for (sources) |source| {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        var dusk = Dusk{ .allocator = arena.allocator(), .stdout_writer = null, .io = testing.io };
        try testing.expectError(error.LexerError, dusk.compile(source));
    }
}

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

test "when on strings" {
    try runCase("when-string.dsk");
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
    try runCase("errors-try-pipe.dsk");
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

test "sema error: nullable anonymous init to plain struct" {
    try runCaseError("sema/anonymous-init-nullable-to-plain.dsk", error.InvalidType);
}

test "sema error: nested array assign type mismatch" {
    try runCaseError("sema/array-nested-assignment-type.dsk", error.InvalidType);
}

test "sema error: array assign type mismatch" {
    try runCaseError("sema/array-assign-type-mismatch.dsk", error.InvalidType);
}

test "sema error: struct field assign type mismatch" {
    try runCaseError("sema/struct-field-assign-type-mismatch.dsk", error.InvalidType);
}

test "sema error: static field can't be mut" {
    try runCaseError("sema/static-field-cant-be-mut.dsk", error.StaticFieldCantBeMutable);
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

test "sema error: struct field assign immutable" {
    try runCaseError("sema/struct-field-assign-immutable.dsk", error.NotMutable);
}

test "sema error: nested struct field assign immutable" {
    try runCaseError("sema/nested-struct-field-assign-immutable.dsk", error.NotMutable);
}

test "sema error: array item field assign immutable" {
    try runCaseError("sema/array-item-field-assign-immutable.dsk", error.NotMutable);
}

test "sema error: struct param field assign" {
    try runCaseError("sema/struct-param-field-assign.dsk", error.NotMutable);
}

test "sema error: self field assign not mut" {
    try runCaseError("sema/self-field-assign-not-mut.dsk", error.NotMutable);
}

test "sema error: capture struct field assign" {
    try runCaseError("sema/capture-struct-field-assign.dsk", error.NotMutable);
}

test "sema error: mut array arg field path" {
    try runCaseError("sema/mut-array-arg-field-path.dsk", error.NotMutable);
}

test "sema error: mut struct arg field path" {
    try runCaseError("sema/mut-struct-arg-field-path.dsk", error.NotMutable);
}

test "sema error: append field path immutable" {
    try runCaseError("sema/append-field-path-immutable.dsk", error.NotMutable);
}

test "temporary values can be changed and chained" {
    try runCase("temporary-changes.dsk");
}

test "mutable paths" {
    try runCase("mutable-paths.dsk");
}

test "nullable box values" {
    try runCase("nullable-box-values.dsk");
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

test "sema error: mut union param reassign" {
    try runCaseError("sema/union-mut-param-reassign.dsk", error.MutBorrowCantBeReassigned);
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

test "sema error: when string needs else" {
    try runCaseError("sema/when-string-needs-else.dsk", error.WhenNotExhaustive);
}

test "sema error: when string duplicate case" {
    try runCaseError("sema/when-string-duplicate-case.dsk", error.WhenDuplicateCase);
}

test "sema error: when string non-literal case" {
    try runCaseError("sema/when-string-non-literal-case.dsk", error.WhenInvalidCase);
}

test "sema error: when string interpolated case" {
    try runCaseError("sema/when-string-interpolated-case.dsk", error.WhenInvalidCase);
}

test "sema error: when string range case" {
    try runCaseError("sema/when-string-range-case.dsk", error.WhenInvalidCase);
}

test "sema error: when string case type mismatch" {
    try runCaseError("sema/when-string-case-type-mismatch.dsk", error.InvalidType);
}

test "sema error: when float subject" {
    try runCaseError("sema/when-float-subject.dsk", error.WhenInvalidCase);
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

test "parser error: float literal with two dots" {
    try runCaseError("parser/float-literal-two-dots.dsk", error.ParserError);
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
    try runCaseError("parser/struct-field-missing-type.dsk", error.ParserError);
    try runCaseError("parser/nested-struct-field-missing-type.dsk", error.ParserError);
    try runCaseError("parser/struct-static-keyword.dsk", error.ParserError);
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
    try runCaseError("parser/pipe-try-non-call.dsk", error.ParserError);
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

test "codegen error: too many variables in a function" {
    try runCaseError("codegen/too-many-variables.dsk", error.TooManyRegisters);
}

test "sema error: top-level let mut" {
    try runCaseError("sema/top-level-let-mut.dsk", error.TopLevelLetNotConstant);
}

test "sema error: top-level let not constant" {
    try runCaseError("sema/top-level-let-not-constant.dsk", error.TopLevelLetNotConstant);
}

test "sema error: statement outside main" {
    try runCaseError("sema/statement-outside-main.dsk", error.StatementOutsideMain);
}

test "sema error: missing main" {
    try runCaseError("sema/missing-main.dsk", error.MissingMain);
}

test "sema error: main with params" {
    try runCaseError("sema/main-with-params.dsk", error.InvalidMainSignature);
}

test "sema error: main returns value" {
    try runCaseError("sema/main-returns-value.dsk", error.InvalidMainSignature);
}

test "sema error: function uses main variable" {
    try runCaseError("sema/function-uses-main-variable.dsk", error.NotDefined);
}

test "sema error: function inside function" {
    try runCaseError("sema/function-inside-function.dsk", error.DeclarationInsideFunction);
}

test "sema error: struct inside function" {
    try runCaseError("sema/struct-inside-function.dsk", error.DeclarationInsideFunction);
}

test "sema error: constant assign" {
    try runCaseError("sema/constant-assign.dsk", error.NotMutable);
}

test "sema error: constant invalid type" {
    try runCaseError("sema/constant-invalid-type.dsk", error.InvalidConstantType);
}

test "sema error: static field not constant" {
    try runCaseError("sema/static-field-not-constant.dsk", error.StaticFieldNotConstant);
}

test "sema error: static field array not constant" {
    try runCaseError("sema/static-field-array-not-constant.dsk", error.StaticFieldNotConstant);
}

test "sema error: success type is the error set" {
    try runCaseError("sema/errors-payload-same-set.dsk", error.PayloadSharesErrors);
}

test "sema error: nullable success type is the error set" {
    try runCaseError("sema/errors-payload-nullable-same-set.dsk", error.PayloadSharesErrors);
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

test "runtime error: unhandled error from main" {
    try runCaseRuntimeError("errors-main-try.dsk", error.UnhandledError);
}

test "main returns error union" {
    try runCase("main-returns-error-union.dsk");
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

test "nullable arrays" {
    try runCase("nullable-arrays.dsk");
}

test "sema error: index into nullable array" {
    try runCaseError("sema/nullable-array-index.dsk", error.NullableMustBeUnwrapped);
}

test "sema error: len of nullable array" {
    try runCaseError("sema/nullable-array-len.dsk", error.InvalidType);
}

test "sema error: nullable array to array" {
    try runCaseError("sema/nullable-array-to-array.dsk", error.InvalidType);
}

test "sema error: field default doesn't match declared type" {
    try runCaseError("sema/field-default-declared-type-mismatch.dsk", error.InvalidType);
}

test "conditional-expressions" {
    try runCase("conditional-expressions.dsk");
}

test "conditional-expression-types" {
    try runCase("conditional-expression-types.dsk");
}

test "conditional-expression-captures" {
    try runCase("conditional-expression-captures.dsk");
}

test "when-expressions" {
    try runCase("when-expressions.dsk");
}

test "conditional-expression-flow" {
    try runCase("conditional-expression-flow.dsk");
}

test "conditional-expression-heap" {
    try runCase("conditional-expression-heap.dsk");
}

test "if-expression-missing-else" {
    try runCaseError("sema/if-expression-missing-else.dsk", error.InvalidExpression);
}

test "if-expression-empty-branch" {
    try runCaseError("sema/if-expression-empty-branch.dsk", error.InvalidExpression);
}

test "if-expression-declaration-tail" {
    try runCaseError("sema/if-expression-declaration-tail.dsk", error.InvalidExpression);
}

test "if-expression-assignment-tail" {
    try runCaseError("sema/if-expression-assignment-tail.dsk", error.InvalidExpression);
}

test "if-expression-void-tail" {
    try runCaseError("sema/if-expression-void-tail.dsk", error.InvalidExpression);
}

test "if-expression-type-mismatch" {
    try runCaseError("sema/if-expression-type-mismatch.dsk", error.InvalidType);
}

test "if-expression-context-mismatch" {
    try runCaseError("sema/if-expression-context-mismatch.dsk", error.InvalidType);
}

test "if-expression-capture-missing-else" {
    try runCaseError("sema/if-expression-capture-missing-else.dsk", error.InvalidExpression);
}

test "if-expression-capture-else-scope" {
    try runCaseError("sema/if-expression-capture-else-scope.dsk", error.NotDefined);
}

test "if-expression-capture-leaks" {
    try runCaseError("sema/if-expression-capture-leaks.dsk", error.NotDefined);
}

test "if-expression-local-leaks" {
    try runCaseError("sema/if-expression-local-leaks.dsk", error.NotDefined);
}

test "when-expression-missing-value" {
    try runCaseError("sema/when-expression-missing-value.dsk", error.InvalidExpression);
}

test "when-expression-type-mismatch" {
    try runCaseError("sema/when-expression-type-mismatch.dsk", error.InvalidType);
}

test "when-expression-not-exhaustive" {
    try runCaseError("sema/when-expression-not-exhaustive.dsk", error.WhenNotExhaustive);
}

test "when-expression-useless-else" {
    try runCaseError("sema/when-expression-useless-else.dsk", error.WhenUselessElse);
}

test "when-expression-capture-leaks" {
    try runCaseError("sema/when-expression-capture-leaks.dsk", error.NotDefined);
}

test "if-expression-condition-not-bool" {
    try runCaseError("sema/if-expression-condition-not-bool.dsk", error.InvalidType);
}

test "if-expression-non-tail-value" {
    try runCaseError("sema/if-expression-non-tail-value.dsk", error.InvalidType);
}

test "if-expression-nullable-to-plain" {
    try runCaseError("sema/if-expression-nullable-to-plain.dsk", error.InvalidType);
}

test "conditional break is not a function return" {
    try runCaseError("sema/conditional-break-is-not-return.dsk", error.InvalidReturnType);
}

test "for-capture" {
    try runCase("for-capture.dsk");
}

test "for-capture-source" {
    try runCase("for-capture-source.dsk");
}

test "for-capture-flow" {
    try runCase("for-capture-flow.dsk");
}

test "for-capture-heap" {
    try runCase("for-capture-heap.dsk");
}

test "for-capture-non-array" {
    try runCaseError("sema/for-capture-non-array.dsk", error.InvalidType);
}

test "for-capture-nullable-array" {
    try runCaseError("sema/for-capture-nullable-array.dsk", error.InvalidType);
}

test "for-capture-duplicate" {
    try runCaseError("sema/for-capture-duplicate.dsk", error.AlreadyDefined);
}

test "for-capture-item-assignment" {
    try runCaseError("sema/for-capture-item-assignment.dsk", error.NotMutable);
}

test "for-capture-index-assignment" {
    try runCaseError("sema/for-capture-index-assignment.dsk", error.NotMutable);
}

test "for-capture-item-leaks" {
    try runCaseError("sema/for-capture-item-leaks.dsk", error.NotDefined);
}

test "for-capture-index-leaks" {
    try runCaseError("sema/for-capture-index-leaks.dsk", error.NotDefined);
}

test "for-capture-ignored-binding" {
    try runCaseError("sema/for-capture-ignored-binding.dsk", error.NotDefined);
}

test "for-capture-struct-mutation" {
    try runCaseError("sema/for-capture-struct-mutation.dsk", error.NotMutable);
}

test "for-capture-array-mutation" {
    try runCaseError("sema/for-capture-array-mutation.dsk", error.NotMutable);
}

test "for-capture-missing-return" {
    try runCaseError("sema/for-capture-missing-return.dsk", error.InvalidReturnType);
}

test "for-capture-missing-item" {
    try runCaseError("parser/for-capture-missing-item.dsk", error.ParserError);
}

test "for-capture-missing-index" {
    try runCaseError("parser/for-capture-missing-index.dsk", error.ParserError);
}

test "for-capture-extra-binding" {
    try runCaseError("parser/for-capture-extra-binding.dsk", error.ParserError);
}

test "sema error: for capture mut primitive" {
    try runCaseError("sema/for-capture-mut-primitive.dsk", error.PrimitiveCaptureCantBeMutable);
}

test "sema error: for capture mut immutable source" {
    try runCaseError("sema/for-capture-mut-immutable-source.dsk", error.UnwrappedCantBeMutable);
}

test "sema error: if capture mut primitive" {
    try runCaseError("sema/if-capture-mut-primitive.dsk", error.PrimitiveCaptureCantBeMutable);
}

test "sema error: when capture mut primitive" {
    try runCaseError("sema/when-capture-mut-primitive.dsk", error.PrimitiveCaptureCantBeMutable);
}

test "sema error: when capture mut immutable subject" {
    try runCaseError("sema/when-capture-mut-immutable-subject.dsk", error.UnwrappedCantBeMutable);
}

test "sema error: if capture mut reassign" {
    try runCaseError("sema/if-capture-mut-reassign.dsk", error.MutBorrowCantBeReassigned);
}

test "sema error: mut param reassign" {
    try runCaseError("sema/mut-param-reassign.dsk", error.MutBorrowCantBeReassigned);
}

test "sema error: for capture mut reassign" {
    try runCaseError("sema/for-capture-mut-reassign.dsk", error.MutBorrowCantBeReassigned);
}

test "sema error: range capture mut" {
    try runCaseError("sema/range-capture-mut.dsk", error.PrimitiveCaptureCantBeMutable);
}

test "mut captures" {
    try runCase("mut-captures.dsk");
}

test "copy" {
    try runCase("copy.dsk");
}

test "values from non-mut names can't go into mutable places" {
    try runCase("read-only-sources.dsk");
    try runCaseError("sema/read-only-into-let-mut.dsk", error.ReadOnlyIntoMutable);
    try runCaseError("sema/read-only-into-constructor.dsk", error.ReadOnlyIntoMutable);
    try runCaseError("sema/read-only-into-append.dsk", error.NotMutable);
    try runCaseError("sema/read-only-into-mut-param.dsk", error.ReadOnlyIntoMutable);
    try runCaseError("sema/read-only-into-field.dsk", error.ReadOnlyIntoMutable);
    try runCaseError("sema/read-only-into-assignment.dsk", error.ReadOnlyIntoMutable);
    try runCaseError("sema/read-only-capture-into-array.dsk", error.NotMutable);
}

test "generics: mut params on a bare slot" {
    try runCase("generics-mut-slot-param.dsk");
    try runCaseError("sema/generic-store-without-mut.dsk", error.NotMutable);
    try runCaseError("sema/read-only-into-generic-mut-param.dsk", error.NotMutable);
}

test "tail calls" {
    try runCase("tail-calls.dsk");
}

test "assignment reads its own target" {
    try runCase("assign-reads-target.dsk");
}

test "division: '//' and '%' by zero are runtime errors" {
    try runCase("division-edges.dsk");
    try runCaseRuntimeError("int-division-by-zero.dsk", error.DivisionByZero);
    try runCaseRuntimeError("int-modulo-by-zero.dsk", error.DivisionByZero);
    try runCaseRuntimeError("float-floor-division-by-zero.dsk", error.DivisionByZero);
    try runCaseRuntimeError("int-division-overflow.dsk", error.DivisionOverflow);
}

test "integer overflow and float to int out of range are runtime errors" {
    try runCase("integer-limits.dsk");
    try runCaseRuntimeError("int-add-overflow.dsk", error.IntegerOverflow);
    try runCaseRuntimeError("int-sub-overflow.dsk", error.IntegerOverflow);
    try runCaseRuntimeError("int-mult-overflow.dsk", error.IntegerOverflow);
    try runCaseRuntimeError("int-negate-overflow.dsk", error.IntegerOverflow);
    try runCaseRuntimeError("float-to-int-out-of-range.dsk", error.FloatOutOfIntRange);
    try runCaseRuntimeError("float-to-int-nan.dsk", error.FloatOutOfIntRange);
}

test "runtime error: call depth exceeded" {
    try runCaseRuntimeError("call-depth-exceeded.dsk", error.CallDepthExceeded);
}

test "min" {
    try runCase("min.dsk");
}

test "gc stress struct field mutation" {
    try runCase("gc-stress-struct-field-mutation.dsk");
}

test "deep recursion" {
    try runCase("deep-recursion.dsk");
}

test "elif" {
    try runCase("elif.dsk");
}

test "gc stress array of structs" {
    try runCase("gc-stress-array-of-structs.dsk");
}

test "gc stress fn return struct" {
    try runCase("gc-stress-fn-return-struct.dsk");
}

test "gc stress struct field append" {
    try runCase("gc-stress-struct-field-append.dsk");
}

test "gc struct with array in loop" {
    try runCase("gc-struct-with-array-in-loop.dsk");
}

test "register stress" {
    try runCase("register-stress.dsk");
}

test "test" {
    try runCase("test.dsk");
}

test "for-capture-missing-do" {
    try runCaseError("parser/for-capture-missing-do.dsk", error.ParserError);
}

test "for-capture-mut-method" {
    try runCaseError("sema/for-capture-mut-method.dsk", error.NotMutable);
}

test "for-capture-array-assignment" {
    try runCaseError("sema/for-capture-array-assignment.dsk", error.NotMutable);
}

test "range-loops" {
    try runCase("range-loops.dsk");
}

test "range-loop-flow" {
    try runCase("range-loop-flow.dsk");
}

test "range-loop-bounds" {
    try runCase("range-loop-bounds.dsk");
}

test "when-integer-ranges" {
    try runCase("when-integer-ranges.dsk");
}

test "constants" {
    try runCase("constants.dsk");
}

test "gc-nullable-starts-null" {
    try runCase("gc-nullable-starts-null.dsk");
}

test "array-nested-assignment" {
    try runCase("array-nested-assignment.dsk");
}

test "nullable-equality" {
    try runCase("nullable-equality.dsk");
}

test "errors-error-set-payload" {
    try runCase("errors-error-set-payload.dsk");
}

test "errors-catch-condition" {
    try runCase("errors-catch-condition.dsk");
}

test "statement-registers" {
    try runCase("statement-registers.dsk");
}

test "when-float-ranges" {
    try runCase("when-float-ranges.dsk");
}

test "range-loop-float" {
    try runCaseError("sema/range-loop-float.dsk", error.InvalidType);
}

test "range-loop-mixed-bounds" {
    try runCaseError("sema/range-loop-mixed-bounds.dsk", error.InvalidType);
}

test "range-loop-nullable-bound" {
    try runCaseError("sema/range-loop-nullable-bound.dsk", error.InvalidType);
}

test "range-loop-string-bound" {
    try runCaseError("sema/range-loop-string-bound.dsk", error.InvalidType);
}

test "range-loop-duplicate-capture" {
    try runCaseError("sema/range-loop-duplicate-capture.dsk", error.AlreadyDefined);
}

test "range-loop-immutable-value" {
    try runCaseError("sema/range-loop-immutable-value.dsk", error.NotMutable);
}

test "range-loop-immutable-index" {
    try runCaseError("sema/range-loop-immutable-index.dsk", error.NotMutable);
}

test "range-loop-capture-leaks" {
    try runCaseError("sema/range-loop-capture-leaks.dsk", error.NotDefined);
}

test "range-stored-value" {
    try runCaseError("sema/range-stored-value.dsk", error.InvalidExpression);
}

test "range-no-capture" {
    try runCaseError("sema/range-no-capture.dsk", error.InvalidExpression);
}

test "when-range-reversed" {
    try runCaseError("sema/when-range-reversed.dsk", error.WhenInvalidCase);
}

test "when-range-overlap" {
    try runCaseError("sema/when-range-overlap.dsk", error.WhenDuplicateCase);
}

test "when-range-touching" {
    try runCaseError("sema/when-range-touching.dsk", error.WhenDuplicateCase);
}

test "when-range-literal-overlap" {
    try runCaseError("sema/when-range-literal-overlap.dsk", error.WhenDuplicateCase);
}

test "when-literal-range-overlap" {
    try runCaseError("sema/when-literal-range-overlap.dsk", error.WhenDuplicateCase);
}

test "when-range-list-overlap" {
    try runCaseError("sema/when-range-list-overlap.dsk", error.WhenDuplicateCase);
}

test "when-range-runtime-bound" {
    try runCaseError("sema/when-range-runtime-bound.dsk", error.WhenInvalidCase);
}

test "when-range-expression-bound" {
    try runCaseError("sema/when-range-expression-bound.dsk", error.WhenInvalidCase);
}

test "when: int cases can be constants, static fields and operators on them" {
    try runCase("when-constant-cases.dsk");
    try runCaseError("sema/when-case-overflow.dsk", error.WhenInvalidCase);
}

test "when-range-float-on-int" {
    try runCaseError("sema/when-range-float-on-int.dsk", error.InvalidType);
}

test "when-range-enum-subject" {
    try runCaseError("sema/when-range-enum-subject.dsk", error.WhenInvalidCase);
}

test "when-range-needs-else" {
    try runCaseError("sema/when-range-needs-else.dsk", error.WhenNotExhaustive);
}

test "when-float-range-needs-else" {
    try runCaseError("sema/when-float-range-needs-else.dsk", error.WhenNotExhaustive);
}

test "when-float-range-overlap" {
    try runCaseError("sema/when-float-range-overlap.dsk", error.WhenDuplicateCase);
}

test "when-float-range-touching" {
    try runCaseError("sema/when-float-range-touching.dsk", error.WhenDuplicateCase);
}

test "when-float-range-reversed" {
    try runCaseError("sema/when-float-range-reversed.dsk", error.WhenInvalidCase);
}

test "when-float-range-runtime-bound" {
    try runCaseError("sema/when-float-range-runtime-bound.dsk", error.WhenInvalidCase);
}

test "when-float-range-string-bound" {
    try runCaseError("sema/when-float-range-string-bound.dsk", error.WhenInvalidCase);
}

test "when-float-scalar-in-list" {
    try runCaseError("sema/when-float-scalar-in-list.dsk", error.WhenInvalidCase);
}

test "when-float-null-non-nullable" {
    try runCaseError("sema/when-float-null-non-nullable.dsk", error.WhenInvalidCase);
}

test "range-missing-end" {
    try runCaseError("parser/range-missing-end.dsk", error.ParserError);
}

test "range-chained" {
    try runCaseError("parser/range-chained.dsk", error.ParserError);
}

test "range-missing-start" {
    try runCaseError("parser/range-missing-start.dsk", error.ParserError);
}

test "range-three-dots" {
    try runCaseError("parser/range-three-dots.dsk", error.ParserError);
}

test "when-float-range-infinite-bound" {
    try runCaseError("sema/when-float-range-infinite-bound.dsk", error.WhenInvalidCase);
}

test "orelse-values" {
    try runCase("orelse-values.dsk");
}

test "orelse-evaluation" {
    try runCase("orelse-evaluation.dsk");
}

test "orelse-context" {
    try runCase("orelse-context.dsk");
}

test "orelse-families" {
    try runCase("orelse-families.dsk");
}

test "orelse-precedence" {
    try runCase("orelse-precedence.dsk");
}

test "orelse-flow-heap" {
    try runCase("orelse-flow-heap.dsk");
}

test "orelse-non-nullable" {
    try runCaseError("sema/orelse-non-nullable.dsk", error.InvalidType);
}

test "orelse-incompatible" {
    try runCaseError("sema/orelse-incompatible.dsk", error.InvalidType);
}

test "orelse-nullable-incompatible" {
    try runCaseError("sema/orelse-nullable-incompatible.dsk", error.InvalidType);
}

test "orelse-void" {
    try runCaseError("sema/orelse-void.dsk", error.InvalidExpression);
}

test "orelse-void-discarded" {
    try runCaseError("sema/orelse-void-discarded.dsk", error.InvalidExpression);
}

test "orelse-null-void" {
    try runCaseError("sema/orelse-null-void.dsk", error.InvalidExpression);
}

test "orelse-nullable-to-plain" {
    try runCaseError("sema/orelse-nullable-to-plain.dsk", error.InvalidType);
}

test "orelse-context-mismatch" {
    try runCaseError("sema/orelse-context-mismatch.dsk", error.InvalidType);
}

test "orelse-no-array-promotion" {
    try runCaseError("sema/orelse-no-array-promotion.dsk", error.InvalidType);
}

test "orelse-unhandled-error" {
    try runCaseError("sema/orelse-unhandled-error.dsk", error.UnhandledError);
}

test "orelse-missing-fallback" {
    try runCaseError("parser/orelse-missing-fallback.dsk", error.ParserError);
}

test "orelse-missing-operand" {
    try runCaseError("parser/orelse-missing-operand.dsk", error.ParserError);
}

test "orelse-keyword" {
    try runCaseError("parser/orelse-keyword.dsk", error.ParserError);
}

test "orelse-direct-return" {
    try runCaseError("parser/orelse-direct-return.dsk", error.ParserError);
}

test "orelse-direct-break" {
    try runCaseError("parser/orelse-direct-break.dsk", error.ParserError);
}

test "orelse-direct-continue" {
    try runCaseError("parser/orelse-direct-continue.dsk", error.ParserError);
}

test "int to float promotion" {
    try runCase("int-to-float-promotion.dsk");
}

test "sema error: int array to float array" {
    try runCaseError("sema/int-array-to-float-array.dsk", error.InvalidType);
}

test "sema error: nullable int to float" {
    try runCaseError("sema/nullable-int-to-float.dsk", error.InvalidType);
}

test "sema error: missing main in an empty file" {
    try runCaseError("sema/missing-main-empty-file.dsk", error.MissingMain);
}

test "float cast" {
    try runCase("float-cast.dsk");
}

test "sema error: float cast of a string" {
    try runCaseError("sema/float-cast-string.dsk", error.InvalidType);
}

test "sema error: float cast of a slot that allows a string" {
    try runCaseError("sema/float-cast-slot-string.dsk", error.InvalidType);
}

test "extern: only the prelude can declare extern functions" {
    try runCaseError("sema/extern-outside-prelude.dsk", error.ExternOutsidePrelude);
    try runCaseError("sema/builtin-redefined.dsk", error.AlreadyDefined);
    try runCaseError("parser/extern-missing-return-type.dsk", error.ParserError);
    try runCaseError("parser/extern-missing-func.dsk", error.ParserError);
}

test "generics: generic functions are checked where they are written" {
    try runCase("generics-definitions.dsk");
    try runCaseError("parser/generic-empty-slots.dsk", error.ParserError);
    try runCaseError("parser/generic-slot-trailing-comma.dsk", error.ParserError);
    try runCaseError("parser/generic-slot-missing-comma.dsk", error.ParserError);
    try runCaseError("sema/generic-slot-arithmetic.dsk", error.SlotOperatorNotAllowed);
    try runCaseError("sema/generic-slot-equality.dsk", error.SlotOperatorNotAllowed);
    try runCaseError("sema/generic-slot-from-int.dsk", error.InvalidType);
    try runCaseError("sema/generic-slot-default.dsk", error.SlotParamCantHaveDefault);
    try runCaseError("sema/generic-duplicate-slot.dsk", error.AlreadyDefined);
    try runCaseError("sema/generic-slot-as-value.dsk", error.SlotUsedAsValue);
    try runCaseError("sema/generic-as-value.dsk", error.GenericNeedsCall);
}

test "generics: calls fill the slots from the arguments and compile one copy per type" {
    try runCase("generics-calls.dsk");
    try runCaseError("sema/generic-conflicting-slots.dsk", error.InvalidType);
    try runCaseError("sema/generic-cant-infer.dsk", error.CantInferSlot);
    try runCaseError("sema/generic-cant-infer-empty-array.dsk", error.CantInferSlot);
    try runCaseError("sema/generic-copy-too-deep.dsk", error.GenericCopyTooDeep);
    try runCaseError("sema/generic-void-argument.dsk", error.InvalidSlotType);
}

test "generics: slots the arguments don't fill come from the expected type" {
    try runCase("generics-expected-type.dsk");
    try runCaseError("sema/generic-expected-type-mismatch.dsk", error.InvalidType);
}

test "generics: generic structs and unions" {
    try runCase("generics-types.dsk");
    try runCaseError("parser/generic-enum.dsk", error.ParserError);
    try runCaseError("sema/generic-type-too-many-args.dsk", error.WrongSlotCount);
    try runCaseError("sema/generic-type-missing-args.dsk", error.WrongSlotCount);
    try runCaseError("sema/generic-args-on-plain-struct.dsk", error.WrongSlotCount);
    try runCaseError("sema/generic-union-cant-infer.dsk", error.CantInferSlot);
    try runCaseError("sema/generic-struct-cant-infer.dsk", error.CantInferSlot);
    try runCaseError("sema/generic-type-void-arg.dsk", error.InvalidSlotType);
    try runCaseError("sema/generic-struct-method-arithmetic.dsk", error.SlotOperatorNotAllowed);
    try runCaseError("sema/generic-type-duplicate-slot.dsk", error.AlreadyDefined);
}

test "generics: append and copy are generic prelude functions" {
    try runCase("generics-prelude.dsk");
    try runCaseError("sema/append-wrong-item-type.dsk", error.InvalidType);
    try runCaseError("sema/append-int-into-strings.dsk", error.InvalidType);
    try runCaseError("sema/generic-conflicting-slots-in-one-argument.dsk", error.ConflictingSlotTypes);
    try runCaseError("sema/len-empty-literal.dsk", error.CantInferSlot);
    try runCaseError("sema/len-not-array.dsk", error.InvalidType);
}

test "generics: type lists constrain a slot to primitives" {
    try runCase("generics-type-lists.dsk");
    try runCaseError("parser/generic-list-trailing-bar.dsk", error.ParserError);
    try runCaseError("parser/trait-missing-eq.dsk", error.ParserError);
    try runCaseError("sema/generic-list-doesnt-fit.dsk", error.TypeDoesntFitSlot);
    try runCaseError("sema/generic-list-annotation-doesnt-fit.dsk", error.TypeDoesntFitSlot);
    try runCaseError("sema/generic-list-slot-doesnt-fit.dsk", error.TypeDoesntFitSlot);
    try runCaseError("sema/generic-list-string-equality.dsk", error.SlotOperatorNotAllowed);
    try runCaseError("sema/generic-list-operator-member.dsk", error.SlotOperatorNotAllowed);
    try runCaseError("sema/generic-list-result-differs.dsk", error.SlotResultDiffers);
    try runCaseError("sema/generic-list-literal-doesnt-fit.dsk", error.InvalidType);
    try runCaseError("sema/generic-list-not-primitive.dsk", error.TypeListNotPrimitive);
    try runCaseError("sema/generic-list-nullable-member.dsk", error.TypeListNotPrimitive);
    try runCaseError("sema/trait-list-cycle.dsk", error.TraitRefersToItself);
    try runCaseError("sema/trait-used-as-type.dsk", error.TraitUsedAsType);
    try runCaseError("sema/trait-used-as-value.dsk", error.TraitUsedAsType);
    try runCaseError("sema/trait-inside-function.dsk", error.DeclarationInsideFunction);
}

test "generics: traits constrain a slot to types with methods" {
    try runCase("generics-traits.dsk");
    try runCaseError("parser/trait-method-with-body.dsk", error.ParserError);
    try runCaseError("parser/trait-method-slots.dsk", error.ParserError);
    try runCaseError("sema/trait-missing-method.dsk", error.DoesntFitTrait);
    try runCaseError("sema/trait-wrong-signature.dsk", error.DoesntFitTrait);
    try runCaseError("sema/trait-mut-self-mismatch.dsk", error.DoesntFitTrait);
    try runCaseError("sema/trait-field-doesnt-fit.dsk", error.DoesntFitTrait);
    try runCaseError("sema/trait-unconstrained-slot-doesnt-fit.dsk", error.DoesntFitTrait);
    try runCaseError("sema/trait-mut-method-immutable.dsk", error.NotMutable);
    try runCaseError("sema/trait-slot-no-method.dsk", error.SlotHasNoMethod);
    try runCaseError("sema/trait-with-type-list.dsk", error.TypeListWithTrait);
    try runCaseError("sema/trait-in-type-list.dsk", error.TypeListWithTrait);
    try runCaseError("sema/trait-clashing-methods.dsk", error.ClashingTraitMethods);
    try runCaseError("sema/trait-parent-cycle.dsk", error.TraitRefersToItself);
    try runCaseError("sema/trait-method-needs-self.dsk", error.TraitMethodNeedsSelf);
    try runCaseError("sema/trait-method-inferred-errors.dsk", error.TraitMethodNeedsErrorSet);
    try runCaseError("sema/trait-parent-not-trait.dsk", error.NotATrait);
    try runCaseError("sema/trait-duplicate-method.dsk", error.AlreadyDefined);
}

test "builtins: pop is a prelude function over an internal builtin" {
    try runCase("array-pop.dsk");
    try runCaseError("sema/internal-builtin.dsk", error.InternalBuiltin);
}

test "builtins: insert, remove and clear change arrays in place" {
    try runCase("array-insert-remove.dsk");
}

test "builtins: reverse, slice, index_of and contains" {
    try runCase("array-search-reverse-slice.dsk");
    try runCaseError("sema/contains-struct.dsk", error.TypeDoesntFitSlot);
}

test "sema error: unknown return type is reported, not a crash" {
    try runCaseError("sema/unknown-return-type.dsk", error.UnknownType);
}

test "prelude: every extern has a binding and every binding has an extern" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const dusk = @import("dusk.zig");
    const bindings = @import("natives.zig").bindings;
    const prelude = try dusk.parse(arena.allocator(), dusk.prelude_src);
    var extern_count: usize = 0;
    for (prelude.statements.items) |stmt| {
        if (stmt.data != .func or !stmt.data.func.is_extern) continue;
        try testing.expect(bindings.has(stmt.data.func.identifier));
        extern_count += 1;
    }
    try testing.expectEqual(bindings.keys().len, extern_count);
}

test "prelude: builtin error ids match the natives constants" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const natives = @import("natives.zig");
    var dusk = Dusk{ .allocator = arena.allocator(), .stdout_writer = null, .io = testing.io };
    const program = try dusk.compile("func main() -> void\nend\n");
    try testing.expectEqualStrings("ConversionError.Invalid", program.error_names[natives.conversion_invalid_id]);
    try testing.expectEqualStrings("ConversionError.Overflow", program.error_names[natives.conversion_overflow_id]);
    try testing.expectEqualStrings("StringError.OutOfRange", program.error_names[natives.string_out_of_range_id]);
    try testing.expectEqualStrings("StringError.NegativeCount", program.error_names[natives.string_negative_count_id]);
    try testing.expectEqualStrings("StringError.InvalidUtf8", program.error_names[natives.string_invalid_utf8_id]);
    try testing.expectEqualStrings("ArrayError.Empty", program.error_names[natives.array_empty_id]);
    try testing.expectEqualStrings("ArrayError.OutOfRange", program.error_names[natives.array_out_of_range_id]);
}

test "array index out of bounds is a runtime error" {
    try runCaseRuntimeError("array-index-out-of-bounds.dsk", error.ArrayOutOfBounds);
    try runCaseRuntimeError("array-negative-index.dsk", error.ArrayOutOfBounds);
    try runCaseRuntimeError("array-store-negative-index.dsk", error.ArrayOutOfBounds);
}
