const std = @import("std");
const testing = std.testing;
const dusk = @import("dusk.zig");
const modules = @import("compiler/modules.zig");
const SemaAnalyzer = @import("compiler/sema.zig").SemaAnalyzer;
const BytecodeGen = @import("compiler/codegen.zig").BytecodeGen;
const Program = @import("runtime/bytecode.zig").Program;
const VM = @import("runtime/vm.zig").VM;
const math = @import("runtime/builtins/std/math.zig");
const math_source = @embedFile("runtime/builtins/std/math.dsk");

test "math: every extern has exactly one binding" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const root = try dusk.parse(arena.allocator(), math_source);
    var extern_count: usize = 0;
    for (root.statements.items) |stmt| {
        if (stmt.data != .func) continue;
        if (!stmt.data.func.is_extern) continue;
        try testing.expect(math.bindings.has(stmt.data.func.identifier));
        extern_count += 1;
    }
    try testing.expectEqual(math.bindings.keys().len, extern_count);
    inline for (math.error_sets) |ErrorEnum| {
        var has_matching_set = false;
        for (root.statements.items) |stmt| {
            if (stmt.data != .error_set) continue;
            const set = stmt.data.error_set;
            const name = @typeName(ErrorEnum);
            const short_name = name[std.mem.lastIndexOfScalar(u8, name, '.').? + 1 ..];
            if (!std.mem.eql(u8, set.identifier, short_name)) continue;
            has_matching_set = true;
            try testing.expectEqual(@typeInfo(ErrorEnum).@"enum".fields.len, set.variants.len);
            inline for (@typeInfo(ErrorEnum).@"enum".fields, 0..) |field, index| {
                try testing.expectEqualStrings(field.name, set.variants[index].identifier);
            }
        }
        try testing.expect(has_matching_set);
    }
}

test "math: numeric functions, constants and special values" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const source = try std.Io.Dir.cwd().readFileAlloc(testing.io, "test/success/math.dsk", arena.allocator(), .unlimited);
    try runMathSource(source);
}

test "math: extrema remain correct across garbage collection" {
    try runMathSource(
        \\import std/math
        \\func main() -> math::MathError!void
        \\    let values = [9, -7, 3]
        \\    let floats = [9.5, -7.5, 3.5]
        \\    let saved_min = try math::min(values)
        \\    let saved_max = try math::max(floats)
        \\    for 0..40000: i do
        \\        let garbage = [i, i + 1, i + 2]
        \\        assert((try math::min(values)) == -7)
        \\        assert((try math::max(values)) == 9)
        \\        assert((try math::min(floats)) == -7.5)
        \\        assert((try math::max(floats)) == 9.5)
        \\    end
        \\    assert(saved_min == -7)
        \\    assert(saved_max == 9.5)
        \\end
    );
}

test "math: extrema read arrays after growth" {
    try runMathSource(
        \\import std/math
        \\import std/array
        \\func main() -> math::MathError!void
        \\    let mut integers = [0]
        \\    let mut floats = [0.0]
        \\    for 1..1000: i do
        \\        array::append(integers, i)
        \\        array::append(floats, -to_float(i))
        \\    end
        \\    assert((try math::min(integers)) == 0)
        \\    assert((try math::max(integers)) == 1000)
        \\    assert((try math::min(floats)) == -1000.0)
        \\    assert((try math::max(floats)) == 0.0)
        \\end
    );
}

test "math: abs rejects the minimum integer" {
    try testing.expectError(error.IntegerOverflow, runMathSource("import std/math\nfunc main() -> void let result = math::abs((-9223372036854775807 - 1)) end"));
}

test "math: integer rounding rejects nonfinite and out of range results" {
    for ([_][]const u8{ "floor", "ceil", "round", "trunc" }) |function| {
        for ([_][]const u8{ "math::inf", "-math::inf", "math::nan", "9223372036854775808.0", "-9223372036854777856.0" }) |value| {
            var arena = std.heap.ArenaAllocator.init(testing.allocator);
            defer arena.deinit();
            const source = try std.fmt.allocPrint(arena.allocator(), "import std/math\nfunc main() -> void let result = math::{s}({s}) end", .{ function, value });
            try testing.expectError(error.FloatOutOfIntRange, runMathSource(source));
        }
    }
}

test "math: min and max reject nonnumeric element types" {
    for ([_][]const u8{ "min", "max" }) |function| {
        for ([_][]const u8{ "[true]", "[\"text\"]", "[null]", "[[1]]" }) |values| {
            var arena = std.heap.ArenaAllocator.init(testing.allocator);
            defer arena.deinit();
            const source = try std.fmt.allocPrint(arena.allocator(), "import std/math\nfunc main() -> void let result = math::{s}({s}) end", .{ function, values });
            const expected_error = if (std.mem.eql(u8, values, "[null]")) error.CantInferSlot else error.TypeDoesntFitSlot;
            try testing.expectError(expected_error, compileMathSource(arena.allocator(), source));
        }
    }
}

test "math: empty extrema return the declared error and propagate through try" {
    try runMathSource(
        \\import std/math
        \\func smallest(values: []int) -> math::MathError!int
        \\    return try math::min(values)
        \\end
        \\func largest(values: []float) -> math::MathError!float
        \\    return try math::max(values)
        \\end
        \\func main() -> math::MathError!void
        \\    let integers: []int = []
        \\    let floats: []float = []
        \\    let mut empty_failures = 0
        \\    smallest(integers) catch err then
        \\        assert(err == math::MathError.Empty)
        \\        assert(err != ConversionError.Overflow)
        \\        empty_failures = empty_failures + 1
        \\    end
        \\    largest(floats) catch err then
        \\        assert(err == math::MathError.Empty)
        \\        empty_failures = empty_failures + 1
        \\    end
        \\    assert(empty_failures == 2)
        \\    assert((try smallest([3, 1])) == 1)
        \\    assert((try largest([3.5, 1.5])) == 3.5)
        \\end
    );
}

fn runMathSource(source: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const program = try compileMathSource(arena.allocator(), source);
    var vm = VM.init(&program, .{});
    defer vm.deinit();
    try vm.run();
}

fn compileMathSource(allocator: std.mem.Allocator, source: []const u8) !Program {
    const math_root = try dusk.parse(allocator, math_source);
    const entry_root = try dusk.parse(allocator, source);
    const prelude_root = try dusk.parse(allocator, dusk.prelude_src);
    const array_source = dusk.stdSource("array");
    const array_root = try dusk.parse(allocator, array_source);
    var imports: std.ArrayList(modules.Import) = .empty;
    for (entry_root.statements.items) |stmt| {
        if (stmt.data != .import) break;
        const index: usize = if (std.mem.eql(u8, stmt.data.import.path, "math")) 0 else 1;
        try imports.append(allocator, .{ .stmt = stmt.data.import, .module_index = index });
    }
    const loaded_modules = [_]modules.Module{
        .{ .path = "std/math", .file_path = "std/math.dsk", .src = math_source, .root = math_root, .bindings = &math.bindings, .imports = &.{} },
        .{ .path = "std/array", .file_path = "std/array.dsk", .src = array_source, .root = array_root, .bindings = &@import("runtime/builtins/std/array.zig").bindings, .imports = &.{} },
        .{ .path = "@/math-test", .file_path = null, .src = source, .root = entry_root, .bindings = null, .imports = imports.items },
    };
    var sema = try SemaAnalyzer.init(allocator);
    const ir = try sema.analyze(&prelude_root, dusk.prelude_src, &loaded_modules);
    var codegen = BytecodeGen.init(allocator, &sema.type_table);
    return codegen.generate(&ir);
}
