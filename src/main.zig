pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    const allocator = arena.allocator();

    const src =
        \\let fib: fn = fn(n: number) -> number
        \\    if n < 2
        \\        return n
        \\
        \\    let a: number = fib(n - 1)
        \\    let b: number = fib(n - 2)
        \\    return a + b
        \\let mut i: number = 1
        \\
        \\for i < 20
        \\    echo(fib(i))
        \\    i = i + 1
        \\echo("Hello world")
    ;

    // const src =
    //     \\let factorial: fn = fn (n: number) -> number
    //     \\    let mut result: number = 1
    //     \\    let mut i: number = 1
    //     \\
    //     \\    for i <= n
    //     \\        result = result * i
    //     \\        i = i + 1
    //     \\
    //     \\    return result
    // ;

    // const src =
    //     \\let test_shadow : fn = fn() -> number
    //     \\    let x: number = 10
    //     \\    if true
    //     \\        let x: number = 999
    //     \\        return x
    //     \\    return x
    // ;

    var token_list: std.ArrayList(Token) = .empty;
    var lexer = Lexer.init(src);

    while (true) {
        const token = lexer.next();
        try token_list.append(allocator, token);

        //todo: remember currently the lexer keep parsing even an error
        //todo: lexer errors for sure needs improvement
        if (token.tag == Tag.eof) break;
        if (token.tag == Tag.err) {
            std.debug.print("Invalid token = {any}\n", .{token.value(src)});
            return;
        }
    }

    try dump(allocator, token_list.items, "tokens_dump.json");

    var parser = Parser.init(allocator, src, token_list);
    const ast = try parser.parse();

    try dump(allocator, ast, "ast_dump.json");

    var sema = try SemaAnalyzer.init(allocator);
    const ir = try sema.analyze(&ast);

    try dump(allocator, ir, "ir_dump.json");

    const js_file = "code.js";
    const file = try std.fs.cwd().createFile(js_file, .{});
    defer file.close();

    var code_generator = Generator{
        .allocator = allocator,
    };

    const code = try code_generator.generate(ir);
    try file.writeAll(code);

    try runJsFile(allocator, js_file);
}

fn dump(allocator: std.mem.Allocator, obj: anytype, file_name: []const u8) !void {
    const file = try std.fs.cwd().createFile(file_name, .{});
    defer file.close();

    var out: std.io.Writer.Allocating = .init(allocator);
    try std.json.Stringify.value(obj, .{ .whitespace = .indent_2 }, &out.writer);

    try file.writeAll(try out.toOwnedSlice());
}

fn runJsFile(allocator: std.mem.Allocator, file_path: []const u8) !void {
    const argv = &[_][]const u8{ "bun", "run", file_path };
    var child = std.process.Child.init(argv, allocator);

    child.stdin_behavior = .Ignore;
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;

    try child.spawn();
    const term = try child.wait();

    switch (term) {
        .Exited => |code| {
            if (code != 0) {
                std.debug.print("Runtime exited with error code: {d}\n", .{code});
                return error.RuntimeError;
            }
        },
        else => {
            std.debug.print("Runtime crashed or was signaled.\n", .{});
            return error.RuntimeCrash;
        },
    }
}

const std = @import("std");
const lexer_mod = @import("lexer.zig");
const parser_mod = @import("parser.zig");
const sema_mod = @import("sema.zig");
const codegen_mod = @import("codegen.zig");

const Lexer = lexer_mod.Lexer;
const Parser = parser_mod.Parser;
const Token = lexer_mod.Token;
const Tag = lexer_mod.Tag;
const SemaAnalyzer = sema_mod.SemaAnalyzer;
const Generator = codegen_mod.Generator;
