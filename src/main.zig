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
        \\    return a - b
    ;

    var token_list: std.ArrayList(Token) = .empty;
    var lexer = Lexer.init(src);

    while (true) {
        const token = lexer.next();
        try token_list.append(allocator, token);

        //todo: remember currently the lexer keep parsing even an error
        //todo: lexer errors for sure needs improvement
        if (token.tag == Tag.eof or token.tag == .err) break;
    }

    try dump(allocator, token_list.items, "tokens_dump.json");

    var parser = Parser.init(allocator, src, token_list);
    const ast = try parser.parse();

    try dump(allocator, ast, "ast_dump.json");

    var sema = try SemaAnalyzer.init(allocator);
    _ = try sema.analyze(&ast);
}

fn dump(allocator: std.mem.Allocator, obj: anytype, file_name: []const u8) !void {
    const file = try std.fs.cwd().createFile(file_name, .{});
    defer file.close();

    var out: std.io.Writer.Allocating = .init(allocator);
    try std.json.Stringify.value(obj, .{ .whitespace = .indent_2 }, &out.writer);

    try file.writeAll(try out.toOwnedSlice());
}

const std = @import("std");
const lexer_mod = @import("lexer.zig");
const parser_mod = @import("parser.zig");
const sema_mod = @import("sema.zig");

const Lexer = lexer_mod.Lexer;
const Parser = parser_mod.Parser;
const Token = lexer_mod.Token;
const Tag = lexer_mod.Tag;
const SemaAnalyzer = sema_mod.SemaAnalyzer;
