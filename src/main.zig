pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    const allocator = arena.allocator();

    const src =
        \\ let main: fn = fn() -> void
        \\    let mut x: number = 1
        \\
        \\    if x > 10
        \\        let mut y: number = 0
        \\    for x > 10
        \\        let z: number = 10
        \\    for
        \\        let q: number = 10
    ;

    var token_list: std.ArrayList(Token) = .empty;
    var lexer = Lexer.init(src);

    while (true) {
        const token = lexer.next();
        try token_list.append(allocator, token);

        //todo: remember currently the lexer keep parsing even an error
        //todo: lexer errors for sure need improvement
        if (token.tag == Tag.eof or token.tag == .err) break;
    }

    std.debug.print("Tokens: {any}\n\n", .{token_list.items});

    var parser = Parser.init(allocator, src, token_list);
    const root = try parser.parse();
    for (root.statements.items) |stmt| {
        std.debug.print("{any}:\n", .{stmt});
    }
}

const std = @import("std");
const lexer_mod = @import("lexer.zig");
const parser_mod = @import("parser.zig");

const Lexer = lexer_mod.Lexer;
const Parser = parser_mod.Parser;
const Token = lexer_mod.Token;
const Tag = lexer_mod.Tag;
