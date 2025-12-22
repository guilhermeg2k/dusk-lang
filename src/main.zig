pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    const allocator = arena.allocator();

    const src = "let mut xyz: number = 100\nif x > 10\n    let mut y: number = 0\n    let x: number = 10\n";

    var token_list: std.ArrayList(Token) = .empty;
    var lexer = Lexer.init(src);

    while (true) {
        const token = lexer.next();
        try token_list.append(allocator, token);

        if (token.tag == Tag.eof) break;
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
