pub const Dusk = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    stdout_writer: std.io.Writer,

    pub fn runFile(self: *Self, file_path: []const u8) !void {
        const compiled_code = try self.compileFile(file_path, null);
        var runtime = try QjsRunTime.init(self.stdout_writer);
        try runtime.eval(compiled_code);
    }

    pub fn compileFile(self: *Self, input_path: []const u8, output_path: ?[]const u8) ![]const u8 {
        const input_file_content: []const u8 = try std.fs.cwd().readFileAlloc(self.allocator, input_path, std.math.maxInt(usize));

        const compiled_code = try self.compile(input_file_content);

        if (output_path) |out| {
            if (std.fs.path.dirname(out)) |dir| {
                try std.fs.cwd().makePath(dir);
            }

            const output_file = try std.fs.cwd().createFile(out, .{});
            defer output_file.close();
            var output_writer = output_file.writer(&.{});
            try output_writer.interface.writeAll(compiled_code);
            try output_writer.interface.flush();
        }

        return compiled_code;
    }

    pub fn compile(self: *Self, src: []const u8) ![]const u8 {
        var dusk_lexer = Lexer.init(self.allocator, src);
        const tokens = try dusk_lexer.list();
        try self.dump(tokens, "build/tokens.json");

        var dusk_parser = Parser.init(self.allocator, src, tokens.items);
        const ast = try dusk_parser.parse();
        try self.dump(ast, "build/ast.json");

        var sema_analyzer = try SemaAnalyzer.init(self.allocator, src, &ast);
        const ir = try sema_analyzer.analyze();
        try self.dump(ir, "build/ir.json");

        var js_code_gen = Generator{ .allocator = self.allocator };
        const compiled_code = try js_code_gen.generate(ir);
        return compiled_code;
    }

    fn dump(self: *Self, obj: anytype, file_name: []const u8) !void {
        const file = try std.fs.cwd().createFile(file_name, .{});
        defer file.close();

        var out: std.io.Writer.Allocating = .init(self.allocator);
        try std.json.Stringify.value(obj, .{ .whitespace = .indent_2 }, &out.writer);

        try file.writeAll(try out.toOwnedSlice());
    }
};

const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const sema = @import("sema.zig");
const codegen = @import("codegen.zig");
const QjsRunTime = @import("runtime.zig").QjsRuntime;

const Lexer = lexer.Lexer;
const Parser = parser.Parser;
const SemaAnalyzer = sema.SemaAnalyzer;
const Generator = codegen.Generator;
