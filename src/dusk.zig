pub const DuskC = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    lexer: Lexer,
    parser: Parser,
    analyzer: SemaAnalyzer,
    codegen: Generator,

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{ .allocator = allocator, .lexer = .{}, .parser = .{ .allocator = allocator }, .analyzer = SemaAnalyzer.init(allocator), .codegen = .{} };
    }

    pub fn compile(self: *Self, src: []const u8) []const u8 {
        const tokens = try self.lexer.list(src);
        const ast = try self.parser.parse(src, tokens);
        const ir = try self.analyzer.analyze(&ast);
        const compiled_code = try self.codegen.generate(ir);
        return compiled_code;
    }

    pub fn compileFile(self: *Self, file_path: []const u8, output_path: ?[]const u8) !void {
        const input_file_content: []const u8 = try std.fs.cwd().readFileAlloc(self.allocator, file_path, std.math.maxInt(usize));
        const file_name = std.fs.path.stem(file_path);

        const output_file_path = output_path orelse try std.fmt.allocPrint(self.allocator, "build/{s}.js", .{file_name});

        if (std.fs.path.dirname(output_file_path)) |dir| {
            try std.fs.cwd().makePath(dir);
        }

        const output_file = try std.fs.cwd().createFile(output_file_path, .{});
        defer output_file.close();
        const output_writer = output_file.writer(&.{});

        const compiled_code = try self.compile(input_file_content);

        try output_writer.interface.writeAll(compiled_code);
        try output_writer.interface.flush();
    }
};

const std = @import("std");

const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const sema = @import("sema.zig");
const codegen = @import("codegen.zig");

const Lexer = lexer.Lexer;
const Parser = parser.Parser;
const SemaAnalyzer = sema.SemaAnalyzer;
const Generator = codegen.Generator;
