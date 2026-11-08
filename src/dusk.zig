const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const sema = @import("sema.zig");
const bc = @import("bytecode.zig");
const vm = @import("vm.zig");

const Lexer = lexer.Lexer;
const Parser = parser.Parser;
const SemaAnalyzer = sema.SemaAnalyzer;
const VM = vm.VM;
const CliErrorReporter = @import("error.zig").CliErrorReporter;

pub const prelude_src = @embedFile("prelude.dsk");

pub fn parse(allocator: std.mem.Allocator, src: []const u8) !@import("ast.zig").Root {
    var dusk_lexer = Lexer.init(allocator, src);
    const tokens = try dusk_lexer.list();
    var dusk_parser = Parser.init(allocator, src, tokens.items);
    return dusk_parser.parse();
}

pub const Dusk = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    stdout_writer: ?*std.Io.Writer,
    io: std.Io,

    pub fn runFile(self: *Self, file_path: []const u8) !void {
        const cwd = std.Io.Dir.cwd();
        const input_file_content = cwd.readFileAlloc(self.io, file_path, self.allocator, .unlimited) catch |failure| {
            return CliErrorReporter.cantReadFile(file_path, failure);
        };
        defer self.allocator.free(input_file_content);
        _ = try self.compileAndRun(input_file_content);
    }

    pub fn compile(self: *Self, src: []const u8) !bc.Program {
        var compilation_pipeline_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer compilation_pipeline_arena.deinit();

        const allocator = compilation_pipeline_arena.allocator();

        const prelude_ast = try parse(allocator, prelude_src);
        const ast = try parse(allocator, src);

        var sema_analyzer = try SemaAnalyzer.init(allocator);
        const ir = try sema_analyzer.analyze(&prelude_ast, prelude_src, &ast, src);

        var Bytecodegen = bc.BytecodeGen.init(self.allocator, &sema_analyzer.type_table);

        return try Bytecodegen.generate(&ir);
    }

    pub fn compileAndRun(self: *Self, src: []const u8) !void {
        const program = try self.compile(src);
        var v = VM.init(&program, self.stdout_writer);
        defer v.deinit();
        try v.run();
    }

    fn dump(self: *Self, obj: anytype, file_name: []const u8) !void {
        const dir_path = std.fs.path.dirname(file_name) orelse ".";
        try std.Io.Dir.cwd().createDirPath(self.io, dir_path);

        const json_str = try std.json.Stringify.valueAlloc(self.allocator, obj, .{ .whitespace = .indent_2 });
        defer self.allocator.free(json_str);

        try std.Io.Dir.cwd().writeFile(self.io, .{ .sub_path = file_name, .data = json_str });
    }
};
