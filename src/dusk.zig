const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const sema = @import("sema.zig");
const bc = @import("bytecode.zig");
const vm = @import("vm.zig");
const builtin = @import("built-in.zig");

const Lexer = lexer.Lexer;
const Parser = parser.Parser;
const SemaAnalyzer = sema.SemaAnalyzer;
const VM = vm.VM;

pub const Dusk = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    stdout_writer: ?*std.Io.Writer,
    io: std.Io,

    pub fn runFile(self: *Self, file_path: []const u8) !void {
        const cwd = std.Io.Dir.cwd();
        const input_file_content = try cwd.readFileAlloc(self.io, file_path, self.allocator, .unlimited);
        defer self.allocator.free(input_file_content);
        _ = try self.compileAndRun(input_file_content);
    }

    pub fn compile(self: *Self, src: []const u8) !bc.Program {
        var dusk_lexer = Lexer.init(self.allocator, src);
        const tokens = try dusk_lexer.list();

        var dusk_parser = Parser.init(self.allocator, src, tokens.items);
        const ast = try dusk_parser.parse();

        var sema_analyzer = try SemaAnalyzer.init(self.allocator);
        const ir = try sema_analyzer.analyze(&ast, src);

        var Bytecodegen = bc.BytecodeGen.init(self.allocator, &sema_analyzer.type_table);
        return try Bytecodegen.generate(&ir, &builtin.BuiltIn.getBytecodeFunctions());
    }

    pub fn compileAndRun(self: *Self, src: []const u8) !void {
        const program = try self.compile(src);

        var v = VM.init(self.allocator, &program);
        if (self.stdout_writer) |writer| {
            builtin.BuiltIn.setEchoWriter(writer);
        }
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
