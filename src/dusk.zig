const std = @import("std");
const sema = @import("compiler/sema.zig");
const modules = @import("compiler/modules.zig");
const bc = @import("runtime/bytecode.zig");
const codegen = @import("compiler/codegen.zig");
const vm = @import("runtime/vm.zig");

const SemaAnalyzer = sema.SemaAnalyzer;
const VM = vm.VM;
const CliErrorReporter = @import("cli_error.zig").CliErrorReporter;

pub const prelude_src = @embedFile("runtime/builtins/prelude.dsk");

pub const parse = modules.parse;

pub const EntryFile = struct {
    project_root: []const u8 = ".",
    module_path: []const u8 = "",

    pub fn fromPath(file_path: []const u8) EntryFile {
        return .{
            .project_root = std.fs.path.dirname(file_path) orelse ".",
            .module_path = std.fs.path.stem(file_path),
        };
    }
};

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
        const program = try self.compileEntry(input_file_content, EntryFile.fromPath(file_path));
        try self.runProgram(&program);
    }

    pub fn compile(self: *Self, src: []const u8) !bc.Program {
        return self.compileEntry(src, .{});
    }

    pub fn compileEntry(self: *Self, src: []const u8, entry: EntryFile) !bc.Program {
        var compilation_pipeline_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer compilation_pipeline_arena.deinit();

        const allocator = compilation_pipeline_arena.allocator();

        const prelude_ast = try parse(allocator, prelude_src);
        var module_loader = modules.ModuleLoader.init(allocator, self.io, entry.project_root);
        const loaded_modules = try module_loader.load(entry.module_path, src);

        var sema_analyzer = try SemaAnalyzer.init(allocator);
        const ir = try sema_analyzer.analyze(&prelude_ast, prelude_src, loaded_modules);

        var Bytecodegen = codegen.BytecodeGen.init(self.allocator, &sema_analyzer.type_table);

        return try Bytecodegen.generate(&ir);
    }

    pub fn compileAndRun(self: *Self, src: []const u8) !void {
        const program = try self.compile(src);
        try self.runProgram(&program);
    }

    pub fn runProgram(self: *Self, program: *const bc.Program) !void {
        var v = VM.init(program, self.stdout_writer);
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
