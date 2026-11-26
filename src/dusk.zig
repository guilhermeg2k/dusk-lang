const std = @import("std");
const sema = @import("compiler/sema.zig");
const modules = @import("compiler/modules.zig");
const bc = @import("runtime/bytecode.zig");
const codegen = @import("compiler/codegen.zig");
const vm = @import("runtime/vm.zig");

const SemaAnalyzer = sema.SemaAnalyzer;
const VM = vm.VM;
const Map = @import("runtime/value.zig").Map;
const CliErrorReporter = @import("cli_error.zig").CliErrorReporter;
pub const Stdio = @import("runtime/io/stdio.zig").Stdio;

pub const prelude_src = @embedFile("runtime/builtins/prelude.dsk");

pub const parse = modules.ModuleLoader.parse;

pub fn stdSource(path: []const u8) []const u8 {
    return @import("runtime/builtins/std.zig").find(path).?.source;
}

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
    stdio: Stdio = .{},
    stdin_fd: c_int = 0,
    io: std.Io,
    program_args: []const []const u8 = &.{},
    environment: ?*const std.process.Environ.Map = null,
    exit_code: u8 = 0,

    pub fn runFile(self: *Self, file_path: []const u8) !void {
        self.exit_code = 1;
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
        self.exit_code = 1;
        const program = try self.compile(src);
        try self.runProgram(&program);
    }

    pub fn runProgram(self: *Self, program: *const bc.Program) !void {
        self.exit_code = 1;
        Map.randomizeSeeds(self.io);
        var v = VM.init(program, self.stdio, self.io);
        v.stdin_fd = self.stdin_fd;
        v.program_args = self.program_args;
        v.environment = self.environment;
        defer v.deinit();
        try v.run();
        self.exit_code = v.exit_code;
    }
};
