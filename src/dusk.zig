pub const Dusk = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    lexer: Lexer,
    parser: Parser,
    analyzer: SemaAnalyzer,
    codegen: Generator,

    pub fn init(allocator: std.mem.Allocator) !Self {
        return .{ .allocator = allocator, .lexer = .{ .allocator = allocator }, .parser = .{ .allocator = allocator }, .analyzer = try SemaAnalyzer.init(allocator, ""), .codegen = .{ .allocator = allocator } };
    }

    pub fn compileAndRunFile(self: *Self, file_path: []const u8) !void {
        const compiled_file_path = try self.compileFile(file_path, null);
        try self.bunRun(compiled_file_path);
    }

    pub fn compileFile(self: *Self, file_path: []const u8, output_path: ?[]const u8) ![]const u8 {
        const input_file_content: []const u8 = try std.fs.cwd().readFileAlloc(self.allocator, file_path, std.math.maxInt(usize));
        const file_name = std.fs.path.stem(file_path);

        const output_file_path = output_path orelse try std.fmt.allocPrint(self.allocator, "build/{s}.js", .{file_name});

        if (std.fs.path.dirname(output_file_path)) |dir| {
            try std.fs.cwd().makePath(dir);
        }

        const output_file = try std.fs.cwd().createFile(output_file_path, .{});
        defer output_file.close();
        var output_writer = output_file.writer(&.{});

        const compiled_code = try self.compile(input_file_content);

        try output_writer.interface.writeAll(compiled_code);
        try output_writer.interface.flush();

        return output_file_path;
    }

    pub fn compile(self: *Self, src: []const u8) ![]const u8 {
        const tokens = try self.lexer.list(src);
        try self.dump(tokens, "build/tokens.json");

        const ast = try self.parser.parse(src, tokens.items);
        try self.dump(ast, "build/ast.json");

        const ir = try self.analyzer.analyze(src, &ast);
        try self.dump(ir, "build/ir.json");

        const compiled_code = try self.codegen.generate(ir);
        return compiled_code;
    }

    pub fn runCaptured(self: *Self, file_path: []const u8) ![]u8 {
        const argv = &[_][]const u8{ "bun", "run", file_path };
        var child = std.process.Child.init(argv, self.allocator);

        var env_map = try std.process.getEnvMap(self.allocator);
        defer env_map.deinit();
        try env_map.put("NO_COLOR", "1");

        child.env_map = &env_map;
        child.stdin_behavior = .Ignore;
        child.stdout_behavior = .Pipe;
        child.stderr_behavior = .Inherit;

        try child.spawn();

        const stdout = try child.stdout.?.readToEndAlloc(self.allocator, 50 * 1024);

        const term = try child.wait();
        switch (term) {
            .Exited => |code| {
                if (code != 0) return error.RuntimeError;
            },
            else => return error.RuntimeCrash,
        }

        return stdout;
    }

    fn bunRun(self: *Self, file_path: []const u8) !void {
        const argv = &[_][]const u8{ "bun", "run", file_path };
        var child = std.process.Child.init(argv, self.allocator);

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

const Lexer = lexer.Lexer;
const Parser = parser.Parser;
const SemaAnalyzer = sema.SemaAnalyzer;
const Generator = codegen.Generator;
