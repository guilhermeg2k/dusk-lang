const std = @import("std");
const ast = @import("ast.zig");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const compile_error = @import("compile_error.zig");
const CompileError = compile_error.CompileError;
const CompileErrorReporter = compile_error.CompileErrorReporter;
const std_library = @import("../runtime/builtins/std.zig");
const BindingMap = @import("../runtime/builtins/native.zig").BindingMap;

pub const ModuleLoader = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    io: std.Io,
    project_root: []const u8,
    modules: std.ArrayList(Module) = .empty,
    loaded_paths: std.StringHashMapUnmanaged(usize) = .empty,
    loading_paths: std.ArrayList([]const u8) = .empty,

    pub fn init(allocator: std.mem.Allocator, io: std.Io, project_root: []const u8) Self {
        return .{ .allocator = allocator, .io = io, .project_root = project_root };
    }

    pub fn load(self: *Self, entry_path: []const u8, entry_src: []const u8) CompileError![]const Module {
        const path = try std.fmt.allocPrint(self.allocator, "@/{s}", .{entry_path});
        _ = try self.loadModule(path, entry_src, null, null);
        return self.modules.items;
    }

    fn loadModule(self: *Self, path: []const u8, src: []const u8, file_path: ?[]const u8, bindings: ?*const BindingMap) CompileError!usize {
        const root = try parseFile(self.allocator, src, file_path);
        var reporter = CompileErrorReporter{ .allocator = self.allocator, .src = src, .file_path = file_path };

        try self.loading_paths.append(self.allocator, path);
        var imports: std.ArrayList(Import) = .empty;
        for (root.statements.items) |stmt| {
            if (stmt.data != .import) break;
            const module_index = try self.loadImport(stmt.data.import, &reporter);
            try imports.append(self.allocator, .{ .stmt = stmt.data.import, .module_index = module_index });
        }
        _ = self.loading_paths.pop();

        const index = self.modules.items.len;
        try self.modules.append(self.allocator, .{
            .path = path,
            .file_path = file_path,
            .bindings = bindings,
            .src = src,
            .root = root,
            .imports = try imports.toOwnedSlice(self.allocator),
        });
        try self.loaded_paths.put(self.allocator, path, index);
        return index;
    }

    fn loadImport(self: *Self, import: ast.ImportStmt, reporter: *CompileErrorReporter) CompileError!usize {
        if (import.source == .library) {
            return reporter.librariesNotSupported(import.path, import.path_loc);
        }
        const std_module = if (import.source == .std) std_library.find(import.path) orelse {
            return reporter.unknownStdModule(import.path, import.path_loc);
        } else null;
        const prefix = if (std_module != null) "std/" else "@/";
        const path = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ prefix, import.path });
        if (self.loaded_paths.get(path)) |index| {
            return index;
        }
        if (self.isLoading(path)) {
            return reporter.importCycle(try self.describeCycle(path), import.path_loc);
        }
        if (std_module) |module| {
            const file_path = try std.fmt.allocPrint(self.allocator, "{s}.dsk", .{path});
            return self.loadModule(path, module.source, file_path, module.bindings);
        }
        const file_path = try self.projectFilePath(import.path);
        const src = try self.readProjectFile(path, file_path, import.path_loc, reporter);
        return self.loadModule(path, src, file_path, null);
    }

    fn isLoading(self: *Self, path: []const u8) bool {
        for (self.loading_paths.items) |loading| {
            if (std.mem.eql(u8, loading, path)) return true;
        }
        return false;
    }

    fn describeCycle(self: *Self, path: []const u8) CompileError![]const u8 {
        var chain: std.ArrayList(u8) = .empty;
        var is_in_cycle = false;
        for (self.loading_paths.items) |loading| {
            if (std.mem.eql(u8, loading, path)) {
                is_in_cycle = true;
            }
            if (!is_in_cycle) continue;
            try chain.print(self.allocator, "{s} -> ", .{loading});
        }
        try chain.print(self.allocator, "{s}", .{path});
        return chain.toOwnedSlice(self.allocator);
    }

    fn projectFilePath(self: *Self, path: []const u8) CompileError![]const u8 {
        const file_name = try std.fmt.allocPrint(self.allocator, "{s}.dsk", .{path});
        return std.fs.path.join(self.allocator, &.{ self.project_root, file_name });
    }

    fn readProjectFile(self: *Self, path: []const u8, file_path: []const u8, loc: compile_error.Loc, reporter: *CompileErrorReporter) CompileError![]const u8 {
        return std.Io.Dir.cwd().readFileAlloc(self.io, file_path, self.allocator, .unlimited) catch |failure| {
            if (failure == error.FileNotFound) {
                return reporter.moduleNotFound(path, file_path, loc);
            }
            return reporter.cantReadModule(path, @errorName(failure), loc);
        };
    }
};

pub const Module = struct {
    path: []const u8,
    file_path: ?[]const u8,
    bindings: ?*const BindingMap,
    src: []const u8,
    root: ast.Root,
    imports: []const Import,
};

pub const Import = struct {
    stmt: ast.ImportStmt,
    module_index: usize,
};

pub fn parse(allocator: std.mem.Allocator, src: []const u8) CompileError!ast.Root {
    return parseFile(allocator, src, null);
}

fn parseFile(allocator: std.mem.Allocator, src: []const u8, file_path: ?[]const u8) CompileError!ast.Root {
    var dusk_lexer = Lexer.init(allocator, src);
    dusk_lexer.err_reporter.file_path = file_path;
    const tokens = try dusk_lexer.list();
    var dusk_parser = Parser.init(allocator, src, tokens.items);
    dusk_parser.err_reporter.file_path = file_path;
    return dusk_parser.parse();
}
