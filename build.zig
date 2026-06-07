const std = @import("std");

fn findLibStdCpp(b: *std.Build) []const u8 {
    const paths = [_][]const u8{
        "/lib64/libstdc++.so.6",
        "/usr/lib64/libstdc++.so.6",
        "/usr/lib/x86_64-linux-gnu/libstdc++.so.6",
        "/usr/lib/aarch64-linux-gnu/libstdc++.so.6",
        "/usr/lib/libstdc++.so.6",
    };
    for (paths) |p| {
        std.Io.Dir.accessAbsolute(b.graph.io, p, .{}) catch continue;
        return p;
    }
    @panic("libstdc++.so.6 not found at any known path. Install libstdc++ (e.g. 'dnf install libstdc++-devel' or 'apt install libstdc++-13-dev')");
}

pub fn build(b: *std.Build) void {
    // run
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const exe = b.addExecutable(.{
        .name = "dusk",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    b.installArtifact(exe);
    const run_step = b.step("run", "Run the app");
    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // tests
    const tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/test.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_tests.step);

    // quickjs
    const qjs_dep = b.dependency("quickjs", .{});
    const qjs_path = qjs_dep.path(".");
    const qjs_flags = &[_][]const u8{
        "-D_GNU_SOURCE",
        "-DCONFIG_VERSION=\"2025-09-13\"",
        "-fno-sanitize=undefined",
    };
    const qjs_files = &[_][]const u8{
        "quickjs.c",
        "libregexp.c",
        "libunicode.c",
        "cutils.c",
        "dtoa.c",
    };

    exe.root_module.addCSourceFiles(.{
        .root = qjs_path,
        .files = qjs_files,
        .flags = qjs_flags,
    });
    exe.root_module.link_libc = true;
    exe.root_module.addIncludePath(qjs_path);

    tests.root_module.addCSourceFiles(.{
        .root = qjs_path,
        .files = qjs_files,
        .flags = qjs_flags,
    });
    tests.root_module.link_libc = true;
    tests.root_module.addIncludePath(qjs_path);

    // binaryen
    const binaryen_dep = b.dependency("binaryen", .{});
    const binaryen_include = binaryen_dep.path("include");
    const binaryen_lib = binaryen_dep.path("lib");

    inline for (&[_]*std.Build.Module{ exe.root_module, tests.root_module }) |mod| {
        mod.addIncludePath(binaryen_include);
        mod.addLibraryPath(binaryen_lib);
        mod.linkSystemLibrary("binaryen", .{ .preferred_link_mode = .static });
        mod.addObjectFile(.{ .cwd_relative = findLibStdCpp(b) });
    }

    // wasmtime
    const wasmtime_dep = b.dependency("wasmtime_c_api", .{});
    const wasmtime_include = wasmtime_dep.path("include");
    const wasmtime_lib = wasmtime_dep.path("lib");

    inline for (&[_]*std.Build.Module{ exe.root_module, tests.root_module }) |mod| {
        mod.addIncludePath(wasmtime_include);
        mod.addLibraryPath(wasmtime_lib);
        mod.linkSystemLibrary("wasmtime", .{ .preferred_link_mode = .dynamic });
        mod.linkSystemLibrary("pthread", .{});
        mod.linkSystemLibrary("dl", .{});
        mod.linkSystemLibrary("unwind", .{});
        mod.addCMacro("WASMTIME_FEATURE_WASI", "");
        mod.addCMacro("WASMTIME_FEATURE_COMPILER", "");
        mod.addCMacro("WASMTIME_FEATURE_GC", "");
    }
}
