const std = @import("std");

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
    const unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/test.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);

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

    exe.addCSourceFiles(.{
        .root = qjs_path,
        .files = qjs_files,
        .flags = qjs_flags,
    });
    exe.root_module.link_libc = true;
    exe.addIncludePath(qjs_path);

    unit_tests.addCSourceFiles(.{
        .root = qjs_path,
        .files = qjs_files,
        .flags = qjs_flags,
    });
    unit_tests.root_module.link_libc = true;
    unit_tests.addIncludePath(qjs_path);
}
