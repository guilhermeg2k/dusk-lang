const std = @import("std");

pub fn build(b: *std.Build) void {
    // run
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const libuv = b.dependency("libuv", .{ .target = target, .optimize = optimize }).artifact("uv");
    const mbedtls = b.dependency("mbedtls", .{ .target = target, .optimize = optimize }).artifact("mbedtls");
    const llhttp = b.dependency("llhttp", .{ .target = target, .optimize = optimize }).artifact("llhttp");
    const libraries: RuntimeLibraries = .{ .libuv = libuv, .mbedtls = mbedtls, .llhttp = llhttp };

    const exe = b.addExecutable(.{
        .name = "dusk",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    linkRuntimeLibraries(b, exe.root_module, libraries);

    exe.root_module.addAnonymousImport("build_zon", .{
        .root_source_file = b.path("build.zig.zon"),
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
            .link_libc = true,
        }),
    });
    linkRuntimeLibraries(b, tests.root_module, libraries);

    const process_fixture = b.addExecutable(.{
        .name = "dusk-process-fixture",
        .root_module = b.createModule(.{ .target = target, .optimize = optimize, .link_libc = true }),
    });
    process_fixture.root_module.addCSourceFile(.{ .file = b.path("test/fixtures/process/child.c"), .flags = &.{"-std=c99"} });
    const fixture_paths = b.addOptions();
    fixture_paths.addOptionPath("process_fixture", process_fixture.getEmittedBin());
    tests.root_module.addOptions("test_paths", fixture_paths);
    const install_fixture = b.addInstallArtifact(process_fixture, .{});
    const fixture_step = b.step("process-fixture", "Build the process test helper");
    fixture_step.dependOn(&install_fixture.step);

    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_tests.step);

    // clean
    const clean_step = b.step("clean", "Remove .zig-cache and zig-out");
    clean_step.dependOn(&b.addSystemCommand(&.{ "rm", "-rf", ".zig-cache" }).step);
    clean_step.dependOn(&b.addSystemCommand(&.{ "rm", "-rf", "zig-out" }).step);
}

const RuntimeLibraries = struct {
    libuv: *std.Build.Step.Compile,
    mbedtls: *std.Build.Step.Compile,
    llhttp: *std.Build.Step.Compile,
};

fn linkRuntimeLibraries(b: *std.Build, module: *std.Build.Module, libraries: RuntimeLibraries) void {
    module.linkLibrary(libraries.libuv);
    module.linkLibrary(libraries.mbedtls);
    module.linkLibrary(libraries.llhttp);
    const flags = &[_][]const u8{ "-std=c99", "-fno-strict-aliasing" };
    module.addCSourceFile(.{ .file = b.path("src/runtime/tasks/tls_glue.c"), .flags = flags });
    module.addCSourceFile(.{ .file = b.path("src/runtime/tasks/http_glue.c"), .flags = flags });
    module.addCSourceFile(.{ .file = b.path("src/runtime/tasks/process_glue.c"), .flags = flags });
}
