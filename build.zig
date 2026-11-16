const std = @import("std");

pub fn build(b: *std.Build) void {
    // run
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const json_backend = b.option(JsonBackend, "json-backend", "JSON codec implementation for comparison") orelse .native;
    const options = b.addOptions();
    options.addOption(JsonBackend, "json_backend", json_backend);
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
    exe.root_module.addOptions("build_options", options);

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
    tests.root_module.addOptions("build_options", options);

    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_tests.step);

    // clean
    const clean_step = b.step("clean", "Remove .zig-cache and zig-out");
    clean_step.dependOn(&b.addSystemCommand(&.{ "rm", "-rf", ".zig-cache" }).step);
    clean_step.dependOn(&b.addSystemCommand(&.{ "rm", "-rf", "zig-out" }).step);
}

const JsonBackend = enum { native, reflection };

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
}
