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
        }),
    });

    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_tests.step);

    // clean
    const clean_step = b.step("clean", "Remove .zig-cache and zig-out");
    clean_step.dependOn(&b.addSystemCommand(&.{ "rm", "-rf", ".zig-cache" }).step);
    clean_step.dependOn(&b.addSystemCommand(&.{ "rm", "-rf", "zig-out" }).step);
}
