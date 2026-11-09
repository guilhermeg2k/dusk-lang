const std = @import("std");

pub fn build(b: *std.Build) void {
    // run
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const libuv = addLibuv(b, target, optimize);
    const uv_module = addUvModule(b, target, optimize);

    const exe = b.addExecutable(.{
        .name = "dusk",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });
    exe.root_module.linkLibrary(libuv);
    exe.root_module.addImport("uv", uv_module);

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
    tests.root_module.linkLibrary(libuv);
    tests.root_module.addImport("uv", uv_module);

    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_tests.step);

    // clean
    const clean_step = b.step("clean", "Remove .zig-cache and zig-out");
    clean_step.dependOn(&b.addSystemCommand(&.{ "rm", "-rf", ".zig-cache" }).step);
    clean_step.dependOn(&b.addSystemCommand(&.{ "rm", "-rf", "zig-out" }).step);
}

const uv_common_sources = [_][]const u8{
    "fs-poll.c",
    "idna.c",
    "inet.c",
    "random.c",
    "strscpy.c",
    "strtok.c",
    "thread-common.c",
    "threadpool.c",
    "timer.c",
    "uv-common.c",
    "uv-data-getter-setters.c",
    "version.c",
};

const uv_unix_sources = [_][]const u8{
    "unix/async.c",
    "unix/core.c",
    "unix/dl.c",
    "unix/fs.c",
    "unix/getaddrinfo.c",
    "unix/getnameinfo.c",
    "unix/loop-watcher.c",
    "unix/loop.c",
    "unix/pipe.c",
    "unix/poll.c",
    "unix/process.c",
    "unix/random-devurandom.c",
    "unix/signal.c",
    "unix/stream.c",
    "unix/tcp.c",
    "unix/thread.c",
    "unix/tty.c",
    "unix/udp.c",
};

const uv_linux_sources = [_][]const u8{
    "unix/proctitle.c",
    "unix/linux.c",
    "unix/procfs-exepath.c",
    "unix/random-getrandom.c",
    "unix/random-sysctl-linux.c",
};

const uv_darwin_sources = [_][]const u8{
    "unix/proctitle.c",
    "unix/bsd-ifaddrs.c",
    "unix/kqueue.c",
    "unix/random-getentropy.c",
    "unix/darwin-proctitle.c",
    "unix/darwin.c",
    "unix/fsevents.c",
};

const uv_windows_sources = [_][]const u8{
    "win/async.c",
    "win/core.c",
    "win/detect-wakeup.c",
    "win/dl.c",
    "win/error.c",
    "win/fs.c",
    "win/fs-event.c",
    "win/getaddrinfo.c",
    "win/getnameinfo.c",
    "win/handle.c",
    "win/loop-watcher.c",
    "win/pipe.c",
    "win/thread.c",
    "win/poll.c",
    "win/process.c",
    "win/process-stdio.c",
    "win/signal.c",
    "win/snprintf.c",
    "win/stream.c",
    "win/tcp.c",
    "win/tty.c",
    "win/udp.c",
    "win/util.c",
    "win/winapi.c",
    "win/winsock.c",
};

const uv_windows_libraries = [_][]const u8{
    "psapi",
    "user32",
    "advapi32",
    "iphlpapi",
    "userenv",
    "ws2_32",
    "dbghelp",
    "ole32",
    "shell32",
    "api-ms-win-core-synch-l1-2-0",
};

fn addLibuv(b: *std.Build, target: std.Build.ResolvedTarget, optimize: std.builtin.OptimizeMode) *std.Build.Step.Compile {
    const upstream = b.dependency("libuv", .{});
    const module = b.createModule(.{
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    module.addIncludePath(upstream.path("include"));
    module.addIncludePath(upstream.path("src"));

    const flags = &[_][]const u8{ "-std=gnu11", "-fno-strict-aliasing", "-Wno-unused-parameter" };
    module.addCSourceFiles(.{ .root = upstream.path("src"), .files = &uv_common_sources, .flags = flags });

    const os = target.result.os.tag;
    switch (os) {
        .windows => {
            module.addCMacro("WIN32_LEAN_AND_MEAN", "");
            module.addCMacro("_WIN32_WINNT", "0x0A00");
            module.addCMacro("_CRT_DECLARE_NONSTDC_NAMES", "0");
            module.addCSourceFiles(.{ .root = upstream.path("src"), .files = &uv_windows_sources, .flags = flags });
            for (uv_windows_libraries) |name| {
                module.linkSystemLibrary(name, .{});
            }
        },
        .linux => {
            addUvUnixSources(module, upstream, flags);
            module.addCMacro("_GNU_SOURCE", "");
            module.addCMacro("_POSIX_C_SOURCE", "200112");
            module.addCSourceFiles(.{ .root = upstream.path("src"), .files = &uv_linux_sources, .flags = flags });
        },
        .macos => {
            addUvUnixSources(module, upstream, flags);
            module.addCMacro("_DARWIN_UNLIMITED_SELECT", "1");
            module.addCMacro("_DARWIN_USE_64_BIT_INODE", "1");
            module.addCSourceFiles(.{ .root = upstream.path("src"), .files = &uv_darwin_sources, .flags = flags });
        },
        else => @panic("libuv: unsupported target OS"),
    }

    return b.addLibrary(.{
        .name = "uv",
        .linkage = .static,
        .root_module = module,
    });
}

fn addUvUnixSources(module: *std.Build.Module, upstream: *std.Build.Dependency, flags: []const []const u8) void {
    module.addCMacro("_FILE_OFFSET_BITS", "64");
    module.addCMacro("_LARGEFILE_SOURCE", "");
    module.addCSourceFiles(.{ .root = upstream.path("src"), .files = &uv_unix_sources, .flags = flags });
}

fn addUvModule(b: *std.Build, target: std.Build.ResolvedTarget, optimize: std.builtin.OptimizeMode) *std.Build.Module {
    const upstream = b.dependency("libuv", .{});
    const translate = b.addTranslateC(.{
        .root_source_file = upstream.path("include/uv.h"),
        .target = target,
        .optimize = optimize,
    });
    translate.addIncludePath(upstream.path("include"));
    return translate.createModule();
}
