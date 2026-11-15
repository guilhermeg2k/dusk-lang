const std = @import("std");

pub fn build(b: *std.Build) void {
    // run
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const libuv = addLibuv(b, target, optimize);
    const mbedtls = addMbedtls(b, target, optimize);

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
    exe.root_module.linkLibrary(mbedtls);

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
    tests.root_module.linkLibrary(mbedtls);

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

const mbedtls_tls_sources = [_][]const u8{
    "debug.c",
    "error.c",
    "mbedtls_config.c",
    "mps_reader.c",
    "mps_trace.c",
    "pkcs7.c",
    "ssl_cache.c",
    "ssl_ciphersuites.c",
    "ssl_client.c",
    "ssl_cookie.c",
    "ssl_debug_helpers_generated.c",
    "ssl_msg.c",
    "ssl_ticket.c",
    "ssl_tls.c",
    "ssl_tls12_client.c",
    "ssl_tls12_server.c",
    "ssl_tls13_client.c",
    "ssl_tls13_generic.c",
    "ssl_tls13_keys.c",
    "ssl_tls13_server.c",
    "timing.c",
    "version.c",
    "version_features.c",
    "x509.c",
    "x509_create.c",
    "x509_crl.c",
    "x509_crt.c",
    "x509_csr.c",
    "x509_oid.c",
    "x509write.c",
    "x509write_crt.c",
    "x509write_csr.c",
};

const mbedtls_crypto_core_sources = [_][]const u8{
    "psa_crypto.c",
    "psa_crypto_client.c",
    "psa_crypto_driver_wrappers_no_static.c",
    "psa_crypto_random.c",
    "psa_crypto_slot_management.c",
    "psa_crypto_storage.c",
    "psa_its_file.c",
    "psa_util.c",
    "tf_psa_crypto_config.c",
    "tf_psa_crypto_version.c",
};

const mbedtls_crypto_builtin_sources = [_][]const u8{
    "aes.c",
    "aesce.c",
    "aesni.c",
    "aria.c",
    "bignum.c",
    "bignum_core.c",
    "bignum_mod.c",
    "bignum_mod_raw.c",
    "block_cipher.c",
    "camellia.c",
    "ccm.c",
    "chacha20.c",
    "chacha20_neon.c",
    "chachapoly.c",
    "cipher.c",
    "cipher_wrap.c",
    "cmac.c",
    "ctr_drbg.c",
    "ecdsa.c",
    "ecjpake.c",
    "ecp.c",
    "ecp_curves.c",
    "ecp_curves_new.c",
    "entropy.c",
    "entropy_poll.c",
    "gcm.c",
    "hmac_drbg.c",
    "md5.c",
    "poly1305.c",
    "psa_crypto_aead.c",
    "psa_crypto_cipher.c",
    "psa_crypto_ecp.c",
    "psa_crypto_ffdh.c",
    "psa_crypto_hash.c",
    "psa_crypto_mac.c",
    "psa_crypto_pake.c",
    "psa_crypto_rsa.c",
    "psa_crypto_xof.c",
    "psa_util_internal.c",
    "ripemd160.c",
    "rsa.c",
    "rsa_alt_helpers.c",
    "sha1.c",
    "sha256.c",
    "sha3.c",
    "sha512.c",
};

const mbedtls_crypto_extras_sources = [_][]const u8{
    "lmots.c",
    "lms.c",
    "md.c",
    "nist_kw.c",
    "pk.c",
    "pk_ecc.c",
    "pk_rsa.c",
    "pk_wrap.c",
    "pkparse.c",
    "pkwrite.c",
};

const mbedtls_crypto_platform_sources = [_][]const u8{
    "memory_buffer_alloc.c",
    "platform.c",
    "platform_util.c",
    "threading.c",
};

const mbedtls_crypto_utilities_sources = [_][]const u8{
    "asn1parse.c",
    "asn1write.c",
    "base64.c",
    "constant_time.c",
    "oid.c",
    "pem.c",
    "pkcs5.c",
};
const mbedtls_source_folders = [_]struct { folder: []const u8, files: []const []const u8 }{
    .{ .folder = "library", .files = &mbedtls_tls_sources },
    .{ .folder = "tf-psa-crypto/core", .files = &mbedtls_crypto_core_sources },
    .{ .folder = "tf-psa-crypto/drivers/builtin/src", .files = &mbedtls_crypto_builtin_sources },
    .{ .folder = "tf-psa-crypto/extras", .files = &mbedtls_crypto_extras_sources },
    .{ .folder = "tf-psa-crypto/platform", .files = &mbedtls_crypto_platform_sources },
    .{ .folder = "tf-psa-crypto/utilities", .files = &mbedtls_crypto_utilities_sources },
};

const mbedtls_include_folders = [_][]const u8{
    "include",
    "library",
    "tf-psa-crypto/include",
    "tf-psa-crypto/core",
    "tf-psa-crypto/dispatch",
    "tf-psa-crypto/drivers/builtin/include",
    "tf-psa-crypto/drivers/builtin/src",
    "tf-psa-crypto/extras",
    "tf-psa-crypto/platform",
    "tf-psa-crypto/utilities",
};

fn addMbedtls(b: *std.Build, target: std.Build.ResolvedTarget, optimize: std.builtin.OptimizeMode) *std.Build.Step.Compile {
    const module = b.createModule(.{
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    for (mbedtls_include_folders) |folder| {
        module.addIncludePath(b.path(b.fmt("libs/mbedtls/{s}", .{folder})));
    }
    const flags = &[_][]const u8{ "-std=c99", "-fno-strict-aliasing" };
    for (mbedtls_source_folders) |group| {
        module.addCSourceFiles(.{ .root = b.path(b.fmt("libs/mbedtls/{s}", .{group.folder})), .files = group.files, .flags = flags });
    }
    module.addCSourceFile(.{ .file = b.path("src/runtime/tasks/tls_glue.c"), .flags = flags });
    if (target.result.os.tag == .windows) {
        module.linkSystemLibrary("bcrypt", .{});
    }

    return b.addLibrary(.{
        .name = "mbedtls",
        .linkage = .static,
        .root_module = module,
    });
}
