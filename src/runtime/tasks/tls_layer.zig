const std = @import("std");
const tls = @import("tls.zig");
const TaskState = @import("scheduler.zig").TaskState;
const NetError = @import("tcp.zig").NetError;

pub const cipher_chunk_size = 64 * 1024;

pub const TlsLayer = struct {
    const Self = @This();

    pub const Role = enum { client, server };
    pub const Handshake = enum { pending, running, done, failed };

    allocator: std.mem.Allocator,
    session: *tls.Session,
    owned_config: ?*tls.Config,
    role: Role,
    handshake: Handshake = .pending,
    handshake_failure: NetError = .HandshakeFailed,
    handshake_waiters: std.ArrayList(HandshakeWaiter) = .empty,
    cipher_scratch: [cipher_chunk_size]u8 = undefined,

    pub fn createClient(allocator: std.mem.Allocator, config: *tls.Config, host: []const u8) !*Self {
        const session = tls.dusk_tls_session_new(config, host.ptr, host.len) orelse return error.TlsSessionFailed;
        errdefer tls.dusk_tls_session_free(session);
        const layer = try allocator.create(Self);
        layer.* = .{ .allocator = allocator, .session = session, .owned_config = config, .role = .client };
        return layer;
    }

    pub fn createServer(allocator: std.mem.Allocator, config: *tls.Config) !*Self {
        const session = tls.dusk_tls_session_new(config, null, 0) orelse return error.TlsSessionFailed;
        errdefer tls.dusk_tls_session_free(session);
        const layer = try allocator.create(Self);
        layer.* = .{ .allocator = allocator, .session = session, .owned_config = null, .role = .server };
        return layer;
    }

    pub fn destroy(self: *Self) void {
        for (self.handshake_waiters.items) |waiter| {
            waiter.free(self.allocator);
        }
        self.handshake_waiters.deinit(self.allocator);
        tls.dusk_tls_session_free(self.session);
        if (self.owned_config) |config| {
            tls.dusk_tls_config_free(config);
        }
        self.allocator.destroy(self);
    }

    pub fn isHandshakeOver(self: *const Self) bool {
        return self.handshake == .done or self.handshake == .failed;
    }

    pub fn takeHandshakeWaiters(self: *Self) std.ArrayList(HandshakeWaiter) {
        const waiters = self.handshake_waiters;
        self.handshake_waiters = .empty;
        return waiters;
    }
};

pub const HandshakeWaiter = struct {
    task: *TaskState,
    pending_write: ?[]u8,

    pub fn free(self: HandshakeWaiter, allocator: std.mem.Allocator) void {
        if (self.pending_write) |bytes| {
            allocator.free(bytes);
        }
    }
};

pub const ServerCertificate = struct {
    certificate: *tls.Certificate,
    config: *tls.Config,

    pub fn create(allocator: std.mem.Allocator, cert_pem: []const u8, key_pem: []const u8) CertificateError!*ServerCertificate {
        try ensureTlsReady();
        var status: tls.Status = .done;
        const certificate = tls.dusk_tls_certificate_new(cert_pem.ptr, cert_pem.len, key_pem.ptr, key_pem.len, &status) orelse {
            return if (status == .invalid) error.InvalidCertificate else error.OutOfMemory;
        };
        errdefer tls.dusk_tls_certificate_free(certificate);
        const config = tls.dusk_tls_server_config_new(certificate) orelse return error.OutOfMemory;
        errdefer tls.dusk_tls_config_free(config);
        const server_certificate = try allocator.create(ServerCertificate);
        server_certificate.* = .{ .certificate = certificate, .config = config };
        return server_certificate;
    }

    pub fn destroy(self: *ServerCertificate, allocator: std.mem.Allocator) void {
        tls.dusk_tls_config_free(self.config);
        tls.dusk_tls_certificate_free(self.certificate);
        allocator.destroy(self);
    }
};

pub const CertificateError = error{ InvalidCertificate, OutOfMemory, TlsInitFailed };

pub fn ensureTlsReady() error{TlsInitFailed}!void {
    if (tls.dusk_tls_global_init() != .done) {
        return error.TlsInitFailed;
    }
}

pub fn newClientConfig(should_verify: bool, ca_pem: ?[]const u8) error{ InvalidCertificate, OutOfMemory, TlsInitFailed }!*tls.Config {
    try ensureTlsReady();
    const config = tls.dusk_tls_client_config_new(@intFromBool(should_verify)) orelse return error.OutOfMemory;
    const pem = ca_pem orelse return config;
    const status = tls.dusk_tls_config_add_ca_pem(config, pem.ptr, pem.len);
    if (status != .done) {
        tls.dusk_tls_config_free(config);
        return if (status == .invalid) error.InvalidCertificate else error.OutOfMemory;
    }
    return config;
}

pub fn netErrorFromHandshake(status: tls.Status) NetError {
    return switch (status) {
        .untrusted => .UntrustedCertificate,
        .expired => .ExpiredCertificate,
        .wrong_host => .WrongHostCertificate,
        else => .HandshakeFailed,
    };
}
