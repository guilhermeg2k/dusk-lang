pub const Certificate = opaque {};
pub const Config = opaque {};
pub const Session = opaque {};

pub const Status = enum(c_int) {
    done = 0,
    want_more = 1,
    failed = -1,
    untrusted = -2,
    expired = -3,
    wrong_host = -4,
    out_of_memory = -5,
    invalid = -6,
    end = -7,
    _,
};

pub extern fn dusk_tls_global_init() Status;

pub extern fn dusk_tls_certificate_new(cert_pem: [*]const u8, cert_length: usize, key_pem: [*]const u8, key_length: usize, status: *Status) ?*Certificate;
pub extern fn dusk_tls_certificate_free(certificate: *Certificate) void;

pub extern fn dusk_tls_client_config_new(should_verify: c_int) ?*Config;
pub extern fn dusk_tls_server_config_new(certificate: *Certificate) ?*Config;
pub extern fn dusk_tls_config_add_ca_pem(config: *Config, pem: [*]const u8, length: usize) Status;
pub extern fn dusk_tls_config_add_ca_der(config: *Config, der: [*]const u8, length: usize) Status;
pub extern fn dusk_tls_config_free(config: *Config) void;

pub extern fn dusk_tls_session_new(config: *Config, host: ?[*]const u8, host_length: usize) ?*Session;
pub extern fn dusk_tls_session_free(session: *Session) void;
pub extern fn dusk_tls_feed(session: *Session, data: [*]const u8, length: usize) Status;
pub extern fn dusk_tls_output_length(session: *const Session) usize;
pub extern fn dusk_tls_output_bytes(session: *const Session) [*]const u8;
pub extern fn dusk_tls_consume_output(session: *Session, count: usize) void;
pub extern fn dusk_tls_handshake(session: *Session) Status;
pub extern fn dusk_tls_read(session: *Session, buffer: [*]u8, capacity: usize) isize;
pub extern fn dusk_tls_write(session: *Session, data: [*]const u8, length: usize) isize;
pub extern fn dusk_tls_close_notify(session: *Session) Status;
