pub const Parser = opaque {};

pub const Status = enum(c_int) {
    done = 0,
    want_more = 1,
    invalid = -1,
    out_of_memory = -2,
    _,
};

pub const offsets_per_header = 4;

pub extern fn dusk_http_response_parser_new(is_head_request: c_int) ?*Parser;
pub extern fn dusk_http_parser_free(parser: *Parser) void;
pub extern fn dusk_http_feed_head(parser: *Parser, data: [*]const u8, length: usize, used: *usize) Status;
pub extern fn dusk_http_feed_body(parser: *Parser, data: [*]const u8, length: usize, out: [*]u8, used: *usize, produced: *usize) Status;
pub extern fn dusk_http_finish_body(parser: *Parser) Status;
pub extern fn dusk_http_status_code(parser: *const Parser) c_int;
pub extern fn dusk_http_reason_span(parser: *const Parser, start: *usize, end: *usize) void;
pub extern fn dusk_http_header_count(parser: *const Parser) usize;
pub extern fn dusk_http_header_offsets(parser: *const Parser) ?[*]const usize;
pub extern fn dusk_http_is_message_complete(parser: *const Parser) c_int;
pub extern fn dusk_http_can_keep_alive(parser: *const Parser) c_int;
pub extern fn dusk_http_error_name(parser: *const Parser) [*:0]const u8;
