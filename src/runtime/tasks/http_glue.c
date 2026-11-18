#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "llhttp.h"

#define DUSK_HTTP_DONE 0
#define DUSK_HTTP_WANT_MORE 1
#define DUSK_HTTP_INVALID -1
#define DUSK_HTTP_OUT_OF_MEMORY -2

#define DUSK_HTTP_FIRST_HEADER_CAPACITY 16
#define DUSK_HTTP_OFFSETS_PER_HEADER 4

typedef struct {
    size_t start;
    size_t end;
    int is_open;
} Span;

typedef struct {
    llhttp_t parser;
    llhttp_settings_t settings;
    const char *chunk;
    size_t chunk_base;
    size_t fed;
    Span status;
    Span url;
    Span field;
    Span value;
    size_t *header_offsets;
    size_t header_count;
    size_t header_capacity;
    unsigned char *body_out;
    size_t body_produced;
    int is_head_request;
    int is_informational;
    int is_head_complete;
    int is_message_complete;
    int is_paused;
    int is_paused_for_upgrade;
    int can_keep_alive;
    int has_failed_allocation;
} DuskHttpParser;

static DuskHttpParser *parser_of(llhttp_t *parser) {
    return (DuskHttpParser *)parser->data;
}

static size_t offset_of(DuskHttpParser *self, const char *at) {
    return self->chunk_base + (size_t)(at - self->chunk);
}

static void extend_span(DuskHttpParser *self, Span *span, const char *at, size_t length) {
    size_t start = offset_of(self, at);
    if (!span->is_open) {
        span->start = start;
        span->is_open = 1;
    }
    span->end = start + length;
}

static int on_message_begin(llhttp_t *parser) {
    DuskHttpParser *self = parser_of(parser);
    self->header_count = 0;
    self->status = (Span){0};
    self->url = (Span){0};
    self->field = (Span){0};
    self->value = (Span){0};
    return 0;
}

static int on_status(llhttp_t *parser, const char *at, size_t length) {
    DuskHttpParser *self = parser_of(parser);
    extend_span(self, &self->status, at, length);
    return 0;
}

static int on_url(llhttp_t *parser, const char *at, size_t length) {
    DuskHttpParser *self = parser_of(parser);
    extend_span(self, &self->url, at, length);
    return 0;
}

static int on_header_field(llhttp_t *parser, const char *at, size_t length) {
    DuskHttpParser *self = parser_of(parser);
    extend_span(self, &self->field, at, length);
    return 0;
}

static int on_header_value(llhttp_t *parser, const char *at, size_t length) {
    DuskHttpParser *self = parser_of(parser);
    extend_span(self, &self->value, at, length);
    return 0;
}

static int append_header(DuskHttpParser *self) {
    if (self->header_count == self->header_capacity) {
        size_t capacity = self->header_capacity == 0 ? DUSK_HTTP_FIRST_HEADER_CAPACITY : self->header_capacity * 2;
        size_t *offsets = realloc(self->header_offsets, capacity * DUSK_HTTP_OFFSETS_PER_HEADER * sizeof(size_t));
        if (offsets == NULL) {
            return -1;
        }
        self->header_offsets = offsets;
        self->header_capacity = capacity;
    }
    size_t *entry = self->header_offsets + self->header_count * DUSK_HTTP_OFFSETS_PER_HEADER;
    entry[0] = self->field.start;
    entry[1] = self->field.end;
    entry[2] = self->value.is_open ? self->value.start : self->field.end;
    entry[3] = self->value.is_open ? self->value.end : self->field.end;
    self->header_count += 1;
    return 0;
}

static int on_header_value_complete(llhttp_t *parser) {
    DuskHttpParser *self = parser_of(parser);
    if (append_header(self) != 0) {
        self->has_failed_allocation = 1;
        return -1;
    }
    self->field = (Span){0};
    self->value = (Span){0};
    return 0;
}

static int on_headers_complete(llhttp_t *parser) {
    DuskHttpParser *self = parser_of(parser);
    int status = llhttp_get_status_code(parser);
    int is_informational = status >= 100 && status < 200 && status != 101;
    if (is_informational) {
        self->is_informational = 1;
        return 0;
    }
    self->is_head_complete = 1;
    if (self->is_head_request) {
        return 1;
    }
    return HPE_PAUSED;
}

static int on_body(llhttp_t *parser, const char *at, size_t length) {
    DuskHttpParser *self = parser_of(parser);
    memcpy(self->body_out + self->body_produced, at, length);
    self->body_produced += length;
    return 0;
}

static int on_message_complete(llhttp_t *parser) {
    DuskHttpParser *self = parser_of(parser);
    if (self->is_informational) {
        self->is_informational = 0;
        return 0;
    }
    self->is_message_complete = 1;
    self->can_keep_alive = llhttp_should_keep_alive(parser);
    return HPE_PAUSED;
}

static DuskHttpParser *new_parser(llhttp_type_t type) {
    DuskHttpParser *self = calloc(1, sizeof(DuskHttpParser));
    if (self == NULL) {
        return NULL;
    }
    llhttp_settings_init(&self->settings);
    self->settings.on_message_begin = on_message_begin;
    self->settings.on_status = on_status;
    self->settings.on_url = on_url;
    self->settings.on_header_field = on_header_field;
    self->settings.on_header_value = on_header_value;
    self->settings.on_header_value_complete = on_header_value_complete;
    self->settings.on_headers_complete = on_headers_complete;
    self->settings.on_body = on_body;
    self->settings.on_message_complete = on_message_complete;
    llhttp_init(&self->parser, type, &self->settings);
    self->parser.data = self;
    return self;
}

DuskHttpParser *dusk_http_response_parser_new(int is_head_request) {
    DuskHttpParser *self = new_parser(HTTP_RESPONSE);
    if (self == NULL) {
        return NULL;
    }
    self->is_head_request = is_head_request;
    return self;
}

DuskHttpParser *dusk_http_request_parser_new(void) {
    return new_parser(HTTP_REQUEST);
}

void dusk_http_parser_free(DuskHttpParser *self) {
    free(self->header_offsets);
    free(self);
}

static void resume_parser(DuskHttpParser *self) {
    if (self->is_paused_for_upgrade) {
        llhttp_resume_after_upgrade(&self->parser);
    } else if (self->is_paused) {
        llhttp_resume(&self->parser);
    }
    self->is_paused = 0;
    self->is_paused_for_upgrade = 0;
}

static int run_parser(DuskHttpParser *self, const char *data, size_t length, size_t *used) {
    self->chunk = data;
    self->chunk_base = self->fed;
    resume_parser(self);
    llhttp_errno_t result = llhttp_execute(&self->parser, data, length);
    int is_pause = result == HPE_PAUSED || result == HPE_PAUSED_UPGRADE;
    if (is_pause) {
        self->is_paused = 1;
        self->is_paused_for_upgrade = result == HPE_PAUSED_UPGRADE;
        *used = (size_t)(llhttp_get_error_pos(&self->parser) - data);
        self->fed += *used;
        return DUSK_HTTP_DONE;
    }
    if (result != HPE_OK) {
        *used = 0;
        return self->has_failed_allocation ? DUSK_HTTP_OUT_OF_MEMORY : DUSK_HTTP_INVALID;
    }
    *used = length;
    self->fed += length;
    return DUSK_HTTP_WANT_MORE;
}

int dusk_http_feed_head(DuskHttpParser *self, const unsigned char *data, size_t length, size_t *used) {
    if (self->is_head_complete) {
        *used = 0;
        return DUSK_HTTP_DONE;
    }
    int status = run_parser(self, (const char *)data, length, used);
    if (status == DUSK_HTTP_DONE && !self->is_head_complete) {
        return DUSK_HTTP_INVALID;
    }
    return status;
}

int dusk_http_feed_body(DuskHttpParser *self, const unsigned char *data, size_t length, unsigned char *out, size_t *used, size_t *produced) {
    *used = 0;
    *produced = 0;
    if (self->is_message_complete) {
        return DUSK_HTTP_DONE;
    }
    if (!self->is_head_complete) {
        return DUSK_HTTP_INVALID;
    }
    self->body_out = out;
    self->body_produced = 0;
    int status = run_parser(self, (const char *)data, length, used);
    *produced = self->body_produced;
    self->body_out = NULL;
    if (status == DUSK_HTTP_DONE && !self->is_message_complete) {
        return DUSK_HTTP_INVALID;
    }
    return status;
}

int dusk_http_finish_body(DuskHttpParser *self) {
    if (self->is_message_complete) {
        return DUSK_HTTP_DONE;
    }
    if (!self->is_head_complete) {
        return DUSK_HTTP_INVALID;
    }
    resume_parser(self);
    llhttp_errno_t result = llhttp_finish(&self->parser);
    int has_finished = result == HPE_OK || result == HPE_PAUSED;
    if (!has_finished || !self->is_message_complete) {
        return DUSK_HTTP_INVALID;
    }
    self->can_keep_alive = 0;
    return DUSK_HTTP_DONE;
}

int dusk_http_status_code(const DuskHttpParser *self) {
    return self->parser.status_code;
}

void dusk_http_reason_span(const DuskHttpParser *self, size_t *start, size_t *end) {
    *start = self->status.start;
    *end = self->status.is_open ? self->status.end : self->status.start;
}

void dusk_http_url_span(const DuskHttpParser *self, size_t *start, size_t *end) {
    *start = self->url.start;
    *end = self->url.is_open ? self->url.end : self->url.start;
}

const char *dusk_http_method_name(const DuskHttpParser *self) {
    return llhttp_method_name((llhttp_method_t)self->parser.method);
}

int dusk_http_version(const DuskHttpParser *self) {
    return self->parser.http_major * 10 + self->parser.http_minor;
}

int dusk_http_head_keeps_alive(const DuskHttpParser *self) {
    return llhttp_should_keep_alive(&self->parser);
}

size_t dusk_http_header_count(const DuskHttpParser *self) {
    return self->header_count;
}

const size_t *dusk_http_header_offsets(const DuskHttpParser *self) {
    return self->header_offsets;
}

int dusk_http_is_message_complete(const DuskHttpParser *self) {
    return self->is_message_complete;
}

int dusk_http_can_keep_alive(const DuskHttpParser *self) {
    return self->can_keep_alive;
}
