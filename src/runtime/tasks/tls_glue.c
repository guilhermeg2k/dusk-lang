#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "mbedtls/pk.h"
#include "mbedtls/ssl.h"
#include "mbedtls/ssl_ticket.h"
#include "mbedtls/x509_crt.h"
#include "psa/crypto.h"

#define DUSK_TLS_DONE 0
#define DUSK_TLS_WANT_MORE 1
#define DUSK_TLS_FAILED -1
#define DUSK_TLS_UNTRUSTED -2
#define DUSK_TLS_EXPIRED -3
#define DUSK_TLS_WRONG_HOST -4
#define DUSK_TLS_OUT_OF_MEMORY -5
#define DUSK_TLS_INVALID -6
#define DUSK_TLS_END -7

#define DUSK_TLS_FIRST_QUEUE_CAPACITY 4096
#define DUSK_TLS_MAX_IDLE_RETRIES 8
#define DUSK_TLS_TICKET_LIFETIME_SECONDS 86400

typedef struct {
    unsigned char *bytes;
    size_t length;
    size_t capacity;
    size_t offset;
} ByteQueue;

typedef struct {
    mbedtls_x509_crt chain;
    mbedtls_pk_context key;
} DuskTlsCertificate;

typedef struct {
    mbedtls_ssl_config ssl;
    mbedtls_x509_crt ca;
    mbedtls_ssl_ticket_context ticket;
} DuskTlsConfig;

typedef struct {
    mbedtls_ssl_context ssl;
    ByteQueue input;
    ByteQueue output;
} DuskTlsSession;

static int byte_queue_append(ByteQueue *queue, const unsigned char *data, size_t length) {
    if (queue->offset > 0) {
        size_t remaining = queue->length - queue->offset;
        memmove(queue->bytes, queue->bytes + queue->offset, remaining);
        queue->length = remaining;
        queue->offset = 0;
    }
    size_t needed = queue->length + length;
    if (needed > queue->capacity) {
        size_t capacity = queue->capacity == 0 ? DUSK_TLS_FIRST_QUEUE_CAPACITY : queue->capacity;
        while (capacity < needed) {
            capacity *= 2;
        }
        unsigned char *bytes = realloc(queue->bytes, capacity);
        if (bytes == NULL) {
            return DUSK_TLS_OUT_OF_MEMORY;
        }
        queue->bytes = bytes;
        queue->capacity = capacity;
    }
    memcpy(queue->bytes + queue->length, data, length);
    queue->length = needed;
    return DUSK_TLS_DONE;
}

static size_t byte_queue_available(const ByteQueue *queue) {
    return queue->length - queue->offset;
}

static void byte_queue_consume(ByteQueue *queue, size_t count) {
    queue->offset += count;
    if (queue->offset == queue->length) {
        queue->offset = 0;
        queue->length = 0;
    }
}

static unsigned char *copy_with_terminator(const unsigned char *text, size_t length) {
    unsigned char *copy = malloc(length + 1);
    if (copy == NULL) {
        return NULL;
    }
    memcpy(copy, text, length);
    copy[length] = 0;
    return copy;
}

static int parse_pem_certificates(mbedtls_x509_crt *chain, const unsigned char *pem, size_t length) {
    unsigned char *text = copy_with_terminator(pem, length);
    if (text == NULL) {
        return DUSK_TLS_OUT_OF_MEMORY;
    }
    int status = mbedtls_x509_crt_parse(chain, text, length + 1);
    free(text);
    return status == 0 ? DUSK_TLS_DONE : DUSK_TLS_INVALID;
}

static int parse_pem_key(mbedtls_pk_context *key, const unsigned char *pem, size_t length) {
    unsigned char *text = copy_with_terminator(pem, length);
    if (text == NULL) {
        return DUSK_TLS_OUT_OF_MEMORY;
    }
    int status = mbedtls_pk_parse_key(key, text, length + 1, NULL, 0);
    free(text);
    return status == 0 ? DUSK_TLS_DONE : DUSK_TLS_INVALID;
}

int dusk_tls_global_init(void) {
    static int is_ready = 0;
    if (is_ready) {
        return DUSK_TLS_DONE;
    }
    if (psa_crypto_init() != PSA_SUCCESS) {
        return DUSK_TLS_FAILED;
    }
    is_ready = 1;
    return DUSK_TLS_DONE;
}

void dusk_tls_certificate_free(DuskTlsCertificate *certificate) {
    mbedtls_x509_crt_free(&certificate->chain);
    mbedtls_pk_free(&certificate->key);
    free(certificate);
}

DuskTlsCertificate *dusk_tls_certificate_new(const unsigned char *cert_pem, size_t cert_length, const unsigned char *key_pem, size_t key_length, int *status) {
    DuskTlsCertificate *certificate = calloc(1, sizeof *certificate);
    if (certificate == NULL) {
        *status = DUSK_TLS_OUT_OF_MEMORY;
        return NULL;
    }
    mbedtls_x509_crt_init(&certificate->chain);
    mbedtls_pk_init(&certificate->key);
    *status = parse_pem_certificates(&certificate->chain, cert_pem, cert_length);
    if (*status == DUSK_TLS_DONE) {
        *status = parse_pem_key(&certificate->key, key_pem, key_length);
    }
    if (*status != DUSK_TLS_DONE) {
        dusk_tls_certificate_free(certificate);
        return NULL;
    }
    if (mbedtls_pk_check_pair(&certificate->chain.pk, &certificate->key) != 0) {
        *status = DUSK_TLS_INVALID;
        dusk_tls_certificate_free(certificate);
        return NULL;
    }
    return certificate;
}

void dusk_tls_config_free(DuskTlsConfig *config) {
    mbedtls_ssl_config_free(&config->ssl);
    mbedtls_x509_crt_free(&config->ca);
    mbedtls_ssl_ticket_free(&config->ticket);
    free(config);
}

static DuskTlsConfig *new_config(int endpoint) {
    DuskTlsConfig *config = calloc(1, sizeof *config);
    if (config == NULL) {
        return NULL;
    }
    mbedtls_ssl_config_init(&config->ssl);
    mbedtls_x509_crt_init(&config->ca);
    mbedtls_ssl_ticket_init(&config->ticket);
    if (mbedtls_ssl_config_defaults(&config->ssl, endpoint, MBEDTLS_SSL_TRANSPORT_STREAM, MBEDTLS_SSL_PRESET_DEFAULT) != 0) {
        dusk_tls_config_free(config);
        return NULL;
    }
    return config;
}

DuskTlsConfig *dusk_tls_client_config_new(int should_verify) {
    DuskTlsConfig *config = new_config(MBEDTLS_SSL_IS_CLIENT);
    if (config == NULL) {
        return NULL;
    }
    mbedtls_ssl_conf_authmode(&config->ssl, should_verify ? MBEDTLS_SSL_VERIFY_REQUIRED : MBEDTLS_SSL_VERIFY_NONE);
    mbedtls_ssl_conf_ca_chain(&config->ssl, &config->ca, NULL);
    return config;
}

DuskTlsConfig *dusk_tls_server_config_new(DuskTlsCertificate *certificate) {
    DuskTlsConfig *config = new_config(MBEDTLS_SSL_IS_SERVER);
    if (config == NULL) {
        return NULL;
    }
    mbedtls_ssl_conf_authmode(&config->ssl, MBEDTLS_SSL_VERIFY_NONE);
    if (mbedtls_ssl_conf_own_cert(&config->ssl, &certificate->chain, &certificate->key) != 0) {
        dusk_tls_config_free(config);
        return NULL;
    }
    if (mbedtls_ssl_ticket_setup(&config->ticket, PSA_ALG_GCM, PSA_KEY_TYPE_AES, 256, DUSK_TLS_TICKET_LIFETIME_SECONDS) != 0) {
        dusk_tls_config_free(config);
        return NULL;
    }
    mbedtls_ssl_conf_session_tickets_cb(&config->ssl, mbedtls_ssl_ticket_write, mbedtls_ssl_ticket_parse, &config->ticket);
    return config;
}

int dusk_tls_config_add_ca_pem(DuskTlsConfig *config, const unsigned char *pem, size_t length) {
    return parse_pem_certificates(&config->ca, pem, length);
}

int dusk_tls_config_add_ca_der(DuskTlsConfig *config, const unsigned char *der, size_t length) {
    return mbedtls_x509_crt_parse_der(&config->ca, der, length) == 0 ? DUSK_TLS_DONE : DUSK_TLS_INVALID;
}

static int send_to_output(void *context, const unsigned char *data, size_t length) {
    DuskTlsSession *session = context;
    if (byte_queue_append(&session->output, data, length) != DUSK_TLS_DONE) {
        return MBEDTLS_ERR_SSL_ALLOC_FAILED;
    }
    return (int)length;
}

static int receive_from_input(void *context, unsigned char *buffer, size_t capacity) {
    DuskTlsSession *session = context;
    size_t available = byte_queue_available(&session->input);
    if (available == 0) {
        return MBEDTLS_ERR_SSL_WANT_READ;
    }
    size_t count = available < capacity ? available : capacity;
    memcpy(buffer, session->input.bytes + session->input.offset, count);
    byte_queue_consume(&session->input, count);
    return (int)count;
}

void dusk_tls_session_free(DuskTlsSession *session) {
    mbedtls_ssl_free(&session->ssl);
    free(session->input.bytes);
    free(session->output.bytes);
    free(session);
}

DuskTlsSession *dusk_tls_session_new(DuskTlsConfig *config, const char *host, size_t host_length) {
    DuskTlsSession *session = calloc(1, sizeof *session);
    if (session == NULL) {
        return NULL;
    }
    mbedtls_ssl_init(&session->ssl);
    if (mbedtls_ssl_setup(&session->ssl, &config->ssl) != 0) {
        dusk_tls_session_free(session);
        return NULL;
    }
    if (host != NULL) {
        char *host_name = (char *)copy_with_terminator((const unsigned char *)host, host_length);
        int status = host_name == NULL ? -1 : mbedtls_ssl_set_hostname(&session->ssl, host_name);
        free(host_name);
        if (status != 0) {
            dusk_tls_session_free(session);
            return NULL;
        }
    }
    mbedtls_ssl_set_bio(&session->ssl, session, send_to_output, receive_from_input, NULL);
    return session;
}

int dusk_tls_feed(DuskTlsSession *session, const unsigned char *data, size_t length) {
    return byte_queue_append(&session->input, data, length);
}

size_t dusk_tls_output_length(const DuskTlsSession *session) {
    return byte_queue_available(&session->output);
}

const unsigned char *dusk_tls_output_bytes(const DuskTlsSession *session) {
    return session->output.bytes + session->output.offset;
}

void dusk_tls_consume_output(DuskTlsSession *session, size_t count) {
    byte_queue_consume(&session->output, count);
}

static int is_waiting(int status) {
    return status == MBEDTLS_ERR_SSL_WANT_READ || status == MBEDTLS_ERR_SSL_WANT_WRITE;
}

static int has_unread_input(DuskTlsSession *session) {
    return byte_queue_available(&session->input) > 0 || mbedtls_ssl_check_pending(&session->ssl);
}

static int should_retry(DuskTlsSession *session, int status, size_t input_before, int *idle_retries) {
    if (!is_waiting(status) || !has_unread_input(session)) {
        return 0;
    }
    if (byte_queue_available(&session->input) != input_before) {
        *idle_retries = 0;
        return 1;
    }
    *idle_retries += 1;
    return *idle_retries <= DUSK_TLS_MAX_IDLE_RETRIES;
}

static int verify_failure(const DuskTlsSession *session) {
    uint32_t flags = mbedtls_ssl_get_verify_result(&session->ssl);
    if (flags & MBEDTLS_X509_BADCERT_EXPIRED) {
        return DUSK_TLS_EXPIRED;
    }
    if (flags & MBEDTLS_X509_BADCERT_CN_MISMATCH) {
        return DUSK_TLS_WRONG_HOST;
    }
    return DUSK_TLS_UNTRUSTED;
}

int dusk_tls_handshake(DuskTlsSession *session) {
    int idle_retries = 0;
    int status;
    size_t input_before;
    do {
        input_before = byte_queue_available(&session->input);
        status = mbedtls_ssl_handshake(&session->ssl);
    } while (should_retry(session, status, input_before, &idle_retries));
    if (status == 0) {
        return DUSK_TLS_DONE;
    }
    if (is_waiting(status)) {
        return DUSK_TLS_WANT_MORE;
    }
    if (status == MBEDTLS_ERR_X509_CERT_VERIFY_FAILED) {
        return verify_failure(session);
    }
    return DUSK_TLS_FAILED;
}

ptrdiff_t dusk_tls_read(DuskTlsSession *session, unsigned char *buffer, size_t capacity) {
    int idle_retries = 0;
    for (;;) {
        size_t input_before = byte_queue_available(&session->input);
        int status = mbedtls_ssl_read(&session->ssl, buffer, capacity);
        if (status > 0) {
            return status;
        }
        if (status == MBEDTLS_ERR_SSL_RECEIVED_NEW_SESSION_TICKET) {
            continue;
        }
        if (should_retry(session, status, input_before, &idle_retries)) {
            continue;
        }
        if (is_waiting(status)) {
            return 0;
        }
        if (status == 0 || status == MBEDTLS_ERR_SSL_PEER_CLOSE_NOTIFY) {
            return DUSK_TLS_END;
        }
        return DUSK_TLS_FAILED;
    }
}

ptrdiff_t dusk_tls_write(DuskTlsSession *session, const unsigned char *data, size_t length) {
    size_t written = 0;
    while (written < length) {
        int status = mbedtls_ssl_write(&session->ssl, data + written, length - written);
        if (status <= 0) {
            return DUSK_TLS_FAILED;
        }
        written += (size_t)status;
    }
    return (ptrdiff_t)written;
}

int dusk_tls_close_notify(DuskTlsSession *session) {
    return mbedtls_ssl_close_notify(&session->ssl) == 0 ? DUSK_TLS_DONE : DUSK_TLS_FAILED;
}
