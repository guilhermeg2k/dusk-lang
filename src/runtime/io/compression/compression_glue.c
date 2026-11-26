#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "zlib-ng.h"

#define DUSK_ZIP_OK 0
#define DUSK_ZIP_DONE 1
#define DUSK_ZIP_INVALID -1
#define DUSK_ZIP_OUT_OF_MEMORY -2

#define DUSK_ZIP_GZIP 0
#define DUSK_ZIP_ZLIB 1
#define DUSK_ZIP_DEFLATE 2

#define DUSK_ZIP_NO_FLUSH 0
#define DUSK_ZIP_FLUSH 1
#define DUSK_ZIP_FINISH 2

#define DUSK_ZIP_SLICE ((size_t)1 << 30)

typedef struct {
    zng_stream stream;
    int is_compressor;
} DuskZip;

static int window_bits_of(int format) {
    if (format == DUSK_ZIP_ZLIB) {
        return 15;
    }
    if (format == DUSK_ZIP_DEFLATE) {
        return -15;
    }
    return 31;
}

static int flush_of(int mode) {
    if (mode == DUSK_ZIP_FLUSH) {
        return Z_SYNC_FLUSH;
    }
    if (mode == DUSK_ZIP_FINISH) {
        return Z_FINISH;
    }
    return Z_NO_FLUSH;
}

static size_t smaller(size_t a, size_t b) {
    return a < b ? a : b;
}

DuskZip *dusk_zip_compressor_new(int format, int level) {
    DuskZip *zip = calloc(1, sizeof(DuskZip));
    if (zip == NULL) {
        return NULL;
    }
    if (zng_deflateInit2(&zip->stream, level, Z_DEFLATED, window_bits_of(format), 8, Z_DEFAULT_STRATEGY) != Z_OK) {
        free(zip);
        return NULL;
    }
    zip->is_compressor = 1;
    return zip;
}

DuskZip *dusk_zip_decompressor_new(int format) {
    DuskZip *zip = calloc(1, sizeof(DuskZip));
    if (zip == NULL) {
        return NULL;
    }
    if (zng_inflateInit2(&zip->stream, window_bits_of(format)) != Z_OK) {
        free(zip);
        return NULL;
    }
    return zip;
}

void dusk_zip_free(DuskZip *zip) {
    if (zip == NULL) {
        return;
    }
    if (zip->is_compressor) {
        zng_deflateEnd(&zip->stream);
    } else {
        zng_inflateEnd(&zip->stream);
    }
    free(zip);
}

size_t dusk_zip_bound(DuskZip *zip, size_t length) {
    return zng_deflateBound(&zip->stream, length);
}

int dusk_zip_restart(DuskZip *zip) {
    int status = zip->is_compressor ? zng_deflateReset(&zip->stream) : zng_inflateReset(&zip->stream);
    return status == Z_OK ? DUSK_ZIP_OK : DUSK_ZIP_INVALID;
}

int dusk_zip_compress(DuskZip *zip, const unsigned char *in, size_t in_len, unsigned char *out, size_t out_cap, int mode, size_t *consumed, size_t *produced) {
    int flush = flush_of(mode);
    size_t in_used = 0;
    size_t out_used = 0;
    int result = DUSK_ZIP_OK;
    while (1) {
        size_t in_piece = smaller(in_len - in_used, DUSK_ZIP_SLICE);
        size_t out_piece = smaller(out_cap - out_used, DUSK_ZIP_SLICE);
        int is_last_input = in_used + in_piece == in_len;
        zip->stream.next_in = in + in_used;
        zip->stream.avail_in = (uint32_t)in_piece;
        zip->stream.next_out = out + out_used;
        zip->stream.avail_out = (uint32_t)out_piece;
        int status = zng_deflate(&zip->stream, is_last_input ? flush : Z_NO_FLUSH);
        in_used += in_piece - zip->stream.avail_in;
        out_used += out_piece - zip->stream.avail_out;
        if (status == Z_STREAM_END) {
            result = DUSK_ZIP_DONE;
            break;
        }
        if (status == Z_STREAM_ERROR) {
            result = DUSK_ZIP_INVALID;
            break;
        }
        if (out_used == out_cap) {
            break;
        }
        if (in_used < in_len) {
            continue;
        }
        int is_flush_complete = flush == Z_NO_FLUSH || zip->stream.avail_out != 0;
        if (is_flush_complete) {
            break;
        }
    }
    *consumed = in_used;
    *produced = out_used;
    return result;
}

int dusk_zip_decompress(DuskZip *zip, const unsigned char *in, size_t in_len, unsigned char *out, size_t out_cap, size_t *consumed, size_t *produced) {
    size_t in_used = 0;
    size_t out_used = 0;
    int result = DUSK_ZIP_OK;
    while (1) {
        size_t in_piece = smaller(in_len - in_used, DUSK_ZIP_SLICE);
        size_t out_piece = smaller(out_cap - out_used, DUSK_ZIP_SLICE);
        zip->stream.next_in = in + in_used;
        zip->stream.avail_in = (uint32_t)in_piece;
        zip->stream.next_out = out + out_used;
        zip->stream.avail_out = (uint32_t)out_piece;
        int status = zng_inflate(&zip->stream, Z_NO_FLUSH);
        size_t took = in_piece - zip->stream.avail_in;
        size_t gave = out_piece - zip->stream.avail_out;
        in_used += took;
        out_used += gave;
        if (status == Z_STREAM_END) {
            result = DUSK_ZIP_DONE;
            break;
        }
        if (status == Z_MEM_ERROR) {
            result = DUSK_ZIP_OUT_OF_MEMORY;
            break;
        }
        int is_bad_data = status == Z_DATA_ERROR || status == Z_NEED_DICT || status == Z_STREAM_ERROR;
        if (is_bad_data) {
            result = DUSK_ZIP_INVALID;
            break;
        }
        int is_stuck = took == 0 && gave == 0;
        int is_done_for_now = out_used == out_cap || in_used == in_len || is_stuck;
        if (is_done_for_now) {
            break;
        }
    }
    *consumed = in_used;
    *produced = out_used;
    return result;
}
