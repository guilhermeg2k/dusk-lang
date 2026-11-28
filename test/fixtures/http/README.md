# HTTP test fixtures

Gzip bodies for `test/success/http-client.dsk`, made with Python's `gzip` module (`mtime=0`, so the files are reproducible):

```sh
python3 - <<'PY'
import gzip
hello = ("hello from gzip\n" * 64).encode()
open("hello.gz", "wb").write(gzip.compress(hello, mtime=0))
open("zeros.gz", "wb").write(gzip.compress(bytes(1 << 20), mtime=0))
broken = bytearray(gzip.compress(hello, mtime=0))
broken[20] ^= 0xFF
open("broken.gz", "wb").write(bytes(broken))
PY
```

- `hello.gz`: `hello from gzip\n` 64 times (1024 bytes).
- `zeros.gz`: 1 MiB of zero bytes, about 1 KiB compressed (checks that `max_body` counts decompressed bytes).
- `broken.gz`: `hello.gz` with one byte of the compressed data flipped.
