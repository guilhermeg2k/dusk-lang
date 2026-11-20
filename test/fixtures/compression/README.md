# Compression test fixtures

Inputs for `test/success/compression.dsk`, made with Python's `gzip` and `zlib` modules (`mtime=0`, so the files are reproducible):

```sh
python3 - <<'PY'
import gzip, zlib
sample = "".join(f"line {i}: the quick brown fox jumps over the lazy dog\n" for i in range(200)).encode()
open("sample.txt", "wb").write(sample)
open("sample.gz", "wb").write(gzip.compress(sample, mtime=0))
open("sample.zlib", "wb").write(zlib.compress(sample, 9))
raw = zlib.compressobj(9, zlib.DEFLATED, -15)
open("sample.deflate", "wb").write(raw.compress(sample) + raw.flush())
half = len(sample) // 2
open("members.gz", "wb").write(gzip.compress(sample[:half], mtime=0) + gzip.compress(sample[half:], mtime=0))
broken = bytearray(gzip.compress(sample, mtime=0))
broken[-6] ^= 0xFF
open("bad-checksum.gz", "wb").write(bytes(broken))
PY
```

- `sample.txt`: 200 numbered lines (10690 bytes), the plain text the others decode to.
- `sample.gz`, `sample.zlib`, `sample.deflate`: `sample.txt` in each format.
- `members.gz`: two gzip members back to back, each holding half of `sample.txt`.
- `bad-checksum.gz`: `sample.gz` with one byte of the CRC-32 flipped.
