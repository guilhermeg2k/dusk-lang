# JSON comparison across runtimes (v0.17)

`std/json` on branch `v017` (`9da8734`) against Go, Java, Bun, Python and Ruby, on the same four workloads, sizes and checksums as the earlier comparisons. Fresh-process, end-to-end measurements on one machine.

## Median elapsed time

Milliseconds per complete workload process; lower is faster.

| Implementation | Small decode | Bulk decode | Bulk encode | Defaults decode |
| --- | ---: | ---: | ---: | ---: |
| Dusk | 86.68 | 198.57 | 82.12 | 35.06 |
| Go encoding/json | 279.73 | 652.27 | 150.13 | 44.16 |
| Java Jackson | 460.22 | 565.07 | 413.27 | 308.30 |
| Bun JSON | 39.85 | 66.28 | 63.85 | 15.52 |
| Python json | 241.06 | 390.94 | 306.30 | 130.15 |
| Ruby JSON | 228.09 | 305.39 | 149.33 | 178.51 |

5 measured samples after 1 warmup per cell, starting runtime rotated each round; every cell has `noisy: false` (5% relative-standard-deviation threshold).

- Dusk is the fastest of the typed decoders and encoders (Go, Java) on all four workloads, and faster than Python and Ruby. Bun is faster everywhere: it builds plain objects without checking them against a type, which the other five don't skip.
- Against the previous comparison (2026-11-18, `e6f7d14`: 214.77 / 526.51 / 112.71 / 58.09 ms), Dusk is 2.5x, 2.7x, 1.4x and 1.7x faster. Decoding got its own lexer, token slices, a bitset for seen fields, a guess of the next field and literal defaults stored in the plan; encoding writes JSON bytes directly; a decode that queued no deferred work skips that loop.
- `json::encode` / `json::decode` run in the native engine for every type. These workloads have no proxies; a proxy adds one `to` / `from` call per value.
- The other runtimes' times match the previous comparison within a few percent, so the gains come from Dusk, not the machine.

Workload details, API differences between runtimes and measurement scope are unchanged; see [the workload README](json_cross/README.md).

## Provenance

Measured 2026-11-20. Source commit `9da8734`; the tree was dirty only with an unrelated deleted example (`examples/time.dsk`). Machine: AMD Ryzen 5 5600X, Fedora Linux 44, x86_64; Dusk ReleaseFast. Bun 1.4.4, Jackson databind/core 2.20.1 and annotations 2.20 (same jar hashes as the previous run); other versions and all source, dependency and binary hashes are in the raw file.

- [Raw results](history/json-cross-language-v017-2.json)
- [Previous raw results (2026-11-18)](history/json-cross-language-v017.json)

```
python3 benchmarks/json_compare.py --bun <bun> --jackson-dir <jars> --runs 5 --warmup 1 --save json-cross-language-v017-2
```
