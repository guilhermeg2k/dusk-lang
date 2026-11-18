# JSON comparison across runtimes (v0.17)

`std/json` on branch `v017` (`e6f7d14`) against Go, Java, Bun, Python and Ruby, on the same four workloads, sizes and checksums as the first comparison (2026-11-16, branch `reflection`). Fresh-process, end-to-end measurements on one machine.

## Median elapsed time

Milliseconds per complete workload process; lower is faster.

| Implementation | Small decode | Bulk decode | Bulk encode | Defaults decode |
| --- | ---: | ---: | ---: | ---: |
| Dusk native | 214.77 | 526.51 | 112.71 | 58.09 |
| Dusk derived | 1119.34 | 2539.25 | 803.39 | 141.29 |
| Go encoding/json | 276.85 | 642.90 | 149.67 | 44.98 |
| Java Jackson | 449.11 | 553.91 | 409.86 | 300.29 |
| Bun JSON | 40.15 | 66.34 | 63.75 | 15.34 |
| Python json | 244.81 | 372.26 | 305.38 | 125.38 |
| Ruby JSON | 226.97 | 300.42 | 149.10 | 177.29 |

5 measured samples after 1 warmup per cell, starting runtime rotated each round; every cell has `noisy: false` (5% relative-standard-deviation threshold).

- **Dusk native** is what `json::encode` / `json::decode` use for every type except a whole-type `via` at the top or directly as a union member. Its times match the first comparison (211.91 / 505.04 / 113.35 / 59.11 ms) within a few percent, so proxy support and the shared defaults/conversions queue cost nothing on these proxy-free records.
- **Dusk derived** is the compiler-written Dusk code over the module's file-only JSON reader and writer, the path left for those two cases. It replaces "Dusk reflection" from the first comparison (14949.62 / 35521.49 / 2658.48 / 4630.01 ms): 13x, 14x, 3.3x and 33x faster than that, and 5.2x, 4.8x, 7.1x and 2.4x slower than native.
- These workloads have no proxies. A proxy adds one `to` / `from` call per value on top of the native engine.

Workload details, API differences between runtimes and measurement scope are unchanged; see [the workload README](json_cross/README.md).

## Provenance

Measured 2026-11-18. Source commit `e6f7d14` (the tree was dirty only with the then-uncommitted harness files). Machine: AMD Ryzen 5 5600X, Fedora Linux 44, x86_64; Dusk ReleaseFast. Bun 1.4.4, Jackson databind/core 2.20.1 and annotations 2.20; other versions and all source, dependency and binary hashes are in the raw file.

- [Raw results](history/json-cross-language-v017.json)

```
python3 benchmarks/json_compare.py --bun <bun> --jackson-dir <jars> --runs 5 --warmup 1 --save json-cross-language-v017
```
