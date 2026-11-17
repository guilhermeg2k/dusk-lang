# JSON rerun after reflection optimizations

Measured source `410286a`, following the four optimization commits after `47f50a4`: the copy traversal allocator change, native member lookup, ordinary struct/cell/closure handles and constructibility/append changes. All 548 tests pass for each combination of native/reflection JSON and Debug/ReleaseFast. Both ReleaseFast executables were rebuilt before measuring.

## Reflection before and after

Milliseconds per complete workload process. Earlier results are from the original seven-runtime comparison; this is a historical comparison, with a separate alternating control for encoding below.

| Workload | Earlier reflection | Updated reflection | Observed change | Updated reflection / native |
| --- | ---: | ---: | ---: | ---: |
| Small decode | 14949.62 | 1948.90 | 7.67× faster | 9.68× |
| Bulk decode | 35521.49 | 7912.75 | 4.49× faster | 16.16× |
| Bulk encode | 2658.48 | 2833.85 | +6.6% time | 24.98× |
| Defaults decode | 4630.01 | 306.42 | 15.11× faster | 5.19× |

Decoding improved substantially. Encoding did not improve. A focused alternating comparison against `47f50a4`, both built with `-Djson-backend=reflection`, measured **2665.21 ms before versus 2812.13 ms after (+5.5% time)** over 10 measured samples after 2 warmups. Neither the current result nor its comparison is noisy. This confirms the encoding result separately from the earlier measurement session.

The native backend remains in the same broad performance range. Historical differences should not be attributed to individual commits: all four optimizations were measured together, and no per-operation profile or commit-by-commit experiment was performed.

## Updated seven-runtime comparison

Median milliseconds; lower is faster. The four workloads, inputs, counts and expected checksums are unchanged.

| Implementation | Small decode | Bulk decode | Bulk encode | Defaults decode |
| --- | ---: | ---: | ---: | ---: |
| Dusk native | 201.28 | 489.63 | 113.45 | 59.05 |
| Dusk reflection | 1948.90 | 7912.75 | 2833.85 | 306.42 |
| Go encoding/json | 276.31 | 648.18 | 150.79 | 44.28 |
| Java Jackson | 455.25 | 559.87 | 423.46 | 311.63 |
| Bun JSON | 39.47 | 65.62 | 64.93 | 15.69 |
| Python json | 248.13 | 380.09 | 313.54 | 130.32 |
| Ruby JSON | 228.83 | 301.52 | 151.41 | 179.04 |

Five measured samples follow one warmup per cell, with rotating runtime order. Go, Java and Bun defaults initially exceeded the 5% relative-standard-deviation noise threshold; a separate 20-sample run after 3 warmups resolved all three flags. Their rerun medians appear in the table. All final selected cells have `noisy: false`. Every process passed its expected-output check.

These are whole-program measurements, including startup, fixture setup, assertions, Dusk source compilation and any JVM/JIT startup work. Go and Java compilation and executable builds are excluded. Dusk/Go/Java use typed records; Bun/Python/Ruby use normal dynamic JSON objects. This is not a steady-state server or parser-only benchmark. The full methodology, encoding key-order difference and default semantics are in [the workload README](json_cross/README.md).

Bun remains fastest in these workloads. The updated reflection decoder is much faster than the initial implementation, while remaining slower than native Dusk. The measurements do not isolate VM overhead, reflection API costs, copying or JSON code, and they do not identify which optimization contributed each gain.

## Provenance and retained results

Same machine and dependency versions as [the first report](json-cross-language-results.md): AMD Ryzen 5 5600X, Fedora Linux 44, toolchain 0.16.0; Go 1.26.10-X:nodwarf5, OpenJDK 25.0.5, Bun 1.4.4, Python 3.14.10, Ruby 4.0.9/JSON 2.19.9, Jackson databind/core 2.20.1 and annotations 2.20. Actual versions, commands and source/dependency/binary hashes are captured in the raw files. Benchmark source and dependency hashes match the first run. The noise rerun uses identical binaries. Newer uncommitted reflection/JSON/example edits appeared while the report was being prepared; they are outside these measurements and correctness runs and were left untouched.

- [Updated initial run](history/json-cross-language-410286a.json).
- [Defaults noise rerun](history/json-cross-language-410286a-rerun-defaults.json).
- [Alternating old/new encode control](history/json-reflection-410286a-encode-control.json).
- [Earlier measurements](history/json-cross-language-initial.json).

Child RSS retains the earlier accounting limitation; no memory ranking is drawn.

Reproduce the updated run:

```
python3 benchmarks/json_compare.py --bun /tmp/dusk-json-cross-deps/bun-linux-x64/bun --jackson-dir /tmp/dusk-json-cross-deps --runs 5 --warmup 1 --save json-cross-language-410286a
python3 benchmarks/json_compare.py --bun /tmp/dusk-json-cross-deps/bun-linux-x64/bun --jackson-dir /tmp/dusk-json-cross-deps --runs 20 --warmup 3 --no-build --only go java bun --workloads json_defaults --save json-cross-language-410286a-rerun-defaults
python3 bench.py --json json_encode --compare 47f50a4 --build-arg=-Djson-backend=reflection --base-build-arg=-Djson-backend=reflection --runs 10 --warmup 2 --save json-reflection-410286a-encode-control
```
