# JSON rerun after reflection views and combined setters

Measured source `5ebc550` on 2026-11-17T02:20:52Z. Both native/reflection JSON backends pass all 548 tests in Debug and ReleaseFast. Both ReleaseFast executables were rebuilt from a clean tracked source tree. Only Dusk workloads were executed; external-language measurements below are retained from the first comparison, as requested.

## Current Dusk versus the first external measurements

Median milliseconds per complete workload process; lower is faster.

| Implementation | Small decode | Bulk decode | Encode | Defaults |
| --- | ---: | ---: | ---: | ---: |
| Dusk native, current | 212.17 | 504.07 | 112.05 | 58.56 |
| Dusk reflection, current | 1666.01 | 6357.80 | 2208.40 | 286.63 |
| Go, first run | 287.83 | 650.46 | 152.96 | 45.75 |
| Java/Jackson, first run | 475.28 | 572.06 | 421.98 | 322.61 |
| Bun, first run | 42.02 | 66.80 | 64.45 | 16.23 |
| Python, first run | 252.27 | 383.06 | 314.48 | 130.66 |
| Ruby, first run | 236.22 | 304.30 | 152.34 | 181.06 |

The first Java defaults cell uses its original 20-sample noise rerun, matching the first published report. External runtimes were not rerun. This table combines separate sessions; close differences should not be interpreted as decisive.

## Reflection progression

Median milliseconds. The last column is the reduction in elapsed time relative to `410286a`, rather than the first implementation.

| Workload | First implementation | Previous: 410286a | Current: 5ebc550 | Less time than previous |
| --- | ---: | ---: | ---: | ---: |
| Small decode | 14949.62 | 1948.90 | 1666.01 | 14.5% |
| Bulk decode | 35521.49 | 7912.75 | 6357.80 | 19.7% |
| Bulk encode | 2658.48 | 2833.85 | 2208.40 | 22.1% |
| Defaults decode | 4630.01 | 306.42 | 286.63 | 6.5% |

All four reflection medians improved over the previous measurement session, including encoding. These are historical comparisons, not paired runs isolating an individual change. The commit changes value-view semantics and combines setter checks in native calls; the benchmark does not attribute gains among those changes.

## Verification and scope

- All 548 tests pass for native Debug, native ReleaseFast, reflection Debug and reflection ReleaseFast.
- Five measured samples follow one warmup per Dusk backend/workload. Both defaults cells were initially noisy; a separate 20-sample run after 3 warmups resolved both flags. Rerun defaults medians appear in the tables. All selected final Dusk cells have `noisy: false` under the 5% relative-standard-deviation threshold.
- Every measured process passed its expected-output check. Workload source hashes are unchanged from the first comparison; the defaults rerun uses identical binaries. Raw files retain samples, output checksums, ranges, standard deviations and provenance. Its dirty flag reflects the untracked initial result file.
- Whole-process timing includes startup, Dusk source compilation, input setup, conversion, assertions and checksum output. Executable builds are excluded. The original external-language timing includes runtime/library startup and JVM/JIT initialization. Typed records versus dynamic JSON objects and encoding key-order differences remain as documented in [the workload README](json_cross/README.md).
- Machine and dependencies match the previous runs. Child RSS retains the earlier accounting limitation; no memory ranking is drawn. No steady-state parser, VM-only or reflection-operation cost is isolated by these measurements.

Raw results: [Dusk initial run](history/json-dusk-5ebc550.json), [Dusk defaults rerun](history/json-dusk-5ebc550-rerun-defaults.json), [first external comparison](json-cross-language-results.md), [previous optimization report](json-reflection-optimization-results.md).

Reproduce after building both Dusk binaries in ReleaseFast:

```
python3 benchmarks/json_compare.py --bun /tmp/dusk-json-cross-deps/bun-linux-x64/bun --jackson-dir /tmp/dusk-json-cross-deps --runs 5 --warmup 1 --no-build --only native reflection --save json-dusk-5ebc550
python3 benchmarks/json_compare.py --bun /tmp/dusk-json-cross-deps/bun-linux-x64/bun --jackson-dir /tmp/dusk-json-cross-deps --runs 20 --warmup 3 --no-build --only native reflection --workloads json_defaults --save json-dusk-5ebc550-rerun-defaults
```
