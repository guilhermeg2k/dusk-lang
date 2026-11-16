# JSON reflection results

Native streaming remains the default JSON backend. Both backends now use the shared reflection type graph and general binding policies; JSON-specific compiler checks, codec generation and program tables have been removed. The public reflection backend works, but its measured cost is too high to replace native conversion.

## Final comparison

Finished source: `13f65af`. Historical baseline: `7aa6591`. Both measurements started with clean tracked source trees. Each comparison alternates current and baseline binaries on the same machine. The percentage and ratio columns use the baseline measured alongside that backend, rather than the earlier standalone baseline.

| Workload | Shared-metadata native median | Change against baseline | Reflection median | Reflection / baseline |
| --- | --- | --- | --- | --- |
| Small decode | 208.47 ms | +0.4% | 14419.07 ms | 70.0× |
| Bulk decode | 504.04 ms | +0.1% | 35075.97 ms | 70.3× |
| Bulk encode | 111.29 ms | +0.1% | 2604.31 ms | 23.5× |
| Defaults decode | 58.51 ms | +1.0% | 4481.25 ms | 76.8× |

The native changes are all below the runner's 5% threshold. Every final workload and comparison has `noisy: false`. The reflection results establish a substantial regression for this implementation and these workloads; they do not establish an inherent lower bound for reflection.

Raw results include medians, standard deviations, ranges, peak RSS, settings, noise flags and provenance:

- [Final native control](history/json-native-shared-metadata-control.json): 15 measured runs after 3 warmups, default backend.
- [Final reflection comparison](history/json-reflection-final-comparison.json): 5 measured runs after 1 warmup, `-Djson-backend=reflection`.
- [Same-source pilot](history/json-reflection-pilot.json): source `eebfa08`, reflection versus native from that same commit, 3 measured runs after 1 warmup. It found a 23–77× gap without noise flags. This predates the final privacy and metadata changes.
- [Original standalone baseline](json-baseline.md), with its [raw results](history/json-native-baseline.json).

The planned 30-sample candidate run was reduced to 5 confirmation samples because a bulk candidate process takes about 35 seconds and the pilot gap was large. No final noise flags required a longer rerun. The final native control separately checks changes since the historical baseline; the same-source pilot isolates the earlier backend change.

## Scope and interpretation

Machine: AMD Ryzen 5 5600X, Fedora Linux 44, toolchain 0.16.0. Measurements were recorded on 2026-11-16 UTC using ReleaseFast builds. All four workload files and their shared model are unchanged from `7aa6591`.

Small decode converts 100,000 nested records. Bulk decode converts 1,000 documents of 256 records. Bulk encode writes 1,000 documents of 256 records and checks every complete output string. Defaults decode converts 100,000 records with three omitted fields and mutates the default array each time. All expected-output checks pass.

Timing covers process startup, compilation, fixture setup, conversion and assertions. It is end-to-end time, rather than parser-only throughput. Peak RSS covers the whole process, including compilation; it does not isolate codec allocation costs.

The reflection candidate retains native lexical scanning and scalar formatting. It batches tokens, traverses type descriptions, crosses Dusk/native call boundaries and copies snapshots and typed results. All those costs are included. These measurements do not attribute time among them; profiling and reducing repeated calls/copies would be separate work before reconsidering the default.

The retained fast path uses the same program reflection descriptors and common field-default callbacks. Adding another format no longer requires compiler support: [the independent decoder](../examples/reflection/record.dsk) uses only public APIs, exercised by [the runnable example](../examples/reflection-decode.dsk). See [the reflection API](../doc/language-ref.md#stdreflect) and [implementation plan](../doc/reflection-plan.md).

## Compatibility verification

The final source passes 549/549 tests in each of four configurations: Debug and ReleaseFast with native and reflection JSON. Both backends also build for the native target, macOS aarch64 and Windows x86_64-gnu. The tests cover JSON errors and diagnostics, defaults, parked callbacks, GC, reflection construction and protected-handle visibility. Documentation examples and the independent decoder execute successfully.

## Reproduction

Check out `13f65af` with the same machine and toolchain, then run sequentially:

```
python3 bench.py --json json_decode json_decode_bulk json_encode json_defaults --compare 7aa6591 --runs 15 --warmup 3 --save json-native-shared-metadata-control
python3 bench.py --json json_decode json_decode_bulk json_encode json_defaults --compare 7aa6591 --build-arg=-Djson-backend=reflection --runs 5 --warmup 1 --save json-reflection-final-comparison
```

The opt-in backend is available with `zig build -Djson-backend=reflection`. Ordinary builds retain native streaming. Both preserve the existing JSON API; the ordinary Dusk encode implementation additionally supports generic function values with a complete expected signature.
