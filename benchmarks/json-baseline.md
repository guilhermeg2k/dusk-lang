# JSON codec baseline

Baseline source: `7aa6591` (`json: add codec benchmarks and reflection migration plan`). The measured source tree was clean. This measures the existing compiler-generated JSON codec plans and native conversion engine, before implementing reflection.

The reflection/construction and migration plan is in [reflection-plan.md](../doc/reflection-plan.md). Machine-readable results are in [json-native-baseline.json](history/json-native-baseline.json).

Recorded 2026-11-16 UTC (2026-11-15 local). Machine: AMD Ryzen 5 5600X, Fedora Linux 44, toolchain 0.16.0. ReleaseFast build, 3 warmups and 15 measured runs per workload. Every workload passed the runner's expected-output verification.

| Workload | Repetitions per process | Median | Std. dev. | Min–max | Peak RSS |
| --- | --- | --- | --- | --- | --- |
| Small decode | 100,000 nested records | 206.65 ms | 2.25 ms | 203.97–213.21 ms | 17.97 MiB |
| Bulk decode | 1,000 documents of 256 records | 501.84 ms | 2.55 ms | 497.65–508.80 ms | 17.97 MiB |
| Bulk encode | 1,000 documents of 256 records | 110.50 ms | 1.01 ms | 109.44–112.86 ms | 17.97 MiB |
| Defaults decode | 100,000 records, three omitted fields | 59.08 ms | 0.74 ms | 58.56–61.34 ms | 17.97 MiB |

All four results have `noisy: false` under the runner's 5% relative-standard-deviation threshold. This is a baseline, not evidence of a performance advantage over a future implementation.

Timing includes process startup, source compilation, input setup, conversion and assertions. Bulk decode visits every record; bulk encode checks every complete output string and includes one initial decode. Defaults decode mutates the default array on every iteration. No file IO or per-iteration printing is included. Peak RSS covers the entire process, including compilation, so equal values here do not establish equal codec allocation costs. These cases do not cover every supported type or error path; compatibility remains the responsibility of the JSON tests.

Reproduce:

```
python3 bench.py --json json_decode json_decode_bulk json_encode json_defaults --runs 15 --warmup 3 --save json-native-baseline
```

The completed migration comparison is in [the final results](json-reflection-results.md). Reproduce its reflection configuration against the source baseline:

```
python3 bench.py --json json_decode json_decode_bulk json_encode json_defaults --compare 7aa6591 --build-arg=-Djson-backend=reflection --runs 5 --warmup 1 --save json-reflection-final-comparison
```

Use the same machine and toolchain and verify that the four workload sources and shared model match the baseline. The runner alternates current and baseline binaries. Retain noise flags and investigate repeatable time or memory regressions above 5% before choosing the default implementation.
