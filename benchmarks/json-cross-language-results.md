# JSON comparison across runtimes

Both Dusk JSON backends were measured against Go, Java, Bun, Python and Ruby using the four unchanged JSON workload sizes and checksums. These are fresh-process, end-to-end measurements on one machine, rather than a general ranking of languages or a measurement of reflection alone.

## Median elapsed time

Milliseconds per complete workload process; lower is faster.

| Implementation | Small decode | Bulk decode | Bulk encode | Defaults decode |
| --- | ---: | ---: | ---: | ---: |
| Dusk native | 211.91 | 505.04 | 113.35 | 59.11 |
| Dusk reflection | 14949.62 | 35521.49 | 2658.48 | 4630.01 |
| Go encoding/json | 287.83 | 650.46 | 152.96 | 45.75 |
| Java Jackson | 475.28 | 572.06 | 421.98 | 322.61 |
| Bun JSON | 42.02 | 66.80 | 64.45 | 16.23 |
| Python json | 252.27 | 383.06 | 314.48 | 130.66 |
| Ruby JSON | 236.22 | 304.30 | 152.34 | 181.06 |

All final selected cells have `noisy: false` under the runner’s 5% relative-standard-deviation threshold.

Native Dusk and reflection Dusk use the same source and workload fixtures, with different build selections. Native Dusk retains direct conversion using shared type metadata; reflection Dusk uses the public reflection APIs and checked builders, with native scanning/scalar helpers. The large gap persists against the external implementations, but this comparison does not separate VM execution cost from reflection calls, checks, allocations or copies.

## Workload and API differences

- Small decode: 100,000 nested records, checking escaped strings, nullable, boolean and floating-point values; checksum `5200000`.
- Bulk decode: 1,000 documents of 256 records, visiting every record; checksum `15104000`.
- Bulk encode: 1,000 documents of 256 records, checking every complete output string; checksum `1000`.
- Defaults decode: 100,000 records with three omitted fields, creating a fresh default array and appending to it each iteration; checksum `4700000`.

Dusk, Go and Java decode into typed records. Bun, Python and Ruby decode into standard dynamic objects/maps. Go uses encoding/json, Java uses a reused Jackson ObjectMapper, Bun uses JSON.parse/stringify, Python uses json, and Ruby uses JSON. Default application follows each implementation's ordinary facilities; this does not reproduce all of Dusk's strict typing, diagnostic and deferred-default contracts.

Go sorts map keys when encoding, so its complete-output check expects the same values with likes before views. Other output strings match the original fixture byte for byte. The foreign programs perform one extra setup encode to generate and validate their expected output, included in process time. No input, iteration count or Dusk workload was reduced to accommodate reflection.

Every timed process is checked for successful exit and the exact expected checksum. The fixtures cover successful conversions, not error-path performance or full cross-runtime semantic compatibility.

## Measurement scope

Timing includes process startup, runtime/library initialization, input setup, JSON conversion, assertions and checksums. Dusk source compilation is included. Building the Dusk executable, compiling Go, and compiling Java classes are excluded. Each Java sample starts a new JVM; JIT compilation during its workload is included. External warmup processes do not create a persistent warmed JVM. This is not a steady-state server benchmark or parser-only throughput test.

Runs execute sequentially. The starting runtime rotates each round to reduce fixed-order effects. Five measured samples and one external warmup were used initially. Only Java defaults was flagged as noisy; a separate 20-sample rerun after 3 warmups resolved that flag. Its median replaces the initial cell in the table, and both files are retained. Individual samples, ranges, standard deviations, process RSS and output checksums are stored in the raw files. Peak RSS is process-wide, including runtime/compiler memory.

Raw child RSS values show an apparent common floor of about 35 MiB across several implementations. Child resource accounting may include the inherited process footprint before execution, so no memory ranking is drawn from these values.

## Provenance and raw results

Measured 2026-11-16 UTC. Benchmark source commit: `5a60d7b`. Dusk runtime code is unchanged from `13f65af`. Initial source tree was clean. The Java rerun reports a dirty tree because the initial raw result was untracked; source and binary hashes match the initial run. Machine: AMD Ryzen 5 5600X, Fedora Linux 44, x86_64; Dusk toolchain 0.16.0. All Dusk executables use ReleaseFast. Versions and dependency/binary/source hashes are also captured in the raw files.

- go: `go version go1.26.10-X:nodwarf5 linux/amd64`
- java: `openjdk version "25.0.5" 2026-10-20`
- bun: `1.4.4`
- python: `Python 3.14.10`
- ruby: `ruby 4.0.9 (2026-10-23 revision 3e93b23afc) +PRISM [x86_64-linux]`
- ruby_json: `2.19.9`
- jackson: `databind/core 2.20.1; annotations 2.20`

- [Initial seven-runtime measurements](history/json-cross-language-initial.json): 5 measured samples after 1 warmup per runtime/workload.
- [json-cross-language-rerun-java-defaults](history/json-cross-language-rerun-java-defaults.json): 20 measured samples after 3 warmups; selected runtimes and cases are recorded in the file.

Sources, pinned dependency setup and rerun commands are documented in [the workload README](json_cross/README.md). The runner is [json_compare.py](json_compare.py). Example:

```
python3 benchmarks/json_compare.py --bun /tmp/dusk-json-cross-deps/bun-linux-x64/bun --jackson-dir /tmp/dusk-json-cross-deps --runs 5 --warmup 1 --save json-cross-language-initial
```

Dependency references: [Go encoding/json](https://pkg.go.dev/encoding/json), [Jackson databind](https://github.com/FasterXML/jackson-databind), [Bun](https://bun.sh/docs), [Python json](https://docs.python.org/3/library/json.html), [Ruby JSON](https://ruby.github.io/json/).
