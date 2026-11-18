# JSON comparison workloads

These programs reproduce the four existing Dusk JSON workloads without changing their inputs, counts or expected checksums. Run the comparison with `benchmarks/json_compare.py` from the repository root. It builds Dusk in ReleaseFast and runs each workload twice: `json_*.dsk` (native engine) and `json_dusk_*.dsk` (the derived Dusk code, reached through a box type with a whole-type `via`), compiles Go and Java outside the timed interval, checks every process output and rotates the starting runtime each round. Each workload result is checkpointed to `benchmarks/history/NAME.json` with individual samples, source/dependency/binary hashes, versions, commands, ranges, standard deviation and noise flags.

Requirements: the repository build toolchain, Go, a JDK, Bun, Python and Ruby with its JSON library. The measured dependency set is Bun 1.4.4, Jackson databind/core 2.20.1 and annotations 2.20. Dependencies are external artifacts and are not vendored.

For example, download the pinned dependencies into a temporary directory:

```
mkdir -p /tmp/dusk-json-cross-deps
curl -fL https://github.com/oven-sh/bun/releases/download/bun-v1.4.4/bun-linux-x64.zip -o /tmp/dusk-json-cross-deps/bun.zip
unzip -o /tmp/dusk-json-cross-deps/bun.zip -d /tmp/dusk-json-cross-deps
curl -fL https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-databind/2.20.1/jackson-databind-2.20.1.jar -o /tmp/dusk-json-cross-deps/jackson-databind.jar
curl -fL https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-core/2.20.1/jackson-core-2.20.1.jar -o /tmp/dusk-json-cross-deps/jackson-core.jar
curl -fL https://repo.maven.apache.org/maven2/com/fasterxml/jackson/core/jackson-annotations/2.20/jackson-annotations-2.20.jar -o /tmp/dusk-json-cross-deps/jackson-annotations.jar
python3 benchmarks/json_compare.py --bun /tmp/dusk-json-cross-deps/bun-linux-x64/bun --jackson-dir /tmp/dusk-json-cross-deps --runs 5 --warmup 1
```

Use `--no-build` to reuse the previously built binaries, `--only` to select runtimes, `--workloads` to select cases, and `--save NAME` to retain a separate rerun. Keep the initial noisy measurements as well as reruns. Do not run other CPU-heavy work concurrently.

## Equivalent work and limits

- Small decode: 100,000 records; escaped name, null, boolean and floating-point field assertions; checksum `5200000`.
- Bulk decode: 1,000 documents of 256 records, visiting every record; checksum `15104000`.
- Bulk encode: 1,000 documents of 256 records, checking each complete output string; checksum `1000`.
- Defaults decode: 100,000 records with three omitted fields and a fresh mutable array default, appending to that array each time; checksum `4700000`.

Dusk, Go and Java decode to typed records. Bun, Python and Ruby decode to normal dynamic objects/maps. Go uses encoding/json, Java uses a reused Jackson ObjectMapper, and the dynamic runtimes use their standard JSON APIs. Go supplies defaults before unmarshalling, Java uses field initializers, and the dynamic programs apply missing-field defaults after decoding. These are idiomatic implementations of this fixture rather than identical validation or missing-field contracts.

Compact encoding preserves the same values and includes nulls. Go's map encoder sorts keys, so its exact-output assertion expects likes before views; the other implementations match the original fixture byte for byte. The encoding setup generates and validates one expected output before the measured loop, and therefore includes one additional encode in each foreign implementation.

Each sample starts a fresh process. Timing includes runtime/library startup, input construction, JSON conversion, assertions and checksums. Dusk also compiles its source at startup. Go native compilation and Java class compilation are excluded. JVM/JIT warmup within a process is included; external warmups do not create a persistent warmed JVM. This measures complete programs, not steady-state parser throughput. Peak RSS is process-wide and includes runtime/compiler memory. No conclusion about reflection alone follows from this comparison.
