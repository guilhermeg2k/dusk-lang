# Benchmark

Cross-language micro-benchmarks tracking **Dusk** against a native **Zig** baseline and five mainstream runtimes. Wall time is measured externally with [`hyperfine`](https://github.com/sharkdp/hyperfine) and peak memory with GNU `/usr/bin/time`; the benchmark programs contain **no timing or instrumentation code**.
> This benchmark is not evidence that X is faster than Y, its main purpose is to track Dusk performance specially on versions change, so we can notice performance problems/gains.

Regenerate this file with:

```bash
python3 run_benchmarks.py
```

> **Note:** This benchmark generation script and the algorithms were AI-generated. I haven't deeply reviewed them, so they may contain errors.

## Machine

| Field | Value |
| --- | --- |
| OS | Fedora Linux 44 (Workstation Edition) |
| Kernel | Linux 7.2.13-200.fc44.x86_64 |
| Architecture | x86_64 |
| CPU | AMD Ryzen 5 5600X 6-Core Processor |
| Cores / Threads | 6 cores / 12 threads |
| CPU max | 4.65 GHz |
| Memory | 31.2 GiB total |
| Date | 2026-11-06 02:24 UTC |

## Language toolchains

| Language | Version / mode |
| --- | --- |
| **Dusk** | **0.13.0 (git 26157d0) — built with Zig 0.16.0, ReleaseFast** |
| Zig (native) | 0.16.0 — native baseline, build-exe -OReleaseFast |
| Java | openjdk version 25.0.4.1 2026-08-18 — precompiled with javac, JVM startup included |
| Node.js | v22.23.1 |
| Python 3 | Python 3.14.8 |
| Ruby | ruby 4.0.7 (2026-09-15 revision 229531a6cf) +PRISM |
| Lua | Lua 5.4.8  Copyright (C) 1994-2025 Lua.org, PUC-Rio |
| hyperfine | hyperfine 1.20.0 |

## Methodology

- Wall time is sampled by hyperfine with `--warmup 2 --min-runs 5 -N` (no intermediate shell).
- Peak memory is the resident-set high-water mark from `/usr/bin/time -f %M`, reported as the **median of 3 runs**. It includes the runtime/interpreter footprint (JVM, V8, CPython, the Dusk VM, …) plus the workload.
- **Zig (native)** is the baseline: compiled with `zig build-exe -OReleaseFast`. The **Relative** column and the summary charts are normalised to it (Zig = 1.00×, i.e. "× slower than native").
- No external libraries are used by any benchmark program.
- Every program is verified to print the expected result before measuring.

## Results

### Recursive Fibonacci (`fib`)

Compute fib(35) with pure recursion (fib(n-1) + fib(n-2)).

Expected output: `9227465`

| Language | Mean | Std dev | Min | Max | Relative | Peak mem |
| --- | --- | --- | --- | --- | --- | --- |
| Zig (native) | 21.6 ms | 4.1 ms | 19.1 ms | 34.9 ms | 1.00× | 0.7 MiB |
| Java | 49.7 ms | 8.4 ms | 42.9 ms | 71.5 ms | 2.30× | 38.3 MiB |
| Node.js | 173.0 ms | 17.1 ms | 149.5 ms | 196.1 ms | 8.00× | 58.7 MiB |
| **Dusk** | **498.7 ms** | **10.9 ms** | **490.2 ms** | **515.5 ms** | **23.05×** | **1.3 MiB** |
| Lua | 518.6 ms | 4.2 ms | 514.5 ms | 524.7 ms | 23.97× | 2.8 MiB |
| Ruby | 751.1 ms | 26.8 ms | 713.7 ms | 780.7 ms | 34.72× | 14.5 MiB |
| Python 3 | 959.5 ms | 16.9 ms | 943.2 ms | 984.3 ms | 44.36× | 9.2 MiB |

![Recursive Fibonacci](assets/fib.png)

![Recursive Fibonacci memory](assets/fib_mem.png)

### Particle System (`particles`)

100,000 particles, 100 update passes of x += vx / y += vy; print x of the 50,000th particle.

Expected output: `50049`

| Language | Mean | Std dev | Min | Max | Relative | Peak mem |
| --- | --- | --- | --- | --- | --- | --- |
| Zig (native) | 8.7 ms | 5.3 ms | 5.8 ms | 44.8 ms | 1.00× | 3.8 MiB |
| Java | 41.8 ms | 13.3 ms | 31.8 ms | 76.9 ms | 4.81× | 43.8 MiB |
| Node.js | 186.6 ms | 13.8 ms | 160.1 ms | 202.0 ms | 21.47× | 85.2 MiB |
| Lua | 309.9 ms | 12.4 ms | 293.1 ms | 331.8 ms | 35.65× | 21.5 MiB |
| **Dusk** | **834.6 ms** | **34.9 ms** | **795.8 ms** | **869.2 ms** | **95.99×** | **16.9 MiB** |
| Ruby | 1.701 s | 38.5 ms | 1.665 s | 1.755 s | 195.59× | 30.7 MiB |
| Python 3 | 1.938 s | 36.8 ms | 1.893 s | 1.992 s | 222.93× | 34.9 MiB |

![Particle System](assets/particles.png)

![Particle System memory](assets/particles_mem.png)

### Sieve of Eratosthenes (`primes`)

Count primes up to 100,000 using a contiguous boolean array.

Expected output: `9592`

| Language | Mean | Std dev | Min | Max | Relative | Peak mem |
| --- | --- | --- | --- | --- | --- | --- |
| Zig (native) | 877.2 µs | 632.7 µs | 597.1 µs | 8.4 ms | 1.00× | 0.8 MiB |
| Lua | 6.6 ms | 3.3 ms | 4.8 ms | 27.4 ms | 7.49× | 4.8 MiB |
| **Dusk** | **12.2 ms** | **4.3 ms** | **10.0 ms** | **35.4 ms** | **13.91×** | **3.0 MiB** |
| Java | 20.4 ms | 9.6 ms | 16.1 ms | 55.5 ms | 23.25× | 38.3 MiB |
| Python 3 | 26.8 ms | 9.4 ms | 21.7 ms | 62.6 ms | 30.55× | 10.1 MiB |
| Ruby | 55.1 ms | 15.8 ms | 44.7 ms | 90.1 ms | 62.79× | 15.2 MiB |
| Node.js | 93.4 ms | 18.8 ms | 74.6 ms | 130.9 ms | 106.42× | 59.7 MiB |

![Sieve of Eratosthenes](assets/primes.png)

![Sieve of Eratosthenes memory](assets/primes_mem.png)

### QuickSort (`quicksort`)

Sort 10,000 descending ints in place with a hand-written, recursive middle-pivot quicksort; print value at index 5000.

Expected output: `5001`

| Language | Mean | Std dev | Min | Max | Relative | Peak mem |
| --- | --- | --- | --- | --- | --- | --- |
| Zig (native) | 783.4 µs | 608.2 µs | 547.0 µs | 8.8 ms | 1.00× | 0.7 MiB |
| Lua | 3.4 ms | 1.6 ms | 2.7 ms | 16.7 ms | 4.39× | 3.0 MiB |
| **Dusk** | **6.1 ms** | **2.1 ms** | **5.1 ms** | **20.7 ms** | **7.73×** | **1.6 MiB** |
| Python 3 | 18.5 ms | 7.5 ms | 14.2 ms | 50.7 ms | 23.66× | 9.7 MiB |
| Java | 21.7 ms | 10.4 ms | 16.1 ms | 59.0 ms | 27.64× | 38.1 MiB |
| Ruby | 52.5 ms | 15.7 ms | 41.1 ms | 89.4 ms | 67.00× | 14.6 MiB |
| Node.js | 96.4 ms | 19.7 ms | 77.1 ms | 125.8 ms | 123.10× | 58.9 MiB |

![QuickSort](assets/quicksort.png)

![QuickSort memory](assets/quicksort_mem.png)

### Mandelbrot (`mandelbrot`)

800x800 grid, up to 1000 iterations per pixel (identical IEEE-754 double math everywhere); print the sum of all iteration counts as a checksum.

Expected output: `141554306`

| Language | Mean | Std dev | Min | Max | Relative | Peak mem |
| --- | --- | --- | --- | --- | --- | --- |
| Zig (native) | 319.7 ms | 3.0 ms | 315.1 ms | 325.0 ms | 1.00× | 0.7 MiB |
| Java | 354.8 ms | 13.0 ms | 342.7 ms | 379.1 ms | 1.11× | 38.7 MiB |
| Node.js | 419.2 ms | 20.0 ms | 401.9 ms | 447.3 ms | 1.31× | 60.7 MiB |
| Lua | 4.465 s | 121.6 ms | 4.274 s | 4.568 s | 13.97× | 3.0 MiB |
| Ruby | 10.556 s | 371.7 ms | 10.227 s | 11.128 s | 33.01× | 14.6 MiB |
| **Dusk** | **10.780 s** | **103.1 ms** | **10.618 s** | **10.880 s** | **33.71×** | **1.3 MiB** |
| Python 3 | 43.590 s | 1.307 s | 41.793 s | 44.991 s | 136.33× | 9.4 MiB |

![Mandelbrot](assets/mandelbrot.png)

![Mandelbrot memory](assets/mandelbrot_mem.png)

### Strings (`strings`)

1,000,000 iterations of: build "item {i}: value={i*3} tag={name}" by interpolation, split it on spaces, join the pieces with commas; print the sum of the line, piece-count and joined lengths.

Expected output: `73037032`

| Language | Mean | Std dev | Min | Max | Relative | Peak mem |
| --- | --- | --- | --- | --- | --- | --- |
| Zig (native) | 93.8 ms | 2.8 ms | 89.0 ms | 102.3 ms | 1.00× | 0.7 MiB |
| **Dusk** | **283.6 ms** | **6.5 ms** | **275.9 ms** | **298.8 ms** | **3.02×** | **3.1 MiB** |
| Java | 342.4 ms | 7.6 ms | 331.8 ms | 353.2 ms | 3.65× | 286.5 MiB |
| Node.js | 475.3 ms | 31.7 ms | 441.3 ms | 521.0 ms | 5.07× | 90.8 MiB |
| Python 3 | 582.9 ms | 16.4 ms | 566.1 ms | 606.8 ms | 6.22× | 9.3 MiB |
| Ruby | 943.6 ms | 23.7 ms | 928.6 ms | 985.5 ms | 10.06× | 14.6 MiB |
| Lua | 1.143 s | 11.5 ms | 1.129 s | 1.158 s | 12.19× | 2.9 MiB |

![Strings](assets/strings.png)

![Strings memory](assets/strings_mem.png)

### String Concatenation (`concat`)

Grow a string by appending "ab" 200,000 times with plain concatenation (no builder), so every step copies the whole string; print the final length.

Expected output: `400000`

| Language | Mean | Std dev | Min | Max | Relative | Peak mem |
| --- | --- | --- | --- | --- | --- | --- |
| Node.js | 98.7 ms | 8.0 ms | 88.7 ms | 119.6 ms | 0.15× | 70.6 MiB |
| Zig (native) | 660.3 ms | 8.8 ms | 649.5 ms | 669.1 ms | 1.00× | 1.3 MiB |
| **Dusk** | **1.174 s** | **19.7 ms** | **1.149 s** | **1.193 s** | **1.78×** | **3.5 MiB** |
| Java | 3.363 s | 39.9 ms | 3.300 s | 3.405 s | 5.09× | 592.9 MiB |
| Ruby | 3.783 s | 445.5 ms | 3.420 s | 4.343 s | 5.73× | 78.8 MiB |
| Lua | 4.842 s | 63.9 ms | 4.774 s | 4.946 s | 7.33× | 4.9 MiB |
| Python 3 | 4.956 s | 601.0 ms | 3.914 s | 5.403 s | 7.51× | 10.1 MiB |

![String Concatenation](assets/concat.png)

![String Concatenation memory](assets/concat_mem.png)

### CSV Parsing (`parse`)

Build 500,000 "i,(i*7)%1000,item{i}" rows, join them with newlines, then split the text back into rows and fields and parse the first two fields as ints; print their sum plus the row count.

Expected output: `125250000000`

| Language | Mean | Std dev | Min | Max | Relative | Peak mem |
| --- | --- | --- | --- | --- | --- | --- |
| Zig (native) | 52.6 ms | 950.4 µs | 51.3 ms | 56.8 ms | 1.00× | 49.4 MiB |
| Java | 230.6 ms | 2.8 ms | 226.4 ms | 236.2 ms | 4.39× | 259.8 MiB |
| Node.js | 334.7 ms | 10.6 ms | 325.7 ms | 358.3 ms | 6.37× | 209.0 MiB |
| Python 3 | 354.2 ms | 6.2 ms | 345.4 ms | 364.5 ms | 6.74× | 88.6 MiB |
| **Dusk** | **411.7 ms** | **3.7 ms** | **407.8 ms** | **416.8 ms** | **7.83×** | **232.4 MiB** |
| Lua | 497.0 ms | 17.4 ms | 480.2 ms | 524.2 ms | 9.45× | 82.2 MiB |
| Ruby | 556.9 ms | 11.1 ms | 548.8 ms | 574.3 ms | 10.59× | 124.6 MiB |

![CSV Parsing](assets/parse.png)

![CSV Parsing memory](assets/parse_mem.png)

## Time summary

Slowdown of each language relative to the **Zig (native)** baseline in each benchmark (log scale).

![Time summary](assets/summary.png)

## Memory summary

Peak resident memory per benchmark (log scale).

![Memory summary](assets/summary_mem.png)
