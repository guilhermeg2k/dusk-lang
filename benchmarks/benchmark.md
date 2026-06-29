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
| Kernel | Linux 7.0.12-201.fc44.x86_64 |
| Architecture | x86_64 |
| CPU | AMD Ryzen 5 5600X 6-Core Processor |
| Cores / Threads | 6 cores / 12 threads |
| CPU max | 4.65 GHz |
| Memory | 31.2 GiB total |
| Date | 2026-06-29 00:49 UTC |

## Language toolchains

| Language | Version / mode |
| --- | --- |
| **Dusk** | **0.10.0 (git ef21578) — built with Zig 0.16.0, ReleaseFast** |
| Zig (native) | 0.16.0 — native baseline, build-exe -OReleaseFast |
| Java | openjdk version 25.0.3 2026-04-21 — precompiled with javac, JVM startup included |
| Node.js | v22.22.2 |
| Python 3 | Python 3.14.5 |
| Ruby | ruby 4.0.5 (2026-05-20 revision 64336ffd0e) +PRISM |
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
| Zig (native) | 19.0 ms | 137.7 µs | 18.8 ms | 19.9 ms | 1.00× | 0.7 MiB |
| Java | 42.5 ms | 951.7 µs | 40.5 ms | 44.7 ms | 2.23× | 38.2 MiB |
| Node.js | 146.0 ms | 3.5 ms | 142.5 ms | 159.5 ms | 7.67× | 58.6 MiB |
| **Dusk** | **405.9 ms** | **21.6 ms** | **382.1 ms** | **433.5 ms** | **21.34×** | **1.0 MiB** |
| Lua | 451.5 ms | 7.2 ms | 440.1 ms | 463.0 ms | 23.73× | 2.7 MiB |
| Ruby | 708.5 ms | 34.2 ms | 687.1 ms | 767.2 ms | 37.24× | 14.4 MiB |
| Python 3 | 885.5 ms | 8.8 ms | 873.8 ms | 893.9 ms | 46.54× | 9.1 MiB |

![Recursive Fibonacci](assets/fib.png)

![Recursive Fibonacci memory](assets/fib_mem.png)

### Particle System (`particles`)

100,000 particles, 100 update passes of x += vx / y += vy; print x of the 50,000th particle.

Expected output: `50049`

| Language | Mean | Std dev | Min | Max | Relative | Peak mem |
| --- | --- | --- | --- | --- | --- | --- |
| Zig (native) | 6.1 ms | 268.1 µs | 5.5 ms | 7.2 ms | 1.00× | 3.7 MiB |
| Java | 31.2 ms | 1.3 ms | 29.7 ms | 35.8 ms | 5.16× | 44.0 MiB |
| Node.js | 148.1 ms | 3.4 ms | 145.2 ms | 160.8 ms | 24.44× | 85.4 MiB |
| Lua | 257.6 ms | 4.5 ms | 250.3 ms | 264.9 ms | 42.53× | 21.5 MiB |
| **Dusk** | **466.2 ms** | **30.8 ms** | **425.9 ms** | **500.7 ms** | **76.96×** | **16.6 MiB** |
| Ruby | 1.489 s | 31.6 ms | 1.433 s | 1.507 s | 245.78× | 30.6 MiB |
| Python 3 | 1.542 s | 56.8 ms | 1.485 s | 1.630 s | 254.55× | 34.9 MiB |

![Particle System](assets/particles.png)

![Particle System memory](assets/particles_mem.png)

### Sieve of Eratosthenes (`primes`)

Count primes up to 100,000 using a contiguous boolean array.

Expected output: `9592`

| Language | Mean | Std dev | Min | Max | Relative | Peak mem |
| --- | --- | --- | --- | --- | --- | --- |
| Zig (native) | 685.9 µs | 59.7 µs | 560.6 µs | 1.2 ms | 1.00× | 0.8 MiB |
| Lua | 5.1 ms | 189.0 µs | 4.8 ms | 6.0 ms | 7.50× | 4.9 MiB |
| **Dusk** | **10.4 ms** | **390.4 µs** | **9.5 ms** | **12.7 ms** | **15.17×** | **2.8 MiB** |
| Java | 16.2 ms | 455.1 µs | 15.3 ms | 18.2 ms | 23.62× | 38.2 MiB |
| Python 3 | 23.0 ms | 1.4 ms | 21.7 ms | 32.9 ms | 33.59× | 10.0 MiB |
| Ruby | 44.3 ms | 527.8 µs | 43.2 ms | 45.8 ms | 64.51× | 15.3 MiB |
| Node.js | 74.2 ms | 963.6 µs | 72.5 ms | 76.0 ms | 108.19× | 59.9 MiB |

![Sieve of Eratosthenes](assets/primes.png)

![Sieve of Eratosthenes memory](assets/primes_mem.png)

### QuickSort (`quicksort`)

Sort 10,000 descending ints in place with a hand-written, recursive middle-pivot quicksort; print value at index 5000.

Expected output: `5001`

| Language | Mean | Std dev | Min | Max | Relative | Peak mem |
| --- | --- | --- | --- | --- | --- | --- |
| Zig (native) | 626.4 µs | 54.4 µs | 510.9 µs | 790.8 µs | 1.00× | 0.7 MiB |
| Lua | 3.0 ms | 86.3 µs | 2.7 ms | 3.4 ms | 4.75× | 3.1 MiB |
| **Dusk** | **5.1 ms** | **187.2 µs** | **4.6 ms** | **7.2 ms** | **8.09×** | **1.3 MiB** |
| Python 3 | 14.1 ms | 295.8 µs | 13.5 ms | 15.2 ms | 22.43× | 9.6 MiB |
| Java | 15.8 ms | 447.4 µs | 15.0 ms | 17.8 ms | 25.21× | 38.5 MiB |
| Ruby | 40.2 ms | 590.8 µs | 39.4 ms | 42.5 ms | 64.18× | 14.4 MiB |
| Node.js | 73.3 ms | 3.2 ms | 70.5 ms | 90.2 ms | 117.07× | 59.0 MiB |

![QuickSort](assets/quicksort.png)

![QuickSort memory](assets/quicksort_mem.png)

### Mandelbrot (`mandelbrot`)

800x800 grid, up to 1000 iterations per pixel (identical IEEE-754 double math everywhere); print the sum of all iteration counts as a checksum.

Expected output: `141554306`

| Language | Mean | Std dev | Min | Max | Relative | Peak mem |
| --- | --- | --- | --- | --- | --- | --- |
| Zig (native) | 306.6 ms | 409.5 µs | 306.2 ms | 307.5 ms | 1.00× | 0.7 MiB |
| Java | 329.5 ms | 460.9 µs | 328.9 ms | 330.2 ms | 1.07× | 38.8 MiB |
| Node.js | 381.6 ms | 1.6 ms | 380.3 ms | 384.7 ms | 1.24× | 60.8 MiB |
| Lua | 4.281 s | 50.9 ms | 4.225 s | 4.341 s | 13.96× | 3.0 MiB |
| **Dusk** | **5.693 s** | **217.7 ms** | **5.365 s** | **5.903 s** | **18.57×** | **1.0 MiB** |
| Ruby | 9.717 s | 144.4 ms | 9.490 s | 9.817 s | 31.69× | 14.4 MiB |
| Python 3 | 39.445 s | 1.555 s | 38.488 s | 42.180 s | 128.65× | 9.5 MiB |

![Mandelbrot](assets/mandelbrot.png)

![Mandelbrot memory](assets/mandelbrot_mem.png)

## Time summary

Slowdown of each language relative to the **Zig (native)** baseline in each benchmark (log scale).

![Time summary](assets/summary.png)

## Memory summary

Peak resident memory per benchmark (log scale).

![Memory summary](assets/summary_mem.png)
