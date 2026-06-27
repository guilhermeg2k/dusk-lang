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
| Date | 2026-06-27 21:31 UTC |

## Language toolchains

| Language | Version / mode |
| --- | --- |
| **Dusk** | **0.10.0 (git db50c05) — built with Zig 0.16.0, ReleaseFast** |
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
| Zig (native) | 19.1 ms | 222.6 µs | 18.8 ms | 19.7 ms | 1.00× | 0.7 MiB |
| Java | 41.6 ms | 801.1 µs | 38.5 ms | 43.6 ms | 2.18× | 38.3 MiB |
| Node.js | 144.1 ms | 1.6 ms | 142.3 ms | 149.8 ms | 7.56× | 58.6 MiB |
| **Dusk** | **399.5 ms** | **18.5 ms** | **372.7 ms** | **423.4 ms** | **20.95×** | **1.0 MiB** |
| Lua | 453.9 ms | 10.2 ms | 442.4 ms | 466.7 ms | 23.80× | 2.7 MiB |
| Ruby | 694.0 ms | 27.4 ms | 665.0 ms | 739.4 ms | 36.39× | 14.4 MiB |
| Python 3 | 883.8 ms | 9.8 ms | 868.8 ms | 893.1 ms | 46.34× | 9.3 MiB |

![Recursive Fibonacci](assets/fib.png)

![Recursive Fibonacci memory](assets/fib_mem.png)

### Particle System (`particles`)

100,000 particles, 100 update passes of x += vx / y += vy; print x of the 50,000th particle.

Expected output: `50049`

| Language | Mean | Std dev | Min | Max | Relative | Peak mem |
| --- | --- | --- | --- | --- | --- | --- |
| Zig (native) | 6.1 ms | 355.5 µs | 5.5 ms | 7.6 ms | 1.00× | 3.8 MiB |
| Java | 31.0 ms | 1.2 ms | 29.6 ms | 34.4 ms | 5.12× | 44.1 MiB |
| Node.js | 146.7 ms | 1.4 ms | 144.4 ms | 149.7 ms | 24.21× | 85.4 MiB |
| Lua | 258.7 ms | 4.0 ms | 253.3 ms | 265.1 ms | 42.70× | 21.4 MiB |
| **Dusk** | **453.6 ms** | **4.5 ms** | **446.5 ms** | **459.7 ms** | **74.87×** | **16.6 MiB** |
| Ruby | 1.536 s | 136.7 ms | 1.445 s | 1.776 s | 253.48× | 30.7 MiB |
| Python 3 | 1.642 s | 53.1 ms | 1.568 s | 1.702 s | 270.99× | 34.9 MiB |

![Particle System](assets/particles.png)

![Particle System memory](assets/particles_mem.png)

### Sieve of Eratosthenes (`primes`)

Count primes up to 100,000 using a contiguous boolean array.

Expected output: `9592`

| Language | Mean | Std dev | Min | Max | Relative | Peak mem |
| --- | --- | --- | --- | --- | --- | --- |
| Zig (native) | 690.7 µs | 58.3 µs | 568.8 µs | 909.8 µs | 1.00× | 0.7 MiB |
| Lua | 5.0 ms | 137.3 µs | 4.7 ms | 6.0 ms | 7.24× | 4.7 MiB |
| **Dusk** | **10.7 ms** | **389.8 µs** | **9.8 ms** | **13.6 ms** | **15.53×** | **2.8 MiB** |
| Java | 15.9 ms | 453.9 µs | 14.8 ms | 18.6 ms | 23.02× | 38.4 MiB |
| Python 3 | 22.8 ms | 790.7 µs | 21.8 ms | 25.9 ms | 33.08× | 10.0 MiB |
| Ruby | 44.0 ms | 454.0 µs | 43.4 ms | 46.3 ms | 63.72× | 15.3 MiB |
| Node.js | 73.7 ms | 863.4 µs | 72.5 ms | 75.8 ms | 106.75× | 59.8 MiB |

![Sieve of Eratosthenes](assets/primes.png)

![Sieve of Eratosthenes memory](assets/primes_mem.png)

### QuickSort (`quicksort`)

Sort 10,000 descending ints in place with a hand-written, recursive middle-pivot quicksort; print value at index 5000.

Expected output: `5001`

| Language | Mean | Std dev | Min | Max | Relative | Peak mem |
| --- | --- | --- | --- | --- | --- | --- |
| Zig (native) | 634.5 µs | 53.6 µs | 523.5 µs | 983.0 µs | 1.00× | 0.7 MiB |
| Lua | 3.0 ms | 113.0 µs | 2.7 ms | 4.0 ms | 4.69× | 3.0 MiB |
| **Dusk** | **5.0 ms** | **142.4 µs** | **4.7 ms** | **5.9 ms** | **7.87×** | **1.4 MiB** |
| Python 3 | 14.1 ms | 287.1 µs | 13.7 ms | 15.8 ms | 22.21× | 9.6 MiB |
| Java | 15.3 ms | 465.1 µs | 14.4 ms | 17.2 ms | 24.11× | 38.5 MiB |
| Ruby | 39.9 ms | 461.4 µs | 39.3 ms | 41.2 ms | 62.90× | 14.7 MiB |
| Node.js | 72.4 ms | 1.3 ms | 70.6 ms | 77.5 ms | 114.06× | 59.1 MiB |

![QuickSort](assets/quicksort.png)

![QuickSort memory](assets/quicksort_mem.png)

### Mandelbrot (`mandelbrot`)

800x800 grid, up to 1000 iterations per pixel (identical IEEE-754 double math everywhere); print the sum of all iteration counts as a checksum.

Expected output: `141554306`

| Language | Mean | Std dev | Min | Max | Relative | Peak mem |
| --- | --- | --- | --- | --- | --- | --- |
| Zig (native) | 306.5 ms | 426.2 µs | 306.0 ms | 307.3 ms | 1.00× | 0.7 MiB |
| Java | 329.2 ms | 491.3 µs | 328.7 ms | 330.1 ms | 1.07× | 38.8 MiB |
| Node.js | 380.8 ms | 945.8 µs | 379.7 ms | 382.3 ms | 1.24× | 61.0 MiB |
| Lua | 4.307 s | 118.3 ms | 4.181 s | 4.447 s | 14.05× | 2.7 MiB |
| **Dusk** | **5.582 s** | **150.0 ms** | **5.453 s** | **5.821 s** | **18.21×** | **1.0 MiB** |
| Ruby | 9.675 s | 170.1 ms | 9.486 s | 9.840 s | 31.57× | 14.5 MiB |
| Python 3 | 40.808 s | 1.796 s | 38.069 s | 42.919 s | 133.14× | 9.4 MiB |

![Mandelbrot](assets/mandelbrot.png)

![Mandelbrot memory](assets/mandelbrot_mem.png)

## Time summary

Slowdown of each language relative to the **Zig (native)** baseline in each benchmark (log scale).

![Time summary](assets/summary.png)

## Memory summary

Peak resident memory per benchmark (log scale).

![Memory summary](assets/summary_mem.png)
