# Benchmark

Cross-language micro-benchmarks tracking **Dusk** against a native **Zig** baseline and five mainstream runtimes. Wall time is measured externally with [`hyperfine`](https://github.com/sharkdp/hyperfine) and peak memory with GNU `/usr/bin/time`; the benchmark programs contain **no timing or instrumentation code**. This benchmark is not an evidence that X is faster than Y, its main purpose is to track Dusk performance specially on versions change, so we can easily notice performance problems/gains.

Regenerate this file with:

```bash
python3 run_benchmarks.py
```

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
| Date | 2026-06-24 04:18 UTC |

> **Note:** This benchmark generation script and the algorithms were AI-generated. I haven't deeply reviewed them, so they may contain errors.

## Language toolchains

| Language | Version / mode |
| --- | --- |
| **Dusk** | **0.10.0-devel (git 5ee4c87) — built with Zig 0.16.0, ReleaseFast** |
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
| Zig (native) | 19.2 ms | 389.9 µs | 18.6 ms | 21.2 ms | 1.00× | 0.7 MiB |
| Java | 39.5 ms | 945.2 µs | 37.6 ms | 42.5 ms | 2.06× | 38.2 MiB |
| Node.js | 146.2 ms | 3.0 ms | 142.8 ms | 155.2 ms | 7.62× | 58.6 MiB |
| **Dusk** | **425.5 ms** | **27.9 ms** | **383.7 ms** | **469.3 ms** | **22.18×** | **1.0 MiB** |
| Lua | 454.9 ms | 10.6 ms | 442.9 ms | 467.7 ms | 23.71× | 2.7 MiB |
| Ruby | 690.9 ms | 5.7 ms | 685.0 ms | 697.4 ms | 36.02× | 14.4 MiB |
| Python 3 | 885.3 ms | 13.0 ms | 869.3 ms | 900.9 ms | 46.15× | 9.3 MiB |

![Recursive Fibonacci](assets/fib.png)

![Recursive Fibonacci memory](assets/fib_mem.png)

### Particle System (`particles`)

100,000 particles, 100 update passes of x += vx / y += vy; print x of the 50,000th particle.

Expected output: `50049`

| Language | Mean | Std dev | Min | Max | Relative | Peak mem |
| --- | --- | --- | --- | --- | --- | --- |
| Zig (native) | 5.7 ms | 581.6 µs | 4.6 ms | 8.3 ms | 1.00× | 3.8 MiB |
| Java | 28.5 ms | 1.2 ms | 26.0 ms | 32.7 ms | 5.00× | 44.2 MiB |
| Node.js | 141.9 ms | 1.7 ms | 139.2 ms | 144.6 ms | 24.90× | 86.0 MiB |
| Lua | 257.5 ms | 10.9 ms | 242.7 ms | 281.6 ms | 45.19× | 21.4 MiB |
| **Dusk** | **562.2 ms** | **21.7 ms** | **536.4 ms** | **590.2 ms** | **98.67×** | **16.7 MiB** |
| Ruby | 1.540 s | 28.4 ms | 1.499 s | 1.578 s | 270.21× | 30.7 MiB |
| Python 3 | 1.648 s | 20.7 ms | 1.615 s | 1.667 s | 289.23× | 35.0 MiB |

![Particle System](assets/particles.png)

![Particle System memory](assets/particles_mem.png)

### Sieve of Eratosthenes (`primes`)

Count primes up to 100,000 using a contiguous boolean array.

Expected output: `9592`

| Language | Mean | Std dev | Min | Max | Relative | Peak mem |
| --- | --- | --- | --- | --- | --- | --- |
| Zig (native) | 400.1 µs | 46.4 µs | 322.8 µs | 820.2 µs | 1.00× | 0.8 MiB |
| Lua | 4.4 ms | 177.7 µs | 4.0 ms | 5.3 ms | 10.89× | 4.8 MiB |
| **Dusk** | **8.1 ms** | **314.0 µs** | **7.3 ms** | **10.0 ms** | **20.15×** | **3.6 MiB** |
| Java | 12.9 ms | 352.1 µs | 12.2 ms | 14.8 ms | 32.17× | 38.2 MiB |
| Python 3 | 21.2 ms | 559.5 µs | 20.3 ms | 23.4 ms | 52.96× | 10.0 MiB |
| Ruby | 39.9 ms | 520.3 µs | 39.2 ms | 41.2 ms | 99.71× | 15.3 MiB |
| Node.js | 68.3 ms | 2.4 ms | 66.6 ms | 82.1 ms | 170.79× | 59.9 MiB |

![Sieve of Eratosthenes](assets/primes.png)

![Sieve of Eratosthenes memory](assets/primes_mem.png)

### QuickSort (`quicksort`)

Sort 10,000 descending ints in place with a hand-written, recursive middle-pivot quicksort; print value at index 5000.

Expected output: `5001`

| Language | Mean | Std dev | Min | Max | Relative | Peak mem |
| --- | --- | --- | --- | --- | --- | --- |
| Zig (native) | 328.1 µs | 35.2 µs | 278.2 µs | 478.3 µs | 1.00× | 0.7 MiB |
| Lua | 2.6 ms | 102.2 µs | 2.4 ms | 4.3 ms | 7.88× | 3.0 MiB |
| **Dusk** | **4.5 ms** | **270.8 µs** | **4.1 ms** | **7.4 ms** | **13.84×** | **1.4 MiB** |
| Java | 12.3 ms | 705.9 µs | 11.4 ms | 18.7 ms | 37.57× | 38.5 MiB |
| Python 3 | 12.7 ms | 318.7 µs | 12.2 ms | 14.3 ms | 38.81× | 9.5 MiB |
| Ruby | 36.0 ms | 922.4 µs | 35.1 ms | 40.2 ms | 109.87× | 14.5 MiB |
| Node.js | 66.5 ms | 1.4 ms | 64.0 ms | 70.7 ms | 202.72× | 58.9 MiB |

![QuickSort](assets/quicksort.png)

![QuickSort memory](assets/quicksort_mem.png)

### Mandelbrot (`mandelbrot`)

800x800 grid, up to 1000 iterations per pixel (identical IEEE-754 double math everywhere); print the sum of all iteration counts as a checksum.

Expected output: `141554306`

| Language | Mean | Std dev | Min | Max | Relative | Peak mem |
| --- | --- | --- | --- | --- | --- | --- |
| Zig (native) | 306.3 ms | 563.5 µs | 305.7 ms | 307.6 ms | 1.00× | 0.7 MiB |
| Java | 324.6 ms | 639.4 µs | 323.7 ms | 325.9 ms | 1.06× | 38.9 MiB |
| Node.js | 374.0 ms | 993.3 µs | 372.6 ms | 375.8 ms | 1.22× | 60.7 MiB |
| Lua | 4.334 s | 37.3 ms | 4.279 s | 4.366 s | 14.15× | 2.9 MiB |
| **Dusk** | **5.952 s** | **164.5 ms** | **5.740 s** | **6.075 s** | **19.43×** | **1.0 MiB** |
| Ruby | 9.491 s | 32.5 ms | 9.446 s | 9.532 s | 30.99× | 14.4 MiB |
| Python 3 | 41.021 s | 2.980 s | 37.094 s | 44.079 s | 133.93× | 9.4 MiB |

![Mandelbrot](assets/mandelbrot.png)

![Mandelbrot memory](assets/mandelbrot_mem.png)

## Time summary

Slowdown of each language relative to the **Zig (native)** baseline in each benchmark (log scale).

![Time summary](assets/summary.png)

## Memory summary

Peak resident memory per benchmark (log scale).

![Memory summary](assets/summary_mem.png)
