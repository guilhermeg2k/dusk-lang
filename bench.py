#!/usr/bin/env python3
"""Dusk-only benchmark runner, for checking if a change made Dusk faster or slower.

Usage:
  python3 bench.py                  # run every benchmark on the current tree
  python3 bench.py fib primes       # run only some benchmarks
  python3 bench.py --compare main   # also build `main` (any git ref) and compare
  python3 bench.py --json           # print only the JSON result to stdout
  python3 bench.py --save v0.11     # also store the result in benchmarks/history/

Every run writes benchmarks/results/latest.json.

Timing is done here instead of hyperfine so that, when comparing, the two
binaries can be run alternately (CPU heat and clock changes hit both equally).
Peak memory comes from the same runs (rusage of the child process).
Only the standard library is needed. For the cross-language report with
charts, use run_benchmarks.py.
"""

from __future__ import annotations

import argparse
import json
import os
import shutil
import statistics
import subprocess
import sys
import tempfile
import time
from datetime import datetime, timezone
from pathlib import Path

from run_benchmarks import BENCH_DIR, BENCHMARKS, ROOT, machine_info, matches

RESULTS_DIR = BENCH_DIR / "results"
HISTORY_DIR = BENCH_DIR / "history"

# Relative stddev above this marks a result as noisy.
NOISE_THRESHOLD = 0.05
# Median change (percent) needed before calling something faster or slower.
CHANGE_THRESHOLD_PCT = 5.0


def log(msg: str) -> None:
    print(msg, file=sys.stderr, flush=True)


def git(*args: str, cwd: Path = ROOT) -> str:
    res = subprocess.run(["git", *args], cwd=str(cwd), capture_output=True, text=True)
    if res.returncode != 0:
        raise SystemExit(f"[git] git {' '.join(args)} failed:\n{res.stderr}")
    return res.stdout.strip()


def build(src_dir: Path, out_bin: Path) -> None:
    log(f"[build] {src_dir} (ReleaseFast)")
    res = subprocess.run(["zig", "build", "-Doptimize=ReleaseFast"], cwd=str(src_dir),
                         capture_output=True, text=True)
    if res.returncode != 0:
        raise SystemExit(f"[build] failed in {src_dir}:\n{res.stdout}{res.stderr}")
    # copied out so later builds of the tree can't change the binary under test
    shutil.copy2(src_dir / "zig-out" / "bin" / "dusk", out_bin)


def run_once(binary: Path, bench_file: Path) -> tuple[float, int, str, int]:
    """Run one benchmark once: (seconds, peak memory KiB, stdout, exit code)."""
    start = time.perf_counter()
    proc = subprocess.Popen([str(binary), str(bench_file)], cwd=str(ROOT),
                            stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, text=True)
    out = proc.stdout.read()
    _, status, rusage = os.wait4(proc.pid, 0)
    elapsed = time.perf_counter() - start
    proc.stdout.close()
    proc.returncode = os.waitstatus_to_exitcode(status)
    return elapsed, rusage.ru_maxrss, out, proc.returncode


def verify(binary: Path, bench: dict) -> bool:
    _, _, out, code = run_once(binary, BENCH_DIR / "dusk" / f"{bench['key']}.dsk")
    return code == 0 and matches(bench["expected"], out, bench["numeric"])


def summarize(times: list[float], mems: list[int]) -> dict:
    median = statistics.median(times)
    stddev = statistics.stdev(times) if len(times) > 1 else 0.0
    return {
        "runs": len(times),
        "median_ms": round(median * 1000, 2),
        "min_ms": round(min(times) * 1000, 2),
        "max_ms": round(max(times) * 1000, 2),
        "stddev_ms": round(stddev * 1000, 2),
        "peak_mem_kib": int(statistics.median(mems)),
        "noisy": median > 0 and stddev / median > NOISE_THRESHOLD,
    }


def measure(bench: dict, binaries: dict[str, Path], runs: int, warmup: int) -> dict[str, dict]:
    """Run every binary alternately, return stats per binary name."""
    bench_file = BENCH_DIR / "dusk" / f"{bench['key']}.dsk"
    samples: dict[str, tuple[list[float], list[int]]] = {name: ([], []) for name in binaries}

    for i in range(warmup + runs):
        for name, binary in binaries.items():
            elapsed, mem, _, code = run_once(binary, bench_file)
            if code != 0:
                raise SystemExit(f"[bench] {bench['key']} exited with {code} ({name})")
            if i >= warmup:
                samples[name][0].append(elapsed)
                samples[name][1].append(mem)

    return {name: summarize(*samples[name]) for name in binaries}


def verdict(current: dict, base: dict) -> dict:
    change = (current["median_ms"] - base["median_ms"]) / base["median_ms"] * 100
    if abs(change) < CHANGE_THRESHOLD_PCT:
        result = "same"
    else:
        result = "slower" if change > 0 else "faster"
    return {
        "base_median_ms": base["median_ms"],
        "change_pct": round(change, 1),
        "verdict": result,
        # a verdict between noisy results needs a rerun with more --runs
        "noisy": current["noisy"] or base["noisy"],
    }


def print_table(result: dict) -> None:
    compare = result.get("compare")
    header = f"{'benchmark':<12}{'median':>11}{'min':>11}{'stddev':>10}{'mem':>10}"
    if compare:
        header += f"{'base':>11}{'change':>9}  verdict"
    log("\n" + header)
    for key, b in result["benchmarks"].items():
        line = (f"{key:<12}{b['median_ms']:>9.1f}ms{b['min_ms']:>9.1f}ms"
                f"{b['stddev_ms']:>8.1f}ms{b['peak_mem_kib'] / 1024:>7.1f}MiB")
        if compare:
            c = compare["benchmarks"][key]
            line += (f"{c['base_median_ms']:>9.1f}ms{c['change_pct']:>+8.1f}%  {c['verdict']}"
                     + (" (noisy)" if c["noisy"] else ""))
        elif b["noisy"]:
            line += "  (noisy)"
        log(line)


def main() -> None:
    parser = argparse.ArgumentParser(description="Dusk-only benchmark runner")
    parser.add_argument("names", nargs="*", help="benchmarks to run (default: all)")
    parser.add_argument("--compare", metavar="REF", help="git ref to compare against")
    parser.add_argument("--runs", type=int, default=10, help="measured runs per binary (default: 10)")
    parser.add_argument("--warmup", type=int, default=2, help="unmeasured runs first (default: 2)")
    parser.add_argument("--json", action="store_true", help="print only the JSON result to stdout")
    parser.add_argument("--save", metavar="NAME", help="also write benchmarks/history/NAME.json")
    args = parser.parse_args()

    known = {b["key"]: b for b in BENCHMARKS}
    unknown = [n for n in args.names if n not in known]
    if unknown:
        raise SystemExit(f"unknown benchmark(s): {', '.join(unknown)} "
                         f"(available: {', '.join(known)})")
    benches = [known[n] for n in args.names] if args.names else BENCHMARKS

    machine = machine_info()
    result: dict = {
        "commit": git("rev-parse", "--short", "HEAD"),
        "dirty": bool(git("status", "--porcelain", "--untracked-files=no")),
        "date": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "machine": {
            "cpu": machine.get("CPU", "unknown"),
            "os": machine.get("OS", "unknown"),
            "zig": subprocess.run(["zig", "version"], capture_output=True, text=True).stdout.strip(),
        },
        "settings": {"runs": args.runs, "warmup": args.warmup},
        "benchmarks": {},
    }

    with tempfile.TemporaryDirectory(prefix="dusk-bench-") as tmp_str:
        tmp = Path(tmp_str)
        binaries = {"current": tmp / "dusk-current"}
        build(ROOT, binaries["current"])

        if args.compare:
            base_commit = git("rev-parse", "--short", args.compare)
            worktree = tmp / "base"
            git("worktree", "add", "--detach", str(worktree), args.compare)
            try:
                binaries["base"] = tmp / "dusk-base"
                build(worktree, binaries["base"])
            finally:
                git("worktree", "remove", "--force", str(worktree))
            result["compare"] = {"against": f"{args.compare}@{base_commit}", "benchmarks": {}}

        for bench in benches:
            for name, binary in binaries.items():
                if not verify(binary, bench):
                    raise SystemExit(f"[verify] {bench['key']} printed the wrong result ({name})")

            log(f"[bench] {bench['key']} ({args.runs} runs)")
            stats = measure(bench, binaries, args.runs, args.warmup)
            result["benchmarks"][bench["key"]] = stats["current"]
            if args.compare:
                result["compare"]["benchmarks"][bench["key"]] = verdict(stats["current"], stats["base"])

    output = json.dumps(result, indent=2)
    RESULTS_DIR.mkdir(exist_ok=True)
    (RESULTS_DIR / "latest.json").write_text(output + "\n")
    if args.save:
        HISTORY_DIR.mkdir(exist_ok=True)
        (HISTORY_DIR / f"{args.save}.json").write_text(output + "\n")

    if args.json:
        print(output)
    else:
        print_table(result)
        log(f"\nwritten to {RESULTS_DIR / 'latest.json'}")


if __name__ == "__main__":
    main()
