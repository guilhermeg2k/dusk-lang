#!/usr/bin/env python3
"""Cross-language benchmark runner for Dusk.

Pipeline:
  1. Bootstrap matplotlib into a local .venv_bench (re-exec into it).
  2. Capture machine + toolchain information.
  3. Build the optimized Dusk binary (zig build -Doptimize=ReleaseFast).
  4. Run a correctness pass (every program must print the expected result).
  5. Benchmark every language with hyperfine (JSON export).
  6. Render matplotlib charts (one per benchmark + a summary).
  7. Generate benchmark.md.

No timing logic lives inside the benchmark programs; hyperfine measures them
externally. Run with:  python3 run_benchmarks.py
"""

from __future__ import annotations

import argparse
import json
import os
import platform
import re
import shutil
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path

ROOT = Path(__file__).resolve().parent
BENCH_DIR = ROOT / "benchmarks"
ASSETS_DIR = BENCH_DIR / "assets"
RESULTS_DIR = BENCH_DIR / "results"
DUSK_BIN = ROOT / "zig-out" / "bin" / "dusk"
VENV_DIR = BENCH_DIR / ".venv_bench"
JAVA_BUILD = BENCH_DIR / "build" / "java"
ZIG_BUILD = BENCH_DIR / "build" / "zig"
REPORT_MD = BENCH_DIR / "benchmark.md"
GNU_TIME = "/usr/bin/time"

# hyperfine sampling parameters.
WARMUP = 2
MIN_RUNS = 5

# Memory pass: peak RSS via /usr/bin/time, median of this many runs.
MEM_RUNS = 3

# Set during setup: "precompiled" (javac available) or "source" (JRE fallback).
JAVA_MODE = "source"
# Set during setup: True if the Zig native baseline compiled successfully.
ZIG_OK = False
# Label used as the 1.00x reference for relative numbers and summary charts.
BASELINE = "Zig (native)"

# Languages, in display order, with the command template per benchmark.
# {file} is replaced by the benchmark file for that language.
LANGUAGES = [
    {"key": "zig", "label": "Zig (native)", "tool": "zig", "ext": "zig",
     "cmd": ["{file}"]},
    {"key": "dusk", "label": "Dusk", "tool": str(DUSK_BIN), "ext": "dsk",
     "cmd": [str(DUSK_BIN), "{file}"]},
    {"key": "java", "label": "Java", "tool": "java", "ext": "java",
     "cmd": ["java", "{file}"]},
    {"key": "node", "label": "Node.js", "tool": "node", "ext": "js",
     "cmd": ["node", "{file}"]},
    {"key": "python", "label": "Python 3", "tool": "python3", "ext": "py",
     "cmd": ["python3", "{file}"]},
    {"key": "ruby", "label": "Ruby", "tool": "ruby", "ext": "rb",
     "cmd": ["ruby", "{file}"]},
    {"key": "lua", "label": "Lua", "tool": "lua", "ext": "lua",
     "cmd": ["lua", "{file}"]},
]

# Java uses PascalCase file names; everything else uses the benchmark key.
JAVA_CLASS = {"fib": "Fib", "particles": "Particles",
              "primes": "Primes", "quicksort": "Quicksort",
              "mandelbrot": "Mandelbrot"}

BENCHMARKS = [
    {
        "key": "fib",
        "title": "Recursive Fibonacci",
        "desc": "Compute fib(35) with pure recursion (fib(n-1) + fib(n-2)).",
        "expected": "9227465",
        "numeric": False,
    },
    {
        "key": "particles",
        "title": "Particle System",
        "desc": "100,000 particles, 100 update passes of x += vx / y += vy; "
                "print x of the 50,000th particle.",
        "expected": "50049",
        "numeric": True,
    },
    {
        "key": "primes",
        "title": "Sieve of Eratosthenes",
        "desc": "Count primes up to 100,000 using a contiguous boolean array.",
        "expected": "9592",
        "numeric": False,
    },
    {
        "key": "quicksort",
        "title": "QuickSort",
        "desc": "Sort 10,000 descending ints in place with a hand-written, "
                "recursive middle-pivot quicksort; print value at index 5000.",
        "expected": "5001",
        "numeric": False,
    },
    {
        "key": "mandelbrot",
        "title": "Mandelbrot",
        "desc": "800x800 grid, up to 1000 iterations per pixel (identical "
                "IEEE-754 double math everywhere); print the sum of all "
                "iteration counts as a checksum.",
        "expected": "141554306",
        "numeric": False,
    },
]


# --------------------------------------------------------------------------- #
# Bootstrap matplotlib
# --------------------------------------------------------------------------- #
def ensure_deps() -> None:
    try:
        import matplotlib  # noqa: F401
        return
    except ImportError:
        pass

    print("[setup] matplotlib not found -> creating .venv_bench ...")
    if not (VENV_DIR / "bin" / "python").exists():
        subprocess.run([sys.executable, "-m", "venv", str(VENV_DIR)], check=True)
    pip = VENV_DIR / "bin" / "pip"
    subprocess.run([str(pip), "install", "--quiet", "--upgrade", "pip"], check=True)
    subprocess.run([str(pip), "install", "--quiet", "matplotlib"], check=True)

    py = VENV_DIR / "bin" / "python"
    print("[setup] re-launching inside .venv_bench ...")
    os.execv(str(py), [str(py), str(Path(__file__).resolve()), *sys.argv[1:]])


# --------------------------------------------------------------------------- #
# Environment capture
# --------------------------------------------------------------------------- #
def sh(cmd: list[str]) -> str:
    try:
        out = subprocess.run(cmd, capture_output=True, text=True, timeout=30)
        return (out.stdout + out.stderr).strip()
    except Exception:
        return ""


def read_file(path: str) -> str:
    try:
        return Path(path).read_text()
    except OSError:
        return ""


def machine_info() -> dict[str, str]:
    info: dict[str, str] = {}

    os_release = read_file("/etc/os-release")
    m = re.search(r'PRETTY_NAME="?(.*?)"?\n', os_release)
    info["OS"] = m.group(1) if m else platform.system()

    info["Kernel"] = f"{platform.system()} {platform.release()}"
    info["Architecture"] = platform.machine()

    lscpu = sh(["lscpu"])
    def lscpu_field(name: str) -> str:
        mm = re.search(rf"^{re.escape(name)}:\s*(.+)$", lscpu, re.MULTILINE)
        return mm.group(1).strip() if mm else ""

    info["CPU"] = lscpu_field("Model name") or platform.processor() or "unknown"
    cores = lscpu_field("Core(s) per socket")
    sockets = lscpu_field("Socket(s)")
    cpus = lscpu_field("CPU(s)")
    if cores and sockets:
        try:
            total_cores = int(cores) * int(sockets)
            info["Cores / Threads"] = f"{total_cores} cores / {cpus} threads"
        except ValueError:
            info["Cores / Threads"] = cpus
    elif cpus:
        info["Cores / Threads"] = f"{cpus} threads"

    cpu_max = lscpu_field("CPU max MHz")
    if cpu_max:
        try:
            info["CPU max"] = f"{float(cpu_max) / 1000:.2f} GHz"
        except ValueError:
            pass

    meminfo = read_file("/proc/meminfo")
    mm = re.search(r"MemTotal:\s*(\d+)\s*kB", meminfo)
    if mm:
        info["Memory"] = f"{int(mm.group(1)) / 1024 / 1024:.1f} GiB total"

    info["Date"] = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M UTC")
    return info


def toolchain_info() -> dict[str, str]:
    info: dict[str, str] = {}

    zon = read_file(str(ROOT / "build.zig.zon"))
    mver = re.search(r'\.version\s*=\s*"([^"]+)"', zon)
    dusk_ver = mver.group(1) if mver else "unknown"
    commit = sh(["git", "-C", str(ROOT), "rev-parse", "--short", "HEAD"]) or "?"
    zig_ver = sh(["zig", "version"]) or "?"
    info["Dusk"] = (f"{dusk_ver} (git {commit}) — built with Zig {zig_ver}, "
                    f"ReleaseFast")
    if shutil.which("zig"):
        info["Zig (native)"] = f"{zig_ver} — native baseline, build-exe -OReleaseFast"

    jv = sh(["java", "-version"]).splitlines()
    java_mode = ("precompiled with javac, JVM startup included"
                 if JAVA_MODE == "precompiled"
                 else "single-file source mode (compile included)")
    info["Java"] = ((jv[0] if jv else "?").replace('"', "") + f" — {java_mode}")
    info["Node.js"] = sh(["node", "--version"])
    info["Python 3"] = sh(["python3", "--version"])
    info["Ruby"] = (sh(["ruby", "--version"]).split(" [")[0])
    info["Lua"] = sh(["lua", "-v"]).splitlines()[0] if sh(["lua", "-v"]) else "?"
    info["hyperfine"] = sh(["hyperfine", "--version"])
    return info


# --------------------------------------------------------------------------- #
# Build / file resolution
# --------------------------------------------------------------------------- #
def build_dusk() -> None:
    print("[build] zig build -Doptimize=ReleaseFast ...")
    res = subprocess.run(
        ["zig", "build", "-Doptimize=ReleaseFast"], cwd=str(ROOT),
        capture_output=True, text=True,
    )
    if res.returncode != 0:
        print(res.stdout)
        print(res.stderr)
        raise SystemExit("[build] Dusk build failed.")
    print("[build] ok ->", DUSK_BIN)


def prepare_java() -> None:
    """Precompile Java sources with javac when available.

    Sets the module-level JAVA_MODE to "precompiled" on success, otherwise
    leaves it as "source" (single-file source mode) as a JRE-only fallback.
    """
    global JAVA_MODE
    if not shutil.which("javac"):
        print("[java] javac not found — falling back to single-file source mode "
              "(compile cost is included in timings).")
        JAVA_MODE = "source"
        return

    JAVA_BUILD.mkdir(parents=True, exist_ok=True)
    sources = [str(BENCH_DIR / "java" / f"{c}.java") for c in JAVA_CLASS.values()]
    print("[java] javac -d benchmarks/build/java ...")
    res = subprocess.run(["javac", "-d", str(JAVA_BUILD), *sources],
                         cwd=str(ROOT), capture_output=True, text=True)
    if res.returncode != 0:
        print(res.stdout)
        print(res.stderr)
        raise SystemExit("[java] compilation failed.")
    JAVA_MODE = "precompiled"
    print("[java] ok -> benchmarks/build/java (benchmarking precompiled classes)")


def prepare_zig() -> None:
    """Compile the Zig native-baseline programs with -OReleaseFast."""
    global ZIG_OK
    if not shutil.which("zig"):
        print("[zig] zig not found — native baseline skipped.")
        ZIG_OK = False
        return

    ZIG_BUILD.mkdir(parents=True, exist_ok=True)
    print("[zig] zig build-exe -OReleaseFast ...")
    for bench in BENCHMARKS:
        src = BENCH_DIR / "zig" / f"{bench['key']}.zig"
        out = ZIG_BUILD / bench["key"]
        res = subprocess.run(
            ["zig", "build-exe", "-OReleaseFast",
             f"-femit-bin={out}", str(src)],
            cwd=str(ROOT), capture_output=True, text=True)
        if res.returncode != 0:
            print(res.stdout)
            print(res.stderr)
            raise SystemExit(f"[zig] compilation of {bench['key']}.zig failed.")
    ZIG_OK = True
    print("[zig] ok -> benchmarks/build/zig (native baseline)")


def bench_file(lang: dict, bench_key: str) -> Path:
    if lang["key"] == "java":
        name = f"{JAVA_CLASS[bench_key]}.java"
    else:
        name = f"{bench_key}.{lang['ext']}"
    return BENCH_DIR / lang["key"] / name


def available_languages() -> list[dict]:
    langs = []
    for lang in LANGUAGES:
        if lang["key"] == "dusk":
            langs.append(lang)
            continue
        if lang["key"] == "zig":
            if ZIG_OK:
                langs.append(lang)
            else:
                print("[warn] Zig native baseline unavailable — skipped.")
            continue
        if shutil.which(lang["tool"]):
            langs.append(lang)
        else:
            print(f"[warn] {lang['label']} ({lang['tool']}) not found — skipped.")
    return langs


def build_command(lang: dict, bench_key: str) -> list[str]:
    if lang["key"] == "java" and JAVA_MODE == "precompiled":
        return ["java", "-cp", str(JAVA_BUILD), JAVA_CLASS[bench_key]]
    if lang["key"] == "zig":
        return [str(ZIG_BUILD / bench_key)]
    f = str(bench_file(lang, bench_key))
    return [part.replace("{file}", f) for part in lang["cmd"]]


# --------------------------------------------------------------------------- #
# Correctness
# --------------------------------------------------------------------------- #
def matches(expected: str, actual: str, numeric: bool) -> bool:
    actual = actual.strip()
    if numeric:
        try:
            return abs(float(actual) - float(expected)) < 1e-6
        except ValueError:
            return False
    return actual == expected


def correctness_pass(langs: list[dict]) -> bool:
    print("\n[verify] correctness pass")
    ok = True
    for bench in BENCHMARKS:
        for lang in langs:
            cmd = build_command(lang, bench["key"])
            res = subprocess.run(cmd, cwd=str(ROOT), capture_output=True, text=True)
            out = res.stdout.strip()
            good = res.returncode == 0 and matches(bench["expected"], out, bench["numeric"])
            mark = "ok " if good else "FAIL"
            print(f"  [{mark}] {bench['key']:<10} {lang['label']:<9} -> {out!r}")
            ok = ok and good
    return ok


# --------------------------------------------------------------------------- #
# Benchmarking
# --------------------------------------------------------------------------- #
def run_hyperfine(bench_key: str, langs: list[dict]) -> dict:
    RESULTS_DIR.mkdir(exist_ok=True)
    json_path = RESULTS_DIR / f"{bench_key}.json"
    cmd = ["hyperfine", "-N", "--warmup", str(WARMUP),
           "--min-runs", str(MIN_RUNS),
           "--export-json", str(json_path)]
    for lang in langs:
        cmd += ["-n", lang["label"], " ".join(build_command(lang, bench_key))]
    print(f"\n[bench] {bench_key}")
    subprocess.run(cmd, cwd=str(ROOT), check=True)
    return json.loads(json_path.read_text())


# --------------------------------------------------------------------------- #
# Memory (peak RSS via /usr/bin/time -f %M)
# --------------------------------------------------------------------------- #
def peak_rss_kb(cmd: list[str]) -> int | None:
    """Run cmd once under /usr/bin/time, return peak RSS in KiB."""
    res = subprocess.run([GNU_TIME, "-f", "%M", *cmd], cwd=str(ROOT),
                         stdout=subprocess.DEVNULL, stderr=subprocess.PIPE,
                         text=True)
    if res.returncode != 0:
        return None
    last = res.stderr.strip().splitlines()[-1].strip() if res.stderr.strip() else ""
    return int(last) if last.isdigit() else None


def measure_memory(langs: list[dict]) -> dict:
    """Peak RSS (MiB), median of MEM_RUNS, per benchmark and language."""
    RESULTS_DIR.mkdir(exist_ok=True)
    if not Path(GNU_TIME).exists():
        print(f"[mem] {GNU_TIME} not found — skipping memory pass.")
        return {}

    print("\n[mem] peak RSS pass (median of %d runs)" % MEM_RUNS)
    data: dict = {}
    for bench in BENCHMARKS:
        data[bench["key"]] = {}
        for lang in langs:
            cmd = build_command(lang, bench["key"])
            samples = [peak_rss_kb(cmd) for _ in range(MEM_RUNS)]
            samples = sorted(s for s in samples if s is not None)
            if not samples:
                continue
            median_kb = samples[len(samples) // 2]
            mib = median_kb / 1024
            data[bench["key"]][lang["label"]] = {
                "peak_mib": mib, "samples_kb": samples,
            }
            print(f"  {bench['key']:<10} {lang['label']:<9} {mib:8.1f} MiB")
    (RESULTS_DIR / "memory.json").write_text(json.dumps(data, indent=2))
    return data


# --------------------------------------------------------------------------- #
# Charts
# --------------------------------------------------------------------------- #
def fmt_time(seconds: float) -> str:
    if seconds < 1e-3:
        return f"{seconds * 1e6:.1f} µs"
    if seconds < 1.0:
        return f"{seconds * 1e3:.1f} ms"
    return f"{seconds:.3f} s"


def fmt_mem(mib: float) -> str:
    if mib >= 1024:
        return f"{mib / 1024:.2f} GiB"
    return f"{mib:.1f} MiB"


def chart_benchmark(bench: dict, data: dict) -> str:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    rows = sorted(data["results"], key=lambda r: r["mean"])
    names = [r["command"] for r in rows]
    means = [r["mean"] for r in rows]
    errs = [r.get("stddev") or 0.0 for r in rows]
    colors = ["#7c4dff" if n == "Dusk" else "#4a90d9" for n in names]

    fig, ax = plt.subplots(figsize=(8, 4.2))
    bars = ax.barh(names, means, xerr=errs, color=colors,
                   error_kw={"ecolor": "#555", "capsize": 3})
    ax.invert_yaxis()
    ax.set_xscale("log")
    ax.set_xlabel("mean time (s, log scale)")
    ax.set_title(f"{bench['title']} — lower is faster")
    for bar, mean in zip(bars, means):
        ax.text(bar.get_width(), bar.get_y() + bar.get_height() / 2,
                "  " + fmt_time(mean), va="center", ha="left", fontsize=9)
    ax.margins(x=0.18)
    fig.tight_layout()
    out = ASSETS_DIR / f"{bench['key']}.png"
    fig.savefig(out, dpi=130)
    plt.close(fig)
    return out.name


def chart_summary(all_data: dict) -> str:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    labels = [b["title"] for b in BENCHMARKS]
    langs_present: list[str] = []
    for b in BENCHMARKS:
        for r in all_data[b["key"]]["results"]:
            if r["command"] not in langs_present:
                langs_present.append(r["command"])

    rel: dict[str, list[float]] = {l: [] for l in langs_present}
    for b in BENCHMARKS:
        results = {r["command"]: r["mean"] for r in all_data[b["key"]]["results"]}
        baseline = results.get(BASELINE) or min(results.values())
        for l in langs_present:
            rel[l].append(results.get(l, float("nan")) / baseline if l in results else float("nan"))

    fig, ax = plt.subplots(figsize=(10, 5))
    n = len(langs_present)
    width = 0.8 / n
    xs = list(range(len(labels)))
    palette = ["#7c4dff", "#26a69a", "#4a90d9", "#43a047", "#fb8c00",
               "#e53935", "#00acc1"]
    for i, l in enumerate(langs_present):
        offsets = [x + (i - (n - 1) / 2) * width for x in xs]
        is_dusk = l == "Dusk"
        ax.bar(offsets, rel[l], width=width, label=l,
               color=palette[i % len(palette)],
               edgecolor="black" if is_dusk else "none",
               linewidth=1.5 if is_dusk else 0,
               zorder=3 if is_dusk else 2)
    ax.set_yscale("log")
    ax.set_ylabel(f"slowdown vs {BASELINE} (×, log scale)")
    ax.set_title(f"Relative performance per benchmark (1× = {BASELINE})")
    ax.set_xticks(xs)
    ax.set_xticklabels(labels, rotation=12, ha="right")
    ax.legend(ncol=n, fontsize=8)
    ax.grid(axis="y", linestyle=":", alpha=0.4)
    fig.tight_layout()
    out = ASSETS_DIR / "summary.png"
    fig.savefig(out, dpi=130)
    plt.close(fig)
    return out.name


def chart_memory(bench: dict, mem: dict) -> str:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    items = sorted(mem.items(), key=lambda kv: kv[1]["peak_mib"])
    names = [k for k, _ in items]
    vals = [v["peak_mib"] for _, v in items]
    colors = ["#7c4dff" if n == "Dusk" else "#26a69a" for n in names]

    fig, ax = plt.subplots(figsize=(8, 4.2))
    bars = ax.barh(names, vals, color=colors)
    ax.invert_yaxis()
    ax.set_xscale("log")
    ax.set_xlabel("peak resident memory (MiB, log scale)")
    ax.set_title(f"{bench['title']} — peak memory (lower is better)")
    for bar, v in zip(bars, vals):
        ax.text(bar.get_width(), bar.get_y() + bar.get_height() / 2,
                "  " + fmt_mem(v), va="center", ha="left", fontsize=9)
    ax.margins(x=0.18)
    fig.tight_layout()
    out = ASSETS_DIR / f"{bench['key']}_mem.png"
    fig.savefig(out, dpi=130)
    plt.close(fig)
    return out.name


def chart_summary_mem(mem_data: dict) -> str:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    labels = [b["title"] for b in BENCHMARKS]
    langs_present: list[str] = []
    for b in BENCHMARKS:
        for l in mem_data.get(b["key"], {}):
            if l not in langs_present:
                langs_present.append(l)

    fig, ax = plt.subplots(figsize=(10, 5))
    n = len(langs_present)
    width = 0.8 / max(n, 1)
    xs = list(range(len(labels)))
    palette = ["#7c4dff", "#26a69a", "#4a90d9", "#43a047", "#fb8c00",
               "#e53935", "#00acc1"]
    for i, l in enumerate(langs_present):
        vals = [mem_data.get(b["key"], {}).get(l, {}).get("peak_mib", float("nan"))
                for b in BENCHMARKS]
        offsets = [x + (i - (n - 1) / 2) * width for x in xs]
        is_dusk = l == "Dusk"
        ax.bar(offsets, vals, width=width, label=l,
               color=palette[i % len(palette)],
               edgecolor="black" if is_dusk else "none",
               linewidth=1.5 if is_dusk else 0,
               zorder=3 if is_dusk else 2)
    ax.set_yscale("log")
    ax.set_ylabel("peak memory (MiB, log scale)")
    ax.set_title("Peak memory per benchmark (lower is better)")
    ax.set_xticks(xs)
    ax.set_xticklabels(labels, rotation=12, ha="right")
    ax.legend(ncol=n, fontsize=8)
    ax.grid(axis="y", linestyle=":", alpha=0.4)
    fig.tight_layout()
    out = ASSETS_DIR / "summary_mem.png"
    fig.savefig(out, dpi=130)
    plt.close(fig)
    return out.name


# --------------------------------------------------------------------------- #
# Markdown report
# --------------------------------------------------------------------------- #
def md_table(headers: list[str], rows: list[list[str]]) -> str:
    line = "| " + " | ".join(headers) + " |\n"
    line += "| " + " | ".join("---" for _ in headers) + " |\n"
    for r in rows:
        line += "| " + " | ".join(r) + " |\n"
    return line


def write_markdown(machine: dict, tools: dict, all_data: dict,
                   bench_imgs: dict, summary_img: str,
                   mem_data: dict, mem_imgs: dict, summary_mem_img: str) -> None:
    has_mem = bool(mem_data)
    out = ["# Benchmark\n",
           "Cross-language micro-benchmarks tracking **Dusk** against a native "
           "**Zig** baseline and five mainstream runtimes. Wall time is "
           "measured externally with "
           "[`hyperfine`](https://github.com/sharkdp/hyperfine) and peak memory "
           "with GNU `/usr/bin/time`; the benchmark programs contain **no "
           "timing or instrumentation code**. This benchmark is not an evidence "
           "that X is faster than Y, its main purpose is to track Dusk performance "
           "specially on versions change, so we can easily notice performance problems/gains.\n",
           "Regenerate this file with:\n",
           "```bash\npython3 run_benchmarks.py\n```\n",
           "## Machine\n",
           md_table(["Field", "Value"], [[k, v] for k, v in machine.items()]),
           "> **Note:** This benchmark generation script and the algorithms were "
           "AI-generated. I haven't deeply reviewed them, so they may contain "
           "errors.\n",
           "## Language toolchains\n",
           md_table(["Language", "Version / mode"],
                    [[f"**{k}**", f"**{v}**"] if k == "Dusk" else [k, v]
                     for k, v in tools.items()]),
           "## Methodology\n",
           f"- Wall time is sampled by hyperfine with `--warmup {WARMUP} "
           f"--min-runs {MIN_RUNS} -N` (no intermediate shell).\n"
           f"- Peak memory is the resident-set high-water mark from "
           f"`/usr/bin/time -f %M`, reported as the **median of {MEM_RUNS} "
           f"runs**. It includes the runtime/interpreter footprint (JVM, V8, "
           f"CPython, the Dusk VM, …) plus the workload.\n"
           "- **Zig (native)** is the baseline: compiled with `zig build-exe "
           "-OReleaseFast`. The **Relative** column and the summary charts are "
           "normalised to it (Zig = 1.00×, i.e. \"× slower than native\").\n"
           "- No external libraries are used by any benchmark program.\n"
           "- Every program is verified to print the expected result before "
           "measuring.\n",
           "## Results\n"]

    for bench in BENCHMARKS:
        data = all_data[bench["key"]]
        rows = sorted(data["results"], key=lambda r: r["mean"])
        means = {r["command"]: r["mean"] for r in rows}
        baseline = means.get(BASELINE) or rows[0]["mean"]
        bmem = mem_data.get(bench["key"], {}) if has_mem else {}
        headers = ["Language", "Mean", "Std dev", "Min", "Max", "Relative"]
        if has_mem:
            headers.append("Peak mem")
        table_rows = []
        for r in rows:
            rel = r["mean"] / baseline
            row = [
                r["command"],
                fmt_time(r["mean"]),
                fmt_time(r.get("stddev") or 0.0),
                fmt_time(r["min"]),
                fmt_time(r["max"]),
                "1.00×" if abs(rel - 1.0) < 0.005 else f"{rel:.2f}×",
            ]
            if has_mem:
                m = bmem.get(r["command"])
                row.append(fmt_mem(m["peak_mib"]) if m else "—")
            if r["command"] == "Dusk":
                row = [f"**{c}**" for c in row]
            table_rows.append(row)
        out.append(f"### {bench['title']} (`{bench['key']}`)\n")
        out.append(bench["desc"] + "\n")
        out.append(f"Expected output: `{bench['expected']}`\n")
        out.append(md_table(headers, table_rows))
        out.append(f"![{bench['title']}](assets/{bench_imgs[bench['key']]})\n")
        if has_mem and bench["key"] in mem_imgs:
            out.append(f"![{bench['title']} memory]"
                       f"(assets/{mem_imgs[bench['key']]})\n")

    out.append("## Time summary\n")
    out.append(f"Slowdown of each language relative to the **{BASELINE}** "
               "baseline in each benchmark (log scale).\n")
    out.append(f"![Time summary](assets/{summary_img})\n")

    if has_mem and summary_mem_img:
        out.append("## Memory summary\n")
        out.append("Peak resident memory per benchmark (log scale).\n")
        out.append(f"![Memory summary](assets/{summary_mem_img})\n")

    REPORT_MD.write_text("\n".join(out))
    print("\n[report] wrote benchmark.md")


# --------------------------------------------------------------------------- #
# Main
# --------------------------------------------------------------------------- #
def render_report(all_data: dict, mem_data: dict) -> None:
    print("\n[charts] rendering ...")
    bench_imgs = {b["key"]: chart_benchmark(b, all_data[b["key"]])
                  for b in BENCHMARKS}
    summary_img = chart_summary(all_data)

    mem_imgs: dict = {}
    summary_mem_img = ""
    if mem_data:
        mem_imgs = {b["key"]: chart_memory(b, mem_data[b["key"]])
                    for b in BENCHMARKS if mem_data.get(b["key"])}
        summary_mem_img = chart_summary_mem(mem_data)

    write_markdown(machine_info(), toolchain_info(), all_data,
                   bench_imgs, summary_img,
                   mem_data, mem_imgs, summary_mem_img)
    print("[done]")


def main() -> None:
    global JAVA_MODE
    parser = argparse.ArgumentParser(description="Dusk benchmark runner")
    parser.add_argument("--no-build", action="store_true",
                        help="skip building the Dusk binary")
    parser.add_argument("--no-run", action="store_true",
                        help="skip everything (build/compile/verify/benchmark) "
                             "and only re-render charts + benchmark.md from the "
                             "cached results/*.json")
    parser.add_argument("--no-mem", action="store_true",
                        help="skip the peak-memory pass")
    parser.add_argument("--no-verify", action="store_true",
                        help="skip the correctness pass")
    args = parser.parse_args()

    ensure_deps()
    ASSETS_DIR.mkdir(exist_ok=True)
    RESULTS_DIR.mkdir(exist_ok=True)

    # Fast path: reuse cached results, re-render charts + markdown only.
    if args.no_run:
        if JAVA_BUILD.exists() and any(JAVA_BUILD.glob("*.class")):
            JAVA_MODE = "precompiled"
        all_data: dict[str, dict] = {}
        for bench in BENCHMARKS:
            jp = RESULTS_DIR / f"{bench['key']}.json"
            if not jp.exists():
                raise SystemExit(
                    f"[no-run] missing {jp} — run once without --no-run first.")
            all_data[bench["key"]] = json.loads(jp.read_text())
        mem_data: dict = {}
        mem_path = RESULTS_DIR / "memory.json"
        if not args.no_mem and mem_path.exists():
            mem_data = json.loads(mem_path.read_text())
        print("[no-run] reusing cached results — re-rendering only.")
        render_report(all_data, mem_data)
        return

    if not args.no_build:
        build_dusk()
    elif not DUSK_BIN.exists():
        build_dusk()

    prepare_java()
    prepare_zig()

    langs = available_languages()

    if not args.no_verify and not correctness_pass(langs):
        raise SystemExit("[verify] correctness pass failed — aborting.")

    all_data = {}
    for bench in BENCHMARKS:
        all_data[bench["key"]] = run_hyperfine(bench["key"], langs)

    mem_data = {}
    if not args.no_mem:
        mem_data = measure_memory(langs)

    render_report(all_data, mem_data)


if __name__ == "__main__":
    main()
