from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import time
from datetime import datetime, timezone

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))
from bench import build, summarize
from run_benchmarks import machine_info

SOURCES = ROOT / 'benchmarks/json_cross'
BUILD = ROOT / 'benchmarks/build/json_cross'
WORKLOADS = {'json_decode': '5200000', 'json_decode_bulk': '15104000', 'json_encode': '1000', 'json_defaults': '4700000'}
LABELS = {'native': 'Dusk native', 'reflection': 'Dusk reflection', 'go': 'Go encoding/json', 'java': 'Java Jackson', 'bun': 'Bun JSON', 'python': 'Python json', 'ruby': 'Ruby JSON'}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--bun', default='bun')
    parser.add_argument('--jackson-dir', type=Path, required=True)
    parser.add_argument('--runs', type=int, default=5)
    parser.add_argument('--warmup', type=int, default=1)
    parser.add_argument('--only', nargs='+', choices=LABELS, default=list(LABELS))
    parser.add_argument('--workloads', nargs='+', choices=WORKLOADS, default=list(WORKLOADS))
    parser.add_argument('--no-build', action='store_true')
    parser.add_argument('--save', default='json-cross-language')
    args = parser.parse_args()
    if args.runs < 2 or args.warmup < 0:
        parser.error('use at least two measured runs and nonnegative warmups')
    deps = args.jackson_dir.resolve()
    jars = [deps / f'jackson-{name}.jar' for name in ('annotations', 'core', 'databind')]
    for jar in jars:
        if not jar.is_file():
            parser.error(f'missing {jar}')
    bun = shutil.which(args.bun)
    if not bun:
        parser.error('Bun executable not found')
    BUILD.mkdir(parents=True, exist_ok=True)
    if not args.no_build:
        build(ROOT, BUILD / 'dusk-native')
        build(ROOT, BUILD / 'dusk-reflection', ['-Djson-backend=reflection'])
        go_environment = dict(os.environ, GOCACHE=str(BUILD / 'go-cache'))
        subprocess.run(['go', 'build', '-o', str(BUILD / 'codec-go'), str(SOURCES / 'codec.go')], check=True, env=go_environment)
        subprocess.run(['javac', '-cp', str(deps / '*'), '-d', str(BUILD), str(SOURCES / 'JsonCodec.java')], check=True)
    commands = {
        'native': [str(BUILD / 'dusk-native')],
        'reflection': [str(BUILD / 'dusk-reflection')],
        'go': [str(BUILD / 'codec-go')],
        'java': ['java', '-cp', str(BUILD) + os.pathsep + str(deps / '*'), 'JsonCodec'],
        'bun': [bun, str(SOURCES / 'codec.js')],
        'python': [sys.executable, str(SOURCES / 'codec.py')],
        'ruby': ['ruby', str(SOURCES / 'codec.rb')],
    }
    versions = {}
    for name, command in {'go': ['go', 'version'], 'java': ['java', '-version'], 'bun': [bun, '--version'], 'python': [sys.executable, '--version'], 'ruby': ['ruby', '--version'], 'ruby_json': ['ruby', '-rjson', '-e', 'puts JSON::VERSION']}.items():
        outcome = subprocess.run(command, capture_output=True, text=True, check=True)
        versions[name] = (outcome.stdout + outcome.stderr).strip()
    versions['jackson'] = 'databind/core 2.20.1; annotations 2.20'
    source_paths = sorted(SOURCES.glob('*')) + [Path(__file__).resolve()] + sorted((ROOT / 'benchmarks/dusk').glob('json_*.dsk')) + [ROOT / 'benchmarks/dusk/support/json_model.dsk']
    result = {
        'commit': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
        'dirty': bool(subprocess.check_output(['git', 'status', '--porcelain', '--untracked-files=normal'], cwd=ROOT, text=True).strip()),
        'date': datetime.now(timezone.utc).strftime('%Y-%m-%dT%H:%M:%SZ'),
        'machine': machine_info(),
        'versions': versions,
        'settings': {'runs': args.runs, 'warmup': args.warmup, 'order': 'rotate starting runtime each round', 'scope': 'fresh process, compilation of Go/Java/Dusk executable excluded; Dusk source compilation included', 'commands': commands},
        'source_sha256': {str(p.relative_to(ROOT)): digest(p) for p in source_paths if p.is_file()},
        'dependency_sha256': {p.name: digest(p) for p in jars},
        'binary_sha256': {name: digest(BUILD / name) for name in ('dusk-native', 'dusk-reflection', 'codec-go')},
        'benchmarks': {},
    }
    destination = ROOT / 'benchmarks/history' / f'{args.save}.json'
    for workload in args.workloads:
        samples = {name: [] for name in args.only}
        print(f'[bench] {workload}: {args.runs} samples + {args.warmup} warmups', flush=True)
        for round_index in range(args.runs + args.warmup):
            offset = round_index % len(args.only)
            order = args.only[offset:] + args.only[:offset]
            for name in order:
                command = commands[name] + ([str(ROOT / 'benchmarks/dusk' / f'{workload}.dsk')] if name in ('native', 'reflection') else [workload])
                elapsed, peak, output = run_once(command)
                if output.strip() != WORKLOADS[workload]:
                    raise RuntimeError(f'{name} {workload}: expected {WORKLOADS[workload]}, got {output!r}')
                print(f'  {round_index + 1}/{args.runs + args.warmup} {name}: {elapsed * 1000:.2f} ms', flush=True)
                if round_index >= args.warmup:
                    samples[name].append({'elapsed_ms': elapsed * 1000, 'peak_mem_kib': peak, 'output': output.strip()})
        result['benchmarks'][workload] = {}
        for name, values in samples.items():
            summary = summarize([value['elapsed_ms'] / 1000 for value in values], [value['peak_mem_kib'] for value in values])
            result['benchmarks'][workload][name] = dict(summary, samples=values)
        destination.write_text(json.dumps(result, indent=2) + '\n')
        print(f'[saved] {destination}', flush=True)
    for workload, measurements in result['benchmarks'].items():
        print(workload)
        for name, stats in measurements.items():
            print(f"  {LABELS[name]}: {stats['median_ms']:.2f} ms; noisy={stats['noisy']}")


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def run_once(command):
    start = time.perf_counter()
    with subprocess.Popen(command, cwd=ROOT, stdout=subprocess.PIPE, stderr=subprocess.PIPE) as process:
        output = process.stdout.read()
        errors = process.stderr.read()
        _, status, usage = os.wait4(process.pid, 0)
        elapsed = time.perf_counter() - start
        process.returncode = os.waitstatus_to_exitcode(status)
        if process.returncode != 0:
            raise RuntimeError(f'{command}: exit {process.returncode}: {errors.decode()}')
    return elapsed, usage.ru_maxrss, output.decode()


if __name__ == '__main__':
    main()
