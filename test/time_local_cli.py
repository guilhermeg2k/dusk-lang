import argparse
import json
import os
from pathlib import Path
import struct
import subprocess
import tempfile


def dusk_string(text):
    escaped = []
    for char in text:
        if char in ('\\', '"', '$'):
            escaped.append('\\' + char)
        elif ord(char) < 32:
            escaped.append('\\u{' + format(ord(char), 'x') + '}')
        else:
            escaped.append(char)
    return '"' + ''.join(escaped) + '"'


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--dusk', type=Path, default=Path('zig-out/bin/dusk'))
    args = parser.parse_args()
    executable = str(args.dusk.resolve())
    source = '''import std/time
import std/io
func main() -> !void
    let zone = time::local_timezone() catch e then
        assert(e == time::TimeError.LocalTimeUnavailable)
        io::println("unavailable")
        return
    end
    io::println(zone.name())
    let epoch = time::from_unix_ms(0)
    io::println("${zone.offset_seconds(epoch)}")
    let text = epoch.to_iso_string(zone) catch e then
        assert(e == time::TimeError.PrecisionLoss)
        io::println("precision")
        return
    end
    io::println(text)
end
'''
    count = 0
    with tempfile.TemporaryDirectory(prefix='dusk-local-time-') as directory:
        root = Path(directory)
        program = root / 'main.dsk'

        def invoke(text, setting, expected=None, should_succeed=True):
            nonlocal count
            program.write_text(text)
            environment = dict(os.environ, TZ=setting, UV_THREADPOOL_SIZE='1')
            completed = subprocess.run([executable, str(program)], env=environment, text=True, capture_output=True, timeout=10)
            if should_succeed:
                assert completed.returncode == 0, completed.stderr
                assert completed.stdout == expected, (setting, completed.stdout, expected, completed.stderr)
            else:
                assert completed.returncode != 0
                assert 'assertion failed' in completed.stderr, completed.stderr
            count += 1

        for setting, expected in (
            ('', 'UTC\n0\n1970-01-01T00:00:00.000Z\n'),
            (':UTC', 'UTC\n0\n1970-01-01T00:00:00.000Z\n'),
            ('UTC0', 'Local\n0\n1970-01-01T00:00:00.000Z\n'),
            ('America/New_York', 'America/New_York\n-18000\n1969-12-31T19:00:00.000-05:00\n'),
            (':US/Eastern', 'US/Eastern\n-18000\n1969-12-31T19:00:00.000-05:00\n'),
            ('<+0530>-5:30', 'Local\n19800\n1970-01-01T05:30:00.000+05:30\n'),
            ('STD-0:00:30', 'Local\n30\nprecision\n'),
        ):
            invoke(source, setting, expected)
        for setting in ('NoSuch/Zone', ':', ':UTC0', 'UTC24', 'STD0DST', 'A' * 5000, str(root / 'missing')):
            invoke(source, setting, 'unavailable\n')

        def static_zone(offset, designation=b'LOCAL\0'):
            return b'TZif\0' + bytes(15) + struct.pack('>6I', 0, 0, 0, 0, 1, len(designation)) + struct.pack('>iBB', offset, 0, 0) + designation

        zone = root / 'zone-ü.tzif'
        payload = static_zone(19800)
        zone.write_bytes(payload)
        expected = 'Local\n19800\n1970-01-01T05:30:00.000+05:30\n'
        invoke(source, str(zone), expected)
        invoke(source, ':' + str(zone), expected)
        manifest = json.loads(Path('vendor/timezone-data/manifest.json').read_text())
        packed = Path('vendor/timezone-data/zones.bin').read_bytes()
        new_york = next(entry for entry in manifest['zones'] if entry['name'] == 'America/New_York')
        named_file = root / 'new-york.tzif'
        named_file.write_bytes(packed[new_york['offset']:new_york['offset'] + new_york['length']])
        invoke(source, str(named_file), 'Local\n-18000\n1969-12-31T19:00:00.000-05:00\n')
        for content in (b'', b'TZif2', static_zone(90000), static_zone(0, b'-00\0'), b'TZif4' + payload[5:]):
            bad = root / 'bad.tzif'
            bad.write_bytes(content)
            invoke(source, str(bad), 'unavailable\n')
        oversized = root / 'oversized.tzif'
        with oversized.open('wb') as output:
            output.truncate(4 * 1024 * 1024 + 1)
        invoke(source, str(oversized), 'unavailable\n')
        invoke(source, str(root), 'unavailable\n')
        if os.name != 'nt':
            fifo = root / 'fifo'
            os.mkfifo(fifo)
            invoke(source, str(fifo), 'unavailable\n')
            link = root / 'link'
            link.symlink_to(zone)
            invoke(source, str(link), expected)
            broken = root / 'broken'
            broken.symlink_to(root / 'absent')
            invoke(source, str(broken), 'unavailable\n')
        snapshot = root / 'snapshot.tzif'
        snapshot.write_bytes(payload)
        cache_source = '''import std/time
import std/fs
func main() -> !void
    let first = try time::local_timezone()
    try fs::delete(PATH)
    let second = try time::local_timezone()
    let epoch = time::from_unix_ms(0)
    assert(first.name() == "Local" and second.name() == "Local")
    assert(first.offset_seconds(epoch) == 19800)
    assert(second.offset_seconds(epoch) == 19800)
end
'''.replace('PATH', dusk_string(str(snapshot)))
        invoke(cache_source, str(snapshot), '')
        retry = root / 'retry.tzif'
        retry.write_bytes(b'bad')
        retry_source = '''import std/time
import std/fs
import std/buffer
func main() -> !void
    let mut has_failed = false
    time::local_timezone() catch e then
        assert(e == time::TimeError.LocalTimeUnavailable)
        has_failed = true
    end
    assert(has_failed)
    try fs::write_bytes(PATH, buffer::from_string(PAYLOAD))
    let zone = try time::local_timezone()
    assert(zone.offset_seconds(time::from_unix_ms(0)) == 19800)
end
'''.replace('PATH', dusk_string(str(retry))).replace('PAYLOAD', dusk_string(payload.decode('ascii')))
        invoke(retry_source, str(retry), '')
        shutdown = '''import std/time
import std/task
func main() -> void
    for 0..15: i do
        let request = spawn(func() -> time::TimeError!time::TimeZone
            return try time::local_timezone()
        end)
    end
    task::sleep(0)
    assert(false)
end
'''
        invoke(shutdown, str(zone), should_succeed=False)
    print(f'{count} local-time CLI checks passed')


if __name__ == '__main__':
    main()
