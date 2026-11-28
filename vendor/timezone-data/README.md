# Bundled timezone data

This local build dependency contains generated IANA tzdb 2026f data and the
CLDR release-48 global Windows mappings. Dusk builds embed this data directly;
named-zone support requires neither a host timezone database nor a data
generator. System-local discovery separately reads the host configuration. Generation tooling stays in the separate `dusk-timezone` repository.

`manifest.json` records exact upstream URLs, SHA-256 hashes, the sorted index,
and Windows mappings. `zones.bin` contains 344 deduplicated slim TZif payloads
for 597 names and aliases. `root.zig` exposes the same name/offset/length index
and bytes to the runtime. Both upstream licenses are included.

The generator's deterministic `timezone-data.tar` has SHA-256
`748253319256190dbfd2ec52bbdedba1a7bdbe77d09e89986a7710d924500cd4`.
The packed `zones.bin` has SHA-256
`e9e0461b093677d50c19c49a8b25928c2ed0cb80fd109b7c1e34c039b87f110e`.

To update, use `read.md` in the tooling repository: verify the pinned archives,
prepare fresh outputs twice, compare archive hashes, and run its independent
verification. From that repository, for example:

```sh
python3 prepare.py --archives /tmp/dusk-tz-archives --output /tmp/dusk-tz-a
python3 prepare.py --archives /tmp/dusk-tz-archives --output /tmp/dusk-tz-b
cmp /tmp/dusk-tz-a/timezone-data.tar /tmp/dusk-tz-b/timezone-data.tar
python3 verify.py /tmp/dusk-tz-a
```

After verifying the artifact, run the following from the Dusk repository root.
It copies the generated manifest, bytes and licenses, then derives the package
index from the manifest without compiling or downloading timezone sources:

```sh
tar -xf /tmp/dusk-tz-a/timezone-data.tar -C vendor/timezone-data
python3 - <<'PY'
import json
from pathlib import Path

package = Path('vendor/timezone-data')
manifest = json.loads((package / 'manifest.json').read_text())
lines = [
    'pub const version = ' + json.dumps(manifest['tzdb_version']) + ';',
    'pub const bytes = @embedFile("zones.bin");',
    'pub const manifest = @embedFile("manifest.json");',
    '',
    'pub const Zone = struct { name: []const u8, offset: u32, length: u32 };',
    'pub const WindowsZone = struct { windows_name: []const u8, iana_name: []const u8 };',
    '',
    'pub const zones = [_]Zone{',
]
for zone in manifest['zones']:
    lines.append('    .{ .name = ' + json.dumps(zone['name'])
                 + ', .offset = ' + str(zone['offset'])
                 + ', .length = ' + str(zone['length']) + ' },')
lines.append('};')
lines.append('')
lines.append('pub const windows_zones = [_]WindowsZone{')
for windows_name, iana_name in sorted(manifest['windows'].items()):
    lines.append('    .{ .windows_name = ' + json.dumps(windows_name)
                 + ', .iana_name = ' + json.dumps(iana_name) + ' },')
lines.append('};')
(package / 'root.zig').write_text('\n'.join(lines) + '\n')
PY
zig fmt vendor/timezone-data/root.zig
```

Update the package version and this README's hashes, the pinned-version/hash
and count checks in Dusk's timezone tests, and the database version in the
language reference. Review alias/name changes and changed historical/future
rules. Dusk's tests compare every index entry and payload hash with the
manifest and check deduplication and extreme timestamp lookups; run both Debug
and ReleaseFast suites before adopting a new release. The independent corpus
and native recurrence checks supplement those integrity tests. Neither a
corpus pass nor a cross-build replaces target-platform local-discovery tests.

This package is local for now. Publishing a standalone data package can later
replace the path dependency without requiring a sibling repository to build.
