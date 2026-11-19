# Timezone data tools

These offline tools compile and validate timezone data. They do not change the
runtime API, download data, run performance benchmarks, or publish packages.
Use Python 3.12 or later, a C99 compiler, make, and the project's pinned Zig.

`sources.json` pins the official IANA source data, matching compiler source,
Windows mapping, and mapping license by URL and SHA-256. Download each listed
file into an archive directory before preparing the data. The preparation
command verifies every checksum before invoking the pinned compiler source.

From the repository root, with fresh output directories:

```sh
python3 tools/timezone/prepare.py --archives /tmp/dusk-tz-archives --output /tmp/dusk-tz-a
python3 tools/timezone/verify.py /tmp/dusk-tz-a
zig run tools/timezone/probe.zig -- /tmp/dusk-tz-a/verified.bin --verify
zig test tools/timezone/recurrence.zig
```

`prepare.py` builds the matching `zic` and `zdump`, compiles slim TZif files
including compatibility aliases, and verifies that every global Windows mapping
resolves to a bundled name. It produces:

- `zones/`: compiled timezone files.
- `manifest.json`: release, source hashes, sorted zone index, payload hashes,
  and Windows mappings.
- `zones.bin`: deduplicated TZif payloads, addressed by index offsets and lengths.
- `timezone-data.tar`: deterministic data archive with manifest, payloads, and
  both data licenses. Archive metadata is normalized; it contains no absolute paths.
- `probe.bin`: every named zone for the standard-library parser compatibility check.
- `source/`: extracted third-party source and host tools, kept outside the repository.

The archive is a data artifact, not a Zig build package. Runtime dependency
integration supplies its build wrapper separately. No generated data or
third-party source is checked into this repository by these commands.

`verify.py` compares a separate recurrence/transition prototype with `zoneinfo`
at explicit transitions, calendar samples, future transitions, and reduced
400-year equivalents of the maximum timestamp. Minimum timestamps exercise the
pre-first-transition type. It also compares synthetic Julian/ordinal/month-rule
cases with the pinned IANA `zdump`. It writes `verified.bin` for the Zig probe.
The probe's i128 arithmetic then evaluates those same vectors natively, including
both signed epoch-millisecond limits. The prototype is not a production reader
for untrusted TZif inputs.

For reproducibility, prepare a second fresh directory and compare archives:

```sh
python3 tools/timezone/prepare.py --archives /tmp/dusk-tz-archives --output /tmp/dusk-tz-b
cmp /tmp/dusk-tz-a/timezone-data.tar /tmp/dusk-tz-b/timezone-data.tar
```

To update data, select versioned upstream URLs, independently verify their
provenance and checksums, update `sources.json`, regenerate both outputs, and
repeat the checks above. Review alias/name changes, missing Windows mappings,
TZif versions, and recurrence differences before adopting a new release.

Sources:

- https://www.iana.org/time-zones
- https://data.iana.org/time-zones/releases/
- https://github.com/unicode-org/cldr/tree/release-48
- https://www.rfc-editor.org/rfc/rfc9636.html
