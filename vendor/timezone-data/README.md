# Bundled timezone data

This local build dependency contains generated IANA tzdb 2026f data and the
CLDR release-48 global Windows mappings. Dusk builds embed this data directly;
building or running Dusk requires neither a host timezone database nor a data
generator. Generation tooling stays in the separate `dusk-timezone` repository.

`manifest.json` records exact upstream URLs, SHA-256 hashes, the sorted index,
and Windows mappings. `zones.bin` contains 344 deduplicated slim TZif payloads
for 597 names and aliases. `root.zig` exposes the same name/offset/length index
and bytes to the runtime. Both upstream licenses are included.

The generator's deterministic `timezone-data.tar` has SHA-256
`748253319256190dbfd2ec52bbdedba1a7bdbe77d09e89986a7710d924500cd4`.

To update, use `read.md` in the tooling repository: verify the pinned archives,
prepare fresh outputs twice, compare archive hashes, and run its independent
verification. Replace the manifest, bytes, licenses and matching `root.zig`
index together. Update the package version and the pinned-version/hash checks
in Dusk's timezone tests. Dusk's tests check every indexed payload and alias;
run both Debug and ReleaseFast suites before adopting a new release.

This package is local for now. Publishing a standalone data package can later
replace the path dependency without requiring a sibling repository to build.
