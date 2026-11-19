import argparse
import hashlib
import io
import json
import os
from pathlib import Path
import struct
import subprocess
import tarfile
import xml.etree.ElementTree as ET


REGIONS = (
    "africa", "antarctica", "asia", "australasia", "europe",
    "northamerica", "southamerica", "etcetera", "backward",
)


def main():
    parser = argparse.ArgumentParser(description="Build pinned timezone probe inputs offline")
    parser.add_argument("--archives", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    sources = json.loads(Path(__file__).with_name("sources.json").read_text())
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    source = output / "source"
    source.mkdir(exist_ok=True)
    for item in sources["sources"]:
        archive = args.archives / item["file"]
        digest = hashlib.sha256(archive.read_bytes()).hexdigest()
        if digest != item["sha256"]:
            raise ValueError(f"Checksum mismatch: {archive}")
        if item["file"].endswith(".tar.gz"):
            with tarfile.open(archive) as bundle:
                bundle.extractall(source, filter="data")
    if (source / "version").read_text().strip() != sources["tzdb_version"]:
        raise ValueError("Unexpected tzdb source version")
    environment = dict(os.environ, LC_ALL="C", TZ="UTC", SOURCE_DATE_EPOCH="0")
    subprocess.run(["make", "zic", "zdump"], cwd=source, env=environment, check=True)
    zones = output / "zones"
    if zones.exists():
        raise ValueError("Use a fresh output directory; stale zone files must not enter the package")
    subprocess.run(
        [str(source / "zic"), "-b", "slim", "-d", str(zones), *REGIONS],
        cwd=source, env=environment, check=True,
    )
    entries = []
    packed = bytearray()
    corpus = bytearray()
    by_digest = {}
    for path in sorted(zones.rglob("*")):
        if not path.is_file():
            continue
        name = path.relative_to(zones).as_posix()
        payload = path.read_bytes()
        digest = hashlib.sha256(payload).hexdigest()
        if digest not in by_digest:
            by_digest[digest] = (len(packed), len(payload))
            packed.extend(payload)
        offset, length = by_digest[digest]
        entries.append({"name": name, "offset": offset, "length": length, "sha256": digest})
        encoded_name = name.encode()
        corpus.extend(struct.pack(">II", len(encoded_name), len(payload)))
        corpus.extend(encoded_name)
        corpus.extend(payload)
    windows = {}
    for mapping in ET.parse(args.archives / "windowsZones.xml").iter("mapZone"):
        if mapping.attrib["territory"] != "001":
            continue
        name = mapping.attrib["type"].split()[0]
        if not (zones / name).is_file():
            raise ValueError(f"Windows mapping refers to missing zone {name}")
        windows[mapping.attrib["other"]] = name
    manifest = {
        "tzdb_version": sources["tzdb_version"], "format": "TZif/slim",
        "zone_count": len(entries), "unique_rule_count": len(by_digest),
        "zones": entries, "windows": windows, "sources": sources["sources"],
    }
    manifest_bytes = (json.dumps(manifest, sort_keys=True, indent=2) + "\n").encode()
    (output / "manifest.json").write_bytes(manifest_bytes)
    (output / "zones.bin").write_bytes(packed)
    (output / "probe.bin").write_bytes(corpus)
    with tarfile.open(output / "timezone-data.tar", "w", format=tarfile.USTAR_FORMAT) as archive:
        for name, payload in (
            ("manifest.json", manifest_bytes), ("zones.bin", packed),
            ("LICENSE.tzdb", (source / "LICENSE").read_bytes()),
            ("LICENSE.unicode", (args.archives / "LICENSE.unicode").read_bytes()),
        ):
            entry = tarfile.TarInfo(name)
            entry.size = len(payload)
            entry.mode = 0o644
            entry.mtime = 0
            archive.addfile(entry, io.BytesIO(payload))
    print(json.dumps({
        "version": sources["tzdb_version"], "zones": len(entries),
        "unique_rules": len(by_digest), "rules_bytes": len(packed),
        "windows_mappings": len(windows),
        "package_sha256": hashlib.sha256((output / "timezone-data.tar").read_bytes()).hexdigest(),
    }, sort_keys=True))


if __name__ == "__main__":
    main()
