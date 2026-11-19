import argparse
from datetime import datetime, timedelta, timezone
import hashlib
import io
import json
from pathlib import Path
import re
import struct
import subprocess
from zoneinfo import ZoneInfo

from rules import days_from_civil, parse_recurrence, parse_tzif, year_from_seconds


EPOCH = datetime(1970, 1, 1, tzinfo=timezone.utc)
CYCLE_SECONDS = 146097 * 86400


def main():
    parser = argparse.ArgumentParser(description="Validate the phase-1 timezone prototype against independent zoneinfo")
    parser.add_argument("directory", type=Path)
    args = parser.parse_args()
    manifest = json.loads((args.directory / "manifest.json").read_text())
    packed = (args.directory / "zones.bin").read_bytes()
    checked_rules = set()
    checked_offsets = 0
    checked_extremes = 0
    second_offset_zones = 0
    footer_forms = set()
    corpus = bytearray()
    for entry in manifest["zones"]:
        payload = packed[entry["offset"]:entry["offset"] + entry["length"]]
        if hashlib.sha256(payload).hexdigest() != entry["sha256"]:
            raise AssertionError(f"Bad packed payload: {entry['name']}")
        if entry["sha256"] in checked_rules:
            append_case(corpus, entry["name"], payload, [])
            continue
        checked_rules.add(entry["sha256"])
        zone = parse_tzif(payload)
        reference = ZoneInfo.from_file(io.BytesIO(payload))
        footer_forms.add(payload.split(b"\n")[-2].decode())
        if any(offset % 60 for offset in zone.offsets):
            second_offset_zones += 1
        samples = {0, -1}
        for transition in zone.transitions:
            samples.update((transition - 1, transition, transition + 1))
        for year in (1, 1800, 1900, 2000, 2038, 2100, 2400, 9998):
            for month in range(1, 13):
                samples.add(days_from_civil(year, month, 15) * 86400 + 43200)
        if zone.footer is not None and zone.footer.daylight_offset is not None:
            for year in (2100, 2400, 9998):
                for rule, previous_offset in ((zone.footer.start, zone.footer.standard_offset), (zone.footer.end, zone.footer.daylight_offset)):
                    transition = rule.transition(year, previous_offset)
                    samples.update((transition - 1, transition, transition + 1))
        comparisons = []
        for timestamp in sorted(samples):
            expected = int((EPOCH + timedelta(seconds=timestamp)).astimezone(reference).utcoffset().total_seconds())
            actual = zone.offset_seconds(timestamp)
            if actual != expected:
                raise AssertionError(f"{entry['name']} at {timestamp}: {actual} != {expected}")
            checked_offsets += 1
            comparisons.append((timestamp, expected))
        for milliseconds in (-(1 << 63), (1 << 63) - 1):
            timestamp = milliseconds // 1000
            offset = zone.offset_seconds(timestamp)
            if not -86400 < offset < 86400:
                raise AssertionError("Extreme-date offset is out of range")
            if timestamp > 0 and zone.footer is not None:
                reduced = timestamp - ((year_from_seconds(timestamp) - 2400) // 400) * CYCLE_SECONDS
                expected = int((EPOCH + timedelta(seconds=reduced)).astimezone(reference).utcoffset().total_seconds())
                if offset != expected:
                    raise AssertionError(f"Full-range recurrence differs: {entry['name']}")
            checked_extremes += 1
            comparisons.append((timestamp, offset))
        append_case(corpus, entry["name"], payload, comparisons)
    synthetic_checks = verify_rule_forms(args.directory, corpus)
    (args.directory / "verified.bin").write_bytes(corpus)
    print(json.dumps({
        "zones": len(manifest["zones"]), "unique_rules": len(checked_rules),
        "offset_comparisons": checked_offsets, "extreme_date_checks": checked_extremes,
        "unique_footers": len(footer_forms), "rules_with_second_offsets": second_offset_zones,
        "synthetic_offset_comparisons": synthetic_checks,
    }, sort_keys=True))


def append_case(corpus, name, payload, comparisons):
    name = name.encode()
    corpus.extend(struct.pack(">II", len(name), len(payload)))
    corpus.extend(name)
    corpus.extend(payload)
    corpus.extend(struct.pack(">I", len(comparisons)))
    for timestamp, offset in comparisons:
        corpus.extend(struct.pack(">qi", timestamp, offset))


def verify_rule_forms(directory, corpus):
    count = 0
    for index, text in enumerate((
        "STD0DST,J60/0,J300/0", "STD0DST,59/0,300/0",
        "STD0DST,M3.1.0/-2,M10.5.0/26", "STD0DST,M3.1.0/167,M10.1.0/-167",
        "STD-1DST0,M10.5.0,M3.5.0/1",
    )):
        recurrence = parse_recurrence(text)
        header = b"TZif2" + bytes(15) + struct.pack(">6I", 0, 0, 0, 0, 1, 4)
        block = struct.pack(">iBB", 0, 0, 0) + b"STD\0"
        payload = header + block + header + block + b"\n" + text.encode() + b"\n"
        path = directory.resolve() / f"synthetic-{index}.tzif"
        path.write_bytes(payload)
        reference = subprocess.run(
            [str(directory.resolve() / "source" / "zdump"), "-V", "-c", "1999,2402", str(path)],
            check=True, capture_output=True, text=True,
            env={"LC_ALL": "C", "TZ": "UTC"},
        )
        comparisons = []
        for line in reference.stdout.splitlines():
            match = re.search(r"  (.+?) UT = .*gmtoff=(-?\d+)$", line)
            if match is None:
                raise AssertionError(f"Unexpected zdump output: {line}")
            date = datetime.strptime(match[1], "%a %b %d %H:%M:%S %Y").replace(tzinfo=timezone.utc)
            timestamp = (date - EPOCH).days * 86400 + (date - EPOCH).seconds
            expected = int(match[2])
            actual = recurrence.offset_seconds(timestamp)
            if actual != expected:
                raise AssertionError(f"Synthetic recurrence {text} at {date}: {actual} != {expected}")
            count += 1
            comparisons.append((timestamp, expected))
        if not reference.stdout:
            raise AssertionError(f"No reference transitions generated for {text}")
        append_case(corpus, f"synthetic-{index}", payload, comparisons)
    return count


if __name__ == "__main__":
    main()
