from bisect import bisect_right
from dataclasses import dataclass
import re
import struct


@dataclass(frozen=True)
class DayRule:
    kind: str
    values: tuple[int, ...]
    seconds: int = 7200

    def transition(self, year, previous_offset):
        if self.kind == "M":
            month, week, weekday = self.values
            first = days_from_civil(year, month, 1)
            day = 1 + (weekday - (first + 4) % 7) % 7 + (week - 1) * 7
            next_month = days_from_civil(year + (month == 12), month % 12 + 1, 1)
            if first + day - 1 >= next_month:
                day -= 7
            days = first + day - 1
        else:
            day = self.values[0]
            if self.kind == "J":
                day = day - 1 + (is_leap_year(year) and day >= 60)
            days = days_from_civil(year, 1, 1) + day
        return days * 86400 + self.seconds - previous_offset


@dataclass(frozen=True)
class Recurrence:
    standard_offset: int
    daylight_offset: int | None = None
    start: DayRule | None = None
    end: DayRule | None = None

    def offset_seconds(self, timestamp):
        if self.daylight_offset is None:
            return self.standard_offset
        year = year_from_seconds(timestamp)
        events = []
        for candidate in range(year - 1, year + 2):
            events.append((self.start.transition(candidate, self.standard_offset), self.daylight_offset))
            events.append((self.end.transition(candidate, self.daylight_offset), self.standard_offset))
        return max(event for event in events if event[0] <= timestamp)[1]


@dataclass(frozen=True)
class Zone:
    transitions: tuple[int, ...]
    indices: bytes
    offsets: tuple[int, ...]
    footer: Recurrence | None

    def offset_seconds(self, timestamp):
        has_future_rules = self.footer is not None and (
            not self.transitions or timestamp > self.transitions[-1]
        )
        if has_future_rules:
            return self.footer.offset_seconds(timestamp)
        index = bisect_right(self.transitions, timestamp)
        type_index = self.indices[index - 1] if index else 0
        return self.offsets[type_index]


def parse_tzif(payload):
    def header(position):
        if payload[position:position + 4] != b"TZif":
            raise ValueError("Bad TZif header")
        return struct.unpack_from(">6I", payload, position + 20)

    counts = header(0)
    version = payload[4:5]
    position = 44
    width = 4
    if version in (b"2", b"3"):
        position += counts[3] * 5 + counts[4] * 6 + counts[5] + counts[2] * 8 + counts[0] + counts[1]
        counts = header(position)
        position += 44
        width = 8
    elif version != b"\0":
        raise ValueError("Unsupported TZif version")
    isut_count, isstd_count, leap_count, transition_count, type_count, char_count = counts
    if not 1 <= type_count <= 256:
        raise ValueError("Invalid type count")
    block_size = transition_count * (width + 1) + type_count * 6 + char_count + leap_count * (width + 4) + isut_count + isstd_count
    if position + block_size > len(payload):
        raise ValueError("Truncated TZif block")
    integer_format = "q" if width == 8 else "i"
    transitions = struct.unpack_from(f">{transition_count}{integer_format}", payload, position)
    position += transition_count * width
    indices = payload[position:position + transition_count]
    position += transition_count
    offsets = tuple(struct.unpack_from(">i", payload, position + i * 6)[0] for i in range(type_count))
    if any(index >= type_count for index in indices):
        raise ValueError("Invalid transition type index")
    if any(a >= b for a, b in zip(transitions, transitions[1:])):
        raise ValueError("Unordered transitions")
    footer_position = position + type_count * 6 + char_count + leap_count * (width + 4) + isut_count + isstd_count
    footer = None
    if width == 8:
        suffix = payload[footer_position:]
        if not suffix.startswith(b"\n") or not suffix.endswith(b"\n"):
            raise ValueError("Malformed TZif footer")
        text = suffix[1:-1].decode("ascii")
        footer = parse_recurrence(text) if text else None
    return Zone(transitions, indices, offsets, footer)


def parse_recurrence(text):
    name = r"(?:[A-Za-z]{3,}|<[A-Za-z0-9+\-]{3,}>)"
    offset = r"[+\-]?\d{1,3}(?::\d{2}(?::\d{2})?)?"
    match = re.fullmatch(f"({name})({offset})(?:({name})({offset})?,([^,]+),([^,]+))?", text)
    if match is None:
        raise ValueError(f"Unsupported recurrence: {text}")
    standard_offset = -parse_clock(match[2], 24)
    if match[3] is None:
        return Recurrence(standard_offset)
    daylight_offset = -parse_clock(match[4], 24) if match[4] else standard_offset + 3600
    return Recurrence(standard_offset, daylight_offset, parse_day_rule(match[5]), parse_day_rule(match[6]))


def parse_clock(text, maximum_hour):
    is_negative = text.startswith("-")
    fields = text.lstrip("+-").split(":")
    hour = int(fields[0])
    minute = int(fields[1]) if len(fields) > 1 else 0
    second = int(fields[2]) if len(fields) > 2 else 0
    if hour > maximum_hour or minute > 59 or second > 59:
        raise ValueError("Clock field out of range")
    return (-1 if is_negative else 1) * (hour * 3600 + minute * 60 + second)


def parse_day_rule(text):
    parts = text.split("/")
    if len(parts) > 2:
        raise ValueError("Malformed transition rule")
    seconds = parse_clock(parts[1], 167) if len(parts) == 2 else 7200
    date = parts[0]
    if date.startswith("M"):
        values = tuple(int(field) for field in date[1:].split("."))
        is_valid = len(values) == 3 and 1 <= values[0] <= 12 and 1 <= values[1] <= 5 and 0 <= values[2] <= 6
        if not is_valid:
            raise ValueError("Invalid month/week/day rule")
        return DayRule("M", values, seconds)
    is_julian = date.startswith("J")
    value = int(date[1:] if is_julian else date)
    minimum, maximum = (1, 365) if is_julian else (0, 365)
    if not minimum <= value <= maximum:
        raise ValueError("Invalid ordinal day rule")
    return DayRule("J" if is_julian else "N", (value,), seconds)


def is_leap_year(year):
    return year % 4 == 0 and (year % 100 != 0 or year % 400 == 0)


def days_from_civil(year, month, day):
    year -= month <= 2
    era, year_of_era = divmod(year, 400)
    shifted_month = month - 3 if month > 2 else month + 9
    day_of_year = (153 * shifted_month + 2) // 5 + day - 1
    day_of_era = year_of_era * 365 + year_of_era // 4 - year_of_era // 100 + day_of_year
    return era * 146097 + day_of_era - 719468


def year_from_seconds(timestamp):
    days = timestamp // 86400 + 719468
    era, day_of_era = divmod(days, 146097)
    year_of_era = (day_of_era - day_of_era // 1460 + day_of_era // 36524 - day_of_era // 146096) // 365
    year = year_of_era + era * 400
    day_of_year = day_of_era - (365 * year_of_era + year_of_era // 4 - year_of_era // 100)
    month = (5 * day_of_year + 2) // 153
    return year + (month >= 10)
