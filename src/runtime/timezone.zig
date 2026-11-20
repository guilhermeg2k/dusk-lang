const std = @import("std");
const bundle = @import("timezone_data");
const Recurrence = @import("timezone_recurrence.zig").Recurrence;
const named_zone_base: i64 = 86400;

pub const Registry = struct {
    rules: std.AutoHashMapUnmanaged(u32, Rules) = .empty,

    pub fn deinit(self: *Registry, allocator: std.mem.Allocator) void {
        self.rules.deinit(allocator);
        self.* = .{};
    }

    pub fn loadNamed(self: *Registry, allocator: std.mem.Allocator, name: []const u8) !?i64 {
        const index = findZone(name) orelse return null;
        const entry = bundle.zones[index];
        if (!self.rules.contains(entry.offset)) {
            const rules = try Rules.parse(bundle.bytes[entry.offset..][0..entry.length]);
            try self.rules.put(allocator, entry.offset, rules);
        }
        return named_zone_base + @as(i64, @intCast(index));
    }

    fn findZone(name: []const u8) ?usize {
        if (!isValidZoneName(name)) {
            return null;
        }
        var left: usize = 0;
        var right = bundle.zones.len;
        while (left < right) {
            const middle = left + (right - left) / 2;
            switch (std.mem.order(u8, name, bundle.zones[middle].name)) {
                .lt => right = middle,
                .gt => left = middle + 1,
                .eq => return middle,
            }
        }
        return null;
    }

    fn isValidZoneName(name: []const u8) bool {
        const has_valid_length = name.len > 0 and name.len <= 255;
        if (!has_valid_length) {
            return false;
        }
        var has_component = false;
        for (name) |byte| {
            if (byte == '/') {
                if (!has_component) {
                    return false;
                }
                has_component = false;
                continue;
            }
            const is_name_byte = std.ascii.isAlphanumeric(byte) or byte == '_' or byte == '-' or byte == '+';
            if (!is_name_byte) {
                return false;
            }
            has_component = true;
        }
        return has_component;
    }

    pub fn zoneName(_: *const Registry, identifier: i64, buffer: *[32]u8) []const u8 {
        if (identifier >= named_zone_base) {
            return bundle.zones[@intCast(identifier - named_zone_base)].name;
        }
        if (identifier == 0) {
            return "UTC";
        }
        const magnitude = @abs(identifier);
        const sign: u8 = if (identifier < 0) '-' else '+';
        const hours = magnitude / 3600;
        const minutes = magnitude / 60 % 60;
        const seconds = magnitude % 60;
        if (seconds == 0) {
            return std.fmt.bufPrint(buffer, "UTC{c}{d:0>2}:{d:0>2}", .{ sign, hours, minutes }) catch unreachable;
        }
        return std.fmt.bufPrint(buffer, "UTC{c}{d:0>2}:{d:0>2}:{d:0>2}", .{ sign, hours, minutes, seconds }) catch unreachable;
    }

    pub fn offsetSeconds(self: *const Registry, identifier: i64, milliseconds: i64) i32 {
        if (identifier < named_zone_base) {
            return @intCast(identifier);
        }
        const entry = bundle.zones[@intCast(identifier - named_zone_base)];
        return self.rules.get(entry.offset).?.offsetSeconds(@divFloor(milliseconds, 1000));
    }
};

pub const Rules = struct {
    transition_times: []const u8,
    transition_types: []const u8,
    local_types: []const u8,
    time_width: u8,
    recurrence: ?Recurrence,

    pub fn parse(bytes: []const u8) error{InvalidTimeZoneData}!Rules {
        if (bytes.len > 4 * 1024 * 1024) {
            return error.InvalidTimeZoneData;
        }
        var reader = Reader{ .bytes = bytes };
        var header = try Header.read(&reader);
        var time_width: u8 = 4;
        var block = try reader.take(try header.blockLength(time_width));
        var footer: ?[]const u8 = null;
        if (header.version != 0) {
            const version = header.version;
            header = try Header.read(&reader);
            if (header.version != version) {
                return error.InvalidTimeZoneData;
            }
            time_width = 8;
            block = try reader.take(try header.blockLength(time_width));
            const remainder = try reader.take(bytes.len - reader.position);
            const has_footer = remainder.len >= 2 and remainder[0] == '\n' and remainder[remainder.len - 1] == '\n';
            if (!has_footer) {
                return error.InvalidTimeZoneData;
            }
            footer = remainder[1 .. remainder.len - 1];
        }
        if (reader.position != bytes.len) {
            return error.InvalidTimeZoneData;
        }
        var block_reader = Reader{ .bytes = block };
        const transition_times = try block_reader.take(@as(usize, header.transition_count) * time_width);
        const transition_types = try block_reader.take(header.transition_count);
        const local_types = try block_reader.take(@as(usize, header.type_count) * 6);
        const designations = try block_reader.take(header.designation_count);
        const standard_flags = try block_reader.take(header.standard_count);
        const universal_flags = try block_reader.take(header.universal_count);
        var result = Rules{
            .transition_times = transition_times,
            .transition_types = transition_types,
            .local_types = local_types,
            .time_width = time_width,
            .recurrence = null,
        };
        for (0..header.type_count) |index| {
            const local_type = local_types[index * 6 ..][0..6];
            const offset = result.typeOffset(index);
            const has_valid_type = offset >= -89999 and offset <= 93599 and local_type[4] <= 1 and local_type[5] < designations.len;
            if (!has_valid_type) {
                return error.InvalidTimeZoneData;
            }
            if (std.mem.indexOfScalar(u8, designations[local_type[5]..], 0) == null) {
                return error.InvalidTimeZoneData;
            }
            const is_standard = standard_flags.len != 0 and standard_flags[index] == 1;
            if (standard_flags.len != 0 and standard_flags[index] > 1) {
                return error.InvalidTimeZoneData;
            }
            if (universal_flags.len != 0) {
                const has_valid_flag = universal_flags[index] <= 1 and (universal_flags[index] == 0 or is_standard);
                if (!has_valid_flag) {
                    return error.InvalidTimeZoneData;
                }
            }
        }
        for (transition_types, 0..) |type_index, index| {
            if (type_index >= header.type_count) {
                return error.InvalidTimeZoneData;
            }
            const is_out_of_order = index > 0 and result.transitionTime(index - 1) >= result.transitionTime(index);
            if (is_out_of_order) {
                return error.InvalidTimeZoneData;
            }
        }
        if (footer) |text| {
            if (text.len != 0) {
                result.recurrence = Recurrence.parse(text) catch return error.InvalidTimeZoneData;
            }
        }
        if (result.recurrence) |recurrence| {
            if (transition_types.len > 0) {
                const last = transition_types.len - 1;
                const is_consistent = recurrence.offsetSeconds(result.transitionTime(last)) == result.typeOffset(transition_types[last]);
                if (!is_consistent) {
                    return error.InvalidTimeZoneData;
                }
            }
        } else if (transition_types.len != 0) {
            return error.InvalidTimeZoneData;
        }
        return result;
    }

    fn typeOffset(self: Rules, index: usize) i32 {
        return std.mem.readInt(i32, self.local_types[index * 6 ..][0..4], .big);
    }

    fn transitionTime(self: Rules, index: usize) i64 {
        if (self.time_width == 4) {
            return std.mem.readInt(i32, self.transition_times[index * 4 ..][0..4], .big);
        }
        return std.mem.readInt(i64, self.transition_times[index * 8 ..][0..8], .big);
    }

    pub fn offsetSeconds(self: Rules, timestamp: i64) i32 {
        const count = self.transition_types.len;
        const needs_recurrence = self.recurrence != null and (count == 0 or timestamp > self.transitionTime(count - 1));
        if (needs_recurrence) {
            return self.recurrence.?.offsetSeconds(timestamp);
        }
        var left: usize = 0;
        var right = count;
        while (left < right) {
            const middle = left + (right - left) / 2;
            if (self.transitionTime(middle) <= timestamp) {
                left = middle + 1;
            } else {
                right = middle;
            }
        }
        const type_index = if (left == 0) 0 else self.transition_types[left - 1];
        return self.typeOffset(type_index);
    }
};

const Reader = struct {
    bytes: []const u8,
    position: usize = 0,

    fn take(self: *Reader, count: usize) error{InvalidTimeZoneData}![]const u8 {
        if (count > self.bytes.len - self.position) {
            return error.InvalidTimeZoneData;
        }
        const result = self.bytes[self.position..][0..count];
        self.position += count;
        return result;
    }
};

const Header = struct {
    version: u8,
    universal_count: u32,
    standard_count: u32,
    transition_count: u32,
    type_count: u32,
    designation_count: u32,

    fn read(reader: *Reader) error{InvalidTimeZoneData}!Header {
        const bytes = try reader.take(44);
        const has_magic = std.mem.eql(u8, bytes[0..4], "TZif");
        const has_version = bytes[4] == 0 or bytes[4] == '2' or bytes[4] == '3';
        const has_leap_seconds = std.mem.readInt(u32, bytes[28..32], .big) != 0;
        const has_supported_header = has_magic and has_version and !has_leap_seconds;
        if (!has_supported_header) {
            return error.InvalidTimeZoneData;
        }
        const result = Header{
            .version = bytes[4],
            .universal_count = std.mem.readInt(u32, bytes[20..24], .big),
            .standard_count = std.mem.readInt(u32, bytes[24..28], .big),
            .transition_count = std.mem.readInt(u32, bytes[32..36], .big),
            .type_count = std.mem.readInt(u32, bytes[36..40], .big),
            .designation_count = std.mem.readInt(u32, bytes[40..44], .big),
        };
        const has_valid_counts = result.type_count > 0 and result.type_count <= 256 and result.designation_count > 0;
        const has_standard_flags = result.standard_count == 0 or result.standard_count == result.type_count;
        const has_universal_flags = result.universal_count == 0 or result.universal_count == result.type_count;
        const has_valid_header = has_valid_counts and has_standard_flags and has_universal_flags;
        if (!has_valid_header) {
            return error.InvalidTimeZoneData;
        }
        return result;
    }

    fn blockLength(self: Header, width: u8) error{InvalidTimeZoneData}!usize {
        const length = @as(u64, self.transition_count) * (width + 1) + @as(u64, self.type_count) * 6 + self.designation_count + self.standard_count + self.universal_count;
        return std.math.cast(usize, length) orelse error.InvalidTimeZoneData;
    }
};

test "timezone bundle metadata, checksums, aliases and bounded registry" {
    const Manifest = struct {
        tzdb_version: []const u8,
        zone_count: usize,
        unique_rule_count: usize,
        zones: []const struct { name: []const u8, offset: u32, length: u32, sha256: []const u8 },
    };
    const manifest = try std.json.parseFromSlice(Manifest, std.testing.allocator, bundle.manifest, .{ .ignore_unknown_fields = true });
    defer manifest.deinit();
    var bundle_digest: [32]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(bundle.bytes, &bundle_digest, .{});
    try std.testing.expectEqualStrings("e9e0461b093677d50c19c49a8b25928c2ed0cb80fd109b7c1e34c039b87f110e", &std.fmt.bytesToHex(bundle_digest, .lower));
    try std.testing.expectEqualStrings("2026f", bundle.version);
    try std.testing.expectEqualStrings(bundle.version, manifest.value.tzdb_version);
    try std.testing.expectEqual(@as(usize, 597), manifest.value.zone_count);
    try std.testing.expectEqual(manifest.value.zone_count, bundle.zones.len);
    try std.testing.expectEqual(manifest.value.zone_count, manifest.value.zones.len);
    var registry: Registry = .{};
    defer registry.deinit(std.testing.allocator);
    var buffer: [32]u8 = undefined;
    for (bundle.zones, manifest.value.zones, 0..) |entry, recorded, index| {
        try std.testing.expectEqualStrings(entry.name, recorded.name);
        try std.testing.expectEqual(entry.offset, recorded.offset);
        try std.testing.expectEqual(entry.length, recorded.length);
        const is_sorted = index == 0 or std.mem.order(u8, bundle.zones[index - 1].name, entry.name) == .lt;
        try std.testing.expect(is_sorted);
        const payload = bundle.bytes[entry.offset..][0..entry.length];
        var digest: [32]u8 = undefined;
        std.crypto.hash.sha2.Sha256.hash(payload, &digest, .{});
        try std.testing.expectEqualStrings(recorded.sha256, &std.fmt.bytesToHex(digest, .lower));
        const identifier = (try registry.loadNamed(std.testing.allocator, entry.name)).?;
        try std.testing.expectEqualStrings(entry.name, registry.zoneName(identifier, &buffer));
        try std.testing.expectEqual(identifier, (try registry.loadNamed(std.testing.allocator, entry.name)).?);
        for ([_]i64{ std.math.minInt(i64), -1, 0, std.math.maxInt(i64) }) |milliseconds| {
            const offset = registry.offsetSeconds(identifier, milliseconds);
            try std.testing.expect(offset >= -89999 and offset <= 93599);
        }
    }
    try std.testing.expectEqual(@as(usize, 344), manifest.value.unique_rule_count);
    try std.testing.expectEqual(manifest.value.unique_rule_count, registry.rules.count());
    for ([_][]const u8{ "", "../UTC", "/UTC", "Etc//UTC", "Etc/UTC/", "Etc/../UTC", "UTC\x00", "utc", "right/UTC", "posix/UTC", "Not/AZone" }) |name| {
        try std.testing.expectEqual(@as(?i64, null), try registry.loadNamed(std.testing.allocator, name));
    }
}

test "timezone registry cleans up every allocation failure" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, loadTestZones, .{});
}

fn loadTestZones(allocator: std.mem.Allocator) !void {
    var registry: Registry = .{};
    defer registry.deinit(allocator);
    for (bundle.zones[0..32]) |entry| {
        _ = (try registry.loadNamed(allocator, entry.name)).?;
    }
}

test "timezone reader bounds and malformed transitions, types and footers" {
    const entry = bundle.zones[Registry.findZone("America/New_York").?];
    const original = bundle.bytes[entry.offset..][0..entry.length];
    for (0..original.len) |length| {
        try std.testing.expectError(error.InvalidTimeZoneData, Rules.parse(original[0..length]));
    }
    const mutated = try std.testing.allocator.dupe(u8, original);
    defer std.testing.allocator.free(mutated);
    var reader = Reader{ .bytes = original };
    const legacy_header = try Header.read(&reader);
    _ = try reader.take(try legacy_header.blockLength(4));
    const header_start = reader.position;
    const header = try Header.read(&reader);
    const block_start = reader.position;
    const type_indexes = block_start + @as(usize, header.transition_count) * 8;
    const local_types = type_indexes + header.transition_count;
    const footer_start = block_start + try header.blockLength(8);
    for ([_]usize{ 0, 4, 20, 24, 28, 32, 36, 40, header_start + 4, header_start + 20, header_start + 24, header_start + 28, header_start + 32, header_start + 36, header_start + 40, type_indexes, local_types + 4, local_types + 5, footer_start, original.len - 1 }) |position| {
        @memcpy(mutated, original);
        mutated[position] = 255;
        try std.testing.expectError(error.InvalidTimeZoneData, Rules.parse(mutated));
    }
    @memcpy(mutated, original);
    @memcpy(mutated[block_start + 8 ..][0..8], mutated[block_start..][0..8]);
    try std.testing.expectError(error.InvalidTimeZoneData, Rules.parse(mutated));
    @memcpy(mutated, original);
    std.mem.writeInt(i32, mutated[local_types..][0..4], std.math.minInt(i32), .big);
    try std.testing.expectError(error.InvalidTimeZoneData, Rules.parse(mutated));
    for ([_][]const u8{ "\n\n", "\nUTC0\n", "\nUTC0\x00\n", "\nUTC0\nextra" }) |footer| {
        const incomplete = try std.mem.concat(std.testing.allocator, u8, &.{ original[0..footer_start], footer });
        defer std.testing.allocator.free(incomplete);
        try std.testing.expectError(error.InvalidTimeZoneData, Rules.parse(incomplete));
    }
    for (0..original.len) |position| {
        @memcpy(mutated, original);
        mutated[position] ^= 255;
        const rules = Rules.parse(mutated) catch continue;
        _ = rules.offsetSeconds(std.math.minInt(i64));
        _ = rules.offsetSeconds(std.math.maxInt(i64));
    }
}

test "timezone reader permits static legacy files and long designations" {
    const header = "TZif" ++ "\x00" ** 16 ++ "\x00" ** 16 ++ "\x00\x00\x00\x01\x00\x00\x00\x09";
    const payload = header ++ "\x00\x00\x00\x1e\x00\x00LONGNAME\x00";
    const rules = try Rules.parse(payload);
    try std.testing.expectEqual(@as(i32, 30), rules.offsetSeconds(std.math.minInt(i64)));
    try std.testing.expectEqual(@as(i32, 30), rules.offsetSeconds(std.math.maxInt(i64)));
}
