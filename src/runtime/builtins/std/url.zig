const std = @import("std");
const v = @import("../../value.zig");
const VM = @import("../../vm.zig").VM;
const native = @import("../native.zig");
const Binding = native.Binding;
const NativeResult = native.NativeResult;
const ok = native.ok;
const fail = native.fail;
const stringResult = native.stringResult;
const bytesArg = native.bytesArg;

pub const UrlError = enum { Invalid, InvalidPort, InvalidEncoding };
pub const error_sets = .{UrlError};

const max_port_digits = 5;
const max_port = 65535;
const hex_digits = "0123456789ABCDEF";

pub const bindings = native.BindingMap.initComptime(.{
    .{ "parse_offsets", Binding{ .native = &parseOffsets } },
    .{ "resolve_text", Binding{ .native = &resolveText } },
    .{ "encode", Binding{ .native = &encodeComponent } },
    .{ "decode", Binding{ .native = &decodeComponent } },
});

fn parseOffsets(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const parts = splitUrl(bytesArg(args, 0)) catch |failure| return fail(urlErrorOf(failure));
    return ok(try vm.allocIntArray(&parts.offsets()));
}

const SplitError = error{ Invalid, InvalidPort };

fn urlErrorOf(failure: SplitError) UrlError {
    return switch (failure) {
        error.Invalid => .Invalid,
        error.InvalidPort => .InvalidPort,
    };
}

const Range = struct {
    start: usize,
    end: usize,

    fn of(self: Range, text: []const u8) []const u8 {
        return text[self.start..self.end];
    }
};

const UrlParts = struct {
    scheme_end: usize,
    host: Range,
    port: ?u16,
    path: Range,
    query: ?Range,
    fragment: ?Range,

    fn offsets(self: UrlParts) [10]i64 {
        return .{
            @intCast(self.scheme_end),
            @intCast(self.host.start),
            @intCast(self.host.end),
            if (self.port) |port| port else -1,
            @intCast(self.path.start),
            @intCast(self.path.end),
            if (self.query) |query| @intCast(query.start) else -1,
            if (self.query) |query| @intCast(query.end) else -1,
            if (self.fragment) |fragment| @intCast(fragment.start) else -1,
            if (self.fragment) |fragment| @intCast(fragment.end) else -1,
        };
    }
};

fn splitUrl(text: []const u8) SplitError!UrlParts {
    const scheme_end = schemeEnd(text) orelse return error.Invalid;
    if (!std.mem.startsWith(u8, text[scheme_end..], "://")) {
        return error.Invalid;
    }
    const authority_start = scheme_end + 3;
    const authority_end = std.mem.indexOfAnyPos(u8, text, authority_start, "/?#") orelse text.len;
    const authority = try splitAuthority(text, .{ .start = authority_start, .end = authority_end });
    const path_end = std.mem.indexOfAnyPos(u8, text, authority_end, "?#") orelse text.len;
    const path: Range = .{ .start = authority_end, .end = path_end };
    try checkPathCharacters(path.of(text));
    var query: ?Range = null;
    var position = path_end;
    if (position < text.len and text[position] == '?') {
        const query_end = std.mem.indexOfScalarPos(u8, text, position + 1, '#') orelse text.len;
        query = .{ .start = position + 1, .end = query_end };
        try checkPathCharacters(query.?.of(text));
        position = query_end;
    }
    var fragment: ?Range = null;
    if (position < text.len) {
        fragment = .{ .start = position + 1, .end = text.len };
        try checkPathCharacters(fragment.?.of(text));
    }
    return .{ .scheme_end = scheme_end, .host = authority.host, .port = authority.port, .path = path, .query = query, .fragment = fragment };
}

fn schemeEnd(text: []const u8) ?usize {
    if (text.len == 0 or !std.ascii.isAlphabetic(text[0])) {
        return null;
    }
    for (text[1..], 1..) |char, index| {
        if (char == ':') {
            return index;
        }
        const is_scheme_char = std.ascii.isAlphanumeric(char) or char == '+' or char == '-' or char == '.';
        if (!is_scheme_char) {
            return null;
        }
    }
    return null;
}

const Authority = struct { host: Range, port: ?u16 };

fn splitAuthority(text: []const u8, authority: Range) SplitError!Authority {
    const raw = authority.of(text);
    if (std.mem.indexOfScalar(u8, raw, '@') != null) {
        return error.Invalid;
    }
    if (raw.len > 0 and raw[0] == '[') {
        return splitBracketedAuthority(text, authority);
    }
    const colon = std.mem.lastIndexOfScalar(u8, raw, ':');
    const host_end = if (colon) |index| authority.start + index else authority.end;
    const host: Range = .{ .start = authority.start, .end = host_end };
    try checkHostCharacters(host.of(text));
    const port_text = if (colon) |index| raw[index + 1 ..] else "";
    return .{ .host = host, .port = try parsePort(port_text) };
}

fn splitBracketedAuthority(text: []const u8, authority: Range) SplitError!Authority {
    const raw = authority.of(text);
    const close = std.mem.indexOfScalar(u8, raw, ']') orelse return error.Invalid;
    const host: Range = .{ .start = authority.start + 1, .end = authority.start + close };
    const address = host.of(text);
    if (std.mem.indexOfScalar(u8, address, ':') == null) {
        return error.Invalid;
    }
    for (address) |char| {
        const is_address_char = std.ascii.isHex(char) or char == ':' or char == '.';
        if (!is_address_char) {
            return error.Invalid;
        }
    }
    const rest = raw[close + 1 ..];
    if (rest.len == 0) {
        return .{ .host = host, .port = null };
    }
    if (rest[0] != ':') {
        return error.Invalid;
    }
    return .{ .host = host, .port = try parsePort(rest[1..]) };
}

fn checkHostCharacters(host: []const u8) SplitError!void {
    if (host.len == 0) {
        return error.Invalid;
    }
    for (host) |char| {
        const is_host_char = std.ascii.isAlphanumeric(char) or char == '-' or char == '.' or char == '_' or char == '~';
        if (!is_host_char) {
            return error.Invalid;
        }
    }
}

fn parsePort(digits: []const u8) SplitError!?u16 {
    if (digits.len == 0) {
        return null;
    }
    if (digits.len > max_port_digits) {
        return error.InvalidPort;
    }
    var port: u32 = 0;
    for (digits) |char| {
        if (!std.ascii.isDigit(char)) {
            return error.InvalidPort;
        }
        port = port * 10 + (char - '0');
    }
    if (port == 0 or port > max_port) {
        return error.InvalidPort;
    }
    return @intCast(port);
}

fn checkPathCharacters(part: []const u8) SplitError!void {
    var index: usize = 0;
    while (index < part.len) : (index += 1) {
        const char = part[index];
        if (char == '%') {
            const has_two_hex_digits = index + 2 < part.len and std.ascii.isHex(part[index + 1]) and std.ascii.isHex(part[index + 2]);
            if (!has_two_hex_digits) {
                return error.Invalid;
            }
            index += 2;
            continue;
        }
        if (!isPathCharacter(char)) {
            return error.Invalid;
        }
    }
}

fn isPathCharacter(char: u8) bool {
    if (char <= ' ' or char >= 0x7f) {
        return false;
    }
    return switch (char) {
        '"', '<', '>', '\\', '^', '`', '{', '|', '}', '#' => false,
        else => true,
    };
}

fn resolveText(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const allocator = std.heap.smp_allocator;
    var resolved: std.ArrayList(u8) = .empty;
    defer resolved.deinit(allocator);
    appendResolved(allocator, &resolved, bytesArg(args, 0), bytesArg(args, 1)) catch |failure| switch (failure) {
        error.OutOfMemory => return error.OutOfMemory,
        error.Invalid, error.InvalidPort => |split_failure| return fail(urlErrorOf(split_failure)),
    };
    const result = try vm.allocString(resolved.items.len);
    @memcpy(result.mutableSlice(), resolved.items);
    return stringResult(result);
}

const Reference = struct {
    path: []const u8,
    query: ?[]const u8,
    fragment: ?[]const u8,

    fn split(text: []const u8) Reference {
        const fragment_start = std.mem.indexOfScalar(u8, text, '#');
        const before_fragment = text[0 .. fragment_start orelse text.len];
        const query_start = std.mem.indexOfScalar(u8, before_fragment, '?');
        return .{
            .path = before_fragment[0 .. query_start orelse before_fragment.len],
            .query = if (query_start) |index| before_fragment[index + 1 ..] else null,
            .fragment = if (fragment_start) |index| text[index + 1 ..] else null,
        };
    }
};

fn appendResolved(allocator: std.mem.Allocator, out: *std.ArrayList(u8), base_text: []const u8, reference_text: []const u8) (SplitError || error{OutOfMemory})!void {
    if (hasScheme(reference_text)) {
        const absolute = try splitUrl(reference_text);
        try out.appendSlice(allocator, reference_text[0..absolute.path.start]);
        try appendWithoutDotSegments(allocator, out, absolute.path.of(reference_text));
        try out.appendSlice(allocator, reference_text[absolute.path.end..]);
        return;
    }
    const base = try splitUrl(base_text);
    if (std.mem.startsWith(u8, reference_text, "//")) {
        const combined = try std.mem.concat(allocator, u8, &.{ base_text[0 .. base.scheme_end + 1], reference_text });
        defer allocator.free(combined);
        const resolved = try splitUrl(combined);
        try out.appendSlice(allocator, combined[0..resolved.path.start]);
        try appendWithoutDotSegments(allocator, out, resolved.path.of(combined));
        try out.appendSlice(allocator, combined[resolved.path.end..]);
        return;
    }
    const reference = Reference.split(reference_text);
    try out.appendSlice(allocator, base_text[0..base.path.start]);
    const base_path = base.path.of(base_text);
    var query = reference.query;
    if (reference.path.len == 0) {
        try out.appendSlice(allocator, if (base_path.len == 0) "/" else base_path);
        if (query == null) {
            query = if (base.query) |range| range.of(base_text) else null;
        }
    } else if (reference.path[0] == '/') {
        try appendWithoutDotSegments(allocator, out, reference.path);
    } else {
        const directory = if (std.mem.lastIndexOfScalar(u8, base_path, '/')) |index| base_path[0 .. index + 1] else "/";
        const merged = try std.mem.concat(allocator, u8, &.{ directory, reference.path });
        defer allocator.free(merged);
        try appendWithoutDotSegments(allocator, out, merged);
    }
    if (query) |text| {
        try out.append(allocator, '?');
        try out.appendSlice(allocator, text);
    }
    if (reference.fragment) |text| {
        try out.append(allocator, '#');
        try out.appendSlice(allocator, text);
    }
}

fn hasScheme(text: []const u8) bool {
    return schemeEnd(text) != null;
}

fn appendWithoutDotSegments(allocator: std.mem.Allocator, out: *std.ArrayList(u8), path: []const u8) error{OutOfMemory}!void {
    const path_start = out.items.len;
    var segments = std.mem.splitScalar(u8, path, '/');
    _ = segments.next();
    while (segments.next()) |segment| {
        const is_last = segments.peek() == null;
        if (std.mem.eql(u8, segment, ".") or std.mem.eql(u8, segment, "..")) {
            if (std.mem.eql(u8, segment, "..")) {
                const last_slash = std.mem.lastIndexOfScalar(u8, out.items[path_start..], '/') orelse 0;
                out.shrinkRetainingCapacity(path_start + last_slash);
            }
            if (is_last) {
                try out.append(allocator, '/');
            }
            continue;
        }
        try out.append(allocator, '/');
        try out.appendSlice(allocator, segment);
    }
    if (out.items.len == path_start) {
        try out.append(allocator, '/');
    }
}

fn encodeComponent(vm: *VM, args: []v.Value) anyerror!NativeResult {
    var length: usize = 0;
    for (bytesArg(args, 0)) |char| {
        length += if (isUnreserved(char)) 1 else 3;
    }
    const result = try vm.allocString(length);
    const out = result.mutableSlice();
    var index: usize = 0;
    for (bytesArg(args, 0)) |char| {
        if (isUnreserved(char)) {
            out[index] = char;
            index += 1;
            continue;
        }
        out[index] = '%';
        out[index + 1] = hex_digits[char >> 4];
        out[index + 2] = hex_digits[char & 0x0f];
        index += 3;
    }
    return stringResult(result);
}

fn isUnreserved(char: u8) bool {
    return std.ascii.isAlphanumeric(char) or char == '-' or char == '.' or char == '_' or char == '~';
}

fn decodeComponent(vm: *VM, args: []v.Value) anyerror!NativeResult {
    const text = bytesArg(args, 0);
    var length: usize = 0;
    var index: usize = 0;
    while (index < text.len) : (length += 1) {
        if (text[index] != '%') {
            index += 1;
            continue;
        }
        const has_two_hex_digits = index + 2 < text.len and std.ascii.isHex(text[index + 1]) and std.ascii.isHex(text[index + 2]);
        if (!has_two_hex_digits) {
            return fail(UrlError.InvalidEncoding);
        }
        index += 3;
    }
    const result = try vm.allocString(length);
    const out = result.mutableSlice();
    const source = bytesArg(args, 0);
    var read: usize = 0;
    for (out) |*byte| {
        if (source[read] != '%') {
            byte.* = source[read];
            read += 1;
            continue;
        }
        byte.* = (hexValue(source[read + 1]) << 4) | hexValue(source[read + 2]);
        read += 3;
    }
    if (!std.unicode.utf8ValidateSlice(out)) {
        return fail(UrlError.InvalidEncoding);
    }
    return stringResult(result);
}

fn hexValue(char: u8) u8 {
    return std.fmt.charToDigit(char, 16) catch unreachable;
}
