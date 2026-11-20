const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;
const test_paths = @import("test_paths");
const timezone_data = @import("timezone_data");

const is_posix = builtin.os.tag != .windows;

extern "c" fn mkfifo(path: [*:0]const u8, mode: c_uint) c_int;

const Cli = struct {
    arena: std.heap.ArenaAllocator,
    temporary: testing.TmpDir,
    folder: []const u8,
    dusk: []const u8,
    helper: []const u8,

    fn init(self: *Cli) !void {
        self.arena = std.heap.ArenaAllocator.init(testing.allocator);
        errdefer self.arena.deinit();
        const allocator = self.arena.allocator();
        self.temporary = testing.tmpDir(.{});
        const parent = try std.process.currentPathAlloc(testing.io, allocator);
        self.folder = try std.fs.path.resolve(allocator, &.{ parent, ".zig-cache", "tmp", &self.temporary.sub_path });
        self.dusk = try std.fs.path.resolve(allocator, &.{ parent, test_paths.dusk });
        self.helper = try std.fs.path.resolve(allocator, &.{ parent, test_paths.process_fixture });
    }

    fn deinit(self: *Cli) void {
        self.temporary.cleanup();
        self.arena.deinit();
    }

    fn pathOf(self: *Cli, name: []const u8) ![]const u8 {
        return std.fs.path.join(self.arena.allocator(), &.{ self.folder, name });
    }

    fn write(self: *Cli, name: []const u8, data: []const u8) ![]const u8 {
        try self.temporary.dir.writeFile(testing.io, .{ .sub_path = name, .data = data });
        return self.pathOf(name);
    }

    fn environment(self: *Cli, pairs: []const [2][]const u8) !*std.process.Environ.Map {
        const map = try self.arena.allocator().create(std.process.Environ.Map);
        map.* = std.process.Environ.Map.init(self.arena.allocator());
        for (pairs) |pair| {
            try map.put(pair[0], pair[1]);
        }
        return map;
    }

    fn run(self: *Cli, source: []const u8, args: []const []const u8, map: ?*std.process.Environ.Map) !std.process.RunResult {
        const program = try self.write("main.dsk", source);
        var argv: std.ArrayList([]const u8) = .empty;
        try argv.appendSlice(self.arena.allocator(), &.{ self.dusk, program });
        try argv.appendSlice(self.arena.allocator(), args);
        return std.process.run(self.arena.allocator(), testing.io, .{ .argv = argv.items, .environ_map = map });
    }
};

fn expectExit(result: std.process.RunResult, code: u8) !void {
    const has_exited = result.term == .exited and result.term.exited == code;
    if (!has_exited) {
        std.debug.print("expected exit {d}, got {any}\nstdout: {s}\nstderr: {s}\n", .{ code, result.term, result.stdout, result.stderr });
        return error.TestUnexpectedResult;
    }
}

test "os CLI: arguments after the script, environment, directory, streams and exit status" {
    var cli: Cli = undefined;
    try cli.init();
    defer cli.deinit();
    const cwd = try std.process.currentPathAlloc(testing.io, cli.arena.allocator());
    const map = try cli.environment(&.{ .{ "DUSK_OS_VALUE", "héllo" }, .{ "DUSK_OS_EMPTY", "" }, .{ "DUSK_OS_EXPECTED_CWD", cwd } });
    const result = try cli.run(
        \\import std/os
        \\import std/array
        \\import std/io
        \\func main() -> !void
        \\    let args = try os::args()
        \\    assert(array::len(args) == 3)
        \\    assert(args[0] == "one two" and args[1] == "--version" and args[2] == "世界")
        \\    assert(try os::env("DUSK_OS_VALUE") == "héllo")
        \\    assert(try os::env("DUSK_OS_EMPTY") == "")
        \\    assert(try os::env("DUSK_OS_MISSING") == null)
        \\    assert(try os::cwd() == try os::env("DUSK_OS_EXPECTED_CWD"))
        \\    io::println("context ok")
        \\    io::eprintln("stderr ok")
        \\    try os::set_exit_code(42)
        \\end
    , &.{ "one two", "--version", "世界" }, map);
    try expectExit(result, 42);
    try testing.expectEqualStrings("context ok\n", result.stdout);
    try testing.expectEqualStrings("stderr ok\n", result.stderr);

    for ([_]u8{ 0, 1, 255 }) |code| {
        const source = try std.fmt.allocPrint(cli.arena.allocator(), "import std/os func main() -> !void try os::set_exit_code({d}) end", .{code});
        try expectExit(try cli.run(source, &.{}, null), code);
    }
    try expectExit(try cli.run(
        \\import std/os
        \\func main() -> !void
        \\    try os::set_exit_code(42)
        \\    let mut has_invalid_code = false
        \\    os::set_exit_code(256) catch e then
        \\        assert(e == os::OsError.InvalidArgument)
        \\        has_invalid_code = true
        \\    end
        \\    assert(has_invalid_code)
        \\end
    , &.{}, null), 42);
    try expectExit(try cli.run("import std/os func main() -> !void try os::set_exit_code(0) assert(false) end", &.{}, null), 1);
}

test "os CLI: an argument that isn't UTF-8" {
    if (!is_posix) return error.SkipZigTest;
    var cli: Cli = undefined;
    try cli.init();
    defer cli.deinit();
    try expectExit(try cli.run(
        \\import std/os
        \\func main() -> !void
        \\    os::args() catch e then
        \\        assert(e == os::OsError.InvalidUtf8)
        \\        return
        \\    end
        \\    assert(false)
        \\end
    , &.{"\xff"}, null), 0);
}

test "os CLI: signal streams receive signals, and closing them restores the default action" {
    if (!is_posix) return error.SkipZigTest;
    for ([_]struct { []const u8, std.posix.SIG }{ .{ "Interrupt", .INT }, .{ "Terminate", .TERM } }) |case| {
        var cli: Cli = undefined;
        try cli.init();
        defer cli.deinit();
        const source = try std.mem.replaceOwned(u8, cli.arena.allocator(),
            \\import std/os
            \\import std/io
            \\import std/task
            \\import std/time
            \\func main() -> !void
            \\    let first = try os::watch_signal(os::Signal.NAME)
            \\    let second = try os::watch_signal(os::Signal.NAME)
            \\    io::println("READY")
            \\    try io::stdout().flush()
            \\    task::sleep(time::milliseconds(50))
            \\    try first.wait()
            \\    try second.wait()
            \\    io::println("BOTH")
            \\    try first.wait()
            \\    io::println("AGAIN")
            \\    first.close()
            \\    second.close()
            \\    io::println("CLOSED")
            \\    task::sleep(time::seconds(10))
            \\end
        , "NAME", case[0]);
        const program = try cli.write("signals.dsk", source);
        var child = try std.process.spawn(testing.io, .{ .argv = &.{ cli.dusk, program }, .stdout = .pipe, .stderr = .ignore });
        defer child.kill(testing.io);
        const pid = child.id.?;
        try expectLine(child.stdout.?, "READY");
        try std.posix.kill(pid, case[1]);
        try expectLine(child.stdout.?, "BOTH");
        try std.posix.kill(pid, case[1]);
        try expectLine(child.stdout.?, "AGAIN");
        try expectLine(child.stdout.?, "CLOSED");
        try std.posix.kill(pid, case[1]);
        const term = try child.wait(testing.io);
        const is_killed_by_signal = term == .signal and term.signal == case[1];
        try testing.expect(is_killed_by_signal);
    }
}

fn expectLine(file: std.Io.File, expected: []const u8) !void {
    var line: [64]u8 = undefined;
    var length: usize = 0;
    while (true) {
        var byte: [1]u8 = undefined;
        const count = try file.readStreaming(testing.io, &.{&byte});
        if (count == 0) {
            return error.EndOfStream;
        }
        if (byte[0] == '\n') break;
        line[length] = byte[0];
        length += 1;
    }
    try testing.expectEqualStrings(expected, line[0..length]);
}

test "process CLI: the program waits for children, keeps output order, and kills children when it fails" {
    var cli: Cli = undefined;
    try cli.init();
    defer cli.deinit();
    const map = try cli.environment(&.{ .{ "PATH", std.fs.path.dirname(cli.helper).? }, .{ "DUSK_PROCESS_PARENT", "parent" } });
    const marker = try cli.pathOf("finished");
    try expectExit(try cli.run(
        \\import std/process
        \\import std/os
        \\func main() -> !void
        \\    let args = try os::args()
        \\    let child = try process::start([args[0], "finish_file", args[1]], stdout_mode = process::Stdio.Ignore, stderr_mode = process::Stdio.Ignore)
        \\end
    , &.{ cli.helper, marker }, map), 0);
    try testing.expectEqualStrings("finished", try cli.temporary.dir.readFileAlloc(testing.io, "finished", cli.arena.allocator(), .unlimited));

    const ordered = try cli.run(
        \\import std/process
        \\import std/os
        \\import std/io
        \\func main() -> !void
        \\    io::print("before:")
        \\    let child = try process::start([(try os::args())[0], "args", "x"])
        \\    let term = try child.wait()
        \\    try child.close()
        \\    io::println("after")
        \\end
    , &.{cli.helper}, map);
    try expectExit(ordered, 0);
    try testing.expectEqualStrings("before:1:x\nafter\n", ordered.stdout);

    const failed = try cli.run(
        \\import std/process
        \\import std/os
        \\import std/io
        \\func main() -> !void
        \\    let helper = (try os::args())[0]
        \\    let child = try process::start([helper, "pid_ready"], stdout_mode = process::Stdio.Pipe, stderr_mode = process::Stdio.Ignore)
        \\    let output = try child.stdout()
        \\    io::println(try output.read_line() orelse "missing pid")
        \\    assert(false)
        \\end
    , &.{cli.helper}, map);
    try expectExit(failed, 1);
    if (!is_posix) return;
    const pid = try std.fmt.parseInt(std.posix.pid_t, std.mem.trim(u8, failed.stdout, " \r\n"), 10);
    try testing.expectError(error.ProcessNotFound, std.posix.kill(pid, @enumFromInt(0)));
}

const local_zone_source =
    \\import std/time
    \\import std/io
    \\func main() -> !void
    \\    let zone = time::local_timezone() catch e then
    \\        assert(e == time::TimeError.LocalTimeUnavailable)
    \\        io::println("unavailable")
    \\        return
    \\    end
    \\    io::println(zone.name())
    \\    let epoch = time::from_unix_ms(0)
    \\    io::println("${zone.offset_seconds(epoch)}")
    \\    let text = time::to_iso_string(epoch, zone) catch e then
    \\        assert(e == time::TimeError.PrecisionLoss)
    \\        io::println("precision")
    \\        return
    \\    end
    \\    io::println(text)
    \\end
;

fn expectLocalZone(cli: *Cli, source: []const u8, setting: []const u8, expected: []const u8) !void {
    const map = try cli.environment(&.{ .{ "TZ", setting }, .{ "UV_THREADPOOL_SIZE", "1" } });
    const result = try cli.run(source, &.{}, map);
    expectExit(result, 0) catch |failure| {
        std.debug.print("TZ={s}\n", .{setting});
        return failure;
    };
    testing.expectEqualStrings(expected, result.stdout) catch |failure| {
        std.debug.print("TZ={s}\n", .{setting});
        return failure;
    };
}

fn staticZone(allocator: std.mem.Allocator, offset: i32, designation: []const u8) ![]u8 {
    var bytes: std.ArrayList(u8) = .empty;
    try bytes.appendSlice(allocator, "TZif\x00");
    try bytes.appendNTimes(allocator, 0, 15);
    for ([_]u32{ 0, 0, 0, 0, 1, @intCast(designation.len) }) |count| {
        try bytes.appendSlice(allocator, &std.mem.toBytes(std.mem.nativeToBig(u32, count)));
    }
    try bytes.appendSlice(allocator, &std.mem.toBytes(std.mem.nativeToBig(i32, offset)));
    try bytes.appendSlice(allocator, &.{ 0, 0 });
    try bytes.appendSlice(allocator, designation);
    return bytes.items;
}

test "time CLI: TZ names, rules and zone files decide the local zone" {
    var cli: Cli = undefined;
    try cli.init();
    defer cli.deinit();
    const allocator = cli.arena.allocator();
    const named = [_][2][]const u8{
        .{ "", "UTC\n0\n1970-01-01T00:00:00.000Z\n" },
        .{ ":UTC", "UTC\n0\n1970-01-01T00:00:00.000Z\n" },
        .{ "UTC0", "Local\n0\n1970-01-01T00:00:00.000Z\n" },
        .{ "America/New_York", "America/New_York\n-18000\n1969-12-31T19:00:00.000-05:00\n" },
        .{ ":US/Eastern", "US/Eastern\n-18000\n1969-12-31T19:00:00.000-05:00\n" },
        .{ "<+0530>-5:30", "Local\n19800\n1970-01-01T05:30:00.000+05:30\n" },
        .{ "STD-0:00:30", "Local\n30\nprecision\n" },
    };
    for (named) |case| {
        try expectLocalZone(&cli, local_zone_source, case[0], case[1]);
    }
    const too_long = try allocator.alloc(u8, 5000);
    @memset(too_long, 'A');
    for ([_][]const u8{ "NoSuch/Zone", ":", ":UTC0", "UTC24", "STD0DST", too_long, try cli.pathOf("missing") }) |setting| {
        try expectLocalZone(&cli, local_zone_source, setting, "unavailable\n");
    }

    const half_hour = "Local\n19800\n1970-01-01T05:30:00.000+05:30\n";
    const payload = try staticZone(allocator, 19800, "LOCAL\x00");
    const zone = try cli.write("zone-ü.tzif", payload);
    try expectLocalZone(&cli, local_zone_source, zone, half_hour);
    try expectLocalZone(&cli, local_zone_source, try std.mem.concat(allocator, u8, &.{ ":", zone }), half_hour);

    const new_york = for (timezone_data.zones) |entry| {
        if (std.mem.eql(u8, entry.name, "America/New_York")) break entry;
    } else unreachable;
    const new_york_file = try cli.write("new-york.tzif", timezone_data.bytes[new_york.offset..][0..new_york.length]);
    try expectLocalZone(&cli, local_zone_source, new_york_file, "Local\n-18000\n1969-12-31T19:00:00.000-05:00\n");

    const version_four = try std.mem.concat(allocator, u8, &.{ "TZif4", payload[5..] });
    for ([_][]const u8{ "", "TZif2", try staticZone(allocator, 90000, "LOCAL\x00"), try staticZone(allocator, 0, "-00\x00"), version_four }) |content| {
        try expectLocalZone(&cli, local_zone_source, try cli.write("bad.tzif", content), "unavailable\n");
    }
    var oversized = try cli.temporary.dir.createFile(testing.io, "oversized.tzif", .{});
    try oversized.setLength(testing.io, 4 * 1024 * 1024 + 1);
    oversized.close(testing.io);
    try expectLocalZone(&cli, local_zone_source, try cli.pathOf("oversized.tzif"), "unavailable\n");
    try expectLocalZone(&cli, local_zone_source, cli.folder, "unavailable\n");

    if (!is_posix) return;
    const fifo = try allocator.dupeZ(u8, try cli.pathOf("fifo"));
    try testing.expectEqual(@as(c_int, 0), mkfifo(fifo.ptr, 0o600));
    try expectLocalZone(&cli, local_zone_source, fifo, "unavailable\n");
    try cli.temporary.dir.symLink(testing.io, zone, "link", .{});
    try expectLocalZone(&cli, local_zone_source, try cli.pathOf("link"), half_hour);
    try cli.temporary.dir.symLink(testing.io, try cli.pathOf("absent"), "broken", .{});
    try expectLocalZone(&cli, local_zone_source, try cli.pathOf("broken"), "unavailable\n");
}

test "time CLI: the local zone is cached after it loads, retried after it fails, and pending lookups end with the program" {
    var cli: Cli = undefined;
    try cli.init();
    defer cli.deinit();
    const allocator = cli.arena.allocator();
    const payload = try staticZone(allocator, 19800, "LOCAL\x00");

    const snapshot = try cli.write("snapshot.tzif", payload);
    const cached = try std.mem.replaceOwned(u8, allocator,
        \\import std/time
        \\import std/fs
        \\func main() -> !void
        \\    let first = try time::local_timezone()
        \\    try fs::delete("PATH")
        \\    let second = try time::local_timezone()
        \\    let epoch = time::from_unix_ms(0)
        \\    assert(first.name() == "Local" and second.name() == "Local")
        \\    assert(first.offset_seconds(epoch) == 19800)
        \\    assert(second.offset_seconds(epoch) == 19800)
        \\end
    , "PATH", try duskStringLiteral(allocator, snapshot));
    try expectLocalZone(&cli, cached, snapshot, "");

    const retry = try cli.write("retry.tzif", "bad");
    const retried = try std.mem.replaceOwned(u8, allocator, try std.mem.replaceOwned(u8, allocator,
        \\import std/time
        \\import std/fs
        \\import std/buffer
        \\func main() -> !void
        \\    let mut has_failed = false
        \\    time::local_timezone() catch e then
        \\        assert(e == time::TimeError.LocalTimeUnavailable)
        \\        has_failed = true
        \\    end
        \\    assert(has_failed)
        \\    try fs::write_bytes("PATH", buffer::from_string("PAYLOAD"))
        \\    let zone = try time::local_timezone()
        \\    assert(zone.offset_seconds(time::from_unix_ms(0)) == 19800)
        \\end
    , "PATH", try duskStringLiteral(allocator, retry)), "PAYLOAD", try duskStringLiteral(allocator, payload));
    try expectLocalZone(&cli, retried, retry, "");

    const map = try cli.environment(&.{ .{ "TZ", snapshot }, .{ "UV_THREADPOOL_SIZE", "1" } });
    _ = try cli.write("snapshot.tzif", payload);
    const pending = try cli.run(
        \\import std/time
        \\import std/task
        \\func main() -> void
        \\    for 0..15: i do
        \\        let request = spawn(func() -> time::TimeError!time::TimeZone
        \\            return try time::local_timezone()
        \\        end)
        \\    end
        \\    task::sleep(time::milliseconds(0))
        \\    assert(false)
        \\end
    , &.{}, map);
    try testing.expect(pending.term != .exited or pending.term.exited != 0);
    try testing.expect(std.mem.indexOf(u8, pending.stderr, "assertion failed") != null);
}

fn duskStringLiteral(allocator: std.mem.Allocator, text: []const u8) ![]const u8 {
    var escaped: std.ArrayList(u8) = .empty;
    for (text) |byte| {
        const needs_backslash = byte == '\\' or byte == '"' or byte == '$';
        if (needs_backslash) {
            try escaped.append(allocator, '\\');
            try escaped.append(allocator, byte);
            continue;
        }
        if (byte < 32) {
            try escaped.print(allocator, "\\u{{{x}}}", .{byte});
            continue;
        }
        try escaped.append(allocator, byte);
    }
    return escaped.items;
}
