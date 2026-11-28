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
        return std.process.run(self.arena.allocator(), testing.io, .{ .argv = argv.items, .environ_map = map, .timeout = .{ .duration = .{ .raw = .fromSeconds(30), .clock = .awake } } });
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
        \\import "std/os"
        \\import "std/array"
        \\import "std/io"
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
        const source = try std.fmt.allocPrint(cli.arena.allocator(), "import \"std/os\" func main() -> !void try os::set_exit_code({d}) end", .{code});
        try expectExit(try cli.run(source, &.{}, null), code);
    }
    try expectExit(try cli.run(
        \\import "std/os"
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
    try expectExit(try cli.run("import \"std/os\" func main() -> !void try os::set_exit_code(0) assert(false) end", &.{}, null), 1);
}

test "os CLI: an argument that isn't UTF-8" {
    if (!is_posix) return error.SkipZigTest;
    var cli: Cli = undefined;
    try cli.init();
    defer cli.deinit();
    try expectExit(try cli.run(
        \\import "std/os"
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
            \\import "std/os"
            \\import "std/io"
            \\import "std/task"
            \\import "std/time"
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

fn readToEnd(allocator: std.mem.Allocator, file: std.Io.File) ![]const u8 {
    var output: std.ArrayList(u8) = .empty;
    var chunk: [4096]u8 = undefined;
    while (true) {
        const count = file.readStreaming(testing.io, &.{&chunk}) catch |failure| {
            if (failure == error.EndOfStream) break;
            return failure;
        };
        if (count == 0) break;
        try output.appendSlice(allocator, chunk[0..count]);
    }
    return output.items;
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
        \\import "std/process"
        \\import "std/os"
        \\func main() -> !void
        \\    let args = try os::args()
        \\    let child = try process::start([args[0], "finish_file", args[1]], stdout_mode = process::Stdio.Ignore, stderr_mode = process::Stdio.Ignore)
        \\end
    , &.{ cli.helper, marker }, map), 0);
    try testing.expectEqualStrings("finished", try cli.temporary.dir.readFileAlloc(testing.io, "finished", cli.arena.allocator(), .unlimited));

    try expectExit(try cli.run(
        \\import "std/process"
        \\import "std/os"
        \\func main() -> !void
        \\    let helper = (try os::args())[0]
        \\    let reader = try process::start([helper, "copy"], stdin_mode = process::Stdio.Pipe, stdout_mode = process::Stdio.Ignore, stderr_mode = process::Stdio.Ignore)
        \\    let writer = try process::start([helper, "dual", "1000000"], stdout_mode = process::Stdio.Pipe, stderr_mode = process::Stdio.Pipe)
        \\end
    , &.{cli.helper}, map), 0);

    const ordered = try cli.run(
        \\import "std/process"
        \\import "std/os"
        \\import "std/io"
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
        \\import "std/process"
        \\import "std/os"
        \\import "std/io"
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

test "process CLI: a child that inherits stdin gets the input that arrives after it starts" {
    if (!is_posix) return error.SkipZigTest;
    var cli: Cli = undefined;
    try cli.init();
    defer cli.deinit();
    const program = try cli.write("inherit.dsk",
        \\import "std/io"
        \\import "std/process"
        \\func main() -> !void
        \\    let first = try io::stdin().read_line()
        \\    let child = try process::start(["sh", "-c", "sleep 0.3; exec cat"], stdin_mode = process::Stdio.Inherit)
        \\    io::println("ready ${first orelse "none"}")
        \\    try io::stdout().flush()
        \\    let term = try child.wait()
        \\end
    );
    var child = try std.process.spawn(testing.io, .{ .argv = &.{ cli.dusk, program }, .stdin = .pipe, .stdout = .pipe, .stderr = .ignore });
    defer child.kill(testing.io);
    try child.stdin.?.writeStreamingAll(testing.io, "first\n");
    try expectLine(child.stdout.?, "ready first");
    try child.stdin.?.writeStreamingAll(testing.io, "second\n");
    child.stdin.?.close(testing.io);
    child.stdin = null;
    try testing.expectEqualStrings("second\n", try readToEnd(cli.arena.allocator(), child.stdout.?));
    const term = try child.wait(testing.io);
    const has_succeeded = term == .exited and term.exited == 0;
    try testing.expect(has_succeeded);
}

const local_zone_source =
    \\import "std/time"
    \\import "std/io"
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
        \\import "std/time"
        \\import "std/fs"
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
        \\import "std/time"
        \\import "std/fs"
        \\import "std/buffer"
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
        \\import "std/time"
        \\import "std/task"
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

extern "c" fn posix_openpt(flags: c_int) c_int;
extern "c" fn grantpt(fd: c_int) c_int;
extern "c" fn unlockpt(fd: c_int) c_int;
extern "c" fn ptsname(fd: c_int) ?[*:0]const u8;

const Terminal = struct {
    master: c_int,
    replica: c_int,
    output: std.ArrayList(u8) = .empty,
    matched_until: usize = 0,

    fn open(columns: u16, rows: u16) !Terminal {
        const master = posix_openpt(@bitCast(std.posix.O{ .ACCMODE = .RDWR, .NOCTTY = true }));
        if (master < 0) {
            return error.SkipZigTest;
        }
        errdefer _ = std.c.close(master);
        const is_unlocked = grantpt(master) == 0 and unlockpt(master) == 0;
        if (!is_unlocked) {
            return error.SkipZigTest;
        }
        const name = ptsname(master) orelse return error.SkipZigTest;
        const replica = std.c.open(name, .{ .ACCMODE = .RDWR, .NOCTTY = true });
        if (replica < 0) {
            return error.SkipZigTest;
        }
        const terminal: Terminal = .{ .master = master, .replica = replica };
        try terminal.resize(columns, rows);
        return terminal;
    }

    fn close(self: *Terminal) void {
        self.output.deinit(testing.allocator);
        _ = std.c.close(self.replica);
        _ = std.c.close(self.master);
    }

    fn file(self: Terminal) std.Io.File {
        return .{ .handle = self.replica, .flags = .{ .nonblocking = false } };
    }

    fn resize(self: Terminal, columns: u16, rows: u16) !void {
        const size: std.posix.winsize = .{ .row = rows, .col = columns, .xpixel = 0, .ypixel = 0 };
        if (std.c.ioctl(self.master, std.posix.T.IOCSWINSZ, &size) != 0) {
            return error.ResizeFailed;
        }
    }

    fn isRaw(self: Terminal) !bool {
        const attributes = try std.posix.tcgetattr(self.replica);
        return !attributes.lflag.ECHO and !attributes.lflag.ICANON;
    }

    fn send(self: Terminal, bytes: []const u8) !void {
        if (std.c.write(self.master, bytes.ptr, bytes.len) != bytes.len) {
            return error.WriteFailed;
        }
    }

    fn expectOutput(self: *Terminal, expected: []const u8) !void {
        var chunk: [256]u8 = undefined;
        while (true) {
            const unmatched = self.output.items[self.matched_until..];
            if (std.mem.indexOf(u8, unmatched, expected)) |position| {
                self.matched_until += position + expected.len;
                return;
            }
            var fds = [_]std.posix.pollfd{.{ .fd = self.master, .events = std.posix.POLL.IN, .revents = 0 }};
            if (try std.posix.poll(&fds, 10_000) == 0) {
                std.debug.print("expected {s}, got {s}\n", .{ expected, unmatched });
                return error.TestUnexpectedResult;
            }
            const count = try std.posix.read(self.master, &chunk);
            try self.output.appendSlice(testing.allocator, chunk[0..count]);
        }
    }
};

const terminal_source =
    \\import "std/io"
    \\import "std/os"
    \\import "std/buffer"
    \\func main() -> !void
    \\    let input = io::stdin()
    \\    let size = io::stdout().size() orelse io::TerminalSize(columns = 0, rows = 0)
    \\    io::println("size ${size.columns}x${size.rows}")
    \\    let resize = try os::watch_signal(os::Signal.WindowChange)
    \\    try input.set_raw(true)
    \\    io::println("raw")
    \\    let first = try input.read_bytes(16) orelse try buffer::new(1)
    \\    io::println("got ${buffer::len(first)} ${first[0]}")
    \\    try resize.wait()
    \\    let after = io::stdout().size() orelse size
    \\    io::println("resized ${after.columns}x${after.rows}")
    \\    if (try os::args())[0] == "crash" then
    \\        let empty: []int = []
    \\        io::println("${empty[1]}")
    \\    end
    \\end
;

fn expectTerminalRun(cli: *Cli, program: []const u8, ending: []const u8, code: u8) !void {
    var terminal = try Terminal.open(100, 30);
    defer terminal.close();
    var child = try std.process.spawn(testing.io, .{ .argv = &.{ cli.dusk, program, ending }, .stdin = .{ .file = terminal.file() }, .stdout = .{ .file = terminal.file() }, .stderr = .{ .file = terminal.file() } });
    defer child.kill(testing.io);
    try terminal.expectOutput("size 100x30");
    try terminal.expectOutput("raw");
    try testing.expect(try terminal.isRaw());
    try terminal.send("\x03");
    try terminal.expectOutput("got 1 3");
    try terminal.resize(120, 40);
    try std.posix.kill(child.id.?, .WINCH);
    try terminal.expectOutput("resized 120x40");
    const term = try child.wait(testing.io);
    const has_exited = term == .exited and term.exited == code;
    try testing.expect(has_exited);
    try testing.expect(!try terminal.isRaw());
}

test "terminal CLI: raw input, size, resize events and the terminal restored at the end" {
    if (builtin.os.tag != .linux) return error.SkipZigTest;
    var cli: Cli = undefined;
    try cli.init();
    defer cli.deinit();
    const program = try cli.write("terminal.dsk", terminal_source);
    try expectTerminalRun(&cli, program, "stay", 0);
    try expectTerminalRun(&cli, program, "crash", 1);
}
