import os
from pathlib import Path
import selectors
import signal
import subprocess
import sys
import tempfile
import time

binary = str(Path(sys.argv[1] if len(sys.argv) > 1 else "zig-out/bin/dusk").resolve())


def execute(folder, name, source, args=(), environment=None):
    path = folder / (name + ".dsk")
    path.write_text(source)
    return subprocess.run([binary, str(path), *args], env=environment, capture_output=True, timeout=15)


def expect_line(child, expected):
    selector = selectors.DefaultSelector()
    selector.register(child.stdout, selectors.EVENT_READ)
    received = bytearray()
    deadline = time.monotonic() + 10
    try:
        while not received.endswith(b"\n"):
            remaining = deadline - time.monotonic()
            assert remaining > 0 and selector.select(remaining), (expected, bytes(received))
            chunk = os.read(child.stdout.fileno(), 1)
            assert chunk, (expected, bytes(received), child.poll())
            received.extend(chunk)
    finally:
        selector.close()
    assert bytes(received) == expected + b"\n", bytes(received)


with tempfile.TemporaryDirectory(prefix="dusk-os-") as temporary:
    folder = Path(temporary)
    environment = dict(os.environ, DUSK_OS_VALUE="héllo", DUSK_OS_EMPTY="", DUSK_OS_EXPECTED_CWD=os.getcwd())
    environment.pop("DUSK_OS_MISSING", None)
    result = execute(folder, "context", """import std/os
import std/array
import std/io
import std/string
func main() -> !void
    let args = try os::args()
    assert(array::len(args) == 3)
    assert(args[0] == "one two" and args[1] == "--version" and args[2] == "世界")
    assert(try os::env("DUSK_OS_VALUE") == "héllo")
    assert(try os::env("DUSK_OS_EMPTY") == "")
    assert(try os::env("DUSK_OS_MISSING") == null)
    assert(try os::cwd() == try os::env("DUSK_OS_EXPECTED_CWD"))
    io::println("context ok")
    io::eprintln("stderr ok")
    try os::set_exit_code(42)
end
""", ["one two", "--version", "世界"], environment)
    assert result.returncode == 42, result
    assert result.stdout == b"context ok\n", result
    assert result.stderr == b"stderr ok\n", result
    for code in (0, 1, 255):
        result = execute(folder, "status", f"import std/os func main() -> !void try os::set_exit_code({code}) end")
        assert result.returncode == code, result
    result = execute(folder, "invalid_status", """import std/os
func main() -> !void
    try os::set_exit_code(42)
    let mut has_invalid_code = false
    os::set_exit_code(256) catch e then
        assert(e == os::OsError.InvalidArgument)
        has_invalid_code = true
    end
    assert(has_invalid_code)
end
""")
    assert result.returncode == 42, result
    result = execute(folder, "failure", "import std/os func main() -> !void try os::set_exit_code(0) assert(false) end")
    assert result.returncode == 1, result
    if os.name == "posix":
        result = execute(folder, "invalid_arg", """import std/os
func main() -> !void
    os::args() catch e then
        assert(e == os::OsError.InvalidUtf8)
        return
    end
    assert(false)
end
""", [b"\xff"])
        assert result.returncode == 0, result
        for signal_name, signal_value in (("Interrupt", signal.SIGINT), ("Terminate", signal.SIGTERM)):
            path = folder / "signals.dsk"
            path.write_text("""import std/os
import std/io
import std/task
func main() -> !void
    let first = try os::watch_signal(os::Signal.SIGNAL_NAME)
    let second = try os::watch_signal(os::Signal.SIGNAL_NAME)
    io::println("READY")
    try io::stdout().flush()
    task::sleep(50)
    try first.wait()
    try second.wait()
    io::println("BOTH")
    try first.wait()
    io::println("AGAIN")
    first.close()
    second.close()
    io::println("CLOSED")
    task::sleep(10000)
end
""".replace("SIGNAL_NAME", signal_name))
            child = subprocess.Popen([binary, str(path)], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            try:
                expect_line(child, b"READY")
                child.send_signal(signal_value)
                expect_line(child, b"BOTH")
                child.send_signal(signal_value)
                expect_line(child, b"AGAIN")
                expect_line(child, b"CLOSED")
                child.send_signal(signal_value)
                child.communicate(timeout=10)
                assert child.returncode == -signal_value, child.returncode
            finally:
                if child.poll() is None:
                    child.kill()
                    child.communicate()
print("std/os CLI checks passed")
