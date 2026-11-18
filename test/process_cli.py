import os
from pathlib import Path
import subprocess
import sys
import tempfile

binary = str(Path(sys.argv[1] if len(sys.argv) > 1 else "zig-out/bin/dusk").resolve())
helper = Path(sys.argv[2] if len(sys.argv) > 2 else "zig-out/bin/dusk-process-fixture").resolve()
environment = dict(os.environ, PATH=str(helper.parent), DUSK_PROCESS_PARENT="parent")
root = Path(__file__).resolve().parent
with tempfile.TemporaryDirectory(prefix="dusk-process-") as temporary:
    folder = Path(temporary)
    for name in ("process-run.dsk", "process-child.dsk", "process-errors.dsk", "process-concurrent.dsk", "process-gc.dsk"):
        result = subprocess.run([binary, str(root / "success" / name), str(helper), helper.name, str(helper.parent), str(folder / "gate")], env=environment, capture_output=True, timeout=15)
        assert result.returncode == 0, (name, result.stdout, result.stderr)
    source = folder / "lifetime.dsk"
    source.write_text("""import std/process
import std/os
func main() -> !void
    let args = try os::args()
    let child = try process::start([args[0], "finish_file", args[1]], stdout_mode = process::Stdio.Ignore, stderr_mode = process::Stdio.Ignore)
end
""")
    marker = folder / "finished"
    result = subprocess.run([binary, str(source), str(helper), str(marker)], env=environment, capture_output=True, timeout=10)
    assert result.returncode == 0 and marker.read_text() == "finished", result
    source.write_text("""import std/process
import std/os
import std/io
func main() -> !void
    io::print("before:")
    let child = try process::start([(try os::args())[0], "args", "x"])
    let term = try child.wait()
    try child.close()
    io::println("after")
end
""")
    result = subprocess.run([binary, str(source), str(helper)], env=environment, capture_output=True, timeout=10)
    assert result.returncode == 0 and result.stdout == b"before:1:x\nafter\n", result
    source.write_text("""import std/process
import std/os
import std/io
func main() -> !void
    let helper = (try os::args())[0]
    let child = try process::start([helper, "pid_ready"], stdout_mode = process::Stdio.Pipe, stderr_mode = process::Stdio.Ignore)
    let output = try child.stdout()
    io::println(try output.read_line() orelse "missing pid")
    assert(false)
end
""")
    result = subprocess.run([binary, str(source), str(helper)], env=environment, capture_output=True, timeout=10)
    assert result.returncode == 1, result
    pid = int(result.stdout.strip())
    if os.name == "posix":
        try:
            os.kill(pid, 0)
        except ProcessLookupError:
            pass
        else:
            raise AssertionError(("owned child still exists after VM failure", pid))
print("std/process CLI checks passed")
