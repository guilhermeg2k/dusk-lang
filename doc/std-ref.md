# Standard Library Reference

This is the reference for the standard library of **v0.17 - Fujiwara** (in development). The language itself, including the prelude (the builtins every program has without an import), is in [language-ref.md](language-ref.md).

Each module is imported with `import std/<name>` and used through its name (`io::println`, `fs::read`), or with the names it exports listed in the import (`import std/array: append, len`). See [Modules](language-ref.md#14-modules).

## Contents

- [std/array](#stdarray)
- [std/map](#stdmap)
- [std/string](#stdstring)
- [std/buffer](#stdbuffer)
- [std/compression](#stdcompression)
- [std/process](#stdprocess)
- [std/random](#stdrandom)
- [std/path](#stdpath)
- [std/os](#stdos)
- [std/time](#stdtime)
- [std/math](#stdmath)
- [std/task](#stdtask)
- [std/io](#stdio)
- [std/fs](#stdfs)
- [std/net](#stdnet)
- [std/url](#stdurl)
- [std/http](#stdhttp)
- [std/serialization](#stdserialization)
- [std/json](#stdjson)
- [Shared error sets](#shared-error-sets)
- [Known limitations](#known-limitations)

## std/array

Functions that change an array take it as `mut`, and the ones that can fail return an error union, handled with `try` or `catch`. The module declares the error set `ArrayError` (`Empty`, `OutOfRange`). `pop`, `insert` and `remove` are native builtins: to pass one as a function value, wrap its call in a lambda or a Dusk function.

| Function | Returns | Description |
| :------- | :------ | :---------- |
| `len(list)` | `int` | The number of items. |
| `append(mut list, mut item)` | `[]T` | Adds an item to the end. |
| `extend(mut list, items)` | `[]T` | Appends all items, retaining references to nested values. Extending an array with itself appends its original contents once. |
| `sort(mut list)` | `[]T` | Sorts numbers or strings in ascending order and returns the same array reference. |
| `sort_by(mut list, is_before)` | `[]T` | Sorts in place using `func(T, T) -> bool`, and returns the same array reference. The callback returns whether its first item comes before its second. |
| `map(list, transform)` | `E![]U` | A new array containing each callback result, in input order. `transform` is `func(T) -> E!U`. |
| `filter(list, is_kept)` | `E![]T` | A new array containing the items for which `func(T) -> E!bool` returns true, preserving order and references to nested values. |
| `pop(mut list)` | `ArrayError!T` | Removes the last item and returns it. Fails with `ArrayError.Empty` on an empty array. |
| `pop_or_null(mut list)` | `?T` | Removes the last item and returns it, or `null` on an empty array. |
| `insert(mut list, index, mut item)` | `ArrayError![]T` | Puts `item` at `index`, moving the later items up. `index` can be `len(list)` (the end). Fails with `ArrayError.OutOfRange` outside `0..len(list)`. |
| `remove(mut list, index)` | `ArrayError!T` | Removes the item at `index`, moving the later items down, and returns it. Fails with `ArrayError.OutOfRange` outside the array. |
| `clear(mut list)` | `[]T` | Removes every item. |
| `reverse(mut list)` | `[]T` | Reverses the items in place. |
| `slice(list, start, stop)` | `ArrayError![]T` | A new array with the items from `start` up to, not including, `stop`. The items are copies, so changing the slice never changes `list`. Fails with `ArrayError.OutOfRange` for a negative bound, `start > stop` or `stop > len(list)`. |
| `index_of(list, item)` | `?int` | The position of the first item equal to `item`, or `null`. |
| `contains(list, item)` | `bool` | Whether some item equals `item`. |

`append`, `extend`, `clear`, `insert`, `reverse`, `sort` and `sort_by` mutate the input and return the same array reference. Pass a copy to preserve the original: `let extended = array::extend(copy(numbers), [4, 5])`, `let reversed = array::reverse(copy(numbers))` or `let cleared = array::clear(copy(numbers))`. Results can be chained, for example `array::sort(array::extend(copy(numbers), [4, 5]))`. `insert` still requires `try` or `catch`. `pop`, `pop_or_null` and `remove` return the removed item instead.

`sort` accepts `int`, `float`, `byte` and `string` arrays. Strings use lexicographic UTF-8 byte order. Floats put NaNs after all other values; signed zeros compare as equal. Both sorts use O(n log n) comparisons and constant auxiliary space; equal items may change relative order. The `sort_by` callback must give a consistent ordering, return false for equal items, and leave the array and its ordering fields unchanged. Use `array::sort(copy(numbers))` to preserve the original array.

`map` and `filter` infer the callback's error set. A non-failing callback gives a plain array result with no `try`; a failing callback stops at the first error and propagates it. `extend`, `map` and `filter` do not deep-copy items; use `copy` when independent nested values are needed. For `map` and `filter`, callbacks must leave the source array unchanged.

`index_of` and `contains` take arrays of `int`, `float`, `byte`, `bool` or `string`, and compare strings by content. Floats compare like `==`, so `nan` is never found and `-0.0` matches `0.0`.

```nim
import std/array: pop
import std/io

func main() -> void
    let mut stack = [1, 2, 3]
    for true do
        let item = pop(stack) catch _ then break end
        io::println("$item")   # 3, 2, 1
    end
end
```

## std/map

Reading and writing entries uses indexing (`m[k]`, `m[k] = v`, see [Maps](language-ref.md#maps)). Functions that change a map take it as `mut`.

| Function | Returns | Description |
| :------- | :------ | :---------- |
| `len(m)` | `int` | The number of entries. |
| `has(m, key)` | `bool` | Whether `key` has an entry, also when its value is `null`. |
| `remove(mut m, key)` | `bool` | Removes the entry for `key`. Returns whether there was one. |
| `keys(m)` | `[]K` | A new array with the keys, in insertion order. |
| `values(m)` | `[]V` | A new array with the values, in insertion order. |
| `clear(mut m)` | `void` | Removes every entry. |

```nim
import std/map
import std/io

func count_words(words: []string) -> map[string, int]
    let mut counts: map[string, int] = {}
    for words: word do
        counts[word] = (counts[word] orelse 0) + 1
    end
    return counts
end

func main() -> void
    let mut counts = count_words(["a", "b", "a"])
    io::println("${counts["a"]}")                 # 2
    map::remove(counts, "b")
    io::println("${map::keys(counts)}")           # [a]
    io::println("${map::len(counts)}")            # 1
end
```

## std/string

String functions never change their argument: a function that produces text returns a new string. They work with pipes: `line |> string::trim()`. The module declares the error set `StringError` (`OutOfRange`, `NegativeCount`, `InvalidUtf8`). Strings compare with `==` (see [Comparison and logic](language-ref.md#comparison-and-logic)).

Positions are **byte offsets**, the same unit as `string::len(s)` and `string::byte_at`, so a position from `string::find` can be passed straight to `string::slice`. `char_count` and `char_at` are the only functions that count characters. The `start` argument of `find` is inclusive; a byte offset inside a multi-byte character searches forward from that byte. `find_reverse` searches backward from an inclusive starting byte offset and returns a match start usable with `slice`. A match may extend beyond the starting offset: `find_reverse("abcdef", "bcd", 1)` returns `1`.

| Function                    | Returns                 | Description                                                                                                                                                       |
| :-------------------------- | :---------------------- | :---------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `len(text)`                 | `int`                   | Number of bytes (UTF-8).                                                                                                                                          |
| `char_count(text)`          | `int`                   | Number of characters (Unicode code points).                                                                                                                       |
| `concat(a, b)`              | `string`                | `b` after `a`.                                                                                                                                                    |
| `join(parts, sep = "")`     | `string`                | The `[]string` parts with `sep` between them (nothing by default); `""` for an empty array.                                                                       |
| `byte_at(text, index)`      | `StringError!byte`      | The byte at byte offset `index`.                                                                                                                                  |
| `char_at(text, index)`      | `StringError!string`    | The `index`-th character, as a one-character string (`string::char_at("olá", 2)` is `"á"`). Scans from the start.                                                 |
| `find(text, needle, start = 0)` | `?int` | Byte offset of the first match at or after `start`, or `null` for no match, an empty needle, or a start outside the string. Negative starts return `null`. |
| `find_reverse(text, needle, start = null)` | `?int` | Byte offset of the rightmost match starting at or before `start`. Omitted/null starts search from the end; past-end starts search from the end too. Negative starts, no match, or an empty needle return `null`. Overlapping matches count. |
| `compare(a, b)` | `int` | Lexicographic UTF-8 byte ordering: exactly `-1` if `a` comes before `b`, `0` if equal, or `1` if it comes after. Case-sensitive, matching `array::sort` on strings. |
| `contains(text, needle)`    | `bool`                  | Whether `needle` appears in `text`; `false` for an empty `needle`.                                                                                                |
| `starts_with(text, prefix)` | `bool`                  | Whether `text` starts with `prefix`; `false` for an empty `prefix`.                                                                                               |
| `ends_with(text, suffix)`   | `bool`                  | Whether `text` ends with `suffix`; `false` for an empty `suffix`.                                                                                                 |
| `count(text, needle)`       | `int`                   | Number of non-overlapping matches (`"aaaa"`, `"aa"` is 2); `0` for an empty `needle`.                                                                             |
| `trim(text)`                | `string`                | Removes spaces, tabs and line breaks (`\t`, `\n`, `\r`, `\v`, `\f`) from both ends.                                                                               |
| `trim_start(text)` | `string` | Removes the same whitespace as `trim`, from the start only. |
| `trim_end(text)` | `string` | Removes the same whitespace as `trim`, from the end only. |
| `upper(text)` / `lower(text)` | `string`              | Changes the case of ASCII and Latin-1 letters (`"ação"` ↔ `"AÇÃO"`). Other characters are unchanged.                                                              |
| `split(text, separator, limit = -1)`    | `[]string`              | The pieces between separators, keeping empty ones: `"a,,b"` gives `["a", "", "b"]`. An empty `separator` splits into characters: `"olá"` gives `["o", "l", "á"]`. A positive `limit` caps the number of parts, preserving the remainder in the last part: `split("a=b=c", "=", 2)` gives `["a", "b=c"]`. A negative limit means unlimited; `0` returns an empty array. |
| `replace(text, old, new)`   | `string`                | Replaces every non-overlapping `old`, left to right. An empty `new` deletes; an empty `old` returns `text` unchanged.                                             |
| `repeat(text, count)`       | `StringError!string`    | `text` repeated `count` times; `0` gives `""`.                                                                                                                    |
| `slice(text, start, stop)`  | `StringError!string`    | The bytes from `start` up to, not including, `stop`: `string::slice(s, 0, string::len(s))` is all of `s`.                                                         |
| `to_int(text)`              | `ConversionError!int`   | Reads an int written like a Dusk literal.                                                                                                                         |
| `to_float(text)`            | `ConversionError!float` | Reads a float (or int) written like a Dusk literal.                                                                                                               |
| `from_bytes(data)`          | `StringError!string`    | Copies a buffer into a new string; fails with `StringError.InvalidUtf8` if it isn't valid UTF-8.                                                                  |

Use `string::compare` for string fields in custom sorting: `array::sort_by(players, func(a, b) -> return string::compare(a.name, b.name) < 0)`. It compares bytes without case folding or locale rules.

`to_int` and `to_float` share their names with the prelude's conversions, so `import std/string: to_int` is an `AlreadyDefined` error: call them as `string::to_int`, or rename them (`import std/string: to_int as parse_int`).

```nim
import std/string
import std/io

func describe_age(text: string) -> string
    let age = string::to_int(text) catch e then
        return when e
            ConversionError.Invalid then "not a number" end
            ConversionError.Overflow then "too big" end
        end
    end
    return "age $age"
end

func main() -> void
    let line = "  name = Dusk, age = 13  "
    for string::split(string::trim(line), ","): field do
        let pair = string::split(field, "=")
        io::println("${string::upper(string::trim(pair[0]))}: ${string::trim(pair[1])}")
    end
    io::println(describe_age("13"))     # age 13
    io::println(describe_age("abc"))    # not a number
end
```

## std/buffer

| Function            | Returns              | Description                                                                                       |
| :------------------ | :------------------- | :------------------------------------------------------------------------------------------------ |
| `len(data)`         | `int`                | Number of bytes.                                                                                  |
| `new(size)`         | `BufferError!buffer` | A new buffer of `size` zero bytes; fails with `BufferError.NegativeSize` for a negative size.     |
| `from_string(text)` | `buffer`             | Copies a string's bytes into a new buffer.                                                        |
| `put(data, at, text)` | `BufferError!int` | Writes the text's bytes into `data` starting at `at` and returns the position after them. |
| `put_bytes(data, at, source)` | `BufferError!int` | Same, copying another buffer. |
| `put_int(data, at, number)` | `BufferError!int` | Writes `number` as decimal text (`-42` is the three bytes `-`, `4`, `2`), without making a string. |
| `slice(data, start, stop)` | `BufferError!buffer` | A new buffer with a copy of bytes `start` to `stop` (not included). |

`put`, `put_bytes` and `put_int` take `data` as `mut` and change it in place. When what they write doesn't fit between `at` and the end of the buffer, or `at` is outside it, they fail with `BufferError.OutOfRange` and write nothing; `slice` fails the same way for bounds outside the buffer or `start > stop`. A buffer never grows: size it with `buffer::new(n)` and reuse it, writing frames, lines or messages into it and sending the part used with `slice`:

```nim
let mut frame = try buffer::new(4096)
let mut used = try buffer::put(frame, 0, "progress: ")
used = try buffer::put_int(frame, used, done)
used = try buffer::put(frame, used, "%\n")
try out.write_bytes(try buffer::slice(frame, 0, used))
```

The module declares the error set `BufferError` (`NegativeSize`, `OutOfRange`). See [Buffers](language-ref.md#buffers).

## std/compression

Compressing and decompressing bytes. Import with `import std/compression`.

| Function                                | Returns                         | Description                                                                 |
| :-------------------------------------- | :------------------------------ | :-------------------------------------------------------------------------- |
| `compress(data, format, level = 6)`     | `CompressionError!buffer`       | Compresses `data` in the given format.                                      |
| `decompress(data, format, max = -1)`    | `CompressionError!buffer`       | Decompresses `data`; fails with `TooLarge` past `max` bytes (`-1`: no limit). |

`format` is a `compression::Format`: `Gzip` (what `.gz` files and HTTP's `gzip` use), `Zlib` (HTTP's `deflate`) or `Deflate` (raw deflate data, no header). `level` goes from `0` (stored, no compression) to `9` (smallest output, slowest); `6` is a good default, and `1` is much faster for a slightly bigger result. A level outside `0..9` fails with `InvalidLevel`.

```nim
let packed = try compression::compress(buffer::from_string(report), compression::Format.Gzip)
try fs::write_bytes("report.txt.gz", packed)

let text = try string::from_bytes(try compression::decompress(try fs::read_bytes("report.txt.gz"), compression::Format.Gzip))
```

`decompress` checks the data's checksum and fails with `InvalidData` for corrupted or cut-short data, data in another format, or bytes left after the end. A gzip file made of several members back to back (as `cat a.gz b.gz` makes) decodes to their contents joined. Pass `max` whenever the data comes from outside: a few kilobytes of compressed data can expand to gigabytes.

Inputs over 256 KiB are compressed or decompressed on a background thread: only the calling task waits, and other tasks keep running. Smaller ones are done right away.

**Streams.** For data that arrives or leaves in pieces (a file read in chunks, a response sent as it's produced), `compression::Compressor(format, level = 6)` and `compression::Decompressor(format, max = -1)` take bytes in and give bytes out, keeping their state between calls. They do no I/O themselves, so their output can go anywhere: a file, a connection, a buffer.

| Method                          | Returns                    | Description                                                                                       |
| :------------------------------ | :------------------------- | :------------------------------------------------------------------------------------------------ |
| `Compressor.write_bytes(data)`  | `CompressionError!buffer`  | Compresses more input; returns the compressed bytes ready so far, often none for small inputs.   |
| `Compressor.flush()`            | `CompressionError!buffer`  | Returns everything still held back, so what was sent so far can be decompressed on the other side. |
| `Compressor.finish()`           | `CompressionError!buffer`  | Ends the stream (the rest plus the format's trailer).                                             |
| `Decompressor.write_bytes(data)`| `CompressionError!buffer`  | Decompresses more input; returns the bytes ready so far.                                          |
| `Decompressor.finish()`         | `CompressionError!void`    | Ends the stream; fails with `InvalidData` if it was cut short.                                    |
| `Decompressor.is_done()`        | `bool`                     | Whether the input seen so far is a whole stream.                                                  |
| `close()` (both)                | `void`                     | Drops a stream without finishing it.                                                              |

```nim
let mut compressor = compression::Compressor(compression::Format.Gzip, level = 1)
let mut out = try fs::open("log.gz", fs::Mode.Write)
for lines: line do
    try out.write_bytes(try compressor.write_bytes(buffer::from_string(line)))
end
try out.write_bytes(try compressor.finish())
out.close()
```

A level outside `0..9` fails with `InvalidLevel` on the first call. Once `finish()` or `close()` ran, every call fails with `Finished`. `flush()` costs a few bytes of output each time, so call it when the other side needs what was sent (a message, an event), not after every write. A `Decompressor` follows the same rules as `decompress`: a gzip stream may hold several members, other bytes after the end fail with `InvalidData`, and `max` counts all its output across calls. After a failure, `close()` the stream. A stream that's neither finished nor closed keeps its memory (about 300 KB for a compressor) until the garbage collector finds it unreachable, or the program ends.

**Wrapping readers and writers.** `compression::writer(inner, format, level = 6)` returns a `compression::Writer` that compresses what's written to it into `inner`, any [`io::Writer`](#stdio) (a file, a connection, your own struct), and `compression::reader(inner, format, max = -1)` returns a `compression::Reader` that decompresses what it reads from `inner`, any `io::Reader`. Both fit the same traits, so they work with `io::copy` and anywhere else a reader or writer goes. They fail with `InvalidLevel` right away for a bad level.

```nim
let mut file = try fs::open("data.json.gz", fs::Mode.Write)
let mut gz = try compression::writer(file, compression::Format.Gzip)
try gz.write(json_text)
try gz.finish()
file.close()

let mut source = try fs::open("data.json.gz")
let mut plain = try compression::reader(source, compression::Format.Gzip, max = 100000000)
let copied = try io::copy(plain, io::stdout())
```

A `Writer` has `write`, `write_bytes`, `flush` (also flushes `inner`) and `finish`, which ends the compressed stream and must be called last; it doesn't close `inner`. A `Reader` has `read_bytes(max, timeout_ms = -1)`, returning `null` after the end of the compressed stream, and `close()`. Errors are `io::IoError`: input that ends before the compressed stream does is `UnexpectedEnd`, bad data or going past `max` is `Other`, and writing after `finish` is `Closed`. `timeout_ms` applies to each read from `inner`.

The module declares the error set `CompressionError` (`InvalidData`, `TooLarge`, `InvalidLevel`, `Finished`, `Other`). It uses a bundled zlib-ng.

## std/process

Runs other programs. `run` starts one, collects its output and waits for it to finish; `start` gives back a `Child` to talk to while it runs. Waiting parks only the running task: other tasks, timers and I/O keep going (see [Tasks](language-ref.md#13-tasks)).

```nim
import std/process
import std/io

func main() -> !void
    let result = try process::run(["git", "--version"])
    io::print(try result.stdout_text())
    assert(result.is_success())
end
```

Functions that can fail return an error union with the module's error set `ProcessError`:

| Variant | When |
| :------ | :--- |
| `InvalidArgument` | An empty argument list or program name, a zero byte in an argument, path or variable, a bad variable name, or a negative `timeout_ms` (other than `-1`) or `max_output` |
| `NotFound` | The program isn't on `PATH`, the path doesn't exist, or `cwd` doesn't exist |
| `PermissionDenied` | The program exists but can't be run |
| `InvalidExecutable` | The file can't be run as a program (on Windows, a `.bat` or `.cmd` file: run it through its interpreter) |
| `ResourceLimit` | The system won't start another process |
| `TimedOut` | `timeout_ms` ran out |
| `OutputTooLarge` | `run` received more than `max_output` bytes |
| `InvalidUtf8` | An argument, path or variable isn't valid UTF-8, or `stdout_text` / `stderr_text` on output that isn't text |
| `Unsupported` | Not available here (a program embedding Dusk without a host, or inheriting an output that the embedding program redirected) |
| `Busy` | Another task is already waiting for the child, or `close` while it's still running |
| `Closed` | The child was closed |
| `NotPiped` | `stdin()`, `stdout()` or `stderr()` on a stream that wasn't set to `Stdio.Pipe` |
| `Other` | Any other failure |

| Function | Returns | Description |
| :------- | :------ | :---------- |
| `run(args, cwd = null, env = null, timeout_ms = -1, max_output = 16777216)` | `ProcessError!Result` | Runs a program and waits for it, collecting its stdout and stderr. |
| `start(args, cwd = null, env = null, stdin_mode = Stdio.Ignore, stdout_mode = Stdio.Inherit, stderr_mode = Stdio.Inherit)` | `ProcessError!Child` | Starts a program and returns right away. |

**Arguments.** `args[0]` is the program and the rest are passed exactly as written: spaces, quotes and empty strings stay as they are. Nothing goes through a shell, so `*`, `>` and `|` are plain characters. A program name with a `/` (or on Windows a `\` or `:`) is a path, relative to the child's folder; a bare name is looked up in the folders of `PATH`, from the program's own environment even when `env` gives the child a different one. Empty or relative `PATH` entries are relative to the child's folder. On Windows the lookup also tries the `.exe` and `.com` endings.

**Folder and environment.** `cwd` is the child's folder; a relative one is relative to the program's current folder. `env = null` gives the child the program's environment (the same one `os::env` reads); a map replaces it entirely (on Windows the system may add a few variables it needs). Names can't be empty or contain `=`. The child's stdin is empty: it reads the end of input right away.

**Results.** `Result` holds `stdout: buffer`, `stderr: buffer` and `termination: Termination`, a union of `Exited: int` (the exit code) and `Signaled: int` (the signal number, which differs between systems). A program that exits with a code other than 0 isn't an error: check `is_success()`, true only for `Exited = 0`. `stdout_text()` and `stderr_text()` give the output as a string, failing with `InvalidUtf8` when it isn't text; the buffers stay available either way.

**Limits.** `max_output` caps the bytes of stdout and stderr together (16 MiB by default; `0` allows no output at all). Going over kills the child and fails with `OutputTooLarge`: the output is never cut short quietly. `timeout_ms = -1` waits forever and `0` fails at once. The time covers starting the program, reading its output and its exit, so it still runs while a child that closed its output keeps going, or while a program it started keeps the output open. When it runs out, the child is killed and `run` fails with `TimedOut` once the child is really gone, which can take a moment past the deadline. Programs the child started itself aren't killed.

### Children

`start` takes the same arguments, folder and environment as `run`. Each standard stream is `Stdio.Inherit` (shared with the program), `Stdio.Ignore` or `Stdio.Pipe` (read or written through the `Child`). Before a child that inherits stdout or stderr starts, the program's own pending output is written, so the two don't mix out of order. When a child inherits stdin, the program stops reading stdin ahead (unless a task is reading it right then), so input that arrives while the child runs goes to the child. Input the program already received stays with the program, and its next read of stdin picks up again.

| Method | Returns | Description |
| :----- | :------ | :---------- |
| `wait(timeout_ms = -1)` | `ProcessError!Termination` | Waits for the child to exit. Calling it again gives the same answer. Running out of time fails with `TimedOut` and leaves the child running. |
| `kill()` | `ProcessError!void` | Asks the system to stop the child, without waiting; `wait` tells when it's gone. Does nothing once it has exited. |
| `stdin()` | `ProcessError!PipeWriter` | Writes to the child's stdin. |
| `stdout()` / `stderr()` | `ProcessError!PipeReader` | Reads the child's output. |
| `close()` | `ProcessError!void` | Frees the child once it has exited, closing its pipes and dropping unread output. Fails with `Busy` while it runs; closing again does nothing. |

Only one task at a time can wait for a child. Copies of a `Child` (including `copy`) all refer to the same process, so closing one closes them all.

`PipeReader` has `read_line(max = -1, timeout_ms = -1)`, `read_bytes(max, timeout_ms = -1)`, `read_exact(count, timeout_ms = -1)`, `read_all(timeout_ms = -1)`, `read_all_bytes(timeout_ms = -1)` and `close()`; `PipeWriter` has `write(text)`, `write_bytes(data)`, `flush()` and `close()`. They fail with `io::IoError`, read and write like the other streams (see [std/io](#stdio)), and fit the `io::Reader` and `io::Writer` traits, so `io::copy` works with them.

Closing the stdin writer sends the end of input; closing a pipe doesn't stop the child, and output the child wrote before exiting can still be read. Read stdout and stderr in their own tasks while waiting (a child blocks once a pipe it writes to is full), or let `run` collect them.

**Lifetime.** The program doesn't end while a child it started is still running, even if nothing waits for it: like a task, a child runs to the end. Once all of the program's tasks have finished, nothing can use the children's pipes anymore, so they're closed: a child waiting for input sees the end of it, and one writing gets a broken pipe. When the program stops on an error instead, its remaining children are killed. Close children when done with them. A `Child` the program no longer reaches (no variable, field or copy holds it or one of its pipes) is cleaned up after a garbage collection finds it: one that has exited is freed, and one still running has its pipes closed, so it sees the end of its input, and is freed when it exits.

## std/random

Random numbers and bytes, plus seeded generators that repeat the same sequence. Functions that can fail return an error union with the module's error set `RandomError`:

| Variant | When |
| :------ | :--- |
| `InvalidBounds` | `integer` with `min > max` |
| `NegativeSize` | `bytes` or `secure_bytes` with a negative size |
| `EntropyUnavailable` | The system's secure random source failed |
| `Unsupported` | No random source here (a program embedding Dusk without a host) |

| Function | Returns | Description |
| :------- | :------ | :---------- |
| `integer(min, max)` | `RandomError!int` | A random `int` from `min` to `max`, both included. Any range works, up to the whole `int` range. |
| `decimal()` | `RandomError!float` | A random `float` from `0.0` (included) to `1.0` (not included). |
| `bytes(size)` | `RandomError!buffer` | A new buffer of `size` random bytes. |
| `secure_bytes(size)` | `RandomError!buffer` | A new buffer of `size` bytes from the system's secure random source. Use it for keys, tokens and anything secret. |
| `seeded(seed)` | `Generator` | A generator that gives the same sequence for the same seed. |

`integer`, `decimal` and `bytes` share one sequence per program, started from the system's random source on first use; tasks draw from it in the order they run. They're fast but not secure: for secrets, use `secure_bytes`, which reads the system source on every call, never falls back to anything weaker, and fails with `EntropyUnavailable` instead of returning a partial buffer. Equal bounds (`integer(5, 5)`) and zero sizes don't use the sequence at all, and neither do calls that fail.

A `Generator` holds its own state, so it needs `let mut`, and draws from it don't touch the shared sequence:

| Method | Returns | Description |
| :----- | :------ | :---------- |
| `integer(min, max)` | `RandomError!int` | Like `random::integer`. |
| `decimal()` | `float` | Like `random::decimal`, but can't fail. |
| `bytes(size)` | `RandomError!buffer` | Like `random::bytes`. |

Any `int` is a valid seed. The same seed and the same calls (with the same arguments, in the same order) give the same results; a different Dusk version may give different ones. Two `bytes(4)` calls don't have to match one `bytes(8)`. Generators are structs: two variables holding the same generator share its state, and `copy(rng)` makes one that continues on its own from the same point.

```nim
let die = try random::integer(1, 6)
let mut rng = random::seeded(42)
let mut replay = copy(rng)
assert(try rng.integer(-10, 10) == try replay.integer(-10, 10))
let token = try random::secure_bytes(32)
```

## std/path

Works with paths as text: joining, splitting and cleaning them up. Nothing here touches the disk: these functions don't check that a path exists, don't follow symbolic links and don't read the current folder. Paths use the system's rules: `/` on Linux and macOS; on Windows, drive letters and network shares with `\` or `/`.

| Function | Returns | Description |
| :------- | :------ | :---------- |
| `join(parts)` | `string` | Joins the parts with the system's separator, without doubling one where two parts meet. `join([])` is `""`. |
| `normalize(text)` | `string` | Removes repeated separators, `.` parts, and `..` parts with a folder before them. An empty result is `"."`. |
| `basename(text)` | `string` | The last part (`"logo.png"` for `"assets/logo.png"`), ignoring a separator at the end. `""` for an empty path or a root. |
| `dirname(text)` | `?string` | Everything before the last part, or `null` for an empty path, a bare name or a root. |
| `extension(text)` | `string` | The last part's extension with its dot (`".gz"` for `"a.tar.gz"`), or `""`. |
| `stem(text)` | `string` | The last part without its extension (`"a.tar"`). |
| `is_absolute(text)` | `bool` | Whether the path starts from a root. |
| `resolve(base, text)` | `PathError!string` | `text` taken from the absolute folder `base`, normalized. An absolute `text` stays as it is (normalized). |
| `relative(from, to)` | `PathError!string` | The path that leads from the absolute folder `from` to the absolute path `to`; `"."` when they're the same. |

`resolve` and `relative` fail with `PathError.NotAbsolute` when a path that must be absolute isn't; there's no implicit current folder, so pass `os::cwd()` when that's what you mean.

- `join` doesn't clean up: `join(["a", "/b"])` is `"a/b"` and `.` or `..` parts stay. `normalize` and `resolve` clean up.
- `normalize` keeps the `..` parts a relative path starts with (`"a/../../b"` is `"../b"`), stops `..` at the root of an absolute path (`"/../etc"` is `"/etc"`), and drops a separator at the end unless the path is a root.
- A name that only starts with a dot has no extension: `extension(".env")` is `""` and its stem is `".env"`. `extension(".env.local")` is `".local"` and `extension("name.")` is `"."`. `.` and `..` have none.
- `basename`, `dirname`, `extension` and `stem` return the text as written, without normalizing it.

On Windows, `is_absolute` is true for a drive with a root (`C:\x`), a network share or a device path; `\x` (no drive) and `C:x` (no root) aren't absolute. `resolve` can complete `\x` from a base with a drive, and fails with `NotAbsolute` when the result still has no root. Normalized paths use `\` and uppercase drive letters. `relative` between different drives or shares returns the destination itself. Names compare by the system's path rules, not by asking the disk.

These functions don't check that a name is allowed on the disk, and don't expand `~`, variables, wildcards or URL escapes. `..` is handled as text, so through a symbolic link it can lead somewhere else than on the disk, and resolving against a folder doesn't guarantee the result stays inside it.

```nim
let base = try os::cwd()
let file = try path::resolve(base, "assets/./images/../logo.png")
assert(path::basename(file) == "logo.png")
assert(path::stem(file) == "logo")
assert(path::extension(file) == ".png")
assert(try path::relative(base, file) == path::join(["assets", "logo.png"]))
```

## std/os

The program's surroundings: its arguments, environment, current folder and exit code, and signals asking it to stop. Functions that can fail return an error union with the module's error set `OsError`:

| Variant | When |
| :------ | :--- |
| `InvalidArgument` | `env` with an empty name or one containing `=`, or `set_exit_code` outside `0..255` |
| `InvalidUtf8` | An argument, environment value or the current folder isn't valid UTF-8 |
| `NotFound` | The current folder no longer exists |
| `Unsupported` | Not available on this system (`Signal.Terminate` on Windows, or a program embedding Dusk without a host) |
| `Busy` | Another task is already waiting on the signal stream |
| `Closed` | The signal stream was closed |
| `Other` | Any other failure |

| Function | Returns | Description |
| :------- | :------ | :---------- |
| `platform()` | `Platform` | The system the program runs on: `Linux`, `MacOS`, `Windows` or `Other`. |
| `args()` | `OsError![]string` | The program's arguments. |
| `env(name)` | `OsError!?string` | An environment variable's value, or `null` when it isn't set (a variable set to nothing is `""`). |
| `cwd()` | `OsError!string` | The current folder, as an absolute path. |
| `set_exit_code(code)` | `OsError!void` | The code the program exits with when it ends normally. The program keeps running. |
| `watch_signal(signal)` | `OsError!SignalStream` | Starts listening for a signal. |

**Arguments and environment.** For `dusk app.dsk first --flag`, `args()` is `["first", "--flag"]`: everything after the script belongs to the program, `--version` included. The environment is the one the program started with; names follow the system's rules for case. This module doesn't change variables or the current folder. The current folder is the one the program was started from, not necessarily the script's folder.

**Exit code.** The exit code starts at `0`. `set_exit_code` takes `0` to `255`; a bad code fails and keeps the previous one. The last successful call wins, from any task. Setting it doesn't end anything: the program still ends when all its tasks are done, and output is written out first. A runtime error or a task error nobody handled still exits with a failure, whatever the code.

When a program embeds Dusk, it passes the arguments and environment in (both empty by default) and reads the code from `Dusk.exit_code` afterwards.

### Signals

`Signal` is `Interrupt` (usually Ctrl+C), `Terminate` (a request to stop, like the one a service manager sends) or `WindowChange` (the terminal window was resized; see [Terminal programs](#terminal-programs)). All three work on Linux and macOS; on Windows, `Interrupt` and `WindowChange` come from the console and `Terminate` fails with `Unsupported`. Ctrl+C doesn't send `Interrupt` while the terminal is in raw mode. `WindowChange` never stops a program, whether it listens or not.

| Method | Returns | Description |
| :----- | :------ | :---------- |
| `wait()` | `OsError!void` | Waits for the signal (or returns at once if it already arrived). Other tasks keep running. |
| `close()` | `void` | Stops listening. Closing again does nothing. |

While a program listens for a signal, the signal no longer stops it: it's delivered to the stream instead, and the program decides what to do (finish its work, close connections) and ends on its own. Closing the last stream for a signal brings back the default, so the next one stops the program.

- Listen before starting the work, so a signal that arrives early isn't missed.
- A stream remembers at most one signal that arrived while nobody was waiting; several in a row count as one. Each stream gets its own copy.
- Only one task at a time can wait on a stream. Closing it wakes that task with `Closed`, and waiting on a closed stream fails with `Closed`.
- A stream nobody waits on doesn't keep the program running; a task waiting on one does. Streams still open when the program ends are closed. A `SignalStream` the program no longer reaches is closed after a garbage collection finds it.
- Signals don't cancel tasks or run cleanup on their own: the task that waits does the cleanup.

```nim
let shutdown = try os::watch_signal(os::Signal.Interrupt)
io::println("Press Ctrl+C to stop.")
try shutdown.wait()
shutdown.close()
io::println("Shutting down...")
```

## std/time

Clocks, timestamps, durations and time zones. A [complete example](../examples/time.dsk) uses most of the module. Functions that can fail return an error union with the module's error set `TimeError`:

| Variant | When |
| :------ | :--- |
| `InvalidTimestamp` | `from_iso_string` with text that isn't a valid timestamp |
| `Overflow` | A result outside the range a `Timestamp` or `Duration` can hold |
| `PrecisionLoss` | Adding a duration with a fraction of a millisecond to a timestamp, or writing an offset with seconds in it as ISO text or with the `Z` token |
| `UnknownTimeZone` | `load_timezone` with a name the bundled database doesn't have |
| `InvalidOffset` | `fixed_timezone` with an offset of a day or more |
| `LocalTimeUnavailable` | `local_timezone` can't find or read the system's zone |
| `InvalidFormat` | `format` with a pattern it doesn't understand |

| Function | Returns | Description |
| :------- | :------ | :---------- |
| `now()` | `Timestamp` | The current date and time. |
| `from_unix_ms(milliseconds)` | `Timestamp` | The timestamp `milliseconds` after 1970-01-01T00:00:00Z (negative for earlier). Any `int` works. |
| `to_unix_ms(timestamp)` | `int` | The other way around. |
| `from_iso_string(text)` | `TimeError!Timestamp` | Reads ISO text such as `2024-07-15T09:30:00Z` (below). |
| `to_iso_string(timestamp, zone = utc_timezone())` | `TimeError!string` | Writes ISO text in a time zone, UTC by default. |
| `to_utc_iso_string(timestamp)` | `string` | Writes ISO text in UTC. Never fails. |
| `nanoseconds(value)` | `Duration` | A duration of `value` nanoseconds (negative is fine). |
| `microseconds(value)`, `milliseconds(value)`, `seconds(value)`, `minutes(value)`, `hours(value)` | `Duration` | A duration of `value` of that unit. |
| `instant()` | `Instant` | A reading of the monotonic clock, for measuring how long something takes. |
| `utc_timezone()` | `TimeZone` | UTC. |
| `fixed_timezone(offset_seconds)` | `TimeError!TimeZone` | A zone always `offset_seconds` ahead of UTC (behind when negative), less than a day either way. |
| `load_timezone(name)` | `TimeError!TimeZone` | A named zone such as `"America/Sao_Paulo"`. |
| `local_timezone()` | `TimeError!TimeZone` | The system's zone. |
| `now_ms()` | `int` | The current time as Unix milliseconds, without making a `Timestamp`. |
| `monotonic_ns()` | `int` | The monotonic clock as nanoseconds, without making an `Instant`. |

### Timestamps

A `Timestamp` is a moment, stored as milliseconds since 1970-01-01T00:00:00Z. It doesn't change: its methods return new values. `==` on two timestamps compares identity, like any struct, so compare moments with these methods:

| Method | Returns | Description |
| :----- | :------ | :---------- |
| `is_equal(other)` / `is_before(other)` / `is_after(other)` | `bool` | Compares two moments. |
| `add(duration)` / `subtract(duration)` | `TimeError!Timestamp` | The moment `duration` later or earlier. The duration must be whole milliseconds (`PrecisionLoss` otherwise). |
| `since(other)` | `TimeError!Duration` | How long after `other` this moment is (negative when before). `Overflow` when that's more than a `Duration` holds. |
| `format(pattern, zone = utc_timezone())` | `TimeError!string` | Writes the date and time with a pattern (see [Custom formats](#custom-formats)). |

Durations measure elapsed time, so there's no adding "a month": months and years aren't a fixed length.

**ISO text.** `to_iso_string` writes `2024-07-15T09:30:00.000Z`: always three digits of milliseconds, and `Z` for UTC or the offset (`-03:00`) in another zone. The date and time are the ones on the clock in that zone, so the same moment reads `2024-07-14T20:00:00.000-04:00` in New York. Years 0000 to 9999 have four digits; later years start with `+` (`+10000-01-01T00:00:00.000Z`) and years before 1 with `-`. The calendar is the Gregorian one, extended back past year 1 through year 0, and there are no leap seconds. Every `int` timestamp can be written, and what `to_iso_string` writes always reads back as the same moment. An offset that isn't a whole number of minutes can't be written as ISO text: `to_iso_string` fails with `PrecisionLoss` instead of rounding (`to_utc_iso_string` always works).

`from_iso_string` reads the same form: a date, an uppercase `T`, a time, and then `Z` or an offset like `+05:30`, with nothing before or after. Seconds can have a fraction of one or more digits; past milliseconds, digits are checked and dropped. It rejects leap seconds, `24:00`, a date or time on its own, week and ordinal dates, and ranges. The offset only places the moment and isn't kept: `1970-01-01T05:30:00+05:30` is the same timestamp as `from_unix_ms(0)`.

`to_utc_iso_string` with `from_iso_string`, and `to_unix_ms` with `from_unix_ms`, are pairs of one-argument functions, so a schema's `via` can store timestamps as text or as numbers: `via(time::to_utc_iso_string, time::from_iso_string)` (see [std/serialization](#stdserialization)). `to_iso_string` doesn't fit there, since it takes a zone and can fail.

```nim
let epoch = time::from_unix_ms(0)
io::println(try time::to_iso_string(epoch))                       # 1970-01-01T00:00:00.000Z
io::println(try time::to_iso_string(time::from_unix_ms(-1)))      # 1969-12-31T23:59:59.999Z
let later = try epoch.add(time::seconds(2))
assert(later.is_after(epoch))
assert((try later.since(epoch)).milliseconds() == 2000)
```

### Durations

A `Duration` is a length of time in nanoseconds, positive or negative, up to about 292 years either way. A factory given more (`time::hours(2562048)`) stops the program with a runtime error, since no correct program asks for that; adding durations is checked and fails with `Overflow` instead.

| Method | Returns | Description |
| :----- | :------ | :---------- |
| `nanoseconds()` | `int` | The exact length. |
| `microseconds()` / `milliseconds()` | `int` | Whole units, dropping the rest (toward zero: `-1.5` ms is `-1`). |
| `seconds()` | `float` | Seconds with a fraction (very long durations lose some precision). |
| `add(other)` / `subtract(other)` | `TimeError!Duration` | The sum or difference. |
| `is_equal(other)` / `is_less_than(other)` / `is_greater_than(other)` | `bool` | Compares two lengths. |

`task::sleep` takes a `Duration`: `task::sleep(time::milliseconds(250))`.

### Measuring elapsed time

`now()` follows the system clock, which can jump when someone sets it or it syncs. To measure how long something takes, use an `Instant`, a reading of the monotonic clock, which only moves forward:

| Method | Returns | Description |
| :----- | :------ | :---------- |
| `elapsed()` | `TimeError!Duration` | How long ago the instant was taken. |
| `since(other)` | `TimeError!Duration` | How long after `other` this instant was taken (negative when before). |

```nim
let started = time::instant()
work()
io::println("took ${(try started.elapsed()).seconds()} s")
```

Two readings in a row can be equal, and nanosecond units don't promise nanosecond precision. Elapsed time includes time spent waiting (for other tasks or I/O); whether it includes time the computer sleeps depends on the system. An instant only means something within one run of the program: don't store it or compare it across runs or machines. `now_ms()` and `monotonic_ns()` read the same two clocks as plain `int`s. Reading a clock never fails or waits; if the system clock itself fails, that's a runtime error.

### Time zones

A `TimeZone` turns a moment into the date and time on a clock somewhere. Dusk carries its own copy of the IANA time zone database (release 2026f, 597 names and aliases), so named zones work the same everywhere, with nothing to install and no network.

| Method | Returns | Description |
| :----- | :------ | :---------- |
| `name()` | `string` | The zone's name: the name it was loaded with (an alias keeps its own name), `UTC`, `UTC+05:30` for a fixed offset, or `Local` for system rules without a name. |
| `offset_seconds(timestamp)` | `int` | How far ahead of UTC the zone's clocks are at that moment, in seconds (negative when behind). |

- `load_timezone` takes the exact, case-sensitive name (`America/New_York`, `Asia/Kathmandu`). It doesn't take file paths or offsets like `+03:00`; use `fixed_timezone` for an offset.
- A named zone follows its full history of changes (summer time, governments moving the offset) and its current rules into the future. Rules can change after this release of the database. Two zones with the same offset today can differ on other dates.
- A fixed zone never changes. `fixed_timezone(0)` is UTC; others are named like `UTC+05:30` and `UTC-00:00:30`.
- Summer time can skip or repeat an hour on the clock; ISO text tells repeated times apart by their offset.

```nim
let new_york = try time::load_timezone("America/New_York")
let summer = try time::from_iso_string("2024-07-15T00:00:00Z")
assert(new_york.offset_seconds(summer) == -14400)
assert((try time::to_iso_string(summer, new_york)) == "2024-07-14T20:00:00.000-04:00")
assert((try time::fixed_timezone(19800)).name() == "UTC+05:30")
```

**The system's zone.** `local_timezone()` finds the zone the system is set to. The first time, it reads the settings in the background while other tasks run; after it succeeds, later calls return the same zone at once, even if the settings change. If it fails, it returns `LocalTimeUnavailable` (never UTC as a guess), and a later call tries again. Nothing else in the module uses the system's zone: without a zone argument, everything is UTC.

When the `TZ` environment variable is set, it decides, on every system:

- empty: UTC;
- a zone name (`America/New_York`, or `:America/New_York`): that zone from the bundled database;
- an absolute path to a zone file (optionally after `:`): the rules in that file, named `Local`;
- a rule string such as `STD-5:30` or one with summer time rules: those rules, named `Local`. In these strings the sign is reversed: `-5:30` means 5 hours 30 minutes ahead of UTC;
- anything else, or anything after `:` that isn't a name or a path: `LocalTimeUnavailable`, without trying other sources.

Without `TZ`, Linux and macOS read `/etc/localtime`: when it links to a known zone, that zone is used from the bundled database; otherwise its rules are read and named `Local`. Windows reads the system's zone setting and maps it to a named zone. A zone file must be a regular file of at most 4 MiB holding complete, supported rules; files with leap seconds, version 4 files and broken data aren't accepted. On Windows, customized settings (such as automatic summer time turned off) aren't supported.

### Custom formats

`timestamp.format(pattern, zone = utc_timezone())` writes the date and time in a zone, following a pattern:

| Token | Writes | Example |
| :---- | :----- | :------ |
| `YYYY` | Year, at least four digits (signs like ISO text) | `2026` |
| `MM` | Month | `11` |
| `DD` | Day | `18` |
| `HH` | Hour, `00` to `23` | `14` |
| `mm` | Minute | `30` |
| `ss` | Second | `45` |
| `SSS` | Millisecond | `123` |
| `Z` | Offset: `Z` for UTC, otherwise `+HH:MM` or `-HH:MM` | `-03:00` |

Tokens are case-sensitive and can touch (`YYYYMMDD`). Every run of the same letter must be a whole token, so `MMMM`, `YYYYY`, `SS` and `ZZ` are errors, and so is any other letter outside brackets; digits, punctuation, spaces and other characters are written as they are. Text in brackets is written as is (`[at]`, `[Z]` for a literal Z); `[[` and `]]` write `[` and `]`. An unclosed bracket or a lone `]` is `InvalidFormat`. `Z` fails with `PrecisionLoss` for an offset with seconds, which old local times have: midnight UTC on 1800-01-01 is `00:09:21` in `Europe/Paris`. Without `Z`, those times are written exactly.

```nim
let timestamp = try time::from_iso_string("2026-11-18T14:30:45.123Z")
assert((try timestamp.format("DD/MM/YYYY")) == "18/11/2026")
assert((try timestamp.format("[Created on] DD/MM/YYYY [at] HH:mm")) == "Created on 18/11/2026 at 14:30")
assert((try timestamp.format("[[YYYY]]")) == "[2026]")
assert((try timestamp.format("YYYY-MM-DD[T]HH:mm:ss.SSSZ")) == (try time::to_iso_string(timestamp)))
```

## std/math

Numbers. Import with `import std/math`.

| Name | Returns | Description |
| :--- | :------ | :---------- |
| `pi`, `tau`, `e` | `float` | The constants π, 2π and e. |
| `inf`, `nan` | `float` | Positive infinity and not-a-number. |
| `min(values)`, `max(values)` | `MathError!T` | The smallest or largest item of an `[]int` or `[]float`; fails with `MathError.Empty` for an empty array. |
| `abs(value)` | `int` | Absolute value of an `int`. `abs` of the smallest `int` doesn't fit and stops the program, like any integer overflow. |
| `f_abs(value)` | `float` | Absolute value of a `float`. |
| `clamp(value, lower, upper)`, `f_clamp(...)` | `MathError!int` / `MathError!float` | `value` limited to `lower..upper`; fails with `MathError.InvalidBounds` when `lower > upper` (or a bound is `nan`). `f_clamp` keeps a `nan` value as it is. |
| `floor(value)`, `ceil(value)`, `round(value)`, `trunc(value)` | `int` | A `float` rounded down, up, to the nearest (halves away from zero: `round(2.5)` is `3`, `round(-2.5)` is `-3`) or toward zero, as an `int`. |
| `sqrt`, `pow(base, exponent)`, `exp`, `log`, `log2`, `log10` | `float` | Roots, powers and logarithms. Out-of-domain inputs give `nan` (`sqrt(-1.0)`, `log(-1.0)`), not an error. |
| `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `atan2(y, x)` | `float` | Trigonometry, in radians. |
| `is_nan`, `is_inf`, `is_finite` | `bool` | Checks for `nan`, infinities, and neither. |

`floor`, `ceil`, `round` and `trunc` return an `int`, so a `float` that isn't a finite number in the `int` range (`nan`, an infinity, `1e300`) stops the program. When the value comes from outside (parsed text, a computation that can overflow), check it with `math::is_finite` first, or use the prelude's `to_int`, which truncates toward zero and fails with `ConversionError.Overflow` instead of stopping.

The module declares the error set `MathError` (`InvalidBounds`, `Empty`).

## std/task

| Function    | Returns | Description                                                     |
| :---------- | :------ | :-------------------------------------------------------------- |
| `sleep(duration: time::Duration)` | `void`  | Pauses the running task for `duration`, rounded up to whole milliseconds; zero or a negative duration (a deadline already passed) just gives other ready tasks a turn (see [Tasks](language-ref.md#13-tasks)). |

## std/io

Writes text to the standard output (stdout) and the standard error output (stderr). The functions take a `string`: use [interpolation](language-ref.md#string-interpolation) for any other value.

| Function         | Returns | Description                             |
| :--------------- | :------ | :-------------------------------------- |
| `print(text)`    | `void`  | Writes `text` to stdout.                |
| `println(text)`  | `void`  | Writes `text` and a newline to stdout.  |
| `eprint(text)`   | `void`  | Writes `text` to stderr.                |
| `eprintln(text)` | `void`  | Writes `text` and a newline to stderr.  |

```nim
import std/io

func main() -> void
    let scores = [10, 20]
    io::println("hello")
    io::println("scores: $scores")    # scores: [10, 20]
    io::print("no newline, ")
    io::println("then one")
    io::eprintln("warning: disk almost full")
end
```

Output is buffered. On a terminal, stdout shows at the end of each line. When stdout is a pipe or a file, it's written in large blocks and flushed when the program ends, before the program waits on timers or other tasks, and before a runtime error message. stderr is written right away.

Printing never needs `try`. If the program reading the output stops early (`dusk app.dsk | head -1`), the program ends quietly, like other command-line tools; any other failure to write is a runtime error (`can't write to stdout: NoSpaceLeft`).

Interpolation formats primitives, arrays and nullables as their values. It formats enums as their number, unions as `<union>`, maps as `<map>`, functions as `<func name>` (`<func>` for a lambda), tasks as `<task>` or `<task done>`, and structs as their raw values.

#### Reading stdin

`io::stdin()` returns the program's standard input as an `io::Stdin`. Every call gives the same input, whether it's a terminal, a pipe (`cat data.txt | dusk app.dsk`) or a file (`dusk app.dsk < data.txt`). Reading waits without blocking the program: only the reading task waits, and other tasks keep running.

| Method | Returns | Description |
| :----- | :------ | :---------- |
| `read_line(max = -1, timeout_ms = -1)` | `IoError!?string` | The next line, without its `\n` or `\r\n`; the last line is returned even without a newline. `null` at the end of the input. |
| `read_bytes(max, timeout_ms = -1)` | `IoError!?buffer` | Up to `max` bytes (at least 1), as soon as some are there: it doesn't wait for `max` of them. `null` at the end of the input. |
| `read_exact(count, timeout_ms = -1)` | `IoError!?buffer` | Exactly `count` bytes (at least 1), waiting until they're all there. `null` at the end of the input; `UnexpectedEnd` when the input ends partway, and the bytes that did arrive stay for the next read. |
| `read_all(timeout_ms = -1)` | `IoError!string` | Everything until the end of the input, as text. |
| `read_all_bytes(timeout_ms = -1)` | `IoError!buffer` | Everything until the end of the input, as bytes. |
| `is_terminal()` | `bool` | Whether the input is a terminal (a person typing) rather than a pipe or a file. |
| `set_raw(is_raw)` | `IoError!void` | Turns the terminal's raw mode on or off (see [Terminal programs](#terminal-programs)). |
| `close()` | `void` | Stops reading. Reading afterwards fails with `Closed`; closing twice does nothing. |

```nim
import std/io

func main() -> !void
    let input = io::stdin()
    if input.is_terminal() then
        io::print("Name: ")
    end
    let name = try input.read_line() orelse "anonymous"
    io::println("Hello, $name")

    for true do
        let next = try input.read_line()
        if next: line then
            io::println("> $line")
        else
            break
        end
    end
end
```

- An empty line is `""`; only `null` means the input ended. The end of input is read with a capture (`if next: line then ... else break end`), since `orelse break` isn't allowed.
- Reading from stdin first sends what's waiting in stdout's buffer when the read has to wait, so a prompt printed with `io::print` shows before the user types.
- `read_line(max = n)` fails with `LineTooLong` when a line is longer than `n` bytes. The line stays unread: drain it with `read_bytes` or stop reading. Without `max`, lines can be of any length.
- `read_line` and `read_all` fail with `InvalidUtf8` on text that isn't valid UTF-8 (`read_line` skips that line). Use `read_bytes` or `read_all_bytes` for binary data.
- `timeout_ms` makes a read fail with `TimedOut` when nothing complete arrives in time. Nothing is lost: what arrived stays for the next read, and the input stays usable.
- Two tasks reading stdin at the same time: the second one fails with `Busy`. A task waiting for input when another task calls `close()` fails with `Closed`.
- When the program has no input at all (started with stdin closed), reads return the end of the input.

#### Writing through stdout and stderr

`io::stdout()` and `io::stderr()` return writers for the same output `io::print` and `io::eprint` use, in the same order. Unlike `io::print`, their methods return errors, for programs that want to handle them:

| Method | Returns | Description |
| :----- | :------ | :---------- |
| `write(text)` | `IoError!void` | Writes `text`. |
| `write_bytes(data)` | `IoError!void` | Writes the bytes of a `buffer`. |
| `flush()` | `IoError!void` | Sends what's buffered now. |
| `is_terminal()` | `bool` | Whether the output is a terminal. |
| `size()` | `?io::TerminalSize` | The terminal's size in characters (`columns`, `rows`), or `null` when the output isn't a terminal or its size is unknown. |

When the program reading the output stopped (`dusk app.dsk | head -1`), they fail with `BrokenPipe` and the program keeps running; the output that couldn't be sent is dropped. Writes follow the same buffering rules as `io::print`, so a failure can show up at the next `write` or `flush` rather than at the write that caused it.

#### Terminal programs

Programs that take over the terminal, like a text editor or a game, read every key as it's pressed and draw the whole screen themselves. `std/io` gives them what only the runtime can do; the rest (escape sequences for colors and the cursor, decoding keys) is ordinary Dusk code that writes and reads bytes.

**Raw mode.** `io::stdin().set_raw(true)` puts the terminal in raw mode: each key arrives as soon as it's pressed, without waiting for Enter, and nothing typed shows on the screen. Ctrl+C, Ctrl+Z and Ctrl+S arrive as bytes (`3`, `26`, `19`) instead of stopping or pausing the program, and Enter arrives as `\r`. Output still turns `\n` into a new line at the start of the row. `set_raw(false)` brings back the normal mode. On a pipe or a file `set_raw` fails with `NotTerminal`, and after `close()` with `Closed`.

The terminal goes back to how it was when the program ends, even if it ends with a runtime error or a task error nobody handled, and the error message shows in the normal mode. A program killed from outside (by `kill` or a crash of the runtime) can leave the terminal in raw mode; the `reset` command fixes it.

**Keys.** In raw mode, read keys with `read_bytes`: a letter is one byte, and keys like the arrows or Home come as escape sequences that start with byte `27` (Esc), such as `27 91 65` for the up arrow. To tell the Esc key from the start of a sequence, read the rest with a short timeout: when nothing follows within a few milliseconds, it was Esc (`is_escape_key` below returns `false` when more bytes came; a real key decoder keeps them).

```nim
func is_escape_key(input: io::Stdin) -> io::IoError!bool
    input.read_bytes(8, timeout_ms = 50) catch e then
        if e == io::IoError.TimedOut then
            return true
        end
        return e
    end
    return false
end
```

**Size.** `io::stdout().size()` gives the terminal's size, and the `os::Signal.WindowChange` signal (see [Signals](#signals)) tells the program when the window was resized, so it can read the size again and redraw.

**Output.** Terminal output shows at the end of each line, so call `io::stdout().flush()` after drawing a screen. On Windows, escape sequences in the output are turned on when the program starts.

```nim
import std/io
import std/os

func main() -> !void
    let input = io::stdin()
    let mut out = io::stdout()
    let resize = try os::watch_signal(os::Signal.WindowChange)
    let size = out.size() orelse io::TerminalSize(columns = 80, rows = 24)
    try input.set_raw(true)
    try out.write("\u{1b}[2J\u{1b}[H${size.columns}x${size.rows}, press q to quit\r\n")
    try out.flush()
    for true do
        let next = try input.read_bytes(16)
        if next: key then
            if key[0] == 'q' then
                break
            end
            try out.write("byte ${key[0]}\r\n")
            try out.flush()
        else
            break
        end
    end
    try input.set_raw(false)
    resize.close()
end
```

#### Errors

| `io::IoError` | When |
| :------------ | :--- |
| `Closed` | Reading after `close()`, or while another task closes the input |
| `Busy` | Another task is already reading the same input |
| `BrokenPipe` | Writing to an output whose reader is gone |
| `ConnectionReset` | The other side dropped the connection |
| `LineTooLong` | A line longer than `read_line`'s `max` |
| `InvalidUtf8` | `read_line` or `read_all` on text that isn't valid UTF-8 |
| `TimedOut` | A read with `timeout_ms` didn't complete in time |
| `UnexpectedEnd` | `read_exact` reached the end of the input before `count` bytes |
| `HandshakeFailed` | Reading or writing a TLS connection whose handshake failed |
| `NotTerminal` | `set_raw` on an input that isn't a terminal |
| `Other` | Any other failure |

#### Readers, writers and `io::copy`

Two traits describe what reads and what writes, so generic code works with any of them:

```nim
trait Reader
    func read_bytes(mut self: @, max: int, timeout_ms: int) -> IoError!?buffer
end

trait Writer
    func write(mut self: @, text: string) -> IoError!void
    func write_bytes(mut self: @, data: buffer) -> IoError!void
    func flush(mut self: @) -> IoError!void
end
```

`io::copy(from, to)` reads from a `Reader` until its end and writes everything to a `Writer`, then flushes it, and returns the number of bytes copied. `io::Stdin`, `io::Stdout`, `io::Stderr`, files (`fs::File`, see [std/fs](#files)) and network connections (`net::TcpConn`, see [std/net](#stdnet)) fit these traits, and so does any struct with the same methods, so your own types work with `io::copy` too:

```nim
import std/io
import std/buffer
import std/array: len

struct Greetings
    names: []string
    position: int = 0

    func read_bytes(mut self: @, max: int, timeout_ms: int) -> io::IoError!?buffer
        if self.position >= len(self.names) then
            return null
        end
        self.position = self.position + 1
        return buffer::from_string("hello ${self.names[self.position - 1]}\n")
    end
end

func main() -> !void
    let mut source = Greetings(names = ["ana", "bob"])
    let mut out = io::stdout()
    let copied = try io::copy(source, out)   # hello ana, hello bob
    try io::copy(io::stdin(), io::stdout())  # copies stdin to stdout
end
```

The trait methods take `mut self` because reading or writing may change the stream, so a type that keeps its position in a field fits. Methods that change nothing, like the ones of `io::Stdin`, fit too. Generic code that uses them takes the stream as `mut` (`io::copy(mut from: R, mut to: W)`), so a stream kept in a variable is passed as `let mut`; temporaries like `io::stdin()` can be passed directly. Calling a stream's own methods (`input.read_line()`) doesn't need `mut`.

## std/fs

Reads and writes files and folders. Every call waits for the disk without blocking the program: it parks only the running task, and other tasks keep running meanwhile (see [Tasks](language-ref.md#13-tasks)). Paths are relative to the current directory. Functions that can fail return an error union with the module's error set `FsError`:

| Variant | When |
| :------ | :--- |
| `NotFound` | The path, or a folder on the way to it, doesn't exist |
| `PermissionDenied` | The program isn't allowed to read, write or list it |
| `AlreadyExists` | `make_dir` on a path that already exists |
| `IsDirectory` | A file operation on a folder |
| `NotDirectory` | A folder operation on a file, or a file in the middle of a path |
| `NotEmpty` | `delete` on a folder that has contents |
| `InvalidUtf8` | `read` on a file that isn't valid UTF-8 text |
| `Other` | Any other failure (a full disk, too many open files...) |

| Function | Returns | Description |
| :------- | :------ | :---------- |
| `read(path)` | `FsError!string` | The whole file as text. |
| `read_bytes(path)` | `FsError!buffer` | The whole file as bytes, for anything that isn't text. |
| `write(path, text)` | `FsError!void` | Creates the file, or replaces its contents. |
| `write_bytes(path, data)` | `FsError!void` | Same, from a `buffer`. |
| `append(path, text)` | `FsError!void` | Adds `text` to the end of the file, creating it if needed. |
| `read_dir(path)` | `FsError![]string` | The names of the entries in a folder (not `.` and `..`), in the order the system gives them. |
| `exists(path)` | `bool` | Whether the path exists. Never fails: any problem gives `false`. |
| `stat(path)` | `FsError!FileInfo` | Information about a file or folder (below). |
| `make_dir(path, recursive = false)` | `FsError!void` | Creates a folder. With `recursive = true`, also creates missing parent folders, and a folder that already exists is fine. |
| `delete(path, recursive = false)` | `FsError!void` | Deletes a file or an empty folder. With `recursive = true`, deletes a folder with everything inside. |
| `rename(from, to)` | `FsError!void` | Renames or moves a file or folder. |

```nim
struct FileInfo
    size: int           # bytes; 0 for a folder
    is_dir: bool
    is_file: bool
    is_symlink: bool
    modified_ms: int    # milliseconds since 1970-01-01 (UTC)
    created_ms: ?int    # null when the file system doesn't record it
    permissions: int    # the permission bits: 420 is rw-r--r--
end
```

- `stat` follows symbolic links: `is_symlink` says the path itself is a link, and the other fields describe the file it points to (or the link itself, when what it points to is gone).
- `delete(path, recursive = true)` never follows symbolic links: a link inside the folder is deleted, not what it points to.
- `write`, `write_bytes` and `append` create new files with the permissions `rw-rw-rw-`, minus what the system's default mask removes (usually `rw-r--r--`).
- Each call reads or writes the whole file at once. `read_bytes` returns a new `buffer`: changing it doesn't change the file until it's written back. To read or write a file piece by piece, open it (below).

```nim
import std/io
import std/fs

func main() -> !void
    try fs::make_dir("out/logs", recursive = true)
    try fs::write("out/logs/today.txt", "started\n")
    try fs::append("out/logs/today.txt", "done\n")
    io::print(try fs::read("out/logs/today.txt"))   # started, done

    let info = try fs::stat("out/logs/today.txt")
    io::println("${info.size} bytes")                     # 13 bytes
    try fs::delete("out", recursive = true)

    let config = fs::read("config.json") catch e then
        when e
            fs::FsError.NotFound then io::println("no config, using defaults") end
            else then io::eprintln("can't read config.json") end
        end
        return
    end
    io::println(config)
end
```

#### Files

`fs::open(path, mode = fs::Mode.Read)` opens a file once and returns an `fs::File` to read or write it piece by piece: big files, a log written line by line, a file read line by line, or a file sent to a connection with `io::copy`. Opening fails with `FsError` (`NotFound`, `IsDirectory` for a folder, `PermissionDenied`...); using a `File` fails with `io::IoError`, like any stream.

| Mode | Opens for | |
| :--- | :-------- | :- |
| `fs::Mode.Read` | reading | The file must exist. |
| `fs::Mode.Write` | writing | Creates the file, or empties it. |
| `fs::Mode.Append` | writing at the end | Creates the file if needed; every write goes to the end. |

| Method | Returns | Description |
| :----- | :------ | :---------- |
| `read_line(max = -1, timeout_ms = -1)` | `IoError!?string` | The next line, without `\n` or `\r\n`; `null` at the end of the file. With `max`, a longer line fails with `LineTooLong`. |
| `read_bytes(max, timeout_ms = -1)` | `IoError!?buffer` | Up to `max` bytes; `null` at the end. |
| `read_exact(count, timeout_ms = -1)` | `IoError!?buffer` | Exactly `count` bytes, `null` when the file ends first. |
| `read_all(timeout_ms = -1)` | `IoError!string` | The rest of the file as text (`InvalidUtf8` if it isn't). |
| `read_all_bytes(timeout_ms = -1)` | `IoError!buffer` | The rest of the file as bytes. |
| `write(text)` / `write_bytes(data)` | `IoError!void` | Adds text or bytes at the current position. |
| `flush()` | `IoError!void` | Sends buffered writes to the file now and waits for them. |
| `seek(offset, from = fs::SeekFrom.Start)` | `IoError!int` | Moves to `offset` bytes from `Start`, `Current` or `End` and returns the new position. |
| `position()` | `int` | The current position, counting buffered writes. |
| `size()` | `IoError!int` | The file's size, counting buffered writes. |
| `close()` | `void` | Sends buffered writes, then closes. Closing twice does nothing. |

- **Writes are buffered.** They collect in memory and go to the file when 64 KiB have built up, on `flush()`, on `close()` and when the program ends, so writing many small pieces is cheap. Until then another reader (`fs::read`, another process) doesn't see them. A failed write shows up at the next `write`, `flush` or `seek`; `close()` can't report it, so call `flush()` first when it matters.
- **Reads** read ahead in pieces, without blocking other tasks. A second task reading the same file while one is waiting fails with `Busy`. `timeout_ms` is there so a `File` fits `io::Reader`; reading a file never times out.
- **Seeking** sends buffered writes first and forgets what was read ahead. It fails with `Busy` while another task is reading, with `Other` for a negative position, and with `Other` on a file opened with `Append` (its writes always go to the end). Seeking past the end of a file opened for writing and writing there leaves a gap of zero bytes.
- Reading a file opened for writing, or writing one opened for reading, fails with `Other`. A `File` the program no longer reaches is closed after a garbage collection finds it, after its buffered writes are sent; one still reachable at the end is closed when the program ends the same way. Close files explicitly (`defer file.close()`) when the moment matters: a collection only runs when the program allocates enough. Several tasks writing to one `File` each get their writes in whole, in call order.
- `File` fits `io::Reader` and `io::Writer`, so `io::copy(file, conn)` sends a file to a connection and `io::copy(conn, file)` saves one.

```nim
import std/fs
import std/io

func main() -> !void
    let log = try fs::open("app.log", fs::Mode.Append)
    for 1..3: i do
        try log.write("step ${i}\n")
    end
    log.close()

    let file = try fs::open("app.log")
    for true do
        let next = try file.read_line()
        if next: line then
            io::println(line)
        else
            break
        end
    end
    try file.seek(-7, fs::SeekFrom.End)
    io::print(try file.read_all())      # step 3
    file.close()
end
```

## std/net

TCP connections over the network, plain or encrypted with TLS (see [TLS](#tls) below). A server listens on an address and accepts connections; a client connects to one. Connecting, accepting, reading and writing wait without blocking the program: only the calling task waits, and other tasks keep running (see [Tasks](language-ref.md#13-tasks)).

| Function | Returns | Description |
| :------- | :------ | :---------- |
| `tcp_connect(host, port, timeout_ms = -1)` | `NetError!TcpConn` | Connects to `host` (a name like `"example.com"` or `"localhost"`, or an IP address like `"127.0.0.1"` or `"::1"`) on `port`. |
| `tcp_listen(host, port, backlog = 128)` | `NetError!TcpListener` | Listens on a local address: `"0.0.0.0"` for every IPv4 interface, `"::"` for IPv6, `"127.0.0.1"` or `"localhost"` for this machine only. Port `0` picks a free port. `backlog` is how many connections can wait for `accept`. |
| `tls_connect(host, port, timeout_ms = -1, ca = null, verify = true)` | `NetError!TcpConn` | Like `tcp_connect`, then a TLS handshake that checks the server's certificate against `host`. `timeout_ms` covers both. |
| `tls_listen(host, port, cert, backlog = 128)` | `NetError!TcpListener` | Like `tcp_listen`, but every accepted connection uses TLS with the `Certificate` `cert`. |
| `certificate(cert_pem, key_pem)` | `NetError!Certificate` | A server certificate and its private key, from PEM text. |

A name is looked up first, and each of its addresses is tried in order until one works (`"localhost"` is usually `::1` and then `127.0.0.1`); IP addresses skip the lookup. `timeout_ms` on `tcp_connect` covers the lookup and every attempt.

**`TcpListener`**

| Method | Returns | Description |
| :----- | :------ | :---------- |
| `accept(timeout_ms = -1)` | `io::IoError!TcpConn` | Waits for the next connection. |
| `port()` | `int` | The port it listens on, useful after listening on port `0`. Still returns it after `close()`. |
| `close()` | `void` | Stops listening. A task waiting in `accept` fails with `Closed`; closing twice does nothing. |

**`TcpConn`** reads like `io::Stdin` and writes like `io::stdout()`:

| Method | Returns | Description |
| :----- | :------ | :---------- |
| `read_line(max = -1, timeout_ms = -1)` | `io::IoError!?string` | The next line, without its `\n` or `\r\n`. `null` when the other side closed the connection. |
| `read_bytes(max, timeout_ms = -1)` | `io::IoError!?buffer` | Up to `max` bytes, as soon as some are there. |
| `read_exact(count, timeout_ms = -1)` | `io::IoError!?buffer` | Exactly `count` bytes. |
| `read_all(timeout_ms = -1)` | `io::IoError!string` | Everything until the other side closes the connection, as text. |
| `read_all_bytes(timeout_ms = -1)` | `io::IoError!buffer` | Same, as bytes. |
| `write(text)` | `io::IoError!void` | Sends `text`. |
| `write_bytes(data)` | `io::IoError!void` | Sends the bytes of a `buffer`. |
| `flush()` | `io::IoError!void` | Writes on a connection are never buffered, so there's nothing to send: it only fails with `Closed` after `close()`, like `write`. It's there so a connection fits `io::Writer`. |
| `set_write_timeout(timeout_ms)` | `void` | How long a write may wait for the other side to take its bytes (`-1`, the default: forever). A write only waits when the other side reads slower than it's written to; when it waits past `timeout_ms`, it fails with `TimedOut`, every other waiting write too, and the connection closes, since part of the bytes may have been sent. Each write that finishes restarts the clock, so a slow but steady reader never hits it. |
| `remote_address()` | `Address` | The other side's address, saved when the connection opened. |
| `start_tls(host, timeout_ms = -1, ca = null, verify = true)` | `NetError!void` | Switches a plain connection to TLS as a client, for protocols that start in plain text. Calling it on a connection that already uses TLS is a runtime error. |
| `accept_tls(cert, timeout_ms = -1)` | `NetError!void` | The server side of the same switch: turns an accepted plain connection into a TLS connection with `cert` (a `net::Certificate`) and waits for the handshake, for protocols with a `STARTTLS` command. Bytes the client already sent for the handshake aren't lost. Fails like a TLS listener's handshake (`HandshakeFailed`, `TimedOut`); on a connection that already uses TLS it's a runtime error. |
| `handshake(timeout_ms = -1)` | `NetError!void` | Waits for the TLS handshake to finish. Does nothing on a plain connection or one whose handshake is done. |
| `is_tls()` | `bool` | Whether the connection uses TLS. |
| `close()` | `void` | Closes the connection. On a TLS connection, it first tells the other side it's closing. |

```nim
struct Address
    host: string    # an IP address: "127.0.0.1", "::1"
    port: int
end
```

A server accepts connections in a loop and serves each one in its own task:

```nim
import std/io
import std/net

func serve(conn: net::TcpConn) -> io::IoError!void
    for true do
        let next = try conn.read_line()
        if next: line then
            try conn.write("you said: ${line}\n")
        else
            break
        end
    end
    conn.close()
end

func main() -> !void
    let listener = try net::tcp_listen("0.0.0.0", 8080)
    io::println("listening on ${listener.port()}")
    for true do
        let conn = try listener.accept()
        spawn(func() -> void
            serve(conn) catch _ then conn.close() end
        end)
    end
end
```

```nim
import std/io
import std/net

func main() -> !void
    let conn = try net::tcp_connect("localhost", 8080, timeout_ms = 5000)
    try conn.write("hello\n")
    io::println(try conn.read_line() orelse "closed")   # you said: hello
    conn.close()
end
```

- A write returns once the system has taken all the bytes. When the other side reads slowly, the writing task waits, so a fast writer can't fill memory.
- Writes from several tasks to the same connection are sent whole, one after the other, in the order they were called: they never mix.
- Reads follow the stdin rules: `null` means the other side closed the connection, `timeout_ms` loses nothing, and a second task reading the same connection at the same time fails with `Busy`. The same goes for two tasks in `accept` on one listener.
- Writing to a connection the other side has closed fails with `BrokenPipe` or `ConnectionReset`. The first write after that may still succeed, since the system only finds out on a later one.
- Closing a connection with writes still waiting (from other tasks) makes them fail with `Closed`. A connection or listener the program no longer reaches (no variable, field or copy holds it) is closed after a garbage collection finds it; one still reachable stays open until `close()` or the end of the program. Collections only run when the program allocates enough, so close connections explicitly (`defer conn.close()`); the collector is a safety net.
- `remote_address()` never fails: it keeps working after the other side leaves and after `close()`, so a server can log who it served. Each call returns a new `Address`.

#### TLS

A TLS connection is a `TcpConn` like any other: it has the same methods and rules, fits `io::Reader` and `io::Writer`, and a function that takes a `net::TcpConn` serves plain and encrypted connections alike. Encryption and decryption happen inside the runtime. TLS 1.3 is used when the other side supports it, TLS 1.2 otherwise.

```nim
import std/io
import std/net

func main() -> !void
    let conn = try net::tls_connect("example.com", 443, timeout_ms = 10000)
    try conn.write("GET / HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\n\r\n")
    io::println(try conn.read_line() orelse "no answer")   # HTTP/1.1 200 OK
    conn.close()
end
```

**Checking the server.** `tls_connect` and `start_tls` check that the server's certificate is valid now, matches `host` (a name, or an IP address when connecting by address), and was issued by a trusted certificate authority:

- By default, the trusted authorities are the system's, loaded once, the first time a connection needs them. The environment variables `SSL_CERT_FILE` (a PEM file) and `SSL_CERT_DIR` (folders of PEM files, separated by `:`, or `;` on Windows) replace the system's list when set.
- A container image without the system's certificate package (`ca-certificates` on most Linux distributions) has no trusted authorities, so every connection fails with `UntrustedCertificate`. Install the package, or point `SSL_CERT_FILE` at a bundle.
- `ca = pem` trusts only the authorities in that PEM text, for a private service or a development certificate.
- `verify = false` skips the check entirely: the connection is encrypted, but anyone in the middle can pretend to be the server. Use it only for tests and local labs.
- Revoked certificates aren't detected.

**Serving TLS.** `net::certificate(cert_pem, key_pem)` reads a certificate and its private key once, failing with `InvalidCertificate` when either isn't valid PEM or the key doesn't belong to the certificate. `cert_pem` can hold the whole chain (the server's certificate followed by the intermediate ones, like a `fullchain.pem`), and all of it is sent to clients. Keys can be RSA or ECDSA (P-256, P-384); password-protected keys and Ed25519 certificates aren't supported. One `Certificate` can serve several listeners.

```nim
import std/fs
import std/net

func main() -> !void
    let cert = try net::certificate(try fs::read("fullchain.pem"), try fs::read("privkey.pem"))
    let listener = try net::tls_listen("0.0.0.0", 8443, cert)
    for true do
        let conn = try listener.accept()
        spawn(func() -> void
            serve(conn) catch _ then conn.close() end
        end)
    end
end
```

`serve` is the same function as in the plain server above.

**When the handshake happens.** `tls_connect` and `start_tls` return once it's done. On the server, `accept` returns right away, and the handshake runs in the connection's own task on its first read or write, so a slow or hostile client can't hold up the others. A server that writes first (a greeting) waits for the handshake before its bytes go out. Reads and writes on a connection whose handshake failed fail with `io::IoError.HandshakeFailed`. `conn.handshake()` waits for it explicitly and reports why it failed as a `NetError`; with `timeout_ms`, a handshake that takes too long fails with `TimedOut`, and the connection can't be used anymore.

#### Errors

Opening (`tcp_connect`, `tcp_listen`, `tls_connect`, `tls_listen`, `certificate`) and the handshake calls (`start_tls`, `handshake`) fail with `NetError`; using a listener or a connection fails with `io::IoError` (see [std/io](#stdio)), so generic code like `io::copy` has one error type.

| `net::NetError` | When |
| :-------------- | :--- |
| `HostNotFound` | The name doesn't exist, or it can't be looked up (no network) |
| `ConnectionRefused` | Nothing listens on that port |
| `AddressInUse` | `tcp_listen` on an address and port something already listens on |
| `NetworkUnreachable` | No route to the address |
| `PermissionDenied` | The system doesn't allow it (a port below 1024 without the rights, for example) |
| `InvalidAddress` | An empty host, a port outside `0..65535`, or an address that isn't on this machine (`tcp_listen`) |
| `TimedOut` | `tcp_connect`, `tls_connect`, `start_tls` or `handshake` didn't finish within `timeout_ms` |
| `HandshakeFailed` | The TLS handshake failed for another reason (the other side doesn't speak TLS, has no protocol in common, or closed the connection) |
| `UntrustedCertificate` | The server's certificate wasn't issued by a trusted authority (self-signed, an unknown authority, or no trusted authorities at all) |
| `ExpiredCertificate` | The server's certificate is expired, or not valid yet |
| `WrongHostCertificate` | The server's certificate is for another name |
| `InvalidCertificate` | `certificate` got text that isn't a valid PEM certificate or key, or a key that doesn't match; or `ca` isn't valid PEM |
| `Other` | Any other failure |

## std/url

Reads and builds URLs.

| Function | Returns | Description |
| :------- | :------ | :---------- |
| `parse(text)` | `UrlError!Url` | Reads an absolute URL like `https://example.com:8443/docs?page=2#intro`. |
| `resolve(base, reference)` | `UrlError!Url` | The URL that `reference` points to when read from `base`: a full URL, `//host/path`, `/path`, `path`, `../path`, `?query` or `#fragment`. `.` and `..` segments are removed. |
| `encode(text)` | `string` | Percent-encodes every byte except letters, digits and `- . _ ~`, for one path segment or one query value: `url::encode("a b/c")` is `a%20b%2Fc`. |
| `decode(text)` | `UrlError!string` | Reverses `encode`. `+` stays `+`. |
| `query(params)` | `string` | Builds a query string from a `map[string, string]`, in the map's order, encoding keys and values: `url::query({"q": "dusk lang", "page": "2"})` is `q=dusk%20lang&page=2`. |
| `parse_query(text)` | `UrlError!map[string, []string]` | Reads a query string or a form body (`a=1&tag=x&tag=y`): every value of each key, in order, decoded, with `+` read as a space. |

```nim
struct Url
    scheme: string      # "https", lowercase
    host: string        # "example.com", lowercase; "::1" for http://[::1]/
    port: ?int          # null when the URL has none
    path: string        # "/docs", or "/" when the URL has no path
    query: ?string      # "page=2", without the "?"; "" for a "?" with nothing after it
    fragment: ?string   # "intro", without the "#"

    func to_string(self: @) -> string
end
```

```nim
import std/io
import std/url

func main() -> !void
    let page = try url::parse("https://example.com/docs/guide/intro?lang=en")
    io::println(page.host)                                      # example.com
    let next = try url::resolve(page, "../api/list?page=2")
    io::println(next.to_string())                               # https://example.com/docs/api/list?page=2
    io::println("/search?${url::query({"q": "dusk lang"})}")    # /search?q=dusk%20lang
end
```

- `parse` only takes absolute URLs with `scheme://`. The path, query and fragment are kept as written, already encoded: they may hold printable ASCII characters other than space, `"`, `<`, `>`, `\`, `^`, the backquote, `{`, `|` and `}`, and `%` must be followed by two hex digits. Text with spaces or non-ASCII characters has to be encoded first (`url::encode`).
- A user name and password in the URL (`https://user:secret@host/`) aren't supported: `parse` fails with `Invalid`.
- Ports go from `1` to `65535`.

| `url::UrlError` | When |
| :-------------- | :--- |
| `Invalid` | Not an absolute URL, an empty or invalid host, a user name and password, or a character that must be encoded |
| `InvalidPort` | A port that isn't a number from `1` to `65535` |
| `InvalidEncoding` | `decode` got a `%` without two hex digits after it, or bytes that aren't valid UTF-8 |

## std/http

HTTP/1.1 clients and servers. `http::request` sends a request and reads the whole response; `http::open` sends one and returns as soon as the response's head arrives, so its body can be read piece by piece. Both take a `http://` or `https://` URL, follow redirects, and reuse connections. `http::listen` and `server.serve` answer requests (see Serving HTTP below). Like every network call, they only make the calling task wait (see [Tasks](language-ref.md#13-tasks)).

```nim
import std/http
import std/io

func main() -> !void
    let resp = try http::request("https://api.example.com/users/1", timeout_ms = 5000)
    if !resp.is_success() then
        io::println("failed: ${resp.status}")
        return
    end
    let user = try resp.text()

    let created = try http::request("https://api.example.com/users", method = "POST",
        body = user,
        headers = {"Content-Type": "application/json", "Authorization": "Bearer secret"})
    io::println("${created.status} ${created.headers.get("location") orelse "-"}")
end
```

| Function | Returns |
| :------- | :------ |
| `request(url, method = "GET", headers = {}, body = null, body_bytes = null, query = {}, timeout_ms = -1, max_redirects = 10, max_body = 67108864, ca = null, verify = true, body_stream = null, body_length = -1)` | `HttpError!Response` |
| `open(url, method = "GET", headers = {}, body = null, body_bytes = null, query = {}, timeout_ms = -1, max_redirects = 10, ca = null, verify = true, body_stream = null, body_length = -1)` | `HttpError!ResponseStream` |

| Parameter | Description |
| :-------- | :---------- |
| `url` | An absolute `http://` or `https://` URL. Spaces, non-ASCII characters and other characters a URL can't hold (`"`, `<`, `>`, `{`...) in the path, query and fragment are percent-encoded for you (`/my files/café` is sent as `/my%20files/caf%C3%A9`); `%XX` already there is kept. Credentials in it (`https://user:pass@host/`, percent-encoded) are sent as `Authorization: Basic ...` unless `headers` has an `Authorization`, and dropped when a redirect goes to another host, like that header. |
| `method` | Any HTTP method, in uppercase: `"GET"`, `"POST"`, `"PUT"`, `"PATCH"`, `"DELETE"`, `"HEAD"`, `"OPTIONS"`... Lowercase letters, spaces and other characters fail with `InvalidMethod`. |
| `headers` | Request headers, as a `map[string, string]`. Names and values are checked before anything is sent: a name with spaces or other characters a header name can't have, or a value with a line break or another control character, fails with `InvalidHeader`. |
| `body` | Text to send. |
| `body_bytes` | Bytes to send. |
| `body_stream`, `body_length` | A body written piece by piece, for uploads too big to hold in memory or made as they go: a `func(mut http::BodyWriter) -> io::IoError!void`, called after the head is sent, with the same writer a server's `http::stream` gets (`write`, `write_bytes`, `flush`, fits `io::Writer`). With `body_length = -1` the body is sent chunked; with a length it's sent with `Content-Length`, and writing more or less fails the request with `Other`. An error the function returns fails the request too (mapped like a connection error). Giving more than one of `body`, `body_bytes` and `body_stream` is a runtime error. |
| `query` | Entries to add to the URL's query, encoded with `url::encode`: `query = {"q": "dusk lang"}` on `https://example.com/search?lang=en` asks for `/search?lang=en&q=dusk%20lang`. |
| `timeout_ms` | The time the whole call may take: looking up the name, connecting, the TLS handshake, sending the request, every redirect, and for `request`, reading the whole body. `-1` waits forever. |
| `max_redirects` | How many redirects to follow (see below). |
| `max_body` | The largest body `request` reads, in bytes, after decompression (64 MiB by default). A larger one fails with `BodyTooLarge`. |
| `ca`, `verify` | How `https` servers are checked, as for `net::tls_connect` (see [TLS](#tls)): `ca` trusts only the authorities in that PEM text, `verify = false` skips the check. |

The request gets `Host`, `User-Agent: dusk/0.16` and `Content-Length` (when there's a body, and `0` for a `POST`, `PUT` or `PATCH` without one) unless `headers` has them, plus `Accept-Encoding: gzip, deflate` (see Compression). A `Content-Length` in `headers` is replaced by the real length, and `Transfer-Encoding` isn't allowed (`InvalidHeader`). The URL's `#fragment` is never sent. `Content-Type` is never guessed: set it for the body you send.

**`Response`**

| Field or method | Type | Description |
| :-------------- | :--- | :---------- |
| `status` | `int` | The status code: `200`, `404`... |
| `reason` | `string` | The text after the code: `"OK"`, `"Not Found"`. |
| `headers` | `Headers` | The response headers. |
| `body` | `buffer` | The body's bytes (decompressed, see Compression). Empty for `HEAD` requests and for `204` and `304` responses. |
| `url` | `string` | The URL the response came from, after redirects. |
| `text()` | `HttpError!string` | The body as text; fails with `InvalidUtf8` when it isn't valid UTF-8. |
| `is_success()` | `bool` | Whether `status` is from `200` to `299`. |

A status code is never an error: a `404` or a `500` is a normal `Response`. Calls fail only when the request can't be made or the response is broken.

**`Headers`** keeps the response headers in the order they came, with their names as sent. Lookups ignore case.

| Method | Returns | Description |
| :----- | :------ | :---------- |
| `get(name)` | `?string` | The first value of that header, or `null`. |
| `get_all(name)` | `[]string` | Every value of that header, in order (`Set-Cookie` can come several times). |
| `has(name)` | `bool` | Whether the header is there. |
| `entries()` | `[]Header` | Every header, as `Header(name, value)`, in order. |
| `set(name, value)`, `add(name, value)`, `remove(name)` | `void` | Change the headers (see Building responses). |

**`ResponseStream`**, from `http::open`, has `status`, `reason`, `headers` and `url` like a `Response`, and reads its body like a `TcpConn`. Each read takes its own `timeout_ms`, and reading fails with `io::IoError` like any connection, so a `ResponseStream` fits `io::Reader` and works with `io::copy`.

| Method | Returns | Description |
| :----- | :------ | :---------- |
| `read_bytes(max, timeout_ms = -1)` | `io::IoError!?buffer` | Up to `max` bytes of the body; `null` once the body is over. |
| `read_line(max = -1, timeout_ms = -1)` | `io::IoError!?string` | The next line of the body, without its `\n` or `\r\n`; `null` once the body is over. |
| `read_all(timeout_ms = -1)` | `io::IoError!string` | The rest of the body, as text. |
| `read_all_bytes(timeout_ms = -1)` | `io::IoError!buffer` | The rest of the body, as bytes. |
| `close()` | `void` | Done with the response. Reads after it fail with `Closed`. |

```nim
import std/http
import std/io

func main() -> !void
    let mut events = try http::open("https://example.com/log", timeout_ms = 5000)
    for true do
        let next = try events.read_line(timeout_ms = 30000)
        if next: line then
            io::println(line)
        else
            break
        end
    end
    events.close()
end
```

A body that ends before its announced length, or that breaks the chunked format, fails with `UnexpectedEnd` or `Other`. `http::open` doesn't ask for compression, so the body comes as the server sent it.

**Redirects.** A `301`, `302`, `303`, `307` or `308` response with a `Location` header is followed, up to `max_redirects` times; one more fails with `TooManyRedirects`. With `max_redirects = 0`, the redirect itself is returned. A `Location` relative to the current URL works, and a redirect to a URL that isn't `http` or `https` fails with `UnsupportedScheme`.

- A `303`, and a `301` or `302` after any method but `GET` or `HEAD`, is followed with a `GET` without the body (and without `Content-Type`). A `307` or `308` sends the same method and body again.
- When a redirect goes to another scheme, host or port, `Authorization`, `Cookie` and a given `Host` header are left out, so credentials meant for one server don't reach another.
- `response.url` is where the final response came from.

**Connections.** After a response, its connection is kept open for the next request to the same scheme, host and port (and, for `https`, the same `ca` and `verify`), which skips connecting and the TLS handshake. Up to 8 idle connections are kept per server. An idle connection closes after 30 seconds, or as soon as the server closes it, and never keeps the program running. A `ResponseStream` gives its connection back on `close()` when the whole response has arrived; otherwise, the connection is closed. A `ResponseStream` dropped without `close()` has its connection closed after a garbage collection finds it. If a kept connection turns out to be closed when it's reused, before any answer arrives, a `GET`, `HEAD`, `PUT`, `DELETE` or `OPTIONS` request is sent again on a new connection; a `POST` or `PATCH` fails with `ConnectionReset` instead, since the server may already have acted on it.

**Compression.** `request` and `open` ask for compressed bodies (`Accept-Encoding: gzip, deflate`) and decompress them, so `body`, `text()` and a `ResponseStream`'s reads give the original content; `max_body` counts the decompressed bytes. `deflate` bodies are accepted with or without their zlib header, since some servers leave it out. The `Content-Encoding` and `Content-Length` headers stay as the server sent them. Setting `Accept-Encoding` in `headers` turns this off: the body is returned as it came. A `ResponseStream` read fails with `io::IoError.Other` for a broken compressed body and `UnexpectedEnd` for one cut short.

**Streamed uploads.** `io::copy` sends a file or any other `io::Reader` as the body:

```nim
let mut file = try fs::open("backup.tar")
let resp = try http::request("https://example.com/backups", method = "PUT", body_length = try file.size(),
    body_stream = func(mut out: http::BodyWriter) -> io::IoError!void
        let sent = try io::copy(file, out)
    end)
file.close()
```

A request with a `body_stream`, or a body of 1 MiB or more, asks the server first with `Expect: 100-continue` and waits up to a second for its go-ahead (servers that never answer it get the body after that second). A server that answers right away with a final status (`401`, `413`...) gets no body: the function isn't called, the response is returned as usual and the connection isn't reused. Setting `Expect` in `headers` turns this off. A streamed body can't be sent twice, so a `307` or `308` redirect is returned instead of followed, and a stale kept connection isn't retried.

**Clients.** An `http::Client` keeps settings for many requests to the same service: a base URL, headers sent with every request, and the defaults for the other settings. Build one with the fields you need; the others keep the defaults of `http::request`.

```nim
let mut api = http::Client(base_url = "https://api.example.com/v1", headers = {"Authorization": "Bearer $token"}, timeout_ms = 5000)
let user = try api.request("/users/7")
let created = try api.request("/users", method = "POST", body = json, headers = {"Content-Type": http::ContentType.JSON})
let mut export = try api.open("/export.csv")
api.headers["X-Trace"] = trace_id
```

| Field | Default | Description |
| :---- | :------ | :---------- |
| `base_url` | `""` | Put in front of relative URLs, with exactly one `/` between: `.../v1` and `/users`, or `.../v1/` and `users`, both give `.../v1/users`. A URL with a scheme (`https://...`) ignores it. |
| `headers` | `{}` | Sent with every request. A header given to a call replaces the client's header of the same name, ignoring case. |
| `timeout_ms`, `max_redirects`, `max_body`, `ca`, `verify` | as in `request` | The defaults for each call. |
| `proxy` | `null` | A proxy every request goes through (see Proxies); `null` or `""` connects directly. |
| `max_connections_per_host` | `-1` | At most this many connections in use at once to one server (`scheme://host:port`); `-1` means no limit. |

**Proxies.** With `proxy = "http://proxy.example.com:3128"`, requests go through that proxy: an `http://` URL is sent to the proxy whole (`GET http://host/path`), and an `https://` one through a tunnel (`CONNECT host:443`) inside which the TLS connection to the server runs as usual, checked against `ca` / `verify`. The proxy can be `https://` too, so the connection to the proxy is encrypted as well (its certificate is checked with the same `ca` / `verify`); `https://` servers are then reached with TLS inside that TLS connection. Credentials go in the proxy URL (`http://user:pass@proxy:3128`, percent-encoded) and are sent as `Proxy-Authorization: Basic ...`, never to the server. Requests to `localhost`, `*.localhost`, `127.x.x.x` and `::1` always connect directly. A proxy that refuses a tunnel (a `407` for missing credentials, a `403`...) fails the request with `ProxyRefused`; a proxy URL that can't be parsed is `InvalidUrl`, and one that isn't `http://` or `https://` (SOCKS) is `UnsupportedScheme`. The environment isn't read on its own: pass the value of `HTTPS_PROXY` or similar yourself if you want it.

```nim
let corporate = http::Client(proxy = "http://alice:s3cret@proxy.internal:3128")
let page = try corporate.request("https://example.com/")
```

**Connection limit.** Firing many requests at one server at once, from many tasks, opens one connection per request. `max_connections_per_host = 16` caps the connections in use to that server: the other requests wait in line, first come first served, and take a connection as soon as one is free (a request waiting past its `timeout_ms` fails with `TimedOut`). The count covers every request in the program to that server, whichever client sent it, and a `ResponseStream` holds its connection until `close()`. Idle kept-alive connections don't count.

`client.request(url, ...)` and `client.open(url, ...)` take the same arguments as `http::request` and `http::open`; a setting given to the call (`timeout_ms = 20000`) overrides the client's for that call only. Changing a field of a `let mut` client affects the requests after it. Every client shares the program's pool of open connections, so two clients talking to the same server reuse each other's connections.

**Serving HTTP.** `http::listen` opens a server and `server.serve(handler)` answers its requests with a handler function, a `func(http::Request) -> E!http::Response`. Each connection runs in its own task, so a slow handler only holds up its own client.

```nim
import std/http
import std/io

func main() -> !void
    let server = try http::listen("0.0.0.0", 8080)
    io::println("listening on ${server.port()}")
    server.serve(func(req: http::Request) -> !http::Response
        when req.path
            "/" then return http::text("hello ${req.query_param("name") orelse "world"}") end
            "/users" then
                if req.method != "POST" then
                    return http::status(405)
                end
                return http::json_text(try req.text(), status = 201)
            end
            else then return http::status(404) end
        end
    end)
end
```

| Function | Returns | Description |
| :------- | :------ | :---------- |
| `listen(host, port, cert = null, max_body = 1048576, head_timeout_ms = 10000, idle_timeout_ms = 30000, backlog = 1024, max_connections = -1, write_timeout_ms = 30000, compress = true, compression_level = 6)` | `HttpError!Server` | Listens on `host` and `port` like `net::tcp_listen` (port `0` picks a free one). With a `net::Certificate` as `cert`, it serves `https`. With `max_connections` above 0, at most that many connections are open at once: the server stops accepting at the limit, so new clients wait in the system's queue (`backlog`) until a connection closes. `compress` and `compression_level` control response compression (see below); a level outside `0..9` is a runtime error. |

**`Server`**

| Method | Returns | Description |
| :----- | :------ | :---------- |
| `serve(handler)` | `void` | Accepts connections until `close()`, calls `handler` for each request, and returns once every connection is done. |
| `port()` | `int` | The port it listens on. |
| `close()` | `void` | Stops the server (see below). |

`close()` stops accepting and closes the connections that are waiting for their next request; requests already being handled finish, and their responses close the connection. Calling it again does nothing.

**`Request`**

| Field or method | Type | Description |
| :-------------- | :--- | :---------- |
| `method` | `string` | `"GET"`, `"POST"`... |
| `target` | `string` | The path and query as sent: `"/users/7?full=1"`. |
| `path` | `string` | The path, percent-decoded: `"/users/7"`. |
| `query` | `?string` | The query as sent, without the `?`: `"full=1"`; `null` when there's none. |
| `headers` | `Headers` | The request headers. |
| `remote_address` | `net::Address` | The client's address. |
| `query_param(name)` | `?string` | The first value of a query parameter, decoded, with `+` read as a space. |
| `text(timeout_ms = -1)` | `HttpError!string` | The body as text (`InvalidUtf8` when it isn't valid UTF-8). |
| `bytes(timeout_ms = -1)` | `HttpError!buffer` | The body as bytes. |
| `read_bytes(max, timeout_ms = -1)` | `io::IoError!?buffer` | The next piece of the body, up to `max` bytes; `null` at its end. For bodies too big to hold at once: it isn't limited by `max_body`. |

The body is read only when the handler asks for it. `text` and `bytes` fail with `BodyTooLarge` past `max_body` bytes, and with `InvalidRequest` when the client sends a broken body.

**Building responses.** A handler returns a `Response`, the same type `http::request` returns, usually made with one of these:

| Function | Response |
| :------- | :------- |
| `text(body, status = 200)` | `body` with `Content-Type: text/plain; charset=utf-8` |
| `json_text(body, status = 200)` | `body` with `Content-Type: application/json` |
| `html(body, status = 200)` | `body` with `Content-Type: text/html; charset=utf-8` |
| `bytes(data, content_type = ContentType.BINARY, status = 200)` | the bytes of a `buffer` |
| `status(code)` | an empty body |
| `redirect(location, status = 302)` | an empty body with `Location` |
| `stream(write, content_type = ContentType.BINARY, status = 200, length = -1)` | a body written by `write` after the head is sent (see Streamed responses) |
| `file(path, content_type, status = 200)` | the file's contents, sent in pieces with its size as `Content-Length`; fails with `fs::FsError` when the file can't be opened |

**Content types.** `content_type` is a string, so any type works (`"application/vnd.api+json"`). `http::ContentType` names the common ones: `HTML`, `CSS`, `JS`, `TEXT`, `CSV` (all with `; charset=utf-8`), `JSON`, `XML`, `SVG`, `PNG`, `JPEG`, `GIF`, `WEBP`, `ICO`, `WASM`, `PDF`, `EVENT_STREAM`, `FORM` and `BINARY` (`application/octet-stream`). `http::content_type_of(path)` picks one from a file name's extension, ignoring case: those types plus `.htm`, `.mjs`, `.map`, `.md`, `.avif`, fonts (`.woff`, `.woff2`, `.ttf`, `.otf`), `.mp4`, `.webm`, `.mp3`, `.ogg`, `.wav`, `.zip` and `.gz`. Anything else, including names without an extension and dotfiles like `.env`, gives `BINARY`.

```nim
"/logo" then return try http::file("assets/logo.svg", http::ContentType.SVG) end
else then
    let path = "public${req.path}"
    return http::file(path, http::content_type_of(path)) catch e then
        return http::status(404)
    end
end
```

A static file server like this must check `req.path` first: a path with `..` reaches files outside `public`.

Headers change through `Headers` methods on a `let mut` response: `set(name, value)` replaces every value of that header, `add(name, value)` adds one more, and `remove(name)` drops them all.

```nim
let mut response = http::text("created", status = 201)
response.headers.set("Location", "/users/7")
response.headers.add("Set-Cookie", "session=abc; HttpOnly")
return response
```

A `reason` left empty is filled from the status code (`"Not Found"`). The server adds `Date` (unless set), `Content-Length` (except for `1xx`, `204` and `304`) and `Connection: close` when the connection is closing; `Content-Length`, `Transfer-Encoding`, `Connection` and `Keep-Alive` set by the handler are replaced. A `Response` from `http::request` can be returned as it is (a proxy), but its body is already decompressed: remove its `Content-Encoding` header first.

**Streamed responses.** `stream` takes a `func(mut http::BodyWriter) -> io::IoError!void`. The server sends the head, then calls it with a writer for the body, so the body doesn't have to fit in memory and can be sent as it's produced. `BodyWriter` has `write(text)`, `write_bytes(data)` and `flush()`, so it fits `io::Writer`. Writes are collected and sent in pieces of up to 16 KiB, when the writer fills up, on `flush()` and at the end; a slow client makes the writes wait instead of filling memory.

- With `length = -1`, the body is sent with `Transfer-Encoding: chunked`. With a length, it's sent with `Content-Length`: a write past it fails with `io::IoError.Other`, and a function that writes less closes the connection.
- Writes fail with `io::IoError.Closed` when the client is gone, once `server.close()` was called, and for a `HEAD` request (only the head is sent, but the function still runs so it can release what it holds). Return the error and the connection closes.
- The head is already sent when the function runs, so a failure can't become a `500`: the connection closes and the client sees a body cut short. Any error other than `Closed` is also written to stderr: `http: GET /path failed while streaming: <error>`.

```nim
"/events" then
    return http::stream(func(mut events: http::BodyWriter) -> io::IoError!void
        let mut n = 0
        for true do
            try events.write("data: tick $n\n\n")
            try events.flush()
            n = n + 1
            task::sleep(time::milliseconds(1000))
        end
    end, content_type = "text/event-stream")
end
"/report" then
    return http::file("data/report.csv", "text/csv") catch e then
        return http::status(404)
    end
end
```

Server-sent events are a stream with `content_type = "text/event-stream"` and a `flush()` after each event. An endless stream ends when the client leaves or the server closes.

**What the server does on its own:**

- An error that escapes the handler becomes a `500` with an empty body, and a line on stderr: `http: GET /path failed: <error>`. A `BodyTooLarge` that escapes becomes a `413`, and a broken body a `400`. A response header that can't be sent (a line break in a value, a bad name) also becomes a `500`.
- A request that isn't valid HTTP/1.1 (bad framing, `Content-Length` together with `Transfer-Encoding`, bad characters, an unknown method, a path whose `%` escapes don't decode) gets `400`, a head over 64 KiB gets `431`, and the handler isn't called. The connection then closes.
- A client that stops reading a response (a big body, a stream) for `write_timeout_ms` is disconnected, like `TcpConn.set_write_timeout`: a streamed response's next write fails with `TimedOut`. A client that reads slowly but steadily isn't.
- A client has `head_timeout_ms` to start sending a request on a new connection and `head_timeout_ms` to finish its head (`408` otherwise). Between requests on a kept connection, it may stay quiet for `idle_timeout_ms` before the server closes it.
- Connections are kept for the next request unless the client asks to close (`Connection: close`, or HTTP/1.0 without `keep-alive`) or the handler sets `Connection: close`. Requests on one connection are answered in order.
- A body the handler didn't read is read and dropped after the response if it's at most 64 KiB; a bigger one closes the connection.
- `Expect: 100-continue`: the client gets `100 Continue` when the handler first reads the body. If it never does, the response goes out and the connection closes, so the client never sends a body nobody wants.
- A `HEAD` request runs the handler like a `GET` (`req.method` is `"HEAD"`); the body isn't sent, and `Content-Length` keeps its size.

**Compression.** Responses are gzip-compressed (`Content-Encoding: gzip`) when all of these hold, so text crosses the network at a fraction of its size:

- the request's `Accept-Encoding` accepts `gzip` (`gzip;q=0` refuses it, `*` accepts it, no header means no compression);
- the response has a body (not `1xx`, `204` or `304`) and the handler didn't set `Content-Encoding` itself (a body that's already compressed, or `identity` to opt one response out);
- its `Content-Type` is text: `text/*`, `application/json`, `application/javascript`, `application/xml`, `application/wasm`, or a type ending in `+json` or `+xml` (`image/svg+xml`);
- a whole body is at least 1 KiB and actually gets smaller; a stream is compressed unless its `length` is under 1 KiB.

Every response that could be compressed gets `Vary: Accept-Encoding` (added to a `Vary` the handler set), so caches keep both versions. A compressed whole body gets the compressed `Content-Length`, also for `HEAD`. A compressed stream (`http::stream`, `http::file`) is sent chunked even when it has a `length`, since the compressed size isn't known in advance; `flush()` also flushes the compressor, so server-sent events still arrive one by one. `compression_level` goes from `0` to `9` like in `std/compression`: `6` (the default) is a good balance, `1` saves CPU on busy servers. `compress = false` turns it all off.
- With `cert`, each connection's TLS handshake runs in its own task; a client that fails it is just disconnected.

| `http::HttpError` | When |
| :---------------- | :--- |
| `InvalidUrl` | The URL isn't valid (see [std/url](#stdurl)) |
| `UnsupportedScheme` | The URL, or a redirect, isn't `http` or `https` |
| `InvalidMethod` | `method` isn't an uppercase HTTP method |
| `InvalidHeader` | A header name or value that can't be sent, or a `Transfer-Encoding` header |
| `HostNotFound`, `ConnectionRefused`, `NetworkUnreachable` | As in `net::NetError` |
| `TimedOut` | The call didn't finish within `timeout_ms` |
| `HandshakeFailed`, `UntrustedCertificate`, `ExpiredCertificate`, `WrongHostCertificate`, `InvalidCertificate` | As in `net::NetError`, for `https` |
| `ConnectionReset` | The server closed or reset the connection before answering |
| `InvalidResponse` | The server's answer isn't valid HTTP, ends too early, or has a broken compressed body |
| `HeadersTooLarge` | The response head is over 64 KiB |
| `BodyTooLarge` | The body is over `max_body` |
| `TooManyRedirects` | More than `max_redirects` redirects |
| `InvalidUtf8` | `text()` on a body that isn't valid UTF-8 |
| `Closed` | The connection was closed |
| `InvalidRequest` | A request body the client sent broken (bad chunks, or shorter than announced), read with `text` or `bytes` |
| `AddressInUse`, `PermissionDenied` | `listen` failed, as in `net::NetError` |
| `ProxyRefused` | The client's proxy refused to open a tunnel (any status other than `2xx` to `CONNECT`) |
| `Other` | Any other failure |

## std/serialization

Turns values into an outside format and back, through code the compiler writes for each type. A format (JSON, rows from a database, a config file syntax) is an ordinary struct with methods; `std/json` is one, and any module can add its own. Import with `import std/serialization`.

```nim
let mut writer = QueryWriter()
try serialization::encode(user, writer)
let mut reader = RowReader(row)
let user: User = try serialization::decode(reader)
```

| Function | Returns | Description |
| :------- | :------ | :---------- |
| `encode[T, W, S: schema](value, mut writer)` | `SerializationError!void` | Writes `value` through `writer`. |
| `decode[T, R, S: schema](mut reader)` | `SerializationError!T` | Reads a `T` from `reader`. |
| `decode_document[T, R, S: schema](mut reader)` | `SerializationError!T` | Like `decode`, then calls `reader.end_document()` before any default runs, so a format can reject trailing input first. |
| `plan_of[T, S: schema]()` | `int` | The id of `T`'s plan, the description formats read field and variant names from. Known when compiling. |
| `has_proxies[T, S: schema]()` | `bool` | Whether `T`'s plan uses `via` anywhere. Known when compiling: an `if` on it only compiles the branch taken. |
| `is_proxied[T, S: schema]()` | `bool` | Whether `T` itself (or the type inside a nullable `T`) is written through a whole-type `via`. Known when compiling, like `has_proxies`. |

For each type, the compiler writes one decode and one encode function that read and fill fields directly, and checks at compile time that the format has every method the type needs. Types must have public instance fields only; functions, tasks, buffers, error values and maps with keys other than `string` can't be written, anywhere inside the type (`InvalidType`, naming the path, such as `Hidden at 'inner[].value'`). Enums are written by variant name; aliases decode by their own names and encode as the first variant with that value.

**Defaults.** A field missing from the input takes its declared default. Defaults made of literals (`0`, `""`, `[]`, `null`) are set right away; defaults that call functions run after the whole input has been read, children before their parents, so they never run for input that turns out to be invalid. Inside a value read through a proxy, they run before its `from` sees it. They can allocate and make the task wait. A field without a default is required, even when it's nullable (`MissingField`). A field given twice is `DuplicateField`.

Values nested deeper than 512 levels fail with `DepthExceeded`, when reading or writing. Writing a value that contains itself also ends in `DepthExceeded`.

`SerializationError` contains `InvalidInput`, `TypeMismatch`, `MissingField`, `UnknownField`, `DuplicateField`, `OutOfRange`, `InvalidValue`, `UnknownVariant`, `DepthExceeded` and `CycleDetected`. Formats map their own failures to these and can keep details (a position, a message) in their own fields.

#### Schemas

A schema is a named set of outside names for one consumer: an API, a database table, a config file. The same type can have several. Without a schema, fields and variants use their declared names.

```nim
schema Api(case = "snake")
    time::Timestamp: via(time::to_utc_iso_string, time::from_iso_string)

    struct models::Address
        zipCode: "postcode"
    end
end

schema Db end

struct User
    userId: int
    displayName: string
    password: string = ""
    nickname: ?string = null
    createdAt: time::Timestamp

    schema Api
        userId: "user_id", "legacy_id"
        password: skip
        nickname: omit_null
    end

    schema Db
        userId: "id"
    end
end

let text = try json::encode[User, Api](user)
let row = try serialization::decode[User, RowReader, Db](reader)
let plain = try json::encode(user)
```

- `schema Name(case = "...") ... end` declares a schema at the top of a file. `case` is `"snake"` (`user_id`), `"camel"` (`userId`), `"kebab"` (`user-id`), `"pascal"` (`UserId`) or `"exact"`, the default. An empty schema is `schema Db end`.
- Inside a struct, enum or union, `schema Name ... end` (after the fields or variants, before the methods) sets that type's names for that schema.
- Inside a schema, a `struct`, `enum` or `union` block does the same for a type declared elsewhere, such as in another module. The keyword must match the type. A generic type is written without its slots, and the rules apply to every version of it.
- A member can have rules in only one place for a given schema. Schemas reach public fields only.
- Nested values follow the same schema: encoding a `User` with `Api` uses `Api`'s names for the `Address` inside it.

| Rule | Meaning |
| :--- | :------ |
| `userId: "user_id"` | The outside name. |
| `userId: "user_id", "legacy_id"` | The first name is written; every name is accepted when reading. Getting two of them is a duplicate. |
| `password: skip` | Never read or written; the field takes its default, so it must have one. |
| `nickname: omit_null` | Not written when `null`. Only for nullable fields. |
| `createdAt: "created", via(to, from)` | The field is written as another type (see Proxies). Rules combine with commas. |
| `OnHold: "on-hold"` | A variant's outside name, in an enum or union. Variants only take names. |
| `time::Timestamp: via(to, from)` | Directly inside a schema: every value of that type is written as another type. |

Names are worked out when compiling, never by changing incoming text. `case` applies first, then explicit names. It splits a name at `_`, `-`, a capital after a lowercase letter or digit, and the last capital of a run of capitals before a lowercase letter: `userID` becomes `user_id`, `HTTPServer` becomes `http_server`, `line2` stays one word. Letters outside ASCII don't change case. Two members ending up with the same name, an unknown member, `skip` without a default and `omit_null` on a field that can't be null are compile errors (`InvalidSchema`).

#### Proxies

`via(to, from)` writes a value as another type through two functions: `to` takes the value and returns the other type, and `from` takes it back, returning the value or failing. The other type can be anything serializable, including a struct with its own schema sections. A `from` that fails makes reading fail with `InvalidValue`.

```nim
func ms_to_seconds(milliseconds: int) -> int
    return milliseconds // 1000
end

func seconds_to_ms(seconds: int) -> int
    return seconds * 1000
end

struct Job
    timeoutMs: int

    schema Api
        timeoutMs: "timeout_s", via(ms_to_seconds, seconds_to_ms)
    end
end
```

Proxies are how a type with private fields, such as `time::Timestamp`, gets written. Both functions must be named functions, not lambdas or generic functions. A whole-type proxy can't return its own type, since it would apply again. Proxies can chain (a `Stamp` written as a `time::Timestamp`, itself written as text), but not through a nullable: a `via` whose other type is nullable and has its own `via` is `InvalidSchema`.

#### Writing a format

A format is a struct whose methods match traits in `std/serialization`. A type only needs the traits for what it contains: a format without `write_null` can't write a type with nullable fields, and that's a compile error naming the field (`'QueryWriter' can't encode 'User': 'User.nickname' is nullable and it has no method 'write_null'`).

| Writer trait | Methods |
| :----------- | :------ |
| `ScalarWriter` | `write_int(int)`, `write_float(float)`, `write_bool(bool)`, `write_string(string)` |
| `NullWriter` | `write_null()` |
| `StructWriter` | `begin_struct(fields, count)`, `field(fields, index)`, `end_struct()` |
| `ArrayWriter` | `begin_array(length)`, `item(index)`, `end_array()` |
| `MapWriter` | `begin_map(length)`, `key(name)`, `end_map()` |
| `EnumWriter` | `write_variant(variants, index)` |
| `UnionWriter` | `begin_union(variants, index)`, `end_union()` |

| Reader trait | Methods |
| :----------- | :------ |
| `ScalarReader` | `read_int()`, `read_float()`, `read_bool()`, `read_string()` |
| `NullReader` | `read_null() -> bool`: `true` when the next value is null, which it consumes |
| `StructReader` | `begin_struct(fields)`, `next_field(fields) -> int`, `missing_field(fields, index)` |
| `ArrayReader` | `begin_array()`, `has_item() -> bool` |
| `MapReader` | `begin_map()`, `next_key() -> ?string` |
| `EnumReader` | `read_variant(variants) -> int` |
| `UnionReader` | `begin_union(variants) -> int`, `end_union()` |
| `DocumentReader` | `end_document()`, for `decode_document` |

Every method takes `mut self: @` and returns `SerializationError!` its result. `byte` values go through `read_int` / `write_int` and are checked against `0..255` (`OutOfRange`).

`fields` and `variants` are plan ids. Read names with `field_count(fields)`, `field_name(fields, index)`, `variant_count(variants)` and `variant_name(variants, index)`, and match input with `find_field(fields, name)` and `find_variant(variants, name)`, which accept every outside name, aliases included, and return an index or `-1`. `find_field_in(fields, text, start, stop)` and `find_variant_in` match a slice of the input without copying it. `next_field` returns the index of the next known field, or `-1` when the struct ends; unknown names are the format's choice to skip or reject. A format reading rows can match column names once per result and then answer `next_field` by position.

```nim
import std/serialization
import std/array
import std/string
import std/url

struct QueryWriter
    parts: []string = []

    func write_int(mut self: @, value: int) -> serialization::SerializationError!void
        array::append(self.parts, "${value}")
    end

    func write_float(mut self: @, value: float) -> serialization::SerializationError!void
        array::append(self.parts, "${value}")
    end

    func write_bool(mut self: @, value: bool) -> serialization::SerializationError!void
        array::append(self.parts, if value then "1" else "0" end)
    end

    func write_string(mut self: @, value: string) -> serialization::SerializationError!void
        array::append(self.parts, url::encode(value))
    end

    func begin_struct(mut self: @, fields: int, count: int) -> serialization::SerializationError!void
    end

    func field(mut self: @, fields: int, index: int) -> serialization::SerializationError!void
        if index > 0 then
            array::append(self.parts, "&")
        end
        array::append(self.parts, "${serialization::field_name(fields, index)}=")
    end

    func end_struct(mut self: @) -> serialization::SerializationError!void
    end
end

struct Search
    term: string
    page: int = 1
end

func main() -> !void
    let mut writer = QueryWriter()
    try serialization::encode(Search(term = "dusk lang"), writer)
    assert(string::join(writer.parts) == "term=dusk%20lang&page=1")
end
```

## std/json

`json::encode[T, S: schema](value, options = @()) -> JsonError!string` writes compact JSON. `json::decode[T, S: schema](text, mut diagnostic = null, options = @()) -> JsonError!T` reads one complete JSON value; trailing whitespace is allowed. The target type can be explicit or come from the expected result type, and the schema is optional (see [Schemas](#schemas)):

```dusk
import std/json

struct User
    name: string
    age: int
    email: ?string = null
end

func main() -> !void
    let text = "{\"name\":\"Ada\",\"age\":42}"
    let user: User = try json::decode(text)
    let another = try json::decode[User](text)
    assert(user.name == another.name)
    assert(user.email == null)
    assert((try json::encode(user)) == "{\"name\":\"Ada\",\"age\":42,\"email\":null}")
end
```

Supported types, at the root and inside other values, are the ones `std/serialization` supports:

| Dusk type | JSON representation |
| :--- | :--- |
| `int`, `byte`, `float` | Number |
| `bool` | Boolean |
| `string` | String |
| `?T` | Null or the representation of `T` |
| `[]T` | Array |
| `map[string, T]` | Object |
| Struct | Object with the field names (or the schema's names) |
| Enum | Variant name as a string |
| Union | Object with exactly one variant-named member and its payload |

Concrete generic and recursive types work. A union built as `Response(success = value)` uses `{"success": ...}`. Static fields and methods do not participate.

**Fields and numbers.** Missing fields and defaults follow `std/serialization`: an absent field uses its declared default or fails with `MissingField`, and defaults that call functions run only after the whole document passed. Explicit JSON null is accepted only for nullable fields, regardless of defaults.

Unknown fields are ignored by default, but their values must still be valid JSON. Duplicate keys fail everywhere, including ignored objects; escaped spellings of the same key, and two accepted names of the same field, count as duplicates. String input must be valid UTF-8, with valid escape sequences and paired surrogate escapes. Encoding also rejects invalid UTF-8.

`int` and `byte` accept integer-form tokens only: `42` works, while `42.0`, `42e0` and `"42"` fail with `TypeMismatch`. Integers preserve their full range; `byte` is checked against `0..255`. Floats accept integer, decimal and exponent forms and round to their representable precision; overflow fails, and sufficiently small values can underflow to signed zero. Encoding NaN or infinity fails with `NonFiniteNumber`.

Encoding includes null fields (unless the schema says `omit_null`) and fields equal to their defaults. Struct fields appear in declaration order, and map entries in insertion order. Shared objects are written independently wherever they occur; cycles fail with `CycleDetected`. Decoding does not restore sharing between separate JSON objects.

**Schemas and proxies.** `json::encode[User, Api](user)` and `json::decode[User, Api](text)` use a schema's names. A fast built-in engine handles every type, proxies included: it reads or writes the other type, and only the `to` and `from` functions themselves run as Dusk code. A `from` that fails makes decoding fail with `InvalidValue`, and a `json::Diagnostic` then points at that value (its path and position).

**Options.** `DecodeOptions` has `should_reject_unknown_fields: bool = false` and `max_depth: int = 128`. `EncodeOptions` has `max_depth: int = 128`. A depth limit must be between 1 and 512, otherwise the operation fails with `InvalidOptions`. Each array or object adds one level; nullable wrappers and scalar values do not. Ignored values use the same limit.

```dusk
let user = try json::decode[User](text,
    options = json::DecodeOptions(should_reject_unknown_fields = true))
let encoded = try json::encode(user, json::EncodeOptions(max_depth = 64))
```

**Diagnostics.** Pass a mutable `json::Diagnostic()` as the second decode argument when the error's field path or location is needed. It contains `path: string`, `byte_offset: ?int`, `line: ?int` and `column: ?int`. The object is cleared before each call; a successful call leaves an empty path and null positions. Each caller owns its diagnostic object.

```dusk
let mut diagnostic = json::Diagnostic()
let user: User = json::decode(text, diagnostic) catch e then
    io::eprintln("invalid user at ${diagnostic.path}, byte ${diagnostic.byte_offset}")
    return e
end
```

Paths use forms such as `users[2].age`; map keys and field names containing punctuation are quoted, as in `["a.b"][0]`. The root path is empty. Byte offsets start at zero; lines and byte columns start at one. Missing fields point to the closing object; lexical errors report where the invalid input was detected.

`JsonError` contains `InvalidJson`, `TypeMismatch`, `MissingField`, `UnknownField`, `DuplicateKey`, `NumberOutOfRange`, `UnknownVariant`, `InvalidUnion`, `NonFiniteNumber`, `DepthExceeded`, `CycleDetected`, `InvalidOptions` and `InvalidValue`. Errors have no payload; diagnostics are separate. Pretty printing and streaming are not supported.

## Shared error sets

Functions that can fail return an error union, handled with `try` or `catch` like any failing call. Error sets from the standard library are written with their module (`string::StringError.OutOfRange`) unless they're imported by name. Each module's section lists its own error set; these are the small ones shared by the collection modules:

| Error | When |
| :---- | :--- |
| `ArrayError.Empty` | `pop` on an empty array. |
| `ArrayError.OutOfRange` | `insert`, `remove` or `slice` outside the array. |
| `StringError.OutOfRange` | `string::slice` with a negative bound, `start > stop`, `stop > string::len(text)`, or a bound inside a multi-byte character; `string::byte_at` / `string::char_at` with an index outside the string. |
| `StringError.NegativeCount` | `string::repeat` with a negative `count`. |
| `StringError.InvalidUtf8` | `string::from_bytes` with bytes that aren't valid UTF-8. |
| `BufferError.NegativeSize` | `buffer::new` with a negative size. |

## Known limitations

These are known gaps in v0.17. Most are planned in the [roadmap](roadmap.md).

- **Strings**: Positions are byte offsets; only `string::char_count` and `string::char_at` count characters (`char_at` scans from the start), and case changes cover ASCII and Latin-1 letters only. There's no case-insensitive comparison: compare `string::lower(a) == string::lower(b)`.
- **Building strings in a loop**: `s = string::concat(s, x)` copies the whole string every time. To build a long string, collect the pieces in an array and call `string::join` once.
- **Array search**: `index_of` and `contains` don't take arrays of enums, structs, unions, arrays or buffers yet.
- **Network**: TCP and TLS only: no UDP, no half-closing a connection and no socket options. Only one task at a time can wait in `accept` on a listener. TLS has no client certificates, no choosing a server certificate by host name, and no revocation checks.
- **HTTP**: HTTP/1.1 only: no HTTP/2 and no WebSocket. The client uses only `http://` and `https://` proxies (no SOCKS, and `HTTP_PROXY` and similar variables aren't read on their own), doesn't store cookies between requests, has no client certificates, and decompresses only gzip and deflate (not `br` or `zstd`). The server has no router (handlers match `req.path` themselves) and compresses only with gzip. Stopping a server on a signal (Ctrl+C, `SIGTERM`) isn't possible yet: another task has to call `server.close()`.
- **Serialization**: map keys must be strings, and schemas have no flattening of nested structs and no unions written without their variant name. `via` takes named, non-generic functions. Values written through `std/serialization` are read field by field into the type, never into a general tree of values.
