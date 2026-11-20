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

External programs with asynchronous exit waiting and pipe I/O. Import with `import std/process`.

```dusk
import std/process
import std/io

func main() -> !void
    let result = try process::run(["git", "--version"])
    io::print(try result.stdout_text())
    assert(result.is_success())
end
```

`run(args: []string, cwd: ?string = null, env: ?map[string, string] = null, timeout_ms: int = -1, max_output: int = 16777216) -> ProcessError!Result` launches a program, captures stdout/stderr concurrently and waits for completion. Only the calling task waits; other tasks, timers and I/O continue. Launch setup, path lookup and validation are synchronous. The first argument is the executable; later arguments are passed literally, preserving empty strings and spaces. There is no implicit shell expansion, redirection or pipeline syntax.

Default stdin is an EOF source. `cwd` changes only the child's directory; relative paths resolve against the parent directory at submission. `env = null` uses the VM's supplied environment snapshot, matching `std/os`; absent embedding context means an empty environment. A supplied map replaces the child environment. On Windows, libuv may add required platform variables that are absent from it. Names must be nonempty and contain no `=` or zero byte, except host-supported drive environment names on Windows. Arguments, names, values and paths cannot contain zero bytes; host context text must be valid UTF-8.

A bare executable name is searched using the VM parent environment's `PATH`, even when `env` supplies a different child environment. Empty or relative PATH entries resolve against the child's working directory. Explicit executable paths resolve against that directory and bypass PATH search. Windows lookup also considers `.exe` and `.com`; `.bat` and `.cmd` files require an explicit interpreter.

`Result` contains `stdout: buffer`, `stderr: buffer` and `termination: Termination`. The termination union has `Exited: int` and `Signaled: int` variants. Exit codes and signal numbers retain the host-reported value; signal numbers are platform-specific. `is_success()` is true only for `Exited = 0`. Nonzero exits are results, not errors. `stdout_text()` and `stderr_text()` return `ProcessError!string`, failing with `InvalidUtf8` for binary data while preserving the original buffers.

`max_output` limits combined stdout/stderr bytes, defaulting to 16 MiB. It must be nonnegative; zero permits empty output only. Overflow returns `OutputTooLarge` after terminating and reaping the direct child. Output is not silently truncated into a successful result.

`timeout_ms = -1` means no deadline, `0` means immediate expiry, and lower values are invalid. The deadline covers launch accounting, output capture and child exit. It stays active if a child closes its outputs but continues running, or exits while a descendant retains a pipe. Expiry closes capture pipes, requests forced termination of the direct child and waits for its exit notification before returning `TimedOut`. Cleanup may outlast the nominal deadline while the host completes termination; descendants are not automatically terminated.

#### Interactive children

`start(args: []string, cwd: ?string = null, env: ?map[string, string] = null, stdin_mode: Stdio = Stdio.Ignore, stdout_mode: Stdio = Stdio.Inherit, stderr_mode: Stdio = Stdio.Inherit) -> ProcessError!Child` uses the same argument/path/environment rules and returns after launch. Modes are `Stdio.Inherit`, `Stdio.Ignore` and `Stdio.Pipe`. Inherited streams are OS standard descriptors; inheriting an embedded custom stdout/stderr writer fails with `Unsupported`. Preceding Dusk output is flushed before an inheriting child starts.

| Child method | Returns | Description |
| :----------- | :------ | :---------- |
| `wait(timeout_ms = -1)` | `ProcessError!Termination` | Waits for exit; completed status is cached for repeat calls. A wait timeout leaves the child running. |
| `kill()` | `ProcessError!void` | Requests forced termination without waiting. Use `wait()` to observe exit. Does nothing after observed exit. |
| `stdin()` | `ProcessError!PipeWriter` | Gets piped stdin, or fails with `NotPiped`. |
| `stdout()` / `stderr()` | `ProcessError!PipeReader` | Gets a piped reader, or fails with `NotPiped`. |
| `close()` | `ProcessError!void` | Releases pipes/resources after exit; `Busy` while running. Repeated close does nothing. |

Only one task can wait for a child at once (`Busy` otherwise). Waiting after close fails with `Closed`. Child wrappers share the same OS process, including wrappers made by `copy`; closing one closes its aliases.

A `PipeReader` supports `read_line(max = -1, timeout_ms = -1)`, `read_bytes(max, timeout_ms = -1)`, `read_exact(count, timeout_ms = -1)`, `read_all(timeout_ms = -1)`, `read_all_bytes(timeout_ms = -1)` and `close()`. A `PipeWriter` supports `write(text)`, `write_bytes(buffer)`, `flush()` and `close()`. Both fit the `std/io` reader/writer traits. Pipe operations use `io::IoError` and the same EOF, text/bytes and backpressure rules as other streams.

Closing stdin sends EOF; closing a pipe does not kill the child. Exit does not discard buffered output. Drain piped stdout/stderr concurrently while waiting, or use `run` for automatic collection. `Child.close()` closes remaining pipes and discards unread output. The program doesn't end while a child it started is still running, even if nothing waits for it: like a task, a child runs to the end. When the program stops on an error instead, the remaining children are killed.

Close children explicitly. A `Child` the program no longer reaches (no variable, field or copy holds it or one of its pipes) is cleaned up after a garbage collection finds it: one that has exited is released, and one still running has its pipes closed, so it sees the end of its input, and is released when it exits.

`ProcessError` contains `InvalidArgument`, `NotFound`, `PermissionDenied`, `InvalidExecutable`, `ResourceLimit`, `TimedOut`, `OutputTooLarge`, `InvalidUtf8`, `Unsupported`, `Other`, `Busy`, `Closed` and `NotPiped`.

## std/random

Random numbers, buffers and reproducible generators. Import with `import std/random`.

| Function | Returns | Description |
| :------- | :------ | :---------- |
| `integer(min: int, max: int)` | `RandomError!int` | Uniform integer with both bounds included. |
| `decimal()` | `RandomError!float` | A value from `0.0` inclusive to `1.0` exclusive. |
| `bytes(size: int)` | `RandomError!buffer` | A fresh buffer of ordinary random bytes. |
| `secure_bytes(size: int)` | `RandomError!buffer` | A fresh buffer from the host's secure entropy source. |
| `seeded(seed: int)` | `Generator` | An independent deterministic generator. |

Integer bounds can span the complete `int` range. `min > max` fails with `InvalidBounds`; equal bounds return that value without consuming randomness. Byte counts must be nonnegative (`NegativeSize` otherwise). Zero-byte requests return a fresh empty buffer without consulting a source or advancing a sequence. Invalid bounds and negative sizes also leave sequences unchanged.

The ordinary module functions share one generator per execution, initialized lazily from the host. Tasks share this sequence, so scheduling affects which task receives each draw. An embedded VM without the required host I/O context gets `Unsupported` when initialization or secure entropy is needed. Normal CLI executions supply that context. `decimal()` needs `try` because its source may need initialization.

`secure_bytes` obtains fresh secure entropy directly from the host. It fails with `EntropyUnavailable` if the source cannot complete, including host cancellation; it never falls back to the ordinary generator and returns no partial buffer on failure. Ordinary module functions and seeded generators provide no security guarantee; use `secure_bytes` for secrets. Calls are synchronous; host entropy acquisition can wait.

A `Generator` has these methods, all taking `mut self`:

| Method | Returns | Description |
| :----- | :------ | :---------- |
| `integer(min: int, max: int)` | `RandomError!int` | The same inclusive integer bounds. |
| `decimal()` | `float` | The same `[0, 1)` range; no host source or `try` needed. |
| `bytes(size: int)` | `RandomError!buffer` | Deterministic bytes with the same size rules. |

Every `int` seed is valid, including zero and negative values. The same seed and the same method calls with the same arguments reproduce the same results within a Dusk version. Sequence compatibility across versions is not guaranteed. Call boundaries matter: concatenating two `bytes(4)` results need not match one `bytes(8)` result.

Generators are ordinary shared structs. Aliases advance the same state; use `copy(rng)` to continue independently from the current state. Store a generator in a `let mut` binding to draw from it. Seeded generators do not consult host entropy and are unaffected by unrelated module draws. Their state and construction fields are private.

```dusk
import std/random
import std/buffer

func main() -> !void
    let die = try random::integer(1, 6)
    assert(die >= 1 and die <= 6)
    let mut rng = random::seeded(42)
    let mut replay = copy(rng)
    assert(try rng.integer(-10, 10) == try replay.integer(-10, 10))
    let fraction = rng.decimal()
    assert(fraction >= 0.0 and fraction < 1.0)
    let token = try random::secure_bytes(32)
    assert(buffer::len(token) == 32)
end
```

`RandomError` contains `InvalidBounds`, `NegativeSize`, `EntropyUnavailable` and `Unsupported`. Impossible buffer sizes and allocation failures follow the runtime allocation-error rules.

## std/path

Lexical path operations. Import with `import std/path`. Paths follow the runtime platform's syntax: slash separators on Unix, and drive/UNC paths with slash or backslash separators on Windows. These functions do not access the filesystem, check existence, follow symlinks or read the working directory.

| Function | Returns | Description |
| :------- | :------ | :---------- |
| `join(parts: []string)` | `string` | Joins nonempty parts with native separators, avoiding a duplicate separator at a boundary. Empty input produces `""`. |
| `normalize(text)` | `string` | Collapses redundant separators, `.` and lexical `..` components. Empty or cancelled relative paths become `"."`. |
| `basename(text)` | `string` | Last component, ignoring trailing separators. Empty paths and roots produce `""`. |
| `dirname(text)` | `?string` | Parent component text, ignoring trailing separators. Empty paths, bare names and roots have no parent (`null`). |
| `extension(text)` | `string` | Final extension of the last component, including its dot; absent extensions produce `""`. |
| `stem(text)` | `string` | Last component with its final extension removed. |
| `is_absolute(text)` | `bool` | Whether the path has a fully qualified root. |
| `resolve(base, text)` | `PathError!string` | Normalizes `text` against an absolute base. A fully absolute `text` replaces the base. |
| `relative(from, to)` | `PathError!string` | Computes a path from the absolute directory `from` to the absolute destination `to`. |

`join` combines text without normalizing dot components or discarding earlier parts when a later part starts with a separator. For example, `join(["a/", "/b"])` gives `"a/b"`. Use `normalize` or `resolve` for lexical dot-component processing. `normalize` preserves leading parent components in relative paths and clamps parent traversal at absolute roots. It removes trailing separators except where required for a root. Component queries preserve the input's component text rather than normalizing it.

A leading dot alone does not introduce an extension: `extension("config/.env")` is `""`, and its stem is `".env"`. `extension(".env.local")` is `".local"`, `stem("archive.tar.gz")` is `"archive.tar"`, and `extension("name.")` is `"."`. The special components `.` and `..` have no extension.

`resolve` requires an absolute base even when `text` is absolute. `relative` requires both inputs to be absolute, and returns `"."` for identical normalized paths. A relative input fails with `PathError.NotAbsolute`. Neither function has an implicit working directory; pass `os::cwd()` explicitly when needed.

On Windows, `is_absolute` accepts drive-qualified, UNC and device absolute roots; a rooted path such as `\folder` and a drive-relative path such as `C:folder` are not fully absolute. `resolve` can use a drive-qualified base to resolve a rooted path, but fails with `NotAbsolute` if a drive-relative destination remains unresolved. Normalization uses backslash separators and uppercase drive letters. `relative` returns the normalized absolute destination when the inputs are on different drives or shares. Ordinary component comparison follows the platform path rules; it does not query filesystem-specific case behavior.

```dusk
import std/path
import std/os

func main() -> !void
    let base = try os::cwd()
    let file = try path::resolve(base, "assets/./images/../logo.png")
    assert(path::basename(file) == "logo.png")
    assert(path::stem(file) == "logo")
    assert(path::extension(file) == ".png")
    assert(try path::relative(base, file) == path::join(["assets", "logo.png"]))
end
```

These operations work on path text and do not validate whether it names a legal filesystem entry. They do not expand `~`, environment variables, wildcards or URL escapes. Lexical `..` processing can differ from physical traversal through symlinks; resolving against a base does not ensure the result stays inside that directory.

## std/os

Program context and shutdown notifications. Import with `import std/os`.

| Function | Returns | Description |
| :------- | :------ | :---------- |
| `platform()` | `Platform` | The operating system targeted by the running runtime: `Linux`, `MacOS`, `Windows` or `Other`. |
| `args()` | `OsError![]string` | A fresh array of program arguments. |
| `env(name)` | `OsError!?string` | An environment value, or `null` when absent. An empty value is `""`. |
| `cwd()` | `OsError!string` | The absolute current working directory. |
| `set_exit_code(code)` | `OsError!void` | Sets the status for normal completion; execution continues. |
| `watch_signal(signal)` | `OsError!SignalStream` | Subscribes to a shutdown signal immediately. |

For `dusk app.dsk first --flag`, `args()` returns `["first", "--flag"]`: it excludes the runtime executable and script path. Arguments after the script path belong to the program, including `--version`. The CLI reads environment values from its startup environment map. Environment names follow host case rules; empty names and names containing `=` or a zero byte fail with `InvalidArgument`. Invalid UTF-8 in arguments, returned environment values or the working-directory path fails with `InvalidUtf8`. These calls copy results into program values.

Embedded executions receive arguments and a read-only environment map from the host; both default to empty. The host keeps their storage alive and unchanged during execution. `cwd()` uses the host-provided I/O context. It reports the process working directory, which can differ from the entry module's directory. This module does not change environment variables or the working directory.

Exit status starts at `0` for each execution. `set_exit_code` accepts `0` through `255`; an invalid code fails with `InvalidArgument` and leaves the previous status unchanged. The last successful call wins, including calls from other tasks. All tasks continue under the normal execution rules; output is flushed and runtime resources are released before the CLI exits. Runtime errors and unhandled task failures still produce failure regardless of the selected status. Embedded executions return control to their host, which can read `Dusk.exit_code` after execution.

`Signal` has `Interrupt` (usually Ctrl+C) and `Terminate` variants. Both are supported on Unix hosts; on Windows, `Terminate` fails with `Unsupported`, while `Interrupt` receives console notifications. Ctrl+C does not generate an interrupt while the terminal is in raw mode.

| SignalStream method | Returns | Description |
| :------------------ | :------ | :---------- |
| `wait()` | `OsError!void` | Consumes a pending notification or waits while other tasks continue. |
| `close()` | `void` | Stops the subscription; repeated calls do nothing. |

Subscribe before starting work to receive signals that arrive before `wait()`. Each subscription retains at most one pending notification; repeated arrivals can coalesce. Separate subscriptions each receive a notification. Only one task can wait on a subscription at a time (`Busy` otherwise). Closing wakes a waiting task with `Closed`; waiting after close also fails with `Closed`. A subscription without a waiting task does not keep execution alive; a waiting task does. Open subscriptions are closed when execution ends.

While subscribed, repeated interrupts follow the same notification rule and do not force an exit. Closing the last subscription ends interception of that signal. A `SignalStream` the program no longer reaches is closed after a garbage collection finds it. Signal handling does not cancel tasks or automatically execute application cleanup; the waiting task performs that cleanup explicitly.

```dusk
import std/os
import std/io

func main() -> !void
    let shutdown = try os::watch_signal(os::Signal.Interrupt)
    io::println("Press Ctrl+C to stop.")
    try shutdown.wait()
    shutdown.close()
    io::println("Shutting down...")
end
```

`OsError` contains `InvalidArgument`, `InvalidUtf8`, `NotFound`, `Unsupported`, `Busy`, `Closed` and `Other`. `NotFound` can occur when the current directory no longer exists; `Other` covers host failures without a more specific variant. `SignalStream` construction and its native handle are private to the module.

## std/time

Clocks, timestamps, durations, and monotonic instants. Import with `import std/time`. A [complete example](../examples/time.dsk) combines timestamp arithmetic, named zones, custom formatting, monotonic timing, and timestamp serialization.

| Function | Returns | Description |
| :------- | :------ | :---------- |
| `now()` | `Timestamp` | A timestamp from the current wall-clock reading. |
| `from_unix_ms(milliseconds)` | `Timestamp` | A timestamp from any `int` epoch-millisecond value, including negative values. |
| `to_unix_ms(timestamp)` | `int` | The timestamp's epoch milliseconds. |
| `to_iso_string(timestamp, zone = utc_timezone())` | `TimeError!string` | Formats an ISO timestamp in the selected zone, defaulting to UTC. |
| `to_utc_iso_string(timestamp)` | `string` | Infallible unary UTC formatter for serialization and other function-value uses. |
| `from_iso_string(text)` | `TimeError!Timestamp` | Reads a full Gregorian timestamp with an explicit `Z` or numeric `+HH:MM` / `-HH:MM` offset. Supports expanded years and optional fractions, truncated to milliseconds. Invalid text fails with `TimeError.InvalidTimestamp`; a valid value outside the `int` millisecond range fails with `TimeError.Overflow`. |
| `now_ms()` | `int` | Milliseconds since 1970-01-01T00:00:00Z (the Unix epoch), matching filesystem timestamp units. |
| `monotonic_ns()` | `int` | A nondecreasing nanosecond reading from an unspecified origin. Subtract two readings to measure elapsed time. |
| `instant()` | `Instant` | Captures the current monotonic reading for elapsed-time measurements. |
| `nanoseconds(value)` | `Duration` | A duration from any signed `int` nanosecond count. |
| `microseconds(value)` | `Duration` | `value` microseconds. |
| `milliseconds(value)` | `Duration` | `value` milliseconds. |
| `seconds(value)` | `Duration` | `value` seconds. |
| `minutes(value)` | `Duration` | `value` minutes. |
| `hours(value)` | `Duration` | `value` hours. |

A `Duration` holds at most about 292 years either way. A factory given more stops the program with a runtime error (`2562048 hours doesn't fit in a Duration`), since no correct program asks for that; adding and subtracting durations are checked instead and fail with `TimeError.Overflow`.
| `utc_timezone()` | `TimeZone` | The UTC zone, with offset zero for every timestamp. |
| `fixed_timezone(offset_seconds)` | `TimeError!TimeZone` | A constant offset east of UTC; requires strictly more than -86400 and less than +86400 seconds. |
| `load_timezone(name)` | `TimeError!TimeZone` | Loads a named zone or alias from the bundled database. Unknown or invalid names return `UnknownTimeZone`. |
| `local_timezone()` | `TimeError!TimeZone` | Discovers and snapshots the configured local zone on its first successful call. Failure returns `LocalTimeUnavailable`. |

`now_ms` is wall-clock time: system clock adjustments can move it forward or backward. It is independent of the local time zone. Fractions are rounded down to milliseconds, including before the epoch.

`monotonic_ns` ignores jumps in the system wall clock. Consecutive readings may be equal; nanosecond units do not guarantee nanosecond resolution. Elapsed time includes waiting for tasks or I/O, rather than only time spent executing. Inclusion of time while the whole system is suspended is platform-dependent. Use differences within a program run; do not persist monotonic readings as timestamps or compare them across runs or machines.

The raw clock calls return immediately, without allocating or yielding to another task. Neither needs `try`. A platform clock failure or a reading outside `int` range is a runtime error. Waiting remains `task::sleep` in `std/task`.

```dusk
import std/time
import std/array
import std/io

func main() -> void
    let mut numbers = [3, 1, 2]
    let start = time::monotonic_ns()
    array::sort(numbers)
    let elapsed_ns = time::monotonic_ns() - start
    let elapsed_ms = to_float(elapsed_ns) / 1000000.0
    io::println("Sorted in $elapsed_ms ms")
end
```

A `Timestamp` stores epoch milliseconds in a private field and has no mutating methods. Create one with `now` or `from_unix_ms`, and read it back with `time::to_unix_ms` or `time::to_iso_string`. It exposes:

| Method | Returns | Description |
| :----- | :------ | :---------- |
| `format(pattern, zone = utc_timezone())` | `TimeError!string` | Formats numeric calendar fields and literal text using a custom pattern. |
| `is_equal(other)` | `bool` | Whether both timestamps represent the same epoch milliseconds. |
| `is_before(other)` | `bool` | Whether this timestamp precedes the other. |
| `is_after(other)` | `bool` | Whether this timestamp follows the other. |
| `add(duration)` | `TimeError!Timestamp` | Adds elapsed time, requiring exact whole milliseconds. |
| `subtract(duration)` | `TimeError!Timestamp` | Subtracts elapsed time, requiring exact whole milliseconds. |
| `since(other)` | `TimeError!Duration` | Signed elapsed time: this timestamp minus the other, in nanoseconds. |

UTC formatting works for every `int` timestamp, including both integer limits. `to_iso_string` requires `try` or `catch`, including when the zone defaults to UTC. The unary module adapter `to_utc_iso_string` returns a plain string. Years `0000–9999` use four digits. Larger years have a leading `+` and all digits; negative years have a leading `-` and at least four digits. The Gregorian calendar extends backward through year zero. Unix milliseconds do not represent leap seconds. Expanded-year output is not strict RFC 3339.

```dusk
let epoch = time::from_unix_ms(0)
io::println(try time::to_iso_string(epoch))
let before_epoch = time::from_unix_ms(-1)
io::println(try time::to_iso_string(before_epoch))
let modified = time::from_unix_ms(info.modified_ms)
```

The first two lines printed are `1970-01-01T00:00:00.000Z` and `1969-12-31T23:59:59.999Z`. The `Z` denotes UTC. With no explicit zone, output always uses UTC, independently of local settings. A year beyond 9999 is written like `+10000-01-01T00:00:00.000Z`.

The raw clock functions above do not allocate; timestamp factories allocate an ordinary struct, and ISO formatting allocates its result string. `now` retains `now_ms`'s runtime clock-error behavior.

`from_iso_string` requires uppercase `T` and a final uppercase `Z` or offset, with no surrounding whitespace or trailing text. Years use four digits, a negative sign with at least four digits, or a positive sign for years above 9999. The Gregorian calendar extends through year zero. A fraction must contain at least one digit; one or two digits are padded to milliseconds, and digits after the third are validated and discarded. Leap seconds, `24:00`, date-only and time-only input, week dates, ordinal dates, and intervals are rejected. Offset hours range from 00 to 23 and minutes from 00 to 59. Both `+00:00` and `-00:00` mean zero offset; the original offset is not retained.

For example, `1970-01-01T05:30:00+05:30` represents epoch millisecond zero, and `+10000-01-01T00:00:00Z` is accepted. Every successful `to_iso_string` result can be read back to recover the same timestamp, including both `int` limits. An offset can move the local date across a day or year boundary; only the resulting UTC epoch milliseconds must fit in `int`.

`to_utc_iso_string` / `from_iso_string` and `to_unix_ms` / `from_unix_ms` are pairs of unary functions, so they can be given to a schema's `via` to write timestamps as text or numbers (see [std/serialization](#stdserialization)). Use `via(time::to_utc_iso_string, time::from_iso_string)` for text. The general `to_iso_string` function takes two parameters, even though the zone has a default, and returns a fallible result; it is not a unary schema encoder. Like other structs, `==` compares identity; use `is_equal`, `is_before`, and `is_after` for chronological comparisons.

A `Duration` stores signed nanoseconds in a private field. Its range is approximately 292 years in either direction, independently of the much wider timestamp range. It has no mutating methods.

| Duration method | Returns | Description |
| :-------------- | :------ | :---------- |
| `nanoseconds()` | `int` | The exact signed nanosecond count. |
| `microseconds()` | `int` | Whole microseconds, truncated toward zero. |
| `milliseconds()` | `int` | Whole milliseconds, truncated toward zero. |
| `seconds()` | `float` | Seconds as a floating-point value; large values may round. |
| `add(other)` | `TimeError!Duration` | The sum, failing with `Overflow` if it exceeds the signed nanosecond range. |
| `subtract(other)` | `TimeError!Duration` | This duration minus the other, with the same overflow check. |
| `is_equal(other)` | `bool` | Whether both durations have equal nanosecond counts. |
| `is_less_than(other)` | `bool` | Whether this duration has a smaller signed nanosecond count. |
| `is_greater_than(other)` | `bool` | Whether this duration has a larger signed nanosecond count. |

Timestamp `add` and `subtract` reject durations containing fractional milliseconds with `TimeError.PrecisionLoss`, including negative fractions. They return `Overflow` when the resulting epoch milliseconds do not fit. Timestamp `since` returns `Overflow` when the signed difference cannot fit in a Duration, even if both timestamps are valid. These operations measure elapsed time; calendar months and years are not units of Duration. The operands remain unchanged.

```dusk
let epoch = time::from_unix_ms(0)
let later = try epoch.add(try time::seconds(2))
assert(later.is_after(epoch))
assert((try later.since(epoch)).milliseconds() == 2000)
assert(time::nanoseconds(-1500000).milliseconds() == -1)
```

An `Instant` stores a private monotonic reading. `instant()` retains the clock resolution, suspend behavior, and runtime clock errors of `monotonic_ns()`. It has no date or time-zone representation; use it only within one program execution.

| Instant method | Returns | Description |
| :------------- | :------ | :---------- |
| `elapsed()` | `TimeError!Duration` | The current monotonic reading minus this instant. |
| `since(other)` | `TimeError!Duration` | This instant minus the other; reversing operands gives a negative duration. |

Both methods check the signed nanosecond range and return `Overflow` when needed. Consecutive instants may have equal readings. Timestamp, Duration, and Instant factories create ordinary structs; copying them preserves their stored values. Use Duration comparison methods rather than struct `==` to compare nanosecond counts. Sleeping continues to take integer milliseconds in `task::sleep`.

```dusk
let started = time::instant()
let elapsed = try started.elapsed()
io::println("Elapsed: ${elapsed.seconds()} seconds")
```

A `TimeZone` is an immutable value with private representation. Dusk bundles IANA tzdb 2026f: 597 named zones and aliases, with no network or host timezone-database requirement at runtime. Use exact case-sensitive names such as `America/Sao_Paulo`, `America/New_York`, or `Asia/Kathmandu`. `load_timezone` requires a bundled name; absolute paths, traversal, NUL bytes, raw offset strings and `right/` or `posix/` database trees are not accepted by that function. An alias keeps its requested name.

| TimeZone method | Returns | Description |
| :-------------- | :------ | :---------- |
| `name()` | `string` | The requested bundled name, a stable UTC/fixed-offset name, or `Local` for anonymous local rules. |
| `offset_seconds(timestamp)` | `int` | The UTC offset at this timestamp, in seconds, positive east of UTC. |

Named offsets use historical transitions and recurring future rules across the full timestamp range. Before the first transition, the zone's initial time type applies. Future values extrapolate the bundled rules; governments can change those rules after this database release. Historical second-level offsets are preserved. Two named zones can share an offset today and have different rules for another date.

Fixed offsets never change with the timestamp. Their names are `UTC` at zero, `UTC+05:30` for an offset of 19800 seconds, or `UTC-00:00:30` for -30 seconds. Unsupported fixed offsets return `TimeError.InvalidOffset`. `utc_timezone()` and `fixed_timezone(0)` both represent UTC.

```dusk
let new_york = try time::load_timezone("America/New_York")
let winter = try time::from_iso_string("2024-01-15T00:00:00Z")
let summer = try time::from_iso_string("2024-07-15T00:00:00Z")
assert(new_york.offset_seconds(winter) == -18000)
assert(new_york.offset_seconds(summer) == -14400)
let fixed = try time::fixed_timezone(19800)
assert(fixed.name() == "UTC+05:30")
```

Named-zone loading and offset lookup do not yield. Parsed named rules are cached per runtime and released on teardown; copied wrappers share them. UTC and fixed zones need no retained registry allocation. Zone names are ordinary allocated strings. Custom numeric patterns use the same zone rules and calendar conversion as ISO formatting.

`to_iso_string` applies the selected zone's offset at the timestamp, then writes local calendar fields and an explicit offset. Zero prints `Z`; other offsets print `+HH:MM` or `-HH:MM`. Applying an offset can move the displayed date across a day, year, or the epoch-millisecond integer boundary; wide calendar arithmetic keeps the represented timestamp unchanged. DST changes can skip or repeat local clock readings, with the explicit suffix distinguishing repeated times.

```dusk
let timestamp = try time::from_iso_string("2024-07-15T00:00:00Z")
let new_york = try time::load_timezone("America/New_York")
assert((try time::to_iso_string(timestamp, new_york)) == "2024-07-14T20:00:00.000-04:00")
assert((try time::to_iso_string(timestamp)) == "2024-07-15T00:00:00.000Z")
assert(time::to_utc_iso_string(timestamp) == "2024-07-15T00:00:00.000Z")
```

ISO suffixes represent whole minutes. If a fixed or historical named-zone offset contains seconds, formatting fails with `TimeError.PrecisionLoss`; it never rounds the offset or changes the moment. `offset_seconds` still reports the exact offset, and `to_utc_iso_string` always provides a UTC representation. Serialization through the UTC adapter writes canonical UTC text and accepts any supported explicit offset when reading.

`local_timezone()` snapshots the first successful discovery for the runtime. Cold calls wait for a worker, allowing other tasks to continue; cached calls do not yield. Later environment, file, or system-setting changes do not refresh that snapshot. A failed discovery leaves the cache empty, so a later call can retry. Failure never silently substitutes UTC.

When `TZ` is present, it is authoritative on every platform:

- An empty value selects UTC. A bundled name, optionally prefixed with `:`, selects that named zone and preserves aliases.
- An absolute file path, optionally prefixed with `:`, loads an owned TZif snapshot named `Local`.
- A supported explicit recurrence string such as `STD-5:30` selects rules named `Local`. Its offset uses the configuration convention: `-5:30` means 5 hours 30 minutes east of UTC. Recurrence forms include Julian and ordinal days, month/week/weekday rules, and signed transition times.
- A `:` prefix permits only a zone name or absolute file path. Invalid or unsupported settings return `LocalTimeUnavailable` without trying another source.

With no `TZ`, Unix systems inspect `/etc/localtime`. A recognized zone name in a standard zoneinfo symlink selects bundled rules; other files are copied into a snapshot named `Local`. Local files must be regular, no larger than 4 MiB, and readable with supported complete rules. Static version-1 TZif and supported version-2/3 files are accepted. Leap-second data, version 4, unspecified time types, out-of-range offsets, malformed data, and transition-bearing files without future rules return `LocalTimeUnavailable`.

With no `TZ`, Windows reads the nonlocalized system zone key and maps it through the bundled CLDR release-48 global mappings. The resulting zone uses the mapping's representative name and bundled rules. Unsupported customized settings, including disabled dynamic rules, return `LocalTimeUnavailable`; a verified plain UTC configuration is supported. This discovery does not change process-wide timezone settings.

```dusk
let local = try time::local_timezone()
let current = time::now()
io::println(try time::to_iso_string(current, local))
```

The default formatter and serialization adapter continue to use UTC. Local discovery does not change their behavior, and ISO formatting can still return `PrecisionLoss` when a local historical offset has seconds.

`timestamp.format(pattern, zone = utc_timezone())` writes numeric calendar fields in the selected zone. It defaults to UTC, requires `try` or `catch`, and supports these case-sensitive tokens:

| Token | Meaning | Example |
| :---- | :------ | :------ |
| `YYYY` | Year with at least four digits and the ISO expanded-year sign rules. | `2026` |
| `MM` | Two-digit month. | `11` |
| `DD` | Two-digit day. | `18` |
| `HH` | Two-digit hour, from 00 to 23. | `14` |
| `mm` | Two-digit minute. | `30` |
| `ss` | Two-digit second. | `45` |
| `SSS` | Three-digit millisecond. | `123` |
| `Z` | UTC offset, with `Z` at zero and `+HH:MM` or `-HH:MM` otherwise. | `-03:00` |

Adjacent different tokens work without separators (`YYYYMMDD`). Each complete repeated-letter run must be a recognized token: `MMMM`, `YYYYY`, `SS`, and `ZZ` are errors. Unescaped ASCII letters must form tokens; punctuation, spaces, digits, and non-ASCII UTF-8 bytes pass through as literals.

Bracketed text is literal: `[Created on]` writes `Created on`, and `[Z]` writes the letter `Z`. Outside bracketed text, `[[` writes `[` and `]]` writes `]`. Brackets cannot nest; unclosed literals and unmatched single closing brackets return `TimeError.InvalidFormat`. Empty literals (`[]`) are allowed, and an empty pattern produces an empty string.

```dusk
let timestamp = try time::from_iso_string("2026-11-18T14:30:45.123Z")
assert((try timestamp.format("YYYY-MM-DD")) == "2026-11-18")
assert((try timestamp.format("DD/MM/YYYY")) == "18/11/2026")
assert((try timestamp.format("[Created on] DD/MM/YYYY [at] HH:mm")) == "Created on 18/11/2026 at 14:30")
assert((try timestamp.format("[[YYYY]]")) == "[2026]")
assert((try timestamp.format("YYYY-MM-DD[T]HH:mm:ss.SSSZ")) == (try time::to_iso_string(timestamp)))
```

`Z` requires an offset that is an exact number of minutes. An offset with a seconds component returns `TimeError.PrecisionLoss` when the pattern contains that token. Patterns without `Z` still display the exact local fields, including historical second-level adjustments. An escaped `[Z]` is literal and imposes no offset-precision requirement. For example, midnight UTC on 1800-01-01 formats as `00:09:21` with `HH:mm:ss` in `Europe/Paris`.

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

When the program reading the output stopped (`dusk app.dsk | head -1`), they fail with `BrokenPipe` and the program keeps running; the output that couldn't be sent is dropped. Writes follow the same buffering rules as `io::print`, so a failure can show up at the next `write` or `flush` rather than at the write that caused it.

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

Functions that can fail return an error union, handled with `try` or `catch` like any failing call. Error sets from the standard library are written with their module (`string::StringError.OutOfRange`) unless they're imported by name. Each module's section lists its own error set; these are the ones the reference lists in one place: the small sets shared by the collection modules and `std/time`'s `TimeError`:

| Error | When |
| :---- | :--- |
| `ArrayError.Empty` | `pop` on an empty array. |
| `ArrayError.OutOfRange` | `insert`, `remove` or `slice` outside the array. |
| `StringError.OutOfRange` | `string::slice` with a negative bound, `start > stop`, `stop > string::len(text)`, or a bound inside a multi-byte character; `string::byte_at` / `string::char_at` with an index outside the string. |
| `StringError.NegativeCount` | `string::repeat` with a negative `count`. |
| `StringError.InvalidUtf8` | `string::from_bytes` with bytes that aren't valid UTF-8. |
| `BufferError.NegativeSize` | `buffer::new` with a negative size. |
| `TimeError.InvalidTimestamp` | `time::from_iso_string` with malformed or unsupported timestamp text, an impossible calendar date, or an invalid offset. |
| `TimeError.Overflow` | A valid parsed timestamp outside the epoch-millisecond range, an overflowing Duration or Timestamp arithmetic operation, or a timestamp/instant difference outside the signed nanosecond range. |
| `TimeError.PrecisionLoss` | Timestamp arithmetic with fractional milliseconds, or ISO formatting/custom `Z` with an offset that is not an exact number of minutes. |
| `TimeError.UnknownTimeZone` | A name is invalid or absent from the bundled timezone database. |
| `TimeError.InvalidOffset` | A fixed timezone offset is outside the supported range. |
| `TimeError.LocalTimeUnavailable` | Explicit local configuration is invalid, the system zone cannot be discovered or mapped, or its local rules are unsupported or unreadable. |
| `TimeError.InvalidFormat` | A custom timestamp pattern has an unsupported token or malformed bracket escaping. |

## Known limitations

These are known gaps in v0.17. Most are planned in the [roadmap](roadmap.md).

- **Strings**: Positions are byte offsets; only `string::char_count` and `string::char_at` count characters (`char_at` scans from the start), and case changes cover ASCII and Latin-1 letters only. There's no case-insensitive comparison: compare `string::lower(a) == string::lower(b)`.
- **Building strings in a loop**: `s = string::concat(s, x)` copies the whole string every time. To build a long string, collect the pieces in an array and call `string::join` once.
- **Array search**: `index_of` and `contains` don't take arrays of enums, structs, unions, arrays or buffers yet.
- **Network**: TCP and TLS only: no UDP, no half-closing a connection and no socket options. Only one task at a time can wait in `accept` on a listener. TLS has no client certificates, no choosing a server certificate by host name, and no revocation checks.
- **HTTP**: HTTP/1.1 only: no HTTP/2 and no WebSocket. The client uses only `http://` and `https://` proxies (no SOCKS, and `HTTP_PROXY` and similar variables aren't read on their own), doesn't store cookies between requests, has no client certificates, and decompresses only gzip and deflate (not `br` or `zstd`). The server has no router (handlers match `req.path` themselves) and compresses only with gzip. Stopping a server on a signal (Ctrl+C, `SIGTERM`) isn't possible yet: another task has to call `server.close()`.
- **Serialization**: map keys must be strings, and schemas have no flattening of nested structs and no unions written without their variant name. `via` takes named, non-generic functions. Values written through `std/serialization` are read field by field into the type, never into a general tree of values.
