# Roadmap

> This is a moving target and can change anytime

## Version 0.17 - Fujiwara
- Expand std::time
- Expand std::buffer
- std::serialization
- std::json
- std::os
- std::path
- std::random
- std::process
- std::sqlite

## Version 0.18 - Toji
- Type lists take any type (`[T: int | Color]`, structs, arrays, unions; unlocks `array::index_of` / `array::contains` on enums)
- Payload-less union variants (`union Token eof; number: int end`)
- Bitwise operators (`&`, `|`, `^`, `<<`, `>>`)
- Auxiliary number notations
- Infinite loops (`for true` without `break`) count as returning
- `task::all` and `task::race`
- `std/crypto` (SHA-256/512, HMAC, random bytes, hex/base64 — built on already-linked mbedTLS)
- `buffer` support in `std/serialization` (unlocks BLOB columns and `buffer` parameters in `std/sqlite`)
- Readable printing/interpolation for structs, unions, enums, and errors (field/variant/error names instead of raw pointers, `<union>`, or numbers)
- Generic functions in `via` (so generic containers like `Set[T]` can be serialized)
- Enum ↔ `int` conversions (e.g. `Color.Red.value()` and `Color.from(n)`) and get name func
- Enforce the member-order rule in structs, enums, and unions
- Dead code check after `return`
- `std/time` calendar fields (year/month/day/hour/minute/second/weekday, date-only ISO parsing, building timestamps from fields)
- Small missing helpers (`string::from_byte`, `string::to_bool`, `math::sum`, `buffer?[i]` optional indexing)
- Source line numbers in runtime errors (instruction-to-line map in bytecode)
- More unboxed map reads (`m[k] == null`, `m[k] == val`, `when m[k]`)
- Schema-level `omit_null` option (`schema Api(omit_null = true)` so nullable fields don't need individual `omit_null` rules)
- Reject using functions with inferred error sets (`-> !T`) as function values (require an explicit error set `E!T` instead of leaking unresolved placeholder error sets)
- Show concrete error set names in function-type mismatch diagnostics instead of printing `!T` (avoids confusing `expected 'func(...) -> !void' found 'func(...) -> !void'` errors)

## Backlog

- Bytecode file generation + caching (Each compilation step)
- Code Formatter
- Language Server
- Package Manager
- Building a single binary
- Fuzz testing
- Improve memory usage
- Improve VM Performance
- Channels between tasks, cancelling a task, timeouts
- Worker Threads

