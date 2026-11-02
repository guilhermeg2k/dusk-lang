# Roadmap

> This is a moving target and can change anytime

## Version 0.12 - Kanade

- Error Handling (implemented)
- `when`, `if` as expressions (implemented)
- Capture for (arrays implemented)
- Ranges (`1..5`) for `for` and `when` cases (implemented)
- `when` on floats (range cases only, e.g. `0.0..1.0 then`) (implemented)
- Nullable fallback (`orelse`) (implemented)

## Version 0.13 - Garou

- Multiline Comments (`#= ... =#`, nested; implemented)
- UTF-8 strings and escape sequences (implemented)
- Multiline strings (`"""` with closing-indentation stripping; implemented)
- `byte` type and `'x'` byte literals (implemented)
- `buffer`: fixed-size mutable bytes, sharing one VM byte object with `string` (implemented)
- Compile-time string literal pool (implemented)
- String operations: `str_*` builtins for search, transform, split/join, slicing, conversion, `StringError` / `ConversionError` (implemented)
- String interpolation (`$name` and `${expression}`, one-allocation `STR_BUILD`; implemented)
- String benchmarks across languages: `strings`, `concat`, `parse` (implemented)
- Real immutability, checked at compile time: mutability from the root of every path (implemented), compile-time constants instead of globals and mutable statics (implemented), consistent `mut` params and captures, `copy` and auto-move between `mut` and `let` values
- `when` on strings (implemented)

## Version 0.14 - Simon

- Meta structs
- Generics

## Version 0.15 - Fujiwara

- Closures (& fn as anything (argument, item, etc))

## Version 0.16 - Enjin

- Coroutines

## Version 0.18 - Youku

- Modules
- Privates~

## Version 0.19 - Shigeo

- Collections: Lists, Sets, HashMaps, Tuples

## Backlog

- Code Formatter
- Fuzz testing
- Improve memory usage
- Improve VM Performance
- Files generation + caching (Each compilation step)
- Improve int -> float promotion
- Verify places where @panic should be used instead of unrecheable
- Add more number notations
