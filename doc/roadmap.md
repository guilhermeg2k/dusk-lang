# Roadmap

> This is a moving target and can change anytime

## Version 0.15 - Enjin

- Closures (& fn as anything (argument, item, etc))
- Tasks: `spawn`, `await`, `sleep` (libuv timers)
- Modules and a standard library (`std/array`, `std/string`, `std/buffer`, `std/task`)
- HashMaps: `map[K, V]`

## Version 0.16 - Shigeo
- Libuv IO: files, TCP, DNS, processes (tasks wait on them like `sleep`), http
- Collections: Lists, Sets, Tuples


## Backlog

- Bytecode file generation + caching (Each compilation step)
- Code Formatter
- Fuzz testing
- Improve memory usage
- Improve VM Performance
- Improve int -> float promotion
- Verify places where @panic should be used instead of unrecheable
- Add more number notations
- Generators (`yield`) on the task engine
- Channels between tasks, cancelling a task, timeouts

