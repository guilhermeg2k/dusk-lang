# Roadmap

> This is a moving target and can change anytime

## Version 0.15 - Enjin'

- Closures (& fn as anything (argument, item, etc))
- Tasks: `spawn`, `await`, `sleep` (libuv timers)

## Version 0.18 - Youku

- Modules
- Privates~
- Collections: Lists, Sets, HashMaps, Tuples

## Version 0.19 - Shigeo

- Libuv IO: files, TCP, DNS, processes (tasks wait on them like `sleep`)
- Channels between tasks, cancelling a task, timeouts

## Backlog

- Code Formatter
- Fuzz testing
- Improve memory usage
- Improve VM Performance
- Files generation + caching (Each compilation step)
- Improve int -> float promotion
- Verify places where @panic should be used instead of unrecheable
- Add more number notations
- Generators (`yield`) on the task engine
