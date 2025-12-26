# Roadmap

## Version: 0.2 - Rukia

- [x] Single Line Comments
- [x] Full support for expressions and operators
- Add possibility to error tracking
- Fixed sized Arrays
- Function return type check for all possible branches
- Organize better internals functions

## Version: 0.3 - Maki

- Type inference (also for func args)
- Loop controll flow
- Remove the need of the keyword fn for functions
- Support for else if
- Support for C loops (With var inicialization)
- Inline returns

## Features Backlog

- Multi Line Comments
- Optionals
  - Optional fn args
- Error Handling
- String interpolation
- Ternary operator
- Structs
- Collections: Lists, Sets, HashMaps, Tuples
- Support function call with named parameters
- Modules
  - Caching starts to being a concern
- Enum
- Union (Zig-like)
- Closures (& fn as argument)
- Upgrade FnCall
  - Currently function calls only allow simple calls as `identifier()`
  - Which means things like that does not works:
    - getCallback()()
    - my_array\[0]()
    - (fn(x) { x + 1 })(10)
- Pattern match
- Pipe operator
- Fuzz testing
