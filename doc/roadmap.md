# Roadmap

## Version: 0.2 - Rukia

- [x] Single Line Comments
- [x] Full support for expressions and operators
- [x] Improve error messages
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

- Optionals
  - Optional fn args
- Enum
- Union (Zig-like)
- Error Handling
- Closures (& fn as argument)
- Pattern match
- Pipe operator
- Ternary operator
- String interpolation
- Structs
- Support function call with named parameters
- Modules
  - Compilation speed starts to being a concern
- Multi Line Comments
- Collections: Lists, Sets, HashMaps, Tuples
- Upgrade FnCall
  - Currently function calls only allow simple calls as `identifier()`
  - Which means things like that does not works:
    - getCallback()()
    - my_array\[0]()
    - (fn(x) { x + 1 })(10)
- Fuzz testing
