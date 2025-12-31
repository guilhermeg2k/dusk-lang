# Roadmap

## Version: 0.3 - Maki

- Type inference (also for func args)
- Loop controll flow
- Function return type check for all possible branches
- Support for the operators: += -+
- Support for else if
- Support for C loops (With var inicialization)
- Inline returns

## Version: 0.4 - Chisato

- Structs
- Pipe operator
- Support line breaks (ex: on operations chain)

## Version: 0.5 - Takina

- Closures (& fn as anything (argument, item, etc))
- Optionals
  - Optional fn args
- Error Handling

## Features Backlog

- Enum
- Union (Zig-like)
- Pattern match
- Generics
- Ternary operator
- String interpolation
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
