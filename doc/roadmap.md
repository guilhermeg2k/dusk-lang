# Roadmap

> This is moving target and can change anytime

## Version: 0.3 - Maki

- [x] Type inference
- [x] Support for else if
- [x] Inline function returns
- Loop controll flow

## Version: 0.4 - Kanade

- Support for C loops (With var inicialization)
- Function parameter mut modifier
- Function parameter default value with inference
- Function return type check for all possible branches

## Version: 0.5 - Chisato

- Structs

## Version: 0.6 - Takina

- Closures (& fn as anything (argument, item, etc))
- Pipe operator

## Version: 0.7 - Shinobu

- Optionals
  - Optional fn args
- Error Handling

## Backlog

- Support for the operators: += -+
- Enum
- Union (Zig-like)
- Generics
- Pattern match
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
- Replace quickjs with VM
