# Roadmap

> This is a moving target and can change anytime

## Version: 0.5 - Chisato

- Structs
- Mut modifer added to the type data
- Support function call with named parameters
- Support for multiple line statements
  - Struct definition
  - Struct inicialziation
  - Function definition
  - Function call

## Version: 0.6 - Takina

- Optionals
  - Optional fn args
- Function parameter default value
- Inference for function arguments (When it has defualt value)

## Version: 0.7 - Mizuhara

- Error Handling

## Version: 0.8 - Shinobu

- Closures (& fn as anything (argument, item, etc))
- Pipe operator

## Backlog

- Enum
- Union (Zig-like)
- Generics
- Pattern match
- Defer
- Modules
  - Compilation speed starts to being a concern
- Multi Line Comments
- Ternary operator
- String interpolation
- Multline strings
- Collections: Lists, Sets, HashMaps, Tuples
- Fancy fors (ex: for in)
- Coroutines
- Upgrade FnCall
  - Currently function calls only allow simple calls as `identifier()`
  - Which means things like that does not works:
    - getCallback()()
    - my_array\[0]()
    - (fn(x) { x + 1 })(10)
- Fuzz testing
- Replace quickjs with own VM
- Improve errors message
