# Roadmap

### Version: 0.2 - Rukia

- Single Line Comments
- [x] Full support for expressions and operators
- Add possibility to error tracking
- Fixed sized Arrays
- Function return type check for all possible branches
- Organize better internals functions

### Version: 0.3 - Maki

- Type inference (also for func args)
- Loop controll flow
- Remove the need of the keyword fn for functions
- Support for else if
- Support for C loops (With var inicialization)
- Improve memory management (We are using arena but still doinga lot of deinits)

# Features Backlog

- Support function call with named parameters
- Enum
- Union (Zig-like)
- Multi Line Comments
- Upgrade FnCall
  - Currently function calls only allow simple calls as `identifier()`
  - Which means things like that does not works:
    - getCallback()()
    - my_array\[0]()
    - (fn(x) { x + 1 })(10)
- Optionals
  - Optional fn args
- Pattern match
- Inline returns
- Error Handling
- String interpolation
- Ternary operator
- Structs
- Modules
  - Caching starts to being a concern
- Pipe operator
- Fuzz testing
