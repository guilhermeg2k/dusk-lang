# Language Reference

This is the reference for **v0.16 - Shigeo** (in development). Planned features are listed in the [roadmap](roadmap.md).

Code blocks use Nim highlighting, which is close enough to Dusk's syntax.

## Contents

1. [Basics](#1-basics)
2. [Variables](#2-variables)
3. [Types](#3-types)
4. [Structs](#4-structs)
5. [Enums](#5-enums)
6. [Unions](#6-unions)
7. [Nullables](#7-nullables)
8. [Operators](#8-operators)
9. [Control Flow](#9-control-flow)
10. [Functions](#10-functions)
11. [Generics and Traits](#11-generics-and-traits)
12. [Error Handling](#12-error-handling)
13. [Tasks](#13-tasks)
14. [Modules](#14-modules)
15. [Prelude and Standard Library](#15-prelude-and-standard-library)
16. [Errors](#16-errors)
17. [Known Limitations](#17-known-limitations)

## 1. Basics

A program starts at its `main` function. Since v0.13 every program must declare one, taking no parameters and returning `void`, or an error union of `void` (`-> !void`) when it uses `try` (see [try](#try)).

```nim
import std/io

func main() -> void
    io::println("Hello, Dusk")
end
```

The top level only holds declarations: functions, structs, enums, unions, error sets and [constants](#constants). Any other statement, a `let mut` or a `let` that isn't a constant at the top level is a compile error (`StatementOutsideMain`, `TopLevelLetNotConstant`). Functions and types are declared at the top level only, not inside a function (`DeclarationInsideFunction`).

To keep them short, the examples in this reference show statements without the `main` around them. Examples that use the standard library (`io::println`, `len`, `append`, `string::split`, `task::sleep`, ...) leave out its imports too: `import std/io`, `import std/array: len, append`, `import std/string` and so on (see [Modules](#14-modules)).

**Comments** start with `#` and run to the end of the line.

```nim
# a comment
let x = 10 # an inline comment
```

Since v0.13, multiline comments use `#= ... =#` and can nest. They can appear between tokens, including on the same line. An unclosed comment is a lexer error pointing at its opening delimiter.

```nim
#= A multiline comment
   #= A nested comment =#
=#
let total = 1 #= explanation =# + 2
```

**Blocks** are delimited by keywords: `then ... end`, `do ... end`, and `end` for definitions. Indentation is only for readability.

**Line breaks** aren't significant. A statement can span several lines, but two expressions on consecutive lines can also join into one: `when n` followed by `-1 then` on the next line is read as `when n - 1`.

**Hoisting**: functions, structs, enums, unions, error sets and [constants](#constants) can be used before they're defined. Other variables must be defined before use.

**Keywords**: `let`, `mut`, `func`, `return`, `struct`, `enum`, `union`, `error`, `trait`, `try`, `catch`, `extern`, `if`, `elif`, `else`, `when`, `for`, `break`, `continue`, `then`, `do`, `end`, `and`, `or`, `orelse`, `import`, `as`, `true`, `false`, `null`, `int`, `byte`, `float`, `string`, `buffer`, `bool`, `void`.

## 2. Variables

Variables are declared with `let`. The type is inferred from the value, or written after a `:`.

```nim
let name = "Dusk"
let count: int = 100
```

Variables are immutable by default. Add `mut` to allow reassignment.

```nim
let x = 10
x = 20          # error: x is not mutable

let mut y = 10
y = 20
```

A variable's mutability also covers everything reached through it: struct fields (at any depth), array items and buffer bytes. Changing any of them, directly, with `append` or by calling a `mut self` method, needs the variable to be `let mut`. The same goes for parameters (`self: @` included) and `for`, `if` and `when` captures. A value that isn't stored in a variable, such as a function result or a new struct, can be changed directly, so `mut self` methods chain: `b.add(1).add(2)` when `add` returns `self`.

```nim
let p = Point(x = 1, y = 2)
p.x = 5          # error: p is not mutable

let mut q = Point(x = 1, y = 2)
q.x = 5
```

`let` only stops changes made through that name: another variable that shares the value can still change it (see [Sharing values](#sharing-values)).

### Sharing values

Arrays, buffers, structs and unions (and their nullables) are shared when assigned or passed: `let b = a` makes `b` and `a` the same array, so a change through one shows through the other. This also holds between a `let mut` variable and a `let` one. Use `copy(x)` for an independent value:

```nim
let mut items = [1, 2]
let view = items
let snapshot = copy(items)
append(items, 3)
io::println("$view")       # [1, 2, 3]
io::println("$snapshot")   # [1, 2]
```

Strings and primitives can't be changed in place, so sharing them never shows.

A value read through a name that can't change it (a `let` variable, a parameter or capture without `mut`, `self` in a method without `mut self`, or a field or item reached through one of them) can't go into a mutable place: a `let mut` variable, an assignment to one or to its fields and items, or a `mut` argument (`append` and `insert` take the stored item as `mut`). Constructors, array literals and `if`, `when`, `orelse` and `catch` results count as the values they hold. Otherwise the value could change without any `mut` on its own name:

```nim
let frozen = [1, 2]
let mut z = frozen                      # error: 'frozen' can't change, so it can't go into a mutable value
let mut x = Obj(items = frozen)         # error, same reason
let view = Obj(items = frozen)          # fine: nothing changes through 'view'
let mut owned = Obj(items = copy(frozen))   # fine
```

New values (literals, constructors of new values, call results and `copy(x)`) can go anywhere. The other way around is fine too: a `let` can share a `let mut` value, and sees its changes.

### Constants

A top-level `let` whose value is known at compile time is a constant: an `int`, `float`, `bool`, `byte`, string or enum value built from literals, other constants, enum variants, static fields and operators. Constants are the only variables allowed at the top level, every function can see them (see [Scope](#scope)), and they can be used anywhere in the program, even above their declaration.

```nim
let WIDTH = 80
let HEIGHT = WIDTH - 20
let TITLE = "dusk"
let MODE = Mode.Fast

func area() -> int
    return WIDTH * HEIGHT
end
```

Any other value (a `let mut`, an array, a struct, a value computed by a call) is a variable and belongs inside a function, usually `main`.

## 3. Types

### Primitives

| Type     | Description                                   | Examples            |
| :------- | :-------------------------------------------- | :------------------ |
| `int`    | 64-bit signed integer                         | `42`, `-10`         |
| `byte`   | Unsigned 8-bit value (`0` to `255`)           | `'a'`, `'\n'`, `65` |
| `float`  | 64-bit floating-point number                  | `3.14`, `-0.5`      |
| `string` | Text in double quotes                         | `"Hello"`           |
| `buffer` | Fixed-size block of mutable bytes             | `buffer::new(16)`        |
| `bool`   | Boolean                                       | `true`, `false`     |
| `void`   | No value, used as a function return type      |                     |

Since v0.13, an `int` converts to `float` wherever a `float` is expected: a typed `let`, an assignment, an argument, a return value, a struct field, a union payload, an array item and a `catch` or `orelse` fallback (`let ratio: float = count`). An `int` or `?int` converts to `?float` the same way. An untyped array literal that mixes them is a `[]float` (`[1, 2.5]`). `to_float(x)` converts an `int` or `byte` explicitly (`let ratio = to_float(count)`).

A `float` never converts to `int` automatically (use `to_int(x)`, which truncates and can fail), a `byte` only converts to `float` with `to_float(x)`, and `[]int` doesn't convert to `[]float`. Ints larger than 2^53 lose precision as floats.

Strings contain valid UTF-8 text in double quotes. Invalid UTF-8 and missing closing quotes are compile errors. The runtime stores the UTF-8 bytes with an explicit byte length, so embedded NUL characters are preserved.

| Escape | Value |
| :----- | :---- |
| `\"` | Double quote |
| `\'` | Single quote |
| `\\` | Backslash |
| `\$` | Literal dollar sign |
| `\n` | Line feed |
| `\r` | Carriage return |
| `\t` | Tab |
| `\0` | NUL |
| `\u{...}` | Unicode scalar value written as one to six hexadecimal digits |

Unicode escapes accept values from `0` through `10FFFF`, excluding surrogates (`D800` through `DFFF`), and encode them as UTF-8. Unknown escapes, malformed Unicode escapes and invalid scalar values are compile errors. Escapes are decoded once during compilation.

```nim
io::println("Hello\nworld")
io::println("Say \"hello\"")
io::println("Moon: \u{1F319}")
io::println("A literal backslash: \\")
```

Strings can't be indexed with `s[i]`. Use `string::byte_at(s, i)` for the byte at a byte offset, or `string::char_at(s, i)` for the `i`-th character (see [String functions](#string-functions)). `for s: b, i do` loops over a string's bytes. Strings are immutable; use a buffer to change bytes.

```nim
let word = "olá"
io::println("${try string::byte_at(word, 0)}")   # 111 ('o')
io::println(try string::char_at(word, 2))   # á
for word: b, i do                # 4 bytes: "á" is two bytes in UTF-8
    io::println("$i: $b")
end
```

A double-quoted string must end on the line where it starts: a line break inside it is a compile error. Use `\n` or a multiline string instead. Line breaks inside a `${ ... }` interpolation are allowed, because they belong to the expression. There is no `+` for strings: join text with interpolation (`"$a$b"`), `string::concat(a, b)` or `string::join(parts)`.

### Bytes

A `byte` holds one value from `0` to `255`. A byte literal is one ASCII character or one escape in single quotes: `'a'`, `'\n'`, `'\''`, `'\u{41}'`. Empty literals, more than one character and non-ASCII characters (`'é'` is two bytes in UTF-8) are compile errors. Bytes print as numbers.

```nim
let comma = ','
let a: byte = 65           # an int constant that fits converts to byte
let n = 200
let b = to_byte(n) catch 0 # fails with ConversionError.Overflow outside 0..255
let digit = '7' - '0'      # arithmetic on bytes gives an int: 7
let code: int = comma      # a byte converts to int automatically

when comma
    'a'..'z' then io::println("lower") end
    ',' then io::println("comma") end
    else then io::println("other") end
end
```

- An int constant outside `0..255` (`let b: byte = 300`) is a compile error. `to_byte(x)` returns `ConversionError!byte` and fails with `ConversionError.Overflow` outside that range, so it needs `try` or `catch`.
- An int variable never converts to `byte` implicitly: use `to_byte(x)`. So `b = b + 1` on a `byte` variable is an error, because `b + 1` is an `int`.
- Bytes work as `when` subjects with literal and range cases (an `else` is required), as array indexes and as range-loop bounds.
- `?byte` works like `?int`. A `[]byte` is a regular array, using as much memory per item as `[]int`.

### Buffers

A `buffer` is a fixed number of mutable bytes, stored packed (one byte per item, unlike `[]byte`). `buffer::new(n)` creates one with `n` zero bytes (`n` can be `0`); its size never changes. It returns `BufferError!buffer`, failing with `BufferError.NegativeSize` for a negative `n`, so the call needs `try` or `catch`.

```nim
let mut data = try buffer::new(4)  # in a function that can fail (-> !T)
data[0] = 'h'                     # writes take a byte
data[1] = 105
let first = data[0]               # reads give a byte
io::println("${buffer::len(data)}")            # 4
io::println("$data")                        # [104, 105, 0, 0]

for data: b, i do
    io::println("$i: $b")
end

let bytes = buffer::from_string("olá")    # [111, 108, 195, 161]
let text = string::from_bytes(bytes) catch "not UTF-8"
```

- The type is written `buffer`: `let mut data: buffer = try buffer::new(4)`, `?buffer`, `[]buffer`, struct fields and parameters.
- Writing requires a `let mut` variable (or a mutable struct holding the buffer). A function that writes to a buffer argument takes it as `mut b: buffer`, and the caller must pass a mutable value (see [Mutable parameters](#mutable-parameters)). An index outside the buffer stops the program with a runtime error.
- `buffer::from_string(s)` copies a string's UTF-8 bytes into a new buffer. `string::from_bytes(b)` copies a buffer into a new string, and fails with `StringError.InvalidUtf8` when the bytes aren't valid UTF-8. Changing the buffer afterwards never changes the string.
- `==` on buffers compares identity: a buffer is only equal to itself.

### Multiline strings

Triple quotes write text across several lines. The text starts on the line after the opening `"""`, and the closing `"""` goes on its own line. The closing line's indentation is removed from every line, so the text can follow the code's indentation:

```nim
func usage(name: string) -> string
    return """
        Usage: $name <file>
          -h  show help
        """
end
# "Usage: dusk <file>\n  -h  show help"
```

- The line break after the opening `"""` and the one before the closing `"""` are not part of the string. To end the string with a line break, leave an empty line before the closing `"""`.
- Every line must start with the closing `"""` indentation; indentation beyond it is kept. A line indented less is a compile error. Lines that are empty or contain only spaces become empty lines.
- Only spaces can be written on the opening `"""` line after the quotes, and on the closing line before them.
- Escapes and interpolation work as in double-quoted strings. `"` and `""` need no escape; write `\"""` for three quotes.

### String interpolation

Use `$name` to insert a variable or `${expression}` to insert an expression:

```nim
let name = "Dusk"
let age = 13
io::println("Hello, $name! You are $age years old.")
let a = 2
let b = 3
io::println("Sum: ${a + b}")
io::println("Nested: ${"Hello, $name"}")
io::println("A literal dollar: \$name")
```

`$name` consumes a complete identifier, including underscores and digits after its first character. Use braces to separate a name from following text (`"${name}_suffix"`), access a member (`"${item.field}"`), index an array or call a function. A dollar sign that starts neither an identifier nor `${` stays literal, as in `"$5"`.

Expressions are evaluated and formatted exactly once, from left to right. Values of any type can be interpolated, including arrays, structs, unions, nullable values and `null` (see [how values are formatted](#stdio)), without adding a newline. This is also how to print a value that isn't a string: `io::println("$count")`. An empty `${}`, an unknown name or a `void` result is a compile error. Failing calls require `try` or `catch`, as elsewhere. Expressions can contain nested strings, interpolations, conditionals and comments; error locations refer to the original source.

Escaped dollars (`\$`) and dollars produced by Unicode escapes (`\u{24}`) remain literal and do not start interpolation.

### Arrays

Arrays are growable lists of a single type. The type is written `[]T`.

```nim
let numbers = [1, 2, 3]
let names: []string = ["Alice", "Bob"]
let mut empty: []int = []

io::println("${numbers[0]}")        # 1
io::println("${len(numbers)}")      # 3
append(empty, 4)
```

Indexing starts at 0. Reading past the end is a runtime error. `append` needs a mutable array.

### Maps

A map stores values by key. The type is written `map[K, V]`, with `K` the key type and `V` the value type.

```nim
let ages = {"ana": 31, "bob": 27}
let mut scores: map[string, int] = {}
scores["ana"] = 10

io::println("${ages["ana"]}")             # 31
io::println("${ages["zoe"]}")             # null
io::println("${ages["zoe"] orelse 0}")    # 0
```

- Keys can be `int`, `byte`, `bool`, `string`, enums or structs. Strings match by content. Structs match by identity: the key is that exact struct, so two `Point(x = 1, y = 2)` built separately are two different keys, and so is a `copy` of a struct.
- `{}` is an empty map. Like `[]`, it needs a written type: an annotation, a parameter, a field or a return type. Otherwise the entries decide: the first key and value give the types, and `int` values next to `float` ones become `float`s. The same constant key twice is a compile error.
- `m[k]` reads a value. The result is always nullable (`?V`): `null` when the key is missing. Read it with `orelse`, `if m[k]: v then` or `map::has`. A map with nullable values can hold `null`, and only `map::has` tells that apart from a missing key.
- `m[k] = v` adds an entry or replaces its value. It needs a mutable map, like changing an array.
- `m?[k]` reads from a nullable map, so lookups chain: `grid[1]?[2]` on a `map[int, map[int, string]]`.
- Entries keep the order they were added in, for loops, `map::keys` and `map::values`. Replacing a value keeps its place.
- `==` compares identity, and interpolating a map gives `<map>`.

The other map functions (`map::len`, `map::remove`, ...) are in [std/map](#stdmap). `map` is a built-in type name: a struct, enum or union can't be called `map`, but functions and variables can.

## 4. Structs

A struct groups named fields and the functions that work on them.

```nim
struct User
    id: string
    username: string
    auth_method = "DUSK"

    func display_name(self: @) -> string
        return self.username
    end

    func change_id(mut self: @, id: string) -> void
        self.id = id
    end
end
```

- A field needs a type, a default value, or both (`auth_method = "DUSK"` infers `string`).
- `@` means "the type being defined". A method takes `self: @` as its first parameter, or `mut self: @` to change the instance.

### Creating and using structs

Call the struct name like a function. Arguments can be named or positional, and fields with defaults can be left out.

```nim
let mut user = User(id = "1", username = "geromel")

io::println(user.username)
io::println(user.display_name())
user.change_id("100")
```

Methods can also be called through the type, passing the instance first: `User.display_name(user)`.

### Static fields

Fields declared with `let` belong to the type, not to an instance. They are constants, with the same rules as top-level [constants](#constants): an `int`, `float`, `bool`, `byte`, string or enum value known at compile time. They can't be `mut` and can't be assigned.

```nim
struct Api
    let host = "127.0.0.1"
    let port = 8080

    endpoint: string = ""

    func url(self: @) -> string
        return "${Api.host}:${Api.port}${self.endpoint}"
    end
end

io::println(Api.host)
```

Members should be written in this order: static fields, then instance fields, then methods. The same order applies to enums and unions. The compiler doesn't check the order yet.

### Nested structs

A field can define a struct inline. Nested structs can only have fields, no methods or static fields.

```nim
struct Customer
    name: string
    address: struct
        city: string
        country: string
    end
end

let c = Customer(name = "Ana", address = @(city = "Recife", country = "BR"))
io::println(c.address.city)
```

### Anonymous initialisation

`@(...)` builds a value of the struct type expected at that point: an annotated `let`, an argument, a return value or a field.

```nim
struct Point
    x: int
    y: int
end

func show(p: Point) -> void
    io::println("${p.x}")
end

let a: Point = @(x = 1, y = 2)
show(@(x = 3, y = 4))
```

`let b = @(x = 1, y = 2)` without an annotation creates a new anonymous struct type with those fields. It can still be used where a named struct is expected (`show(b)`), as long as it has the same fields, in the same order, of the same types. A field set to `null` (`@(next = null)`) only holds `null`: it fits a nullable field, and reads as `null`. A field set to an empty `[]` needs a type, so build that struct where its type is known (`let c: Cart = @(items = [])`).

## 5. Enums

An enum is a fixed set of named values.

```nim
enum Color
    Red
    Green
    Blue
end

let c = Color.Green
```

Variants are numbered from 0 in order. You can give every variant an explicit `int` value instead:

```nim
enum HttpStatus
    Ok = 200
    NotFound = 404
    ServerError = 500
end
```

Enums can have static fields and methods, using the same rules as structs:

```nim
enum Direction
    let DEFAULT = Direction.North

    North
    South

    func is_north(self: @) -> bool
        return self == Direction.North
    end
end

io::println("${Direction.DEFAULT.is_north()}")
```

- Enums support `==` and `!=`, but not `<`, `>` or arithmetic.
- An enum is a different type from `int`: `Color.Red == 0` is a type error.
- Interpolating an enum gives its number, not its name.

## 6. Unions

A union holds exactly one of several variants, and each variant carries a value (its payload).

```nim
union Response
    success: string
    failure: string
end

let ok = Response(success = "saved")
let failed = Response(failure = "not found")
```

Build a union by calling its name with exactly one named variant. Where a union type is expected (annotated `let`, assignment, argument, return value, field or array item), `@(variant = value)` works too:

```nim
func fetch(id: int) -> Response
    if id == 1 then
        return @(success = "found")
    end
    return @(failure = "missing")
end
```

### Reading variants

Reading a variant gives a nullable (`?T`): its payload when it's the active variant, `null` otherwise. Capture it with `if`:

```nim
if ok.success: message then
    io::println(message)
end
```

To handle every variant, use [`when`](#when):

```nim
when fetch(2)
    success: message then io::println(message) end
    failure: reason then io::println(reason) end
end
```

### Changing a union

To change a union, assign a new one. This works for `let mut` variables, struct fields and array items:

```nim
let mut status = Response(failure = "pending")
status = Response(success = "done")
status = @(failure = "timeout")
```

Writing to a single variant (`status.success = "x"`) is an error, and a `mut` parameter can't be reassigned (see [Mutable parameters](#mutable-parameters)), so a function that switches a union to another variant returns the new one:

```nim
func resolve(r: Response) -> Response
    return @(success = "resolved")
end

status = resolve(status)
```

A payload that is an array, buffer, struct or union can still be changed in place, through a `mut` capture of a mutable union (`if shape.poly: mut points then ... end`, or a `when` case like `poly: mut points then`). A union parameter can be `mut` for that, including `mut self`.

### Methods and static fields

These work like they do for structs:

```nim
union Shape
    let sides_of_square = 4

    circle: int
    square: int

    func area(self: @) -> int
        when self
            circle: r then return 3 * r * r end
            square: side then return side * side end
        end
        return 0
    end
end

io::println("${Shape(square = 3).area()}")
```

- `==` and `!=` can't compare unions: check the variant with `when` or `if u.variant: value then ... end`.
- Interpolating a union gives `<union>`.

## 7. Nullables

A `?` before a type allows `null` as a value. It works for primitives, enums, structs, unions, arrays (`?[]int`) and error sets. `[]?int` is an array whose items can be null.

```nim
let mut name: ?string = null
name = "Dusk"

let mut count: ?int = 5
count = null
```

A nullable can't be used directly as its inner type. Unwrap it first.

`==` and `!=` are the exception: nullable primitives and enums (`?int`, `?float`, `?bool`, `?byte`, `?Enum`) compare by value. `count == 5` is true when `count` holds `5`, two nulls are equal and `null` is never equal to a value. Other nullables (`?[]int`, `?Struct`) compare like their inner type: identity, with `null` equal only to `null`. `?string` compares by content (see [Comparison and logic](#comparison-and-logic)).

### Capture

`if value: name then` runs the block only when the value isn't null, with `name` bound to the inner value. An `else` block is allowed.

```nim
if name: n then
    io::println("$n")
else
    io::println("no name")
end
```

`if value: mut name` captures the value so it can be changed through `name`: setting a field, an item or appending changes the original. It needs a mutable subject (a `let mut` variable or a path from one), and only works for arrays, maps, buffers, structs and unions: a `mut` capture of a primitive, string or enum is an error. The capture itself can't be reassigned (`name = other` is an error), since that would only change the local name.

### Nullable fallback

`value orelse fallback` unwraps a non-null value, or evaluates the fallback when the value is null:

```nim
let count: ?int = null
let selected = count orelse 10

let primary: ?int = null
let secondary: ?int = 4
let total = primary orelse secondary orelse 0
```

The left operand is evaluated once. The fallback runs only when it is needed; `false`, zero and empty arrays are non-null values. The left operand must be nullable or `null`. A nullable fallback keeps the result nullable; adding a non-null final fallback unwraps the chain.

Result types follow conditional-expression rules, including numeric promotion (`?int orelse float` produces `float`). Expected types apply to contextual initializers; otherwise the left payload type supplies context for anonymous initializers and empty arrays. Incompatible types and `void` fallbacks are errors.

`orelse` has the same precedence as `catch`, below pipes, logical operators, comparisons and arithmetic. Both group right-to-left:

```nim
let number = primary orelse secondary orelse 0
let is_ready = maybe_ready orelse first or second
let result = maybe_number orelse fetch_number() catch 0
```

These group as `primary orelse (secondary orelse 0)`, `maybe_ready orelse (first or second)` and `maybe_number orelse (fetch_number() catch 0)`. To compare a coalesced result, write `(maybe_number orelse 0) == 0`. To pipe the selected result, write `(maybe_number orelse 0) |> process()`.

Fallbacks are expressions, including `if` and `when` expressions. Direct `orelse return`, `orelse break` and `orelse continue` forms are not supported. `orelse` is a reserved keyword.

### Optional chaining

`?.` reads a field, variant or method of a nullable value. If any link in the chain is null, the result is `null`. The result is always nullable.

```nim
struct Country
    name: string
end

struct User
    country: ?Country
end

let user = User(country = null)
io::println("${user.country?.name}")   # null
```

`?[` reads an item from a nullable array or map the same way: `maybe_items?[0]`, `grid[1]?[2]`.

Using `?.` or `?[` on a value that can't be null is an error. Assigning through them (`user.country?.name = "x"`) is an error too: unwrap the value first with `if user.country: mut country then`.

## 8. Operators

### Precedence

From lowest to highest:

| Operators                       | Description           |
| :------------------------------ | :-------------------- |
| `catch` `orelse`                | Error / nullable fallback |
| `\|>`                           | Pipe                  |
| `or`                            | Logical or            |
| `and`                           | Logical and           |
| `==` `!=` `<` `<=` `>` `>=`     | Comparison            |
| `..`                            | Inclusive range       |
| `+` `-`                         | Addition, subtraction |
| `*` `/` `//` `%`                | Multiplication, division, modulo |
| `-x` `!x` `try x`               | Negation, logical not, try |
| `x.y` `x?.y` `x[i]` `f()`       | Access, index, call   |

Use parentheses to change the order.

### Arithmetic

| Operator | Result                                                        |
| :------- | :------------------------------------------------------------ |
| `+` `-` `*` `%` | `int` if both sides are `int`, otherwise `float`       |
| `/`      | Always `float`, even for two `int`s (`3 / 2` is `1.5`)        |
| `//`     | Always `int`: divides, then truncates (`7 // 2` is `3`)       |

An `int` mixed with a `float` is promoted to `float` (see [Primitives](#primitives) for the other places an `int` becomes a `float`).

`%` takes the sign of the right side (`7 % -2` is `-1`, `-7 % 2` is `1`), while `//` truncates toward zero (`-7 // 2` is `-3`). `//` turns `float` sides into `int`s first. An `int` `//` or `%` by `0` stops the program with a runtime error. So does any `int` result that doesn't fit in 64 bits (`9223372036854775807 + 1`, the smallest `int` `// -1` or negated), and a `float` side of `//` that's `nan`, infinite or outside the `int` range. `/` by `0` follows floating point: `1 / 0` is `inf`, and `%` with `float` sides gives `nan` for a `0` right side.

### Comparison and logic

`==`, `!=`, `<`, `<=`, `>`, `>=` compare numbers. `==` and `!=` also work on `bool`, enums and errors. `and`, `or` and `!` work on `bool`. `and` and `or` only evaluate their right side when the left side doesn't decide the result: in `false and f()` and `true or f()`, `f` isn't called. So `i < len(items) and items[i] > 0` never reads past the end.

`==` and `!=` only compare values of the same type: `true == 1`, `1 == "a"` and a `Point` with a `Size` are compile errors, since they could never be equal. Numbers are the exception: an `int`, `float` or `byte` compares with any other number (`1 == 1.0`). A nullable compares with `null` or with its inner type, and `x == null` is an error when `x` can't be null.

On arrays, maps, structs and buffers, `==` and `!=` compare identity: two separately built `[1, 2]` are not equal, and a value is equal to its `copy` only if it's the same object. There are no content-comparison functions for them yet. Unions and functions can't be compared with `==`.

Strings compare by content: `name == "Dusk"` is true when `name` holds that text, wherever it was built. A `?string` compares with `null` or with a `string` (two nulls are equal, `null` is never equal to a string).

### Assignment

`=` assigns. There are no compound assignment operators: write `x = x + 1`, not `x += 1`.

## 9. Control Flow

### If

```nim
if x > 10 then
    io::println("big")
elif x > 0 then
    io::println("small")
else
    io::println("zero or less")
end
```

The condition must be a `bool`. For nullable capture (`if value: name then`), see [Nullables](#capture). Both forms can also [produce a value](#conditional-expressions).

### When

`when` runs the case that matches a value. It works on `int`, `byte`, `float`, `string`, enums, unions, error sets and their nullable versions. Each case is `pattern then ... end`, and the whole `when` ends with `end`. Cases never fall through.

```nim
when color
    Color.Red then io::println("red") end
    Color.Green, Color.Blue then
        io::println("cold")
    end
end

when code
    200 then io::println("ok") end
    404, 410 then io::println("gone") end
    else then io::println("other") end
end

when command
    "start", "run" then io::println("go") end
    "stop" then io::println("halt") end
    else then io::println("unknown") end
end
```

Union cases name a variant and capture its payload. Use `_` to ignore it:

```nim
when shape
    circle: r then io::println("${3 * r * r}") end
    square: _ then io::println("square") end
end
```

`variant: mut name` captures a payload so it can be changed in place, with the same rules as `if value: mut name`: the subject must be mutable, the payload must be an array, buffer, struct or union, and the capture can't be reassigned:

```nim
when shape
    poly: mut points then append(points, Point(x = 0, y = 0)) end
    circle: _ then end
end
```

A nullable subject can have a `null` case. In the other cases the value is already unwrapped:

```nim
when maybe_shape
    null then io::println("none") end
    circle: r then io::println("$r") end
    square: s then io::println("$s") end
end
```

Rules:

- Enum and union cases must cover every variant, and `null` for a nullable subject, unless there's an `else`. An `else` that can never run is an error.
- `int`, `byte`, `float` and `string` subjects always need an `else`, even if ranges appear to cover every value.
- Int cases can be values known at compile time or inclusive ranges of them (`1..5`): literals (`1`, `-1`), [constants](#constants), static fields (`Grid.SIZE`) and `+`, `-`, `*`, `//`, `%` on them (`LOW + 1`, `-Grid.SIZE`). A case can combine them (`1..5, 8, LOW..HIGH then ... end`). Variables and calls aren't allowed, and a case value that overflows or divides by zero is a compile error.
- Float cases must be inclusive ranges (`0.0..1.0`), not individual values. Bounds must be finite numeric literals; int bounds are promoted to float. NaN and infinities reach `else`.
- A case range must start at or below its end. Ranges and individual cases cannot overlap, including within a comma-separated case. Inclusive ranges `0.0..1.0` and `1.0..2.0` overlap at `1.0`.
- Enum cases use the full name (`Color.Red`).
- A value or variant can only appear in one case.
- String cases must be string literals without interpolation (`"stop"`, `"\u{41}"`). They compare by content, like `==`. Two literals with the same text after escapes (`"A"` and `"\u{41}"`) are the same case.
- `break`, `continue` and `return` in a case act on the enclosing loop or function.

Range matching uses the same rules in statements and expressions:

```nim
let category = when temperature
    0.0..10.0 then
        "cold"
    end

    10.1..25.0 then
        "mild"
    end

    else then
        "other"
    end
end
```

For a negative first case, parenthesize its lower bound (`(-5)..(-1)`) so the minus is not parsed as part of the subject expression.

### Conditional expressions

`if` and `when` can produce values in declarations, assignments, arguments, returns, array items and other expressions. Each selected branch produces its final expression:

```nim
let size = if is_ready then 10 else 0 end

func sign(n: int) -> return if n > 0 then 1 elif n < 0 then -1 else 0 end

let description = when code
    200 then "ok" end
    404 then "missing" end
    else then "other" end
end
```

A branch can run statements before its final expression. Nested `if` and `when` expressions work too:

```nim
let total = if is_ready then
    let base = 10
    base * 2
else
    0
end

let count = if maybe_count: captured then captured else 0 end
```

Rules:

- An `if` used for its value requires `else`. A `when` uses the same exhaustiveness rules as its statement form.
- Every branch that finishes normally must end in a value expression. An empty branch, an assignment, a declaration or a `void` call cannot supply that value.
- `return` exits the enclosing function; `break` and `continue` act on the enclosing loop. A path that exits this way does not need to supply a branch value.
- Only the selected branch runs. Conditions are tested once each as needed; a `when` subject or nullable-capture subject is evaluated once.
- Branch locals and captured names are visible only in their own branch.
- An enclosing expected type applies to every branch, including anonymous initializers and empty arrays. Otherwise compatible branch types determine the result: `int` with `float` produces `float`, and a value with `null` produces its nullable type. Numeric promotion also works for nullable numeric results. Incompatible types are errors; numeric promotion does not extend to array items.
- Used as statements, `if` and `when` keep their existing behavior. Statement `if` does not require `else`.

### Loops

`for condition do ... end` repeats while the condition is `true`. `for true do` loops until a `break`.

```nim
let mut i = 0
for i < 5 do
    io::println("$i")
    i = i + 1
end
```

Arrays can be traversed with a capture loop. The first capture is the item; the optional second capture is its zero-based `int` index:

```nim
let items = [10, 20, 30]
for items: item, index do
    io::println("$index: $item")
end

for items: item do
    io::println("$item")
end
```

Use `_` to ignore either capture, as in `for items: _, index do ... end`. Captures are immutable and visible only inside the loop body. They can shadow outer names, which become visible again after the loop. Assignments and mutations through a capture are errors. `for items: mut item do` makes the item capture mutable, so changes through it (fields, items, `append`) change the array's element. It needs a mutable source and items that are arrays, maps, buffers, structs or unions, and the capture can't be reassigned. To replace elements, or to change items of other types, use a mutable source array and its index.

The source can be any expression returning a non-nullable array, buffer or string. Unwrap nullable values first. Buffers and strings are traversed byte by byte: the item is a `byte` and the index its byte offset. The loop evaluates the source and its length once, retains that array, and reads each item when its iteration begins. Empty arrays run zero iterations. Appended items are not visited; changes to upcoming elements are visible. Reassigning the source variable does not change the array being traversed. Heap values retain their ordinary shared identity; iteration does not clone them.

Maps are traversed the same way, in the order their entries were added. The first capture is the value and the second the key:

```nim
let ages = {"ana": 31, "bob": 27}
for ages: age, name do
    io::println("$name: $age")
end
```

The map can change inside the loop: an entry removed before the loop reaches it is skipped, and an entry added during the loop is visited at the end. `mut` captures work as for arrays.

A range keeps its lower and upper bounds separate. Ranges are supported directly in capture loops and `when` cases; they cannot be stored in variables, passed as arguments or returned as values yet.

Integer ranges use the same capture syntax, without allocating an array:

```nim
for 1..5: value, index do
    io::println("$index: $value")
end
```

`1..5` includes both endpoints, producing `1`, `2`, `3`, `4`, `5`. The optional index counts from zero. Bounds can be integer expressions, evaluated once from left to right before the loop starts. Arithmetic binds more tightly than `..`, so `start + 1..stop * 2` uses those arithmetic results as its bounds.

Iteration advances by one. Equal endpoints produce one item; a start greater than the end produces no items. The loop stops at its upper bound without incrementing past it, including at the largest `int`. Float range iteration, descending steps and open-ended ranges are not supported.

`break` leaves the innermost loop, and `continue` jumps to its next iteration. Both are errors outside a loop. In a capture loop, `continue` advances to the next item.

```nim
let mut n = 0
for true do
    n = n + 1
    if n % 2 == 0 then
        continue
    end
    if n > 7 then
        break
    end
    io::println("$n")   # 1, 3, 5, 7
end
```

## 10. Functions

```nim
func add(a: int, b: int) -> int
    return a + b
end

func greet(name: string) -> void
    io::println(name)
end
```

Parameter types and the return type are required. A function that returns nothing uses `-> void`.

A function with a return type must return on every path. A path returns if it has a `return`, an `if` whose branches all return (including `else`), or a `when` whose cases all return and that has an `else` or covers every variant.

```nim
func sign(x: int) -> int
    if x > 0 then
        return 1
    elif x < 0 then
        return -1
    else
        return 0
    end
end
```

Loops don't count, even `for true`: add a `return` after the loop.

### Inline return

A function whose body is a single expression can be written on one line. Its return type is inferred.

```nim
func square(n: int) -> return n * n
```

### Default values

A parameter with a default value can be left out of a call. Its type can be inferred from the default.

```nim
func scale(x: int, factor = 2) -> return x * factor

io::println("${scale(10)}")      # 20
io::println("${scale(10, 3)}")   # 30
```

### Named arguments

Arguments can be passed by name. A call with only named arguments can list them in any order:

```nim
io::println("${scale(factor = 5, x = 1)}")   # 5
```

A call can also start with positional arguments and continue with named ones. Positional arguments fill the parameters in order; once an argument is named, the ones after it must be named too, in any order. Parameters with a default value can be left out.

```nim
func delete(path: string, recursive: bool = false, force: bool = false) -> void

delete("build", recursive = true)                 # force keeps its default
delete("build", force = true, recursive = true)   # named ones in any order
delete("build", true, force = true)               # positional ones first
delete(force = true, path = "build")              # only named arguments
```

A positional argument after a named one is a `PositionalAfterNamed` error (`delete("build", recursive = true, true)`). A name that isn't a parameter is `UnknownArgumentName`, and a parameter given twice, by position and by name or by name twice, is `ArgumentGivenTwice`. Calls through a function value only take positional arguments (`NamedArgumentsOnValue`).

### Mutable parameters

Parameters are immutable. Arrays, buffers, structs and unions can be taken as `mut` to change the caller's value through them: items, fields, `append`, and union payloads through `mut` captures. The argument must then be mutable: a `let mut` variable or a field or item of one (`grow(config.items)`), or a new value such as `[1, 2]`. Primitive, string and enum parameters can't be `mut`.

A generic `T` parameter can be `mut` too (`mut item: T`). The argument then has to be mutable only when `T` is filled with an array, buffer, struct or union; an `int`, string or enum goes through as is. A function that stores a `T` argument somewhere mutable, like `append`, needs it as `mut`, since the stored value could be changed through that place later:

```nim
func push[T](mut list: []T, mut item: T) -> void
    append(list, item)
end

let n = 3
let frozen = [1, 2]
let mut numbers: []int = []
let mut lists: [][]int = []
push(numbers, n)        # fine: an int can't be changed
push(lists, frozen)     # error: 'frozen' is not mutable
push(lists, [1, 2])     # fine: a new value
```

A `mut` parameter can't be reassigned (`list = []` is an error): the new value would only change the local name, not the caller's variable. To replace a value, return the new one and assign it at the call site, or change it through a `mut self` method on a struct that holds it.

```nim
func push_one(mut list: []int) -> void
    append(list, 1)
end

let mut items = [10, 20]
push_one(items)
io::println("$items")   # [10, 20, 1]
```

### Pipe

`value |> f(args)` calls `f(value, args)`, so nested calls read left to right. The right side must be a call, including its parentheses.

```nim
func is_even(x: int) -> return x % 2 == 0

let result = 2
    |> add(1)
    |> is_even()

assert(result == false)
```

The right side can also be a `try` call: `x |> try f()` calls `f(x)` and passes its error up, so a chain of failing steps reads left to right:

```nim
func run(text: string) -> ParseError!int
    return try parse(text) |> try half() |> add(1)
end
```

`try` binds tighter than `|>`, so `try x |> f()` means `(try x) |> f()`. To `try` the result of a whole pipe, write `x |> try f()` or `try (x |> f())`.

### Scope

A function can see its parameters, its own variables, other functions and types, static fields and top-level [constants](#constants). A function declared with a name can't see the variables of other functions, including `main`: state it reads or changes must be passed to it, with `mut` to change it. [Lambdas](#lambdas) are the exception: they can use the variables of the functions around them (see [Closures](#closures)). Default values (field and parameter defaults) can only use constants.

```nim
let LIMIT = 3          # a constant: visible everywhere

struct Stats
    count = 0
end

func bump(mut stats: Stats) -> void
    if stats.count < LIMIT then stats.count = stats.count + 1 end
end

func main() -> void
    let mut stats = Stats()
    bump(stats)
    io::println("${stats.count}")  # 1
end
```

Functions can be called before they are declared.

### Function values

A function is a value. Its name without parentheses is the function itself, which can be stored in a variable, passed as an argument, put in an array or a struct field, and returned from another function. Calling a value works like calling the function:

```nim
func add(a: int, b: int) -> return a + b
func sub(a: int, b: int) -> return a - b

func apply(f: func(int, int) -> int, a: int, b: int) -> int
    return f(a, b)
end

let op = add
io::println("${op(1, 2)}")              # 3
io::println("${apply(sub, 5, 3)}")      # 2
let ops = [add, sub]
io::println("${ops[1](10, 4)}")         # 6
io::println("$op")                    # <func add>
```

A call through a value passes every argument, in order: default values and named arguments belong to the declaration, so they don't carry over to a value.

A struct field that holds a function is called like a method: `button.on_click()`. A struct can't have a field and a method with the same name, so `x.name()` always has one meaning.

A generic function becomes a value when a function type fills its slots: `let same: func(int) -> int = id` makes `id[int]`. Without one, `let f = id` is an error. Builtins can't be used as values: call the builtin from a lambda or a function and use that instead.

Functions can't be compared with `==` or `!=`. Interpolating a named function gives `<func add>`, and a lambda gives `<func>`.

### Function types

A function type lists the parameter types and the return type: `func(int, int) -> int`. A parameter that the function takes as `mut` is written `mut` in the type too (`func(mut []int) -> void`), and a function that can fail names its error set (`func(string) -> ParseError!int`). An error set can't be left to inference (`-> !int`) in a type. `?func() -> void` is a function that may be missing, unwrapped like any nullable:

```nim
let reader: func(string) -> ParseError!int = parse
let size = reader("") catch -1

let mut handler: ?func() -> void = null
handler = func() -> void
    io::println("handled")
end
if handler: run then
    run()
end
```

A function fits a function type only when everything matches: the parameter types, which parameters are `mut`, the return type and the error set. `add` (`-> int`) doesn't fit `func(int, int) -> float`, and `func(xs: []int)` doesn't fit `func(mut []int) -> void`.

### Lambdas

A lambda is a function without a name, written where a value goes. Like a declaration, it has an inline form that returns one expression and a block form that ends with `end`. The block form can name its return type after `->`:

```nim
let square = func(x: int) -> return x * x
let describe = func(x: int) -> string
    if x > 10 then
        return "big"
    end
    return "small"
end

io::println("${apply(func(a: int, b: int) -> return a * b, 6, 7)}")    # 42
```

Where a function type is expected (an annotation, a parameter, a field, an array item, a return value, the fallback of `orelse` on a `?func(...)`), the lambda's parameters can leave out their types and take them from that function type. A `mut` parameter is still written `mut`, matching the function type:

```nim
let double: func(int) -> int = func(x) -> return x * 2
io::println("${apply(func(a, b) -> return a - b, 9, 4)}")    # 5
```

In a call to a generic function, the other arguments fill the slots first, the lambda's parameters take their types from them, and what the lambda returns can fill the slots that are left:

```nim
# signatures only: keep returns the items ok accepts, map_to the results of f
func keep[T](items: []T, ok: func(T) -> bool) -> []T
func map_to[T, U](items: []T, f: func(T) -> U) -> []U

io::println("${keep([1, 5, 12], func(x) -> return x > 2)}")            # [5, 12]
io::println("${map_to(["a", "bbb"], func(s) -> return string::len(s))}")   # [1, 3]: U is int
```

A lambda can also leave out its return type. Where a function type is expected, it takes the return type from it. Otherwise its body decides: every `return` must give the same type, and a lambda that never returns a value returns `void`. Using `try`, or returning an error, makes the lambda fail with the errors it can return, as `-> !T` does for a declaration:

```nim
let describe = func(x: int)
    if x > 10 then
        return "big"
    end
    return "small"
end

let people = map_to([1, 2], func(n)
    let name = "user ${n}"
    return @(id = n, name = name)
end)
io::println("${people[1].id}")    # 2

let sum_all = func(items: []string)
    let mut total = 0
    for items: item do
        total = total + try string::to_int(item)
    end
    return total
end
io::println("${try sum_all(["1", "2"])}")          # 3
io::println("${sum_all(["1", "x"]) catch -1}")     # -1
```

Returns of different types are an error (`return 1` in one place and `return "one"` in another), and so are `return user` and `return null` in one lambda: name the type (`-> ?User`) for that. Named functions always name their return type, unless they use the inline form.

A lambda without a function type to take its param types from needs them written: `let f = func(x) -> return x` is an error. Its parameters can't have default values, and `break` and `continue` inside it only see its own loops.

A lambda can't call itself through the name it's being assigned to (`let fact = func(n: int) -> int ... fact(n - 1) ... end` is an error): declare a function with a name to recurse.

### Closures

A lambda can use the variables of the functions around it. It shares them with that function instead of copying them: changing a `let mut` variable inside the lambda changes it for the function, and the other way around.

```nim
let limit = 2
let mut total = 0
let add = func(x: int) -> void
    if x > limit then
        total = total + x
    end
end
add(5)
add(7)
io::println("$total")    # 12
total = 0
add(3)
io::println("$total")    # 3
```

A lambda keeps its variables after the function that made it returns, so each call below makes a separate counter:

```nim
func make_counter() -> func() -> int
    let mut count = 0
    return func() -> int
        count = count + 1
        return count
    end
end

let next = make_counter()
let other = make_counter()
io::println("${next()}")     # 1
io::println("${next()}")     # 2
io::println("${other()}")    # 1
```

Each pass of a loop has its own loop variable, so lambdas made in a loop see different values:

```nim
let mut steps: []func() -> int = []
for 1..3: i do
    append(steps, func() -> return i * 10)
end
io::println("${steps[0]()} ${steps[1]()} ${steps[2]()}")    # 10 20 30
```

The usual mutability rules apply to what a lambda uses: a `let` variable or a parameter without `mut` can be read but not changed, and a `let mut` one can be reassigned or changed through. A lambda inside another lambda can use the variables of every function around it.

## 11. Generics and Traits

A **type slot** is a placeholder for a type, written in brackets after a function, struct or union name. Each use fills the slots with real types, and the compiler makes one copy of the declaration per set of types.

```nim
func first[T](xs: []T) -> T
    return xs[0]
end

io::println("${first([1, 2])}")         # first[int]
io::println("${first(["a", "b"])}")     # first[string]
```

### Filling slots

A call fills its slots from the arguments. The first argument that mentions a slot fills it, and the others must convert to that type: `both(2.5, 1)` makes `T` a `float`, while `both(1, 2.5)` is an error at `2.5`.

Slots the arguments leave open come from the type the result is expected to have: a variable's annotation, a return type, a parameter or a field.

```nim
func empty[T]() -> []T
    return []
end

let xs: []int = empty()     # empty[int]
```

A slot that nothing fills is an error (`Can't work out 'T' for 'empty'`). There's no way to write the types in a call, like `empty[int]()`. A generic function used as a value takes its slots from the function type it's used as (see [Function values](#function-values)).

Lambda arguments are checked after the other arguments: their parameters take their types from the slots filled so far, and their result can fill the rest (see [Lambdas](#lambdas)).

A slot can't be `void` or a type that can fail. Passing `parse`, which returns `ConversionError!int`, for `f: func(T) -> U` is an error at the call (`'U' can't be 'ConversionError!int'`), and so is a lambda that uses `try`: take the errors through an [error slot](#error-slots) instead. A caught error value fills a slot like any other value (`id(e)` after `catch e then`).

### Generic structs and unions

Structs and unions take slots too. In types they're written with their arguments, `Stack[int]`. When building one, the slots come from the field or variant values, or from the expected type.

```nim
struct Stack[T]
    items: []T

    func push(mut self: @, mut item: T) -> void
        append(self.items, item)
    end
end

union Result[T, E]
    ok: T
    err: E
end

let mut numbers = Stack(items = [1, 2])          # Stack[int]
numbers.push(3)
let mut names: Stack[string] = @(items = [])
let parsed: Result[int, string] = @(ok = 42)
```

Each filled type is its own type: a `Stack[int]` isn't a `Stack[float]`. Recursive types work (`next: ?Node[T]`), and static fields are shared by every copy (`Stack.MAX`).

### Checked where written

A generic body is checked once, where it's written, with `T` standing for any type. Only what works for every type is allowed: `T` can be stored, passed, returned, put in arrays, interpolated and given to `copy` and `append`. Arithmetic, comparisons and method calls on `T` need a constraint (below). A `T` param can't have a default value; it can be `mut` (see [Mutable parameters](#mutable-parameters)).

Each copy is checked again with its real types. An error there names the copy and the call that made it:

```
error: in first[[]int] (created at line 7): ...
```

### Type lists

A slot can be limited to some primitives with `|`. Members are `int`, `float`, `byte`, `string` and `bool`. A `trait` with `=` names a list, and lists can include other named lists.

```nim
trait Number = int | float
trait Scalar = Number | string | bool

func sum[T: Number](xs: []T) -> T
    let mut total: T = 0
    for xs: x do
        total = total + x
    end
    return total
end

io::println("${sum([1, 2, 3])}")        # 6
io::println("${sum([0.5, 1.5])}")       # 2
sum(["a"])                  # error: 'string' doesn't fit T: Number
```

Each operator on `T` is checked for every member with the usual rules. It's allowed when every member supports it, and its type is:

- the type every member gives, if they all give the same one: `x / 2` is a `float`, `a < b` a `bool`
- otherwise `T`, when each member gives itself back: `a + b` on `int | float`
- otherwise an error: on `byte | float`, `a + b` gives `int` for `byte` and `float` for `float`

A value converts to or from `T` when it converts for every member: `let total: T = 0` works for `Number` (`0` fits `int` and `float`), `let x: T = 2.5` doesn't. `==` and `!=` on `T` compare each member like they do outside a generic (strings by content). `x == null` works on any `?T`.

### Error slots

A slot marked `: error` holds an error set. Written before `!`, it lets a generic function take functions that can fail and pass their errors on:

```nim
func map[T, U, E: error](items: []T, f: func(T) -> E!U) -> E![]U
    let mut out: []U = []
    for items: item do
        append(out, try f(item))
    end
    return out
end

io::println("${map(["a", "bbb"], func(s) -> return string::len(s))}")    # [1, 3]
io::println("${try map(["1", "2"], parse)}")                         # [1, 2]
io::println("${map(["1", "x"], parse) catch []}")                    # []
```

`E` takes the error set of the function passed in: `ConversionError` for `parse`. When that function can't fail, `E` is `none`, the error set with no errors, and `none!U` is just `U`: that `map` can't fail, so its call takes no `try`, and inside it `try f(item)` is a plain call.

- A slot written before `!` must be marked: `func run[T, E](f: func() -> E!T)` is an error.
- Only error sets fill an error slot (`'E' is an error slot, so it must be an error set, not 'int'`), and `none` only fills error slots.
- A function with an error slot fails only with `E`: `try string::to_int(s)` in its body is an error (`'ConversionError.Invalid' is not in error E`). Catch other errors there.
- Two functions passed for the same `E` must fail with the same set. One that can't fail doesn't fit where another one fills `E` with `ConversionError`.

`catch e then` on a call that fails with `E` gives `e` the type `E`. It can be returned, passed on, stored (`?E`, `[]E`) or printed, but not matched with `when`, since its errors depend on the call:

```nim
func first_error[T, E: error](items: []T, f: func(T) -> E!int) -> ?E
    for items: item do
        f(item) catch e then
            return e
        end
    end
    return null
end
```

Structs and unions take error slots too, for a function kept in a field. `none` can be written in a type to say the function can't fail:

```nim
struct Job[T, E: error]
    run: func() -> E!T

    func go(self: @) -> E!T
        return try self.run()
    end
end

let later = Job(run = func() -> return try parse("8"))
io::println("${try later.go()}")                                # 8
let ready: Job[int, none] = @(run = func() -> return 1)
io::println("${ready.go()}")                                    # 1
```

### Traits

A trait lists methods a type must have. Its methods are signatures without a body, take `self: @` or `mut self: @` first, and `@` stands for the type that fits the trait. `[T: Shape]` lets the generic code call them.

```nim
trait Shape
    func area(self: @) -> float
    func scale(mut self: @, by: float) -> void
end

struct Circle
    r: float

    func area(self: @) -> float
        return 3.14 * self.r * self.r
    end

    func scale(mut self: @, by: float) -> void
        self.r = self.r * by
    end
end

func total_area[T: Shape](shapes: []T) -> float
    let mut total = 0.0
    for shapes: s do
        total = total + s.area()
    end
    return total
end

func grow[T: Shape](mut shape: T) -> void
    shape.scale(2.0)
end
```

A struct, enum or union fits a trait when it has each of its methods with the same parameter types and return type (parameter names don't matter). A plain `self` method can stand in for a `mut self` one, not the other way around. There's no `implements` declaration: the check happens where a type fills the slot, in a call, a field or an annotation.

```
error: 'Plain' doesn't fit T: Shape: it has no method 'area'
error: 'Wrong' doesn't fit T: Shape: its 'area' is (self: Wrong) -> int, the trait wants (self: Wrong) -> float
```

`+` combines traits, when declaring one or in a constraint. A trait that only combines others has an empty body.

```nim
trait Named
    func name(self: @) -> string
end

trait Item: Shape + Named end

func describe[T: Shape + Named](x: T) -> string
    return "${x.name()}: ${x.area()}"
end
```

Two traits can share a method if its signature is the same; different signatures are an error. A trait method that can fail names its error set (`func parse(self: @) -> ParseError!int`), and calls to it on `T` use `try` and `catch` as usual. A type list can't be combined with a trait (`Number + Shape`), since primitives have no methods.

Traits only exist at compile time: a trait isn't a type, so `let s: Shape = ...` is an error. To keep different types in one array, use a [union](#6-unions).

## 12. Error Handling

A function that can fail says so in its return type. Errors are values: the caller has to handle them, and nothing is thrown or unwound.

### Error sets

An error set lists the errors a function can return. Errors have no payload.

```nim
error ParseError
    Empty
    BadDigit
end
```

An error is written like an enum variant (`ParseError.Empty`). It can be stored in variables, fields, arrays and nullables (`?ParseError`), passed around, and compared with `==` and `!=`, also against errors from other sets. Interpolating an error gives its number.

### Functions that can fail

`E!T` means "returns a `T`, or fails with an error from `E`". `return` with a `T` succeeds, `return` with an error fails.

```nim
func parse_digit(n: int) -> ParseError!int
    if n < 0 then
        return ParseError.BadDigit
    end
    return n
end
```

`!T` lets the compiler work out the error set from the body: every error the function returns, and every error it passes on with `try`.

```nim
func double(n: int) -> !int
    let digit = try parse_digit(n)
    return digit * 2
end
```

`E!void` and `!void` are allowed: the function can end without a `return`.

`T` can itself be an error set: in `E!Kind`, `return Kind.A` succeeds and `return E.Bad` fails. `T` can't be `E` itself (`E!E` or `E!?E` is a compile error), since `return` couldn't tell the two apart.

`E!T` and `!T` only exist as return types. A call to a function that can fail must be handled right away with `try` or `catch`: `let x = parse_digit(1)` is a compile error.

### try

`try call` gives the value if the call succeeds. If it fails, the current function returns the same error.

```nim
func sum(a: int, b: int) -> ParseError!int
    return try parse_digit(a) + try parse_digit(b)
end
```

- The current function must be able to fail. With an explicit set (`ParseError!int`), every error the call can return must be in it.
- In `main`, which must then return `!void` (or `E!void`), an error that reaches the end of `main` stops the program: `error: unhandled error ParseError.BadDigit`.
- `try` applies to the call right after it, including `.` and `[]` (`try a.b(c)`).

### catch

`call catch value` uses `value` when the call fails. It must have the call's success type.

```nim
let n = parse_digit(-1) catch 0
let m = parse_digit(-1) catch parse_digit(2) catch 0
```

When the result goes where a nullable is expected (a typed `let`, an assignment, an argument or a `return`), the fallback can be `null`: the success value becomes a `?T` too.

```nim
let digit: ?int = parse_digit(-1) catch null

func first_or_null[T](mut items: []T) -> ?T
    return pop(items) catch null
end
```

`call catch e then ... end` runs a block with the error bound to `e`. When the result is used (`let`, assignment, argument, `return`), the block must leave with `return`, `break` or `continue`. Used as a statement, or for an `E!void` call, it can just end.

```nim
func describe(n: int) -> int
    let digit = parse_digit(n) catch e then
        return -1
    end
    return digit
end

parse_digit(-1) catch e then
    io::println("$e")
end
```

`catch` has the lowest precedence of all operators: `parse_digit(1) catch 0 == 0` is `parse_digit(1) catch (0 == 0)`. Use parentheses: `(parse_digit(1) catch 0) == 0`.

### Branching on errors

Catch the error, then use [`when`](#when) on it. Cases name the errors, and without an `else` every error the call can return must be listed:

```nim
let digit = parse_digit(n) catch e then
    when e
        ParseError.Empty then return 0 end
        ParseError.BadDigit then return -1 end
    end
end
```

This also works when the error set is inferred (`!T`): the cases must then cover every error the function can return.

A caught error can also be passed to a function, stored in a field or assigned to a variable typed with an error set. This works when every error it can hold is in that set, for inferred sets too:

```nim
func report(e: ParseError) -> void
    io::println("$e")
end

double(-1) catch e then
    report(e)
end
```

### Runtime errors

Errors are for failures the program expects. A failed `assert`, an index out of bounds, an integer `//` or `%` by `0`, an `int` result that doesn't fit in 64 bits, a `float` side of `//` outside the `int` range or more than 100,000 nested calls still stops the program and can't be caught.

The message names the function that was running, and the builtin when the failure happened inside one:

```
error: index 5 is out of bounds for array of length 2
  in function 'pick'
error: out of memory
  in builtin 'repeat', called from function 'grow'
```

There are no line numbers yet.

## 13. Tasks

A task runs a function alongside the rest of the program. `spawn` starts one and returns a `Task`, `await` waits for its result, and `task::sleep` (from `import std/task`) pauses the running task:

```nim
func main() -> void
    let answer = spawn(func() -> return 6 * 7)
    io::println("waiting")
    io::println("${await(answer)}")   # 42
end
```

| Function | Signature | Description |
| :------- | :-------- | :---------- |
| `spawn` | `spawn(body: func() -> E!T) -> Task[T, E]` | Starts a task that runs `body`. |
| `await` | `await(task: Task[T, E]) -> E!T` | Waits until the task is done and returns its result, or fails with its error. |
| `task::sleep` | `sleep(ms: int) -> void` | Pauses the running task for `ms` milliseconds. In `std/task`. |

### Running order

All tasks run on one thread, one at a time. A task keeps running until it waits (`await` on a task that isn't done, `sleep`) or ends; then the next ready task runs. A new task starts the first time the task that spawned it waits or ends, and ready tasks take turns in the order they became ready. `task::sleep(0)` gives the other ready tasks a turn:

```nim
func step(name: string) -> void
    for 1..2: i do
        io::println("$name $i")
        task::sleep(0)
    end
end

func main() -> void
    let a = spawn(func() -> return step("a"))
    let b = spawn(func() -> return step("b"))
    await(a)
    await(b)
end
# a 1, b 1, a 2, b 2
```

Waiting doesn't change a function's type: `step` above is a plain `-> void` function that happens to call `sleep`, and any function can call `sleep` or `await`. While one task sleeps, the others run, so two tasks that each sleep 100 ms finish in about 100 ms, not 200:

```nim
func slow_double(x: int) -> int
    task::sleep(100)
    return x * 2
end

func main() -> void
    let first = spawn(func() -> return slow_double(1))
    let second = spawn(func() -> return slow_double(2))
    io::println("${await(first) + await(second)}")   # 6, after about 100 ms
end
```

A task that never waits keeps the others from running until it ends: put a `task::sleep(0)` in long loops that should share time.

Lambdas given to `spawn` share the variables they use with the function around them, as any closure does. Values that tasks share change only while the task changing them runs, but any call that waits lets other tasks run in between:

```nim
func main() -> void
    let mut count = 0
    let counter = spawn(func()
        for 1..3: _ do
            count = count + 1
            task::sleep(0)
        end
    end)
    await(counter)
    io::println("$count")   # 3
end
```

### Task types

`Task[T, E]` is a task that gives a `T` or fails with an error from `E`. `spawn` takes both from its function: `spawn(func() -> return 6 * 7)` is a `Task[int, none]`, and a body that returns nothing gives `Task[void, none]`. `Task[T]` is short for `Task[T, none]`, a task that can't fail, so its `await` takes no `try`. A task whose body can fail passes its errors to `await`:

```nim
func main() -> !void
    let parsed = spawn(func() -> return try string::to_int("12"))   # Task[int, ConversionError]
    io::println("${try await(parsed)}")                                    # 12

    let bad = spawn(func() -> return try string::to_int("x"))
    io::println("${await(bad) catch -1}")                                  # -1

    let mut tasks: []Task[int, ConversionError] = []
    for ["1", "2", "3"]: text do
        append(tasks, spawn(func() -> return try string::to_int(text)))
    end
    let mut total = 0
    for tasks: t do
        total = total + (await(t) catch 0)
    end
    io::println("$total")                                                # 6
end
```

A task fits a `Task` type with the same `T` when every error it can fail with is in that type's error set, as for `try`. `await` can be called any number of times, from any number of tasks: once the task is done, it returns the same result right away.

- A task whose result isn't used can't fail: `spawn(func() -> return try save(data))` as a statement is a compile error (`this task's result isn't used, so it can't fail`). Catch the errors inside it, or keep the task and `await` it.
- A task that failed and was never awaited is reported when the program ends (`error: unhandled error in task: ConversionError.Invalid`), and the program exits with an error.
- Tasks can be stored in arrays, fields and nullables (`?Task[int]`), and interpolating one gives `<task>`, or `<task done>` once it's done. `copy` keeps the same task. Tasks can't be compared (`CantCompare`), and `Task(...)` can't create one (`TaskNeedsSpawn`): use `spawn`.

### Program end

The program ends when every task is done, so a task spawned near the end still finishes:

```nim
func main() -> void
    spawn(func()
        task::sleep(50)
        io::println("background work done")
    end)
    io::println("top level done")
end
# top level done, then background work done
```

These stop the program like other runtime errors:

- `await` on the running task itself.
- Every unfinished task waiting for another one, with no `sleep` that could wake any of them (`all tasks are waiting: nothing can wake them up`).
- `sleep` with a negative time.

A runtime error in any task stops the whole program.

## 14. Modules

Each file is a module. A file uses other modules by importing them at the top, before any other declaration:

```nim
import @/physics                    # physics.dsk in this project
import @/ui/button as btn           # ui/button.dsk, used as btn
import @/ui/theme: dark, light      # also brings dark and light in as plain names
import std/string                   # a standard library module
import std/array: len, append
import std/string: split as split_text,
    trim
```

- `@/` is the project: the folder of the file passed to `dusk`. `@/ui/button` is the file `ui/button.dsk` in that folder. Paths use `/` and have no quotes or `.dsk` extension.
- `std/` is the [standard library](#15-prelude-and-standard-library): `std/array`, `std/string`, `std/buffer`, `std/task`, `std/io`, `std/fs`, `std/net`, `std/time`.
- Any other first part (`import jsonc`) names a library. Libraries aren't supported yet (`LibrariesNotSupported`).
- A missing file is a `ModuleNotFound` error and an unknown `std/` module an `UnknownStdModule` error, both at the import.

### Using a module

`::` reaches what a module declares: functions, types, constants and error sets.

```nim
let world = physics::World(gravity = 9.8)
physics::step(world, 0.016)
let bodies: []physics::Body = []
let theme: ?btn::Theme = null
let parts = string::split("a,b", ",")
let color = physics::Color.Red
let limit = physics::MAX_BODIES
```

The name before `::` is the last part of the path, or the name after `as`. It's only a module name: it never clashes with variables, functions or types, so a variable can be called `physics` in a file that imports `@/physics`. Using a module that isn't imported is a `ModuleNotImported` error.

`: names` after the path also brings those names in as plain names, and `as` renames one. The module name still works, so `len(xs)` and `array::len(xs)` are the same after `import std/array: len`. A long list continues on the next line after a trailing comma.

In a file, a name means one thing: a declaration, a selected name and a prelude builtin can't share a name, and two imports can't use the same module name (`AlreadyDefined`). Use `as` to rename one of them.

`physics::x` only reaches what `physics.dsk` declares itself, not what it imported.

### File-only declarations

`~` before a name keeps it in its file. Without `~`, a declaration can be used by any file that imports the module.

```nim
let ~SCRATCH_SIZE = 256

func step(world: mut World, dt: float) -> void
    integrate(world, dt)
end

func ~integrate(world: mut World, dt: float) -> void
end

struct Counter
    count: int = 0
    ~history: []int = []

    func increment(mut self: @) -> void
        self.remember()
        self.count = self.count + 1
    end

    func ~remember(mut self: @) -> void
        append(self.history, self.count)
    end
end
```

- `~` goes on top-level functions, structs, enums, unions, error sets, traits and constants, and on fields and methods. Variants and static fields follow their type. It's only written where the name is declared.
- From another file, a file-only name is a `NotVisible` error, through `::`, a selective import, a field read or write, or a method call. Inside its own file it works like any other name.
- Another file can build a struct only through its public fields: a file-only field with a default value gets it, and a file-only field without one makes the struct impossible to build outside its file (`CantBuildOutsideModule`; give the file a function that builds it).
- A public declaration can't mention a file-only type: a public function's parameters, return type and error set, a public field or static field, a public method, a union variant (`PrivateTypeInPublicSignature`). Mark both `~`, or neither.
- A file-only method doesn't count for a trait outside its file.

### Rules

- Importing a module runs nothing: modules only hold declarations and constants.
- Only the `main` of the file passed to `dusk` runs. Other modules can have one, and it's ignored when they're imported.
- Modules can't import each other in a cycle (`ImportCycle`, with the chain: `@/a -> @/b -> @/a`).
- A generic function or type from another module is checked in its own file: it can use that file's file-only helpers, and its copies are named after it (`util::first[int]`).
- Traits match by shape, so a struct from one module fits a trait from another.
- Compile errors in another file name it (`... at line 2 col 12 in ui/button.dsk`). Types from other modules are shown with their module (`struct shapes::Square`), and so are functions in runtime errors (`in function 'util::divide'`) and errors (`unhandled error util::ParseError.Empty`).

## 15. Prelude and Standard Library

The **prelude** is always available. Everything else is in the **standard library**, a set of modules imported with `std/`.

`T` means any type.

| Function   | Signature                                         | Description                                                                          |
| :--------- | :------------------------------------------------ | :----------------------------------------------------------------------------------- |
| `assert`   | `assert(cond: bool) -> void`                      | Stops the program with an error if `cond` is false.                                  |
| `copy`     | `copy(value: T) -> T`                             | Returns a deep copy of an array, map, buffer, struct or union (see below).           |
| `to_byte`  | `to_byte(value: int) -> ConversionError!byte`     | Converts an `int` or `byte` to `byte`; fails with `ConversionError.Overflow` outside `0..255`. |
| `to_float` | `to_float(value: int \| byte \| float) -> float` | Converts an `int`, `byte` or `float` to `float`. Also takes a `T` whose type list only has `int`, `byte` and `float` (`[T: int \| float]`). |
| `to_int`   | `to_int(value: float) -> ConversionError!int`     | Converts a `float` to `int`, dropping the fraction toward zero (`-7.9` gives `-7`); fails with `ConversionError.Overflow` for `nan`, infinities and values outside the `int` range. |
| `spawn`    | `spawn(body: func() -> E!T) -> Task[T, E]`        | Starts a task (see [Tasks](#13-tasks)).                                              |
| `await`    | `await(task: Task[T, E]) -> E!T`                  | Waits for a task's result.                                                           |

The prelude also declares the error set `ConversionError` (`Invalid`, `Overflow`).

`copy(x)` copies everything reachable from `x`: nested arrays, map values, struct fields, union payloads and buffers, so changing the copy never changes the original (shared references inside `x`, including cycles, are kept shared within the copy). Map keys aren't copied: a struct key stays the same struct, so it still finds its entry in the copy. Strings and nullable primitives are immutable, so they are shared instead of copied, and `copy` of an `int`, `float`, `bool`, `byte`, string or enum returns the value itself.

### std/array

Functions that change an array take it as `mut`, and the ones that can fail return an error union, handled with `try` or `catch`. The module declares the error set `ArrayError` (`Empty`, `OutOfRange`). `pop`, `insert` and `remove` are native builtins: to pass one as a function value, wrap its call in a lambda or a Dusk function.

| Function | Returns | Description |
| :------- | :------ | :---------- |
| `len(list)` | `int` | The number of items. |
| `append(mut list, mut item)` | `[]T` | Adds an item to the end. |
| `extend(mut list, items)` | `[]T` | Appends all items, retaining references to nested values. Extending an array with itself appends its original contents once. |
| `sort(mut list)` | `[]T` | Sorts numbers or strings in ascending order and returns the same array reference. |
| `sort_by(mut list, is_before)` | `[]T` | Sorts in place using `func(T, T) -> bool`, and returns the same array reference. The callback returns whether its first item comes before its second. |
| `map(list, transform)` | `E![]U` | A new array containing each callback result, in input order. `transform` is `func(T) -> E!U`. |
| `filter(list, is_kept)` | `E![]T` | A new array containing the items for which `func(T) -> E!bool` returns true, preserving order and references to nested values. |
| `pop(mut list)` | `ArrayError!T` | Removes the last item and returns it. Fails with `ArrayError.Empty` on an empty array. |
| `pop_or_null(mut list)` | `?T` | Removes the last item and returns it, or `null` on an empty array. |
| `insert(mut list, index, mut item)` | `ArrayError![]T` | Puts `item` at `index`, moving the later items up. `index` can be `len(list)` (the end). Fails with `ArrayError.OutOfRange` outside `0..len(list)`. |
| `remove(mut list, index)` | `ArrayError!T` | Removes the item at `index`, moving the later items down, and returns it. Fails with `ArrayError.OutOfRange` outside the array. |
| `clear(mut list)` | `[]T` | Removes every item. |
| `reverse(mut list)` | `[]T` | Reverses the items in place. |
| `slice(list, start, stop)` | `ArrayError![]T` | A new array with the items from `start` up to, not including, `stop`. The items are copies, so changing the slice never changes `list`. Fails with `ArrayError.OutOfRange` for a negative bound, `start > stop` or `stop > len(list)`. |
| `index_of(list, item)` | `?int` | The position of the first item equal to `item`, or `null`. |
| `contains(list, item)` | `bool` | Whether some item equals `item`. |

`append`, `extend`, `clear`, `insert`, `reverse`, `sort` and `sort_by` mutate the input and return the same array reference. Pass a copy to preserve the original: `let extended = array::extend(copy(numbers), [4, 5])`, `let reversed = array::reverse(copy(numbers))` or `let cleared = array::clear(copy(numbers))`. Results can be chained, for example `array::sort(array::extend(copy(numbers), [4, 5]))`. `insert` still requires `try` or `catch`. `pop`, `pop_or_null` and `remove` return the removed item instead.

`sort` accepts `int`, `float`, `byte` and `string` arrays. Strings use lexicographic UTF-8 byte order. Floats put NaNs after all other values; signed zeros compare as equal. Both sorts use O(n log n) comparisons and constant auxiliary space; equal items may change relative order. The `sort_by` callback must give a consistent ordering, return false for equal items, and leave the array and its ordering fields unchanged. Use `array::sort(copy(numbers))` to preserve the original array.

`map` and `filter` infer the callback's error set. A non-failing callback gives a plain array result with no `try`; a failing callback stops at the first error and propagates it. `extend`, `map` and `filter` do not deep-copy items; use `copy` when independent nested values are needed. For `map` and `filter`, callbacks must leave the source array unchanged.

`index_of` and `contains` take arrays of `int`, `float`, `byte`, `bool` or `string`, and compare strings by content. Floats compare like `==`, so `nan` is never found and `-0.0` matches `0.0`.

```nim
import std/array: pop
import std/io

func main() -> void
    let mut stack = [1, 2, 3]
    for true do
        let item = pop(stack) catch _ then break end
        io::println("$item")   # 3, 2, 1
    end
end
```

### std/map

Reading and writing entries uses indexing (`m[k]`, `m[k] = v`, see [Maps](#maps)). Functions that change a map take it as `mut`.

| Function | Returns | Description |
| :------- | :------ | :---------- |
| `len(m)` | `int` | The number of entries. |
| `has(m, key)` | `bool` | Whether `key` has an entry, also when its value is `null`. |
| `remove(mut m, key)` | `bool` | Removes the entry for `key`. Returns whether there was one. |
| `keys(m)` | `[]K` | A new array with the keys, in insertion order. |
| `values(m)` | `[]V` | A new array with the values, in insertion order. |
| `clear(mut m)` | `void` | Removes every entry. |

```nim
import std/map
import std/io

func count_words(words: []string) -> map[string, int]
    let mut counts: map[string, int] = {}
    for words: word do
        counts[word] = (counts[word] orelse 0) + 1
    end
    return counts
end

func main() -> void
    let mut counts = count_words(["a", "b", "a"])
    io::println("${counts["a"]}")                 # 2
    map::remove(counts, "b")
    io::println("${map::keys(counts)}")           # [a]
    io::println("${map::len(counts)}")            # 1
end
```

### std/string

String functions never change their argument: a function that produces text returns a new string. They work with pipes: `line |> string::trim()`. The module declares the error set `StringError` (`OutOfRange`, `NegativeCount`, `InvalidUtf8`). Strings compare with `==` (see [Comparison and logic](#comparison-and-logic)).

Positions are **byte offsets**, the same unit as `string::len(s)` and `string::byte_at`, so a position from `string::find` can be passed straight to `string::slice`. `char_count` and `char_at` are the only functions that count characters. The `start` argument of `find` is inclusive; a byte offset inside a multi-byte character searches forward from that byte. `find_reverse` searches backward from an inclusive starting byte offset and returns a match start usable with `slice`. A match may extend beyond the starting offset: `find_reverse("abcdef", "bcd", 1)` returns `1`.

| Function                    | Returns                 | Description                                                                                                                                                       |
| :-------------------------- | :---------------------- | :---------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `len(text)`                 | `int`                   | Number of bytes (UTF-8).                                                                                                                                          |
| `char_count(text)`          | `int`                   | Number of characters (Unicode code points).                                                                                                                       |
| `concat(a, b)`              | `string`                | `b` after `a`.                                                                                                                                                    |
| `join(parts, sep = "")`     | `string`                | The `[]string` parts with `sep` between them (nothing by default); `""` for an empty array.                                                                       |
| `byte_at(text, index)`      | `StringError!byte`      | The byte at byte offset `index`.                                                                                                                                  |
| `char_at(text, index)`      | `StringError!string`    | The `index`-th character, as a one-character string (`string::char_at("olá", 2)` is `"á"`). Scans from the start.                                                 |
| `find(text, needle, start = 0)` | `?int` | Byte offset of the first match at or after `start`, or `null` for no match, an empty needle, or a start outside the string. Negative starts return `null`. |
| `find_reverse(text, needle, start = null)` | `?int` | Byte offset of the rightmost match starting at or before `start`. Omitted/null starts search from the end; past-end starts search from the end too. Negative starts, no match, or an empty needle return `null`. Overlapping matches count. |
| `compare(a, b)` | `int` | Lexicographic UTF-8 byte ordering: exactly `-1` if `a` comes before `b`, `0` if equal, or `1` if it comes after. Case-sensitive, matching `array::sort` on strings. |
| `contains(text, needle)`    | `bool`                  | Whether `needle` appears in `text`; `false` for an empty `needle`.                                                                                                |
| `starts_with(text, prefix)` | `bool`                  | Whether `text` starts with `prefix`; `false` for an empty `prefix`.                                                                                               |
| `ends_with(text, suffix)`   | `bool`                  | Whether `text` ends with `suffix`; `false` for an empty `suffix`.                                                                                                 |
| `count(text, needle)`       | `int`                   | Number of non-overlapping matches (`"aaaa"`, `"aa"` is 2); `0` for an empty `needle`.                                                                             |
| `trim(text)`                | `string`                | Removes spaces, tabs and line breaks (`\t`, `\n`, `\r`, `\v`, `\f`) from both ends.                                                                               |
| `trim_start(text)` | `string` | Removes the same whitespace as `trim`, from the start only. |
| `trim_end(text)` | `string` | Removes the same whitespace as `trim`, from the end only. |
| `upper(text)` / `lower(text)` | `string`              | Changes the case of ASCII and Latin-1 letters (`"ação"` ↔ `"AÇÃO"`). Other characters are unchanged.                                                              |
| `split(text, separator, limit = -1)`    | `[]string`              | The pieces between separators, keeping empty ones: `"a,,b"` gives `["a", "", "b"]`. An empty `separator` splits into characters: `"olá"` gives `["o", "l", "á"]`. A positive `limit` caps the number of parts, preserving the remainder in the last part: `split("a=b=c", "=", 2)` gives `["a", "b=c"]`. A negative limit means unlimited; `0` returns an empty array. |
| `replace(text, old, new)`   | `string`                | Replaces every non-overlapping `old`, left to right. An empty `new` deletes; an empty `old` returns `text` unchanged.                                             |
| `repeat(text, count)`       | `StringError!string`    | `text` repeated `count` times; `0` gives `""`.                                                                                                                    |
| `slice(text, start, stop)`  | `StringError!string`    | The bytes from `start` up to, not including, `stop`: `string::slice(s, 0, string::len(s))` is all of `s`.                                                         |
| `to_int(text)`              | `ConversionError!int`   | Reads an int written like a Dusk literal.                                                                                                                         |
| `to_float(text)`            | `ConversionError!float` | Reads a float (or int) written like a Dusk literal.                                                                                                               |
| `from_bytes(data)`          | `StringError!string`    | Copies a buffer into a new string; fails with `StringError.InvalidUtf8` if it isn't valid UTF-8.                                                                  |

Use `string::compare` for string fields in custom sorting: `array::sort_by(players, func(a, b) -> return string::compare(a.name, b.name) < 0)`. It compares bytes without case folding or locale rules.

`to_int` and `to_float` share their names with the prelude's conversions, so `import std/string: to_int` is an `AlreadyDefined` error: call them as `string::to_int`, or rename them (`import std/string: to_int as parse_int`).

```nim
import std/string
import std/io

func describe_age(text: string) -> string
    let age = string::to_int(text) catch e then
        return when e
            ConversionError.Invalid then "not a number" end
            ConversionError.Overflow then "too big" end
        end
    end
    return "age $age"
end

func main() -> void
    let line = "  name = Dusk, age = 13  "
    for string::split(string::trim(line), ","): field do
        let pair = string::split(field, "=")
        io::println("${string::upper(string::trim(pair[0]))}: ${string::trim(pair[1])}")
    end
    io::println(describe_age("13"))     # age 13
    io::println(describe_age("abc"))    # not a number
end
```

### std/buffer

| Function            | Returns              | Description                                                                                       |
| :------------------ | :------------------- | :------------------------------------------------------------------------------------------------ |
| `len(data)`         | `int`                | Number of bytes.                                                                                  |
| `new(size)`         | `BufferError!buffer` | A new buffer of `size` zero bytes; fails with `BufferError.NegativeSize` for a negative size.     |
| `from_string(text)` | `buffer`             | Copies a string's bytes into a new buffer.                                                        |

The module declares the error set `BufferError` (`NegativeSize`). See [Buffers](#buffers).

### std/time

Clocks and timestamp values. Import with `import std/time`.

| Function | Returns | Description |
| :------- | :------ | :---------- |
| `now()` | `Timestamp` | A timestamp from the current wall-clock reading. |
| `from_unix_ms(milliseconds)` | `Timestamp` | A timestamp from any `int` epoch-millisecond value, including negative values. |
| `now_ms()` | `int` | Milliseconds since 1970-01-01T00:00:00Z (the Unix epoch), matching filesystem timestamp units. |
| `monotonic_ns()` | `int` | A nondecreasing nanosecond reading from an unspecified origin. Subtract two readings to measure elapsed time. |

`now_ms` is wall-clock time: system clock adjustments can move it forward or backward. It is independent of the local time zone. Fractions are rounded down to milliseconds, including before the epoch.

`monotonic_ns` ignores jumps in the system wall clock. Consecutive readings may be equal; nanosecond units do not guarantee nanosecond resolution. Elapsed time includes waiting for tasks or I/O, rather than only time spent executing. Inclusion of time while the whole system is suspended is platform-dependent. Use differences within a program run; do not persist monotonic readings as timestamps or compare them across runs or machines.

The raw clock calls return immediately, without allocating or yielding to another task. Neither needs `try`. A platform clock failure or a reading outside `int` range is a runtime error. Waiting remains `task::sleep` in `std/task`.

```dusk
import std/time
import std/array
import std/io

func main() -> void
    let mut numbers = [3, 1, 2]
    let start = time::monotonic_ns()
    array::sort(numbers)
    let elapsed_ns = time::monotonic_ns() - start
    let elapsed_ms = to_float(elapsed_ns) / 1000000.0
    io::println("Sorted in $elapsed_ms ms")
end
```

A `Timestamp` stores epoch milliseconds in a private field and has no mutating methods. Create one with `now` or `from_unix_ms`. It exposes:

| Method | Returns | Description |
| :----- | :------ | :---------- |
| `unix_ms()` | `int` | The stored epoch milliseconds, unchanged. |
| `to_string()` | `string` | ISO 8601-style UTC text with exactly three fractional millisecond digits. |

Formatting works for every `int` timestamp, without `try` or a date-range error. Years `0000–9999` use four digits. Larger years have a leading `+` and all digits; negative years have a leading `-` and at least four digits. The Gregorian calendar extends backward through year zero. Unix milliseconds do not represent leap seconds. Expanded-year output is not strict RFC 3339.

```dusk
let epoch = time::from_unix_ms(0)
io::println(epoch.to_string())
let before_epoch = time::from_unix_ms(-1)
io::println(before_epoch.to_string())
let modified = time::from_unix_ms(info.modified_ms)
```

The first two lines printed are `1970-01-01T00:00:00.000Z` and `1969-12-31T23:59:59.999Z`. The `Z` denotes UTC; local time zone settings do not affect the output. A year beyond 9999 is written like `+10000-01-01T00:00:00.000Z`.

The raw clock functions above do not allocate; timestamp factories allocate an ordinary struct, and `to_string` allocates its result string. `now` retains `now_ms`'s runtime clock-error behavior. Parsing, date arithmetic and timestamp comparison methods are not provided. Like other structs, `==` compares identity; use `unix_ms()` values when chronological equality or ordering is needed.

### std/task

| Function    | Returns | Description                                                     |
| :---------- | :------ | :-------------------------------------------------------------- |
| `sleep(ms)` | `void`  | Pauses the running task for `ms` milliseconds (see [Tasks](#13-tasks)). |

### std/io

Writes text to the standard output (stdout) and the standard error output (stderr). The functions take a `string`: use [interpolation](#string-interpolation) for any other value.

| Function         | Returns | Description                             |
| :--------------- | :------ | :-------------------------------------- |
| `print(text)`    | `void`  | Writes `text` to stdout.                |
| `println(text)`  | `void`  | Writes `text` and a newline to stdout.  |
| `eprint(text)`   | `void`  | Writes `text` to stderr.                |
| `eprintln(text)` | `void`  | Writes `text` and a newline to stderr.  |

```nim
import std/io

func main() -> void
    let scores = [10, 20]
    io::println("hello")
    io::println("scores: $scores")    # scores: [10, 20]
    io::print("no newline, ")
    io::println("then one")
    io::eprintln("warning: disk almost full")
end
```

Output is buffered. On a terminal, stdout shows at the end of each line. When stdout is a pipe or a file, it's written in large blocks and flushed when the program ends, before the program waits on timers or other tasks, and before a runtime error message. stderr is written right away.

Printing never needs `try`. If the program reading the output stops early (`dusk app.dsk | head -1`), the program ends quietly, like other command-line tools; any other failure to write is a runtime error (`can't write to stdout: NoSpaceLeft`).

Interpolation formats primitives, arrays and nullables as their values. It formats enums as their number, unions as `<union>`, maps as `<map>`, functions as `<func name>` (`<func>` for a lambda), tasks as `<task>` or `<task done>`, and structs as their raw values.

#### Reading stdin

`io::stdin()` returns the program's standard input as an `io::Stdin`. Every call gives the same input, whether it's a terminal, a pipe (`cat data.txt | dusk app.dsk`) or a file (`dusk app.dsk < data.txt`). Reading waits without blocking the program: only the reading task waits, and other tasks keep running.

| Method | Returns | Description |
| :----- | :------ | :---------- |
| `read_line(max = -1, timeout_ms = -1)` | `IoError!?string` | The next line, without its `\n` or `\r\n`; the last line is returned even without a newline. `null` at the end of the input. |
| `read_bytes(max, timeout_ms = -1)` | `IoError!?buffer` | Up to `max` bytes (at least 1), as soon as some are there: it doesn't wait for `max` of them. `null` at the end of the input. |
| `read_exact(count, timeout_ms = -1)` | `IoError!?buffer` | Exactly `count` bytes (at least 1), waiting until they're all there. `null` at the end of the input; `UnexpectedEnd` when the input ends partway, and the bytes that did arrive stay for the next read. |
| `read_all(timeout_ms = -1)` | `IoError!string` | Everything until the end of the input, as text. |
| `read_all_bytes(timeout_ms = -1)` | `IoError!buffer` | Everything until the end of the input, as bytes. |
| `is_terminal()` | `bool` | Whether the input is a terminal (a person typing) rather than a pipe or a file. |
| `close()` | `void` | Stops reading. Reading afterwards fails with `Closed`; closing twice does nothing. |

```nim
import std/io

func main() -> !void
    let input = io::stdin()
    if input.is_terminal() then
        io::print("Name: ")
    end
    let name = try input.read_line() orelse "anonymous"
    io::println("Hello, $name")

    for true do
        let next = try input.read_line()
        if next: line then
            io::println("> $line")
        else
            break
        end
    end
end
```

- An empty line is `""`; only `null` means the input ended. The end of input is read with a capture (`if next: line then ... else break end`), since `orelse break` isn't allowed.
- Reading from stdin first sends what's waiting in stdout's buffer when the read has to wait, so a prompt printed with `io::print` shows before the user types.
- `read_line(max = n)` fails with `LineTooLong` when a line is longer than `n` bytes. The line stays unread: drain it with `read_bytes` or stop reading. Without `max`, lines can be of any length.
- `read_line` and `read_all` fail with `InvalidUtf8` on text that isn't valid UTF-8 (`read_line` skips that line). Use `read_bytes` or `read_all_bytes` for binary data.
- `timeout_ms` makes a read fail with `TimedOut` when nothing complete arrives in time. Nothing is lost: what arrived stays for the next read, and the input stays usable.
- Two tasks reading stdin at the same time: the second one fails with `Busy`. A task waiting for input when another task calls `close()` fails with `Closed`.
- When the program has no input at all (started with stdin closed), reads return the end of the input.

#### Writing through stdout and stderr

`io::stdout()` and `io::stderr()` return writers for the same output `io::print` and `io::eprint` use, in the same order. Unlike `io::print`, their methods return errors, for programs that want to handle them:

| Method | Returns | Description |
| :----- | :------ | :---------- |
| `write(text)` | `IoError!void` | Writes `text`. |
| `write_bytes(data)` | `IoError!void` | Writes the bytes of a `buffer`. |
| `flush()` | `IoError!void` | Sends what's buffered now. |
| `is_terminal()` | `bool` | Whether the output is a terminal. |

When the program reading the output stopped (`dusk app.dsk | head -1`), they fail with `BrokenPipe` and the program keeps running; the output that couldn't be sent is dropped. Writes follow the same buffering rules as `io::print`, so a failure can show up at the next `write` or `flush` rather than at the write that caused it.

#### Errors

| `io::IoError` | When |
| :------------ | :--- |
| `Closed` | Reading after `close()`, or while another task closes the input |
| `Busy` | Another task is already reading the same input |
| `BrokenPipe` | Writing to an output whose reader is gone |
| `ConnectionReset` | The other side dropped the connection |
| `LineTooLong` | A line longer than `read_line`'s `max` |
| `InvalidUtf8` | `read_line` or `read_all` on text that isn't valid UTF-8 |
| `TimedOut` | A read with `timeout_ms` didn't complete in time |
| `UnexpectedEnd` | `read_exact` reached the end of the input before `count` bytes |
| `Other` | Any other failure |

#### Readers, writers and `io::copy`

Two traits describe what reads and what writes, so generic code works with any of them:

```nim
trait Reader
    func read_bytes(mut self: @, max: int, timeout_ms: int) -> IoError!?buffer
end

trait Writer
    func write(mut self: @, text: string) -> IoError!void
    func write_bytes(mut self: @, data: buffer) -> IoError!void
    func flush(mut self: @) -> IoError!void
end
```

`io::copy(from, to)` reads from a `Reader` until its end and writes everything to a `Writer`, then flushes it, and returns the number of bytes copied. `io::Stdin`, `io::Stdout`, `io::Stderr` and network connections (`net::TcpConn`, see [std/net](#stdnet)) fit these traits, and so does any struct with the same methods, so your own types work with `io::copy` too:

```nim
import std/io
import std/buffer
import std/array: len

struct Greetings
    names: []string
    position: int = 0

    func read_bytes(mut self: @, max: int, timeout_ms: int) -> io::IoError!?buffer
        if self.position >= len(self.names) then
            return null
        end
        self.position = self.position + 1
        return buffer::from_string("hello ${self.names[self.position - 1]}\n")
    end
end

func main() -> !void
    let mut source = Greetings(names = ["ana", "bob"])
    let mut out = io::stdout()
    let copied = try io::copy(source, out)   # hello ana, hello bob
    try io::copy(io::stdin(), io::stdout())  # copies stdin to stdout
end
```

The trait methods take `mut self` because reading or writing may change the stream, so a type that keeps its position in a field fits. Methods that change nothing, like the ones of `io::Stdin`, fit too. Generic code that uses them takes the stream as `mut` (`io::copy(mut from: R, mut to: W)`), so a stream kept in a variable is passed as `let mut`; temporaries like `io::stdin()` can be passed directly. Calling a stream's own methods (`input.read_line()`) doesn't need `mut`.

### std/fs

Reads and writes files and folders. Every call waits for the disk without blocking the program: it parks only the running task, and other tasks keep running meanwhile (see [Tasks](#13-tasks)). Paths are relative to the current directory. Functions that can fail return an error union with the module's error set `FsError`:

| Variant | When |
| :------ | :--- |
| `NotFound` | The path, or a folder on the way to it, doesn't exist |
| `PermissionDenied` | The program isn't allowed to read, write or list it |
| `AlreadyExists` | `make_dir` on a path that already exists |
| `IsDirectory` | A file operation on a folder |
| `NotDirectory` | A folder operation on a file, or a file in the middle of a path |
| `NotEmpty` | `delete` on a folder that has contents |
| `InvalidUtf8` | `read` on a file that isn't valid UTF-8 text |
| `Other` | Any other failure (a full disk, too many open files...) |

| Function | Returns | Description |
| :------- | :------ | :---------- |
| `read(path)` | `FsError!string` | The whole file as text. |
| `read_bytes(path)` | `FsError!buffer` | The whole file as bytes, for anything that isn't text. |
| `write(path, text)` | `FsError!void` | Creates the file, or replaces its contents. |
| `write_bytes(path, data)` | `FsError!void` | Same, from a `buffer`. |
| `append(path, text)` | `FsError!void` | Adds `text` to the end of the file, creating it if needed. |
| `read_dir(path)` | `FsError![]string` | The names of the entries in a folder (not `.` and `..`), in the order the system gives them. |
| `exists(path)` | `bool` | Whether the path exists. Never fails: any problem gives `false`. |
| `stat(path)` | `FsError!FileInfo` | Information about a file or folder (below). |
| `make_dir(path, recursive = false)` | `FsError!void` | Creates a folder. With `recursive = true`, also creates missing parent folders, and a folder that already exists is fine. |
| `delete(path, recursive = false)` | `FsError!void` | Deletes a file or an empty folder. With `recursive = true`, deletes a folder with everything inside. |
| `rename(from, to)` | `FsError!void` | Renames or moves a file or folder. |

```nim
struct FileInfo
    size: int           # bytes; 0 for a folder
    is_dir: bool
    is_file: bool
    is_symlink: bool
    modified_ms: int    # milliseconds since 1970-01-01 (UTC)
    created_ms: ?int    # null when the file system doesn't record it
    permissions: int    # the permission bits: 420 is rw-r--r--
end
```

- `stat` follows symbolic links: `is_symlink` says the path itself is a link, and the other fields describe the file it points to (or the link itself, when what it points to is gone).
- `delete(path, recursive = true)` never follows symbolic links: a link inside the folder is deleted, not what it points to.
- `write`, `write_bytes` and `append` create new files with the permissions `rw-rw-rw-`, minus what the system's default mask removes (usually `rw-r--r--`).
- Each call reads or writes the whole file at once. `read_bytes` returns a new `buffer`: changing it doesn't change the file until it's written back.

```nim
import std/io
import std/fs

func main() -> !void
    try fs::make_dir("out/logs", recursive = true)
    try fs::write("out/logs/today.txt", "started\n")
    try fs::append("out/logs/today.txt", "done\n")
    io::print(try fs::read("out/logs/today.txt"))   # started, done

    let info = try fs::stat("out/logs/today.txt")
    io::println("${info.size} bytes")                     # 13 bytes
    try fs::delete("out", recursive = true)

    let config = fs::read("config.json") catch e then
        when e
            fs::FsError.NotFound then io::println("no config, using defaults") end
            else then io::eprintln("can't read config.json") end
        end
        return
    end
    io::println(config)
end
```

### std/net

TCP connections over the network. A server listens on an address and accepts connections; a client connects to one. Connecting, accepting, reading and writing wait without blocking the program: only the calling task waits, and other tasks keep running (see [Tasks](#13-tasks)).

| Function | Returns | Description |
| :------- | :------ | :---------- |
| `tcp_connect(host, port, timeout_ms = -1)` | `NetError!TcpConn` | Connects to `host` (a name like `"example.com"` or `"localhost"`, or an IP address like `"127.0.0.1"` or `"::1"`) on `port`. |
| `tcp_listen(host, port, backlog = 128)` | `NetError!TcpListener` | Listens on a local address: `"0.0.0.0"` for every IPv4 interface, `"::"` for IPv6, `"127.0.0.1"` or `"localhost"` for this machine only. Port `0` picks a free port. `backlog` is how many connections can wait for `accept`. |

A name is looked up first, and each of its addresses is tried in order until one works (`"localhost"` is usually `::1` and then `127.0.0.1`); IP addresses skip the lookup. `timeout_ms` on `tcp_connect` covers the lookup and every attempt.

**`TcpListener`**

| Method | Returns | Description |
| :----- | :------ | :---------- |
| `accept(timeout_ms = -1)` | `io::IoError!TcpConn` | Waits for the next connection. |
| `port()` | `int` | The port it listens on, useful after listening on port `0`. Still returns it after `close()`. |
| `close()` | `void` | Stops listening. A task waiting in `accept` fails with `Closed`; closing twice does nothing. |

**`TcpConn`** reads like `io::Stdin` and writes like `io::stdout()`:

| Method | Returns | Description |
| :----- | :------ | :---------- |
| `read_line(max = -1, timeout_ms = -1)` | `io::IoError!?string` | The next line, without its `\n` or `\r\n`. `null` when the other side closed the connection. |
| `read_bytes(max, timeout_ms = -1)` | `io::IoError!?buffer` | Up to `max` bytes, as soon as some are there. |
| `read_exact(count, timeout_ms = -1)` | `io::IoError!?buffer` | Exactly `count` bytes. |
| `read_all(timeout_ms = -1)` | `io::IoError!string` | Everything until the other side closes the connection, as text. |
| `read_all_bytes(timeout_ms = -1)` | `io::IoError!buffer` | Same, as bytes. |
| `write(text)` | `io::IoError!void` | Sends `text`. |
| `write_bytes(data)` | `io::IoError!void` | Sends the bytes of a `buffer`. |
| `flush()` | `io::IoError!void` | Writes are never buffered, so there's nothing to send: it only fails with `Closed` after `close()`, like `write`. It's there so a connection fits `io::Writer`. |
| `remote_address()` | `Address` | The other side's address, saved when the connection opened. |
| `close()` | `void` | Closes the connection. |

```nim
struct Address
    host: string    # an IP address: "127.0.0.1", "::1"
    port: int
end
```

A server accepts connections in a loop and serves each one in its own task:

```nim
import std/io
import std/net

func serve(conn: net::TcpConn) -> io::IoError!void
    for true do
        let next = try conn.read_line()
        if next: line then
            try conn.write("you said: ${line}\n")
        else
            break
        end
    end
    conn.close()
end

func main() -> !void
    let listener = try net::tcp_listen("0.0.0.0", 8080)
    io::println("listening on ${listener.port()}")
    for true do
        let conn = try listener.accept()
        spawn(func() -> void
            serve(conn) catch _ then conn.close() end
        end)
    end
end
```

```nim
import std/io
import std/net

func main() -> !void
    let conn = try net::tcp_connect("localhost", 8080, timeout_ms = 5000)
    try conn.write("hello\n")
    io::println(try conn.read_line() orelse "closed")   # you said: hello
    conn.close()
end
```

- A write returns once the system has taken all the bytes. When the other side reads slowly, the writing task waits, so a fast writer can't fill memory.
- Writes from several tasks to the same connection are sent whole, one after the other, in the order they were called: they never mix.
- Reads follow the stdin rules: `null` means the other side closed the connection, `timeout_ms` loses nothing, and a second task reading the same connection at the same time fails with `Busy`. The same goes for two tasks in `accept` on one listener.
- Writing to a connection the other side has closed fails with `BrokenPipe` or `ConnectionReset`. The first write after that may still succeed, since the system only finds out on a later one.
- Closing a connection with writes still waiting (from other tasks) makes them fail with `Closed`. A connection or listener that isn't closed stays open until the program ends.
- `remote_address()` never fails: it keeps working after the other side leaves and after `close()`, so a server can log who it served. Each call returns a new `Address`.

Opening (`tcp_connect`, `tcp_listen`) fails with `NetError`; using a listener or a connection fails with `io::IoError` (see [std/io](#stdio)), so generic code like `io::copy` has one error type.

| `net::NetError` | When |
| :-------------- | :--- |
| `HostNotFound` | The name doesn't exist, or it can't be looked up (no network) |
| `ConnectionRefused` | Nothing listens on that port |
| `AddressInUse` | `tcp_listen` on an address and port something already listens on |
| `NetworkUnreachable` | No route to the address |
| `PermissionDenied` | The system doesn't allow it (a port below 1024 without the rights, for example) |
| `InvalidAddress` | An empty host, a port outside `0..65535`, or an address that isn't on this machine (`tcp_listen`) |
| `TimedOut` | `tcp_connect` didn't finish within `timeout_ms` |
| `Other` | Any other failure |

### Errors from builtins

Functions that can fail return an error union, handled with `try` or `catch` like any failing call. Error sets from the standard library are written with their module (`string::StringError.OutOfRange`) unless they're imported by name.

| Error | When |
| :---- | :--- |
| `ConversionError.Invalid` | The text isn't a number. `string::to_int` accepts an optional `-` and digits; `string::to_float` also accepts one `.` followed by digits. Spaces, `+`, `_`, exponents, hex, `inf` and `nan` are invalid. |
| `ConversionError.Overflow` | An int that doesn't fit in 64 bits, a float too large to represent, a `to_byte` value outside `0..255`, or a `to_int` value that's `nan`, infinite or outside the `int` range. |
| `ArrayError.Empty` | `pop` on an empty array. |
| `ArrayError.OutOfRange` | `insert`, `remove` or `slice` outside the array. |
| `StringError.OutOfRange` | `string::slice` with a negative bound, `start > stop`, `stop > string::len(text)`, or a bound inside a multi-byte character; `string::byte_at` / `string::char_at` with an index outside the string. |
| `StringError.NegativeCount` | `string::repeat` with a negative `count`. |
| `StringError.InvalidUtf8` | `string::from_bytes` with bytes that aren't valid UTF-8. |
| `BufferError.NegativeSize` | `buffer::new` with a negative size. |

## 16. Errors

Compile errors point to the line and column of the problem:

```
error: Invalid type: expected 'int' found 'string' at line 1 col 14
--> 1: let x: int = "dusk"
                    ^~~~~~^
```

The program doesn't run if it has a compile error. Runtime errors (a failed `assert`, an index out of bounds, an integer division by `0`, an `int` overflow, too many nested calls, an error returned from `main`, a task error nobody awaited; see [Runtime errors](#runtime-errors) and [Tasks](#13-tasks)) stop the program. For errors a program can handle, see [Error Handling](#12-error-handling).

## 17. Known Limitations

These are known gaps in v0.15. Most are planned in the [roadmap](roadmap.md).

- **Registers**: a function can have at most 255 variables and temporaries alive at once. Going over is a compile error; split the code into smaller functions.
- **Function size**: a function can compile to at most 65,535 bytecode instructions. Going over is a compile error; split the code into smaller functions.
- **Call depth**: at most 100,000 nested calls. Going deeper stops the program with a runtime error. A call returned directly (`return f(x)`, also in a `void` function) is a tail call: it reuses the current call instead of nesting, so tail-recursive functions can run any number of steps. In a function that can fail, `return try f(x)` is a tail call too when `f` succeeds with the same type (a `-> E!?int` function returning `try` of an `E!int` call isn't, since the result is converted first).
- **Strings**: Positions are byte offsets; only `string::char_count` and `string::char_at` count characters (`char_at` scans from the start), and case changes cover ASCII and Latin-1 letters only. There's no case-insensitive comparison: compare `string::lower(a) == string::lower(b)`.
- **Building strings in a loop**: `s = string::concat(s, x)` copies the whole string every time. To build a long string, collect the pieces in an array and call `string::join` once.
- **String literals**: a program can have at most 65,536 different string literals.
- **Formatting**: interpolating a struct shows its raw values, not its field names, and interpolating a union doesn't show which variant is active.
- **Read-only values**: a function call counts as a new value, so a function that returns its own non-`mut` parameter (`func keep(items: []int) -> []int return items end`) lets `let mut y = keep(frozen)` change `frozen`.
- **Union size**: a union can have at most 256 variants.
- **Array search**: `index_of` and `contains` don't take arrays of enums, structs, unions, arrays or buffers yet.
- **Maps**: keys can't be floats, nullables, arrays, maps, buffers, unions or functions, and struct keys match by identity only. There's no content comparison for maps (`==` is identity).
- **Prelude names**: a top-level declaration can't use the name of a prelude builtin (`assert`, `copy`, `to_int`, ...). Standard library functions don't have this problem: they're only visible after an import, and `as` renames them.
- **Modules**: libraries (`import jsonc`) and package manifests aren't supported yet, a folder can't be a module, and a module can't re-export what it imports.
- **Function values**: builtins and methods can't be used as values (`let f = string::len`, `let f = point.length`): wrap them in a lambda. A lambda can use at most 255 variables from the functions around it.
- **Lambda parameter types**: in a generic call, a slot that only the expected result type fills (`let xs: []string = make(func(x) -> ...)`) doesn't reach a lambda's parameters, so that lambda needs its parameter types written.
- **Tasks**: all tasks share one thread, and a task only gives way when it waits (`await`, `task::sleep`, or a file, stdin or network call). A running task can't be stopped from outside.
- **Network**: only TCP for now: no UDP, no TLS (`https`), no half-closing a connection and no socket options. Only one task at a time can wait in `accept` on a listener.
- **Generics**: type arguments can't be written in a call (`empty[int]()`); slots come from the arguments or the expected type. Methods can't have their own slots, traits can't require static methods or fields, and type lists hold primitives only. A generic copy calling itself with a bigger type (`f([x])` inside `f[T]`) stops after 64 nested copies.
