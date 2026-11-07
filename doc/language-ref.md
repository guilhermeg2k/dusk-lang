# Language Reference

This is the reference for **v0.14 - Simon (in development)**. Planned features are listed in the [roadmap](roadmap.md).

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
13. [Built-in Functions](#13-built-in-functions)
14. [Errors](#14-errors)
15. [Known Limitations](#15-known-limitations)

## 1. Basics

A program starts at its `main` function. Since v0.13 every program must declare one, taking no parameters and returning `void`, or an error union of `void` (`-> !void`) when it uses `try` (see [try](#try)).

```nim
func main() -> void
    echo("Hello, Dusk")
end
```

The top level only holds declarations: functions, structs, enums, unions, error sets and [constants](#constants). Any other statement, a `let mut` or a `let` that isn't a constant at the top level is a compile error (`StatementOutsideMain`, `TopLevelLetNotConstant`). Functions and types are declared at the top level only, not inside a function (`DeclarationInsideFunction`).

To keep them short, the examples in this reference show statements without the `main` around them.

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

**Keywords**: `let`, `mut`, `func`, `return`, `struct`, `enum`, `union`, `error`, `try`, `catch`, `extern`, `if`, `elif`, `else`, `when`, `for`, `break`, `continue`, `then`, `do`, `end`, `and`, `or`, `orelse`, `true`, `false`, `null`, `int`, `byte`, `float`, `string`, `buffer`, `bool`, `void`.

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
echo(view)       # [1, 2, 3]
echo(snapshot)   # [1, 2]
```

Strings and primitives can't be changed in place, so sharing them never shows.

A value read through a name that can't change it (a `let` variable, a parameter or capture without `mut`, `self` in a method without `mut self`, or a field or item reached through one of them) can't go into a mutable place: a `let mut` variable, an assignment to one or to its fields and items, `append` and `insert`, or a `mut` argument. Constructors, array literals and `if`, `when`, `orelse` and `catch` results count as the values they hold. Otherwise the value could change without any `mut` on its own name:

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
| `buffer` | Fixed-size block of mutable bytes             | `new_buffer(16)`        |
| `bool`   | Boolean                                       | `true`, `false`     |
| `void`   | No value, used as a function return type      |                     |

Since v0.13, an `int` converts to `float` wherever a `float` is expected: a typed `let`, an assignment, an argument, a return value, a struct field, a union payload, an array item and a `catch` or `orelse` fallback (`let ratio: float = count`). An `int` or `?int` converts to `?float` the same way. An untyped array literal that mixes them is a `[]float` (`[1, 2.5]`). `to_float(x)` converts an `int` or `byte` explicitly (`echo(to_float(count))`).

A `float` never converts to `int` (use `//`), a `byte` only converts to `float` with `to_float(x)`, and `[]int` doesn't convert to `[]float`. Ints larger than 2^53 lose precision as floats.

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
echo("Hello\nworld")
echo("Say \"hello\"")
echo("Moon: \u{1F319}")
echo("A literal backslash: \\")
```

Strings can't be indexed with `s[i]`. Use `str_byte_at(s, i)` for the byte at a byte offset, or `str_char_at(s, i)` for the `i`-th character (see [String functions](#string-functions)). `for s: b, i do` loops over a string's bytes. Strings are immutable; use a buffer to change bytes.

```nim
let word = "olá"
echo(try str_byte_at(word, 0))   # 111 ('o')
echo(try str_char_at(word, 2))   # á
for word: b, i do                # 4 bytes: "á" is two bytes in UTF-8
    echo("$i: $b")
end
```

A double-quoted string must end on the line where it starts: a line break inside it is a compile error. Use `\n` or a multiline string instead. Line breaks inside a `${ ... }` interpolation are allowed, because they belong to the expression. There is no `+` for strings: join text with interpolation (`"$a$b"`), `str_concat(a, b)` or `str_join(parts)`.

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
    'a'..'z' then echo("lower") end
    ',' then echo("comma") end
    else then echo("other") end
end
```

- An int constant outside `0..255` (`let b: byte = 300`) is a compile error. `to_byte(x)` returns `ConversionError!byte` and fails with `ConversionError.Overflow` outside that range, so it needs `try` or `catch`.
- An int variable never converts to `byte` implicitly: use `to_byte(x)`. So `b = b + 1` on a `byte` variable is an error, because `b + 1` is an `int`.
- Bytes work as `when` subjects with literal and range cases (an `else` is required), as array indexes and as range-loop bounds.
- `?byte` works like `?int`. A `[]byte` is a regular array, using as much memory per item as `[]int`.

### Buffers

A `buffer` is a fixed number of mutable bytes, stored packed (one byte per item, unlike `[]byte`). `new_buffer(n)` creates one with `n` zero bytes (`n` can be `0`); its size never changes. It returns `BufferError!buffer`, failing with `BufferError.NegativeSize` for a negative `n`, so the call needs `try` or `catch`.

```nim
let mut data = try new_buffer(4)  # in a function that can fail (-> !T)
data[0] = 'h'                     # writes take a byte
data[1] = 105
let first = data[0]               # reads give a byte
echo(buffer_len(data))            # 4
echo(data)                        # [104, 105, 0, 0]

for data: b, i do
    echo("$i: $b")
end

let bytes = buffer_from("olá")    # [111, 108, 195, 161]
let text = string_from(bytes) catch "not UTF-8"
```

- The type is written `buffer`: `let mut data: buffer = try new_buffer(4)`, `?buffer`, `[]buffer`, struct fields and parameters.
- Writing requires a `let mut` variable (or a mutable struct holding the buffer). A function that writes to a buffer argument takes it as `mut b: buffer`, and the caller must pass a mutable value (see [Mutable parameters](#mutable-parameters)). An index outside the buffer stops the program with a runtime error.
- `buffer_from(s)` copies a string's UTF-8 bytes into a new buffer. `string_from(b)` copies a buffer into a new string, and fails with `StringError.InvalidUtf8` when the bytes aren't valid UTF-8. Changing the buffer afterwards never changes the string.
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
echo("Hello, $name! You are $age years old.")
let a = 2
let b = 3
echo("Sum: ${a + b}")
echo("Nested: ${"Hello, $name"}")
echo("A literal dollar: \$name")
```

`$name` consumes a complete identifier, including underscores and digits after its first character. Use braces to separate a name from following text (`"${name}_suffix"`), access a member (`"${item.field}"`), index an array or call a function. A dollar sign that starts neither an identifier nor `${` stays literal, as in `"$5"`.

Expressions are evaluated and formatted exactly once, from left to right. Values use the current `echo` formatting, including arrays, structs, unions, nullable values and `null`, without adding a newline. An empty `${}`, an unknown name or a `void` result is a compile error. Failing calls require `try` or `catch`, as elsewhere. Expressions can contain nested strings, interpolations, conditionals and comments; error locations refer to the original source.

Escaped dollars (`\$`) and dollars produced by Unicode escapes (`\u{24}`) remain literal and do not start interpolation.

### Arrays

Arrays are growable lists of a single type. The type is written `[]T`.

```nim
let numbers = [1, 2, 3]
let names: []string = ["Alice", "Bob"]
let mut empty: []int = []

echo(numbers[0])        # 1
echo(len(numbers))      # 3
append(empty, 4)
```

Indexing starts at 0. Reading past the end is a runtime error. `append` needs a mutable array.

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

echo(user.username)
echo(user.display_name())
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

echo(Api.host)
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
echo(c.address.city)
```

### Anonymous initialisation

`@(...)` builds a value of the struct type expected at that point: an annotated `let`, an argument, a return value or a field.

```nim
struct Point
    x: int
    y: int
end

func show(p: Point) -> void
    echo(p.x)
end

let a: Point = @(x = 1, y = 2)
show(@(x = 3, y = 4))
```

`let b = @(x = 1, y = 2)` without an annotation creates a new anonymous struct type with those fields.

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

echo(Direction.DEFAULT.is_north())
```

- Enums support `==` and `!=`, but not `<`, `>` or arithmetic.
- An enum is a different type from `int`: `Color.Red == 0` is a type error.
- `echo` prints an enum's number, not its name.

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
    echo(message)
end
```

To handle every variant, use [`when`](#when):

```nim
when fetch(2)
    success: message then echo(message) end
    failure: reason then echo(reason) end
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

echo(Shape(square = 3).area())
```

- `==` and `!=` can't compare unions: check the variant with `when` or `if u.variant: value then ... end`.
- `echo` on a union prints `<union>`.

## 7. Nullables

A `?` before a type allows `null` as a value. It works for primitives, enums, structs, unions, arrays (`?[]int`) and error sets. `[]?int` is an array whose items can be null.

```nim
let mut name: ?string = null
name = "Dusk"

let mut count: ?int = 5
count = null
```

A nullable can't be used directly as its inner type. Unwrap it first.

`==` and `!=` are the exception: nullable primitives and enums (`?int`, `?float`, `?bool`, `?byte`, `?Enum`) compare by value. `count == 5` is true when `count` holds `5`, two nulls are equal and `null` is never equal to a value. Other nullables (`?[]int`, `?Struct`) compare like their inner type: identity, with `null` equal only to `null`. `?string` can only be compared with `null`.

### Capture

`if value: name then` runs the block only when the value isn't null, with `name` bound to the inner value. An `else` block is allowed.

```nim
if name: n then
    echo(n)
else
    echo("no name")
end
```

`if value: mut name` captures the value so it can be changed through `name`: setting a field, an item or appending changes the original. It needs a mutable subject (a `let mut` variable or a path from one), and only works for arrays, buffers, structs and unions: a `mut` capture of a primitive, string or enum is an error. The capture itself can't be reassigned (`name = other` is an error), since that would only change the local name.

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
echo(user.country?.name)   # null
```

Using `?.` on a value that can't be null is an error.

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

`==`, `!=`, `<`, `<=`, `>`, `>=` compare numbers. `==` and `!=` also work on `bool`, enums and errors. `and`, `or` and `!` work on `bool`.

`==` and `!=` only compare values of the same type: `true == 1`, `1 == "a"` and a `Point` with a `Size` are compile errors, since they could never be equal. Numbers are the exception: an `int`, `float` or `byte` compares with any other number (`1 == 1.0`). A nullable compares with `null` or with its inner type, and `x == null` is an error when `x` can't be null.

On arrays, structs and buffers, `==` and `!=` compare identity: two separately built `[1, 2]` are not equal, and a value is equal to its `copy` only if it's the same object. There are no content-comparison functions for them yet. Unions can't be compared with `==`.

`==` and `!=` can't compare strings: use `str_equal(a, b)` to compare their text.

### Assignment

`=` assigns. There are no compound assignment operators: write `x = x + 1`, not `x += 1`.

## 9. Control Flow

### If

```nim
if x > 10 then
    echo("big")
elif x > 0 then
    echo("small")
else
    echo("zero or less")
end
```

The condition must be a `bool`. For nullable capture (`if value: name then`), see [Nullables](#capture). Both forms can also [produce a value](#conditional-expressions).

### When

`when` runs the case that matches a value. It works on `int`, `byte`, `float`, `string`, enums, unions, error sets and their nullable versions. Each case is `pattern then ... end`, and the whole `when` ends with `end`. Cases never fall through.

```nim
when color
    Color.Red then echo("red") end
    Color.Green, Color.Blue then
        echo("cold")
    end
end

when code
    200 then echo("ok") end
    404, 410 then echo("gone") end
    else then echo("other") end
end

when command
    "start", "run" then echo("go") end
    "stop" then echo("halt") end
    else then echo("unknown") end
end
```

Union cases name a variant and capture its payload. Use `_` to ignore it:

```nim
when shape
    circle: r then echo(3 * r * r) end
    square: _ then echo("square") end
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
    null then echo("none") end
    circle: r then echo(r) end
    square: s then echo(s) end
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
- String cases must be string literals without interpolation (`"stop"`, `"\u{41}"`). They compare by content, like `str_equal`. Two literals with the same text after escapes (`"A"` and `"\u{41}"`) are the same case.
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
    echo(i)
    i = i + 1
end
```

Arrays can be traversed with a capture loop. The first capture is the item; the optional second capture is its zero-based `int` index:

```nim
let items = [10, 20, 30]
for items: item, index do
    echo("$index: $item")
end

for items: item do
    echo(item)
end
```

Use `_` to ignore either capture, as in `for items: _, index do ... end`. Captures are immutable and visible only inside the loop body. They can shadow outer names, which become visible again after the loop. Assignments and mutations through a capture are errors. `for items: mut item do` makes the item capture mutable, so changes through it (fields, items, `append`) change the array's element. It needs a mutable source and items that are arrays, buffers, structs or unions, and the capture can't be reassigned. To replace elements, or to change items of other types, use a mutable source array and its index.

The source can be any expression returning a non-nullable array, buffer or string. Unwrap nullable values first. Buffers and strings are traversed byte by byte: the item is a `byte` and the index its byte offset. The loop evaluates the source and its length once, retains that array, and reads each item when its iteration begins. Empty arrays run zero iterations. Appended items are not visited; changes to upcoming elements are visible. Reassigning the source variable does not change the array being traversed. Heap values retain their ordinary shared identity; iteration does not clone them.

A range keeps its lower and upper bounds separate. Ranges are supported directly in capture loops and `when` cases; they cannot be stored in variables, passed as arguments or returned as values yet.

Integer ranges use the same capture syntax, without allocating an array:

```nim
for 1..5: value, index do
    echo("$index: $value")
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
    echo(n)   # 1, 3, 5, 7
end
```

## 10. Functions

```nim
func add(a: int, b: int) -> int
    return a + b
end

func greet(name: string) -> void
    echo(name)
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

echo(scale(10))      # 20
echo(scale(10, 3))   # 30
```

### Named arguments

Arguments can be passed by name, in any order. A call uses either all named or all positional arguments.

```nim
echo(scale(factor = 5, x = 1))   # 5
```

### Mutable parameters

Parameters are immutable. Arrays, buffers, structs and unions can be taken as `mut` to change the caller's value through them: items, fields, `append`, and union payloads through `mut` captures. The argument must then be mutable: a `let mut` variable or a field or item of one (`grow(config.items)`), or a new value such as `[1, 2]`. Primitive, string and enum parameters can't be `mut`.

A `mut` parameter can't be reassigned (`list = []` is an error): the new value would only change the local name, not the caller's variable. To replace a value, return the new one and assign it at the call site, or change it through a `mut self` method on a struct that holds it.

```nim
func push_one(mut list: []int) -> void
    append(list, 1)
end

let mut items = [10, 20]
push_one(items)
echo(items)   # [10, 20, 1]
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

A function can see its parameters, its own variables, other functions and types, static fields and top-level [constants](#constants). It can't see the variables of other functions, including `main`: state a function reads or changes must be passed to it, with `mut` to change it. Default values (field and parameter defaults) can only use constants.

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
    echo(stats.count)  # 1
end
```

Functions can be called before they are declared.

## 11. Generics and Traits

A **type slot** is a placeholder for a type, written in brackets after a function, struct or union name. Each use fills the slots with real types, and the compiler makes one copy of the declaration per set of types.

```nim
func first[T](xs: []T) -> T
    return xs[0]
end

echo(first([1, 2]))         # first[int]
echo(first(["a", "b"]))     # first[string]
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

A slot that nothing fills is an error (`Can't work out 'T' for 'empty'`). There's no way to write the types in a call, like `empty[int]()`. A generic function can only be called, not used as a value.

### Generic structs and unions

Structs and unions take slots too. In types they're written with their arguments, `Stack[int]`. When building one, the slots come from the field or variant values, or from the expected type.

```nim
struct Stack[T]
    items: []T

    func push(mut self: @, item: T) -> void
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

A generic body is checked once, where it's written, with `T` standing for any type. Only what works for every type is allowed: `T` can be stored, passed, returned, put in arrays, interpolated and given to `echo`, `copy` and `append`. Arithmetic, comparisons and method calls on `T` need a constraint (below). A `T` param can't have a default value or be `mut` without a trait constraint.

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

echo(sum([1, 2, 3]))        # 6
echo(sum([0.5, 1.5]))       # 2
sum(["a"])                  # error: 'string' doesn't fit T: Number
```

Each operator on `T` is checked for every member with the usual rules. It's allowed when every member supports it, and its type is:

- the type every member gives, if they all give the same one: `x / 2` is a `float`, `a < b` a `bool`
- otherwise `T`, when each member gives itself back: `a + b` on `int | float`
- otherwise an error: on `byte | float`, `a + b` gives `int` for `byte` and `float` for `float`

A value converts to or from `T` when it converts for every member: `let total: T = 0` works for `Number` (`0` fits `int` and `float`), `let x: T = 2.5` doesn't. `==` and `!=` on `T` need a list without `string`, since strings can't be compared with `==` (use `str_equal`). `x == null` works on any `?T`.

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

An error is written like an enum variant (`ParseError.Empty`). It can be stored in variables, fields, arrays and nullables (`?ParseError`), passed around, and compared with `==` and `!=`, also against errors from other sets. `echo` prints its number.

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
    echo(e)
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
    echo(e)
end

double(-1) catch e then
    report(e)
end
```

### Runtime errors

Errors are for failures the program expects. A failed `assert`, an index out of bounds, an integer `//` or `%` by `0`, an `int` result that doesn't fit in 64 bits or more than 100,000 nested calls still stops the program and can't be caught.

## 13. Built-in Functions

`T` means any type.

| Function      | Signature                                         | Description                                                                          |
| :------------ | :------------------------------------------------ | :----------------------------------------------------------------------------------- |
| `echo`        | `echo(value: T) -> void`                          | Prints a value and a newline.                                                        |
| `assert`      | `assert(cond: bool) -> void`                      | Stops the program with an error if `cond` is false.                                  |
| `len`         | `len(list: []T) -> int`                           | Returns the number of items in an array.                                             |
| `buffer_len`  | `buffer_len(data: buffer) -> int`                 | Returns the number of bytes in a buffer.                                             |
| `new_buffer`  | `new_buffer(size: int) -> BufferError!buffer`     | Returns a new buffer of `size` zero bytes; fails with `BufferError.NegativeSize` for a negative size. |
| `to_byte`     | `to_byte(value: int) -> ConversionError!byte`     | Converts an `int` or `byte` to `byte`; fails with `ConversionError.Overflow` outside `0..255`. |
| `to_float`    | `to_float(value: int) -> float`                   | Converts an `int`, `byte` or `float` to `float`.                                     |
| `str_len`     | `str_len(text: string) -> int`                    | Returns the number of bytes in a string (UTF-8).                                     |
| `append`      | `append(mut list: []T, item: T) -> void`          | Adds an item to the end of a mutable array.                                          |
| `copy`        | `copy(value: T) -> T`                             | Returns a deep copy of an array, buffer, struct or union (see below).                |
| `buffer_from` | `buffer_from(text: string) -> buffer`             | Copies a string's bytes into a new buffer.                                           |
| `string_from` | `string_from(data: buffer) -> StringError!string` | Copies a buffer into a new string; fails with `InvalidUtf8` if it isn't valid UTF-8. |

`echo` prints primitives, arrays and nullables. It prints enums as their number, unions as `<union>`, and structs as raw values.

`copy(x)` copies everything reachable from `x`: nested arrays, struct fields, union payloads and buffers, so changing the copy never changes the original (shared references inside `x`, including cycles, are kept shared within the copy). Strings and nullable primitives are immutable, so they are shared instead of copied, and `copy` of an `int`, `float`, `bool`, `byte`, string or enum returns the value itself.

### Array functions

Functions that change an array take it as `mut`, and the ones that can fail return an error union, handled with `try` or `catch`.

| Function | Returns | Description |
| :------- | :------ | :---------- |
| `pop(mut list)` | `ArrayError!T` | Removes the last item and returns it. Fails with `ArrayError.Empty` on an empty array. |
| `pop_or_null(mut list)` | `?T` | Removes the last item and returns it, or `null` on an empty array. |
| `insert(mut list, index, item)` | `ArrayError!void` | Puts `item` at `index`, moving the later items up. `index` can be `len(list)` (the end). Fails with `ArrayError.OutOfRange` outside `0..len(list)`. |
| `remove(mut list, index)` | `ArrayError!T` | Removes the item at `index`, moving the later items down, and returns it. Fails with `ArrayError.OutOfRange` outside the array. |
| `clear(mut list)` | `void` | Removes every item. |
| `reverse(mut list)` | `void` | Reverses the items in place. |
| `slice(list, start, stop)` | `ArrayError![]T` | A new array with the items from `start` up to, not including, `stop`. The items are copies, so changing the slice never changes `list`. Fails with `ArrayError.OutOfRange` for a negative bound, `start > stop` or `stop > len(list)`. |
| `index_of(list, item)` | `?int` | The position of the first item equal to `item`, or `null`. |
| `contains(list, item)` | `bool` | Whether some item equals `item`. |

`index_of` and `contains` take arrays of `int`, `float`, `byte`, `bool` or `string`, and compare strings by content.

```nim
let mut stack = [1, 2, 3]
for true do
    let item = pop(stack) catch _ then break end
    echo(item)   # 3, 2, 1
end
```

### String functions

String functions are builtins starting with `str_`. They never change their argument: a function that produces text returns a new string. They work with pipes: `line |> str_trim()`.

Positions are **byte offsets**, the same unit as `str_len(s)` and `str_byte_at`, so a position from `str_find` can be passed straight to `str_slice`. `str_char_count` and `str_char_at` are the only functions that count characters.

| Function                               | Returns                 | Description                                                                                                                                                       |
| :------------------------------------- | :---------------------- | :---------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `str_equal(a, b, ignore_case = false)` | `bool`                  | `true` when both strings have the same text. With `ignore_case`, ASCII and Latin-1 letters match regardless of case.                                              |
| `str_char_count(text)`                 | `int`                   | Number of characters (Unicode code points).                                                                                                                       |
| `str_concat(a, b)`                     | `string`                | `b` after `a`.                                                                                                                                                    |
| `str_join(parts, sep = "")`            | `string`                | The `[]string` parts with `sep` between them (nothing by default); `""` for an empty array.                                                                       |
| `str_byte_at(text, index)`             | `StringError!byte`      | The byte at byte offset `index`.                                                                                                                                  |
| `str_char_at(text, index)`             | `StringError!string`    | The `index`-th character, as a one-character string (`str_char_at("olá", 2)` is `"á"`). Scans from the start.                                                     |
| `str_find(text, needle)`               | `?int`                  | Byte offset of the first match, `null` if there is none or `needle` is empty.                                                                                     |
| `str_contains(text, needle)`           | `bool`                  | Whether `needle` appears in `text`; `false` for an empty `needle`.                                                                                                |
| `str_starts_with(text, prefix)`        | `bool`                  | Whether `text` starts with `prefix`; `false` for an empty `prefix`.                                                                                               |
| `str_ends_with(text, suffix)`          | `bool`                  | Whether `text` ends with `suffix`; `false` for an empty `suffix`.                                                                                                 |
| `str_count(text, needle)`              | `int`                   | Number of non-overlapping matches (`"aaaa"`, `"aa"` is 2); `0` for an empty `needle`.                                                                             |
| `str_trim(text)`                       | `string`                | Removes spaces, tabs and line breaks (`\t`, `\n`, `\r`, `\v`, `\f`) from both ends.                                                                               |
| `str_upper(text)` / `str_lower(text)`  | `string`                | Changes the case of ASCII and Latin-1 letters (`"ação"` ↔ `"AÇÃO"`). Other characters are unchanged.                                                              |
| `str_split(text, separator)`           | `[]string`              | The pieces between separators, keeping empty ones: `"a,,b"` gives `["a", "", "b"]`. An empty `separator` splits into characters: `"olá"` gives `["o", "l", "á"]`. |
| `str_replace(text, old, new)`          | `string`                | Replaces every non-overlapping `old`, left to right. An empty `new` deletes; an empty `old` returns `text` unchanged.                                             |
| `str_repeat(text, count)`              | `StringError!string`    | `text` repeated `count` times; `0` gives `""`.                                                                                                                    |
| `str_slice(text, start, stop)`         | `StringError!string`    | The bytes from `start` up to, not including, `stop`: `str_slice(s, 0, str_len(s))` is all of `s`.                                                                 |
| `str_to_int(text)`                     | `ConversionError!int`   | Reads an int written like a Dusk literal.                                                                                                                         |
| `str_to_float(text)`                   | `ConversionError!float` | Reads a float (or int) written like a Dusk literal.                                                                                                               |

**Errors.** Functions that can fail return an error union, handled with `try` or `catch` like any failing call. Four builtin error sets exist (`ArrayError` is described with the [array functions](#array-functions)), and their names can't be used for other error sets:

| Error | When |
| :---- | :--- |
| `StringError.OutOfRange` | `str_slice` with a negative bound, `start > stop`, `stop > str_len(text)`, or a bound inside a multi-byte character; `str_byte_at` / `str_char_at` with an index outside the string. |
| `StringError.NegativeCount` | `str_repeat` with a negative `count`. |
| `StringError.InvalidUtf8` | `string_from` with bytes that aren't valid UTF-8. |
| `ConversionError.Invalid` | The text isn't a number. `str_to_int` accepts an optional `-` and digits; `str_to_float` also accepts one `.` followed by digits. Spaces, `+`, `_`, exponents, hex, `inf` and `nan` are invalid. |
| `ConversionError.Overflow` | An int that doesn't fit in 64 bits, a float too large to represent, or a `to_byte` value outside `0..255`. |
| `BufferError.NegativeSize` | `new_buffer` with a negative size. |

```nim
let line = "  name = Dusk, age = 13  "
for str_split(str_trim(line), ","): field do
    let pair = str_split(field, "=")
    echo("${str_upper(str_trim(pair[0]))}: ${str_trim(pair[1])}")
end

let at = str_find(line, "=") orelse 0
let name = str_slice(line, 0, at) catch ""

func describe_age(text: string) -> string
    let age = str_to_int(text) catch e then
        return when e
            ConversionError.Invalid then "not a number" end
            ConversionError.Overflow then "too big" end
        end
    end
    return "age $age"
end
echo(describe_age("13"))     # age 13
echo(describe_age("abc"))    # not a number
```

Optional parameters follow the usual call rule: a call is all positional or all named, so write `str_equal(a, b, true)` or `str_equal(a = x, b = y, ignore_case = true)`.

## 14. Errors

Compile errors point to the line and column of the problem:

```
error: Invalid type: expected 'int' found 'string' at line 1 col 14
--> 1: let x: int = "dusk"
                    ^~~~~~^
```

The program doesn't run if it has a compile error. Runtime errors (a failed `assert`, an index out of bounds, too many nested calls, an error returned from `main`) stop the program. For errors a program can handle, see [Error Handling](#12-error-handling).

## 15. Known Limitations

These are known gaps in v0.14 development. Most are planned in the [roadmap](roadmap.md).

- **Registers**: a function can have at most 255 variables and temporaries alive at once. Going over is a compile error; split the code into smaller functions.
- **Call depth**: at most 100,000 nested calls. Going deeper stops the program with a runtime error. A call returned directly (`return f(x)`, also in a `void` function) is a tail call: it reuses the current call instead of nesting, so tail-recursive functions can run any number of steps. In a function that can fail, `return try f(x)` is a tail call too when `f` succeeds with the same type (a `-> E!?int` function returning `try` of an `E!int` call isn't, since the result is converted first).
- **Strings**: Positions are byte offsets; only `str_char_count` and `str_char_at` count characters (`str_char_at` scans from the start), and case changes cover ASCII and Latin-1 letters only.
- **Building strings in a loop**: `s = str_concat(s, x)` copies the whole string every time. To build a long string, collect the pieces in an array and call `str_join` once.
- **String literals**: a program can have at most 65,536 different string literals.
- **Printing**: `echo` doesn't print struct fields or union variants.
- **Read-only values**: a function call counts as a new value, so a function that returns its own non-`mut` parameter (`func keep(items: []int) -> []int return items end`) lets `let mut y = keep(frozen)` change `frozen`. Generic code storing a bare `T` (`append(self.items, item)` with `item: T`) isn't checked either.
- **Union size**: a union can have at most 256 variants.
- **Array search**: `index_of` and `contains` don't take arrays of enums, structs, unions, arrays or buffers yet.
- **Builtin names**: a top-level function can't use the name of a builtin (`pop`, `insert`, `reverse`, ...).
- **Generics**: type arguments can't be written in a call (`empty[int]()`); slots come from the arguments or the expected type. Methods can't have their own slots, traits can't require static methods or fields, and type lists hold primitives only. A generic copy calling itself with a bigger type (`f([x])` inside `f[T]`) stops after 64 nested copies.
