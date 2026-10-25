# Language Reference

This is the reference for **v0.11 - Power** (in development). Planned features are listed in the [roadmap](roadmap.md).

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
11. [Built-in Functions](#11-built-in-functions)
12. [Errors](#12-errors)
13. [Known Limitations](#13-known-limitations)

## 1. Basics

A program runs from top to bottom. There is no `main` function.

```nim
echo("Hello, Dusk")
```

**Comments** start with `#` and run to the end of the line.

```nim
# a comment
let x = 10 # an inline comment
```

**Blocks** are delimited by keywords: `then ... end`, `do ... end`, and `end` for definitions. Indentation is only for readability.

**Line breaks** aren't significant. A statement can span several lines, but two expressions on consecutive lines can also join into one: `when n` followed by `-1 then` on the next line is read as `when n - 1`.

**Hoisting**: functions, structs, enums and unions can be used before they're defined. Variables must be defined before use.

**Keywords**: `let`, `mut`, `func`, `return`, `struct`, `enum`, `union`, `if`, `elif`, `else`, `when`, `for`, `break`, `continue`, `then`, `do`, `end`, `and`, `or`, `true`, `false`, `null`, `int`, `float`, `string`, `bool`, `void`.

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

## 3. Types

### Primitives

| Type     | Description                                   | Examples            |
| :------- | :-------------------------------------------- | :------------------ |
| `int`    | 64-bit signed integer                         | `42`, `-10`         |
| `float`  | 64-bit floating-point number                  | `3.14`, `-0.5`      |
| `string` | Text in double quotes                         | `"Hello"`           |
| `bool`   | Boolean                                       | `true`, `false`     |
| `void`   | No value, used as a function return type      |                     |

Strings have no escape sequences (`"a\nb"` prints `a\nb` as written) and no operations yet. See [Known Limitations](#13-known-limitations).

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

Fields declared with `let` belong to the type, not to an instance. Use `let mut` to make them assignable.

```nim
struct Api
    let host = "127.0.0.1"
    let mut port = "8080"

    endpoint: string = ""

    func url(self: @) -> string
        return self.endpoint
    end
end

Api.port = "9090"
echo(Api.host)
```

Members must be written in this order: static fields, then instance fields, then methods. The same order applies to enums and unions.

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
    error: string
end

let ok = Response(success = "saved")
let failed = Response(error = "not found")
```

Build a union by calling its name with exactly one named variant. Where a union type is expected (annotated `let`, assignment, argument, return value, field or array item), `@(variant = value)` works too:

```nim
func fetch(id: int) -> Response
    if id == 1 then
        return @(success = "found")
    end
    return @(error = "missing")
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
    error: reason then echo(reason) end
end
```

### Changing a union

To change a union, assign a new one. This works for `let mut` variables, struct fields and array items:

```nim
let mut status = Response(error = "pending")
status = Response(success = "done")
status = @(error = "timeout")
```

Writing to a single variant (`status.success = "x"`) is an error. Union parameters, including `self`, can't be `mut` either, so a function that changes a union returns the new one:

```nim
func resolve(r: Response) -> Response
    return @(success = "resolved")
end

status = resolve(status)
```

### Methods and static fields

These work like they do for structs:

```nim
union Shape
    let mut created = 0

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

- `==` on unions compares identity, not contents.
- `echo` on a union prints `<union>`.

## 7. Nullables

A `?` before a type allows `null` as a value. It works for primitives, enums, structs and unions (not arrays yet).

```nim
let mut name: ?string = null
name = "Dusk"

let mut count: ?int = 5
count = null
```

A nullable can't be used directly as its inner type. Unwrap it first.

### Capture

`if value: name then` runs the block only when the value isn't null, with `name` bound to the inner value. An `else` block is allowed.

```nim
if name: n then
    echo(n)
else
    echo("no name")
end
```

`if value: mut name` makes the captured variable mutable. For structs, this changes the original value. For primitives (`int`, `float`, `bool`, `string`), it only changes the local copy.

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
| `\|>`                           | Pipe                  |
| `or`                            | Logical or            |
| `and`                           | Logical and           |
| `==` `!=` `<` `<=` `>` `>=`     | Comparison            |
| `+` `-`                         | Addition, subtraction |
| `*` `/` `//` `%`                | Multiplication, division, modulo |
| `-x` `!x`                       | Negation, logical not |
| `x.y` `x?.y` `x[i]` `f()`       | Access, index, call   |

Use parentheses to change the order.

### Arithmetic

| Operator | Result                                                        |
| :------- | :------------------------------------------------------------ |
| `+` `-` `*` `%` | `int` if both sides are `int`, otherwise `float`       |
| `/`      | Always `float`, even for two `int`s (`3 / 2` is `1.5`)        |
| `//`     | Always `int`: divides, then truncates (`7 // 2` is `3`)       |

An `int` mixed with a `float` is promoted to `float`.

### Comparison and logic

`==`, `!=`, `<`, `<=`, `>`, `>=` compare numbers. `==` and `!=` also work on `bool` and enums. `and`, `or` and `!` work on `bool`.

`==` on strings compares identity, not text: `"dusk" == "dusk"` is `false`. Real string comparison is planned for v0.18.

### Assignment

`=` assigns, and `+=` and `-=` add or subtract in place. There are no other compound operators (`*=`, `/=`).

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

The condition must be a `bool`. For nullable capture (`if value: name then`), see [Nullables](#capture).

### When

`when` runs the case that matches a value. It works on `int`, enums, unions and their nullable versions. Each case is `pattern then ... end`, and the statement ends with `end`. Cases never fall through.

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
```

Union cases name a variant and capture its payload. Use `_` to ignore it:

```nim
when shape
    circle: r then echo(3 * r * r) end
    square: _ then echo("square") end
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
- `int` subjects always need an `else`. Int cases must be literals (`1`, `-1`).
- Enum cases use the full name (`Color.Red`).
- A value or variant can only appear in one case.
- `string` and `float` subjects aren't supported yet.
- `break`, `continue` and `return` in a case act on the enclosing loop or function.

### Loops

`for condition do ... end` repeats while the condition is `true`. `for true do` loops until a `break`.

```nim
let mut i = 0
for i < 5 do
    echo(i)
    i += 1
end
```

`break` leaves the innermost loop, and `continue` jumps to its next iteration. Both are errors outside a loop.

```nim
let mut n = 0
for true do
    n += 1
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

Parameters are immutable. Arrays and structs can be taken as `mut` to change the caller's value, and the argument must then be a mutable variable. Primitive and union parameters can't be `mut`.

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

### Scope

A function can see its parameters, its own variables, other functions and types, and static fields. It **can't** see variables declared at the top level of the program (see [Known Limitations](#13-known-limitations)).

## 11. Built-in Functions

`T` means any type.

| Function | Signature                         | Description                                          |
| :------- | :-------------------------------- | :--------------------------------------------------- |
| `echo`   | `echo(value: T) -> void`          | Prints a value and a newline.                        |
| `assert` | `assert(cond: bool) -> void`      | Stops the program with an error if `cond` is false.  |
| `len`    | `len(list: []T) -> int`           | Returns the number of items in an array.             |
| `append` | `append(list: []T, item: T) -> void` | Adds an item to the end of a mutable array.       |

`echo` prints primitives, arrays and nullables. It prints enums as their number, unions as `<union>`, and structs as raw values.

## 12. Errors

Compile errors point to the line and column of the problem:

```
error: Invalid type: expected 'int' found 'string' at line 1 col 14
--> 1: let x: int = "dusk"
                    ^~~~~~^
```

The program doesn't run if it has a compile error. Runtime errors (a failed `assert`, an index out of bounds) stop the program.

## 13. Known Limitations

These are known gaps in v0.11. Most are planned in the [roadmap](roadmap.md).

- **Top-level variables inside functions**: reading a top-level `let` from a function fails with `UndefinedVariable`. Pass it as a parameter, or use a static field.
- **Strings**: no escape sequences, concatenation or interpolation, and `==` compares identity (v0.18).
- **Nullable arrays**: `?[]int` can't hold `null` yet.
- **Printing**: `echo` doesn't print struct fields or union variants.
- **Compound assignment**: only `+=` and `-=`.
- **Union size**: a union can have at most 256 variants.
