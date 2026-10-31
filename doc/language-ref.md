# Language Reference

This is the reference for **v0.12 - Kanade**. Planned features are listed in the [roadmap](roadmap.md).

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
11. [Error Handling](#11-error-handling)
12. [Built-in Functions](#12-built-in-functions)
13. [Errors](#13-errors)
14. [Known Limitations](#14-known-limitations)

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

Since v0.13, multiline comments use `#= ... =#` and can nest. They can appear between tokens, including on the same line. An unclosed comment is a lexer error pointing at its opening delimiter.

```nim
#= A multiline comment
   #= A nested comment =#
=#
let total = 1 #= explanation =# + 2
```

**Blocks** are delimited by keywords: `then ... end`, `do ... end`, and `end` for definitions. Indentation is only for readability.

**Line breaks** aren't significant. A statement can span several lines, but two expressions on consecutive lines can also join into one: `when n` followed by `-1 then` on the next line is read as `when n - 1`.

**Hoisting**: functions, structs, enums, unions and error sets can be used before they're defined. Variables must be defined before use.

**Keywords**: `let`, `mut`, `func`, `return`, `struct`, `enum`, `union`, `error`, `try`, `catch`, `if`, `elif`, `else`, `when`, `for`, `break`, `continue`, `then`, `do`, `end`, `and`, `or`, `orelse`, `true`, `false`, `null`, `int`, `float`, `string`, `bool`, `void`.

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

Strings have no escape sequences (`"a\nb"` prints `a\nb` as written) and no operations yet. See [Known Limitations](#14-known-limitations).

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

A `?` before a type allows `null` as a value. It works for primitives, enums, structs, unions, arrays (`?[]int`) and error sets. `[]?int` is an array whose items can be null.

```nim
let mut name: ?string = null
name = "Dusk"

let mut count: ?int = 5
count = null
```

A nullable can't be used directly as its inner type. Unwrap it first.

`==` and `!=` are the exception: a nullable compares by value. `count == 5` is true when `count` holds `5`, two nulls are equal and `null` is never equal to a value.

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

The condition must be a `bool`. For nullable capture (`if value: name then`), see [Nullables](#capture). Both forms can also [produce a value](#conditional-expressions).

### When

`when` runs the case that matches a value. It works on `int`, `float`, enums, unions, error sets and their nullable versions. Each case is `pattern then ... end`, and the whole `when` ends with `end`. Cases never fall through.

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
- `int` and `float` subjects always need an `else`, even if ranges appear to cover every value.
- Int cases can be literals (`1`, `-1`) or inclusive ranges (`1..5`). A case can combine them (`1..5, 8, 10..12 then ... end`). Range bounds must be int literals.
- Float cases must be inclusive ranges (`0.0..1.0`), not individual values. Bounds must be finite numeric literals; int bounds are promoted to float. NaN and infinities reach `else`.
- A case range must start at or below its end. Ranges and individual cases cannot overlap, including within a comma-separated case. Inclusive ranges `0.0..1.0` and `1.0..2.0` overlap at `1.0`.
- Enum cases use the full name (`Color.Red`).
- A value or variant can only appear in one case.
- `string` subjects aren't supported yet.
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
    i += 1
end
```

Arrays can be traversed with a capture loop. The first capture is the item; the optional second capture is its zero-based `int` index:

```nim
let items = [10, 20, 30]
for items: item, index do
    echo(item, index)
end

for items: item do
    echo(item)
end
```

Use `_` to ignore either capture, as in `for items: _, index do ... end`. Captures are immutable and visible only inside the loop body. They can shadow outer names, which become visible again after the loop. Assignments and mutations through a capture are errors; use a mutable source array and its index to update elements explicitly.

The source can be any expression returning a non-nullable array. Unwrap nullable arrays first. The loop evaluates the source and its length once, retains that array, and reads each item when its iteration begins. Empty arrays run zero iterations. Appended items are not visited; changes to upcoming elements are visible. Reassigning the source variable does not change the array being traversed. Heap values retain their ordinary shared identity; iteration does not clone them.

A range keeps its lower and upper bounds separate. Ranges are supported directly in capture loops and `when` cases; they cannot be stored in variables, passed as arguments or returned as values yet.

Integer ranges use the same capture syntax, without allocating an array:

```nim
for 1..5: value, index do
    echo(value, index)
end
```

`1..5` includes both endpoints, producing `1`, `2`, `3`, `4`, `5`. The optional index counts from zero. Bounds can be integer expressions, evaluated once from left to right before the loop starts. Arithmetic binds more tightly than `..`, so `start + 1..stop * 2` uses those arithmetic results as its bounds.

Iteration advances by one. Equal endpoints produce one item; a start greater than the end produces no items. The loop stops at its upper bound without incrementing past it, including at the largest `int`. Float range iteration, descending steps and open-ended ranges are not supported.

`break` leaves the innermost loop, and `continue` jumps to its next iteration. Both are errors outside a loop. In a capture loop, `continue` advances to the next item.

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

A function can see its parameters, its own variables, other functions and types, static fields, and the top-level variables declared above it. It can read and assign them (`let mut` ones only):

```nim
let mut count = 0

func bump() -> void
    count += 1
end

bump()
echo(count)   # 1
```

Functions can be called before they are declared, so a call must come after every top-level variable the function uses, directly or through the functions it calls. Calling `bump()` above `let mut count = 0` is a compile error. A default value (a static field, a field or a parameter default) can't call a function that uses top-level variables.

## 11. Error Handling

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
- At the top level, outside any function, a failing `try` stops the program: `error: unhandled error ParseError.BadDigit`.
- `try` applies to the call right after it, including `.` and `[]` (`try a.b(c)`).

### catch

`call catch value` uses `value` when the call fails. It must have the call's success type.

```nim
let n = parse_digit(-1) catch 0
let m = parse_digit(-1) catch parse_digit(2) catch 0
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

Errors are for failures the program expects. A failed `assert` or an index out of bounds still stops the program and can't be caught.

## 12. Built-in Functions

`T` means any type.

| Function | Signature                         | Description                                          |
| :------- | :-------------------------------- | :--------------------------------------------------- |
| `echo`   | `echo(value: T) -> void`          | Prints a value and a newline.                        |
| `assert` | `assert(cond: bool) -> void`      | Stops the program with an error if `cond` is false.  |
| `len`    | `len(list: []T) -> int`           | Returns the number of items in an array.             |
| `append` | `append(list: []T, item: T) -> void` | Adds an item to the end of a mutable array.       |

`echo` prints primitives, arrays and nullables. It prints enums as their number, unions as `<union>`, and structs as raw values.

## 13. Errors

Compile errors point to the line and column of the problem:

```
error: Invalid type: expected 'int' found 'string' at line 1 col 14
--> 1: let x: int = "dusk"
                    ^~~~~~^
```

The program doesn't run if it has a compile error. Runtime errors (a failed `assert`, an index out of bounds, a failing `try` at the top level) stop the program. For errors a program can handle, see [Error Handling](#11-error-handling).

## 14. Known Limitations

These are known gaps in v0.12. Most are planned in the [roadmap](roadmap.md).

- **Registers**: a function (or the top-level code) can have at most 255 variables and temporaries alive at once. Going over is a compile error; split the code into smaller functions.
- **Strings**: no escape sequences, concatenation or interpolation, and `==` compares identity (v0.18).
- **Printing**: `echo` doesn't print struct fields or union variants.
- **Compound assignment**: only `+=` and `-=`.
- **Union size**: a union can have at most 256 variants.
