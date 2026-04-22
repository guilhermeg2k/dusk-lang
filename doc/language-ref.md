# Language Reference

This document provides a reference for **v0.5::Chisato** of Dusk programming language.

## 1. Comments

Dusk supports single-line comments starting with the `#` character.

```rust
# This is a comment
let x: number = 10 # Inline comment
```

## 2. Variables & Data Types

### Declaration

Variables are declared using the `let` keyword. Type annotations are **optional** if the type can be inferred from the value.

```rust
# Type is inferred from the value
let name = "Dusk" # string
let count = 100   # number

# Explicit type annotation still works
let is_active: bool = true
```

### Mutability

Variables are immutable by default. To make a variable mutable, use the `mut` keyword.

```rust
let x: number = 10
# x = 20  <-- Error: Immutable variable

let mut y: number = 10
y = 20    # OK
```

### Primitive Types

| Type     | Description                                       | Example             |
| :------- | :------------------------------------------------ | :------------------ |
| `number` | 64-bit floating-point number (doubles as integer) | `42`, `3.14`, `-10` |
| `string` | Sequence of characters (double quotes)            | `"Hello World"`     |
| `bool`   | Boolean value                                     | `true`, `false`     |
| `void`   | Represents no value (mostly for return types)     | `void`              |

### Arrays

Arrays are dynamic lists of elements of the same type.

- **Type Syntax**: `[]Type` (e.g., `[]number`, `[]string`)
- **Literal Syntax**: `[val1, val2, ...]`

```rust
let numbers: []number = [1, 2, 3, 4]
let names: []string = ["Alice", "Bob"]
```

### Structs

Structs are custom data types that group related data and methods.

#### Definition

Structs are defined using the `struct` keyword.

```rust
let User = struct
    id: string
    username: string

    # Method
    to_string: (self: @) -> string
        return concat(self.id, self.username)

    # Mutable Method
    change_id: (mut self: @, id: string) -> void
        self.id = id
```

The `@` symbol refers to the current struct type.

#### Initialization

Structs are initialized by calling the struct name as a function. Named parameters are supported and recommended for clarity.

```rust
let user = User(id="1", username="Geromel")

# Multi-line initialization
let user2 = User(
    id = "2",
    username = "Dusk",
)
```

#### Accessing Fields and Methods

Fields and methods are accessed using the dot (`.`) operator.

```rust
echo(user.username)
user.change_id("100")
echo(user.to_string())
```

## 3. Operators

### Arithmetic

| Operator | Description    |
| :------- | :------------- |
| `+`      | Addition       |
| `-`      | Subtraction    |
| `*`      | Multiplication |
| `/`      | Division       |
| `%`      | Modulo         |

### Comparison

| Operator | Description              |
| :------- | :----------------------- |
| `==`     | Equal to                 |
| `!=`     | Not equal to             |
| `<`      | Less than                |
| `<=`     | Less than or equal to    |
| `>`      | Greater than             |
| `>=`     | Greater than or equal to |

### Logical

| Operator | Description |
| :------- | :---------- |
| `and`    | Logical AND |
| `or`     | Logical OR  |
| `!`      | Logical NOT |

### Assignment

| Operator | Description         | Equivalent to |
| :------- | :------------------ | :------------ |
| `+=`     | Add and assign      | `a = a + b`   |
| `-=`     | Subtract and assign | `a = a - b`   |

## 4. Control Flow

Dusk uses indentation (4 spaces) to define blocks.

### If / Else If / Else

Dusk supports `else if` for multiple conditions.

```rust
if x > 10
    echo("Greater than 10")
else if x > 0
    echo("Between 1 and 10")
else
    echo("0 or less")
```

### Loops

Dusk supports `for` loops, which can be conditional (like a `while` loop) or infinite.

**Conditional Loop:**

```rust
let mut i: number = 0
for i < 5
    echo(i)
    i += 1
```

**Infinite Loop:**

An infinite loop is created using `for true`.

```rust
let mut x = 10
for true
    echo(x)
    x -= 1
    if x < 1
        break # exit the loop
```

### Loop Control Statements

Dusk provides `break` and `continue` to control loop execution.

- **`break`**: Immediately terminates the innermost loop.
- **`continue`**: Skips the current iteration and proceeds to the next one.

**Example with `continue` and `break`:**

```rust
let mut i = 0
for i < 10
    i += 1
    if i % 2 == 0
        continue # Skip even numbers

    if i > 7
        break # Exit loop

    echo(i) # Only prints 1, 3, 5, 7
```

## 5. Functions

Functions are first-class citizens, return types are only inferred for inline returns
otherwise are mandatory

### Definition

The compiler can infer that `add` is a function.

```rust
let add = (a: number, b: number) -> number
    return a + b
```

### Inline Return

For functions with a single return expression, you can use the inline `return`

```rust
let add = (a: number, b: number) -> return a + b
```

### Void Functions

If a function does not return a value, use `void`.

```rust
let greet: fn = (name: string) -> void
    echo(concat("Hello ", name))
```

### Mutable Parameters

By default, function parameters are immutable. To allow a function to modify a parameter (like an array), you can use the `mut` keyword. This is especially useful for in-place modifications.

```rust
# The mut keyword allows this function to modify list
let add_one = (mut list: []number) -> void
    append(list, 1)

let mut my_list = [10, 20]
add_one(my_list)
echo(my_list) # Prints [10, 20, 1]
```

### Calling Functions

Functions can be called using positional arguments or named parameters.

```rust
let sum: number = add(5, 3)
greet("Dusk")

# Named parameters
let result = add(a=10, b=20)
```

Dusk also supports multi-line function calls for better readability.

```rust
let total = add(
    a = 5,
    b = 15,
)
```

## 6. Built-in Functions

Dusk provides a set of intrinsic functions.

| Function    | Signature                                       | Description                                      |
| :---------- | :---------------------------------------------- | :----------------------------------------------- |
| `echo`      | `echo(msg: dynamic) -> void`                    | Prints a value to stdout, followed by a newline. |
| `len`       | `len(arr: []dynamic) -> number`                 | Returns the length of an array.                  |
| `append`    | `append(arr: []dynamic, item: dynamic) -> void` | Adds an item to the end of a mutable array.      |
| `floor`     | `floor(n: number) -> number`                    | Rounds a number down to the nearest integer      |
| `concat`    | `concat(s1: string, s2: string) -> string`      | Concatenates two strings.                        |
| `stringify` | `stringify(obj: dynamic) -> string`             | Returns a JSON string representation of a value. |

## 7. Program Structure

- **Entry Point**: The program executes from top to bottom. There is no `main` function requirement.
- **Scope**: Variables must be defined before use. However, **functions and structs are hoisted**, meaning they can be defined anywhere in the script and used before their declaration.

## 8. Runtime

- Currently it transpiles to JS and evals it using embed [quickjs](https://bellard.org/quickjs/)
