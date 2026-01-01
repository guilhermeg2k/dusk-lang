# Language Reference

This document provides a reference for **v0.2::Rukia** of Dusk programming language.

## 1. Comments

Dusk supports single-line comments starting with the `#` character.

```rust
# This is a comment
let x: number = 10 # Inline comment
```

## 2. Variables & Data Types

### Declaration

Variables are declared using the `let` keyword. Type annotations are **mandatory**.

```rust
let variable_name: type = value
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

| Type | Description | Example |
| :--- | :--- | :--- |
| `number` | 64-bit floating-point number (doubles as integer) | `42`, `3.14`, `-10` |
| `string` | Sequence of characters (double quotes) | `"Hello World"` |
| `bool` | Boolean value | `true`, `false` |
| `void` | Represents no value (mostly for return types) | `void` |

### Arrays

Arrays are dynamic lists of elements of the same type.

- **Type Syntax**: `[]Type` (e.g., `[]number`, `[]string`)
- **Literal Syntax**: `[val1, val2, ...]`

```rust
let numbers: []number = [1, 2, 3, 4]
let names: []string = ["Alice", "Bob"]
```

## 3. Operators

### Arithmetic

| Operator | Description |
| :--- | :--- |
| `+` | Addition |
| `-` | Subtraction |
| `*` | Multiplication |
| `/` | Division |
| `%` | Modulo |

### Comparison

| Operator | Description |
| :--- | :--- |
| `==` | Equal to |
| `!=` | Not equal to |
| `<` | Less than |
| `<=` | Less than or equal to |
| `>` | Greater than |
| `>=` | Greater than or equal to |

### Logical

| Operator | Description |
| :--- | :--- |
| `and` | Logical AND |
| `or` | Logical OR |
| `!` | Logical NOT |

## 4. Control Flow

Dusk uses indentation (4 spaces) to define blocks.

### If / Else

```rust
if x > 10
    echo("Greater than 10")
else
    echo("10 or less")
```

### Loops

Dusk currently supports `for` loops which act like `while` loops.

**Conditional Loop:**

```rust
let mut i: number = 0
for i < 5
    echo(i)
    i = i + 1
```

**Infinite Loop:**

```rust
for
    echo("Running forever...")
    # Use return to exit function, no break statement yet
```

## 5. Functions

Functions are first-class citizens and are defined using `let` with the `fn` type.

### Definition

```rust
let add: fn = (a: number, b: number) -> number
    return a + b
```

### Void Functions

If a function does not return a value, use `void`.

```rust
let greet: fn = (name: string) -> void
    echo(concat("Hello ", name))
```

### Calling Functions

```rust
let sum: number = add(5, 3)
greet("Dusk")
```

## 6. Built-in Functions

Dusk provides a set of intrinsic functions.

| Function | Signature | Description |
| :--- | :--- | :--- |
| `echo` | `echo(msg: dynamic) -> void` | Prints a value to stdout, followed by a newline. |
| `len` | `len(arr: []dynamic) -> number` | Returns the length of an array. |
| `append` | `append(arr: []dynamic, item: dynamic) -> void` | Adds an item to the end of a mutable array. |
| `floor` | `floor(n: number) -> number` | Rounds a number down to the nearest integer |
| `concat` | `concat(s1: string, s2: string) -> string` | Concatenates two strings. |

## 7. Program Structure

- **Entry Point**: The program executes from top to bottom. There is no `main` function requirement.
- **Scope**: Variables must be defined before use.
## 8. Runtime
- Currently it transpiles to JS and evals it using embed [quickjs](https://bellard.org/quickjs/).
