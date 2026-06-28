# Language Reference

This document provides a reference for **v0.10::Frieren** of Dusk programming language.

## 1. Comments

Dusk supports single-line comments starting with the `#` character.

```nim
# This is a comment
let x: int = 10 # Inline comment
```

## 2. Variables & Data Types

### Declaration

Variables are declared using the `let` keyword. Type annotations are **optional** if the type can be inferred from the value.

```nim
# Type is inferred from the value
let name = "Dusk" # string
let count = 100   # int

# Explicit type annotation still works
let is_active: bool = true
```

### Mutability

Variables are immutable by default. To make a variable mutable, use the `mut` keyword.

```nim
let x: int = 10
# x = 20  <-- Error: Immutable variable

let mut y: int = 10
y = 20    # OK
```

### Primitive Types

| Type     | Description                                   | Example             |
| :------- | :-------------------------------------------- | :------------------ |
| `int`    | 64-bit signed integer                         | `42`, `-10`         |
| `float`  | 64-bit floating-point number                  | `3.14`, `-0.5`      |
| `string` | Sequence of characters (double quotes)        | `"Hello World"`     |
| `bool`   | Boolean value                                 | `true`, `false`     |
| `void`   | Represents no value (mostly for return types) | `void`              |


### Arrays

Arrays are dynamic lists of elements of the same type.

- **Type Syntax**: `[]Type` (e.g., `[]int`, `[]string`)
- **Literal Syntax**: `[val1, val2, ...]`

```nim
let numbers: []int = [1, 2, 3, 4]
let names: []string = ["Alice", "Bob"]
```

### Structs

Structs are custom data types that group related data and methods.

#### Definition

Structs are defined using the `struct` keyword. Structs can also contain nested struct definitions and default values for fields. Note that **nested structs can only contain fields**, they cannot contain methods or static fields.

```nim
let User = struct
    id: string
    username: string
    auth_method = "DUSK" # Field with default value

    # Nested struct
    address: struct
        city: string
        country: string
    end

    # Method
    to_string: (self: @) -> string
        return self.username
    end

    # Mutable Method
    change_id: (mut self: @, id: string) -> void
        self.id = id
    end
end
```

The `@` symbol refers to the current struct type.

#### Initialization

Structs are initialized by calling the struct name as a function. Named parameters are supported and recommended for clarity.

```nim
let user = User(id="1", username="Geromel")
```

#### Accessing Fields and Methods

Fields and methods are accessed using the dot (`.`) operator.

```nim
echo(user.username)
user.change_id("100")
echo(user.to_string())
```

#### Static Fields and Methods

Structs can have static fields and methods. Static fields are declared with `let` inside the struct body. Methods can be called statically by passing the instance as the first argument.

**Note:** Struct members must follow a specific order:

1. **Static fields** (declared with `let` or `let mut`)
2. **Instance fields**
3. **Methods**

```nim
let Api = struct
    # Static fields
    let ip: string = "127.0.0.1"
    let mut port: string = "8080"

    # Instance field
    endpoint: string = ""

    # Methods
    fetch: (self: @) -> string
        return self.endpoint
    end
end

# Accessing static fields
Api.port = ":9090"

let api = Api(endpoint="/me")
echo(api.fetch())      # Instance call
echo(Api.fetch(api))   # Static call
```

#### Anonymous Structs

Dusk supports anonymous struct initialization using the `@(...)` syntax.

```nim
let Point2D = struct
    x: int
    y: int
end

let print_point = (point: Point2D) -> void
    echo(point)
end

let point = Point2D(x = 10, y = 10)
let point2: Point2D = @(x = 20, y = 20)
let point3 = @(x = 30, y = 30)

print_point(point)
print_point(point2)
print_point(point3)
print_point(Point2D(x = 50, y = 50))
print_point(@(x=40,y=40))
```

### Nullable Types

Dusk supports nullable types by prefixing the type with `?`. This means the value can either be of the specified type or `null`.

```nim
let mut name: ?string = null
name = "Dusk"
```

You can use optional chaining (`?.`) to safely access fields of nullable structs. If any part of the chain is `null`, the result is `null`.

```nim
let Country = struct
    name: string
end

let User = struct
    country: ?Country
end

let user = User(country=null)
echo(user.country?.name) # null
```

## 3. Operators

### Arithmetic

| Operator | Description    |
| :------- | :------------- |
| `+`      | Addition       |
| `-`      | Subtraction    |
| `*`      | Multiplication |
| `/`      | Division       |
| `//`     | Trunc division |
| `%`      | Modulo         |

#### Arithmetic Typing Rules

Dusk supports implicit promotion of integers to floats when mixed in binary expressions. The resulting type of an arithmetic operation is defined as follows:

- **Division (`/`)**: Always returns a `float`, even if both operands are `int`.
- **Trunc Division (`//`)**: Always returns an `int`. It divides the left operand by the right, then truncates the result to an integer.
- **Other Operators (`+`, `-`, `*`, `%`)**: Returns a `float` if at least one operand is a `float`. Otherwise (if both operands are `int`), returns an `int`.


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

Dusk uses keyword-delimited blocks (`then`/`do`/`end`) and indentation for readability.

### If / Elif / Else

Dusk supports `elif` for multiple conditions.

```nim
if x > 10 then
    echo("Greater than 10")
elif x > 0 then
    echo("Between 1 and 10")
else
    echo("0 or less")
end
```

### Nullable Capture

You can safely unwrap nullable values using the `if value: captured` syntax. The captured value is only available within the `if` block. You can also capture it as a mutable value using `if value: mut captured`.

```nim
let mut username: ?string = null

if username: name then
    echo(name)
end

username = "Dusk"
if username: mut name then
    name = "Dusk 2.0"
end
```

> **Note on Mutability:** Capturing a primitive value (like `int`, `float`, `string`, `bool`) as `mut` will only mutate the captured local variable, not the original value. Mutating the original value through a capture only works for reference types like structs and arrays.

### Loops

Dusk supports `for` loops, which can be conditional (like a `while` loop) or infinite.

**Conditional Loop:**

```nim
let mut i: int = 0
for i < 5 do
    echo(i)
    i += 1
end
```

**Infinite Loop:**

An infinite loop is created using `for true`.

```nim
let mut x = 10
for true do
    echo(x)
    x -= 1
    if x < 1 then
        break # exit the loop
    end
end
```

### Loop Control Statements

Dusk provides `break` and `continue` to control loop execution.

- **`break`**: Immediately terminates the innermost loop.
- **`continue`**: Skips the current iteration and proceeds to the next one.

**Example with `continue` and `break`:**

```nim
let mut i = 0
for i < 10 do
    i += 1
    if i % 2 == 0 then
        continue # Skip even numbers
    end

    if i > 7 then
        break # Exit loop
    end

    echo(i) # Only prints 1, 3, 5, 7
end
```

## 5. Functions

Return types are only inferred for inline returns; otherwise, they are mandatory.

### Definition

The compiler can infer function types in most cases.

```nim
let add = (a: int, b: int) -> int
    return a + b
end
```

#### Default Values

Function parameters can have default values. When a default value is provided, the type annotation becomes optional and is inferred from the default value.

```nim
let add = (x: int, y = 2) -> return x + y

echo(add(10))        # 12
echo(add(10, 5))     # 15
echo(add(y=10, x=5)) # 15
```

### Inline Return

For functions with a single return expression, you can use the inline `return`

```nim
let add = (a: int, b: int) -> return a + b
```

### Void Functions

If a function does not return a value, use `void`.

```nim
let greet: fn = (name: string) -> void
    echo(name)
end
```

### Mutable Parameters

By default, function parameters are immutable. To allow a function to modify a parameter (like an array), you can use the `mut` keyword. This is especially useful for in-place modifications.

```nim
# The mut keyword allows this function to modify list
let add_one = (mut list: []int) -> void
    append(list, 1)
end

let mut my_list = [10, 20]
add_one(my_list)
echo(my_list) # Prints [10, 20, 1]
```

### Calling Functions

Functions can be called using positional arguments or named parameters.

```nim
let sum: int = add(5, 3)
greet("Dusk")

# Named parameters
let result = add(a=10, b=20)
```

#### Pipe Operator

Dusk supports the pipe operator (`|>`), which allows passing the result of an expression as the first argument of a function call. This enables a clean, readable, left-to-right method-chaining-like syntax for nested function calls.

The right-hand side of the pipe operator must be a function call. Parentheses are required even for functions with a single parameter.

```nim
let add = (a: int, b: int) -> int
    return a + b
end

let is_even = (x: int) -> bool
    return (x % 2) == 0
end

# The following calls are equivalent:
# is_even(add(2, 1))
let res = 2 |> add(1) |> is_even()

assert(res == false)

let res2 = 2
    |> add(2)
    |> is_even()
```

## 6. Built-in Functions

Dusk provides a set of intrinsic functions.

| Function | Signature                                       | Description                                      |
| :------- | :---------------------------------------------- | :----------------------------------------------- |
| `assert` | `assert(cond: bool) -> void`                    | Raises a runtime error if the condition is false. |
| `echo`   | `echo(msg: dynamic) -> void`                    | Prints a value to stdout, followed by a newline. |
| `len`    | `len(arr: []dynamic) -> int`                    | Returns the length of an array.                  |
| `append` | `append(arr: []dynamic, item: dynamic) -> void` | Adds an item to the end of a mutable array.      |

## 7. Program Structure

- **Entry Point**: The program executes from top to bottom. There is no `main` function requirement.
- **Scope**: Variables must be defined before use. However, **functions and structs are hoisted**, meaning they can be defined anywhere in the script and used before their declaration.

## 8. Runtime

- Register-based virtual machine with a garbage collector.
