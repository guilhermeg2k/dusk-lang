# Changelogs

## v0.2 - Rukia

- [x] Single Line Comments
- [x] Full support for expressions and operators
- [x] Improve error messages
- [x] Arrays
- [x] Organize better internals functions
- [x] quickjs as embbed runtime
- [x] Remove the need of the keyword fn for function declaration

## v0.1 - Bocchi

**Core Philosophy:** Explicit, Top-down, Script-like execution. 

---

### 1. Program Structure

#### Script Execution

- **Entry Point:** No main function is required. The compiler reads the file from top to bottom and executes statements immediately.
- **Strict Ordering:** You must define a variable or function before you use it. (No hoisting).

---

### 2. Variables & Data

#### Declaration

- **Keyword:** let
- **Type Annotations:** MANDATORY for all data variables.
- **Mutability:** Variables are immutable by default.

#### Primitives

- number: 64-bit float/int (unified for JS target).
- string: double quoted literals. No interpolation (#{}) in v0.1.
- bool: true / false.
- void: Used for function return types.
- fn: used for defining functions

**Syntax:**

##### VALID

```
let mut x: number = 10
let name: string = 'Dusk'
let is_valid: bool = true
```

##### INVALID in v0.1

```
let x = 10              # Error: Missing type annotation
let msg = "Val: #{x}"   # Error: Interpolation not supported yet
```

---

## 3. Functions (First-Class)

### Definition

- **Syntax:** Uses let binding with the fn keyword.
- **Parameters:** Types are mandatory.
- **Return Type:** Mandatory after ->.

### Body

- **Indentation:** Uses Python-style indentation (INDENT/DEDENT).
- **Return:** Explicit return keyword is required.

**Syntax:**

### Function Definition

```
let add: fn = (a: number, b: number) -> number
    return a + b
```

### Empty Function

```
let start_engine: fn = () -> void
    echo('Engine started')
```

### Call

```
let result: number = add(5, 10)
```

---

## 4. Control Flow

### Conditionals

- Standard if, else.

```
if x > 10
    echo('Big')
else
    echo('Small')
```

### Loops

- **Strategy:** Only the Condition Loop (While-style) is supported in v0.1.
- **Syntax:** Uses for keyword followed by a boolean expression.

```
let i: number = 0

for i < 10
    echo(i)
    i = i + 1
```

---

## 5. Output (Intrinsics)

- **echo(expression)**: A built-in intrinsic function.
- **Behavior:** Prints the string representation of the primitive to stdout (compiles to console.log in JS).

---

## 6. Operators

**This version gonna only support simple binary operations**

**By that i mean only \<value> \<operator> \<value> is supported**

- **Math:** +, -, \*, /.
- **Logic:** and, or.
- **Comparison:** ==, !=, <, >, <=, >=.

---

## 7. Compiler Pipeline (v0.1)

1. Lexer: Generates Tokens + INDENT/DEDENT tokens.
2. Parser: Recursive descent; produces AST.
3. Semantic Analysis:
   - Validates types (e.g. number + number).
   - Resolves symbols (checks if variable exists).
   - Generates Universal IR (High-Level).
4. Backend (Bun):
   - Transpiles Universal IR to JavaScript.
   - Executes via bun run.

---
