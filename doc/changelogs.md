# Changelogs

## v0.12 - Kanade

- Nullable fallback: `value orelse fallback` evaluates its left operand once, unwraps it when non-null and evaluates the fallback only for null. Nullable fallbacks support chains; compatible result types use conditional-expression inference and numeric promotion
- `orelse` is now a reserved keyword. It groups right-to-left at the same precedence as `catch`, and supports contextual anonymous initializers and empty arrays

- Inclusive integer range loops: `for start..stop: value, index do ... end`, with optional indexes and ignored captures. Bounds are evaluated once, iteration allocates no array, and reversed bounds produce no items
- Integer `when` cases accept literal ranges and mixtures of ranges and individual literals. Float and nullable-float subjects are supported with range cases only; bounds are finite numeric literals
- Numeric `when` subjects require `else`; overlapping cases, shared endpoints and reversed case ranges are compile errors. Range loops stop safely at the largest integer

- Array capture loops: `for items: item, index do ... end`, with an optional zero-based index and `_` to ignore captures. Captures are immutable and scoped to the body
- Capture loops evaluate their array and length once, read each item as they reach it, and exclude appended items. `continue` advances to the next item

- `if`, nullable-capture `if` and `when` now work as expressions. Branches produce their final expression, with statements allowed before it; value-producing `if` requires `else`
- Conditional expressions infer compatible result types, promote mixed numeric results, support nullable results and propagate expected types into branch values, anonymous initializers and empty arrays
- Expression branches can `return`, `break` or `continue` without supplying a value. Existing statement forms retain their behavior
- `if` and `when` statements compile straight to branches again instead of going through the expression path, so statements don't run extra instructions. Statements and expressions now share one AST form and one lowering path
- Nullable captures evaluate their subject once, unwrap only in the matching branch and keep captured names out of `else`; union captures also preserve outer variables with the same name

- Error handling: error sets (`error ParseError ... end`), functions that can fail (`-> E!T`, or `-> !T` to infer the error set from the body), `try` to pass an error on, and `catch` with a fallback value (`f() catch 0`) or a block (`f() catch e then ... end`)
- A call to a function that can fail must be handled with `try` or `catch`. Sema checks that every error a function can return is in its declared set, also through chains of inferred sets and recursion
- Error values can be stored, passed to functions, compared with `==`/`!=` and matched with `when` (exhaustive, also for inferred sets). An error from an inferred set can go wherever a declared set is expected, as long as every error it can hold is in that set
- A failing `try` at the top level stops the program with `unhandled error <Set.Error>`
- `error`, `try` and `catch` are now keywords, so they can't be used as names (e.g. a union variant called `error`)
- Fixed compiler crash when a function calls another function defined after it (mutual recursion)
- Fixed `Register limit exceeded` crash in blocks with a few hundred `if`, `when`, nullable-capture `if`, capture `for` or call statements: the hidden variables each statement uses are now freed when it ends
- Fixed functions whose success type is an error set (`E!Kind`): `return Kind.A` now succeeds instead of being checked against `E`. Using `E` itself as the success type (`E!E`, `E!?E`) is a compile error
- Fixed `==`/`!=` on nullable primitives, enums and error sets comparing the null box instead of the value: `?int == int` and `?int == ?int` were always false
- Debug builds check that every call to a function that can fail is directly followed by its success jump, which `RETURN_ERR` relies on
- Fixed assigning into nested arrays: `grid[0][1] = 5` was rejected, `grid[0][1] = [7]` was accepted, and `bag.items[0] = 9` (an array inside a struct field) crashed the compiler. Assignment now checks against the element type of the array being indexed, and evaluates its target once
- A number with two dots (`1.2.3`) is reported as an invalid float literal again, instead of a confusing member-access error
- Fixed a nullable anonymous initializer (`if c then @(x = 1) else null end`) being accepted where a non-nullable struct is expected, which crashed at runtime. A nullable value never coerces to a non-nullable type, whatever its kind
- Fixed a GC crash when a nullable primitive variable started as `null` and was assigned later (`let mut n: ?int = null` then `n = 1` in an allocating loop)
- Fixed `if f() catch fallback then` failing to parse: inside an `if`/`elif` condition, `catch name then` is a fallback value. A catch block still works there inside parentheses or call arguments
- Fixed nullable arrays: `?[]int` variables, params, returns and struct fields can now hold `null`, and a nullable array must be unwrapped before indexing, `len` or `append`
- Fixed struct fields with both a type and a default (`retries: ?int = null`) taking the default's type instead of the declared one
- Fixed array literals ignoring the declared item type (`let a: []?int = [null, 1]`)
- Language reference: new Error Handling section

## v0.11 - Power

- First version developed with AI assistance (see [AI Usage](ai-disclaimer.md))
- Enums with explicit values, methods and static fields
- Unions (`union U ... end`), built with a single named variant: `U(variant = value)`
- Reading a union variant returns a nullable (`?T`), `null` when it isn't the active variant
- Union methods (`u.method()` and `U.method(u)`) and static fields
- Anonymous syntax for unions: `@(variant = value)` builds a union wherever a union type is expected (annotated `let`, assignment, arguments, returns, struct fields, payloads and array literals)
- `when` statement: branches on `int`, enums, unions (with payload capture) and their nullable versions, with a `null` case. Sema checks that every enum/union variant is handled, that `int` subjects have an `else`, that no `else` is unreachable, and that no case is repeated
- Union variant tags now follow declaration order
- Functions can return only from inside branches: an `if/elif/else` where every branch returns, or a `when` where every case returns, counts as returning
- Clearer error for `mut` union params (including `mut self`): union variants can't be changed in place
- Unions are limited to 256 variants, with a clear error past that
- Fixed nullable primitives (`?int`, `?float`, `?bool`) not being tracked by the garbage collector
- Fixed garbage collector losing arrays of unions and arrays of nullables
- Fixed `if value: captured` on a null primitive ending the program
- Fixed `echo` on nullable values
- Fixed compiler crash when calling a method on an indexed array item (`items[i].method()`)
- Fixed optional chaining (`?.`) crashing when a link in the chain is null, and when the chain ends in an `int`/`float`/`bool`/enum field
- Fixed compiler crash when assigning or passing an empty array literal (`list = []`)
- Fixed assigning a call result to a variable declared before other variables (`out = len(a)`) crashing or giving garbage
- Fixed passing a plain value to a `?int`/`?float`/`?bool`/`?Enum` parameter, or returning one from a function returning such a type, crashing the program
- Fixed compiler crash on a bare `@` (`let x = @`, `@.foo`): `@` must be followed by `(...)`, otherwise it's a syntax error
- Fixed compiler crash on unions with more than 256 variants
- Rewrote the language reference for v0.11: enums, unions, nullables, operator precedence, errors and known limitations
- Added `bench.py`: a Dusk-only benchmark runner with JSON output and `--compare <git-ref>` to measure a change against another version
- Regenerated the benchmark report for v0.11
- Internal: `@(...)` is parsed as its own `anonymous_init` expression, not as a call
- Internal: renamed booleans to read as yes/no questions (`nullable` type flag → `is_nullable`, `coerce` → `canCoerce`, `typeNeedsNullBox` → `needsNullBox`, `findIsFieldMut` → `isFieldMut`, `alwaysReturns` → `areAllPathsReturning`, plus a few locals)
- Internal: split sema's `visitWhenStmt` into smaller steps: resolving the subject, checking cases, checking exhaustiveness, storing the subject and lowering each case
- Internal: removed the unreachable `LARGE_UNION_LOAD`/`LARGE_UNION_STORE` opcodes
- Internal: `NULL_BOX_STORE` in the VM handles the null case first with an early `continue`

## v0.10 - Frieren

- Bytecode compiler generating a custom instruction set
- Custom VM with bytecode interpreter, replacing the QuickJS JavaScript runtime
- Garbage collector for heap-allocated values (strings, arrays, structs)
- Keyword-delimited blocks: `if/then/end` and `for/do/end` replace Python-style indentation
- New `elif` keyword as a shorthand for `else if` chains
- Struct and function bodies now closed with `end` keyword
- Added `--version` CLI flag
- Removed `concat`, `stringify`, and `floor` built-in functions
- Added benchmark suite (fib, primes, quicksort, mandelbrot, particles)

## v0.9 - Shinobu

- Improved error messages with precise location highlighting (`^~~~` markers and line:col info)
- Added `assert` built-in function for runtime assertions
- Static method dispatch for struct methods
- Upgraded to latest Zig version (0.16)
- Internal: TypeTable as centralized type registry
- Internal: IR Value refactored with `type_id` tracking
- Internal: Symbol Table refactored with scoped parent-chain lookups
- Expanded test suite with 43 negative test cases

## v0.8 - Seras Victoria

- Separated `int` (64-bit signed integer) and `float` (64-bit floating point) primitive types, replacing the unified `number` type.
- Enabled operations between `float` and `int` with implicit promotion of integers to floats.
- Added support for the Pipe operator (`|>`) for left-to-right function call chaining.
- Implemented Trunc division operator (`//`).

## v0.7 - Mizuhara

- Nullable types (`?Type`) and optional chaining (`?.`)
- Nullable capture (`if value: captured` and `if value: mut captured`)
- Function parameter default values and inference
- Struct field default values
- Nested Struct definition (`field: struct ...`)

## v0.6 - Takina

- Support for multiline function definitions
- Anonymous structs (`@(...)` syntax)
- Static struct fields (using `let`) and static method calls

## v0.5 - Chisato

- Structs (definition, initialization, methods)
- Named parameters in function calls
- Added `stringify` built-in function
- Hoisting for functions and structs
- Support for multiple line statements (structs and function calls)

## v0.4 - Himeno

- Loop controll flow (break, continue)
- Support for the operators: += -+
- Infinite loops now requires condition "true"

## v0.3 - Maki

- let type inference
- Support for else if
- Inline function returns
- Function parameter mut modifier

## v0.2 - Rukia

- Single Line Comments
- Full support for expressions and operators
- Improve error messages
- Arrays
- Organize better internals functions
- quickjs as embbed runtime
- Remove the need of the keyword fn for function declaration

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
