# Changelogs

## v0.14 - Simon (in development)

Started v0.14 development: updated the CLI version to `0.14.0-devel`.

### Features

- Generic functions can be declared with type slots: `func first[T](xs: []T) -> T`. The body is checked where it's written, with `T` standing for any type: arithmetic, `==`/`!=` and assigning other types to `T` are errors, a `T` param can't be `mut` or have a default value, and `T` can't be used as a value. Using a generic function as a value is a `GenericNeedsCall` error
- Calling a generic function fills its slots from the arguments (`first([1, 2])` makes `first[int]`) and compiles one copy per filled type. Generic functions can call each other, recurse, and be called before they're declared; inferred error sets (`-> !int`) are per copy. Each copy is checked with its real types, and an error there names the copy: `in first[[]int] (created at line 7): ...`. New errors: `CantInferSlot` (nothing to work out a slot from, like `first([])`), `ConflictingSlotTypes` (`both(1, 2.5)`: no conversion while filling slots), `InvalidSlotType` (a `void` argument), and `GenericCopyTooDeep` (more than 64 nested copies, like a function calling itself with `[x]`)
- Slots the arguments don't fill come from the type the call's result is expected to have: `let xs: []int = empty()`, a return value, another function's parameter, a struct field, an assignment, or an `if`/`when` branch. Arguments still win: `let s: string = first([1])` makes `first[int]` and then reports the mismatch
- Generic structs and unions: `struct Stack[T]`, `struct Pair[A, B]`, `union Result[T, E]`, written as `Stack[int]` in types. Slots are filled from the field or variant values (`Stack(items = [1, 2])`) or from the expected type (`let s: Stack[string] = @(items = [])`). Methods can use `T` and `@`, recursive types like `Node[T]` with `next: ?Node[T]` work, generic functions can take and return them (`func top[T](s: Stack[T]) -> T`), and static fields are shared by every copy (`Stack.MAX`). Each filled type is its own struct or union, checked like the generic functions' copies. New `WrongSlotCount` error for `Stack[int, int]`, a bare `Stack` in a type, or type arguments on a type that isn't generic
- `append`, `copy` and `len` are generic prelude functions (`extern func append[T](mut array: []T, value: T) -> void`, `extern func copy[T](value: T) -> T`, `extern func len[T](array: []T) -> int`), so `append` checks its item: `append(ints, 2.5)` and `append(names, 1)` are errors, while `append(prices, 1)` still stores `1.0`. The prelude can declare `extern func` with slots; every copy uses the same native
- Type lists constrain a slot to primitives: `func sum[T: int | float](xs: []T) -> T`, and `trait Number = int | float` names a list for reuse (`[T: Number]`, `trait Scalar = Number | string | bool`). Members are `int`, `float`, `byte`, `string` and `bool`. In the body, an operator on `T` is checked once per member with the usual rules: it's allowed when every member supports it, and its type is the one result they all share (`x / 2` is a `float`, `a < b` a `bool`), else `T` when each member gives itself back (`a + b` on `int | float`). A value converts to or from `T` when it converts for every member, so `let total: T = 0` works and `let x: T = 2.5` doesn't. `==`/`!=` on `T` need a list without `string`, since on a string they compare pointers. `x == null` on a `?T` is allowed with or without a list. New errors: `TypeDoesntFitSlot` (`'string' doesn't fit T: Number`, at the argument or type annotation), `SlotResultDiffers` (`a + b` on `byte | float` gives `int` for one and `float` for the other), `TypeListNotPrimitive`, `TraitRefersToItself` and `TraitUsedAsType` (`let x: Number = 1`). Arithmetic or unary operators on an unconstrained `T` now report `SlotOperatorNotAllowed`, which says the slot needs a type list
- Traits: `trait Shape ... end` lists method signatures without bodies (`func area(self: @) -> float`), and `[T: Shape]` lets generic code call them (`x.area()`). Traits combine with `+`, both when declaring (`trait Item: Shape + Named ... end`, or `trait Item: Shape + Named end` with no methods of its own) and in constraints (`[T: Shape + Named]`). A struct, enum or union fits a trait when it has every method with the same param types and return type (`@` is the type itself; param names don't matter). A plain `self` method can stand in for a `mut self` one, not the other way around. A `mut self` trait method needs a mutable receiver, and `mut x: T` is allowed when `T` has a trait constraint. Each use is checked where the type fills the slot, in calls, struct fields and annotations: `'Plain' doesn't fit T: Shape: it has no method 'area'`, or `its 'area' is (self: Wrong) -> int, the trait wants (self: Wrong) -> float`. New errors: `DoesntFitTrait`, `SlotHasNoMethod`, `ClashingTraitMethods` (two traits giving one method different signatures), `TypeListWithTrait` (`Number + Shape`: primitives have no methods), `NotATrait`, `TraitMethodNeedsSelf` (trait methods take `self` first) and `TraitMethodNeedsErrorSet` (a failing trait method names its error set, `ParseError!int`)
- `pop(mut list: []T) -> ArrayError!T` removes and returns the last item, failing with `ArrayError.Empty` on an empty array. New builtin error set `ArrayError` (`Empty`, `OutOfRange`); user error ids now start at 8. `pop` is written in Dusk in the prelude, which can now hold functions with bodies; prelude names starting with `__` are internal, and using one is an `InternalBuiltin` error
- `insert(mut list, index, item) -> ArrayError!void`, `remove(mut list, index) -> ArrayError!T` and `clear(mut list)`. `insert` and `remove` fail with `ArrayError.OutOfRange` for an index outside the array (`insert` also accepts `len(list)`)
- `reverse(mut list)`, `slice(list, start, stop) -> ArrayError![]T` (items are copied, so the slice is independent), `index_of(list, item) -> ?int` and `contains(list, item) -> bool`. `index_of` and `contains` take arrays of `int`, `float`, `byte`, `bool` or `string`, and compare strings by content
- When several arguments mention the same slot, the first one fills it and the others must convert to it with the usual rules: `both(2.5, 1)` makes `T` a `float`, `both(1, 2.5)` is an error at `2.5`. `ConflictingSlotTypes` is now only for one argument giving a slot two types (a `Pair[T, T]` param receiving a `Pair[int, string]`)
- Breaking: removed the v0.13 sharing rule. A `let` and a `let mut` variable can share a value again, so `let` only stops changes made through that name. Moves, the `NeedsCopy` and `ChangedWhileRead` errors and the rule against returning a parameter are gone, so functions can return their parameters (`return self`). One local check stays: a value read through a non-`mut` name (a `let`, a param or capture without `mut`) can't go into a mutable place (`let mut`, assignment, `append`/`insert`, a `mut` argument), including inside a constructor or array literal: `let mut x = Obj(items = frozen)` is a new `ReadOnlyIntoMutable` error. Use `copy(x)` or a `mut` name
- A pipe's right side can be a `try` call: `try parse(text) |> try half() |> add(1)` calls `half(...)` and passes its error up. The pipe syntax error now names `|>` instead of `->`
- Temporary values can be changed: function results and new values can call `mut self` methods and have their fields and items assigned, so a `mut self` method that returns `self` chains (`b.add(1).add(2)`). The `TemporaryNotMutable` error is gone
- Removed the `+=` and `-=` operators. Write `x = x + 1` and `x = x - 1`; `x += 1` is now a syntax error

### Fixes

- Integer overflow (`9223372036854775807 + 1`, and `-`, `*` or negating the smallest `int`) crashed the VM and was undefined behavior in release builds; it's now an `IntegerOverflow` runtime error. A `float` side of `//` that's `nan`, infinite or outside the `int` range crashed too; it's now a `FloatOutOfIntRange` runtime error
- An integer `//` or `%` by `0` crashed the VM (and was undefined behavior in release builds), and so did `7.0 // 0.0`; it's now a `DivisionByZero` runtime error. The smallest `int` `// -1` is a `DivisionOverflow` runtime error, and its `% -1` gives `0` instead of crashing
- `len([])` crashed the compiler; it's now a `CantInferSlot` error, since an empty literal gives no item type. Passing a non-array where a generic function takes `[]T` (`len(5)`, `first(5)`) reports `expected 'an array' found 'int'` instead of `CantInferSlot`
- A function whose return type names an unknown type (`func f() -> Missing`) crashed the compiler; it's now an `Invalid type 'Missing'` error
- A struct field with neither a type nor a default (`x` alone, or `static x = 1`, which reads as a field named `static`) is now a parser error, `expected ':' or '=' after field 'x'`, instead of crashing the compiler. Fields of inline nested structs are checked too

## v0.13 - Garou

Started v0.13 development: updated the CLI version to `0.13.0-devel` and recorded the implementation phases for comments and string features.

### Features

- Added nested multiline comments with `#= ... =#` and a lexer diagnostic for unclosed comments
- Strings now require valid UTF-8 and support `\"`, `\\`, `\n`, `\r`, `\t`, `\0` and `\u{...}` escapes, decoded at compile time. Invalid escapes and Unicode scalar values are compile errors; unterminated strings report an error instead of hanging
- Strings accept the `\'` escape
- Multiline strings with `"""`: the text starts on the line after the opening quotes, and the closing `"""` indentation is removed from every line. Under-indented lines, text on the opening or closing line and unclosed strings are compile errors. Escapes and interpolation work as in double-quoted strings
- Breaking: a raw line break inside a double-quoted string is now a compile error (use `\n` or `"""`). Line breaks inside `${ ... }` are still allowed
- `byte` type: one value from `0` to `255`, written as `'a'`, `'\n'` or `'\u{41}'` (one ASCII character or escape). Int constants that fit convert to `byte`, `byte(x)` converts an int and is checked at run time (out of range stops the program), and a byte converts to `int` automatically. Arithmetic on bytes gives an `int`. Bytes work in `when` (literal and range cases), as array indexes, as range-loop bounds and as `?byte`, and print as numbers. At run time a byte is stored like an `int`
- `buffer` type: a fixed number of mutable bytes stored packed. `buffer(n)` creates `n` zero bytes, `b[i]` reads a `byte`, `b[i] = x` writes one (bounds-checked, needs `let mut`), `len(b)` gives the size and `for b: byte, index do` loops over it. `buffer_from(s)` copies a string into a buffer, and `string_from(b) -> StringError!string` copies a buffer into a string (failing with `InvalidUtf8` if the bytes aren't valid UTF-8). `echo` prints a buffer like an array of numbers. The type annotation is `buffer`, and functions can take `mut b: buffer` parameters (the caller must pass a mutable variable), like arrays
- String literals are built once at compile time into one program-wide pool and shared: loading a literal no longer allocates or can trigger garbage collection, and the GC skips pool strings. Literals with the same text are now the same string, so `"a" == "a"` is `true` (`==` still compares identity). More than 65,536 different literals is a compile error.
- `str_equal(a, b)` compares two strings by content (`a |> str_equal(b)` works too). `==` on strings stays a reference comparison by design. String operations use `str_`-prefixed builtin functions
- `len(s)` returns a string's length in bytes (UTF-8), and `str_char_count(s)` returns its number of characters (Unicode code points): `len("olá")` is 4, `str_char_count("olá")` is 3
- `str_concat(a, b)` returns a new string with `b` after `a`, and `str_join(parts, sep = "")` joins a `[]string` with an optional separator between items (allocating the result once). Both work with pipes: `"a" |> str_concat("b")`
- `str_to_int(s)` and `str_to_float(s)` read numbers written like Dusk literals (an optional `-`, digits, and for floats an optional `.digits`; `"3"` gives `3.0`). They return `ConversionError!int` / `ConversionError!float`, a new builtin error set with `Invalid` and `Overflow`. Inline builtins can now fail: the opcode skips the following `JUMP` to run the `try`/`catch` handler, like `RETURN_ERR`
- Strings can't be indexed with `s[i]`: `str_byte_at(s, i) -> StringError!byte` reads the byte at a byte offset, and `str_char_at(s, i) -> StringError!string` returns the `i`-th character as a string; both fail with `OutOfRange`. `for s: b, i do` loops over a string's bytes, like buffers
- String search: `str_find(s, x) -> ?int` (byte offset of the first match, `null` if none), `str_contains`, `str_starts_with`, `str_ends_with`, and `str_count(s, x)` (non-overlapping matches, `0` for an empty pattern). New builtin error set `StringError` with `OutOfRange`, `NegativeCount` and `InvalidUtf8`. User error ids (printed raw) shift by three more
- String transforms: `str_trim` (ASCII whitespace), `str_upper` / `str_lower` (ASCII and Latin-1 letters: `"ação"` ↔ `"AÇÃO"`), `str_split(s, sep) -> []string` (keeps empty pieces; an empty `sep` splits into characters), `str_replace(s, old, new)` (an empty `old` returns `s` unchanged) and `str_repeat(s, n) -> StringError!string` (negative `n` fails with `NegativeCount`). Allocating string functions size their result first, collect at most once, then reread their arguments
- `str_slice(s, start, stop) -> StringError!string` copies the bytes from `start` up to (not including) `stop`. Negative or reversed bounds, `stop > len(s)` and a bound inside a multi-byte character fail with `StringError.OutOfRange`
- `str_equal(a, b, ignore_case = false)`: the new optional parameter compares ASCII and Latin-1 letters without case (`str_equal("José", "JOSÉ", true)`). `str_equal` now runs through `STR_OP`, and the unused `BYTES_EQ` opcode is gone
- An empty pattern never matches in string search: `str_find(s, "")` returns `null`, and `str_contains`, `str_starts_with` and `str_ends_with` return `false` (like `str_count` giving `0` and `str_replace` leaving the text unchanged). `str_split(s, "")` still splits into characters
- Error values printed with `echo` or interpolation (still their global id) shift by two, because `ConversionError` takes the first two ids
- String interpolation: `$name` and `${expression}` insert values with the current `echo` formatting, evaluating and formatting each expression once from left to right. Nested strings, conditionals, `try`/`catch` and comments are supported; `\$` escapes a literal dollar sign
- Interpolated strings participate in GC heap maps and preserve existing variables during self-assignment. Empty expressions, unknown names, void results and unhandled failing calls are compile errors
- Faster string interpolation: each interpolated string is built by one `STR_BUILD` instruction (parts in consecutive registers, a per-chunk recipe of formatting kinds, built in batches of up to 32 parts), formatted into a reused VM scratch buffer and allocated once. Before, every part copied the whole string built so far, allocated a new heap string and a temporary page-allocator buffer (O(n²)). Arrays, structs, unions, buffers and nullable values are formatted right after they're evaluated (`TO_STRING`), so later `${}` side effects can't change them, as before
- Benchmarks: new string benchmarks in every language: `strings` (1,000,000 interpolations, splits and joins), `concat` (200,000 plain concatenations) and `parse` (build and parse 500,000 CSV rows)
- Breaking: a `let` variable's struct fields can't be changed anymore. Mutability now covers the whole path from the variable: `p.x = 1`, `bx.p.x = 1` and `ps[0].x = 1` need a `let mut` root, struct fields can't be assigned through a `self: @` or other non-`mut` parameter, or through an `if` capture without `mut`, and `mut` arguments are checked through fields and items (`grow(bx.items)` and `append(bx.items, 1)` need a mutable `bx`). Assigning into a `let` static field (`S.items[0] = 1`, `append(E.list, 1)`) is rejected too. Changing a temporary (`make().x = 1`, `make().set(1)`) is a new `TemporaryNotMutable` error instead of a compiler crash. Errors on `mut self` calls name the variable instead of the method. Struct construction no longer requires a mutable variable for array or struct arguments (`Box(items = numbers)` with `let numbers`)
- Breaking: functions, methods and default values can't see top-level variables anymore, only top-level constants. A constant is a top-level `let` whose value is known at compile time: an `int`, `float`, `bool`, `byte`, string or enum value built from literals, other constants, enum variants, static fields and operators (`let WIDTH = 80`, `let HEIGHT = WIDTH - 20`). Constants are inlined at each use and can be used anywhere, even above their declaration. State a function reads or changes is passed as a parameter (`mut` to change it). New errors: `TopLevelVariableNotVisible`, `InvalidConstantType`
- Breaking: static fields (`let` inside a struct, enum or union) are constants with the same rules, and can't be `mut` (`StaticFieldCantBeMutable`) or hold other values (`StaticFieldNotConstant`). The `UsedBeforeDeclared` checks are gone, since functions no longer depend on top-level variables. The `GLOBAL_LOAD`, `GLOBAL_STORE`, `STATIC_LOAD` and `STATIC_STORE` opcodes and the VM's static storage are removed
- Breaking: `mut` params and `mut` captures follow one rule: they change values through them and can never be reassigned. Reassigning a `mut` param or capture (`list = []`, `if maybe: mut p then p = other`) is a new `MutBorrowCantBeReassigned` error (it used to change only the local name, silently). `mut` captures only work for arrays, buffers, structs and unions (`PrimitiveCaptureCantBeMutable`): `if count: mut k` on a `?int` used to change only a local copy. Union params can now be `mut` (including `mut self`), changing payloads in place. New `mut` captures in `when` union cases (`poly: mut points then`) and in `for` loops (`for items: mut item do`), which need a mutable subject or source
- Breaking: mutable and immutable values can't share data anymore, so a `let` value is never changed by anything. Storing a mutable value into a `let` (or the other way around) needs `copy(x)`, unless the source is a local that doesn't share its value and isn't used again, in which case it moves for free (also for fields and items of such a local). Functions can only return values that don't share data with their parameters, and can only store into a `mut` parameter values that don't come from their other parameters, so methods that keep an argument copy it (`append(self.items, copy(item))`). Mutable values can still share with each other. New `NeedsCopy` error, whose message names the reason and the fix
- `copy(x)` returns a deep copy of an array, buffer, struct or union (and their nullables): nested arrays, struct fields, union payloads and buffers are copied, shared references and cycles inside the value stay shared within the copy, and strings and nullable primitive boxes are shared since they never change. For other types it returns the value itself. New `DEEP_COPY` opcode
- Captures without `mut` read their value: while one is in use, the variable it came from can't change (assignment, `append`, `mut` arguments, `mut self` calls, `mut` captures). New `ChangedWhileRead` error
- `when` on strings (and `?string`): cases are string literals compared by content (like `str_equal`), an `else` is required, a repeated literal is a duplicate-case error (also when escapes spell the same text), and interpolated, non-literal and range cases are rejected
- Breaking: programs start at a required `main` function, `func main() -> void` or `func main() -> !void` (also `E!void`), with no parameters. The top level only holds functions, types, error sets and constants; other statements, `let mut` and non-constant `let`s there are compile errors, and an error returned from `main` stops the program like a top-level `try` did. Declaring a function or type inside a function is now a clear error instead of a crash. New errors: `MissingMain`, `InvalidMainSignature`, `StatementOutsideMain`, `TopLevelLetNotConstant`, `DeclarationInsideFunction`; `TopLevelVariableNotVisible` is gone. Codegen no longer special-cases top-level code (`is_generating_main`)
- An `int` converts to `float` wherever a `float` is expected: typed `let`s, assignments, arguments, return values, struct fields, union payloads, array items, default values, constants and `catch` fallbacks (before, only arithmetic, `if`/`when` results and `orelse` did). `int` and `?int` also convert to `?float`. `[]int` still doesn't convert to `[]float`, and `float` never converts to `int`. Int literals are converted at compile time
- `float(x)` converts an `int` or `byte` to `float`
- Tail calls: `return f(x)` reuses the current call frame (new `TAIL_CALL` opcode), so tail-recursive and mutually recursive functions run in constant stack space (also `return f(x)` in a `void` function). Calls whose result is an error union aren't tail calls yet
- Breaking: `len` only takes arrays. Use `str_len(s)` for the byte count of a string and `buffer_len(b)` for a buffer.
- `extern` is a reserved keyword. It declares builtins in the prelude, and using it in a program is an `ExternOutsidePrelude` error
- Strings and buffers share one runtime byte object (`String` became `Bytes` in `value.zig`), with new `BYTES_NEW`, `BYTES_LEN`, `BYTES_GET`, `BYTES_SET` and `BYTES_COPY` opcodes. `len` is now type-checked by sema and accepts arrays and buffers
- String functions that take string arguments share one `STR_OP` opcode, generated from a spec table in `built-in.zig`
- Registered ten test files that were never run (`min`, `gc-stress-struct-field-mutation`, `deep-recursion`, `elif` and others); two of them were fixed for the new mutability rules
- Planned the rest of v0.13 strings: `byte`, then a fixed-size `buffer` sharing one VM byte object with `string`, then a compile-time string literal pool, then string operations
- Backlog: recorded readable enum, error and union formatting for both `echo` and string interpolation

### Fixes

- Fixed a negative array index (`a[-1]`, `a[i] = x` with `i < 0`) crashing the VM. It is now an `ArrayOutOfBounds` runtime error, and array index errors print a message (`index 5 is out of bounds for array of length 2`) like buffer index errors
- An untyped array literal that starts with ints and has a float item is a `[]float` (`[1, 2.5]`), instead of an error that depended on item order
- Compile errors on a last line without a trailing newline print the marker on its own line, and an empty file reports `MissingMain` instead of crashing the error printer
- Language reference review: fixed two loop examples that called `echo` with two arguments and the sharing example (a `mut` value that isn't used again moves instead of failing); constants are hoisted; only nullable primitives and enums compare by value; `==` on arrays, structs, unions and buffers compares identity; member order isn't checked yet; `mut` arguments can be fields or new values; `when` captures follow the capture rules; the call depth runtime error; new known limitations for sharing values
- Fixed recursion deeper than 256 calls crashing the VM (out-of-bounds frame write). The limit is now 100,000 nested calls (frames and registers are allocated when the program starts, and only touched as they're used), and going over stops with a `CallDepthExceeded` runtime error
- Fixed assigning a struct, array, union or `if`/`when` value that reads the variable being assigned: `head = Node(value = i, next = head)` built the new value in `head`'s register first, so `next` pointed to the new node itself (a cycle)
- Fixed nullable primitive variables (`?int`, `?float`, `?bool`, `?byte`, `?Enum`) sharing their value: assigning to one changed its heap box in place, so `let a: ?int = 5; let mut b = a; b = 6` also changed `a`, and a function could change its caller's variable the same way. Assignment now builds a new box, like `let` and field or item assignment already did. The `NULL_BOX_STORE` opcode is gone
- Bytecode owns its string constant bytes, preserving decoded literals after the compiler's temporary arena is freed
- Fixed single-line comments without a final newline hanging the lexer and trailing spaces producing an unexpected token

## v0.12 - Kanade

- `dusk --version` reports `0.12.0` (it still said `0.11.0-devel`); the language reference now points string features to v0.13
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
- Functions can read and assign top-level variables declared above them. A call that runs before a variable the function uses is declared (directly or through other functions) is a compile error, and so is a default value that calls such a function
- Too many variables or temporaries alive at once in one function is now a compile error naming the function, instead of a `Register limit exceeded` panic
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
