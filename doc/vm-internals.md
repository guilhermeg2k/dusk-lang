# VM internals

How Dusk programs run: the compiled program format, values and heap objects, the register machine, calls and errors, and the garbage collector. It describes the code as of v0.15 (closures and tasks). The language itself is described in [language-ref.md](language-ref.md).

The source files involved:

| File | What it holds |
|---|---|
| `src/bytecode.zig` | Code generation from IR (`BytecodeGen`), the compiled program format (`Program`, `Function`, `Chunk`), the instruction format and opcodes |
| `src/vm.zig` | The interpreter (`VM.execute`), call frames, allocation helpers, the garbage collector, `copy` |
| `src/value.zig` | `Value`, `ValueType` and every heap object layout |
| `src/natives.zig` | Builtins written in Zig, and the intrinsics table |
| `src/format.zig` | Printing values (`echo`, interpolation, `TO_STRING`) |
| `src/error.zig` | `RuntimeErrorReporter`, the runtime error messages |
| `src/scheduler.zig` | `Scheduler` (every task, the ready queue) and `TaskState` (a task's stack, frames and result) |
| `src/event_loop.zig` | `EventLoop`, the only code that drives libuv (timers for `sleep`) |
| `src/uv.zig` | The few libuv functions Dusk calls, declared by hand |

---

## 1. From source to a running program

`Dusk.compile` (`src/dusk.zig`) parses the prelude and the user's file, runs sema to get IR, and passes it to `BytecodeGen.generate`, which returns a `Program`. Everything before codegen uses an arena that is freed once codegen finishes. The `Program` outlives it.

`Dusk.compileAndRun` then creates a `VM` and calls `VM.run`, which:

1. creates the first task, for `$main`, the function codegen builds from the top-level code (section 11),
2. runs the scheduler loop: it runs ready tasks with `execute` until every task is done,
3. reports failed tasks nobody awaited,
4. on a runtime error, prints where it happened (`reportWhere`) and returns the error.

## 2. The compiled program

```
Program
├── functions: []Function          one per extern, method, function and lambda, then $main last
├── static_closures: []Closure     one per function, same index (see 3.2)
├── string_pool: StringPool        every string literal of the program
├── struct_descriptors: []StructDescriptor
├── union_descriptors: []UnionDescriptor
├── error_names: [][]const u8      error id -> name, for unhandled-error messages
└── main_func_index
```

### Functions

A `Function` has a `uid`, a `name` and a `kind`:

- `.dusk`: a `Chunk`, meaning compiled bytecode.
- `.native`: a Zig function pointer plus its argument count (`NativeFn`).
- `.intrinsic`: a builtin that codegen expands inline (`len`, `append`, `copy`, ...). It is never called at runtime (section 7.3).

Function indexes are fixed before any body is generated. Externs come first, in prelude order, then methods of user types, then free functions and lambdas. `$main` is always last. So `CALL FN[i]` and `LOAD_FUNC FN[i]` can refer to a function whose body hasn't been generated yet.

### Chunks

```zig
Chunk {
    num_registers: u8,                  // registers this function needs (peak use)
    num_params: u8,
    instructions: []Instruction,
    constants: []Value,                 // ints, floats, bools, null; strings use the pool
    shadow_types: []ValueType,          // type of each constant, for the disassembler
    heap_maps: []HeapMap,               // GC roots per instruction (section 9)
    build_recipes: [][]ValueType,       // part kinds for each STR_BUILD
    jump_tables: []JumpTable,           // for JUMP_TABLE
    capture_kinds: []ValueType,         // one per capture, for closures (section 8)
}
```

Constants are deduplicated per chunk (`const_id_by_value`). Error ids, enum values and bytes are plain `int64` constants.

### Descriptors

- **`StructDescriptor`**: `field_count`, plus `heap_bitmap`, where bit *i* is set when field *i* holds a pointer. There is one per struct type, shared by all instances (`Struct.descriptor_id`).
- **`UnionDescriptor`**: `heap_bitmap` (variants whose payload is a pointer), `box_bitmap` (primitive variants, which need a null box when read as `?T`), and `variant_kinds`.

Both limit a type to 256 fields or variants, the size of a `u256` bitmap.

### The string pool

Every string literal in the program goes into one `StringPool`. That is a single allocation where each string is laid out as a real `Bytes` heap object. `LOAD_STRING R[a] STR[i]` loads `pool.values[i]`, a pointer into that block, so loading a literal never allocates. The GC recognizes pool pointers with an address range check (`StringPool.contains`) and never moves them.

## 3. Values and heap objects

### 3.1 `Value`

```zig
Value = extern union { int64: i64, float64: f64, bool: bool, null: void, heap_obj: *HeapValue }
```

A value is 8 bytes with **no runtime tag**. Sema knows every type, so each instruction knows how to read its operands: `I_ADD` reads `.int64`, `F_ADD` reads `.float64`, and `STRUCT_LOAD` reads `.heap_obj`. `null` is the all-zero bit pattern, so a null pointer and the `null` value are the same thing.

How Dusk types map to values (`ValueType.fromTypeId`):

| Dusk type | Runtime value |
|---|---|
| `int`, `byte`, enums, error ids | `int64` |
| `float` | `float64` |
| `bool` | `bool` |
| `void`, `null` | `0` |
| `string`, `buffer` | pointer to `Bytes` |
| `[]T` | pointer to `Array` |
| struct | pointer to `Struct` |
| union | pointer to `Union` |
| `?int`, `?float`, `?bool`, `?byte`, `?Enum` | pointer to `NullBox`, or `0` |
| `?string`, `?[]T`, `?Struct`, ... | the pointer itself, or `0` |
| `func(...) -> T` | pointer to `Closure` |
| `Task[T, E]` | pointer to `Task` |
| captured `let mut` (internal) | pointer to `Cell` |

The few places that need a type at runtime carry it explicitly:
- `Array.kind`: the element kind, used for printing and GC tracing.
- `NullBox.kind` and `Cell.kind`: the kind of the boxed value.
- Struct and union descriptors.
- `capture_kinds` for closures.
- `build_recipes` for interpolation.
- The hidden kind argument given to natives with `dynamic` params (`echo`).

### 3.2 Heap objects

Every heap object starts with the same header:

```zig
HeapValue = extern struct {
    kind: HeapValueType,         // array, struct, string, buffer, union, null_box, closure, cell, task
    gc_forward: ?*HeapValue,     // set during a GC: where this object was copied
    relocated: ?*HeapValue,      // set when an array grows: where it moved
}
```

The header is 24 bytes. Each object type embeds it as its `obj` field, and code goes from a header to the full object with `HeapValue.getParentPtr(T, ptr)`. Variable-size objects store their data right after the struct, in the same allocation (`getDataPtr()`).

| Object | Fields after the header | Data after the struct | Size |
|---|---|---|---|
| `Bytes` (string, buffer) | `len` | `len` bytes | 32 + len |
| `Array` | `kind`, `len`, `capacity` | `capacity` values | 48 + 8 × capacity |
| `Struct` | `descriptor_id`, `field_count` | `field_count` values | 32 + 8 × fields |
| `Union` | `descriptor_id`, `active_tag` | one value (the payload) | 40 |
| `NullBox` | `kind`, `value` | none | 40 |
| `Closure` | `function`, `capture_count` | `capture_count` values | 40 + 8 × captures |
| `Cell` | `kind`, `value` | none | 40 |
| `Task` | `state` (a `*TaskState`, section 11) | none | 32 |

Notes:

- **Strings and buffers** share `Bytes` and differ only in `kind`. Strings are immutable. Buffers are changed in place with `BYTES_SET`.
- **Arrays** start with a capacity of at least 8 (`ARRAY_INIT` takes `max(len, 8)`, capped at 255 as an operand), and grow by 1.5× (`calcNewCapacity`). Growing allocates a new array and leaves `relocated` pointing to it in the old one (section 6.2).
- **Unions** always hold one payload slot. `active_tag == NO_TAG` (`0xFFFF`) means no variant has been set.
- **Static closures**: every function has a `Closure` with `capture_count == 0` in `Program.static_closures`, built when codegen finishes. They live outside the GC heap. `Closure.isStatic` identifies them, and the GC skips them.

## 4. Registers, the stack and frames

### 4.1 Memory

Each task (section 11), `$main` included, has its own value stack and frame array, allocated outside the GC heap with the scheduler's allocator (the C allocator):

- **The value stack** starts at `INITIAL_STACK_SLOTS = 1024` values (8 KB) and grows up to `STACK_SLOTS = 4_000_000`.
- **The frames** start at `INITIAL_FRAMES = 32` `CallFrame`s and grow up to `MAX_CALL_DEPTH = 100_000`.

`VM.stack`, `VM.frames` and `VM.frame_count` are the running task's, swapped in and out by the scheduler, so the interpreter reads them without going through the task.

```zig
CallFrame { function: *const Function, cur_inst: usize, stack_offset: usize }
```

- `stack_offset` is where the frame's register 0 lives on the stack.
- `cur_inst` is the saved instruction pointer. For a caller frame it points just after the call. For the running frame it is updated only before an instruction that can allocate or call (sections 7 and 9).

### 4.2 Register windows

Each function sees its registers as `R[0] .. R[num_registers - 1]`, which are `stack[stack_offset + i]`. A function can use at most 255 registers (`TooManyRegisters` at compile time). Parameters are `R[0] .. R[num_params - 1]`.

Calls overlap windows instead of copying arguments. A call puts the callee, or the result slot, in `R[a]` and the arguments in `R[a+1] ...` of the caller. The callee's frame then starts at `R[a+1]`:

```
caller:  R[0] ... R[a-1] | R[a]        | R[a+1] R[a+2] ...
                         | result slot | arg0   arg1
callee:                  | (base - 1)  | R[0]   R[1]   R[2] ...
```

This gives three properties:
- Passing arguments costs nothing: they are already in the callee's parameter registers.
- `RETURN` writes the result to `stack[stack_base - 1]`, which is the caller's `R[a]`, exactly where the caller expects it.
- For `CALL_VALUE`, `stack[stack_base - 1]` holds the closure being called for the whole call. That's how `CAPTURE_GET` finds its captures (section 8).

Before pushing a frame, a call checks that a frame is free and that `new_offset + 256` slots still fit on the stack. Otherwise `growForCall` (a cold path) doubles the frames or the stack with `realloc`, zero-fills the new slots, and the handler reloads its `stack` local. Growing is safe because frames hold offsets, captured variables live in heap `Cell`s and natives never call back into Dusk: nothing keeps a pointer into a stack across a call. Past the limits it fails with "call depth exceeded".

### 4.3 Register allocation in codegen

Registers are allocated like a stack: `consumeRegister` takes `next_free_register`, and `freeRegisterN` gives registers back. Each block, statement and expression saves `next_free_register` and restores it when done, so temporaries are reused right away. `peak_register` becomes `num_registers`.

- A variable gets a register when it is declared (`bindVariable`) and keeps it until its block ends.
- `register_types[i]` tracks the Dusk type in each register while generating code. This is what heap maps are computed from (section 9), so it must always say whether a register holds a pointer. Freed registers go back to `void`.
- Calls and closures need their callee or result register to be the last register in use, so the arguments can follow it. When it isn't, `genFnCall`, `genCallValue` and `genMakeClosure` generate into a fresh register and `LOAD` the result into the target.

## 5. The interpreter loop

`VM.execute` is one `while (true)` loop with a `switch` on the opcode. Its hot state lives in locals:

```zig
current_frame      // *CallFrame
cur_instruction    // index of the next instruction in `instructions`
instructions       // current function's code
stack_base         // current_frame.stack_offset
```

`execute` runs one task. It starts from the top frame's saved `cur_inst` and `stack_offset`, so a task that parked continues where it stopped. It returns when the task's last frame returns or the task parks (section 11.3).

Each iteration reads `instructions[cur_instruction]` and increments it before running the handler. So inside a handler `cur_instruction` already points at the next instruction, and that is the value calls save as the return point.

Slow paths are `noinline` helpers: `makeClosure`, `newCell`, `newBuffer`, `deepCopy`, `toString`, `buildString`, `loadUnionVariant`, `collectGarbage`. Error reporters are `noinline` with `@branchHint(.cold)`. That keeps `execute` small. Performance is very sensitive to the machine code layout of this loop, so even unrelated edits to it can move benchmarks noticeably.

### 5.1 Instruction format

```zig
Instruction = packed struct { op: OpCode, a: u8, b: u8, c: u8 }    // 32 bits
```

Most instructions use three 8-bit operands, usually register indexes. Two wide forms join operands into 16 bits:

- `bEx()` = `b:c`, used for constant, string, function, descriptor, jump table and jump target indexes.
- `aEx()` = `a:b`, used for `JUMP` targets.

Jump targets are therefore 16 bits, which limits a function to 65,536 instructions (`FunctionTooLong` at compile time).

### 5.2 Instruction set

`R[x]` is a register, `C[x]` a constant, `FN[x]` a function index and `I[x]` an instruction index.

**Loads and moves**

| Instruction | Effect |
|---|---|
| `LOAD_CONST R[a] C[bEx]` | `R[a] = constants[bEx]` |
| `LOAD_STRING R[a] STR[bEx]` | `R[a] = string_pool[bEx]`, no allocation |
| `LOAD R[a] R[b]` | `R[a] = R[b]` |
| `LOAD_FUNC R[a] FN[bEx]` | `R[a] = &static_closures[bEx]` |

**Integers.** These check for overflow and report a runtime error instead of wrapping.

| Instruction | Effect |
|---|---|
| `I_ADD`, `I_SUB`, `I_MULT` `R[a] R[b] R[c]` | `R[a] = R[b] op R[c]`, with overflow check |
| `TRUNC_DIV R[a] R[b] R[c]` | `//`: checks for zero and for `minInt // -1` |
| `I_MOD R[a] R[b] R[c]` | `%`: checks for zero; `x % -1` is `0` |
| `I_EQ`, `I_NEQ`, `I_LT`, `I_LE`, `I_GT`, `I_GE` | comparison, result in `.bool` |
| `I_NEG R[a] R[b]` | negation, checks for `minInt` |
| `I_TO_BYTE R[a] R[b]` | checks for `0..255` |
| `I_TO_F R[a]` | in place: int to float |
| `F_TO_I R[a]` | in place: truncates; reports an error if the result doesn't fit an int |

**Floats and bools**

| Instruction | Effect |
|---|---|
| `F_ADD`, `F_SUB`, `F_MULT`, `F_DIV`, `F_MOD` | float arithmetic (IEEE, no checks) |
| `F_EQ` ... `F_GE`, `F_NEG` | float comparisons and negation |
| `B_EQ`, `B_NEQ`, `B_NOT` | bool comparisons and negation |
| `B_AND`, `B_OR` | bool operations on two registers. Codegen doesn't emit them anymore: `and`/`or` compile to jumps (6.7) |

Mixed int/float arithmetic is lowered with `I_TO_F` on the int operand first (`genBinOp`).

**Strings and buffers**

| Instruction | Effect |
|---|---|
| `STR_BUILD R[a] RECIPE[bEx]` | formats `R[a+1 ..]` using the recipe's kinds into one new string (section 6.4) |
| `TO_STRING R[a] R[b] T[c]` | formats `R[b]` as kind `c` into a new string |
| `BYTES_NEW R[a] R[b]` | new zeroed buffer of length `R[b]` |
| `BYTES_LEN R[a] R[b]` | length of a string or buffer |
| `BYTES_GET R[a] R[b] R[c]`, `BYTES_SET R[a] R[b] R[c]` | read or write one byte, with bounds check |

**Arrays**

| Instruction | Effect |
|---|---|
| `ARRAY_INIT R[a] T[b] C[c]` | new empty array of element kind `b` and capacity `c` |
| `ARRAY_APPEND R[a] R[b]` | append `R[b]`, growing the array first if it is full |
| `ARRAY_LOAD R[a] R[b] R[c]`, `ARRAY_STORE R[a] R[b] R[c]` | read or write an element, with bounds check |
| `ARRAY_LEN R[a] R[b]` | length |
| `ARRAY_POP R[a] R[b]` | remove and return the last element (`0` if empty; the prelude's `pop` checks first) |
| `DEEP_COPY R[a] R[b]` | `copy(x)` (section 10) |

**Structs, unions and nullables**

| Instruction | Effect |
|---|---|
| `STRUCT_INIT R[a] D[bEx]` | new struct for descriptor `bEx`, all fields `0` |
| `STRUCT_LOAD R[a] R[b] F[c]`, `STRUCT_STORE R[a] F[b] R[c]` | read or write field `c` / `b` |
| `UNION_INIT R[a] D[bEx]` | new union with no active variant |
| `UNION_STORE R[a] T[b] R[c]` | set the payload and the active tag |
| `UNION_LOAD R[a] R[b] T[c]` | `u.variant` as `?T`: the payload if `c` is active, otherwise `0`. A primitive payload is put in a new `NullBox` |
| `UNION_TAG R[a] R[b]`, `UNION_PAYLOAD R[a] R[b]` | active tag and raw payload, used by `when` |
| `NULL_BOX_INIT R[a] K[b] R[c]` | box the primitive `R[c]` of kind `b` |
| `NULL_BOX_UNWRAP R[a] R[b]` | the boxed value, or `0` if `R[b]` is null |
| `NULL_BOX_NNULL R[a] R[b]` | `R[b] != 0` |

**Closures**: `MAKE_CLOSURE`, `CAPTURE_GET`, `CELL_NEW`, `CELL_GET`, `CELL_SET`. See section 8.

**Tasks**: `TASK_SPAWN R[a] R[b] K[c]` starts a task running the closure in `R[b]`, whose result has kind `c`, and puts its `Task` in `R[a]`. `await` and `sleep` are natives called with `CALL`. See section 11.

**Calls and control flow**

| Instruction | Effect |
|---|---|
| `CALL R[a] FN[bEx]` | call a Dusk or native function; the arguments are in `R[a+1 ..]` and the result goes in `R[a]` |
| `CALL_VALUE R[a]` | call the closure in `R[a]`; the arguments are in `R[a+1 ..]` |
| `TAIL_CALL R[a] FN[bEx]` | reuse the current frame for a call to a Dusk function |
| `RETURN R[a]` | return `R[a]` |
| `RETURN_ERR R[a]` | return the error id in `R[a]` |
| `PANIC_ERR R[a]` | stop the program: unhandled error `R[a]` |
| `JUMP I[aEx]` | jump |
| `JUMP_IF_FALSE R[a] I[bEx]` | jump if `R[a]` is false |
| `JUMP_TABLE R[a] T[bEx]` | jump through a table indexed by `R[a]` |

`STORE_VAR` still exists in `OpCode` but codegen never emits it and the VM has no handler for it.

## 6. Data in practice

### 6.1 Structs and fields

Fields are stored in declaration order, so `STRUCT_LOAD` and `STRUCT_STORE` use a field index fixed at compile time and there is no lookup at runtime. A new struct has all fields set to `0`. Codegen then stores each initial value with `STRUCT_STORE`.

### 6.2 Array growth and `relocated`

Growing an array can't update every reference to it: other registers, struct fields or arrays may point to the old copy. So `Array.resize` copies the array to a bigger allocation and sets `old.obj.relocated = new`. Then:

- Array instructions read their register through `getArrayFromHeapValuePtr`, which follows the `relocated` chain and also updates the register to the newest copy.
- Natives and the formatter call `followRelocated()` too.
- The GC copies the newest version and makes every reference point to it (`copyHeapValue` follows the chain first), so after a collection no chains are left.

Between collections the old copies stay in the heap, and an array that grows many times leaves a chain that later reads must walk.

### 6.3 Nullables and union reads

`?int` and the other nullable primitives need a pointer to tell null apart from a real `0`, so they are boxed in a `NullBox`. Nullable heap types use the `0` pointer directly. Reading a primitive union variant (`shape.circle` where `circle` is a `float`) gives a `?float`, so `UNION_LOAD` allocates a new box each time when that variant is active (`loadUnionVariant`). `when` doesn't go through this: it tests `UNION_TAG` and reads `UNION_PAYLOAD` directly.

### 6.4 Strings and interpolation

`"a {x} b {y}"` compiles to its parts in consecutive registers followed by `STR_BUILD R[a] RECIPE[i]`. The recipe is the list of each part's `ValueType`. `buildString` formats every part into `VM.scratch`, a reused buffer, then allocates the result once and copies it in.

- Parts of mutable types (arrays, structs, unions, buffers, nullables) get a `TO_STRING` right after they are evaluated. That way a later part that changes them can't change how an earlier part prints.
- A build holds at most 32 parts. Longer interpolations become several builds, each starting with the previous result as its first part.

### 6.5 `when`

When a `when` matches an int, byte, enum or union subject against at least 4 constant values (ranges count one value per number), and those values are dense enough (the span from smallest to largest is at most 1024 and at most twice the number of values), sema builds a jump table (`buildJumpTable`). Codegen emits `JUMP_TABLE R[a] T[i]` with `JumpTable { min, target_offsets, default }`. The VM computes `R[a] - min` and jumps to `target_offsets[offset]` when it is in range, or to `default` otherwise. Any other `when` becomes a chain of `JUMP_IF_FALSE` tests. A `when` on a nullable first tests for null with `JUMP_IF_FALSE`.

### 6.6 Loops

`for cond do ... end` becomes: condition, `JUMP_IF_FALSE` to the exit, body, advancement (for loops with a step), and a `JUMP` back to the condition. `break` and `continue` emit a `JUMP` with a placeholder target, which is patched when the loop is closed: `break` goes to the exit and `continue` to the advancement.

### 6.7 `and` and `or`

`and` and `or` short-circuit (`genShortCircuit`). The left side goes into a fresh register, a jump skips the right side when the left side already decides the result, and the right side otherwise overwrites that register:

```
a and b                          a or b
  <a> -> R[t]                      <a> -> R[t]
  JUMP_IF_FALSE R[t] end           JUMP_IF_FALSE R[t] right
  <b> -> R[t]                      JUMP end
end:                             right:
  LOAD R[target] R[t]              <b> -> R[t]
                                 end:
                                   LOAD R[target] R[t]
```

The fresh register matters for `flag = other and flag`: writing the left side straight into `flag`'s register would change `flag` before the right side reads it. Sema's own conditions (range checks in `when`, nullable `==`) are lowered the same way.

## 7. Calls, returns and errors

### 7.1 Calling a Dusk function

```
CALL R[a] FN[i]          ; function i is a Dusk function
```

1. Save `cur_inst = cur_instruction` (the return point) in the current frame.
2. Check the call depth and stack space.
3. Push `{ function, cur_inst = 0, stack_offset = stack_base + a + 1 }`.
4. Switch `current_frame`, `instructions`, `cur_instruction = 0` and `stack_base`.

`CALL_VALUE R[a]` does the same, but takes the function from the closure in `R[a]`.

### 7.2 Returning

```
RETURN R[a]
```

1. Pop the frame.
2. `stack[stack_base - 1] = R[a]`, which is the caller's call register. For a task's first frame that is `stack[0]`, where the task keeps its result.
3. If no frame is left, the task is done and `execute` ends.
4. Otherwise restore the caller's `instructions`, `stack_base`, and `cur_instruction = caller.cur_inst`.

`RETURN_ERR` does the same and, when no frame is left, marks the task as failed.

### 7.3 Natives and intrinsics

A **native** call doesn't push a frame:

```zig
const args = stack[stack_base + a + 1 ..][0..num_args];
const result = try native(vm, args);
if (result.is_parked) return;               // the task waits (section 11.3)
stack[stack_base + a] = result.value;
if (result.has_failed) cur_instruction += 1;
```

- `args` points directly into the caller's registers. A native that allocates can trigger a GC, which moves objects and updates those stack slots. So natives size their whole result, allocate once, and only then reread their arguments from `args`.
- Natives with `dynamic` parameters get one extra hidden `int` argument per `dynamic` parameter, after the normal ones, holding its `ValueType`. Codegen loads it with `genArgKinds`. This is how `echo` knows how to print.
- If a native returns a Zig error (a runtime error), its name is saved in `failed_builtin` for the error message.

**Intrinsics** never run as calls. Codegen evaluates the arguments into `R[a+1 ..]` as usual, then emits the instruction for that intrinsic (`genIntrinsicCall`), for example `len` → `ARRAY_LEN`, `append` → `ARRAY_APPEND`, `copy` → `DEEP_COPY`, `to_float` → `LOAD` + `I_TO_F`.

### 7.4 Tail calls

`return f(x)`, where `f` is a Dusk function called directly, becomes `TAIL_CALL R[a] FN[i]`:
1. Copy the arguments from `R[a+1 ..]` down to `R[0 ..]`.
2. Replace `current_frame.function`.
3. Restart at instruction 0.

The frame stays the same, so deep recursion in tail position doesn't use stack. This applies to `CALL` only: calls of function values, natives, and calls whose result is an error union are not tail calls. The one exception is a `try` that is not at the top level: its error goes back to the same caller either way.

### 7.5 Error unions

A value of type `E!T` takes one register. It holds either the success value or the error id (an `int64`). **Which of the two it holds is decided by control flow, not stored anywhere**. Each failing call is followed by a `JUMP` to the success path:

```
CALL       R[2] FN[42]   ; let a = try parse(n)
JUMP       I[4]          ; success lands here and jumps past the handler
RETURN_ERR R[2]          ; failure lands here: propagate the error id
LOAD       R[1] R[2]     ; success path
```

- `RETURN R[a]` resumes the caller at `cur_inst`, which is the `JUMP`, so the success path runs.
- `RETURN_ERR R[a]` resumes the caller at `cur_inst + 1`, skipping the `JUMP` and landing on the handler.
- A native returns `fail(id)` (`has_failed = true`), and the VM skips the `JUMP` the same way.
- The handler depends on the form (`genFailingCall`):
  - `try` inside a function: `RETURN_ERR`.
  - `try` at the top level: `PANIC_ERR`, which ends the program with "unhandled error".
  - `catch value`: the fallback value is loaded into the same register.
  - `catch e then ... end`: the block runs with `e` bound to the register.

This is why only `CALL` and `CALL_VALUE` can fail (`OpCode.canFail`), and codegen asserts that a `JUMP` always follows one. Error ids are global: builtin error sets use 1 to 8, then user errors follow in declaration order. `Program.error_names` maps an id back to its name.

A full example, from `let x = twice(3) catch 0` in `main`:

```
0000 LOAD_CONST  R[2] C[0]     ; 3
0001 CALL        R[1] FN[43]   ; twice(3)
0002 JUMP        I[4]          ; ok -> 4
0003 LOAD_CONST  R[1] C[1]     ; failed: fallback 0
0004 LOAD        R[0] R[1]     ; x
```

### 7.6 Runtime errors

Problems such as a bad index, an overflow, division by zero, a failed `assert` or running out of call depth are not Dusk errors. The reporter prints the message (`RuntimeErrorReporter.*`) and returns a Zig error, which goes up through `execute` to `VM.run`. There, `reportWhere` adds the name of the running function (or the top level, for `$`-prefixed names) and the name of the failing builtin, if any. The program stops: nothing in Dusk can catch these.

## 8. Closures and cells

What closures mean in the language is described in the language reference. For the VM:

- A function value is a `*Closure`. Functions and lambdas that capture nothing use their static closure (`LOAD_FUNC`), so taking one costs no allocation.
- `MAKE_CLOSURE R[a] FN[i]` allocates a closure with `capture_kinds.len` captures and copies them from `R[a+1 ..]`. Codegen evaluates the capture sources into those registers first.
- `CAPTURE_GET R[a] CAP[b]` reads capture `b` of the running closure, which is always at `stack[stack_base - 1]` (section 4.2).
- Captured variables that can't be reassigned are copied into the closure. A captured `let mut` is moved into a `Cell` when it is declared (`CELL_NEW`). From then on the function reads and writes it with `CELL_GET` and `CELL_SET`, and every closure capturing it shares that same cell. Cells only exist for variables that are both captured and `let mut`.
- A closure's captures are traced using `capture_kinds`; a boxed capture has kind `cell`.

## 9. The garbage collector

### 9.1 Design

The heap uses two arenas (`Heap.arenas`), and only one is active at a time. All objects are allocated with a bump allocator from the active arena (`getCurrentHeapAllocator`). A collection copies every live object into the other arena, switches arenas, and resets the old one in one step. Nothing is freed object by object.

Allocation sites follow one pattern:

```zig
try self.maybeCollectGarbage(size);            // may collect: objects may move
const obj = try T.init(self.getCurrentHeapAllocator(), ...);
self.increaseHeapAllocationCount(size);
```

`maybeCollectGarbage` collects when `allocated + size > threshold`. After a collection, `threshold = max(2 × live bytes, 1 MB)`. So the heap can grow to about twice the live data before the next collection.

### 9.2 Roots: heap maps

Values have no runtime tag, so the GC can't tell by looking whether a register holds a pointer or an int. Codegen tells it, for each instruction that can allocate, with a **heap map**:

```zig
HeapMap { pc: u16, bitmap: u256 }      // bit i set: R[i] holds a pointer at this instruction
```

After emitting such an instruction, codegen calls `recordHeapMap()`. That computes the bitmap from `register_types` at that moment (`computeHeapBitmap`). The instructions that can allocate are listed in `OpCode.canTriggerGc`: array, struct, union and null box creation, `UNION_LOAD`, string building, `BYTES_NEW`, `DEEP_COPY`, `MAKE_CLOSURE`, `CELL_NEW`, `TASK_SPAWN`, and calls. Calls count because the callee, or a native, can allocate.

The GC first saves the running task's stack and frames into its `TaskState`, then visits every task that isn't done (`copyTaskRoots`): its `stack[0]` (the entry closure), any value waiting to be delivered to it (section 11.3), and then, for every frame of that task:

```zig
pc = frame.cur_inst - 1                // the instruction this frame is stopped at
bitmap = binarySearch(chunk.heap_maps, pc) or 0
owned = top frame ? num_registers : next_frame.stack_offset - base
for each register i < owned with bit i set: copy stack[base + i]
```

- For caller frames, `cur_inst - 1` is their `CALL`. For the top frame of a parked task, it is the native `CALL` it is waiting in.
- A frame with `cur_inst == 0` hasn't started (a spawned task that never ran), so it has nothing to scan yet.
- For the running frame, it's the allocating instruction, because each such handler sets `current_frame.cur_inst = cur_instruction` before it can allocate.
- **A caller frame only scans `R[0] .. R[a]`**, up to and including its call register. Its argument registers are the callee's parameters (section 4.2), so the callee scans them. If both frames scanned them, the second pass would copy the already-copied object again, and the parameter would end up pointing to a different object than the caller's variable. The call register stays with the caller, so the closure of a `CALL_VALUE` is kept alive. Natives don't push frames, so the arguments of a native call are scanned as part of the top frame.

So the two rules for adding an allocating instruction are: **record a heap map in codegen, and set `cur_inst` in the VM before allocating.** If an instruction has no heap map, its frame gets bitmap `0` and none of its registers are kept alive.

A register's type is changed only *after* the heap map of the instruction that writes it is recorded. For example, `ARRAY_INIT R[3]` records its map while `R[3]` is still typed `void`. So the GC never reads a target register whose content is stale.

### 9.3 Copying

`copyHeapValue(slot)` for each root:

1. Skip `0`, pointers into the string pool, and static closures.
2. Follow `relocated` to the newest copy of an array.
3. If the object already has `gc_forward` set, point the slot at the copy.
4. Otherwise clone the object into the new arena, set `gc_forward` on the old object, point the slot at the clone, and push the clone onto `gc_queue` unless it is a string or buffer (those hold no pointers).

Then the GC empties `gc_queue`, which is used as a stack, so objects are visited depth first. For each object it copies the fields that hold pointers:

| Object | Which slots are traced |
|---|---|
| `Array` | all `len` elements, when `kind` is a heap kind |
| `Struct` | fields with their bit set in `heap_bitmap` |
| `Union` | the payload, when the active variant has its bit set in `heap_bitmap` |
| `Closure` | captures whose `capture_kinds` entry is a heap kind |
| `Cell` | `value`, when `kind` is a heap kind |
| `NullBox` | nothing (it only boxes primitives) |
| `Task` | marks its `TaskState` as reached; when the task is done and succeeded with a heap value, its `result` |

Clones are byte copies with `gc_forward` and `relocated` cleared.

Then `Scheduler.sweep` frees every done task that no `Task` handle reached, except failed tasks nobody awaited, which are kept for the report at the end. Afterwards, the new arena becomes active and the threshold is updated. If the old arena kept more capacity than the new threshold, it is reset with that limit and `gc_queue` is shrunk, so memory goes back to the OS after a peak.

## 10. `copy(x)`: deep copy

`DEEP_COPY` (`deepCopy`) clones a whole object graph:

- **Identity.** A map from original to copy ensures shared objects stay shared once, and cycles end.
- **Order.** A worklist processes objects in the same order as GC tracing.
- **What is shared instead of cloned.** Strings, null boxes, closures and cells are treated as immutable or shared by design, so copies point to the same object.
- **GC check.** `maybeCollectGarbage` runs once, at the start, for the size of the root object. The copies after that are allocated without checks, because the objects still waiting in the worklist aren't GC roots, so a collection in the middle would lose them.

## 11. Tasks

What tasks mean in the language is described in the language reference (section 13). In the VM, a task is a function call with its own stack: the program runs one task at a time, and switching tasks means leaving `execute` and entering it again with another task's stack.

### 11.1 Two objects per task

```zig
TaskState {                     // src/scheduler.zig, outside the GC heap: its address never changes
    stack: []Value, frames: []CallFrame, frame_count,
    status: ready | running | parked | done,
    has_failed, is_awaited, is_marked,
    result: Value, result_kind: ValueType,   // set when done (result_kind: kind of T, from TASK_SPAWN)
    pending: ?Delivery,                      // a value waiting to be written into the task (11.3)
    waiters: []*TaskState,                   // tasks parked in await on this one
}

Task { obj: HeapValue, state: *TaskState }   // src/value.zig: the Dusk value, in the GC heap
```

The GC moves objects, but the scheduler, waiter lists and libuv requests need pointers that stay valid, and stacks are large. So the Dusk value is a small `Task` handle that the GC copies like any object, and everything else lives in the `TaskState`, which never moves. `Scheduler.tasks` holds every `TaskState` that hasn't been freed.

**Slot 0.** A task's stack starts with its entry closure in `stack[0]` and its first frame at `stack_offset = 1`. That is the layout `CALL_VALUE` makes (section 4.2), so `CAPTURE_GET` finds the lambda's captures at `stack[base - 1]`, and the final `RETURN` writes the result into `stack[0]`. `$main` is a task too, with its static closure in `stack[0]`.

### 11.2 The scheduler loop

```
run:
    spawn the $main task
    loop:
        batch = number of ready tasks
        if batch > 0:
            run each of them (enterTask, execute, leaveTask; finish the ones with no frame left)
            if timers are pending: poll the event loop without blocking
            continue
        if timers are pending: block in the event loop until one fires; continue
        if some task isn't done: runtime error "all tasks are waiting"
        stop
    report failed tasks nobody awaited
```

- The ready queue is first in, first out. `spawn` adds the new task at the back, so it runs after the tasks already waiting.
- Ready tasks run in batches: the tasks ready when the batch starts. Polling the event loop between batches lets timers fire even while some task keeps giving way with `sleep(0)`, which would otherwise keep the queue full forever.
- `enterTask` puts the task's stack and frames in `VM.stack`, `VM.frames` and `VM.frame_count`; `leaveTask` saves them back, since a call may have grown them. The error path saves them too.
- `finish` copies `stack[0]` into `result`, frees the stack and frames, marks the task done, and delivers the result to every waiter.

### 11.3 Parking and delivering

A task waits by **parking inside a native call**. The native records who should wake it up, sets the task's status to `parked` and returns `NativeResult{ .is_parked = true }`. The `CALL` handler then returns from `execute`. The top frame already saved `cur_inst` just after the `CALL` (on the `JUMP` for a call that can fail), exactly as a caller frame does while it waits for a callee.

To wake a task, `Scheduler.deliver(task, value)` stores the value in `task.pending` and puts the task at the back of the ready queue. When the task is entered again, `enterTask` writes the value into the register of the `CALL` it parked in (`instructions[cur_inst - 1].a`), and moves `cur_inst` past the `JUMP` if the value is an error. That is the rule of `RETURN_ERR`, so `try`, `catch` and `catch e then` work on `await` with no new code.

The value waits in `pending` instead of going straight into the register because other tasks run in between, and any of them can start a collection. The `CALL`'s target register isn't in its heap map (section 9.2), so a pointer written there early wouldn't be kept alive. `pending` is a GC root, and it's written into the register only at the switch, when nothing can collect.

| Native | What it does |
|---|---|
| `await(t)` | Marks `t` as awaited. If `t` is done, returns its result (or fails with its error id) without parking. If `t` is the running task, a runtime error. Otherwise adds the running task to `t.waiters` and parks. |
| `sleep(0)` | Delivers `void` to the running task right away (back of the queue) and parks. |
| `sleep(ms)` | Starts a libuv timer for the running task and parks; the timer's callback delivers `void`. |

`spawn` isn't a native: it is the `TASK_SPAWN R[a] R[b] K[c]` intrinsic, which creates the `TaskState` from the closure in `R[b]`, records `c` (the kind of `T`) as `result_kind`, queues it, and puts a new `Task` handle in `R[a]`.

### 11.4 The event loop

`EventLoop` (`src/event_loop.zig`) is the only code that calls libuv. It creates its `uv_loop_t` on the first `sleep` with a time above 0, so programs that never sleep don't start one. Each `sleep` allocates a request holding the timer handle, the scheduler and the task; the timer's callback only calls `deliver` and closes the handle (the close callback frees the request). Dusk code never runs inside a libuv callback.

libuv's structs differ per OS, so Dusk doesn't use their layout: `src/uv.zig` declares the functions it calls with opaque `Loop` and `Handle` types, and their memory is allocated with the sizes libuv reports (`uv_loop_size`, `uv_handle_size`). The handle's `data` (`uv_handle_set_data`) points back to the request. `uv_update_time` runs before each timer starts, since the loop's clock only moves when the loop runs. At exit, `deinit` closes every handle still open, runs the loop until they are closed, and closes the loop.

### 11.5 Freeing tasks

A task that isn't done is never freed, even with no handle left (a task nobody keeps still runs to the end). A done task is freed by the sweep after a collection when no handle reached it (section 9.3). A failed task that nobody awaited is kept until the program ends, so `run` can report it.

## 12. Known issues and observations

### 12.1 Fixed: the GC copied call arguments twice

Fixed in v0.15 (section 9.2); `test/success/gc-heap-arguments.dsk` covers it. It's kept here because the failure mode is worth knowing when changing frame layout or root scanning.

The argument registers of a Dusk call are both the caller's `R[a+1 ..]` and the callee's parameters (section 4.2). Both heap maps mark them as pointers, so before the fix, one collection copied the same slot twice:

1. The caller's pass copies object `O` to `N` and points the slot at `N`.
2. The callee's pass sees `N`, which has no `gc_forward` yet, clones it again to `N2`, and sets `N.gc_forward = N2`.

Now the parameter points to `N2` while every other reference points to `N`, which is a split identity. Worse, `N` keeps a `gc_forward` into the current arena. At the next collection, any reference to `N` is redirected to `N2` instead of being copied, which leaves it pointing into the arena that is about to be reset. A later collection then reads freed memory and crashes. Any heap value passed to a Dusk function that allocates enough to collect twice triggers it:

```dusk
func churn(b: []int) -> void
    let mut i = 0
    for i < 200000 do
        let tmp = [i, i, i, i]
        i = i + 1
    end
end

func main() -> void
    let b = [1]
    churn(b)
    echo(b)
end
```

This segfaulted in `copyHeapValue`. Now each caller frame scans only up to its call register, so every slot has one owner and is copied once.

### 12.2 Observations

- **A missing heap map is silent.** If an instruction has no heap map, its frame gets bitmap `0` and its roots aren't kept. A debug assertion in `collectGarbage` when `binarySearch` finds nothing would catch codegen mistakes right away.
- **Extra moves.** Codegen copies variables into temporaries before using them (`LOAD R[3] R[1]` before `I_LT R[2] R[3] R[4]`), and moves a call result through more than one register after a `try`. Reading variables in place in binary operations, or a small peephole pass, would remove many instructions.
- **Repeated `CAPTURE_GET`.** A lambda reloads the same capture each time it uses it.
- **The header is 24 bytes.** Only arrays use `relocated`, but every object pays for it. Moving it into `Array`, or reusing `gc_forward` for it, would make every object 8 bytes smaller.
- **Array relocation chains.** Each growth leaves the old copy until the next GC, and references that still point to an old copy walk the chain on every access.
- **`copy` uses the page allocator.** `deepCopy`'s map and worklist use `VM.allocator`, which is `std.heap.page_allocator`. So each `copy` of a non-trivial value makes page-sized system allocations, which is costly in a loop.
- **Union reads allocate.** Each `u.variant` read of an active primitive variant allocates a new `NullBox`.
- **Struct printing.** `format.printValue` prints every struct field as an `int64`.
- **Task stacks aren't reused.** Each `spawn` allocates a new stack and frame array, and a finished task frees them. A free list of default-size stacks would make spawning cheaper.
- **`STORE_VAR` is dead.** It is still in `OpCode` and the disassembler, but nothing emits it and the VM has no handler.
