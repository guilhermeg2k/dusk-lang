# VM internals

How Dusk programs run: the compiled program format, values and heap objects, the register machine, calls and errors, the garbage collector and tasks. It describes the code as of v0.16 (in development). The language itself is described in [language-ref.md](language-ref.md); how the standard library's modules work inside the runtime (files, streams, TCP, TLS, HTTP, compression) is in [std-internals.md](std-internals.md).

The source files involved:

| File | What it holds |
|---|---|
| `src/compiler/codegen.zig` | Code generation from IR (`BytecodeGen`) |
| `src/runtime/bytecode.zig` | The compiled program format (`Program`, `Function`, `Chunk`), the instruction format and opcodes |
| `src/runtime/vm.zig` | The interpreter (`VM.execute`), allocation helpers, `copy` |
| `src/runtime/gc.zig` | The garbage collector: `Heap` (the two arenas, allocation counters, `collect`) |
| `src/runtime/value.zig` | `Value`, `ValueType`, every heap object layout and the work on them that doesn't allocate (array items; map hashing, lookup, insert and removal, section 6.8) |
| `src/runtime/format.zig` | Formatting values (interpolation, `TO_STRING`) |
| `src/runtime/stdio.zig` | Writing to stdout and stderr (`Stdio.writeBytes`, flush rules) |
| `src/runtime/runtime_error.zig` | `RuntimeErrorReporter`, the runtime error messages |
| `src/runtime/builtins/prelude.zig` | Prelude builtins written in Zig |
| `src/runtime/builtins/std.zig`, `std/*.zig` | Standard library registry and each module's natives |
| `src/runtime/builtins/native.zig` | Shared native types (`Binding`, `NativeResult`, the `Intrinsic` table) and helpers |
| `src/runtime/builtins/prelude.dsk` | The builtins' signatures, compiled before the user's file |
| `src/runtime/tasks/scheduler.zig` | `Scheduler` (every task, the ready queue) and `TaskState` (a task's stack, frames and result) |
| `src/runtime/tasks/event_loop.zig` | `EventLoop`: the libuv loop, timers for `sleep`, queuing fs jobs, and immediate clock helpers |
| `src/runtime/tasks/uv.zig` | The few libuv functions Dusk calls, declared by hand |
| `src/runtime/tasks/handle_table.zig` | `HandleTable(T)`: the stream and listener tables, whose handles carry a slot generation so freed slots are reused safely |
| `src/runtime/tasks/resource_registry.zig` | `ResourceRegistry`: what each GC-tracked native resource is (kind, handle, released), for the sweep after each collection (section 9.3) |

Code in `src/runtime/` never imports `src/compiler/`: the compiler produces the runtime's format (`codegen.zig` writes `bytecode.zig`'s `Program`), not the other way around.

---

## 1. From source to a running program

`Dusk.compile` (`src/dusk.zig`) parses the prelude and the user's file, runs sema to get IR, and passes it to `BytecodeGen.generate`, which returns a `Program`. Everything before codegen uses an arena that is freed once codegen finishes. The `Program` outlives it.

`Dusk.compileAndRun` then creates a `VM` and calls `VM.run`, which:

1. creates the first task, for `$main`, the function codegen builds from the top-level code (section 11),
2. runs the scheduler loop: it runs ready tasks with `execute` until every task is done,
3. reports failed tasks nobody awaited,
4. on a runtime error, prints where it happened (`reportErrorLocation`) and returns the error.

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

How Dusk types map to values (`valueTypeFromTypeId` in `codegen.zig`):

| Dusk type | Runtime value |
|---|---|
| `int`, `byte`, enums, error ids | `int64` |
| `float` | `float64` |
| `bool` | `bool` |
| `void`, `null` | `0` |
| `string`, `buffer` | pointer to `Bytes` |
| `[]T` | pointer to `Array` |
| `map[K, V]` | pointer to `Map` |
| struct | pointer to `Struct` |
| union | pointer to `Union` |
| `?int`, `?float`, `?bool`, `?byte`, `?Enum` | pointer to `NullBox`, or `0` |
| `?string`, `?[]T`, `?Struct`, ... | the pointer itself, or `0` |
| `func(...) -> T` | pointer to `Closure` |
| `Task[T, E]` | pointer to `Task` |
| `resource` (std/prelude internal) | pointer to `Resource` |
| captured `let mut` (internal) | pointer to `Cell` |

The few places that need a type at runtime carry it explicitly:
- `Array.kind`: the element kind, used for printing and GC tracing.
- `Map.key_type` / `Map.value_type` (copied into `MapStorage`): how to hash and compare keys, and what the GC traces.
- `NullBox.kind` and `Cell.kind`: the kind of the boxed value.
- Struct and union descriptors.
- `capture_kinds` for closures.
- `build_recipes` for interpolation.

### 3.2 Heap objects

Every heap object starts with the same header:

```zig
HeapValue = extern struct {
    kind: HeapValueType,         // array, struct, string, buffer, union, null_box, closure, cell, task, map, map_storage, resource
    gc_forward: ?*HeapValue,     // set during a GC: where this object was copied
}
```

The header is 16 bytes. Each object type embeds it as its `obj` field, and code goes from a header to the full object with `HeapValue.getParentPtr(T, ptr)`. Variable-size objects store their data right after the struct, in the same allocation (`getDataPtr()`).

| Object | Fields after the header | Data after the struct | Size |
|---|---|---|---|
| `Bytes` (string, buffer) | `len` | `len` bytes | 24 + len |
| `Array` | `kind`, `relocated`, `len`, `capacity` | `capacity` values | 48 + 8 × capacity |
| `Struct` | `descriptor_id`, `field_count`, `identity` | `field_count` values | 24 + 8 × fields |
| `Union` | `descriptor_id`, `active_tag` | one value (the payload) | 32 |
| `NullBox` | `kind`, `value` | none | 32 |
| `Closure` | `function`, `capture_count` | `capture_count` values | 32 + 8 × captures |
| `Resource` | `index`, `generation` (an id into `EventLoop.resources`) | none | 24 |
| `Cell` | `kind`, `value` | none | 32 |
| `Task` | `state` (a `*TaskState`, section 11) | none | 24 |
| `Map` | `key_type`, `value_type`, `next_seq`, `seed`, `storage` | none | 40 |
| `MapStorage` | `key_type`, `value_type`, `len`, `count`, `capacity`, `entry_index_mask` | `capacity` entries, then the entry indexes | 40 + 24 × capacity + 4 × entry index slots |

Notes:

- **Strings and buffers** share `Bytes` and differ only in `kind`. Strings are immutable. Buffers are changed in place with `BYTES_SET`.
- **Arrays** start with a capacity of at least 8 (`ARRAY_INIT` takes `max(len, 8)`, capped at 255 as an operand), and grow by 1.5× (`calcNewCapacity`). Growing allocates a new array and leaves `Array.relocated` pointing to it in the old one (section 6.2).
- **Maps** are two objects: a fixed `Map` header that values point to, and a `MapStorage` it owns (section 6.8).
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
- For `CALL_CLOSURE`, `stack[stack_base - 1]` holds the closure being called for the whole call. That's how `CAPTURE_GET` finds its captures (section 8).

Before pushing a frame, a call checks that a frame is free and that `new_offset + 256` slots still fit on the stack. Otherwise `growCallStack` (a cold path) doubles the frames or the stack with `realloc`, zero-fills the new slots, and the handler reloads its `stack` local. Growing is safe because frames hold offsets, captured variables live in heap `Cell`s and natives never call back into Dusk: nothing keeps a pointer into a stack across a call. Past the limits it fails with "call depth exceeded".

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

Slow paths are `noinline` helpers: `makeClosure`, `newCell`, `newBuffer`, `deepCopy`, `toString`, `buildString`, `loadUnionVariant`, and `Heap.collect` in `gc.zig`. Error reporters are `noinline` with `@branchHint(.cold)`. That keeps `execute` small. Performance is very sensitive to the machine code layout of this loop, so even unrelated edits to it can move benchmarks noticeably. `execute` is declared `align(64)`: without it, code added anywhere before it in the binary (even in the compiler) could move its start off a 64-byte boundary and shift every jump target inside it, which can slow the interpreter by tens of percent with an identical loop.

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
| `B_AND`, `B_OR` | bool operations on two registers. Codegen doesn't emit them: `and`/`or` compile to jumps (6.7) |

Mixed int/float arithmetic is lowered with `I_TO_F` on the int operand first (`genBinOp`).

**Strings and buffers**

| Instruction | Effect |
|---|---|
| `STR_BUILD R[a] RECIPE[bEx]` | formats `R[a+1 ..]` using the recipe's kinds into one new string (section 6.4) |
| `TO_STRING R[a] R[b] T[c]` | formats `R[b]` as kind `c` into a new string |
| `BYTES_NEW R[a] R[b]` | new zeroed buffer of length `R[b]` |
| `BYTES_LEN R[a] R[b]` | length of a string or buffer |
| `STR_EQ R[a] R[b] R[c]` | `R[b]` and `R[c]` are `?string`: `true` when both are `null` or have the same bytes. Same pointer is `true` right away, so a string compared with itself or with the same literal (string pool) skips the byte comparison |
| `BYTES_GET R[a] R[b] R[c]`, `BYTES_SET R[a] R[b] R[c]` | read or write one byte, with bounds check |

**Arrays**

| Instruction | Effect |
|---|---|
| `ARRAY_INIT R[a] T[b] C[c]` | new empty array of element kind `b` and capacity `c` |
| `ARRAY_APPEND R[a] R[b]` | append `R[b]`, growing the array first if it is full |
| `ARRAY_LOAD R[a] R[b] R[c]`, `ARRAY_STORE R[a] R[b] R[c]` | read or write an element, with bounds check |
| `ARRAY_LEN R[a] R[b]` | length |
| `DEEP_COPY R[a] R[b]` | `copy(x)` (section 10) |

**Maps** (section 6.8). The handlers call `Map` methods (`value.zig`); the three that can allocate go through VM helpers (`newMap`, `readMapEntry`, `setMapEntry`).

| Instruction | Effect |
|---|---|
| `MAP_INIT R[a] T[b] T[c]` | new empty map with key kind `b` and value kind `c` (the storage comes with the first entry) |
| `MAP_GET R[a] R[b] R[c]` | `R[b][R[c]]` as `?V`: `0` when missing; a primitive value is put in a new `NullBox` |
| `MAP_SET R[a] R[b] R[c]` | `R[a][R[b]] = R[c]`, growing the storage first if it is full |
| `MAP_FIND R[a] R[b] R[c]` | `R[a]` = whether `R[c]` has an entry, `R[a+1]` = its raw value (`if m[k]: v` and `m[k] orelse d` with primitive values) |
| `MAP_NEXT R[a] R[b] R[c]` | moves the loop cursor `R[c]` to the next live entry of `R[b]`; `R[a]` = whether there was one |
| `MAP_KEY_AT R[a] R[b] R[c]`, `MAP_VALUE_AT R[a] R[b] R[c]` | key or value of the entry under cursor `R[c]` |

`std/map` (`len`, `has`, `remove`, `keys`, `values`, `clear`) are natives called with `CALL`.

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
| `CALL_CLOSURE R[a]` | call the closure in `R[a]`; the arguments are in `R[a+1 ..]` |
| `TAIL_CALL R[a] FN[bEx]` | reuse the current frame for a call to a Dusk function |
| `RETURN R[a]` | return `R[a]` |
| `RETURN_ERR R[a]` | return the error id in `R[a]` |
| `PANIC_ERR R[a]` | stop the program: unhandled error `R[a]` |
| `JUMP I[aEx]` | jump |
| `JUMP_IF_FALSE R[a] I[bEx]` | jump if `R[a]` is false |
| `JUMP_TABLE R[a] T[bEx]` | jump through a table indexed by `R[a]` |

## 6. Data in practice

### 6.1 Structs and fields

Fields are stored in declaration order, so `STRUCT_LOAD` and `STRUCT_STORE` use a field index fixed at compile time and there is no lookup at runtime. A new struct has all fields set to `0`. Codegen then stores each initial value with `STRUCT_STORE`.

### 6.2 Array growth and `relocated`

Growing an array can't update every reference to it: other registers, struct fields or arrays may point to the old copy. So `Array.resize` (and `array::extend`) copies the array to a bigger allocation and sets `old.relocated = new`. Only arrays have this field; every other object's address changes only in a GC. Then:

- Array instructions read their register through `getArrayFromHeapValuePtr`, which follows the chain with `Array.newestCopy()` and also updates the register to the newest copy.
- Natives and the formatter that read an array call `newestCopy()` too.
- Code that handles any kind of object (the GC, `copy`) calls `HeapValue.followRelocated()`, which follows the chain for an array and returns any other object unchanged.
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

### 6.8 Maps

A `Map` header never moves except in a GC, so growing a map doesn't need `relocated`: only its `storage` pointer changes. Like arrays, the work on entries that doesn't allocate is a method of `Map` or `MapStorage` in `value.zig`; allocation (`newMap`, `ensureRoomForMapEntry`, boxing in `readMapEntry`) lives on the VM next to `growArray`, and the loop cursor (`MapCursor`) is in `vm.zig`. `MapStorage` holds the entries in insertion order, followed by an open-addressing index:

```zig
MapEntry = extern struct { key: Value, value: Value, hash: u32, seq: u32 }  // 24 bytes
```

- **Entry indexes** (`entryIndexes()`). A power-of-two array of `u32`, at least twice the capacity, so it is at most half full. A slot holds `entry position + 1`, or `0` when empty. A lookup hashes the key and probes linearly from `hash & entry_index_mask`.
- **Hashing** (`Map.hashKey`): ints, bytes and enums hash their `i64`; bools only `value.bool` (the other 7 bytes of a bool `Value` are undefined); strings `Wyhash` over their bytes, seeded with the map's seed; structs their `identity`. The result is xored with the map's seed, goes through `std.hash.int` and keeps 31 bits.
- **Seeds.** Each map has its own random `seed` (from `Map.seed_generator`, a SplitMix64 that `Dusk.runProgram` starts from `io.random` through `Map.randomizeSeeds`), so colliding keys can't be computed ahead of time, and what collides in one map says nothing about another. A map gets a new seed when it becomes empty (`clear`, or `remove` of the last live entry): no live entry needs rehashing then, and tombstones never match. Growth keeps the seed, so it reuses the stored hashes. `copy` keeps the seed, since the copied index was built with it.
- **Equality.** Ints by value, strings by length and bytes, structs by pointer. The GC updates both the stored key and the key being looked up, so pointer equality survives collections.
- **Struct identity.** The copying GC moves structs, so a struct key can't hash its address. `Struct.identity` (a `u32` in the padding after `field_count`, so it costs no space) is `0` until the struct is first hashed, then takes the next value of a global counter (`Struct.ensureIdentity`). GC clones keep it (byte copies). `copy` resets it on the copies. Wrapping the counter only makes buckets collide.
- **Removal** sets the top bit of `hash`, clears the key and value (so the GC frees what they held), and leaves the index slot pointing at the dead entry so probe chains stay intact. It never allocates.
- **Growth.** When `len == capacity`, `VM.ensureRoomForMapEntry` allocates a new storage and copies the live entries in order, rebuilding the index: the same capacity when at least a quarter of the entries are removed (compaction), otherwise twice it. The first storage has 8 entries.
- **Loops.** Each entry has a `seq` (insertion number, increasing along the entry array; `Map.next_seq` starts at 1). The loop cursor is one int register: the position after the visited entry (low 32 bits) and that entry's `seq` (high 32 bits). The handlers unpack it into a `MapCursor` (`vm.zig`), call `advance(map)` or `entryIn(map)`, and pack it back. The cursor relies on `value.zig` keeping seqs increasing along the entries and never reused: `Map.append` stamps them, `copyLiveEntriesFrom` keeps their order, and `clear` doesn't reset `next_seq`. `MAP_NEXT` checks the visited entry is still at its position; if a compaction moved entries, it binary searches the first entry with a larger `seq`. Removed entries are skipped and appended ones are reached at the end. `seq` wraps after 4 billion inserts into one map; a loop running at that point could revisit entries.
- **Reads without boxes.** `MAP_GET` boxes a primitive value because `m[k]` is a `?V`. Sema lowers `if m[k]: v` and `m[k] orelse d` (non-nullable map, primitive non-nullable `V`) to `ir.map_find`, which codegen turns into `MAP_FIND` with two consecutive registers bound to the found flag and the raw value.

## 7. Calls, returns and errors

### 7.1 Calling a Dusk function

```
CALL R[a] FN[i]          ; function i is a Dusk function
```

1. Save `cur_inst = cur_instruction` (the return point) in the current frame.
2. Check the call depth and stack space.
3. Push `{ function, cur_inst = 0, stack_offset = stack_base + a + 1 }`.
4. Switch `current_frame`, `instructions`, `cur_instruction = 0` and `stack_base`.

`CALL_CLOSURE R[a]` does the same, but takes the function from the closure in `R[a]`.

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
if (result.has_failed) {
    stack[stack_base + a].int64 += error_base;
    cur_instruction += 1;
}
```

- `args` points directly into the caller's registers. A native that allocates can trigger a GC, which moves objects and updates those stack slots. So natives size their whole result, allocate once, and only then reread their arguments from `args`.
- If a native returns a Zig error (a runtime error), its name is saved in `failed_builtin` for the error message.

`array::pop`, `array::insert` and `array::remove` are checked natives. They return `ArrayError.Empty` or `ArrayError.OutOfRange` through the native error path; `array::pop_or_null` is a Dusk wrapper over `pop`.

**Intrinsics** never run as calls. Codegen evaluates the arguments into `R[a+1 ..]` as usual, then emits the instruction for that intrinsic (`genIntrinsicCall`), for example `len` → `ARRAY_LEN`, `append` → `ARRAY_APPEND` + `LOAD` (returning the array after any growth), `copy` → `DEEP_COPY`, `to_float` → `LOAD` + `I_TO_F`. `==` and `!=` on strings are lowered by sema to the hidden prelude intrinsic `~strings_equal`, which becomes `STR_EQ` (`!=` adds a `B_NOT`).

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
- A native returns `fail(Set.Variant)` (`has_failed = true`), and the VM skips the `JUMP` the same way. The native only knows the variant's position in its error set (a Zig enum mirroring the `.dsk` set); the compiler stores the set's first error id on the native (`error_base`) and the VM adds it. A native whose error comes from elsewhere (`await` passing on a task's error) has base 0 and returns the full id.
- The handler depends on the form (`genFailingCall`):
  - `try` inside a function: `RETURN_ERR`.
  - `try` at the top level: `PANIC_ERR`, which ends the program with "unhandled error".
  - `catch value`: the fallback value is loaded into the same register.
  - `catch e then ... end`: the block runs with `e` bound to the register.

This is why only `CALL` and `CALL_CLOSURE` can fail (`OpCode.canFail`), and codegen asserts that a `JUMP` always follows one. Error ids are global: each variant gets the next id, starting at 1, as its error set is registered (`TypeTable`), so 0 is never an error. `Program.error_names` maps an id back to its name.

A full example, from `let x = twice(3) catch 0` in `main`:

```
0000 LOAD_CONST  R[2] C[0]     ; 3
0001 CALL        R[1] FN[43]   ; twice(3)
0002 JUMP        I[4]          ; ok -> 4
0003 LOAD_CONST  R[1] C[1]     ; failed: fallback 0
0004 LOAD        R[0] R[1]     ; x
```

`defer` / `errdefer` never reach the runtime: sema lowers each deferred statement once, where it's written (so names resolve there, even if an inner block later shadows them), and puts that same IR block at every exit of its block: before the block's end, before `break` / `continue` (the scopes inside the loop), and before `return` (all scopes of the function, after the return value is stored in a temporary; `errdefer` blocks only for `return_error`). A failing `try` carries them in `ir.TryCall.cleanup`, which codegen emits between the failure jump and `RETURN_ERR`.

### 7.6 Runtime errors

Problems such as a bad index, an overflow, division by zero, a failed `assert` or running out of call depth are not Dusk errors. The reporter prints the message (`RuntimeErrorReporter.*`) and returns a Zig error, which goes up through `execute` to `VM.run`. There, `reportErrorLocation` adds the name of the running function (or the top level, for `$`-prefixed names) and the name of the failing builtin, if any. The program stops: nothing in Dusk can catch these.

## 8. Closures and cells

What closures mean in the language is described in the language reference. For the VM:

- A function value is a `*Closure`. Functions and lambdas that capture nothing use their static closure (`LOAD_FUNC`), so taking one costs no allocation.
- `MAKE_CLOSURE R[a] FN[i]` allocates a closure with `capture_kinds.len` captures and copies them from `R[a+1 ..]`. Codegen evaluates the capture sources into those registers first.
- `CAPTURE_GET R[a] CAP[b]` reads capture `b` of the running closure, which is always at `stack[stack_base - 1]` (section 4.2).
- Captured variables that can't be reassigned are copied into the closure. A captured `let mut` is moved into a `Cell` when it is declared (`CELL_NEW`). From then on the function reads and writes it with `CELL_GET` and `CELL_SET`, and every closure capturing it shares that same cell. Cells only exist for variables that are both captured and `let mut`.
- A closure's captures are traced using `capture_kinds`; a boxed capture has kind `cell`.

## 9. The garbage collector

### 9.1 Design

The GC lives in `src/runtime/gc.zig`. The heap uses two arenas (`Heap.arenas`), and only one is active at a time. All objects are allocated with a bump allocator from the active arena (`getCurrentHeapAllocator`). A collection copies every live object into the other arena, switches arenas, and resets the old one in one step. Nothing is freed object by object.

Allocation sites follow one pattern:

```zig
try self.maybeCollectGarbage(size);            // may collect: objects may move
const obj = try T.init(self.getCurrentHeapAllocator(), ...);
self.increaseHeapAllocationCount(size);
```

`maybeCollectGarbage` asks `Heap.needsCollection`, and when `allocated + size > threshold` it saves the running task's stack and calls `Heap.collect`. After a collection, `threshold = max(2 × live bytes, 1 MB)`. So the heap can grow to about twice the live data before the next collection.

### 9.2 Roots: heap maps

Values have no runtime tag, so the GC can't tell by looking whether a register holds a pointer or an int. Codegen tells it, for each instruction that can allocate, with a **heap map**:

```zig
HeapMap { pc: u16, bitmap: u256 }      // bit i set: R[i] holds a pointer at this instruction
```

After emitting such an instruction, codegen calls `recordHeapMap()`. That computes the bitmap from `register_types` at that moment (`computeHeapBitmap`). The instructions that can allocate are listed in `OpCode.canTriggerGc`: array, struct, union and null box creation, `UNION_LOAD`, string building, `BYTES_NEW`, `DEEP_COPY`, `MAKE_CLOSURE`, `CELL_NEW`, `TASK_SPAWN`, and calls. Calls count because the callee, or a native, can allocate.

`Heap.collect` builds a `Collector` for one collection (the heap, the program and the new arena). The running task's stack and frames are already saved into its `TaskState`. It first copies the **temporary roots** (`Heap.temporary_roots`), then visits every task (`copyTaskRoots`): its `wake_result` if a heap value is waiting there (section 11.3), and, for a task that isn't done, its `stack[0]` (the entry closure) and every frame:

```zig
pc = frame.cur_inst - 1                // the instruction this frame is stopped at
bitmap = binarySearch(chunk.heap_maps, pc)   // asserted to exist
owned = top frame ? num_registers : next_frame.stack_offset - base
for each register i < owned with bit i set: copy stack[base + i]
```

- For caller frames, `cur_inst - 1` is their `CALL`. For the top frame of a parked task, it is the native `CALL` it is waiting in.
- A frame with `cur_inst == 0` hasn't started (a spawned task that never ran), so it has nothing to scan yet.
- For the running frame, it's the allocating instruction, because each such handler sets `current_frame.cur_inst = cur_instruction` before it can allocate.
- **A caller frame only scans `R[0] .. R[a]`**, up to and including its call register. Its argument registers are the callee's parameters (section 4.2), so the callee scans them. If both frames scanned them, the second pass would copy the already-copied object again, and the parameter would end up pointing to a different object than the caller's variable. The call register stays with the caller, so the closure of a `CALL_CLOSURE` is kept alive. Natives don't push frames, so the arguments of a native call are scanned as part of the top frame.

So the two rules for adding an allocating instruction are: **record a heap map in codegen, and set `cur_inst` in the VM before allocating.** A frame stopped at an instruction with no heap map fails the debug assertion in `copyTaskRoots`.

A register's type is changed only *after* the heap map of the instruction that writes it is recorded. For example, `ARRAY_INIT R[3]` records its map while `R[3]` is still typed `void`. So the GC never reads a target register whose content is stale.

### 9.3 Copying

`copyHeapValue(slot)` for each root:

1. Skip `0`, pointers into the string pool, and static closures.
2. Follow `Array.relocated` to the newest copy of an array.
3. If the object already has `gc_forward` set, point the slot at the copy.
4. Otherwise clone the object into the new arena, set `gc_forward` on the old object, point the slot at the clone, and push the clone onto `gc_queue` unless it is a string, buffer, null box or resource (those hold no pointers).

Then the GC empties `gc_queue`, which is used as a stack, so objects are visited depth first. For each object it copies the fields that hold pointers:

| Object | Which slots are traced |
|---|---|
| `Array` | all `len` elements, when `kind` is a heap kind |
| `Struct` | fields with their bit set in `heap_bitmap` |
| `Union` | the payload, when the active variant has its bit set in `heap_bitmap` |
| `Closure` | captures whose `capture_kinds` entry is a heap kind |
| `Cell` | `value`, when `kind` is a heap kind |
| `NullBox` | nothing (it only boxes primitives) |
| `Task` | sets `is_reachable` on its `TaskState`; when the task is done and succeeded with a heap value, its `result` |
| `Resource` | nothing (it's a leaf, like strings) |
| `Map` | its `storage` |
| `MapStorage` | keys when `key_type` is a heap kind, values when `value_type` is, for the `len` used entries (removed ones hold `0`) |

Clones are byte copies with `gc_forward` cleared (and `relocated`, for arrays).

Then `Scheduler.freeUnreachableTasks` frees every done task that no `Task` handle reached, except failed tasks nobody awaited, which are kept for the report at the end. Afterwards, the new arena becomes active and the threshold is updated. If the old arena kept more capacity than the new threshold, it is reset with that limit and `gc_queue` is shrunk, so memory goes back to the OS after a peak.

**Resources.** A std struct that owns a native resource (the list is in [std-internals](std-internals.md) section 9) keeps its `int` handle for the natives plus a file-only `owner` field: a `Resource` object holding only an id into `EventLoop.resources` (`ResourceRegistry`, `tasks/resource_registry.zig`: kind, handle, generation, released flag). `copy()` shares it, so every copy holds one owner. `Heap.resources` lists the live owners; at the end of a collection `sweepResources` keeps the ones that were moved (`gc_forward`) and puts the others' ids in `dead_resource_ids`. `gc.zig` doesn't know the event loop: `VM.maybeCollectGarbage` hands each dead id to `EventLoop.closeResource`, which ignores stale ids (generation) and released slots (`close()` / `finish()` call `release`) and closes the rest by kind. The `resource` type exists only in std modules and the prelude.

## 10. `copy(x)`: deep copy

`DEEP_COPY` (`deepCopy`) clones a whole object graph:

- **Identity.** A map from original to copy ensures shared objects stay shared once, and cycles end.
- **Order.** A worklist processes objects in the same order as GC tracing.
- **What is shared instead of cloned.** Strings, null boxes, closures, cells, tasks and resources are treated as immutable or shared by design, so copies point to the same object. Map keys are shared too: the copy clones the `Map` and its `MapStorage` and deep-copies only the values, so a struct key still finds its entry. Struct copies get `identity = 0`.
- **GC check.** `maybeCollectGarbage` runs once, at the start, for the size of the root object. The copies after that are allocated without checks, because the objects still waiting in the worklist aren't GC roots, so a collection in the middle would lose them.

## 11. Tasks

What tasks mean in the language is described in the language reference (section 13). In the VM, a task is a function call with its own stack: the program runs one task at a time, and switching tasks means leaving `execute` and entering it again with another task's stack.

### 11.1 Two objects per task

```zig
TaskState {                     // src/runtime/tasks/scheduler.zig, outside the GC heap: its address never changes
    stack: []Value, frames: []CallFrame, frame_count,
    status: ready | running | parked | done,
    has_failed, is_awaited, is_reachable,
    result: Value, result_kind: ValueType,   // set when done (result_kind: kind of T, from TASK_SPAWN)
    wake_result: ?WakeResult,                // the parked CALL's result, waiting to be written (11.3)
    waiters: []*TaskState,                   // tasks parked in await on this one
}

Task { obj: HeapValue, state: *TaskState }   // src/runtime/value.zig: the Dusk value, in the GC heap
```

The GC moves objects, but the scheduler, waiter lists and libuv requests need pointers that stay valid, and stacks are large. So the Dusk value is a small `Task` handle that the GC copies like any object, and everything else lives in the `TaskState`, which never moves. `Scheduler.tasks` holds every `TaskState` that hasn't been freed.

**Slot 0.** A task's stack starts with its entry closure in `stack[0]` and its first frame at `stack_offset = 1`. That is the layout `CALL_CLOSURE` makes (section 4.2), so `CAPTURE_GET` finds the lambda's captures at `stack[base - 1]`, and the final `RETURN` writes the result into `stack[0]`. `$main` is a task too, with its static closure in `stack[0]`.

### 11.2 The scheduler loop

```
run:
    spawn the $main task
    loop:
        batch = number of ready tasks
        if batch > 0:
            run each of them (enterTask, execute, leaveTask; finish the ones with no frame left)
            if libuv operations are pending: poll the event loop without blocking
            continue
        if libuv operations are pending: flush stdout, block in the event loop until one finishes; continue
        if some task isn't done: runtime error "all tasks are waiting"
        stop
    report failed tasks nobody awaited
```

- The ready queue is first in, first out. `spawn` adds the new task at the back, so it runs after the tasks already waiting.
- Ready tasks run in batches: the tasks ready when the batch starts. Polling the event loop between batches lets timers fire and fs jobs finish even while some task keeps giving way with `sleep(0)`, which would otherwise keep the queue full forever.
- "Pending" counts parked tasks waiting on libuv (`EventLoop.pending_operations`: one per timer, fs job, compression job, stream read, write, flush or handshake wait, open request, accept wait, connection slot wait, or trust store load), never open handles.
- Before blocking, the loop flushes stdout (`Stdio.flushStdout`), so output printed before a long wait shows up even when stdout is a pipe.
- `enterTask` puts the task's stack and frames in `VM.stack`, `VM.frames` and `VM.frame_count`; `leaveTask` saves them back, since a call may have grown them. The error path saves them too.
- `finish` copies `stack[0]` into `result`, frees the stack and frames, marks the task done, and wakes every waiter with the result.

### 11.3 Parking and waking

A task waits by **parking inside a native call**. The native records who should wake it up, sets the task's status to `parked` and returns `NativeResult{ .is_parked = true }`. The `CALL` handler then returns from `execute`. The top frame already saved `cur_inst` just after the `CALL` (on the `JUMP` for a call that can fail), exactly as a caller frame does while it waits for a callee.

To wake a task, `Scheduler.wake(task, result)` stores a `WakeResult` in `task.wake_result` and puts the task at the back of the ready queue. When the task is entered again, `enterTask` calls `VM.finishParkedCall`, which writes the value into the register of the `CALL` it parked in (`instructions[cur_inst - 1].a`), and moves `cur_inst` past the `JUMP` if the value is an error. That is the rule of `RETURN_ERR`, so `try`, `catch` and `catch e then` work on `await` and fs calls with no new code.

```zig
WakeResult {                    // src/runtime/tasks/scheduler.zig
    value: Value, kind: ValueType,
    has_failed: bool,
    error_is_relative: bool,    // value is a variant of the parked native's own error set
    payload: none | ints: []i64 | bytes: []u8 | names: [][]u8,   // C memory, built into a Dusk value at wake
}
```

- **Why the value waits** instead of going straight into the register: other tasks run in between, and any of them can start a collection. The `CALL`'s target register isn't in its heap map (section 9.2), so a pointer written there early wouldn't be kept alive. A heap `value` (an `await` result) is a GC root while it waits.
- **Payloads.** A result produced off the VM's thread (an fs job) can't be a GC value yet, since only the VM thread allocates in the heap. It comes as a payload in C memory, and `finishParkedCall` builds the Dusk value from it (`buildWakeValue`: a string or buffer for `bytes` depending on `kind`, an `[]int` for `ints`, a `[]string` for `names`), then frees the C memory. Like `string::split`, it sizes everything first and calls `maybeCollectGarbage` once, so no collection runs between its allocations, and it writes the register only after allocating. A collection there is safe for the same reason as during a native call: the parked frame's `cur_inst` points after its `CALL`. An unconsumed payload is freed with its `TaskState`.
- **Error ids.** A native that fails right away returns a variant of its own error set, and the `CALL` handler adds the extern's `error_base`. A parked native's error arrives later through `wake`, so `error_is_relative` says which kind it is: fs jobs send their `FsError` variant (relative), and `finishParkedCall` adds the `error_base` of the native the `CALL` points to; `await` sends the awaited task's error id, which is already absolute.

| Native | What it does |
|---|---|
| `await(t)` | Marks `t` as awaited. If `t` is done, returns its result (or fails with its error id) without parking. If `t` is the running task, a runtime error. Otherwise adds the running task to `t.waiters` and parks. |
| `sleep(0)` | Delivers `void` to the running task right away (back of the queue) and parks. |
| `sleep(ms)` | Starts a libuv timer for the running task and parks; the timer's callback wakes it with `void`. |
| `std/fs` functions | Copy their paths (and the data to write) into an `FsJob`, start it on libuv's thread pool ([std-internals](std-internals.md) 2) and park; the job's callback wakes the task with the result or an `FsError` variant. |
| stream reads (stdin, sockets, files) | Return right away when the stream's buffer already holds the answer; otherwise park as the stream's waiter ([std-internals](std-internals.md) 3), woken by data, the end of input, a timeout or `close()`. |

`spawn` isn't a native: it is the `TASK_SPAWN R[a] R[b] K[c]` intrinsic, which creates the `TaskState` from the closure in `R[b]`, records `c` (the kind of `T`) as `result_kind`, queues it, and puts a new `Task` handle in `R[a]`.

### 11.4 The event loop

`EventLoop` (`src/runtime/tasks/event_loop.zig`), the fs jobs (`src/runtime/tasks/fs_job.zig`, [std-internals](std-internals.md) 2), the streams (`src/runtime/tasks/stream.zig`, [std-internals](std-internals.md) 3), TCP (`src/runtime/tasks/tcp.zig`, [std-internals](std-internals.md) 4) and the trust store (`src/runtime/tasks/trust_store.zig`, [std-internals](std-internals.md) 5) are the only code that calls libuv, apart from `uv_guess_handle` in the `~is_terminal_fd` native. mbedTLS is called only from `src/runtime/tasks/tls_glue.c` ([std-internals](std-internals.md) 5). `EventLoop` creates its `uv_loop_t` on first use (`ensureLoopStarted`: a `sleep` above 0, an fs call, a stream, a socket, a compression job, or a connection slot wait), so programs that use none of them don't start one. Each `sleep` allocates a request holding the timer handle, the scheduler and the task; the timer's callback only calls `Scheduler.wake` and closes the handle (the close callback frees the request). Dusk code never runs inside a libuv callback.

libuv's structs differ per OS, so Dusk mostly doesn't use their layout: `src/runtime/tasks/uv.zig` declares the functions it calls with opaque `Loop`, `Handle` and `Request` types, and their memory is allocated with the sizes libuv reports (`uv_loop_size`, `uv_handle_size`, `uv_req_size`). The few plain C structs whose fields Dusk reads or fills are mirrored as `extern struct`s: `Stat` (`uv_stat_t`, with `c_long` times so it also matches Windows), `Buffer` (`uv_buf_t`, whose field order differs on Windows), the first public fields of `uv_dirent_t` and `uv_dir_t`, and `SocketAddress`, 128 zeroed bytes big enough for any `sockaddr` (Dusk only reads the port, at the same offset for IPv4 and IPv6). `addrinfo` comes from Zig's `std.c`, which already knows each OS's layout. Every handle's `data` (`uv_handle_set_data`) points to a `HandleOwner` field (`kind`: `timer`, `stream`, `stream_timer`, `stream_handshake_timer`, `stream_write_timer`, `listener`, `listener_timer`, `open_timer` or `slot_timer`, the timeout of an HTTP request waiting for a connection slot) inside the struct that owns it, and `@fieldParentPtr` gets from the field back to the owner, so `onHandleClosed` and the exit cleanup know what they're freeing. `uv_update_time` runs before each timer starts, since the loop's clock only moves when the loop runs. At exit, `deinit` closes every handle still open, runs the loop until they are closed, and closes the loop.

### 11.5 Freeing tasks

A task that isn't done is never freed, even with no handle left (a task nobody keeps still runs to the end). A done task is freed by `Scheduler.freeUnreachableTasks` after a collection when no handle reached it (section 9.3). A failed task that nobody awaited is kept until the program ends, so `run` can report it.

## 12. Known issues and observations

- **Extra moves.** Codegen copies variables into temporaries before using them (`LOAD R[3] R[1]` before `I_LT R[2] R[3] R[4]`), and moves a call result through more than one register after a `try`. Reading variables in place in binary operations, or a small peephole pass, would remove many instructions.
- **Repeated `CAPTURE_GET`.** A lambda reloads the same capture each time it uses it.
- **Array relocation chains.** Each growth leaves the old copy until the next GC, and references that still point to an old copy walk the chain on every access.
- **Union reads allocate.** Each `u.variant` read of an active primitive variant allocates a new `NullBox`.
- **Struct printing.** `format.printValue` prints every struct field as an `int64`.
- **Task stacks aren't reused.** Each `spawn` allocates a new stack and frame array, and a finished task frees them. A free list of default-size stacks would make spawning cheaper.
