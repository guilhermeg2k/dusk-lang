# VM internals

How Dusk programs run: the compiled program format, values and heap objects, the register machine, calls and errors, and the garbage collector. It describes the code as of v0.16 (in development: std/io output and stdin streams, fs jobs on libuv's thread pool). The language itself is described in [language-ref.md](language-ref.md).

The source files involved:

| File | What it holds |
|---|---|
| `src/compiler/codegen.zig` | Code generation from IR (`BytecodeGen`) |
| `src/runtime/bytecode.zig` | The compiled program format (`Program`, `Function`, `Chunk`), the instruction format and opcodes |
| `src/runtime/vm.zig` | The interpreter (`VM.execute`), allocation helpers, `copy` |
| `src/runtime/gc.zig` | The garbage collector: `Heap` (the two arenas, allocation counters, `collect`) |
| `src/runtime/value.zig` | `Value`, `ValueType`, every heap object layout and the work on them that doesn't allocate (array items; map hashing, lookup, insert and removal, section 6.8) |
| `src/runtime/format.zig` | Formatting values (interpolation, `TO_STRING`) |
| `src/runtime/reflection_format.zig`, `reflection.zig`, `json_codec.zig` | Shared type descriptions, safe reflection construction and native JSON conversion (section 12) |
| `src/runtime/stdio.zig` | Writing to stdout and stderr (`Stdio.writeBytes`, flush rules) |
| `src/runtime/runtime_error.zig` | `RuntimeErrorReporter`, the runtime error messages |
| `src/runtime/builtins/prelude.zig` | Prelude builtins written in Zig |
| `src/runtime/builtins/std.zig`, `std/*.zig` | Standard library registry and each module's natives |
| `src/runtime/builtins/native.zig` | Shared native types (`Binding`, `NativeResult`, the `Intrinsic` table) and helpers |
| `src/runtime/builtins/prelude.dsk` | The builtins' signatures, compiled before the user's file |
| `src/runtime/tasks/scheduler.zig` | `Scheduler` (every task, the ready queue) and `TaskState` (a task's stack, frames and result) |
| `src/runtime/tasks/event_loop.zig` | `EventLoop`: the libuv loop, timers for `sleep`, queuing fs jobs, and immediate clock helpers |
| `src/runtime/tasks/fs_job.zig` | `FsJob`: one `std/fs` call, run with synchronous `uv_fs_*` calls on libuv's thread pool (section 11.5) |
| `src/runtime/tasks/stream.zig` | `Stream`: stdin or a TCP connection, with its C-side read buffer, parked reader, timer, libuv read callbacks and the write path (section 11.6) |
| `src/runtime/tasks/tcp.zig` | `OpenRequest` (name lookup, connect and listen attempts, timeout) and `Listener` (accepting connections) for `std/net` (section 11.7) |
| `src/runtime/tasks/uv.zig` | The few libuv functions Dusk calls, declared by hand |
| `src/runtime/tasks/tls_glue.c` | The only code that calls mbedTLS: owns its sessions, configs, certificates and keys, behind a few `dusk_tls_*` functions (section 11.8) |
| `src/runtime/tasks/tls.zig` | The `dusk_tls_*` functions and their `Status` codes, declared by hand |
| `src/runtime/tasks/tls_layer.zig` | `TlsLayer` (a stream's TLS state), `ServerCertificate`, client configs (section 11.8) |
| `src/runtime/tasks/trust_store.zig` | `TrustStore`: the system's root certificates, loaded once on libuv's thread pool (section 11.8) |

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
├── functions: []Function          externs, methods, functions/lambdas, $main and generated default callbacks
├── static_closures: []Closure     one per function, same index (see 3.2)
├── string_pool: StringPool        every string literal of the program
├── struct_descriptors: []StructDescriptor
├── union_descriptors: []UnionDescriptor
├── error_names: [][]const u8      error id -> name, for unhandled-error messages
├── reflection_types: []Descriptor shared concrete type graph
└── main_func_index
```

### Functions

A `Function` has a `uid`, a `name` and a `kind`:

- `.dusk`: a `Chunk`, meaning compiled bytecode.
- `.native`: a Zig function pointer plus its argument count (`NativeFn`).
- `.intrinsic`: a builtin that codegen expands inline (`len`, `append`, `copy`, ...). It is never called at runtime (section 7.3).

Indexes for externs, methods and ordinary functions are fixed before bodies are generated. Externs come first, in prelude order, then methods of user types, then free functions and lambdas. `$main` follows those functions; generated JSON default callbacks follow it when present. `main_func_index` identifies the entry function. Default callback indexes are reserved as codec plans are generated. So `CALL FN[i]` and `LOAD_FUNC FN[i]` can refer to a function whose body hasn't been generated yet.

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
| `map[K, V]` | pointer to `Map` |
| struct | pointer to `Struct` |
| union | pointer to `Union` |
| `?int`, `?float`, `?bool`, `?byte`, `?Enum` | pointer to `NullBox`, or `0` |
| `?string`, `?[]T`, `?Struct`, ... | the pointer itself, or `0` |
| `func(...) -> T` | pointer to `Closure` |
| `Task[T, E]` | pointer to `Task` |
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
    kind: HeapValueType,         // array, struct, string, buffer, union, null_box, closure, cell, task, map, map_storage
    gc_forward: ?*HeapValue,     // set during a GC: where this object was copied
    relocated: ?*HeapValue,      // set when an array grows: where it moved
}
```

The header is 24 bytes. Each object type embeds it as its `obj` field, and code goes from a header to the full object with `HeapValue.getParentPtr(T, ptr)`. Variable-size objects store their data right after the struct, in the same allocation (`getDataPtr()`).

| Object | Fields after the header | Data after the struct | Size |
|---|---|---|---|
| `Bytes` (string, buffer) | `len` | `len` bytes | 32 + len |
| `Array` | `kind`, `len`, `capacity` | `capacity` values | 48 + 8 × capacity |
| `Struct` | `descriptor_id`, `field_count`, `identity` | `field_count` values | 32 + 8 × fields |
| `Union` | `descriptor_id`, `active_tag` | one value (the payload) | 40 |
| `NullBox` | `kind`, `value` | none | 40 |
| `Closure` | `function`, `capture_count` | `capture_count` values | 40 + 8 × captures |
| `Cell` | `kind`, `value` | none | 40 |
| `Task` | `state` (a `*TaskState`, section 11) | none | 32 |
| `Map` | `key_type`, `value_type`, `next_seq`, `seed`, `storage` | none | 48 |
| `MapStorage` | `key_type`, `value_type`, `len`, `count`, `capacity`, `entry_index_mask` | `capacity` entries, then the entry indexes | 48 + 24 × capacity + 4 × entry index slots |

Notes:

- **Strings and buffers** share `Bytes` and differ only in `kind`. Strings are immutable. Buffers are changed in place with `BYTES_SET`.
- **Arrays** start with a capacity of at least 8 (`ARRAY_INIT` takes `max(len, 8)`, capped at 255 as an operand), and grow by 1.5× (`calcNewCapacity`). Growing allocates a new array and leaves `relocated` pointing to it in the old one (section 6.2).
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

Slow paths are `noinline` helpers: `makeClosure`, `newCell`, `newBuffer`, `deepCopy`, `toString`, `buildString`, `loadUnionVariant`, and `Heap.collect` in `gc.zig`. Error reporters are `noinline` with `@branchHint(.cold)`. That keeps `execute` small. Performance is very sensitive to the machine code layout of this loop, so even unrelated edits to it can move benchmarks noticeably. `execute` is declared `align(64)`: without it, code added anywhere before it in the binary (even in the compiler) could move its start off a 64-byte boundary and shift every jump target inside it, which once made `fib` 28% slower with an identical loop.

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

### 6.8 Maps

A `Map` header never moves except in a GC, so growing a map doesn't need `relocated`: only its `storage` pointer changes. Like arrays, the work on entries that doesn't allocate is a method of `Map` or `MapStorage` in `value.zig`; allocation (`newMap`, `ensureRoomForMapEntry`, boxing in `readMapEntry`) lives on the VM next to `growArray`, and the loop cursor (`MapCursor`) is in `vm.zig`. `MapStorage` holds the entries in insertion order, followed by an open-addressing index:

```zig
MapEntry = extern struct { key: Value, value: Value, hash: u32, seq: u32 }  // 24 bytes
```

- **Entry indexes** (`entryIndexes()`). A power-of-two array of `u32`, at least twice the capacity, so it is at most half full. A slot holds `entry position + 1`, or `0` when empty. A lookup hashes the key and probes linearly from `hash & entry_index_mask`.
- **Hashing** (`Map.hashKey`): ints, bytes and enums hash their `i64`; bools only `value.bool` (the other 7 bytes of a bool `Value` are undefined); strings `Wyhash` over their bytes, seeded with the map's seed; structs their `identity`. The result is xored with the map's seed, goes through `std.hash.int` and keeps 31 bits.
- **Seeds.** Each map has its own random `seed` (from `Map.seed_generator`, a SplitMix64 that `Dusk.runProgram` starts from `io.random` through `Map.randomizeSeeds`), so colliding keys can't be computed ahead of time, and what collides in one map says nothing about another. A map gets a new seed when it becomes empty (`clear`, or `remove` of the last live entry): no live entry needs rehashing then, and tombstones never match. Growth keeps the seed, so it reuses the stored hashes. `copy` keeps the seed, since the copied index was built with it.
- **Equality.** Ints by value, strings by length and bytes, structs by pointer. The GC updates both the stored key and the key being looked up, so pointer equality survives collections.
- **Struct identity.** The copying GC moves structs, so a struct key can't hash its address. `Struct.identity` (a `u32` in the header padding, so structs didn't grow) is `0` until the struct is first hashed, then takes the next value of a global counter (`Struct.ensureIdentity`). GC clones keep it (byte copies). `copy` resets it on the copies. Wrapping the counter only makes buckets collide.
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

The two raw `std/time` clock natives return unboxed integers without parking. Static `EventLoop` clock helpers call `uv_gettimeofday` for Unix-epoch milliseconds and `uv_hrtime` for monotonic nanoseconds; they do not initialize a loop or allocate GC objects. Wall-clock conversion uses a wide intermediate and floors fractional milliseconds. Both conversions check the final signed integer range. Platform failures and unrepresentable readings go through `RuntimeErrorReporter`. The cached `uv_now` reading is not used.

`time::Timestamp` is an ordinary Dusk struct with a private millisecond field. Its Dusk factories wrap `now_ms` or an explicit integer; methods read the field or call the file-only `format_timestamp` native. Formatting uses floor division, nonnegative day remainders and Gregorian 400-year cycles, supporting the full signed millisecond range. A fixed 32-byte local buffer holds UTC text with a signed expanded year where necessary. The native finishes formatting before allocating one result string, so no moving heap references survive the allocation.

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

This is why only `CALL` and `CALL_CLOSURE` can fail (`OpCode.canFail`), and codegen asserts that a `JUMP` always follows one. Error ids are global: builtin error sets use 1 to 8, then user errors follow in declaration order. `Program.error_names` maps an id back to its name.

A full example, from `let x = twice(3) catch 0` in `main`:

```
0000 LOAD_CONST  R[2] C[0]     ; 3
0001 CALL        R[1] FN[43]   ; twice(3)
0002 JUMP        I[4]          ; ok -> 4
0003 LOAD_CONST  R[1] C[1]     ; failed: fallback 0
0004 LOAD        R[0] R[1]     ; x
```

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

`Heap.collect` builds a `Collector` for one collection (the heap, the program and the new arena). The running task's stack and frames are already saved into its `TaskState`. It visits every task that isn't done (`copyTaskRoots`): its `stack[0]` (the entry closure), its `wake_result` if one is waiting (section 11.3), and then, for every frame of that task:

```zig
pc = frame.cur_inst - 1                // the instruction this frame is stopped at
bitmap = binarySearch(chunk.heap_maps, pc) or 0
owned = top frame ? num_registers : next_frame.stack_offset - base
for each register i < owned with bit i set: copy stack[base + i]
```

- For caller frames, `cur_inst - 1` is their `CALL`. For the top frame of a parked task, it is the native `CALL` it is waiting in.
- A frame with `cur_inst == 0` hasn't started (a spawned task that never ran), so it has nothing to scan yet.
- For the running frame, it's the allocating instruction, because each such handler sets `current_frame.cur_inst = cur_instruction` before it can allocate.
- **A caller frame only scans `R[0] .. R[a]`**, up to and including its call register. Its argument registers are the callee's parameters (section 4.2), so the callee scans them. If both frames scanned them, the second pass would copy the already-copied object again, and the parameter would end up pointing to a different object than the caller's variable. The call register stays with the caller, so the closure of a `CALL_CLOSURE` is kept alive. Natives don't push frames, so the arguments of a native call are scanned as part of the top frame.

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
| `Task` | sets `is_reachable` on its `TaskState`; when the task is done and succeeded with a heap value, its `result` |
| `Map` | its `storage` |
| `MapStorage` | keys when `key_type` is a heap kind, values when `value_type` is, for the `len` used entries (removed ones hold `0`) |

Clones are byte copies with `gc_forward` and `relocated` cleared.

Then `Scheduler.freeUnreachableTasks` frees every done task that no `Task` handle reached, except failed tasks nobody awaited, which are kept for the report at the end. Afterwards, the new arena becomes active and the threshold is updated. If the old arena kept more capacity than the new threshold, it is reset with that limit and `gc_queue` is shrunk, so memory goes back to the OS after a peak.

## 10. `copy(x)`: deep copy

`DEEP_COPY` (`deepCopy`) clones a whole object graph:

- **Identity.** A map from original to copy ensures shared objects stay shared once, and cycles end.
- **Order.** A worklist processes objects in the same order as GC tracing.
- **What is shared instead of cloned.** Strings, null boxes, closures and cells are treated as immutable or shared by design, so copies point to the same object. Map keys are shared too: the copy clones the `Map` and its `MapStorage` and deep-copies only the values, so a struct key still finds its entry. Struct copies get `identity = 0`.
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
- "Pending" counts parked tasks waiting on libuv (`EventLoop.pending_operations`: one per timer or fs job), never open handles.
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
| `std/fs` functions | Copy their paths (and the data to write) into an `FsJob`, start it on libuv's thread pool (11.5) and park; the job's callback wakes the task with the result or an `FsError` variant. |
| stdin reads | Return right away when the stream's buffer already holds the answer; otherwise park as the stream's waiter (11.6), woken by data, the end of input, a timeout or `close()`. |

`spawn` isn't a native: it is the `TASK_SPAWN R[a] R[b] K[c]` intrinsic, which creates the `TaskState` from the closure in `R[b]`, records `c` (the kind of `T`) as `result_kind`, queues it, and puts a new `Task` handle in `R[a]`.

### 11.4 The event loop

`EventLoop` (`src/runtime/tasks/event_loop.zig`), the fs jobs (`src/runtime/tasks/fs_job.zig`, 11.5), the streams (`src/runtime/tasks/stream.zig`, 11.6) and TCP (`src/runtime/tasks/tcp.zig`, 11.7) are the only code that calls libuv. mbedTLS is called only from `src/runtime/tasks/tls_glue.c` (11.8). `EventLoop` creates its `uv_loop_t` on the first `sleep` with a time above 0 or the first fs call, so programs that use neither don't start one. Each `sleep` allocates a request holding the timer handle, the scheduler and the task; the timer's callback only calls `Scheduler.wake` and closes the handle (the close callback frees the request). Dusk code never runs inside a libuv callback.

libuv's structs differ per OS, so Dusk mostly doesn't use their layout: `src/runtime/tasks/uv.zig` declares the functions it calls with opaque `Loop`, `Handle` and `Request` types, and their memory is allocated with the sizes libuv reports (`uv_loop_size`, `uv_handle_size`, `uv_req_size`). The few plain C structs whose fields Dusk reads or fills are mirrored as `extern struct`s: `Stat` (`uv_stat_t`, with `c_long` times so it also matches Windows), `Buffer` (`uv_buf_t`, whose field order differs on Windows), the first public fields of `uv_dirent_t` and `uv_dir_t`, and `SocketAddress`, 128 zeroed bytes big enough for any `sockaddr` (Dusk only reads the port, at the same offset for IPv4 and IPv6). `addrinfo` comes from Zig's `std.c`, which already knows each OS's layout. Every handle's `data` (`uv_handle_set_data`) points to a `HandleOwner` field (`kind`: `timer`, `stream`, `stream_timer`, `listener`, `listener_timer` or `open_timer`) inside the struct that owns it, and `@fieldParentPtr` gets from the field back to the owner, so `onHandleClosed` and the exit cleanup know what they're freeing. `uv_update_time` runs before each timer starts, since the loop's clock only moves when the loop runs. At exit, `deinit` closes every handle still open, runs the loop until they are closed, and closes the loop.

### 11.5 fs jobs

Every `std/fs` call is one `FsJob` (`src/runtime/tasks/fs_job.zig`), run with `uv_queue_work` on libuv's thread pool:

1. The native copies its paths (NUL-terminated) and, for writes, the bytes out of the GC heap into the job, since the GC moves objects. `EventLoop.startFsJob` queues the job, counts it in `pending_operations`, and the native parks.
2. `onFsJobWork` runs on a pool thread. It calls `FsJob.runOnPoolThread`, which makes the blocking calls back to back as **synchronous** `uv_fs_*` calls (no callback), so libuv still handles the platform differences. `fs::read` is one job doing open, fstat (a size hint), read until EOF, close and the UTF-8 check. Recursive `make_dir` and `delete` loop inside the same job. One job means one round trip through the pool per Dusk call, where the usual chain of async libuv requests would take one per step. libuv's sync fs calls don't register with the loop, so they're safe from a pool thread.
3. `onFsJobDone` runs on the loop thread: it uncounts the job, turns its result into a `WakeResult` (`FsJob.takeWakeResult`, moving the C memory into the payload), calls `Scheduler.wake` and frees the job.

**Thread rule.** The pool thread only touches the job's own memory (paths, input, result fields), allocated with the C allocator. It never touches the VM, the GC heap or the scheduler.

**Errors** are mapped by name with `uv_err_name` (`ENOENT` to `NotFound`, `EACCES`/`EPERM` to `PermissionDenied`, `EEXIST`, `EISDIR`, `ENOTDIR`, `ENOTEMPTY`, anything else to `Other`), since libuv's error numbers differ per OS. The Zig enum `FsError` mirrors the error set in `std/fs.dsk` (a test checks names and order), and the wake sends its variant as a relative error id (11.3).

**Details.** Writes open with `WRONLY | CREAT | TRUNC` (or `APPEND`), taken from `std.posix.O` on POSIX and from the C runtime's `_O_*` values on Windows. `read_dir` uses `uv_fs_opendir`/`uv_fs_readdir` rather than `uv_fs_scandir`, which sorts the names. Recursive `delete` reads a folder's entries before deleting them, uses the entry type `readdir` reports (an `lstat` only when it's unknown), and never follows symlinks. `stat` runs `lstat` first and `stat` only for a link, so the other fields describe the link's target.

### 11.6 Streams (stdin and TCP connections)

`io::stdin()` calls `EventLoop.openStdin`, which opens a `Stream` (`src/runtime/tasks/stream.zig`) on the first call and keeps its index in `EventLoop.streams`. The Dusk value (`io::Stdin`) only holds that index in a file-only field, and the `io::Stdin` methods are Dusk code calling file-only natives with it. The fd is `VM.stdin_fd` (0 unless a test passes a pipe or a file). `uv_guess_handle` picks the kind:

| Kind | Opened as | Reads with |
|---|---|---|
| `tty` | `uv_tty_init` | `uv_read_start` |
| `pipe` | `uv_pipe_init` + `uv_pipe_open` | `uv_read_start` |
| `file` | the fd itself | 64 KiB `read_chunk` fs jobs (11.5), one at a time |
| `tcp` | `uv_tcp_init` (by connect or accept, 11.7) | `uv_read_start`, through the TLS layer when `tls_layer` is set (11.8) |
| `closed_input` | — | always at the end |

A `Stream` lives in C memory: the bytes read so far (a byte array and a read offset; consumed bytes are dropped by moving the rest down once the offset passes half), the end and failure flags, at most one `waiter` (the parked task and what it asked for), and an optional timer.

**One rule for every read.** `Stream.readyOutcome(mode)` decides whether a read (`bytes`, `exact`, `line`, `all_text`, `all_bytes`) can complete now, and returns the data (a slice of the buffer and how much to consume), the end, or an error. The native calls it first and, when it's ready, builds the string or buffer right there with no park. That's the common case when lines come from a pipe. Otherwise it parks the task as the waiter (or fails with `Busy` if there already is one), turns reading on, starts the timer for `timeout_ms`, and counts one pending operation. Before parking on stdin, it flushes stdout, so prompts show up.

**Callbacks.** libuv's `onAlloc` hands out free space at the end of the buffer, and `onRead` appends to it, or records the end (`UV_EOF`) or a failure. A finished `read_chunk` job does the same through `Stream.finishChunk`. Then `serveWaiter` runs `readyOutcome` for the waiter: if the read can complete, it copies the result into a payload (11.3), consumes it, stops the timer and wakes the task. If no task waits and 1 MiB or more is buffered, reading stops (`uv_read_stop`), so a fast writer fills the OS pipe and slows down instead of filling memory. The next read turns it back on.

**Timeouts and one wake per wait.** Each stream has one timer handle, made on its first timed read and reused. Data (`serveWaiter`), the timer (`onReadTimeout`) and `close()` all clear the same `waiter`: the first one wakes the task and stops the timer, and the others find no waiter and do nothing. Everything runs on the loop thread. A timed-out read consumes nothing.

**Writing (TCP).** `Stream.write(task, data)` first tries `uv_try_write` straight from the string's or buffer's bytes in the GC heap: the call is synchronous, so the GC can't move them meanwhile, and a small write finishes with no park and no copy. Whatever the socket didn't take is copied into a `WriteRequest` (C memory, with its own `uv_write_t`) and queued with `uv_write`; the task parks and `onWrite` wakes it when the system has taken everything (`ECANCELED`, from a close, becomes `Closed`). libuv's `uv_try_write` returns `EAGAIN` while any write is queued, so a later write never jumps ahead of an earlier one: writes from several tasks go out whole and in call order. `requests_in_flight` counts queued writes (and a pending connect, 11.7), and a request keeps its own allocator so it can free itself after the stream is gone.

**Closing and freeing.** `close()` wakes a waiter with `Closed`, stops reading, closes the stream handle and the timer, and clears the table slot. `EventLoop.stdin_handle` keeps the old index, so later `io::stdin()` reads fail with `Closed`. The `Stream` is freed only when it's closed, its open handles (`open_handles`: the stream handle and the timer) have all reported back to `onHandleClosed`, no chunk job is running (a job still on the pool writes into the stream when it finishes), and no connect or write request is in flight (libuv calls their callbacks with `ECANCELED` before the handle's close callback). At exit, `releaseAtExit` marks every stream closed and the loop's handle walk closes what's left. File streams close their fd only when it's above 2; libuv never closes stdio fds either.

**Missing stdio fds.** libuv asserts that it never closes fds 0 to 2. If the program starts with one of them closed, the loop's own fds would get those numbers, so `main.zig` opens `/dev/null` in their place first; closed stdin then reads as the end of the input.

**Writers.** `io::stdout()` and `io::stderr()` don't use libuv: their methods go through `Stdio` like `io::print`, but `Stdio.tryWriteBytes` / `tryFlush` return the error (`BrokenPipe` becomes `IoError.BrokenPipe`) instead of ending the program, and drop the bytes that couldn't be sent so the exit flush doesn't fail again.

### 11.7 TCP

`std/net` (`src/runtime/builtins/std/net.dsk` + `net.zig`) keeps the same shape as stdin: `net::TcpConn` and `net::TcpListener` hold an index in a file-only field, and their methods call file-only natives. A connection is a `tcp` `Stream` in `EventLoop.streams`, so it reuses the stream reads and writes (`net.zig` binds its `~stream_*` externs to the natives in `io.zig`). Listeners have their own table, `EventLoop.listeners`. Both tables are released at exit.

**Opening.** `tcp_connect` and `tcp_listen` both create an `OpenRequest`:

1. An IP address is parsed with `uv_ip4_addr` / `uv_ip6_addr`. Anything else is looked up with `uv_getaddrinfo` on libuv's thread pool, and the task parks.
2. `openNextAddress` tries each address in order. For a connect, it opens a new `tcp` stream for each attempt and calls `uv_tcp_connect` (a handle can't be reused after a failed connect); for a listen, it opens a new `Listener` and calls `uv_tcp_bind` + `uv_listen`. A failed attempt closes what it opened and moves on; when no address is left, the last failure is the result.
3. The result is an `OpenProgress`: `opened` or `failed` right away (an IP address to listen on, a bad address) returns from the native with no park; `waiting` parks the task, and the callbacks (`onLookup`, `onConnect`) finish it with `Scheduler.wake`. A connect stream goes into the table before connecting, so the exit cleanup can release it, but its index only reaches Dusk on success. On success (and on accept) the stream saves the peer's address (`uv_tcp_getpeername`), and the Dusk side copies it right away into `TcpConn`'s file-only `remote_host` / `remote_port` fields (`net::open_conn`), so `remote_address()` needs no native call and still works after the stream is freed.

`timeout_ms` adds a timer to the request. When it fires first, it cancels the lookup (`uv_cancel`, which only works while the job hasn't started) or closes the attempt's stream, and wakes the task with `TimedOut`. The callbacks that still arrive find the request finished, or the loop shutting down (`EventLoop.is_shutting_down`, set first in `deinit`), and only clean up: the request is freed once the lookup, the attempt and the timer's close have all reported back (`destroyIfDone`).

**Accepting.** `onConnection` counts a pending connection and serves the waiter, if there is one. `Listener.takeReadyConnection` is the one rule for `accept`, used by the native (no park when a connection is already pending) and by `serveWaiter`: it opens a `tcp` stream, `uv_accept`s into it and returns the index. libuv accepts one connection itself and stops watching the socket until `uv_accept` takes it, so connections that nobody accepts wait in the system's queue (`backlog`). One task waits at a time (`Busy` otherwise); the listener's timer (reused, like a stream's) and `close()` clear the same waiter, so only one of them wakes it.

**Errors** are mapped by name: connect, bind and lookup failures to `NetError` (`ECONNREFUSED`, `EADDRINUSE`, the `EAI_*` lookup errors to `HostNotFound`...), stream failures to `IoError` (`EPIPE` to `BrokenPipe`, `ECONNRESET`, `ECANCELED` to `Closed`). SIGPIPE is ignored, so a write to a closed socket returns `EPIPE` instead of ending the process. `accept` fails with `IoError` while `tcp_connect` fails with `NetError`: each extern's `error_base` comes from the set in its own signature, and the wakes send relative ids (11.3).

### 11.8 TLS

**mbedTLS through a C glue file.** mbedTLS 4.1.1 (the long-term-support line) comes from the `duskilang/mbedtls` fork through `build.zig.zon`, like libuv (`duskilang/libuv`): its releases are `.tar.bz2` files Zig's package fetcher can't read and its git tags lack the generated sources, so the fork's `dusk/v4.1.1` branch holds the release contents unchanged plus a `build.zig`, which compiles it with the default configuration (`DUSK.md` there has the upgrade steps). Its structs have sizes and layouts only C knows (they depend on the configuration), and its constants are macros, so Zig never touches them: `tls_glue.c` owns every mbedTLS object (`mbedtls_ssl_context`, `mbedtls_ssl_config`, certificates, keys, the session-ticket key) and exposes about 20 functions with opaque pointers and its own `DUSK_TLS_*` status codes (done, want more, failed, untrusted, expired, wrong host, out of memory, invalid, end). `tls.zig` declares them by hand, like `uv.zig`. `psa_crypto_init` runs once, on the first TLS call. mbedTLS isn't built thread-safe, so it's only called from the loop thread.

**Memory I/O.** Each session has two byte queues in the glue, given to `mbedtls_ssl_set_bio`: encrypted bytes from the socket go into `input` (`dusk_tls_feed`), and mbedTLS's output collects in `output`, which Zig sends and then consumes. mbedTLS never touches the socket. When mbedTLS returns "want read" while `input` still holds bytes, it means "call again" (it does that after a TLS 1.3 post-handshake message such as a session ticket), so `dusk_tls_read` and `dusk_tls_handshake` call again while input is queued, up to 8 calls in a row without progress.

**The stream side.** A TLS stream is a `tcp` stream with `tls_layer` set (`TlsLayer`: the session, client or server role, handshake state `pending` / `running` / `done` / `failed`, the `NetError` it failed with, and the tasks waiting on it). `onAlloc` hands libuv the layer's 64 KiB `cipher_scratch`, and `onRead` feeds it to the session (`receiveCiphertext`), then either steps the handshake or decrypts everything it can into the plaintext `buffered` (`decryptAvailable`), after which the usual `serveWaiter` / `readyOutcome` rules from 11.6 run unchanged. A peer's `close_notify` is the end of input. A write encrypts with `dusk_tls_write` and sends the output through `sendBytes`, the same `uv_try_write`-then-`WriteRequest` path plain writes use, so the ordering and backpressure rules hold. Handshake records and alerts go through `sendBytes` with no task: their `WriteRequest` wakes nobody and records a failure instead.

**The handshake.** A client handshake starts in `start_tls` (`Stream.startClientTls`, which `tls_connect` calls after connecting); a server stream from a TLS listener starts it on its first read (`io.readStream`), write or `handshake()` (`ensureHandshakeStarted`). Each arrival of data steps it (`stepHandshake`), sending what mbedTLS produced. Tasks wait in `handshake_waiters`: `start_tls` and `handshake()` callers (woken with nothing, or the mapped `NetError`), and writes made before it finished (their bytes are kept and sent once it's done, or they fail with `IoError.HandshakeFailed`). `finishHandshake` wakes them all; a failed handshake also sets the stream's failure, so reads fail with `HandshakeFailed`. Starting never wakes anyone synchronously: a caller is added as a waiter only after the first step. Verification failures come from `mbedtls_ssl_get_verify_result` flags, checked expired, then wrong host, then untrusted. A second timer per stream (`HandleOwner` kind `stream_handshake_timer`, separate from the read timer) implements `timeout_ms`: when it fires first, the handshake fails with `TimedOut`. `close` wakes the waiters, then sends `close_notify` with a best-effort `uv_try_write`.

**Configs and certificates.** `net::certificate` builds a `ServerCertificate` (the parsed chain and key, checked to match, plus one server config with a session-ticket key), kept in `EventLoop.certificates` until exit and shared by every listener given it (`Listener.tls_certificate`, which turns accepted streams into TLS servers). A client gets its own config when it passes `ca` or `verify = false`, owned by its `TlsLayer`; otherwise it shares the trust store's.

**The trust store.** The first verifying `start_tls` without `ca` parks in `TrustStore.waiters` and starts a `LoadJob` on libuv's thread pool. The job makes its own `std.Io.Threaded` and fills a Zig `std.crypto.Certificate.Bundle` from `SSL_CERT_FILE` / `SSL_CERT_DIR` when set, else from the system (`Bundle.rescan`: the distribution's bundle paths on Linux, the system keychains on macOS, the ROOT store on Windows); it touches only its own memory and calls no mbedTLS. Back on the loop thread, `finishLoading` adds each DER certificate to one shared client config and continues every waiter with `startClientTls`; the task stays parked and moves to the handshake waiters. At exit, a job still running finishes and is freed without waking anyone.

### 11.9 Freeing tasks

A task that isn't done is never freed, even with no handle left (a task nobody keeps still runs to the end). A done task is freed by `Scheduler.freeUnreachableTasks` after a collection when no handle reached it (section 9.3). A failed task that nobody awaited is kept until the program ends, so `run` can report it.

## 12. JSON codecs

`Program.reflection_types` owns the shared runtime-only concrete type graph (`reflection_format.zig`). Reflection inspection, public builders and both JSON backends use the same canonical descriptors and field-default callbacks. Indices are reserved before visiting children so recursive types terminate. There is no separate JSON plan format or table. Names and descriptors outlive the compilation arena; runtime code imports no compiler definitions.

The native JSON module supplies a general `Binding.reflection` operation, native function pointer and literal value requirements. Its policy allows JSON-compatible kinds and requires string map keys; the general checker rejects other concrete graphs. Codegen emits ordinary native `CALL`s with a type index, and for decoding a result-wrapper descriptor. Generic arguments already retained on IR calls identify the type directly. No JSON-specific binding tag, sema checker, codec emitter, opcode or dispatch path remains. Error ids still use the extern's ordinary `error_base`.

The native engine in `json_codec.zig` consumes scanner tokens directly into Dusk values and drives the JSON writer directly from values. Decode owns a stable copy of the input and a temporary arena for scanner strings, duplicate-key sets, paths and pending defaults. Conversion uses bounded recursion, with `max_depth` limited to 1..512. Encoding tracks active ancestors, so sharing is permitted and cycles fail. It allocates the final Dusk string after writing finishes.

**Native roots.** `Heap.temporary_roots` contains typed values held outside the VM stack. `protectValue` returns an index; native code keeps indices across list growth and reacquires object pointers after allocating. Collection copies these roots before scanning tasks. Every operation restores its saved root count on success and failure. JSON decode keeps the partially built values rooted through parser completion, callback construction and diagnostic writes. Object slots are initialized before collection can inspect them. Temporary native storage is released on every exit.

**Defaults.** Field defaults can call arbitrary Dusk functions. Codegen emits a zero-argument closure function for each defaulted field, capturing the destination struct and performing the ordinary typed field assignment. Native decoding records only missing fields, then returns a private `Decoded[T]` containing the value and an array of those closures. The public Dusk `decode` wrapper invokes them through ordinary closure calls before returning the value. Defaults therefore support allocation, nested decode calls and task parking without re-entering `VM.execute` from a native. Invalid documents never execute defaults. Each decoded child object queues its defaults before its containing object; fields within an object use declaration order.

Diagnostics are caller-owned Dusk structs. Paths and optional position boxes are written through refreshed roots; no scanner pointers or global last-error state escape. Path tracking is enabled only when a diagnostic is supplied; line and column calculation occurs on failure. Successful calls clear supplied diagnostics.

### Type reflection

`Program.reflection_types` separately owns format-neutral descriptors emitted on demand by `getReflectionType`. Each concrete type gets a canonical index reserved before walking child references. Descriptors contain a kind, exact type name, storage kind, public field slots/default-presence flags, collection child references and ordered enum/union variants. Private fields are omitted and flagged on the containing descriptor. The runtime has no compiler imports.

`Binding.reflection` carries a native function and reflection operation. Sema retains resolved generic arguments on IR calls, so zero-argument `reflect::type_of[T]()` has its concrete target available to codegen. Calls pass the type index and result-wrapper descriptor as hidden scalar arguments through an ordinary `CALL`. Type handles are ordinary one-field structs with a file-only index; native child queries reuse their wrapper layout. Public description functions build independent metadata snapshots in Dusk, leaving program-owned descriptors immutable. Reflection does not change parser syntax or interpreter dispatch.

`ReflectedValue` is a traced native heap object with a struct-compatible header and one private type-index slot. Its hidden payload stores the real value and storage kind plus an optional construction-session reference. Collection follows the payload only for heap storage kinds, and always traces the session. Layout assertions cover the public header boundary. Snapshot capture and typed casts use `VM.copyValue`, whose scoped temporary root survives collection. Reflected views share immutable snapshot storage; `copy` preserves their handles.

Builder/session state uses ordinary protected structs tagged `reflection_state`. The collector clones and traces them using their normal struct descriptors, while explicit deep copies preserve their shared identity. Aliases therefore observe the same one-shot builder state and session lifecycle. No external native resources or untraced addresses are stored in these handles. Native construction follows relocated array aliases, roots all partial results and refreshes pointers after allocation.

The public construction operations and session lifecycle are implemented in Dusk. Native `construct` allocates typed structs, arrays, maps, enum values, unions and nullable boxes from checked reflected inputs, validating descriptor identities again. Missing fields queue captured default closures on the session. `complete_session` changes the session from open to completing, invokes callbacks through ordinary Dusk calls, then marks it complete and clears callbacks. Pending values can be attached only within their session and refuse reads/casts until completion, including while a default is parked. Invalid construction marks the session failed. The compiler's `field_defaults` and deduplicated `$field-default` functions are shared with the existing JSON codec.

### Reflection JSON comparison backend

The `json-backend` build option chooses the standard JSON module source and bindings. `native` remains the default. `reflection` selects `json_reflected.dsk`, whose typed decoder, encoder traversal, duplicate/unknown-field policies, diagnostics, depth checks and cycle detection use public `std/reflect` facilities. Both configurations use the shared reflection graph; neither needs a JSON-specific compiler path. The streaming native codec remains the default for performance, and the Dusk conversion candidate is available for controlled comparisons.

The lexical adapter in `std/json_reflected.zig` retains the standard scanner and writer. Scanning produces a private flat array of decoded-string/number token records plus byte offsets. A lexical failure becomes an invalid token at its detection offset, preserving earlier semantic-error precedence when the Dusk decoder visits the prefix. Scanner memory and input copies are native temporary allocations; returned token objects/strings are ordinary rooted heap values. No typed destination struct is allocated by the scanner. A numeric-token helper preserves exponent parsing and rounding without changing `std/string` conversion policy. Scalar formatting uses the same writer behavior as the native codec.

`Binding.reflection.result_type` supplies a format-neutral result descriptor graph to native factories, so the token adapter needs no JSON-specific compiler rules. `require_value[T]` validates literal kind/key policies in sema and is erased before its argument expressions are emitted. JSON uses it to preserve compile-time rejection of unsupported targets. The reflection backend's `encode` is an ordinary generic Dusk function and can also be used as a function value with its full signature.

## 13. Known issues and observations

### 13.1 Fixed: the GC copied call arguments twice

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
    io::println("$b")
end
```

This segfaulted in `copyHeapValue`. Now each caller frame scans only up to its call register, so every slot has one owner and is copied once.

### 13.2 Observations

- **A missing heap map is silent.** If an instruction has no heap map, its frame gets bitmap `0` and its roots aren't kept. A debug assertion in `Collector.copyTaskRoots` when `binarySearch` finds nothing would catch codegen mistakes right away.
- **Extra moves.** Codegen copies variables into temporaries before using them (`LOAD R[3] R[1]` before `I_LT R[2] R[3] R[4]`), and moves a call result through more than one register after a `try`. Reading variables in place in binary operations, or a small peephole pass, would remove many instructions.
- **Repeated `CAPTURE_GET`.** A lambda reloads the same capture each time it uses it.
- **The header is 24 bytes.** Only arrays use `relocated`, but every object pays for it. Moving it into `Array`, or reusing `gc_forward` for it, would make every object 8 bytes smaller.
- **Array relocation chains.** Each growth leaves the old copy until the next GC, and references that still point to an old copy walk the chain on every access.
- **`copy` uses the page allocator.** `deepCopy`'s map and worklist use `VM.allocator`, which is `std.heap.page_allocator`. So each `copy` of a non-trivial value makes page-sized system allocations, which is costly in a loop.
- **Union reads allocate.** Each `u.variant` read of an active primitive variant allocates a new `NullBox`.
- **Struct printing.** `format.printValue` prints every struct field as an `int64`.
- **Task stacks aren't reused.** Each `spawn` allocates a new stack and frame array, and a finished task frees them. A free list of default-size stacks would make spawning cheaper.
- **`STORE_VAR` is dead.** It is still in `OpCode` and the disassembler, but nothing emits it and the VM has no handler.
