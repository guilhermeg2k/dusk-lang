# In Progress

## v0.17: std/os planning

- Branch `v017std`, worktree `dusk-lang-codex`, based on `v017` at `1225e8e`.
- Initial proposal: `.claude/v0.17-os-plan.md`; scope and API awaiting discussion.
- Platform identification added to scope by user request (2026-11-17); proposed `platform() -> Platform` enum, representation pending discussion.
- Next: agree arguments/environment/cwd, exit policy, and whether signals and process-global mutations belong in the first slice. Implementation has not started.

## v0.16 - Shigeo: std/fs slice A

Plan: `.claude/v0.16-fs-plan.md` (2026-11-15).

- [x] 1: groundwork + `exists` + `stat`. `uv.zig`: `Request`, `RequestType`, `Stat`/`Timespec` (extern structs, `c_long`), `uv_req_size/get_data/set_data`, `uv_err_name`, `uv_queue_work`, `uv_fs_stat/lstat/get_statbuf/req_cleanup`. New `tasks/fs_job.zig`: `FsJob` (C-owned path + work/fs request memory, `runOnPoolThread` with sync `uv_fs_*`, `takeWakeResult`), `FsError` (mirrors fs.dsk; std/fs.zig re-exports it for `error_sets`), errno names mapped with `uv_err_name`. `EventLoop.startFsJob` + `onFsJobWork`/`onFsJobDone`. `WakeResult`: `error_is_relative`, `payload` (`WakePayload` = none | ints), `isHeapValue` false with a payload; `TaskState.destroy` frees an unconsumed payload. `VM.finishParkedCall` is a method: `buildWakeValue` (`allocIntArray`), adds the native's `error_base` for relative errors; `enterTask` can fail. `std/fs.dsk`: `FsError`, `FileInfo` (7 fields), `exists`, `~stat_numbers` + Dusk `stat` (lstat, then stat for a link; folder size 0; `created_ms` null when 0). Request header for handles skipped: only timers are handles until streams (work requests aren't walked or closed). Tests: `fs-stat.dsk`, `fs-tasks.dsk` (counter task runs while parked, fs + await error ids, 3000 stats with GC), Zig tmpDir symlink/permissions/mtime test, error-set mirror, `fs-exists-not-string.dsk`. Fixtures `test/fixtures/fs/`. 487/487 Debug + ReleaseFast, cross builds OK.
- [x] 2: reading. `uv.Buffer` (field order differs on Windows), `uv_fs_open/read/close/fstat`. `FsJob` kinds `read_text`/`read_bytes`: open read-only, fstat as size hint (min 4096, +1 so EOF comes in one read), read until EOF (doubling), realloc to size, close; UTF-8 check on the pool thread. `WakePayload.bytes` -> `VM.allocBytesValue` (string or buffer by the wake kind). Fixtures `unicode.txt`, `empty.txt`, `invalid-utf8.bin`; `fs-read.dsk` (contents, errors, buffer copy independence, 2000 reads with GC); Zig test: 3 MB file + `/proc/self/stat` (size 0) on Linux. 490/490 Debug + ReleaseFast, committed alone (user's array.zig work in progress broke the working-tree build; staged tree verified in a copy).
- [ ] 3: writing (`write_text`, `write_bytes`, `append_text`)
- [ ] 4: folders (`read_dir`, `make_dir`, `remove`, `rename`)
- [ ] 5: docs + changelog

## v0.15 - Enjin': modules and std

Plan: `.claude/v0.15-modules-plan.md` (decisions in backlog "Modules design" / "Std prerequisites"). Started 2026-11-11 on `dev`.

- [x] A1: syntax (lexer `import`/`as`/`::`/`~`, `ast.ImportStmt`, `module_member` expressions, `module` on type annotations and named error sets, `is_file_only` flags; imports first in the file; sema rejects imports and `::` with `ModulesNotSupported` until A2/A3). Notes for A3: every sema reader of a type annotation's `.@"struct"` name must handle `annotation.module` (`traitIndexOf` and `namesSlot` already skip module names; audit `mentionsSlot` and friends). `~` is parsed and stored (`is_file_only` on FuncStmt, StructStmt, EnumStmt, UnionStmt, ErrorSetStmt, TraitStmt, LetStmt, Field) but not checked until A4.
- [x] A2: loader (`compiler/modules.zig`): `ModuleLoader.load(entry_path, src)` returns modules dependencies-first (entry last), each parsed once (keyed by the import path), `Import { stmt, module_index }`; `@/` paths read from the entry file's folder (`EntryFile.fromPath`, `Dusk.compileEntry`; `Dusk.compile(src)` uses `.` and entry path `""`); errors `ModuleNotFound`, `CantReadModule`, `LibrariesNotSupported`, `UnknownStdModule` (every `std/` import until B1), `ImportCycle` with the chain. Sema still gets only the entry module. Notes for A3: module sources are read with the compile arena (freed after compile), so if the bytecode keeps slices of them (function names), read them with `Dusk.allocator` instead; errors in a dependency print its line but not its file name (2.3 in the plan).
- [x] A3: sema across modules. The parser turns `m::x` (expressions, type annotations, error sets) into one identifier `"m::x"` (`ast.ModuleMember` and the `module` fields are gone); a module's root `SymbolTable` has a `ModuleScope` (imports, selected names) and `get` resolves `m::x` through it, so every existing name lookup handles modules. Per-module root tables under the prelude table; declarations clash with the prelude and selected names (`put` on a module root); `self.module` replaces the root walks; `enterModuleContext` (src, file path, prelude flag, imported names for errors) replaces `enterPreludeContext` and is used by generic function/type copies, traits and trait methods, constants, slot constraints (`slot_modules`, keyed by the constraint slice pointer), error edges and pending whens. Types from imported modules are named `shapes::Square` (`qualifyTypeName`). Errors: `ModuleNotImported`, `notDefinedInModule`, file path in messages from imported files; `ModulesNotSupported` removed. Also fixed: an unknown type in a `let` annotation exited silently.
- [x] A4: visibility (`~`). `Symbol.is_file_only` (set at hoist), qualified lookups and selected imports hide `~` names (`NotVisible`, via `ModuleScope.hidden_names` handed to the reporter), module roots look into the prelude with `getPublic` (so prelude `~` names are invisible and reusable); fields (`TypedIdentifier.is_file_only`) and methods (`file_only_methods`) checked on read, write, method call, field callee, construction (`checkStructBuildable` / `checkFieldArgument`, `CantBuildOutsideModule`) and trait fit, using `type_modules`; `checkPublicSignatures` (syntactic, after hoist) gives `PrivateTypeInPublicSignature`. Prelude `__x` became `~unchecked_x`; `InternalBuiltin` removed.
- [x] A5: runtime naming. Top-level functions and generic function copies of imported modules are named `util::double` / `util::first[int]` (`qualifyFunctionName`, externs left alone until B1), error names `util::ParseError.Empty`, types already qualified in A3. Codegen now dupes function and extern names (they pointed into the compile arena: `echo` of a generic copy crashed even in one file). Nothing else to qualify at runtime: structs/enums/unions print without type names.
- [x] B1: std registry. `builtins/native.zig` (Binding, NativeResult, Intrinsic, ok/fail/parked/stringResult/bytesArg), `natives.zig` renamed `prelude.zig`, `std.zig` registry (`modules`, `find`) with `std/task.dsk` + `std/task.zig` (`sleep` moved out of the prelude). Loader keys are display paths (`@/ui/button`, `std/task`); std modules carry their `bindings`; sema `registerExterns` resolves each extern's binding from its module (`ir.Extern.binding`, codegen no longer looks up by name), `ExternOutsideStd`/`ExternWithoutBinding`; `visitFuncStmt` skips externs. Per-module extern/binding test. Task tests use `import std/task` (gc-tasks uses the selective form).
- [x] B2: typed error ids. `native.fail(variant)` returns the enum position; `prelude.zig` has `ConversionError`/`StringError` enums + `error_sets` (internal Zig error set renamed `ParseFailure`); `ir.Extern.error_base` (first id of a declared set, `declaredErrorBase`), `bc.NativeFn.error_base`, VM adds it on failure. `*_id` constants and their test replaced by the enum-vs-set test. Wake errors stay absolute (no parking native has a declared set yet).
- [x] C1: `std/array` (`std/array.dsk` + `std/array.zig`): len, append, clear, index_of, contains, the `~unchecked_*` array externs, pop, pop_or_null, insert, remove, reverse, slice, `ArrayError`. 115 test/benchmark files got `import std/array: ...` from a script (selective names actually used, skipping names the file declares). Error ids after the prelude shifted (an interpolated `Fault.Bad` prints 7 now). README/doc examples still to migrate in C4.
- [x] C2: `std/string` (every `str_*` without the prefix, `from_bytes` was `string_from`, `StringError`) and `std/buffer` (`len`, `new`, `from_string` was `buffer_from`, `BufferError`), natives in `std/string.zig` / `std/buffer.zig`. Std modules may reuse prelude names (`string::to_int`; `SymbolTable.put` checks `bindings`), users still can't (`import std/string: to_int` clashes). Sema's string `when` uses a hidden prelude native `~strings_equal`. 90 test/benchmark files migrated by script (qualified `string::`/`buffer::`). Name list NOT reviewed with the user yet (they were away): flag it.
- [x] C3: `==`/`!=` on strings compare content (`areStringOperands` -> `strEqual` -> null-safe `~strings_equal(?string, ?string)`), `string::equal` removed (ignore-case comparison is gone with it: flag for the user), the string rule for `==` on slots dropped (`slotStringEquality`, `cantCompareStrings` removed). 36 files converted from `string::equal(a, b)` to `a == b` by script (operands with orelse/catch/|> parenthesized by hand).
- [x] C4: docs. language-ref §14 Modules (imports, `::`, selective imports, `~`, rules), §15 Prelude and Standard Library (prelude table, std/array, std/string, std/buffer, std/task, errors table), Errors/Known Limitations renumbered to 16/17, examples use `string::`/`task::sleep` with a note that std imports are left out; changelog v0.15 Features (modules, `~`, std move, string `==`), roadmap (maps moved into v0.15, per the user).

Open for the user (decided alone while they were away, 2026-11-11):
- C2 name list: `str_*` -> `string::*` without prefix, `buffer::len/new/from_string`, `string::from_bytes`; std modules may reuse prelude names (`string::to_int`, `string::to_float`).
- C3: `str_equal(..., ignore_case)` removed with no replacement.
- Prelude internals renamed `~unchecked_*` (was `__array_pop` etc.).
- Part D designed with the user 2026-11-11: `.claude/v0.15-maps-design.md` (user took all five proposals: `map` is a type name only where a type is written, not a keyword; `map::remove -> bool`; `?[` for arrays too; unboxed `if m[k]:` / `orelse`; maps coerce only between identical types; `copy` shares keys).

### Part D: maps
- [x] D1: type and literals. `map[K, V]` is parsed as a named type with two args (like `Task`) and resolved by name in `resolveTypeAnnotation` (`map_type_name`, no symbol, so `func map` / `let map` stay legal; declaring a type `map` is `ReservedTypeName`). `tt.MapType` + `getOrAddMap`, names `map[K, V]`. `checkMapKeyType` (int, byte, bool, string, enums, structs, slots; nullable and others `InvalidMapKey`) on annotations, literals and generic copies. `{k: v}` (`ast.MapLiteral`, trailing comma ok, `{` starts an expression) -> `evalMapLiteral` (expected type or first entry, int->float promotion, `DuplicateMapKey` on equal constants, `CantInferMapLiteralType` for `{}` without expected type, also inside `if` branches without one) -> `ir.map_init` -> `MAP_INIT` + `MAP_SET`. Runtime: `v.Map` header + `v.MapStorage` (entries + index, growth/compaction in `runtime/map.zig`), `Struct.identity`, GC tracing, `<map>`. `inferSlots`/`typeContainsSlot` see maps; maps coerce only when identical; `==` identity. Order change: D3 (indexing) before D2's tests, since GC/copy tests need reads.
- [x] D2: runtime object complete. Growth/compaction and GC tracing came with D1; now `deepCopy` copies the map header and storage and deep-copies values only (keys shared), struct copies get `identity = 0`; `map.removeEntry` (removed bit, key/value nulled, index slot kept) and `map.clear` are ready for D5. Tests: `gc-maps.dsk` (20k string keys, 5k struct keys, nested maps, maps in struct fields, closures and arrays; 6 collections run), `map-copy.dsk`. Compaction is exercised in D5 once `map::remove` exists.
- [x] D3: indexing. `indexInto` types `m[k]` as `?V` (key checked with `InvalidIndexing`), assignment expects `V` and `assignedFieldType` returns it; `?[` parsed for arrays and maps (`parseIndexAccess`), codegen `genNullableIndexedLoad` handles arrays (boxes primitive items) and maps. Opcodes `MAP_GET` (boxes a primitive V, like `UNION_LOAD`) and `MAP_FIND` (found bool in `R[a]`, raw value in `R[a+1]`). `if m[k]: v` and `m[k] orelse d` on a non-nullable map with a primitive, non-nullable V lower to `ir.map_find` (`lowerUnboxedMapRead`, binds two uids) and never box. Writing through `?.`/`?[` is now `CantAssignThroughOptionalChain` (it compiled and segfaulted on null before: changelog fix). Map literal keys don't count for the read-only store check (identity keys).
- [x] D4: loops. `visitForMapStmt` lowers `for m: value, key` to a loop over `ir.map_advance` (condition, moves the packed cursor variable: next position + seq of the visited entry) with `map_entry_value`/`map_entry_key` bound by `lowerCaptureLoopBody`; opcodes `MAP_NEXT`, `MAP_KEY_AT`, `MAP_VALUE_AT` (`runtime/map.zig` `advanceCursor`: if the visited entry moved, binary search the first larger seq). `mut value` like arrays. "can't be mut" messages now list maps. Removal during a loop is tested in D5 (needs `map::remove`).
- [x] D5: `std/map` (`std/map.dsk` + `std/map.zig`: len, has, remove -> bool, keys, values, clear as natives; `keys`/`values` size the array first then reread the map) + docs: language-ref "Maps" in §3, `?[` in Optional chaining, map loops in Loops, `std/map` in §15, copy/echo/comparison/mut captures mention maps, Known Limitations; changelog Features (maps headline after the std bullets, `?[` on arrays); vm-internals 6.8 + tables; AGENTS.md note. Tests: `map-functions.dsk` (removal + compaction during loops, churn of 20k inserts/removes). 474/474 Debug and ReleaseFast. Benchmarks not run (offer the user a `bench.py --compare` since `VM.run` got 7 handlers).

## v0.15 - Enjin: closures

Branch `closures` (started fd9a341, 2026-11-08). Plan: `.claude/v0.15-closures-plan.md`.

Decided 2026-11-08:
- Lambdas use declaration syntax without a name: `func(x: int) -> int ... end`, inline `func(x: int) -> return x * x`.
- Captures are by reference: the closure and the enclosing function share the variable; only variables that are captured and reassigned get boxed.
- Function types: `func(int, int) -> int`, `func(mut []int) -> void`, `func(string) -> ParseError!int`.
- In scope: named functions as values (`let f = add`; generic ones pick a copy from the expected type; defaults/named args don't go through a value), lambda param types from context (`func(x) -> return x * 2` when the expected type is known; generic slot filling must check lambda args after the others).
- `filter` completed in std/array with the other array additions (2026-11-15).
- Out: other higher-order builtins (map/reduce/sort...), methods as values, nested named functions.

- [x] Design questions (lambda recursion, exact matching, CantCompare, printing, loop vars, capture mutability, missing types, no function constants) accepted 2026-11-08
- [x] P1: function types and named functions as values
  - `func(...) -> T` types (`ast.FuncTypeAnnotation`, interned `tt.FuncType` so matching is id equality; error unions keyed by set + payload)
  - `ir.func_ref` / `ir.call_value`; calls through variables, struct fields (methods win over fields), array items and any other expression
  - Runtime: `v.Closure` heap object (function pointer + capture count); every function gets a static closure in `Program.static_closures` (`LOAD_FUNC`), skipped by the GC (`Closure.isStatic`: no captures); `CALL_VALUE` pushes a frame like `CALL`; `ValueType.function`
  - Generic functions as values from the expected function type (`evalGenericFuncRef`); slot inference, `mentionsSlot`, `typeContainsSlot` look inside function types
  - Builtins can't be values (`BuiltinAsValue`), so `CALL_VALUE` only handles Dusk functions
  - Tests: `function-values.dsk`, `gc-function-values.dsk`, 10 error cases
  - `FieldAndMethodSameName` (user, 2026-11-08): a struct field and method can't share a name, so `x.name()` has one meaning
- [x] P2: lambdas without captures
  - Parser: `func` in prefix position is `ast.Exp.lambda` (`FnDef`); resets `loop_depth`; `func` can start an expression
  - Sema `evalLambda`: hidden `$lambda` function (`fulfillFuncType` + `visitFnDef`), appended unless `is_checking_opaque` (generic body checks); value is a `func_ref`, so codegen/VM needed nothing new
  - Outer variables (uid below `lambda_uid_floor`) are a temporary `CaptureNotSupported` until P3
  - `LambdaCallsItself` via `lambda_binding` (the `let` name being assigned), `LambdaParamCantHaveDefault`
  - Fixed `fulfillFuncType` resetting the enclosing return type to `void` (only mattered once functions could be lowered inside other functions)
  - Known: a lambda inside an inline-return function is lowered twice (once to infer the return type); the extra copy is unused
- [x] P3: captures
  - Only captured `let mut` (`is_mut and !is_borrowed`) is boxed in a `v.Cell` (heap kind `cell`, typed value, traced by the GC); everything else is copied into the closure (same meaning, since it can't be reassigned)
  - Sema: `lambda_contexts` stack (floor uid + captures); `resolveCapture` chains through middle lambdas (`capture_raw`), parent locals are `identifier` / `var_box`; `boxed_uids` goes to codegen through `ir.Program`
  - IR: `make_closure`, `capture_ref`, `capture_raw`, `var_box`, `update_capture`; `ir.Func.captures` -> `Chunk.capture_kinds` (GC kinds per capture)
  - VM: `MAKE_CLOSURE`, `CAPTURE_GET` (closure is `stack[base - 1]`, the caller's `CALL_VALUE` register, kept alive by the caller's heap map), `CELL_NEW/GET/SET`; boxed registers use the internal `cell` primitive type so heap maps see them
  - Lambdas with no captures stay static (`func_ref`)
  - Fixed: `func(mut T)` in a generic copy filled with a primitive tripped the mut check (`namesSlot`)
  - Removed `CaptureNotSupported`; added `TooManyCaptures` (255)
  - Tests: `closures.dsk`, `gc-closures.dsk`
- [x] P4: lambda param types from context
  - `evalExpExpectingUnrecorded` passes the expected `func_type`'s params as hints to `evalLambda`; `typedLambdaDef` fills untyped params with `ast.TypeAnnotation.resolved` (a type id), checks count and `mut`
  - Generic calls and generic struct constructors put lambda args aside in the first slot-filling pass, then `inferSlotsFromLambda` types them from the bound slots (`lambdaHints` + `enterBoundSlotScope`) and fills the rest from the lambda's type; the expected type still comes last
  - New errors: `LambdaParamNeedsType` (replaces `InvalidParameterType` for lambdas), `WrongLambdaParamCount`, `LambdaParamMutDiffers`
  - Known: a slot only the expected type can fill doesn't reach a lambda's params (lambdas come before the expected type), so that lambda needs its param types
  - Tests: `lambda-param-inference.dsk`, 6 error cases
- [x] P5: docs and full tests
  - Language reference: version line v0.15 (in development); section 10 gets "Function values", "Function types", "Lambdas", "Closures" (no renumbering); "Scope", comparison, slot filling, `echo` and Known Limitations updated; every example run first
  - Changelog v0.15 bullets polished
  - ReleaseFast build ok; 442/442 tests in Debug and ReleaseFast
- [x] P6: lambda return types from the body (lambdas only; named functions keep naming their return type)
  - Decided 2026-11-08: block form without `->` (`func(n)` newline body `end`); every `return` must have the same type (no merging); no `return` is `void`; the error set is inferred too (`try` or returning an error makes the lambda failing)
  - When the expected function type knows the return type, the lambda takes it (as params do), inline form included (`LambdaHints.return_type_id`, from `functionTypeHints` or bound slots in `lambdaHints`)
  - Parser: `parseLambdaDef` (no `->` means a block body); named functions keep `parseFnDef`, so `func foo(x: int)` + body is still `expected arrow`
  - Sema: `typedLambdaDef` gives every lambda a resolved return annotation (`void` placeholder when inferred), so `fulfillFuncType`'s inline inference never runs for lambdas (no more double lowering of inline lambdas); `LambdaContext` holds `is_return_inferred`, `body_depth`, `return_payload`, `error_set`; `activeReturnInference` only matches at the lambda's own `fn_depth` (generic copies lowered mid-body keep their own return type); `visitInferredReturnStmt`, `evalTry` feed it; `visitFnDef` builds the final type and `evalLambda` writes it back to the function's metadata
  - Error set is a `createInferredErrorSet("$lambda")`, resolved by `resolveErrorEdges` like `-> !T`
  - New error `LambdaReturnsDiffer`; removed `parser/lambda-block-without-return-type.dsk` (now valid); `lambda-inferred-wrong-return.dsk` is now `InvalidReturnType` at the return expression
  - Follow-up (user, 2026-11-08): a failing lambda or function passed for `func(T) -> U` used to make `U` the error union and fail inside the generic copy; `inferSlots` now rejects error unions for a slot (`SlotCantFail`, next to the `void` check). `syntaxName` names the error set (`errorSetSyntaxName`), so copy names and messages show `ConversionError!int`. Error values (`e` from `catch e then`) still fill slots
  - Known: no way to pass a failing callback through a generic function (a slot can't stand for an error set, `func(T) -> E!U`)
  - Tests: `lambda-return-inference.dsk`, 7 error cases; 443/443 Debug and ReleaseFast
- [x] P7: error slots (built 2026-11-08 while the user was away)
  - Implementation: `ErrorSet.kind` enum (`declared`, `inferred`, `slot`, `none`) replaces `is_inferred`. `ast.Slot.is_error_set` (`parseSlot`). Opaque checks: `addSlotType` makes an `E: error` slot a placeholder error set (kind `slot`), so the existing error-set code (edges, `return e`, `ErrorNotInSet`) works unchanged; `when` on it is `CantMatchSlotErrors` (`resolveWhenSubject`). `none` is a built-in error set in the root scope (`declareNoneErrorSet`), so `Task[int, none]` resolves like any set. `errorUnionType` turns `none!U` into `U` (all 3 places that build error unions). `resolveFnErrorSet` accepts slot names (`SlotNotMarkedError` when the slot isn't `: error`). `inferSlots` `.function` binds the error slot from the arg's set or `none`; shared `bindSlot` (error slots bind exactly, no coercion) + `checkSlotFits` (`SlotNeedsErrorSet`, plain slot can't be `none`) serve functions, structs and unions. `mentionsSlot` / `areMentionedSlotsBound` / `typeContainsSlot` / `lambdaHints` see the error slot. `has_none_error_slot` (set in `enterCopyLowering`) lets `try`/`catch` take calls that can't fail in `none` copies (`evalFailingCall`); safe because the opaque check already proved those calls fail through `E`
  - Conflicts between two callbacks are `InvalidType` at the second argument (bound slots check params against the resolved type, as for normal slots)
  - A lambda without a return type passed where `E` is already bound takes `E!U` from the expected type (P6 rule), so it fits; a named function that can't fail doesn't
  - Tests: `error-slots.dsk`, 10 error cases; Debug and ReleaseFast pass except `gc-functions.dsk`, which has uncommitted outside edits (not touched)
  - **Error slots: failing callbacks through generic functions** (user, 2026-11-08; started the same day on `closures`): today a callback that can fail can't go through `f: func(T) -> U` (`SlotCantFail`), so `map(ids, func(n) -> return try db.get(n))` doesn't work. Decided:
    - Visible error slot only: `func map[T, U, E: error](items: []T, f: func(T) -> E!U) -> E![]U`. `E` is filled from the callback's type at the call. The hidden form (`f: func(T) -> !U`, errors worked out per copy) is left for later. If it comes, a function without slots can't use it (needs `[]` or a named slot, so `let g = run_twice` never fails on a slot nobody wrote).
    - `E` can be "none": `none!U` is `U`, so `map(texts, str_len)` needs no `try`, and `try map(texts, str_to_int)` fails with `ConversionError`. `try` on a call whose errors come from an error slot is allowed, and compiles to a plain call in the "none" copy.
    - A function with an error slot fails only with `E`: no `E + ArrayError`. Other failing calls in its body must be caught there.
    - Marked with a constraint: `[T, U, E: error]` (trait syntax + `error` keyword). `E!U` with a slot that isn't `: error` is an error (`'E' must be marked 'error' to come before '!'`), and filling an error slot with a non-error type too (`'E' must be an error set`). Decided 2026-11-08.
    - Inside the generic body, `catch e then` gives `e: E`: it can be returned (`return e`), passed to a function taking `E`, stored (`[]E`) and echoed. `when e` with cases is an error (`Can't match the cases of 'E': its errors depend on the call`). `f(x) catch 0` is allowed; in the `none` copy the fallback is dropped like `try` becomes a plain call.
    - "No errors" shows as `none` in copy names and messages (`map[string, int, none]`, `'E' is none: 'str_len' can't fail`); function types drop it (`func(string) -> int`). `none` confirmed by the user over `never` (2026-11-08).
    - Two callbacks giving `E` different sets is a conflict (`'E' can't be both 'ConversionError' and 'ArrayError'`), as for normal slots. A non-failing function doesn't fit `func() -> ConversionError!T` either (exact function type matching). Merging and that subtyping can come later without breaking anything.
    - Error slots work everywhere slots do: generic functions, structs and unions (`struct Task[T, E: error]` with `run: func() -> E!T`, methods doing `try self.run()`, `Task[int, none]` in type names). User asked for good code here: one shared error-slot path (binding, `none` collapse, `try`/`catch` on slot-derived calls) used by functions, structs and unions alike, not per-kind special cases.
    - Comparison of how other languages handle it, from the 2026-11-08 discussion: Rust/Go/Haskell treat errors as values (`Result`, `try_fold`, `traverse`); Swift had `rethrows` and moved to typed throws `throws(E)` with `Never` as "no errors" (the model chosen here); Zig works out error sets per generic copy (`anytype` + `!T`, `error{}` as empty, the later hidden form); Java's checked exceptions can't cross lambdas (the failure case); Koka makes `map` generic over effects.
- [x] Gap check after P7 (2026-11-08)
  - `let g = c.get` (method as value) exited with code 1 and no message (the `indexed` value got the method's `.function` type); now `MethodAsValue`. `constant-invalid-type.dsk` used `P.make` to reach `InvalidConstantType`: moved to `method-as-value-constant.dsk`, and the constant test now uses a union variant (`U.a`)
  - `orelse` fallback on `?func(...)` now types its lambda (`orelseFallbackContext` includes `.func_type`; `evalExpWithContext` treats lambdas as needing context)
  - Parser: `func(` in an `if`/`when` expression branch is a lambda, not a function declaration (`parseStmt`, `branch_depth > 0`)
  - Not changed: `if`/`when` branches don't share context with each other (same as `@(...)`), so `if c then f else func(x) -> ... end` needs an expected type
  - Found: `P.x` (instance field through the type) panics: backlog
- [x] `filter` in std/array (completed with array core additions, 2026-11-15)

## v0.14 - Simon: drop the sharing rule

Decided 2026-11-07: keep the first mutability model (`let` / `let mut`, mutability from the root of the access path, `mut` params, `mut self`, `mut` captures) and drop the sharing rule. The `mut`-in-the-type redesign was tried and abandoned (git stash "mut-in-type phase 2 (abandoned 2026-11-07)").

- [x] Remove the sharing rule (ec5df36)
- [x] Docs: language reference "Sharing values" section and known limitations, changelog
- [x] Light store check (1840abb): values read through non-`mut` names can't go into mutable places (`let mut`, assignment, `append`/`insert`, `mut` args), constructors/literals/branches count as their parts; skipped in prelude and generic copies (bare `T` isn't checked); calls count as new values (known hole)
- [x] Chaining: temporaries can be changed (TemporaryNotMutable removed), so `b.add(1).add(2)` works

## v0.14 - Simon: generics and traits

All phases done on branch `type-slots` (2026-11-06). The plan doc was deleted at the user's request; the design is in `doc/language-ref.md` section 11.

- [x] Phase 0: open v0.14
  - [x] Version `0.14.0-devel`, changelog section, plan updated for the released v0.13 (6daae43)
  - [x] Phase list written here
  - [x] Roadmap: "Meta structs" becomes "Traits"
  - [x] Language reference version line: v0.14 - Simon (in development)
- [x] Phase 1a: syntax and the slot type (generic functions compile and are checked, can't be called yet)
- [x] Phase 1b: copies and inference from arguments
- [x] Phase 1c: inference from the expected type
- [x] Phase 2: generic structs and unions
- [x] Phase 3: generic prelude signatures (`append[T]`, `copy[T]`; fixes `append` item check)
- [x] Phase 4: type lists (`[T: int | float]`, `trait Number = ...`; `+` moved to Phase 5)
- [x] Phase 5: traits (block of bodiless `func` signatures, `+` composition, exact matching with names ignored, no static trait methods, lists can't mix with traits)
- [x] Phase 6: docs (language reference section 11 "Generics and Traits", limitations, `append` signature), full tests and ReleaseFast build; plan doc deleted (user)

Decided 2026-11-06: `mut x: T` only with a trait constraint; `==`/`!=` on `T` only for type lists; first argument fills a slot and later ones convert to it; type lists check operators per member, reject `==` when the list has `string`, and `+` waits for Phase 5; sharing rules for a bare `T` are checked per copy, not at the definition (no `copy` needed in generic bodies; `[]T` etc. still checked at the definition).
