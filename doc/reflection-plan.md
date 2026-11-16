# Reflection, safe construction and JSON migration

Status: implementation started after approval, 2026-11-15. The first delivery adds JSON benchmark workloads and records the current implementation's baseline at `7aa6591`; see [results](../benchmarks/json-baseline.md). Reflection implementation and JSON migration follow as separate tasks. Read-only type inspection is implemented as documented in the language reference. Checked values, construction, capability validation and JSON migration below remain planned; their API names illustrate the design.

## Implementation progress

- Baseline recorded at `7aa6591`; benchmark sources remain unchanged.
- Read-only `std/reflect` implemented: protected `Type` handles; explicit/inferred concrete targets; identity, names and kinds; tagged descriptions; independent public-field, enum and union snapshots; recursive descriptor references. Resolved generic arguments are retained in IR calls, so zero-argument generic reflection needs no syntax change.
- Descriptor authority uses private-field wrappers and immutable runtime program tables. Public snapshots cannot modify those tables. Private fields are hidden regardless of the caller's module; structs with hidden fields expose a flag.
- Unresolved types in reflected anonymous fields are rejected in sema rather than reaching code generation.
- Checked values, safe builders/default sessions, the independent decoder, capability validation and JSON migration remain upcoming work. No JSON behavior or benchmark workload has changed.

## Goal

Allow an ordinary Dusk module to inspect a concrete type, read its values and construct a valid value from runtime input. A user should be able to implement a decoder for a new format without adding format-specific rules to the compiler.

Preserve the existing `std/json` API and behavior while moving its type-directed conversion onto those public facilities. Measure the cost against the existing native codec implementation before making the new implementation the default.

This is more than read-only metadata. The minimum complete feature has three parts:

1. Compiler-generated type descriptions.
2. Checked reflected values for working with types discovered at runtime.
3. Builders that publish fully initialized values and apply declared defaults.

General compile-time execution, compile-time loops, macros, user-defined compiler plugins and generic methods are outside this plan.

## Compilation model

Use the existing explicit generic arguments and generic specialization. A call such as `reflect::type_of[User]()` requests a descriptor for `User`; a specialized `decode[User]` requests the same descriptor. Ordinary function-call syntax is sufficient.

The compiler emits immutable, program-owned metadata only for requested types and their reachable members. Reserve descriptor identities before visiting children so recursive types terminate. Use stable descriptor references rather than pointers into growable compiler tables. Descriptor identity is exact concrete type identity, not merely a shared runtime layout.

Metadata is available at runtime. Branching on a reflected kind does not specialize ordinary control flow or allow otherwise invalid typed code. A runtime field descriptor cannot be supplied as a generic type argument. Checked reflected values bridge that gap.

No format names or parsing rules belong in this machinery. Existing JSON type validation must eventually become a use of general reflection capabilities, while preserving compile-time rejection for statically unsupported JSON targets.

## Proposed public surface

Use `std/reflect` so the feature does not reserve a new keyword or prelude function. Start with explicit module functions; methods with their own generic arguments are a separate language feature.

| Operation | Purpose |
| --- | --- |
| `reflect::type_of[T]()` | Obtain a descriptor without an existing value. |
| `reflect::type_of_value(value)` | Infer the concrete type from an expression. The expression follows normal evaluation rules. |
| `reflect::describe(type)` | Obtain a tagged description suitable for ordinary `when` inspection. |
| `reflect::value(value)` | Wrap an existing typed value in a checked reflected value for traversal. |
| `reflect::cast[T](value)` | Recover a typed value after checking exact descriptor identity. |
| `reflect::builder[T]()` | Start construction for a statically known target. |
| `reflect::builder_for(type)` | Start construction for a target discovered during traversal. |
| `reflect::set(mut builder, name, value)` | Convenience generic function accepting a typed field value. |
| `reflect::set_value(mut builder, field, value)` | Set a field using checked reflected values and field descriptors. |
| `reflect::finish(mut builder)` | Validate and finish construction, yielding a reflected value. |
| `reflect::finish_as[T](mut builder)` | Finish and recover a statically typed result. |

These are proposed responsibilities, not final signatures. Define an explicit payload-free reflection error set for type mismatch, unknown/foreign field, missing field, duplicate assignment, invalid builder state and invalid variant/index. Keep ordinary allocation failures in the runtime's existing failure path.

Illustrative generic construction:

```dusk
func make_user[T](name: string, age: int) -> reflect::ReflectError!T
    let mut builder = reflect::builder[T]()
    try reflect::set(builder, "name", name)
    try reflect::set(builder, "age", age)
    return try reflect::finish_as[T](builder)
end
```

The target is known in each specialization, but string field names and supplied values can still be runtime inputs. Field lookup, descriptor compatibility and builder completeness are checked. There are no implicit parsing conversions inside reflection.

## Type descriptions

A tagged `TypeInfo` description distinguishes primitives, nullable, array, map, struct, enum, union, buffer, function, task and error types. Inspection and constructibility are separate capabilities; not every inspectable type is constructible in the first version.

- Primitive descriptions identify their exact type.
- Nullable and array descriptions reference their child type.
- Maps reference both key and value types; JSON separately restricts keys to strings.
- Struct descriptions expose the type name, public instance fields in declaration order, each field's exact type and whether it has an explicit default. Static fields and methods are excluded initially.
- Enum descriptions expose names and numeric values in declaration order, including aliases.
- Union descriptions expose variant names and payload types in declaration order.
- Function, task, buffer and error descriptions initially provide identification and kind; broader manipulation is deferred.

Use protected descriptor and field handles as the authority for native operations. Writable metadata objects or guessed integer indices must never allow a program to forge a type, bypass visibility or access arbitrary memory. Exposed names and lists are descriptive snapshots or otherwise protected from mutating the internal tables.

Recursive child references are descriptor handles, not endlessly nested metadata values. Descriptor names are descriptive; identity comparisons use the canonical handle. Program lifetime owns metadata; no pointers into discarded compilation memory.

## Checked reflected values

A reflected `Value` carries an exact descriptor plus a representation that the GC can trace. It is a restricted dynamic value, not an unchecked cast or a public raw pointer. It must represent primitive, heap and nullable values correctly, including nullable primitive boxes and a present nullable value holding null.

Provide checked operations for:

- Primitive extraction and wrapping.
- Struct field reads by validated field descriptor.
- Array length and element reads.
- Map entry iteration preserving insertion order and distinguishing absence from a present null value.
- Enum name/value inspection and construction by a declared variant.
- Active union variant inspection and payload reads.
- Nullable presence and child access.

For example, a decoder reads a field's descriptor, recursively parses a reflected value of that descriptor, then calls `set_value`. It never needs to turn that runtime descriptor into a new generic type argument.

Mutability is a phase-0 gate. Reflected reads must not turn a read-only reference into a mutable alias, and builders must not create a new hole in ordinary store checks. Begin with owned copies at boundaries that would otherwise grant mutable access; read traversal itself should not copy the entire object on every field lookup. Distinguish owned builder results from borrowed read views. Finalize the exact borrowing/copy rules and their tests before implementation; do not assume an opaque wrapper preserves mutability automatically.

Native-backed representation must integrate with moving GC, copies, parked tasks and task completion. Prefer existing ordinary heap representations if they can enforce all invariants; otherwise add a dedicated traced object kind. Raw untraced integer handles to heap objects are unacceptable. Compiler-emitted hidden metadata arguments may specialize ordinary native calls without new format-specific opcodes.

## Safe construction

Implement a common builder for structs, arrays, maps, nullable values, enums and tagged unions. Collection operations have explicit append/insert/select responsibilities rather than pretending every target is a string-keyed struct.

- Validate exact target/field identity and supplied value type before storing. Numeric parsing and coercion belong to the decoder.
- Struct builders track presence independently from stored bits. Missing and explicitly null remain distinct.
- A required field must be supplied even when nullable. Only an explicit declared default permits omission.
- Reject duplicate field assignment in the initial API. A decoder separately tracks duplicate source keys, including unknown keys it ignores.
- Collections grow with GC-safe storage. Union builders require exactly one selected variant with a compatible payload. Enums require a declared value/variant.
- Unfinished storage is never returned as `T` or exposed through reflected reads. It cannot be used to create a cycle through an incomplete value.
- Builders are one-shot: all aliases observe consumed or failed state. Reject further writes or a second finish. If a default fails through a runtime error, no partial result is published.
- Abandoned builders and failed operations release temporary storage through normal ownership/GC. Completed values do not retain builder scratch state.

Defaults reuse compiler-generated typed callbacks and execute as ordinary Dusk functions. They may allocate, call nested codecs and park. Native functions must not reenter the interpreter. Preserve child-before-parent and declaration ordering, and execute defaults only after the full input document has passed validation.

For nested construction, provide a private construction session that validates the whole object graph and queues defaults for a single final completion. Public immediate finish remains useful for ordinary construction; a documented deferred-completion session is required for third-party decoders to reproduce JSON's no-default-side-effects-on-invalid-document behavior. Session-owned incomplete values can be attached only within their session and remain inaccessible until successful completion. This protocol must be public and format-neutral, not a JSON-only escape hatch.

Privacy: reflection must preserve file-only visibility, including through imported generic functions. Start by refusing reflective construction of structs containing private instance fields and refusing reads of private fields. Do not expose a constructor that can bypass an opaque standard-library type's invariants. Add visibility tests across modules and generic specialization contexts.

## JSON migration

Keep the current codec until the replacement passes compatibility and performance checks. Implement a format-neutral reflected traversal layer; leave JSON policy in `std/json`.

First migrate typed conversion: retain the existing native scanner/writer backend behind narrow lexical token and output operations, while implementing type-directed traversal and construction in Dusk through `std/reflect`. The lexical interface must not allocate typed destination structs or call JSON-specific compiler hooks. A user-written parser must be able to use the same reflection/construction APIs without those lexical helpers.

Writing the tokenizer itself in Dusk is a separate optional experiment. Combining a tokenizer replacement with reflection migration would mix two sources of performance changes. Report clearly which parts execute natively and which execute as Dusk code.

Preserve all current contracts:

- Explicit and inferred generic decoding, aliases and selected imports.
- Supported roots and nested types, recursive/generic types and compile-time unsupported-type diagnostics.
- Field names/order, explicit defaults, missing nullable behavior and private-field rejection.
- Strict integer tokens, full integer precision, float range policy, UTF-8/escape validation and complete-input consumption.
- Duplicate keys in every subtree, unknown-field policy, depth bounds and cycle detection.
- Enum alias policy, tagged-union shape, map insertion order and compact encoding.
- Payload-free errors, caller-owned diagnostic reset and exact location/path conventions.
- Default ordering, mutable-default independence and defaults that park, with no defaults executed for an invalid document.

Compile-time compatibility needs a general capability-validation mechanism over known descriptors, rather than ordinary runtime checks disguised as compile-time reflection. Prototype a format-neutral validator covering allowed kinds, map keys and field visibility. Keep that gate explicit: metadata introspection alone cannot preserve today's compile-time JSON rejection. Finalize whether it is a restricted declarative capability argument or a dedicated compiler-recognized reflection operation before migration.

After migration, remove JSON-specific sema/codegen tables and bindings. Share descriptors and default callback machinery between reflection consumers. Runtime code continues to depend only on runtime-owned formats. Keep any baseline implementation only in history or an explicitly temporary comparison build.

## Benchmarks and comparison

The baseline uses the existing process-level `bench.py` runner and ReleaseFast builds. JSON cases are Dusk-only entries; they do not add incomplete entries to the separate comparison report.

| Workload | Work per process | Checked output |
| --- | --- | --- |
| `json_decode` | Decode a nested record 100,000 times: escaped string, nullable, array, map and nested struct. | `5200000` |
| `json_decode_bulk` | Build a fixed 256-record input once; decode it 1,000 times and visit all records. | `15104000` |
| `json_encode` | Decode the fixed 256-record input once; encode it 1,000 times, checking complete output equality. | `1000` |
| `json_defaults` | Decode 100,000 small records with three omitted fields; mutate the default array to check independent construction. | `4700000` |

The shared model/input lives in `benchmarks/dusk/support/json_model.dsk`. Fixtures, repetitions, checksums and validation must remain unchanged between implementations. No file IO or per-iteration printing is included. Process startup, source compilation, fixture setup and assertions are included in elapsed time. Peak RSS is process-wide; these are end-to-end workload measurements, not isolated parser throughput or allocation counts.

Run the baseline after committing the workloads:

```
python3 bench.py --json json_decode json_decode_bulk json_encode json_defaults --runs 15 --warmup 3 --save json-native-baseline
```

Store the machine, toolchain, source commit, settings, median, spread, peak memory and noise flags in `benchmarks/history/`. If a result is noisy, rerun with more samples and retain/report the noise status; do not claim a speed difference from noisy data. Record reruns separately rather than silently replacing the first samples.

After migration, run the same cases alternately against the baseline source commit:

```
python3 bench.py --json json_decode json_decode_bulk json_encode json_defaults --compare 7aa6591 --runs 30 --warmup 3 --save json-reflection-comparison
```

`--compare` uses each commit's own workload, so explicitly verify that the benchmark sources and shared model are identical. Pin the baseline commit that includes these files, not the earlier JSON feature commit. Build and compare on the same machine/toolchain with no competing benchmark or heavy build work.

Report time and peak-memory changes per workload. Retain full results even when slower. A repeatable time or memory regression above 5% triggers investigation; this is an investigation threshold, not a guarantee that smaller changes are meaningful. Do not replace the default codec while regressions remain unexplained or compatibility differs. If costs remain substantial, discuss keeping the native fast path alongside public reflection.

## Phases and acceptance gates

1. **Baseline and plan.** Commit deterministic workloads, run correctness checks, save baseline measurements and provenance, and write the implementation plan. This is the current task.
2. **API and safety prototype.** Finalize descriptor identity, reflected value ownership/mutability, capability validation, builder state and deferred default sessions. Demonstrate a generic builder and a runtime-discovered nested field without format-specific compiler changes. Do not expand the language with generic methods just for convenience.
3. **Read-only reflection.** Emit canonical recursive descriptors; add inspection, checked primitive extraction and traversal. Test recursive types, exact identity, aliases, imported generics, visibility and side-effect evaluation. Add documentation and commit.
4. **Construction.** Add checked values/builders for all JSON-supported shapes, typed recovery, completion sessions and default callbacks. Test all error states, GC relocation, abandoned/aliased builders, task parking and mutable defaults. Register every new fixture. Add documentation and commit.
5. **Independent decoder proof.** Implement a small decoder in an ordinary user module with generic roots, a nested struct and a collection using only public reflection APIs. No externs, compiler edits, JSON helpers or private standard-library access. Demonstrate that a new format can be added by a user. Commit the example and verification.
6. **JSON migration.** Move type-directed conversion onto public reflection, retain the lexical backend initially, preserve compile-time diagnostics and the full JSON test suite, then remove redundant JSON compiler machinery. Verify Debug/ReleaseFast tests, native build and supported cross-builds. Commit separately.
7. **Comparison and decision.** Run alternating baseline/current measurements with unchanged workloads; investigate noise/regressions and publish all results. Select the implementation only after correctness and measured cost are understood. Record any remaining fast path explicitly.

Each phase updates task tracking and gets its own commit. Reflection and migration user-visible behavior receive changelog entries when implemented; this planning document and benchmark additions do not announce an implemented language feature.

## Decisions still to settle

The requested direction is reflection plus safe construction, followed by JSON migration and comparison. The exact API above remains proposed. Before implementation, settle:

- Owned reflected values versus borrowed views and where a copy is required to preserve mutability.
- The smallest public deferred-completion/session API that preserves default semantics for independent decoders.
- The format-neutral compile-time capability-validation surface.
- The protected runtime representation for descriptors, values and builders.

These are implementation gates, not claims that ordinary generic specialization already solves dynamic typed construction.
