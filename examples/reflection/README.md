# A decoder written with reflection

Run `./zig-out/bin/dusk examples/reflection-decode.dsk` from the project root.

`record.dsk` implements a small record notation using only public standard-library APIs:

```
{name=Ana;profile={city=Porto};scores=[3,5,8];role=Writer;email=null}
```

Records use braces and semicolon-separated `name=value` fields; arrays use brackets and commas. Text is bare and trimmed. The module supports int, byte, float, bool, string, enum, nullable, array and public-field struct targets, including nested and generic structs. Other target kinds are outside this example's scope. It is an illustration, with no quoting, escapes, maps, unions, format customization or configurable depth limits.

`decode[T]` obtains the target descriptor and recursively returns checked reflected values. Nested builders share a construction session; defaults run only after the complete input validates. Required fields, duplicate assignments and exact child types are checked by the builder API. Unknown fields and trailing input are rejected by the decoder. The example exercises explicit and inferred generic roots, defaults that wait for a task, nested records and arrays, plus malformed inputs.

There are no extern declarations or private standard-library calls. The compiler and runtime have no knowledge of this format.
