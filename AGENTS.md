# prime-lang: Current State & Roadmap

## Core Language (as of November 2025)
- **Type System**: Explicit scalar types (bool, rune, int8-64, uint8-64, isize/usize, float32/64, string), value structs/enums with embedding, slices/arrays, references (`&`, `&mut`) and raw pointers (`*`).
- **Control flow**: `if/else`, `for range`, `while`, `match` expressions with exhaustive checking, `break/continue`, `return`, `defer` for deterministic cleanup.
- **Functions**: Free functions with explicit parameters/returns, multiple return values (tuple style), method-call sugar (`player.heal()` desugars to `heal(player)`), immutable-by-default bindings with `let`/`let mut`.
- **Memory & Heap helpers**: Box, slice, and map runtime values with explicit APIs (`box_new`, `slice_push`, etc.) plus move semantics for heap-owning types; borrow tracking prevents double mutable borrows.
- **Interfaces**: Static `interface Foo { fn bar(self: &Type) -> ... }` plus `impl Foo for Type { ... }`. Methods are zero-cost (no vtables) and share the existing function registry.
- **Tooling**: Built-in `prime run`, `prime build`, formatter, and LSP (completion, hover, diagnostics) aware of slices/interfaces/methods.

## Recent Milestones
- Refined formatter/LSP to handle interfaces and `impl` blocks (symbols, hover, completion, indentation).
- Namespaced functions by `(name, receiver)` so multiple methods with the same name can coexist (prereq for interfaces).
- Added heap demo (`heap_features.prime`) and namespacing demo (`ns_demo.prime`) to validate runtime/build mode parity.

## In-Progress / Deferred
1. **Generic Functions (Phase A)** – Extend `FunctionDef`/calls with type parameters, add monomorphization cache keyed by `(name, receiver, type args)` in both interpreter and compiler, and implement type substitution.
2. **Interface Ergonomics** – After generics, allow interface-typed parameters (`fn draw(obj: Drawable)`) by instantiating per concrete type; add `self` shorthand (`fn draw(self: &Sprite)`) and auto-register method aliases so `sprite.draw()` and `draw(sprite)` share symbols.
3. **Generic Interfaces/Impls (Phase B)** – Support `interface Serializer[T] { ... }` and `impl Serializer[Hero] for HeroSerializer { ... }`, reusing the monomorphization machinery.
4. **Error-Handling Sugar** – Provide `try`/`?` syntax for `Result`, ensuring both runtime and build-mode codegen handle early returns cleanly.
5. **Deprecated Helpers Sunset** – Replace `slice_*`/`map_*` functions with first-class literals/methods and remove the warning layer once new APIs land.

## Next Steps
- Finish Phase A (generic functions) before touching interface ergonomics.
- Update demos/tests once generics land (identity function, interface+generic combo, generic heap usage).
- Continue iterating with formatter/LSP after each language feature to keep tooling parity.
