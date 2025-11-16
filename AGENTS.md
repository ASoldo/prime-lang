# prime-lang: Current State & Roadmap

## Core Language (as of November 2025)
- **Type System**: Explicit scalar types (bool, rune, int8-64, uint8-64, isize/usize, float32/64, string) plus user structs/enums with embedding. Slice (`[]T`) and map (`Map[K, V]`) literals are parsed into real AST nodes rather than helper calls.
- **Control flow**: `if/else`, `for range`, `while`, `match` expressions with exhaustive checking, `break/continue`, `return`, `defer` for deterministic cleanup.
- **Functions**: Free functions with explicit parameters/returns, multiple return values (tuple style), method-call sugar (`player.heal()` desugars to `heal(player)`), immutable-by-default bindings with `let`/`let mut`.
- **Memory model**: Heap-backed boxes/slices/maps exist as first-class runtime values. Move semantics guard heap-owning identifiers (`let moved = move counter;`) and a simple borrow stack enforces “one mutable borrow at a time” for `&mut`.
- **Interfaces**: Static `interface Foo { fn bar(hero: Hero) -> ... }` with `impl Foo for Hero { ... }`. Calls remain zero-cost (no vtables) and reuse the function registry. Formatter/LSP understand the new blocks and keep indentation/mappings intact.
- **Tooling**: Built-in `prime run`, `prime build`, formatter, and LSP cover slices, interfaces, impl blocks, move expressions, and heap literals.

## Recent Milestones
- Added array/map literals plus runtime support for `Value::Slice`/`Value::Map`, move expressions, and borrow tracking; `heap_features.prime` showcases the new behavior.
- Brought `interface`/`impl` syntax online (formatter + parser + runtime) so structs expose methods via static interfaces; completions/go-to-def now cover interface methods.
- Finished generic functions (Phase A): both interpreter and compiler cache specializations by `(name, receiver, type args)` and substitute concrete types, with `generic_demo.prime` serving as the regression case.

## In-Progress / Deferred
1. **Interface Ergonomics** – Allow interface-typed parameters (`fn draw(obj: Drawable)`), add `self` shorthand (`fn draw(self: &Sprite)`) and auto-register aliases so `sprite.draw()` and `draw(sprite)` share the same symbol entry and tooling metadata.
2. **Generic Interfaces/Impls (Phase B)** – Support `interface Serializer[T] { ... }` and `impl Serializer[Hero] for HeroSerializer { ... }`, piggybacking on the generic monomorphization cache.
3. **Error-Handling Sugar** – Provide `try`/`?` syntax for `Result`, ensuring both runtime and build-mode codegen handle early returns cleanly.
4. **Module/Packaging metadata** – Formalize imports via `prime.toml`, add visibility controls (`pub fn`, `pub struct`) and prep dependency resolution for larger projects.
5. **Deprecated Helpers Sunset** – Remove the `slice_*`/`map_*` helper API after literals/methods cover all use cases; warnings already fire to nudge users.

## Next Steps
- Ship interface-typed parameters and `self` shorthand, then update the formatter/LSP so completions show interface-provided methods automatically.
- Add demos/tests mixing generics with interfaces (e.g., `interface Printable[T]`) to exercise the new specialization pipeline in both `prime run` and `prime build`.
- Start designing `prime.toml` + visibility rules so packages/modules remain maintainable as we scale.
- Once ergonomics and packaging land, circle back to error-handling sugar and finish removing deprecated slice/map helpers.
