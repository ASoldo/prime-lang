# prime-lang: Current State & Roadmap

## Core Language (as of March 2026)
- **Type System**: Explicit scalar types (bool, rune, int8-64, uint8-64, isize/usize, float32/64, string) plus user structs/enums with embedding. Slice (`[]T`) and map (`Map[K, V]`) literals are parsed into real AST nodes rather than helper calls.
- **Control flow**: `if/else`, `for range`, `while`, `match` expressions with exhaustive checking, `break/continue`, `return`, `defer` for deterministic cleanup.
- **Functions**: Free functions with explicit parameters/returns, multiple return values (tuple style), method-call sugar (`player.heal()` desugars to `heal(player)`), immutable-by-default bindings with `let`/`let mut`.
- **Memory model**: Heap-backed boxes/slices/maps exist as first-class runtime values. Move semantics guard heap-owning identifiers (`let moved = move counter;`) and a simple borrow stack enforces “one mutable borrow at a time” for `&mut`.
- **Interfaces**: Static `interface Foo { fn bar(hero: Hero) -> ... }` with `impl Foo for Hero { ... }`. Calls remain zero-cost (no vtables) and reuse the function registry. Formatter/LSP understand interfaces, generic impls, move semantics, and symbolic imports/manifests.
- **Tooling**: `prime run/build/fmt/lint` plus the LSP use the manifest graph (`prime.toml` v2) to resolve modules, imports, interfaces, and impl blocks. Treesitter/formatter keep indentation/mappings in sync with the new module/import syntax.

## Recent Milestones
- Tree-sitter + Neovim updates: grammar covers `module`, `pub import`, interfaces/impls, and scoped identifiers; highlights/Aerial queries follow the new nodes in lock-step with the parser.
- Manifest-aware lint/formatter: `prime lint` surfaces missing headers/duplicate imports/manifest mismatches; `prime fmt` auto-inserts module declarations based on `prime.toml`.
- LSP improvements: scope-aware type tracking feeds completions/hover, so `self`/parameters resolve to the correct struct, chained completions like `self.position.x` work, and interface-typed values still expose their methods with manifest-aware visibility checks.
- Added array/map literals plus runtime support for `Value::Slice`/`Value::Map`, move expressions, and borrow tracking; `heap_features.prime` showcases the new behavior.
- Brought `interface`/`impl` syntax online (formatter + parser + runtime) so structs expose methods via static interfaces; completions/go-to-def now cover interface methods.
- Finished generic functions (Phase A): both interpreter and compiler cache specializations by `(name, receiver, type args)` and substitute concrete types, with `generic_demo.prime` serving as the regression case.
- Landed interface ergonomics: interface-typed parameters now validate against concrete impls, the parser/formatter support `self` shorthand (value/reference/pointer), and the LSP surfaces interface methods for completion/hover.
- Enabled generic interfaces/impls (Phase B): `interface Foo[T]` and `impl Foo[Type] for Bar` clone via the existing monomorphization cache across interpreter/compiler; `interface_generics_demo.prime` exercises interface + generic combos.

## In-Progress / Deferred
1. **Module/Packaging metadata** – Manifest parsing (`prime.toml`) and `pub` visibility are wired in, but tooling still needs to enforce accessibility in linters/formatters, validate manifests, and expose package metadata to IDEs. Dependency resolution across manifests remains open.
2. **Manifest-aware navigation cache** – Symbol index/go-to-def/reference queries must preload modules declared in the manifest, honor visibility (`pub`/package/private), and work even if files aren’t opened in the editor.
3. **Formatter/lint parity** – `prime fmt`/`prime lint` need to share the same manifest/module/import diagnostics so CLI workflows match the LSP (missing headers, duplicate imports, manifest mismatches, suggested quick-fixes).
4. **Error-Handling Sugar** – Provide `try`/`?` syntax for `Result`, ensuring both runtime and build-mode codegen handle early returns cleanly.
5. **Trait-style helpers** – Auto-alias interface methods across modules (workspace symbols, go-to-def) and keep formatter/LSP parity as new syntax lands.
6. **Deprecated Helpers Sunset** – Remove the `slice_*`/`map_*` helper API after literals/methods cover all use cases; warnings already fire to nudge users.

## Next Steps
- Add regression demos/tests that mix generics with interfaces (e.g., interface constraints inside generic functions) to keep coverage high.
- Extend `prime.toml` tooling: lint/formatter should resolve modules via the manifest, validate entry modules, and warn on private-item leaks. Hook package metadata into LSP diagnostics/hover.
- Build the manifest-aware go-to-def/reference cache and land identical diagnostics/fixes in `prime fmt`/`prime lint` so CLI + editor experiences stay aligned.
- Once packaging lands, finish the error-handling sugar and remove deprecated slice/map helpers.
