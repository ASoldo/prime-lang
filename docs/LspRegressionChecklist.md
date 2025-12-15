# LSP / Editor Regression Checklist

This is a quick manual checklist for validating Prime’s editor experience (LSP + tree-sitter + formatter) before tagging a release.

## Pre-flight

- `cargo install --path . --force`
- Start the LSP server: `prime-lang lsp`
- (Optional) Lint the whole workspace: `./scripts/check_examples.sh`

## LSP feature checklist (per file)

For each demo below, confirm:

- Diagnostics: no spurious “unknown symbol” errors.
- Completion: identifiers, imports, and member access complete as expected.
- Hover: shows kind + signature + doc comments when present.
- Go-to-definition + references: navigates to the right symbol.
- Rename: updates all references (including macro params where relevant).
- Formatting: `prime-lang fmt --write` preserves `//` and keeps `///` doc comments intact.
- Symbols: document symbols/outlines (Aerial) match what’s in the file.

## Demo coverage

Open each module and spot-check the items above:

- Closures: `workspace/demos/closures/closure_demo.prime`
- Macros: `workspace/demos/macro/macro_demo.prime`, `workspace/tests/macros.prime`
- Heap/collections: `workspace/demos/heap/heap_demo.prime`, `workspace/demos/heap/heap_features.prime`
- Input: `workspace/demos/input/input_demo.prime`
- Parallel/channels: `workspace/demos/parallel/parallel_demo.prime`, `workspace/demos/channel_demo/channel_demo.prime`
- Time: `workspace/demos/time/time_demo.prime`
- IO (host-only): `workspace/demos/fs/fs_demo.prime`
- no_std parity fixture: `workspace/demos/no_std_parity/main.prime`

## Neovim UX (if using the bundled AstroNvim config)

- Config entrypoint: `lazy/astronvim/plugins/prime-lang.lua`
- Comment toggling: selection/line inserts `//` with correct indentation.
- Folding: code folds work (treesitter or LSP-based, depending on config).
- Macro expansion UI: expanded output opens in a scratch buffer, closes with `q`, and jumps to docs when relevant.

