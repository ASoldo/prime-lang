# Prime Lang

The implemented language is described in docs/LanguageOutline.md. Keep changes to syntax, type rules, code generation and tooling consistent with that document and docs/AsyncAwait.md.

- Use the repository toolchain and committed Cargo locks. Run ./scripts/ci.sh from a Rust/LLVM host. It exercises the Rust CLI, editor grammar, examples, Prime tests, documentation and headless Tetris. On a small ARM host set CARGO_BUILD_JOBS=2.
- For a semantic change, include a minimal successful program and a regression covering the failure. Compare the interpreter with the emitted native binary for backend changes.
- Preserve the versioned machine-readable output of prime-lang check. Keep LSP UTF-16 coordinates distinct from UTF-8 source byte offsets.
- Update the Tree-sitter grammar, generated parser and corpus when syntax changes. Edit grammar.js and regenerate with the supported generator.
- For embedded changes, report the target and distinguish firmware build results from execution on a board. Use --no-flash for build verification.
