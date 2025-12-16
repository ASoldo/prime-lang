# Development Workflow

Prime-lang is a single Rust binary (`prime-lang`) plus a workspace of `.prime` demos used as fixtures for the CLI and docs.

## Build & install

- Build (iterative): `cargo build`
- Install the CLI: `cargo install --path . --force`

## Checks

- CI-equivalent (recommended): `./scripts/ci.sh`
- Format: `cargo fmt --all -- --check`
- Tests (single-threaded integration harness): `cargo test --all -- --test-threads=1`
- Lint (Rust): `cargo clippy --all-targets --all-features -- -D warnings`

Note: `cargo test -- --format=json` and `cargo test -- --report-time` require nightly `-Z unstable-options`.

## Prime docs (HTML app)

- Generate HTML: `prime-lang docs --generate --out docs.html`
- Serve over HTTP: `prime-lang docs --serve --port 7878` (default: `http://127.0.0.1:7878`, auto-regenerates + reloads on save)

Docs are sourced from Prime doc comments:

- `///` attaches to the next item (fn/struct/interface/macro/etc.)
- `//!` is module/file-level documentation (top of file)

## Editor / LSP

- Start the language server (stdio): `prime-lang lsp`
- Neovim config for AstroNvim lives under `lazy/astronvim/plugins/prime-lang.lua`.
- Manual LSP smoke checklist: `docs/LspRegressionChecklist.md`

## Example regression sweep

Run the bundled demos through the same parser + typecheck used by the LSP:

- `./scripts/check_examples.sh`
- `PRIME_RUN_EXAMPLES=1 ./scripts/check_examples.sh` (also executes runnable demos)
