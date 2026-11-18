# Development Workflow

Prime-lang is a single Rust binary (`prime-lang`) plus a workspace of `.prime` demos used as fixtures for the CLI and docs.

## Build & install

- Build (iterative): `cargo build --locked`
- Install the CLI: `cargo install --locked --path .`

A native `prime-lang build` compiles the bundled Rust runtime with its own lockfile. Set `CARGO_TARGET_DIR` to an absolute path to share compiled dependencies between Prime projects. Cargo recompiles the ABI when its bundled sources or build settings change. Graphics and audio libraries still need their system development headers.

## Checks

- CI-equivalent (recommended): `./scripts/ci.sh`
- Format: `cargo fmt --all -- --check`
- Tests (single-threaded integration harness): `cargo test --locked --all -- --test-threads=1`
- Lint (Rust): `cargo clippy --locked --all-targets --all-features -- -D warnings`

Note: `cargo test -- --format=json` and `cargo test -- --report-time` require nightly `-Z unstable-options`.

The Linux runners use Xvfb to open a real X11 window for interpreted and compiled Tetris after the headless checks. Set `PRIME_CI_X11=1` when running `./scripts/ci.sh` on a host with `xvfb-run` and `xauth` installed. Each game exits after three frames.

## Dependency advisories

Install the audit tool with `cargo install --locked --version 0.23.0 cargo-audit`, then run `./scripts/audit-dependencies.sh`. It refreshes the RustSec database once and checks four lockfiles against the same snapshot: the CLI, the bundled host and embedded runtimes, and the editor grammar. A reported vulnerability or failure to audit a file makes the command fail. The output also lists maintenance warnings. The **Dependency audit** workflow runs on changes to these lockfiles or the audit itself, and can be triggered manually for unchanged releases. [RustSec advisory database](https://rustsec.org/)

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

`prime-lang lint <file.prime>` checks lexing, parsing, and module declarations and imports against the nearest `prime.toml`, when present. Use `prime-lang check <file.prime>` for semantic checks and `prime-lang run <file.prime>` to execute an entry module.

| One-shot lint exit code | Meaning |
| --- | --- |
| `0` | The check completed cleanly or only produced warnings. Missing or mismatched module names and duplicate imports are examples of warnings. |
| `1` | The source or manifest could not be read or parsed, or lint found an error such as an unresolved import or a module kind mismatch. |
| `2` | The command line was invalid, for example because the file argument was omitted. |

Diagnostics go to stderr. A clean file prints `no diagnostics` to stdout. With `--watch`, the CLI checks immediately and again when the source changes, including atomic saves. A failed check leaves the watcher running so the next edit can fix it. A watcher setup or backend failure exits with code `1`. Press Ctrl+C to stop watching.

`./scripts/check_examples.sh` requires Bash and Python 3.11 or newer. It reads the module, library and test paths in every `workspace/**/prime.toml`, resolves them relative to their manifest, and runs both lint and semantic checking for each distinct source. The first line reports the file and manifest counts. Set `PRIME_RUN_EXAMPLES=1` to also invoke `prime-lang run` after each successful check. This includes library and test files, and some demos may require input or hardware.

The script uses `PRIME_BIN` when set (an executable path or command name). Otherwise it tries the local debug build, the local release build, then `prime-lang` on `PATH`. It respects `CARGO_TARGET_DIR` for local builds. Empty inventories, invalid manifests, missing source files and unavailable tools fail the sweep. A failed lint, check or run stops the script with that command's exit status.

After `cargo build --locked`, run `python3 scripts/tests/test_lint.py -v` to check the CLI exit codes, watch recovery and sweep behavior. To run only the inventory and shell checks, use `python3 scripts/tests/test_lint.py ExampleSweep -v`.
