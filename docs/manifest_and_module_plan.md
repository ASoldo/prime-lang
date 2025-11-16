# Prime Manifest & Module System Plan

This note captures how we can evolve `prime.toml`, module declarations, and the
import syntax so that Prime workspaces feel as modern as Cargo or uv based
ecosystems. It keeps today’s behavior working while making room for smarter
tooling (formatter, LSP, linting, IDE integrations).

## Goals

- Single manifest describes every runnable artefact (bins, libs, tests),
  dependency, feature flag, and module boundary, so `prime run/build/fmt` never
  guess which files to include.
- Symbolic module/import syntax unlocks hover/completion/go-to-def, lets
  linters enforce visibility, and keeps formatter aware of package layout.
- Workspaces scale to mono-repos: shared dependencies at the top, per-member
  manifests inheriting profile/toolchain settings, and commands that operate on
  the whole workspace.
- Backward-compatible migration path; existing `prime.toml` files continue to
  work until users opt in, and a `prime migrate` command can rewrite files.

## Manifest Layout

Prime manifests stay in `prime.toml` but get a richer schema. Names stay close
to Cargo so the intent is obvious while remaining specific to Prime.

```toml
[workspace]
members = ["crates/runtime", "apps/chat", "examples/*"]
default-members = ["apps/chat"]
exclude = ["crates/old"]
metadata.toolchain = { channel = "latest", components = ["fmt", "lsp"] }
[workspace.dependencies]
std = { path = "crates/std" }
tower = { version = "1.2", registry = "primeforge" }

[package]
name = "prime-lang-playground"
version = "0.1.0"
edition = 2025
description = "Playground targets for the Prime toolchain"
license = "Apache-2.0"
authors = ["Prime Team <prime@example.com>"]
publish = false              # keep local; default true
entry = "app"                # default binary target
kind = "binary"              # "binary" | "library"

[lib]
name = "runtime"
path = "src/lib.prime"
visibility = "pub(crate)"    # "pub", "pub(crate)", "private"
deps = ["std", "pkg_core"]

[[targets.bin]]
name = "app"
path = "pkg_app.prime"
entry = true
deps = ["runtime"]
features = ["http", "metrics"]
env = { PRIME_BACKEND = "jit" }

[[targets.test]]
name = "runtime-tests"
path = "tests/runtime.prime"
deps = ["runtime", "testkit"]

[dependencies]
pkg_core = { path = "../pkg_core" }
serde_prime = { version = "0.4", registry = "primeforge", features = ["derive"] }

[dev-dependencies]
testkit = { path = "crates/testkit" }

[[modules]]
name = "core::types"
path = "types.prime"
package = "prime-lang-playground"
visibility = "pub"
doc = "Structs/enums shared by demos"
```

### Key Sections

- `[workspace]`: Enumerates members, default targets, exclusions, workspace-wide
  dependencies, and metadata (toolchain channel, lint profile). The CLI walks
  this list to implement `prime workspace list`, `prime fmt --workspace`, etc.
- `[package]`: Identifies the current member. `kind` tells the CLI whether the
  default build is a binary/app or a library; `entry` is the preferred binary
  when multiple `[[targets.bin]]` exist.
- `[lib]`, `[[targets.bin]]`, `[[targets.test]]`, `[[targets.bench]]`,
  `[[targets.example]]`: Explicit target tables declare path, dependencies,
  optional env/feature gates, and whether the target is runnable by default.
- `[dependencies]`, `[dev-dependencies]`, `[build-dependencies]`: Each entry can
  specify `version`, `path`, `git`, `branch`, `tag`, `rev`, `registry`,
  `features`, `default-features`, and `workspace = true` to share a dependency
  declared at the workspace level.
- `[[modules]]`: Registry of file-backed modules. Each module indicates its
  canonical symbol (`core::types`), file path, owning package, and visibility.
  Formatter/LSP/CLI use this to resolve imports without scanning the fs.

The manifest parser stays tolerant: if `[[modules]]` is missing, Prime falls
back to the old `[modules]` table or path-based inference.

## Module & Import Syntax

1. **Module declaration** (first non-comment line):

   ```prime
   module core::types;
   ```

   This tells the resolver which `[[modules]]` entry the file belongs to, keeps
   crate-relative paths stable, and lets tools verify that files live under the
   correct namespace (e.g., `core::math::vec` must be inside `core/math/vec.prime`).

2. **Symbolic imports** replace string literals:

   ```prime
   import core::types::{Player, Vec2};
   import pkg::math as prime_math;
   pub import std::testing::*;
   ```

   - Paths are AST nodes, so the LSP can offer completion, hover, rename, and
     diagnostics.
   - `pub import` re-exports the binding from the current module; the resolver
     checks that the source module allows `pub` visibility.
   - `import package::module::{Item as Alias}` keeps aliasing optional but
     explicit.

3. **Scopes** mirror Rust: top-level modules map to packages, `core::` for
   workspace members, `std::` for the standard library, and `pkg::` for
   third-party dependencies pulled from registries.

4. **Formatter/LSP** maintain canonical ordering: std → workspace → local, with
   blank lines between tiers.

## Tooling & LSP Integration

- **CLI** (`prime run/build/fmt/lint/test`):
  - Reads `prime.toml`, selects targets, ensures referenced modules exist, and
    wires dependencies according to the manifest.
  - Offers `prime workspace <cmd>` wrappers, `prime add <dep>`, `prime init
    --workspace`, and `prime migrate-manifest` to convert legacy files.

- **Formatter**:
  - Uses `[[modules]]` to resolve canonical names, sorts imports, and warns when
    a file’s `module` declaration does not match its path or manifest entry.

- **LSP**:
  - Loads the manifest once, watches it for changes, and seeds completion lists
    with every module/dependency.
  - Hover/go-to-def on imports jump directly to the file recorded in
    `[[modules]]`.
  - Diagnostics cover missing modules, attempting to import non-`pub` symbols,
    and stale manifest entries.

## Migration Strategy

1. **Manifest scaffolding**:
   - Ship `prime migrate-manifest` to transform `[modules]` into `[[modules]]`
     and introduce default `[package]` metadata based on current CLI flags.
   - Old fields remain supported until users opt-in; we gate new behavior behind
     `manifest_version = "2"` to avoid surprises.

2. **Module declarations**:
   - Parser warns (not errors) when a file lacks `module ...;`.
   - Formatter can insert the declaration automatically (`prime fmt --fix`).
   - Imports keep accepting strings for one release with a lint reminding users
     to convert.

3. **Tooling**:
   - LSP prioritizes the manifest-driven graph but gracefully degrades if the
     manifest is missing or malformed.
   - CI/docs emphasize the new structure, and sample projects (like this repo)
     ship with v2 manifests to demonstrate best practices.

## Next Steps

1. Implement the new TOML schema (parser + data types) behind a feature flag;
   unit-test deserialization using samples similar to the snippet above.
2. Teach the resolver and CLI to build a module graph from `[[modules]]` and
   `module` declarations, enforcing visibility rules.
3. Update the parser/LSP to accept symbolic imports, surface completions, and
   emit lints for the legacy string form.
4. Refresh documentation + templates (README, `prime init`, blog posts) to
   reflect the workspace-first workflow.
