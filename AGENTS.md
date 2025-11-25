# prime-lang: Current State & Roadmap

  - Docs: Fill roadmap/AGENTS; add a concise “What’s in 1.0” changelog; installation matrix/platform support; quickstart for
    editors (Neovim/VScode settings, expand usage).
  - LSP polish: Completion snippets/details for macros (@sep hint, repeat syntax); hover/diagnostics for @sep misuse and hygiene
    escapes; ensure all keywords (using, try, move, etc.) have hover docs; add tests for imported macro hovers and custom-sep
    completions.
  - Macro UX: Formatter golden for @sep = |; CLI expand optional flag to show call-site snippet; consider a mode to emit expanded
    snippets with original line numbers for debugging.
  - Testing: Add integration tests for CLI expand behavior; end-to-end manifest-driven prime-lang test if desired; more
    concurrency edge tests (channel close/blocking), pointer/heap safety corner cases.
  - Tooling/Release: Changelog and versioning; release script/CI matrix (lint/fmt/test on Linux/macOS/Windows); binary
    distribution plan or install instructions; verify prime-lang test discoverability in docs/usage.
  - Error messaging: Audit remaining “Unexpected tokens…” zero-span sites; better span/help for format placeholders and macro
    separator errors.
  - Performance/ergonomics: Optional cached manifest/module load for editor workflows; confirm deterministic build mode vs run
    parity is documented and tested.

