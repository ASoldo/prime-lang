• Here’s a focused v1.0.0 checklist from where we are now:

  - Diagnostics parity: Build-mode runtime errors still use plain strings. Thread borrower/move spans and names into build runtime
    errors (and prime build output) to exactly mirror run-mode wording, including multi-label hints.
  - No-std readiness: Replace the std-backed platform shim with a HAL trait impl and add the std-builtins feature gating path in
    CI. Add a no-std (or std-disabled) smoke test to ensure fs/time/spawn/channel are blocked with clear messages.
  - Docs polish: Expand the borrow/move diagnostics section in docs with before/after examples, and add a short “deterministic
    build effects” note for fs/time/iter in build mode.
  - Test coverage: Add build-mode regression tests for borrow conflicts, move-during-borrow, and drop-after-move to assert the new
    messages. Include a test that captures borrower names/spans in both run/build outputs.
  - Code cleanup: Remove temporary span placeholders (e.g., TODO capturing real move spans in build mode) so all recorded spans
    are meaningful.
  - Release readiness: Run cargo install --path . in release mode plus prime-lang test/demos in both run/build to ensure end-to-
    end sanity; tag the release once diagnostics/docs/tests pass.
