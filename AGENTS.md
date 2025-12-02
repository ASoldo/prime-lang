- Runtime Result/? parity: Our workaround removed ? from the blink loop. We should add compiler support for runtime Result
  unwrap in async blocks (build-mode lowering that uses runtime handles for enums), then reintroduce ? in embedded demos to keep
  semantics aligned.
- Channel/task pool tuning + leak checks: Add a small no_std test/demo that repeatedly opens/closes channels/tasks to ensure
  pool recycling stays safe (build/run and embedded). This will catch the earlier slot thrash and any residual corruption.
- Async task join semantics: Ensure spawn/join in no_std use the task runtime fully (not just deterministic paths). Add a parity
  test: spawn async, await recv_task, join, and assert outputs match run/build.
- Docs/tests coverage: Update the parity table to list embedded spawn/join as supported, and add a no_std parity test fixture
  that mirrors a host sample (channels + async + Result).
- Formatter regression guard: Keep the new spawn async { ... } golden; consider adding a no_std demo formatter case if you
  expand the blink file further.
