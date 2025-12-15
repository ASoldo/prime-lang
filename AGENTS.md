- No unwinding/exception model—Result/? is the path; embedded panics abort.
- Limited standard library/FFI story; interacting with C/OS or richer collections is still thin.
- Tooling/tests: keep host parity tests for no_std probes to lock in behavior.

## Next tasks (agent)
- Run full `cargo test` + fix any failures.
- Run `cargo fmt` + `cargo clippy --all-targets --all-features -- -D warnings`.
- Update docs to describe async cancellation/timeouts (`CancelToken`, `await_timeout`, `await_cancel`, `await_cancel_timeout`).

• Updated Plan
  └ ✔ Reproduce current build failures
    ✔ Inspect existing task/await codegen
    ✔ Implement await_* builtin codegen
    □ Add docs + parity tests
    □ Run fmt/test/clippy and fix
