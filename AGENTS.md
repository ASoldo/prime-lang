• - Embedded async polish: replace the calibrated busy-wait in recv_task/recv_timeout with a small wait queue/poll strategy and
    make channel/task pool sizes configurable; document expected latency on Xtensa.
  - Toolchain hardening: freeze known-good Xtensa/esp-clang + linker script set (with -relocation-model=static and libc/libgcc)
    and capture it in a template; add a smoke build/flash step (or emulated) in CI.
  - Diagnostics/contract: clarify panic/unwind/abort expectations for host and embedded; emit a clear error if async is used on
    targets that can’t supply runtime handles; stabilize error codes/messages.
  - Docs: expand embedded docs with strap-pin/LED guidance, runtime-handle defaults for async, and the new static relocation/
    linker flags; add a concise language/reference outline (expressions, ownership, macro hygiene).
  - Formatting/tests: add formatter regression cases for async initializers and multiline lets; add golden-output tests for key
    demos (channel/async, blink) and keep CI coverage for host + embedded builds.
  - Memory model/interop: document channel/thread ordering guarantees and determinism expectations; outline ABI versioning/
    stability for host/embedded surfaces.
  - Misc cleanup: resolve the lingering dead-code warnings (TaskValue::complete, Xtensa slot statics) or silence them
    intentionally; keep LSP/parser/formatter in lockstep.
