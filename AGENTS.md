• Here’s the remaining parity gap list with concrete fixes to get to a v1.0-ready state:

  - Build snapshot coverage
      - Allow capturing channel endpoints, join handles, pointers/references, and closures when PRIME_BUILD_PARALLEL=1.
      - Implement a serializable handle form for these types (similar to closures we just added) and ensure value_to_build_value/
        build_value_to_value round-trip them safely.
  - Dynamic operands in build mode
      - Lift “constant only” constraints where safe: slice/map indexing, recv_timeout millis, and range bounds.
      - For any ops that must stay constant (e.g., array lengths), surface explicit diagnostics and document them.
  - Boxes/pointers in build mode
      - Allow introspection of captured box handles (e.g., box_get/box_set) and pointer-derived values during build-mode runs, not
        just at runtime.
      - Ensure pointer captures can be serialized in snapshots if parallel build is enabled.
  - Channel semantics in build snapshots
      - Permit channel endpoints in snapshots and replay their queues/closed state deterministically when applying build effects,
        mirroring runtime behavior.
  - Printing/formatting parity
      - Make out/debug_show handle structs and other runtime-only values in build mode, matching runtime formatting (or explicitly
        document any remaining limits).
  - Borrow/defer parity for captured handles
      - Extend the captured-borrow tracking we added for references to boxes/pointers/channels so borrow diagnostics match runtime
        behavior for all handle types.
  - Docs/tests
      - Document the above behaviors and add regression tests that cover the newly supported capture/handle scenarios in build
        mode (channels, join handles, pointers, boxes, dynamic indices).
