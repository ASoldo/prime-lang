  ## High-level shape

  - Represent a closure as { env_ptr, code_ptr }, where:
      - env_ptr points to an environment struct containing captured values (by move).
      - code_ptr points to a synthesized function with signature ret = __closure_N(env_ptr, params...).
  - At codegen time:
      - Collect captures for each closure expression (already done in typechecker) and materialize an LLVM struct type per
        closure.
      - Generate a private LLVM function for the closure body; its first argument is the env pointer (opaque pointer to the env
        struct), followed by user params.
      - Closure values are lowered into a small LLVM struct { i8* env, i8* fn } or a concrete struct with the function pointer
        type; you can pick a stable shape in RuntimeAbi (e.g., { i8*, i8* }).
      - Calling a closure means: extract fn pointer, extract env pointer, and invoke fn(env_ptr, args...).

  ———

  ## Incremental implementation plan

  ### 1) Compiler IR/value support

  - Extend the compiler-side Value enum (in src/language/compiler.rs) with Closure { env: LLVMValueRef, func: LLVMValueRef,
    signature: FnSignature }.
  - Add a small FnSignature struct capturing params/returns for closures (reuse existing function metadata).
  - Update helper functions for arity/return handling to accept closure signatures.
  - Add a utility to materialize a closure LLVM struct constant/value from {env, fn}.

  ### 2) Env struct type + capture materialization

  - For each Expr::Closure during codegen:
      - Collect captured names and types from the annotated AST node (captures field).
      - Define an LLVM struct type __closure_envN with fields for each capture (in declaration order).
      - Allocate and populate the env struct at the closure creation site:
          - For move-only captures: load each captured value; if it’s a heap/move-checked value, respect move semantics already
            enforced by the typechecker; store into env fields.
      - Keep a map from closure AST ids to env struct types and the synthesized function symbol name (e.g., __closure_fnN).

  ### 3) Synthesized closure function emission

  - Add a pass to emit functions for all closures:
      - Signature: ret_ty __closure_fnN(%env_ty* env, param_tys...).
      - In the function body, materialize local bindings for captures by loading from env fields.
      - Then emit the user body exactly like a normal function body (reusing the block emitter).
      - Handle return values as with regular functions; if multiple returns are supported, keep tuple lowering consistent with
        existing conventions.
  - Ensure these functions are registered in the module (private linkage).

  ### 4) Closure literal codegen path

  - In emit_expression for Expr::Closure:
      - Emit the env allocation/population.
      - Obtain the LLVM function pointer for __closure_fnN.
      - Build the closure value { env_ptr, fn_ptr }.
      - Return it as an EvaluatedValue with the correct TypeExpr::Function signature.

  ### 5) Calling closures

  - Extend call emission to accept either:
      - A known function (existing path), or
      - A closure value (new path): load fn_ptr and env_ptr, cast fn_ptr to the synthesized function type, and call with env_ptr
        + args.
  - Update typechecking in the compiler path (already done logically) to allow Value::Closure as a callable target.
  - If interfaces or function pointers are used elsewhere, ensure compatibility: a closure fn type should be invokable where a
    function type is expected.

  ### 6) Runtime ABI updates (if needed)

  - If the runtime helper set assumes only free functions, add a closure-call helper or encode the calling convention directly in
    LLVM (preferred: direct call via function pointer, no new ABI function).
  - Keep the ABI stable by defining the closure struct shape in RuntimeAbi for C interop if necessary; otherwise, keep it
    internal.

  ### 7) Moves and cleanup semantics

  - Respect move-only captures:
      - On closure creation, mark captured bindings as moved in the codegen environment (consistent with typechecker).
      - Do not allow double moves; rely on typechecker correctness.
  - Ensure env allocations are owned by the closure value; free-lifetimes can be ignored if you rely on process lifetime, or add a
    drop path if you later support destructors.

  ### 8) Tests and demos

  - Add backend tests:
      - A closure returning arithmetic with captured values.
      - Higher-order call (passing a closure to a function and invoking).
      - Multiple captures and multiple params.
  - Update the closure demo build to produce a native binary and assert output.
  - Keep interpreter tests intact for parity.

  ### 9) Tooling/LSP

  - Ensure hover/formatting already reflects function types; no change needed beyond verifying closure calls are recognized as
    callable in analysis (likely already fine).

  ———

  ## Detailed task breakdown (landing order)

  1. Data structures:
      - Add Closure variant to compiler Value.
      - Add FnSignature helper for closures.
      - Add env struct registry/maps keyed by closure ids.
  2. Env struct builder:
      - Walk Expr::Closure nodes at codegen to define env LLVM types and capture lists.
      - Emit allocation/population code for env instances.
  3. Closure function emission:
      - Emit a new LLVM function per closure with env_ptr first.
      - In the body, bind captures from env_ptr and reuse existing block emission.
  4. Closure literal lowering:
      - In emit_expression, lower Expr::Closure to { env_ptr, fn_ptr }.
  5. Call-site support:
      - Extend call emission to recognize closure values and call them.
      - Add any necessary casting for function pointer types.
  6. CLI behavior:
      - Remove/fix the interpreter fallback once backend supports closures; restore normal build errors to surface genuine issues.
  7. Tests/demos:
      - Add unit/integration tests to the compiler/backend test suite.
      - Ensure workspace/demos/closures builds and runs natively (validate output).

  ———

  ## Notes/assumptions

  - Captures are move-only; no borrow semantics modeled in backend.
  - No recursion helpers beyond capturing a binding that references the closure value (allowed if typechecker permits).
  - Multi-return handling should mirror existing function lowering (tuples, etc.).
  - Memory management: env structs can be heap-allocated with the same lifetime assumptions as other heap values in your runtime;
    if you have a GC or refcount, integrate accordingly.
