  ### 1) Syntax & AST

  - Introduce closure literals, e.g.:
      - |x: int32, y| x + y (optional types; infer where possible)
      - Allow mut on params? (likely no)
      - Support return type annotation: |x| -> int32 { x + 1 } and expression-bodied |x| x + 1.
  - AST additions in src/language/ast.rs:
      - New Expr::Closure { params: Vec<FunctionParam>, body: ClosureBody, ret: Option<TypeAnnotation>, span, captures:
        Vec<CapturedVar> (populated later) }
      - ClosureBody to distinguish block vs expression.
      - CapturedVar struct to record name, mutable flag, type (later), and capture mode (by move/borrow?).
  - Update parser (src/language/parser.rs) to parse closure literals in expression position:
      - Recognize leading | as closure start; parse params (ident [: type]?), then either -> type or not, then block or expr.
      - Ensure precedence: closure should bind tighter than match/if but looser than call? Typically closure is an atom like a
        function literal.
      - Update formatter to print closures.
      - Update Tree-sitter grammar to include closure nodes.

  ### 2) Type system design

  - Closure type representation in TypeExpr:
      - Add TypeExpr::Closure { params: Vec<TypeAnnotation>, returns: Vec<TypeAnnotation>, captures: Vec<CaptureKind> } or a
        simpler nominal shape with an environment type ID.
      - Prefer a structural function type with captures ignored in the signature but stored in a side table: TypeExpr::Function
        { params, returns } already exists? Reuse if present; else add. Keep runtime/env info alongside.
  - Capture rules:
      - Default capture by reference (&T) when used immutably; by mutable reference (&mut T) if mut borrowed; by move if ownership
        is moved into the closure. Or pick a simpler rule: capture by value, move semantics (like Rust move closures), and later
        extend? To minimize complexity, start with “capture by move (copy for Copy types)”; no implicit borrowing.
      - Disallow capturing mutable borrows across closure lifetimes unless you model lifetimes; simplest is move-only capture.
  - Type inference:
      - Infer param types if the closure is assigned to a function-typed binding (let f: fn(int32) -> int32 = |x| x+1;). Otherwise
        require param annotations.
      - Return type inferred from body if omitted.

  ### 3) Captures analysis

  - In typechecker (src/language/typecheck.rs), when encountering Expr::Closure:
      - Push a new scope with parameters; analyze body; collect free variables from outer scopes.
      - For each free var, decide capture mode (initially: move).
      - Record captures on the AST node (or side table).
      - Ensure capture visibility & borrow rules (if move-only, check the variable can be moved here).
  - The closure gets a function signature type; store an internal synthetic function def in the registry keyed by a fresh name
    (e.g., __closureN within the module) with an implicit first “env” param if needed for IR.

  ### 4) IR/runtime representation

  - Decide runtime shape:
      - Lower each closure to a struct { env_field(s)..., fn_ptr_or_tag }.
      - Interpreter: represent closures as a new Value::Closure { env: Map<String, Value>, body: Rc<Expr>, params, returns } or
        pre-lowered callable with captured values stored.
      - Build/IR: generate an env struct type per closure, populate it with captured values, and emit a static function taking
        (env_ptr, params...). The closure value is a pair (env_ptr, function_ptr) or a fat pointer struct.
  - Add a callable trait/ABI:
      - In IR builder, add a “call closure” opcode: when calling a closure value, pass env pointer as first arg.
      - Adjust typechecker to allow calling closure-typed expressions like functions.

  ### 5) Language surface & semantics

  - No recursion through closures unless supported by storing self? (Could be supported by capture of a binding holding the
    closure.)
  - No async; just synchronous call.
  - Borrow checker: if move-capture only, decrement/move the outer binding. If later you want borrow captures, you need lifetime-
    like checks; defer that.

  ### 6) Tooling updates

  - Formatter: pretty-print closure params, optional return arrow, block vs expr bodies.
  - LSP:
      - Completion: treat closures as expressions; no special completion needed beyond params.
      - Hover: show closure signature.
      - Diagnostics: ensure free-var capture errors surface clearly.
  - Docs/README: add a short section “Closures (experimental)” with syntax and simple examples.

  ### 7) Tests

  - Parser tests for closure syntax (block/expression, typed/inferred params, return annotation).
  - Typechecker tests:
      - Capturing outer vars (move semantics), disallow use-after-move.
      - Inferred param types from context.
      - Callability: passing closures to functions expecting fn-like types.
  - Interpreter/IR tests:
      - Simple map/filter demo; arithmetic closure; captured counter increment; ensure captured value is moved.
  - Integration: tiny demo file under demos/ to exercise build+run parity.

  ### 8) Incremental rollout

  - Start with parser + AST + formatter + grammar updates.
  - Add TypeExpr::Function/closure-friendly type if not present; treat closure type as compatible with fn types where captures are
    move-only.
  - Add typechecking for closures with move captures.
  - Interpreter support for closures.
  - Compiler lowering to env+fn pair; calling convention update.
  - LSP/docs/tests.

  Given scope, I’d implement in phases (parser/AST + typeck + interpreter first) and gate build/IR last to keep progress testable.
