# Prime architecture

The CLI resolves an entry path or manifest module through `src/project`, loads imports, expands macros and checks the resulting program for the selected target. File and stdin entries share this path, including the bundled core fallback. The checker reports diagnostics. Both execution paths consume the expanded AST. The interpreter and native compiler consume the same checked integer operations and widths. The native compiler also receives match and postfix-propagation facts indexed by expression identity, including variant tags, payload types and guard types. [Checked program](../src/language/typecheck/checked.rs) · [CLI entry points](../src/main.rs) · [Package loader](../src/project/program.rs) · [Language outline](LanguageOutline.md)

| Command | Consumer of the expanded AST | Where program effects occur |
| --- | --- | --- |
| `check` | Target-aware type checker | Stops before evaluating the program. [Diagnostic contract](Checking.md) |
| `run` | `Interpreter::run`, `eval_statement` and `eval_expression` walk the AST with runtime values, bindings and cleanup scopes. | In the interpreter process. [AST evaluator](../src/runtime/interpreter.rs) |
| `build` | `Compiler::compile_checked_program` registers definitions and lowers from the entry point. Its emitter works with both compiler-known values and LLVM values. | The linked binary performs emitted operations such as reading stdin, opening a socket and writing to stdout. [Compiler flow](CompilerFlow.md) |

On the host, interpreted and compiled TCP calls share the loopback transport. In compiled code the call crosses a C-compatible ABI declared as LLVM function types and exported by a Rust static library. The CLI packages runtime source and lockfiles for separate host and embedded builds. ESP32 uses fixed-capacity storage and its own startup and timing code. [TCP implementation](../src/runtime/tcp.rs) · [Runtime bundle](../src/runtime/distribution/mod.rs) · [ABI and target limits](RuntimeFFI.md)

The language server uses the Rust frontend for source analysis. Tree-sitter supplies the editor grammar independently. Diagnostic JSON uses UTF-8 byte offsets, while LSP positions use UTF-16 code units. [Check output](Checking.md) · [Editor grammar](../tree-sitter/prime/grammar.js)

## Design risks and acceptance criteria

| Evidence in the implementation | Criterion for a change |
| --- | --- |
| Host code branches on the runtime Result tag and joins the exiting paths at the enclosing function or `try`. The filesystem regression exercises both paths in one binary and checks cleanup order. Embedded code diagnoses an unresolved tag. [Filesystem regression](../tests/filesystem_cli.rs) · [Codegen diagnostics](../tests/check_cli.rs) | Compile once, exercise both variants with different runtime inputs, and compare return values and subsequent side effects with the interpreter. Unsupported cases should report the source of the operation. |
| The emitter declares LLVM parameter widths separately from the Rust exports. For example, a timer task accepts an i128 delay and returns a handle. [ABI declarations](../src/language/runtime_abi.rs) · [ABI definitions](../src/runtime/abi.rs) | Exercise changed signatures through linked code on each affected target. Check the payload, status and handle lifetime, including on a 32-bit target. |
| Task boundaries check references and raw pointers transitively in captures and results. Native host workers still reject mutable captures, and embedded channels cannot own a token. [Task ownership](AsyncAwait.md) · [Token lifetime](EmbeddedTimers.md#token-pool-lifetime) | Check the same aggregate through function returns, task captures and channels. Validate each supported storage path through the last release, with one slot available and with the pool full. |

### A mutable parameter across a call

This program prints `7` in both the interpreter and the host binary. In an ordinary named function call, the reference to `n` keeps the caller’s storage. The native emitter loads and stores through the same LLVM slot when the binding is an integer or boolean. Passing an existing reference on to another named function keeps the original loan live in the caller. [Interpreter/native regression](../scripts/tests/test_mutable_references.py)

```prime
module example;
fn bump(n: &mut int32) { *n = *n + 1; }
fn main() {
  let mut int32 n = 6;
  bump(&mut n);
  out(n);
}
```

Save it as `example.prime` in a fresh directory and run:

```sh
prime-lang check example.prime
prime-lang run example.prime
prime-lang build example.prime --name example
./.build.prime/example/example
```

In a named function call, both `bump(r)` and `bump(&mut *r)` forward the named mutable reference `r` to its original storage. The checker counts both spellings as the same loan when checking the arguments of one call. It rejects storing an explicit reborrow in another binding, reborrowing a shared reference or raw pointer, and taking a mutable reference to a struct field or an indexed element. To replace a field, construct the updated struct and assign it through a mutable reference to the whole struct. Closure calls use a separate native calling convention.
