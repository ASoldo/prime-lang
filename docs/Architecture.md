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
| Host and embedded code branch on the declared Result tag and join the exiting paths at the enclosing function or `try`. The host and RV32 regressions exercise both paths in one binary and check cleanup order. [Host regression](../tests/filesystem_cli.rs) · [RV32 ELF regression](../scripts/test_c3_strings.py) · [Codegen diagnostics](../tests/check_cli.rs) | Compile once, exercise both variants with different runtime inputs, and compare return values and subsequent side effects with the interpreter. Unsupported cases should report the source of the operation. |
| The emitter declares LLVM parameter widths separately from the Rust exports. For example, a timer task accepts an i128 delay and returns a handle. [ABI declarations](../src/language/runtime_abi.rs) · [ABI definitions](../src/runtime/abi.rs) | Exercise changed signatures through linked code on each affected target. Check the payload, status and handle lifetime, including on a 32-bit target. |
| Task boundaries check references and raw pointers transitively in captures and results. Native host workers still reject mutable captures, and embedded channels cannot own a token. [Task ownership](AsyncAwait.md) · [Token lifetime](EmbeddedTimers.md#token-pool-lifetime) | Check the same aggregate through function returns, task captures and channels. Validate each supported storage path through the last release, with one slot available and with the pool full. |

### A mutable parameter across a call

This program prints `7` in both the interpreter and the host binary. The argument selects the original field, so both named functions update the same storage. [Interpreter/native regression](../scripts/tests/test_mutable_references.py)

```prime
module example;
struct Pair { value: int32; other: int32; }
fn bump(n: &mut int32) { *n = *n + 1; }
fn relay(n: &mut int32) { bump(&mut *n); }
fn main() {
  let mut Pair p = Pair{6, 9};
  relay(&mut p.value);
  out(p.value);
}
```

Save it as `example.prime` in a fresh directory and run:

```sh
prime-lang check example.prime
prime-lang run example.prime
prime-lang build example.prime --name example
./.build.prime/example/example
```

The interpreter moves a field or slice element into a stable cell on its first borrow. Copying a struct copies its field values. The LLVM emitter allocates slots for borrowed scalar fields on entry to the enclosing function and initializes them when it binds the mutable struct. Reading the struct loads those fields. For a host slice or array, the native reference carries the collection handle and the index evaluated at the borrow. A bounds check runs before the callee starts, including when the callee never dereferences its parameter. Reads and writes use the same handle and index through named calls and reborrows. An embedded fixed array occupies a contiguous LLVM allocation. A checked index selects its element address before a named call. The RV32 regression executes both field and indexed updates in a linked ESP32-C3 program in Unicorn. [RV32 regression](../scripts/test_c3_strings.py)
