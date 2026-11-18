# Run Prime from standard input

A quoted shell heredoc sends UTF-8 source directly to the interpreter. Quoting the delimiter preserves dollar signs, backticks and backslashes in the program.

```sh
prime-lang <<'PRIME'
out(6 * 7);
PRIME
```

This prints `42` and exits with status 0. With piped or redirected input, `prime-lang`, `prime-lang -` and `prime-lang run -` run the same source. With no arguments and a terminal for stdin, the CLI displays help. Use the explicit subcommand when supplying options.

## Check, interpret and compile the same bytes

```sh
program() {
  cat <<'PRIME'
module demo;
fn twice(x: int32) -> int32 { x + x }
fn main() { out(twice(21)); }
PRIME
}

program | prime-lang check - --format json
program | prime-lang run -
program | prime-lang build - --name answer
./.build.prime/answer/answer
```

Both executions print `42`. Run `program | prime-lang run --trace - 2>calls.ndjson` to collect the calls to `main` and `twice`. Both records identify `<stdin>` and index the function names in the heredoc. See the [trace format](Debugging.md).

The checker writes one schema-version-1 JSON object on stdout, even for a program error. Success exits with 0. Source, import and type errors exit with 1. Invalid command usage exits with 2. Ordinary interpreter and build errors go to stderr. The native build writes artifacts under the current directory's `.build.prime`. Checking and interpreting a heredoc require no source file on disk.

In the JSON report, `target` is `"-"`, and `entry_path` is `"<stdin>"` for a resolved stdin program. Diagnostics identify the original source using `<stdin>` or the actual imported file's path. `span.start` and `span.end` are zero-based, half-open byte offsets. To extract a diagnostic in Python, use `source.encode("utf-8")[start:end].decode("utf-8")`. See the [check report fields](Checking.md).

## Script or module

A header (`module`, `library`, `test`), declaration, `export` or top-level item macro invocation selects the usual module grammar. Comments and leading imports can precede a script. When the first remaining token starts a statement or expression, the parser uses an implicit `fn main()` body. It chooses once. A syntax error stays in the selected grammar. Put declarations and an explicit `fn main()` together when a program needs both declarations and executable statements.

```prime
let limit = 7;
out(limit * 6);
```

A script's final expression may omit its semicolon. Use `out` to print a value. The same rules apply to a saved `.prime` source. Try the [standalone script](../workspace/demos/inline/answer.prime).

## Relative imports and frozen packages

A stdin entry resolves relative imports from the current working directory. Manifest discovery starts there and walks up the directory tree. Imported files resolve their own relative imports from their containing directory. From a workspace, `--project` selects a member by package name or member path and uses the member directory for the stdin entry's relative imports. All three commands accept `--frozen` and validate the selected package's existing lock before loading its modules.

```sh
# In a directory containing helper.prime
prime-lang run - <<'PRIME'
import helper::{twice};
out(twice(21));
PRIME
```

## Give the program its own input

When stdin carries the source, the interpreter reads it through EOF before calling `main`. A host `in[T]` at EOF returns `Result::Err("end of input")`. Sending a blank line to `in[string]` returns `Result::Ok("")`.

To supply application data, save the program and pipe the data to the interpreter. A native program built from a heredoc also has its own stdin on every execution.

```sh
cat > reader.prime <<'PRIME'
module reader;
enum Result[T, E] { Ok(T), Err(E) }
fn main() {
  match in[string]("") {
    Result::Ok(line) => out(line),
    Result::Err(message) => out(message),
  }
}
PRIME

printf 'hello\n' | prime-lang run reader.prime
prime-lang build - --name reader < reader.prime
printf 'hello\n' | ./.build.prime/reader/reader
```
