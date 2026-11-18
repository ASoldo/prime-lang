# Checking a Prime program

Use `prime-lang check TARGET` to find errors before running a program. The command resolves the target, loads its imports, expands macros and checks types. It stops there. A program with a constant initializer that writes a file or a `main` that opens a window can be checked without triggering either effect. Libraries and modules without a `main` can be checked directly.

```sh
prime-lang check workspace/demos/macro/macro_demo.prime
prime-lang check demos::macro_demo --project workspace/demos/macro
prime-lang check workspace/app/main.prime --frozen --format json
```

`TARGET` accepts a source path or module name. When resolving a module from a workspace, `--project` (or `-p`) selects a member by its path or package name. The checker uses the same target settings as `run`, including the target in the manifest and the `PRIME_TARGET` fallback. It reads local dependency manifests and reuses shared dependencies. A dependency cycle reports the manifest paths that form the loop.

| Exit code | Meaning |
| --- | --- |
| `0` | The target and its imports passed all front-end checks. |
| `1` | The target, manifest, dependency or program could not be checked, or the output could not be written. |
| `2` | Invalid command-line usage, such as omitting `TARGET` or passing an unknown `--format`. |

The default `--format text` prints a short success summary on stdout or diagnostics on stderr. Diagnostics include the source path and byte range when available.

## Frozen dependencies

`check`, `run` and `build` share the same `--frozen` check. The entry package needs a readable `prime.lock`. A dependency package that declares dependencies needs its own lock. A package lock lists its direct dependencies, including their source, package selection and features. The checker accepts the older dotted dependency names in shipped locks, such as `libpkg.lib` for `libpkg::lib`.

Path dependencies must resolve to the locked directory. Their files can be edited in place. For Git dependencies, the checkout under `.prime/deps` must have the declared origin, the locked commit as its current HEAD, and a clean index and tracked working tree. The frozen check runs locally and leaves the checkout and lockfile untouched.

`prime-lang add NAME --git URL` attempts the initial fetch. `prime-lang sync` records the current HEAD of an available checkout in the package lock. For a temporarily unavailable checkout, sync keeps an existing pin for the same dependency and URL. A new Git dependency can be recorded before its first successful fetch. Frozen commands require the checkout and a pinned revision.

## JSON for tools

`--format json` writes one UTF-8 JSON object followed by a newline to stdout for both success and program or input errors. A client can parse stdout even when the command exits with `1`. Clap reports usage errors on stderr before checking begins.

For example, checking a standalone `hello.prime` containing `module hello;` from `/tmp/example` produces this report, shown with indentation.

```json
{
  "schema_version": 1,
  "outcome": "ok",
  "target": "hello.prime",
  "entry_path": "/tmp/example/hello.prime",
  "manifest_path": null,
  "paths": ["/tmp/example/hello.prime"],
  "diagnostics": []
}
```

| Field | Meaning in schema version 1 |
| --- | --- |
| `outcome` | `ok` or `error`. |
| `target` | Original target argument. |
| `entry_path` | Canonical entry file path, or `null` if resolution failed. |
| `manifest_path` | Selected manifest path, or `null` if no manifest was loaded. |
| `paths` | Sorted, unique canonical paths of the loaded source files. Empty until the entire import graph has loaded. |
| `diagnostics` | Errors from the first failing phase, sorted by path, byte range and diagnostic details. |

Each diagnostic has a `phase` (`input`, `io`, `manifest`, `syntax`, `macro` or `type`), `path`, `message`, optional-valued `code` and `help`, and a `span`. A span is either `null` or an object with `start` and `end`. Both are zero-based UTF-8 **byte offsets** in the identified file. The start is inclusive and the end is exclusive. For example, if the lexer rejects `§`, its span covers two bytes. A missing file has a path and a `null` span.
