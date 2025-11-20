pub struct Topic {
    pub key: &'static str,
    pub title: &'static str,
    pub summary: &'static str,
    pub category: &'static str,
    pub aliases: &'static [&'static str],
    pub sections: &'static [TopicSection],
}

impl Topic {
    pub fn all_keys(&self) -> impl Iterator<Item = &str> {
        std::iter::once(self.key).chain(self.aliases.iter().copied())
    }
}

pub struct TopicSection {
    pub title: &'static str,
    pub snippet: &'static str,
    pub explanation: &'static str,
}

pub const TOPICS: &[Topic] = &[
    Topic {
        key: "core-syntax",
        title: "Prime Core Syntax",
        category: "Core Syntax",
        summary: "Module headers, imports, bindings, and struct declarations live at the heart of every `.prime` file. These constructs are parsed into the AST in `src/language/ast.rs` before the compiler or interpreter sees them.",
        aliases: &[
            "core",
            "syntax",
            "module",
            "modules",
            "import",
            "imports",
            "const",
            "constants",
            "let",
            "binding",
            "bindings",
            "fn",
            "function",
            "functions",
            "declaration",
            "declarations",
        ],
        sections: &[
            TopicSection {
                title: "Module headers & imports",
                snippet: r#"module app::main;

import core::types;
import demos::error_handling;"#,
                explanation: "Every file begins with `module path::to::module;`. Imports use the same canonical segments, allowing the manifest loader (`project::package`) and the LSP to resolve dependencies unambiguously.",
            },
            TopicSection {
                title: "Bindings, constants, and functions",
                snippet: r#"const TRAINING_LIMIT: int32 = 3;

fn divmod(a: int32, b: int32) -> (int32, int32) {
  (a / b, a % b)
}

fn assign_example() {
  let int32 value = 5;
  let mut int32 counter = value + 1;
  counter = counter + 2;
}"#,
                explanation: "`const` introduces immutable globals, `let` bindings are immutable unless `mut` is specified, and function signatures list each parameter plus tuple returns. The typechecker enforces the declared shapes before interpretation or code generation.",
            },
            TopicSection {
                title: "Struct declarations",
                snippet: r#"struct Vec2 {
  x: float32;
  y: float32;
}

struct Player {
  name: string;
  level: int32;
  stats: Stats;
}"#,
                explanation: "Struct layouts mirror the syntax in `types.prime`. Embedded structs flatten into their parents, so code such as `player.stats.hp` works without inheritance. Patterns reuse the same syntax with `Struct{ field: binding, .. }`.",
            },
        ],
    },
    Topic {
        key: "prime-basics",
        title: "Prime Basics: CLI & Workspace",
        category: "Basics",
        summary: "The `prime-lang` binary exposes subcommands for running, building, linting, formatting, initializing, adding modules, and printing these docs. Workspace metadata lives in `prime.toml`.",
        aliases: &[
            "basics",
            "cli",
            "command",
            "commands",
            "run",
            "build",
            "lint",
            "fmt",
            "format",
            "docs",
            "init",
            "add",
            "help",
            "workspace",
            "prime",
            "manifest",
            "prime.toml",
            "modules",
            "module-entries",
        ],
        sections: &[
            TopicSection {
                title: "Essential CLI commands",
                snippet: r#"$ prime-lang run main.prime
$ prime-lang build main.prime --name demo
$ prime-lang lint main.prime --watch
$ prime-lang fmt main.prime --write
$ prime-lang lsp
$ prime-lang init adventure
$ prime-lang add demos::new --path demos/new_demo.prime
$ prime-lang docs --query core-syntax"#,
                explanation: "Each subcommand is wired up in `src/main.rs` via clap. `run` interprets a file after loading the manifest, `build` emits LLVM + native binaries, `lint`/`fmt` offer single-shot or watch modes, `lsp` serves editors, `init` scaffolds a workspace, `add` appends a manifest entry and stub file, and `docs` prints these references.",
            },
            TopicSection {
                title: "prime.toml manifest",
                snippet: r#"manifest_version = "2"

[package]
entry = "app::main"
kind = "binary"
name = "prime-lang-playground"
version = "0.1.0"

[[modules]]
name = "app::main"
path = "main.prime"
visibility = "pub""#,
                explanation: "`project::manifest::PackageManifest` reads `prime.toml`, mapping module names to canonical file paths. Entries must match each file's `module ...;` header so `load_package` can pull the correct dependencies before typechecking or interpretation.",
            },
            TopicSection {
                title: "Module discovery & add",
                snippet: r#"$ prime-lang add demos::patterns --path pattern_demo.prime --visibility pub
# creates the manifest entry and stubs a module with `module demos::patterns;`
"#,
                explanation: "`prime-lang add` edits the manifest and writes a stub file, ensuring new modules stay in sync with the manifest graph. This keeps the CLI, interpreter, compiler, and LSP aligned about available modules.",
            },
        ],
    },
    Topic {
        key: "prime-intermediate",
        title: "Prime Intermediate: Control Flow & Patterns",
        category: "Intermediate",
        summary: "Control-flow constructs (`if`, `match`, loops, destructuring) show up throughout `pattern_demo.prime` and `error_handling_demo.prime`. They are validated by the typechecker before runtime.",
        aliases: &[
            "intermediate",
            "control-flow",
            "if",
            "else",
            "if let",
            "match",
            "pattern",
            "patterns",
            "switch",
            "for",
            "loop",
            "loops",
            "while",
            "while let",
            "destructure",
            "guard",
        ],
        sections: &[
            TopicSection {
                title: "Branching with multiple returns",
                snippet: r#"fn span_measurements(seed: int32) -> (int32, int32) {
  if seed < 0 {
    return 0, 0;
  }
  if seed > 100 && seed < 150 {
    return seed - 10, seed + 10;
  } else if seed >= 150 {
    return seed - 5, seed + 5;
  }
  let int32 midpoint = seed / 2;
  return midpoint - 5, midpoint + 5;
}"#,
                explanation: "Standard `if`/`else if` chains allow early `return` statements. The typechecker ensures every path returns the `(int32, int32)` tuple promised in the signature.",
            },
            TopicSection {
                title: "Pattern matching",
                snippet: r#"fn describe_reading(reading: (string, int32)) {
  match reading {
    ("temp", value) => out(`temperature reading {value}`),
    ("status", code) => out(`status code {code}`),
    _ => out("unrecognized reading"),
  }
}

fn summarize_stats(stats: Map[string, int32]) {
  match stats {
    #{ "hp": hp, "mp": mp } => out(`stats ready hp {hp} mp {mp}`),
    _ => out("missing stats"),
  }
}"#,
                explanation: "`match` supports tuples, maps, enums, structs, and slice patterns. Guards (`if condition`) refine matches, and every arm must cover the declared type. Destructuring binds the needed fields directly in each arm.",
            },
            TopicSection {
                title: "Loops & iteration",
                snippet: r#"fn loop_merging() -> int32 {
  let mut int32 total = 0;
  for count in 0..2 {
    let &mut int32 alias = &mut total;
    *alias = *alias + count;
  }
  let []int32 entries = [3, 4];
  for entry in entries {
    let &mut int32 alias = &mut total;
    *alias = *alias + entry;
  }
  let []string notes = ["ok", "stable"];
  let mut int32 idx = 0;
  while let Some(note) = notes.get(idx) {
    out(`note {idx}: {note}`);
    idx = idx + 1;
  }
  total
}"#,
                explanation: "Range loops (`start..end`) and slice iteration move values directly into the loop binding. `while let` keeps looping as long as a pattern binds successfully. Borrow checking (see advanced topic) guarantees each body finishes before the next borrow.",
            },
        ],
    },
    Topic {
        key: "prime-advanced",
        title: "Prime Advanced: Traits, Generics, Borrowing",
        category: "Advanced",
        summary: "Advanced features include the borrow checker, generics, interfaces, heap helpers, and `Result`-based error propagation. Examples live in `borrow_demo.prime`, `generic_demo.prime`, `interface_generics_demo.prime`, `heap_demo.prime`, and `error_handling_demo.prime`.",
        aliases: &[
            "advanced",
            "generics",
            "generic",
            "interface",
            "interfaces",
            "trait",
            "traits",
            "impl",
            "implement",
            "borrow",
            "borrowing",
            "mut",
            "refs",
            "references",
            "error",
            "errors",
            "result",
            "try",
            "heap",
            "box",
            "slice",
            "map",
        ],
        sections: &[
            TopicSection {
                title: "Borrow checker scenarios",
                snippet: r#"fn alias_mut() {
  let mut int32 value = 10;
  let int32 copy = value;
  let &mut int32 first_ref = &mut value;
  *first_ref = copy + 5;
  let int32 final_value = *first_ref;
  out(`alias_mut copy {copy} final {}`, final_value);
}"#,
                explanation: "Mutable borrows (`&mut`) must be unique. The typechecker (`language::typecheck`) tracks lifetimes so each reference ends before the next borrow begins, even across loops and branches.",
            },
            TopicSection {
                title: "Generics & duplication",
                snippet: r#"fn identity[T](value: T) -> T { value }

fn duplicate[T](value: T) -> (T, T) {
  (value, value)
}

let pair = duplicate[string]("Prime Hero");"#,
                explanation: "Type parameters appear inside square brackets and can be inferred or specified explicitly. The compiler monomorphizes these functions so they work uniformly for structs, scalars, or user-defined types.",
            },
            TopicSection {
                title: "Interfaces and implementations",
                snippet: r#"interface Nameable[T] {
  fn label(self: T) -> string;
  fn pair(self: T, other: T) -> string;
}

impl Nameable[Hero] for Hero {
  fn label(hero: Hero) -> string { hero.name }
  fn pair(_hero: Hero, other: Hero) -> string { other.name }
}"#,
                explanation: "Interfaces behave like traits. `impl` blocks attach behaviors to structs so code can call `hero.label()` or `hero.pair(partner)` directly. Generic interfaces (`Nameable[T]`) let disparate structs share the same contract.",
            },
            TopicSection {
                title: "Error propagation and heap helpers",
                snippet: r#"fn apply_boost(value: int32) -> Result[int32, string] {
  let int32 parsed = parse_energy(value)?;
  if parsed % 2 == 0 {
    Ok(parsed * 2)
  } else {
    Err("boost requires even energy")
  }
}

let Box[int32] counter = box_new(0);
bump_counter(&counter, 5);
out(`box counter -> {}`, counter.box_get());"#,
                explanation: "`Result[T, E]` plus the `?` operator short-circuits on failure, and `try { ... }` blocks wrap multi-expression workflows. Import `core::types` to bring the `Result`/`Option` enums (and variants like `Ok`, `Err`, `Some`, `None`) into scope. Heap primitives such as `Box`, slices (`[]T`), and maps (`Map[K, V]`) provide helper methods like `.box_get()`, `.len()`, and `.get(key)` for safe ownership transfers.",
            },
        ],
    },
];
