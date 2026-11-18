# Prime Tree-sitter grammar

The generated parser exports `tree_sitter_prime()` and uses Tree-sitter language ABI 15. The Rust crate exposes this function as `tree_sitter_prime_lang::LANGUAGE`.

## Validate the grammar

Run the CI check from the Prime repository root. It needs Cargo and a C compiler.

```sh
cargo test --locked --manifest-path tree-sitter/prime/Cargo.toml -- --nocapture
```

The Rust binding tests parse valid and malformed Prime and check that `()` has different syntax nodes in a return type and a function body. The `workspace_corpus` test recursively discovers every `.prime` file under `workspace/`, including formatter fixtures. It prints the number of files parsed and reports the first error or missing token in each failing file, with its line and column. These checks cover syntax, so examples containing intentional type or borrow errors also belong in the corpus.

## Regenerate the parser

From the repository root, use Node.js and Tree-sitter CLI 0.25.10 to rebuild the checked-in C parser and syntax metadata after editing `grammar.js`.

```sh
cd tree-sitter/prime
tree-sitter generate --abi 15
```

Run the CI check again to test the regenerated parser.
