#!/usr/bin/env bash
set -euo pipefail

root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
CARGO_TARGET_DIR=$(realpath -m "${CARGO_TARGET_DIR:-target}")
export CARGO_TARGET_DIR
# Child Cargo invocations also run in temporary Prime projects outside this checkout.
if command -v rustup >/dev/null; then
  toolchain=$(rustup show active-toolchain)
  export RUSTUP_TOOLCHAIN="${toolchain%% *}"
fi
scratch=$(mktemp -d)
trap 'rm -rf -- "$scratch"' EXIT

cargo fmt --all -- --check
cargo fmt --manifest-path tree-sitter/prime/Cargo.toml -- --check
cargo clippy --locked --all-targets --all-features -- -D warnings
cargo check --locked --no-default-features
cargo build --locked --bin prime-lang
export PRIME_BIN="$CARGO_TARGET_DIR/debug/prime-lang"
env "CARGO_BIN_EXE_prime-lang=$PRIME_BIN" cargo test --locked --all -- --test-threads=1
cargo test --manifest-path tree-sitter/prime/Cargo.toml --locked -- --nocapture
python3 scripts/tests/test_lint.py -v
./scripts/check_examples.sh

# Some Prime tests write files relative to their working directory.
cp -R workspace prime.toml "$scratch/"
cp "$PRIME_BIN" "$scratch/prime-lang"
cd "$scratch"
export PRIME_TEST_INPUTS='21|abc|true|maybe|98.6|nope|Prime|Y|200|42|-1|500|70000|1000000|128|3.14|badf'
./prime-lang test </dev/null
./prime-lang docs --generate --out docs.html
grep -Fq 'const data = [' docs.html

export PRIME_AUDIO_SILENT=1 PRIME_GFX_HEADLESS=1 PRIME_GFX_MAX_FRAMES=3
entry=workspace/demos/tetris/tetris.prime
./prime-lang check "$entry" --format json
./prime-lang run "$entry"
./prime-lang build "$entry" --name prime_tetris
./.build.prime/prime_tetris/prime_tetris
