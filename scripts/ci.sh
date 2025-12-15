#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$root_dir"

echo "==> cargo fmt --check"
cargo fmt --all -- --check

echo "==> cargo clippy (-D warnings)"
cargo clippy --all-targets --all-features -- -D warnings

echo "==> cargo test (single-threaded, timeboxed)"
timeout -k 10s 600s cargo test --locked --all -- --test-threads=1

echo "==> docs generation smoke test"
tmp_out="$(mktemp -t prime-docs.XXXXXX.html)"
trap 'rm -f "$tmp_out"' EXIT
cargo run -q --bin prime-lang -- docs --generate --out "$tmp_out"
grep -q "const data = \\[" "$tmp_out"

echo "CI checks passed"
