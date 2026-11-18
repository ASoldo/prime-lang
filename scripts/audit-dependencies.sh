#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname -- "${BASH_SOURCE[0]}")/.."
cargo audit update-db

locks=(
  Cargo.lock
  src/runtime/distribution/host.Cargo.lock
  src/runtime/distribution/embedded.Cargo.lock
  tree-sitter/prime/Cargo.lock
)
status=0
for lock in "${locks[@]}"; do
  printf 'Auditing %s\n' "$lock"
  cargo audit --no-fetch --file "$lock" || status=1
done
exit "$status"
