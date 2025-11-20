#!/usr/bin/env bash
set -euo pipefail

# Lints every module declared in prime.toml. Optionally run each entry if
# PRIME_RUN_EXAMPLES=1 is set in the environment.

root_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$root_dir"

if ! command -v prime-lang >/dev/null 2>&1; then
  echo "prime-lang not found on PATH; build/install the CLI first (cargo install --path .)" >&2
  exit 1
fi

mapfile -t files < <(rg -o 'path = "[^"]+"' prime.toml | sed -E 's/path = "([^"]+)"/\1/')

echo "Linting ${#files[@]} example modules from prime.toml"
for file in "${files[@]}"; do
  echo "• lint ${file}"
  prime-lang lint "$file"
  if [[ "${PRIME_RUN_EXAMPLES:-0}" == "1" ]]; then
    echo "  run ${file}"
    prime-lang run "$file"
  fi
done

echo "All example modules linted successfully"
