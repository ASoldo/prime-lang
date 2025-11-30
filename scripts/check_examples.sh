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

mapfile -t manifests < <(find workspace -name prime.toml -print | sort)

declare -A seen
files=()
for manifest in "${manifests[@]}"; do
  dir="$(cd -- "$(dirname -- "$manifest")" && pwd)"
  while IFS= read -r path; do
    abs="${dir}/${path}"
    if [[ -f "$abs" && -z "${seen[$abs]:-}" ]]; then
      files+=("$abs")
      seen[$abs]=1
    fi
  done < <(rg -o 'path = "([^"]+\\.prime)"' -r '$1' "$manifest" || true)
done

echo "Linting ${#files[@]} example modules from ${#manifests[@]} manifests"
for file in "${files[@]}"; do
  echo "• lint ${file}"
  prime-lang lint "$file"
  if [[ "${PRIME_RUN_EXAMPLES:-0}" == "1" ]]; then
    echo "  run ${file}"
    prime-lang run "$file"
  fi
done

echo "All example modules linted successfully"
