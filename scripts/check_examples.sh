#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
target_dir=${CARGO_TARGET_DIR:-target}
[[ "$target_dir" = /* ]] || target_dir="$root_dir/$target_dir"
if [[ ${PRIME_BIN+x} ]]; then
  cli="$PRIME_BIN"
elif [[ -x "$target_dir/debug/prime-lang" && -f "$target_dir/debug/prime-lang" ]]; then
  cli="$target_dir/debug/prime-lang"
elif [[ -x "$target_dir/release/prime-lang" && -f "$target_dir/release/prime-lang" ]]; then
  cli="$target_dir/release/prime-lang"
else
  cli=prime-lang
fi
if ! cli_path="$(type -P -- "$cli")" || [[ ! -f "$cli_path" || ! -x "$cli_path" ]]; then
  echo "Cannot execute '${cli}'. Build prime-lang with cargo build or set PRIME_BIN." >&2
  exit 1
fi
# Resolve relative overrides and PATH entries before changing directory.
[[ "$cli_path" = /* ]] || cli_path="$PWD/$cli_path"
cd "$root_dir"

inventory="$(mktemp)"
trap 'rm -f -- "$inventory"' EXIT
python3 - "$inventory" <<'PY'
import os
from pathlib import Path
import sys

try:
    import tomllib
except ModuleNotFoundError:
    sys.exit("check_examples.sh requires Python 3.11 or newer for TOML support")


def entries(data):
    if "module" in data:
        yield data["module"]
    for section in ("modules", "libraries", "tests"):
        group = data.get(section, [])
        if isinstance(group, dict):
            for key, value in group.items():
                if key != "items":
                    yield {"path": value} if isinstance(value, str) else value
            group = group.get("items", [])
        if not isinstance(group, list):
            raise ValueError(f"{section} entries must form a table or array")
        yield from group


def raise_walk_error(err):
    raise err


try:
    manifests = sorted(Path(folder) / "prime.toml"
                       for folder, _, names in os.walk("workspace", onerror=raise_walk_error)
                       if "prime.toml" in names)
    if not manifests:
        sys.exit("No prime.toml manifests found under workspace")
    files = set()
    for manifest in manifests:
        try:
            with manifest.open("rb") as stream:
                declarations = list(entries(tomllib.load(stream)))
            if not declarations:
                raise ValueError("no module, library or test declarations")
            for entry in declarations:
                path = entry.get("path") if isinstance(entry, dict) else None
                if not isinstance(path, str) or Path(path).suffix != ".prime":
                    raise ValueError(f"expected a .prime source path, got {path!r}")
                source = (manifest.parent / path).resolve(strict=True)
                if not source.is_file():
                    raise ValueError(f"expected a source file at {source}")
                files.add(source)
        except (OSError, ValueError) as err:
            sys.exit(f"{manifest}: {err}")
    with open(sys.argv[1], "wb") as inventory:
        for source in sorted(files):
            inventory.write(os.fsencode(source) + b"\0")
    print(f"Linting {len(files)} example modules from {len(manifests)} manifests")
except OSError as err:
    sys.exit(str(err))
PY

while IFS= read -r -d '' file <&3; do
  echo "• lint $file"
  "$cli_path" lint "$file"
  "$cli_path" check "$file"
  if [[ "${PRIME_RUN_EXAMPLES:-0}" == "1" ]]; then
    echo "  run $file"
    "$cli_path" run "$file"
  fi
done 3<"$inventory"

echo "All example modules linted and type-checked successfully"
