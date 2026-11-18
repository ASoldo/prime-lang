#!/usr/bin/env bash
# Build and exercise exactly the crate that a release would distribute.
set -euo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"
if command -v rustup >/dev/null; then
  toolchain=$(rustup show active-toolchain)
  export RUSTUP_TOOLCHAIN="${toolchain%% *}"
fi
target=$(realpath -m "${CARGO_TARGET_DIR:-target}")
export CARGO_TARGET_DIR="$target"
release_dir="$target/prime-dist"
mkdir -p "$release_dir"
scratch=$(mktemp -d "$release_dir/.candidate.XXXXXXXX")
trap 'rm -rf -- "$scratch"' EXIT
version=$(python3 -c 'import tomllib; print(tomllib.load(open("Cargo.toml", "rb"))["package"]["version"])')
host=$(rustc -vV | sed -n 's/^host: //p')
name="prime-lang-$version"
binary_bundle="$name-$host"

cargo package --locked --no-verify --target-dir "$scratch/package-target"
archive="$scratch/package-target/package/$name.crate"
tar -xzf "$archive" -C "$scratch"
cmp LICENSE "$scratch/$name/LICENSE"
if [[ -e "$scratch/$name/workspace/demos/tetris/assets" ]]; then
  echo 'Unexpected Tetris media in the release package' >&2
  exit 1
fi
cargo install --locked --path "$scratch/$name" --root "$scratch/installed" \
  --target-dir "$scratch/install-target"
mkdir "$scratch/$binary_bundle" "$scratch/consumer"
cp "$scratch/installed/bin/prime-lang" "$scratch/$binary_bundle/"
cp "$scratch/$name/LICENSE" "$scratch/$binary_bundle/"
cd "$scratch/consumer"
cli="$scratch/$binary_bundle/prime-lang"
cat > main.prime <<'PRIME'
out(6 * 7);
PRIME
printf '42\n' > expected
"$cli" check - --format json < main.prime | python3 -c \
  'import json, sys; r=json.load(sys.stdin); assert r["schema_version"] == 1 and r["outcome"] == "ok"'
"$cli" < main.prime > actual
cmp expected actual
"$cli" run - < main.prime > actual
cmp expected actual
"$cli" build - --name hello < main.prime
./.build.prime/hello/hello > actual
cmp expected actual

cd "$scratch"
mkdir dist
cp "$archive" dist/
tar -czf "dist/$binary_bundle.tar.gz" "$binary_bundle"
cd dist
sha256sum "$name.crate" "$binary_bundle.tar.gz" > SHA256SUMS
sha256sum -c SHA256SUMS
# Each verified candidate gets its own directory, including repeated builds of a version.
out=$(mktemp -d "$release_dir/$binary_bundle.XXXXXXXX")
mv -- "$name.crate" "$binary_bundle.tar.gz" SHA256SUMS "$out/"
if [[ -n ${GITHUB_OUTPUT:-} ]]; then
  printf 'directory=%s\n' "$out" >> "$GITHUB_OUTPUT"
fi
printf 'Verified release candidate in %s\n' "$out"
