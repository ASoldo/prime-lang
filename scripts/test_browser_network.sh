#!/usr/bin/env bash
set -euo pipefail
cli=$(realpath "${1:?usage: test_browser_network.sh PATH_TO_PRIME_LANG}")
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
chrome=${CHROMIUM:-$(command -v chromium || command -v google-chrome)}
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT

go -C adapters/rest_sse test -race ./...
python3 scripts/test_rest_sse.py "$cli" --chromium "$chrome"
python3 sdk/websocket/test_service.py "$cli" --chromium "$chrome"

go -C adapters/webrtc mod verify
go -C adapters/webrtc test -mod=readonly -race ./...
go -C adapters/webrtc build -mod=readonly -trimpath -o "$work/webrtc" .
python3 adapters/webrtc/test_browser.py "$cli" "$work/webrtc" --chromium "$chrome"
