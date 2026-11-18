#!/usr/bin/env bash
# Start a local match with one Prime process owning every well.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
demo="$root/workspace/demos/field_arena"
entry="$root/workspace/demos/tetris_arena/main.prime"
mode=native
rtc=0
args=()
for arg in "$@"; do
  case "$arg" in
    --interpret) mode=interpreter ;;
    --webrtc) rtc=1 ;;
    -h|--help)
      echo 'Usage: play_riftwell.sh [--interpret] [--webrtc] [--port PORT] [--seats 2..99] [--humans N] [--seed N] [--tick-ms N]'
      echo 'Defaults: native Prime, WebSocket, 8 seats, 2 human seats, port 8000. --webrtc adds a second transport.'
      exit 0 ;;
    *) args+=("$arg") ;;
  esac
done
cli=$(command -v "${PRIME_BIN:-prime-lang}") || { echo 'Install prime-lang or set PRIME_BIN to its executable.' >&2; exit 1; }
cli=$(realpath "$cli")
python3 -c 'import websockets.asyncio.server' || { echo 'Install sdk/websocket/requirements.txt in the active Python environment.' >&2; exit 1; }
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
cd "$work"
options=(--ws-service "$root/sdk/websocket/service.py")
if (( rtc )); then
  go -C "$root/adapters/webrtc" build -mod=readonly -trimpath -o "$work/webrtc" .
  options+=(--rtc-bin "$work/webrtc")
fi
if [[ $mode == native ]]; then
  "$cli" build "$entry" --name riftwell
  engine=("$work/.build.prime/riftwell/riftwell")
else
  engine=("$cli" run "$entry")
fi
child=
stop() {
  [[ -z $child ]] || kill -TERM "$child" 2>/dev/null || true
}
trap stop INT TERM
python3 "$demo/arena.py" "${options[@]}" "${args[@]}" -- "${engine[@]}" &
child=$!
# A trapped signal interrupts wait before the Python launcher has reaped its children.
while kill -0 "$child" 2>/dev/null; do
  if wait "$child"; then exit 0; else status=$?; fi
  if ! kill -0 "$child" 2>/dev/null; then exit "$status"; fi
done
wait "$child"
