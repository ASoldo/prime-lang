#!/usr/bin/env bash
# Accept a prebuilt local games image using the same entry points as a player.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
launcher=./scripts/tetris
export COMPOSE_PROJECT_NAME="prime-game-test-$$"
export RIFTWELL_SEATS=2 RIFTWELL_HUMANS=2 RIFTWELL_TICK_MS=0
export RIFTWELL_PUBLISH_IP=127.0.0.1 RIFTWELL_ICE_ADVERTISE_IP=127.0.0.1
unset RIFTWELL_TLS_CERT RIFTWELL_TLS_KEY RIFTWELL_CLOCK
# Ask Linux for available host ports so development sessions can stay running.
read -r RIFTWELL_HOST_PORT RIFTWELL_ICE_UDP_PORT < <(python3 - <<'PY'
import socket
with socket.socket() as tcp, socket.socket(type=socket.SOCK_DGRAM) as udp:
    tcp.bind(("127.0.0.1", 0))
    udp.bind(("127.0.0.1", 0))
    print(tcp.getsockname()[1], udp.getsockname()[1])
PY
)
export RIFTWELL_HOST_PORT RIFTWELL_ICE_UDP_PORT
export RIFTWELL_PUBLIC_ORIGIN="http://127.0.0.1:$RIFTWELL_HOST_PORT"
match_id() {
    curl --noproxy '*' --fail --silent --show-error "$RIFTWELL_PUBLIC_ORIGIN/config" |
        python3 -c 'import json,sys; print(json.load(sys.stdin)["match"])'
}

"$launcher" solo --headless
trap '"$launcher" down' EXIT
"$launcher" online
first=$(match_id)
python3 workspace/demos/field_arena/test_network.py --origin "$RIFTWELL_PUBLIC_ORIGIN" \
    --ice-ip "$RIFTWELL_ICE_ADVERTISE_IP" --ice-port "$RIFTWELL_ICE_UDP_PORT"
"$launcher" down
if curl --noproxy '*' --fail --silent --connect-timeout 2 "$RIFTWELL_PUBLIC_ORIGIN/healthz" >/dev/null; then
    echo 'The game listener survived compose down.' >&2
    exit 1
fi
export RIFTWELL_SEATS=99 RIFTWELL_HUMANS=99 RIFTWELL_SEED=137 RIFTWELL_CLOCK=--lockstep
"$launcher" online
second=$(match_id)
[[ -n $first && -n $second && $first != "$second" ]] || { echo 'Expected a fresh match after restart.' >&2; exit 1; }
python3 workspace/demos/field_arena/test_roster.py --origin "$RIFTWELL_PUBLIC_ORIGIN"
echo 'Podman accepted solo frames, two transports, graceful stop and a fresh 99-player match.'
