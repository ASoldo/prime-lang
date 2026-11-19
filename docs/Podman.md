# Run Prime and Tetris with Podman

From the repository root on Linux, use rootless Podman and the `podman-compose` provider. The launcher selects that provider for `podman compose`. A player who already has the games image needs Podman, the provider and the launcher files. Rust, LLVM, Go and Python are inside the images.

```sh
./scripts/tetris build
./scripts/tetris solo
./scripts/tetris online
```

**Solo** opens a native window on the current X11 or Xwayland display. Enter starts a game, arrows move, X rotates and Esc quits. The launcher passes the display socket and a temporary single-display Xauthority file. If the session has a PulseAudio or PipeWire-Pulse socket, the game uses it for sound. For a silent run set `PRIME_AUDIO_SILENT=1`. For a machine without a display, `./scripts/tetris solo --headless` renders three frames and exits. [Full controls](../workspace/demos/tetris/README.md)

**Online** starts one native Prime ECS process and both transport adapters. Once the launcher reports ready, open the printed URL in two browser windows, choose WebSocket or WebRTC, and start the match. The default roster has two human seats and six bots. The arena holds the match in memory. A client can reclaim its seat during that process's lifetime. Starting a new container starts a new match.

```sh
./scripts/tetris status
./scripts/tetris logs
./scripts/tetris down
RIFTWELL_SEATS=99 RIFTWELL_HUMANS=2 ./scripts/tetris online
```

For a deterministic all-human match, set `RIFTWELL_CLOCK=--lockstep` and give `RIFTWELL_HUMANS` and `RIFTWELL_SEATS` the same value. Each tick advances after every live seat has submitted a move.

The default publishes only on IPv4 loopback. TCP 8000 serves the page, WebSocket and SDP signaling. UDP 8001 carries WebRTC. To choose other localhost ports set `RIFTWELL_HOST_PORT` and `RIFTWELL_ICE_UDP_PORT`. The launcher computes the HTTP origin from the selected TCP port. The game container runs with a read-only root filesystem, a private writable temporary directory and all Linux capabilities dropped. The solo container has networking disabled.

### Two machines

For a WebSocket game over SSH, leave the server on loopback. Run `ssh -NT -L 8000:127.0.0.1:8000 user@game-host` on the second machine and open `http://127.0.0.1:8000` there. Join with the WebSocket transport. The tunnel forwards TCP.

For direct WebRTC on a LAN, choose the host's reachable IPv4 address, publish the TCP and UDP ports on it and use a certificate for a name the browsers trust. In this example, replace the address, hostname and two absolute PEM paths with your own. The private key must be readable by the rootless Podman user. Both transports share the HTTPS origin.

```sh
export RIFTWELL_PUBLISH_IP=192.0.2.15
export RIFTWELL_ICE_ADVERTISE_IP="$RIFTWELL_PUBLISH_IP"
export RIFTWELL_PUBLIC_ORIGIN=https://games.example.test:8000
export RIFTWELL_TLS_CERT="$HOME/.config/riftwell/server.crt"
export RIFTWELL_TLS_KEY="$HOME/.config/riftwell/server.key"
./scripts/tetris online
```

The listener advertises the configured host address and UDP port to ICE. Routing, DNS and client trust must reach that address. The separate [WebRTC SDK](WebRTC.md) documents TURN for networks that require a relay. [Arena protocol and replay](../workspace/demos/field_arena/README.md)

### Develop another Prime application

Build the SDK target and send a complete Prime program through stdin. The SDK contains the compiler and host runtime toolchain. The games image contains the compiled programs.

```sh
podman build -f deploy/podman/Containerfile --target sdk -t localhost/prime-sdk:dev .
printf 'out(6 * 7);\n' | podman run --rm -i --network none localhost/prime-sdk:dev
```

To save a native executable, run the build with a writable project directory mounted at `/work`. The output is an ELF for the image architecture and runtime environment.

```sh
mkdir -p sample
printf 'out(6 * 7);\n' > sample/main.prime
podman run --rm --network none --userns keep-id -v "$PWD/sample:/work:Z" \
  localhost/prime-sdk:dev build main.prime --name answer
podman run --rm --network none -v "$PWD/sample:/work:ro,Z" \
  --entrypoint /work/.build.prime/answer/answer localhost/prime-sdk:dev
```

The SDK can also check a service against an independent peer. The repository's socket example exchanges binary TCP and UDP data with Python on container loopback. The same command tests the interpreter and the compiled service using ephemeral ports.

```sh
podman run --rm --network none --userns keep-id --read-only \
  --tmpfs /tmp:rw,exec,mode=1777 -e CARGO_TARGET_DIR=/tmp/prime-cache \
  -v "$PWD/scripts:/qa:ro" -w /tmp --entrypoint /usr/bin/python3 \
  localhost/prime-sdk:dev /qa/test_host_sockets.py /usr/local/bin/prime-lang
```

### Transfer a local build

Use an image built for the destination architecture. For a private transfer, `podman save -o prime-games.tar localhost/prime-games:dev` on the build host and `podman load -i prime-games.tar` on the player host. Copy the matching `compose.yaml`, `scripts/tetris` and `deploy/podman/` as well. Set `PRIME_GAMES_IMAGE` to choose a different local tag. Keep the games image local or transfer it privately between your own machines while redistribution rights for the demo media are being confirmed. [Project license and media](../README.md#license)
