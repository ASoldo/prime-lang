# Riftwell on a rootless Podman bridge

Set `RIFTWELL_PUBLIC_ORIGIN` to publish the page, WebSocket at `/ws`, and WebRTC signaling at `/offer` through one TCP port. The adapters keep their private listeners on container loopback. A browser on another computer connects to the Podman host's address. Use an IPv4 address on a network both players are allowed to reach.

| Environment variable | Launcher option | Example |
| --- | --- | --- |
| `RIFTWELL_BIND` | `--bind` | `0.0.0.0` inside the container |
| `RIFTWELL_PORT` | `--port` | `8000` inside the container |
| `RIFTWELL_PUBLIC_ORIGIN` | `--public-origin` | `https://game.lan:8443` as opened by the browser |
| `RIFTWELL_TLS_CERT`, `RIFTWELL_TLS_KEY` | `--tls-cert`, `--tls-key` | Readable PEM certificate chain and private key paths inside the container |
| `RIFTWELL_ICE_UDP_PORT` | `--ice-udp-port` | `8001` for all RTC peers |
| `RIFTWELL_ICE_ADVERTISE_IP` | `--ice-advertise-ip` | Podman host's reachable IPv4 address for RTC |

For example, suppose the Podman host owns `192.168.1.20` and a permitted browser resolves `game.lan` to that address. Configure the environment as above, mount a certificate trusted by that browser for `game.lan`, and use these Podman port mappings on the bridge:

```sh
-p 192.168.1.20:8443:8000/tcp -p 192.168.1.20:8001:8001/udp
```

Open `https://game.lan:8443/` for WebSocket or `https://game.lan:8443/?transport=rtc` for WebRTC. `/config` advertises `wss://game.lan:8443/ws`. Remote browsers need HTTPS for the Web Crypto calls that generate seat tokens and verify frame checksums. For a browser on the Podman host, loopback HTTP also provides a secure context. A TLS-terminating reverse proxy can forward HTTP/1.1, WebSocket upgrades and the original Host header to the arena. Set the public origin to the proxy's HTTPS origin and omit the launcher's TLS file options in that setup.

The public origin must match the browser's serialized origin exactly. Use a lowercase hostname, include a non-default public TCP port, and omit any path or trailing slash. The launcher checks Host on page, health and signaling requests. Signaling and the published WebSocket also require that exact Origin. A container-local health probe can request `/healthz` with the public Host header. It returns an empty 200 while the Prime engine and selected adapters are running.

Enable the RTC adapter with `--rtc-bin` (or `--webrtc` in the play script). The fixed-port profile uses ICE-lite with UDP/IPv4. Publish the same UDP port on the host and container. Set the advertised IP when clients cannot route to the bridge address. Without an override Pion advertises the container interface address. This direct profile needs a UDP path from browser to published host port. A WS-only match needs only TCP. Omitting the deployment options retains the local launcher, ephemeral WS port and loopback ICE defaults.

## Acceptance check

Start a fresh match on the bridge with both adapters, two human seats, and `--tick-ms 0`. On a machine outside the container namespace install `sdk/websocket/requirements-test.txt` in a Python environment and put Chromium and its matching chromedriver on PATH. With the example addresses above run:

```sh
python workspace/demos/field_arena/test_network.py \
  --origin https://game.lan:8443 --ice-ip 192.168.1.20 --ice-port 8001
```

The test checks health, Host and Origin handling, seats one tab on each transport, exchanges a move from each, and compares their verified frame hashes. It reads Chromium's selected ICE candidate pair and checks the remote UDP address and port. For a private CA, trust it in Chromium and pass `--ca-file CA.pem` to the test for its HTTP checks. Restart the match before repeating on the other Prime backend.

A two-computer check used an AArch64 rootless Podman bridge and Chromium on a second x86-64 LAN host. The browser reached TCP port 18000 forwarded to container port 8000 and selected the advertised host IPv4 address on UDP port 18001. With seed 17 and two human seats, the interpreter and native runs both produced tick-2 SHA-256 `fa420f00244b437c6ad36167a755572484a7d64ddeb6fad29cbc04033815b2d8`.
