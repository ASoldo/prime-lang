# Networking in Prime

Start with the transport your peer speaks. Prime programs use the same entry point with `prime-lang run` and `prime-lang build`.

| Peer or application | Prime interface | Implementation and limits |
| --- | --- | --- |
| TCP stream or UDP datagram | [Host sockets](HostSockets.md) | The std runtime provides binary I/O, address lookup on open handles, deadlines and explicit close. Endpoints are numeric IPv4 or IPv6 addresses. |
| Small loopback HTTP server | [Prime HTTP demo](LoopbackWebServer.md) | A Prime program chooses the frame delimiter and routes complete requests. Up to four connections can have headers in progress. |
| REST client or EventSource | [REST and SSE](RestSse.md) | Prime owns the reading and revision. A Go HTTP/1.1 adapter handles requests, JSON and event streams. A subscriber can resume within the last 16 revisions. |
| Browser WebSocket | [WebSocket SDK](../sdk/websocket/README.md) | A Python host adapter delivers text, binary and close events to a Prime process. The peer cap is configurable. Frames can arrive fragmented. |
| Browser RTCPeerConnection | [WebRTC SDK](WebRTC.md) | A Pion host adapter negotiates ICE, DTLS and SCTP. The application receives data-channel events and can address an individual peer. |
| Classic ESP32 with ESP-IDF | [Prime component using lwIP](EspIdfSockets.md) | The Prime application exchanges TCP and UDP bytes between local endpoints under FreeRTOS. |

The WebSocket and WebRTC adapters use the same application protocol. Each incoming event has three lines containing its kind, peer ID and payload. The Prime process can issue a send or close command independently of incoming events. Each new connection receives a transport peer ID. A game that supports reconnecting tracks the player's seat separately from this ID. Keep a WebSocket application message within 4096 decoded bytes when sharing code between the two transports.

The host socket builtins require std. An embedded program needs its target SDK to supply the network stack and bring up the interface. The host browser adapters run as separate processes. Their dependencies are installed when building those examples.

## Running the browser suite

On Linux, install Go 1.24 or later, Node.js 18 or later, Python with venv, and matching Chromium and ChromeDriver executables. The browser suite uses loopback and ephemeral ports. The default WebRTC example uses host ICE candidates. The first build needs access to the pinned Go and Python dependencies, or a prepared local cache.

```sh
cargo build --locked --bin prime-lang
python3 -m venv .venv-network
.venv-network/bin/pip install -r sdk/websocket/requirements-test.txt
PATH="$PWD/.venv-network/bin:$PATH" ./scripts/test_browser_network.sh target/debug/prime-lang
```

Set `CHROMIUM` to select a browser executable and place its matching `chromedriver` on `PATH`. Each adapter's guide includes commands to start its example independently.
