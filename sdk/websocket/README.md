# Loopback WebSocket SDK for Prime

One host process runs the Prime application for all connected peers. The Python adapter owns the IPv4 loopback listener, RFC 6455 opening and closing handshakes, framing, UTF-8 validation and control frames via the [websockets asyncio server](https://websockets.readthedocs.io/en/stable/reference/asyncio/server.html). The Prime application receives complete messages, chooses recipients and produces the response bytes.

## Start the echo demo

From the repository root on Linux, with Python 3.11+ and a host Prime CLI on PATH, run the following commands.

```sh
python3 -m venv /tmp/prime-ws-venv
. /tmp/prime-ws-venv/bin/activate
pip install -r sdk/websocket/requirements.txt
python sdk/websocket/service.py --port 8765 --origin http://127.0.0.1:8000 -- prime-lang run sdk/websocket/echo.prime
```

For native execution, build and launch the same source.

```sh
prime-lang build sdk/websocket/echo.prime --name ws_echo
python sdk/websocket/service.py --port 8765 --origin http://127.0.0.1:8000 -- ./.build.prime/ws_echo/ws_echo
```

The adapter prints `LISTEN <port>`. Omit `--port` for an OS-assigned port. A client without an Origin header can connect by default. Pass each allowed browser page origin in a separate `--origin` argument. A local page served from the example origin can run this in its developer console.

```js
const ws = new WebSocket("ws://127.0.0.1:8765/");
ws.binaryType = "arraybuffer";
ws.onmessage = ({data}) => console.log(typeof data === "string" ? data : new Uint8Array(data));
ws.onopen = () => ws.send(new Uint8Array([0, 255, 0]));
```

Open a second client. Sending `ws.send("both")` on either client makes both receive the text `ready`. Send `move` from each browser to increment a shared counter. Both receive `state COUNT by PEER`. Send `who` for a private ID and active-player count, or `bye` to close the sender's connection. Other text and binary messages echo only to the sender. Stop the adapter with Ctrl-C. [Prime demo](echo.prime)

## Prime process interface

The adapter starts the command following `--` directly. WebSocket and Pion DataChannel applications share the following peer protocol. Read exactly three newline-terminated lines from stdin for each event.

| Line | Prime call | Contents |
| --- | --- | --- |
| 1 | `in[string]("")` | `open`, `close`, `text`, `text64` or `bytes`. |
| 2 | `in[int64]("")` | Positive decimal connection ID. Reconnecting assigns a fresh ID. |
| 3 | `in[string]("")` | Empty on open. A short transport reason on close. Otherwise the message payload. |

Single-line UTF-8 messages use `text`. Text containing a control character or surrounding whitespace uses `text64` to preserve the bytes through both Prime string readers. Binary messages use `bytes`. Both encoded kinds use standard padded base64. An empty data message has an empty third line. The 4096-byte limit applies to the decoded payload.

Write any number of newline-terminated commands to stdout, independently of input events. Write `text ID PAYLOAD`, `text64 ID BASE64`, `bytes ID BASE64`, or `close ID`. For an empty data message retain the space after the ID. A command addressed to a departed peer is discarded. For example, `bytes 2 AP8A` sends the three bytes `00 ff 00` to peer 2. To broadcast, the application emits one command for each currently connected peer. Each command addresses one connection on one adapter instance.

Keep prompts empty and reserve stdout for commands. Input and output run independently with ordered event records and ordered stdout commands. A malformed command, blocked stdin write (three seconds), or unexpected child exit ends the service. During shutdown the child receives EOF and has two seconds to exit.

## Bounds and target support

| Resource | Limit or behavior |
| --- | --- |
| Active application peers | `--max-peers N` accepts 1–256 and defaults to 4. Excess upgraded connections close with 1013. OS backlog hint max(8, N) and a three-second opening handshake timeout. |
| Incoming message | 4096 bytes after fragment assembly, at most 64 fragments and 120 seconds per complete message including idle time. Oversize closes with 1009. Fragment count or time exhaustion closes with 1008. |
| Per-peer application budget | Up to 256 data messages per rolling second. The next message inside that window closes the peer with 1008. Continuing below the rate limit supports long matches. Ping/pong and close are handled by the WebSocket library. |
| Backpressure | Receive queue high-water mark of four frames, write high-water mark of 8192 bytes, one pending stdin record per handler and one stdout send at a time across all peers. Two-second send deadline, one-second close deadline. |
| Wire and lifetime | Plain loopback WS, compression disabled. One service instance per application. IDs and game state are process-local. |

This SDK requires host stdio, process creation, TCP and Python. There is no firmware or no_std implementation. Installing or building Prime and running an offline hello requires none of the SDK's Python packages. This local development service accepts unauthenticated local clients.

## Tetris exchange contract

Start one adapter with `--max-peers 99` and one authoritative game process. The application assigns roles on `open`, validates each input against its connection ID and releases the role on `close`. IDs start at 1 for each adapter process. Hosts combining WebSocket and WebRTC namespaces must key connections by transport instance as well as peer ID.

The adapter carries the game's move and state formats unchanged. For example, a client can send the text `move 7 2` as its seventh input. The host can acknowledge just that player, then send state to multiple recipients independently. For a snapshot larger than 4096 bytes, the game must split it into messages within that limit. Give the parts a common tick or snapshot ID, part number and part count so receivers can apply a complete snapshot atomically. WebSocket fragmentation alone does not increase the per-message limit. The SDK demo's shared counter exercises authoritative updates and fan-out; arena game rules live in the application.

## Interoperability regression

Use a Linux host with Chromium and matching chromedriver on PATH. The regression creates its own loopback HTTP origin and launches two independent temporary headless Chromium instances. From the repository root, run these commands in the SDK virtual environment.

```sh
cargo build --locked --bin prime-lang
pip install -r sdk/websocket/requirements-test.txt
python sdk/websocket/test_service.py "${CARGO_TARGET_DIR:-target}/debug/prime-lang"
```

Each backend runs the same independent websocket-client checks and browser WebSocket API scenario. The suite covers byte-exact empty, NUL, multibyte and maximum-sized payloads, concurrent clients, a ping inside fragmented text, close, interrupted TCP, client admission, rate and fragment limits, progressing beyond 256 lifetime inputs, oversized input and two-recipient broadcast. A separate run on each backend admits 99 simultaneous connections (97 independent clients and both browsers), rejects the 100th, verifies two successive state broadcasts at every peer, reuses the released browser slot with a fresh ID, and checks the final application peer count after disconnects.
