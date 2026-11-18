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

Open a second client. Sending `ws.send("both")` on either client makes both receive the text `ready`. All other messages echo only to the sender. Close with `ws.close(1000)`; stop the adapter with Ctrl-C.

## Prime process interface

The adapter starts the command following `--` directly and serializes all events on its stdin. For each event, read exactly three newline-terminated lines with `in[int32]("")`, `in[string]("")` and `in[string]("")`.

| Line | Contents |
| --- | --- |
| 1 | Positive decimal peer ID, stable until disconnect. Reconnecting assigns a fresh ID. |
| 2 | `open`, `text`, `binary` or `close`. A transport failure also produces `close`. |
| 3 | Lowercase hexadecimal bytes with two digits per byte. Text uses UTF-8. Open, close and zero-length messages have an empty third line. |

Reply once per event on stdout with `out("drop")` or one line of the form `DEST KIND HEX`. DEST is `self`, `all` (including the sender), or a decimal peer ID. KIND is `text` or `binary`. HEX accepts up to 8192 lowercase hex digits, in pairs. An empty message ends with the space after KIND. A reply to a disconnected peer is discarded. Reserve stdout for replies and keep input prompts empty. EOF ends the application. A malformed reply, invalid text output, application exit or a five-second response timeout stops the service with an error.

For example, the event lines `2`, `binary`, `00ff00` can produce `2 binary 00ff00`. A reply of `all text 7265616479` sends the UTF-8 string `ready`. Prime can change both the message and its destination. The adapter waits for delivery attempts before processing the next event. Concurrent senders are serialized in arrival-to-handler order. One slow write can delay the other senders.

## Bounds and target support

| Resource | Limit or behavior |
| --- | --- |
| Active application peers | 4. Excess upgraded connections close with 1013. OS backlog hint 8 and a three-second opening handshake timeout. |
| Incoming message | 4096 bytes after fragment assembly, at most 64 fragments and 120 seconds per complete message including idle time. Oversize closes with 1009. Fragment count or time exhaustion closes with 1008. |
| Per-peer application budget | After processing 256 data messages the server closes with 1008. Ping/pong and close are handled by the WebSocket library. |
| Backpressure | Receive queue high-water mark of four frames, write high-water mark of 8192 bytes, one pending application exchange per handler. Two-second send deadline, one-second close deadline. |
| Wire and lifetime | Plain loopback WS, compression disabled. One service instance per application. IDs and game state are process-local. |

This SDK requires host stdio, process creation, TCP and Python. There is no firmware or no_std implementation. Installing or building Prime and running an offline hello requires none of the SDK's Python packages. This local development service accepts unauthenticated local clients.

## Tetris exchange contract

A game can use one text message per input: `t1:left`, `t1:right`, `t1:rotate` or `t1:drop`. Assign player roles on `open`, validate each input against the peer ID and game state, and release the role on `close`. A full display snapshot fits in one binary message of 201 bytes. The first byte is the version (1); bytes 1 through 200 represent the visible 10 × 20 board in row-major order starting at the top left. Cell 0 is empty and 1–7 select the game palette. Reply to a newly admitted peer with the latest snapshot and broadcast updated snapshots after accepted inputs. Reconnection starts a fresh session. Applications can drop unknown commands and clients can discard unsupported snapshot versions or lengths.

This proposed game contract covers input-driven snapshots. The supplied demo implements echo and broadcast. Game simulation, authentication and continuous server-initiated ticks are outside the adapter interface. REST and WebRTC can evolve independently.

## Interoperability regression

Use a Linux host with Chromium and matching chromedriver on PATH. The regression creates its own loopback HTTP origin and launches a temporary headless browser. From the repository root, run these commands in the SDK virtual environment.

```sh
cargo build --locked --bin prime-lang
pip install -r sdk/websocket/requirements-test.txt
python sdk/websocket/test_service.py "${CARGO_TARGET_DIR:-target}/debug/prime-lang"
```

Each backend runs the same independent websocket-client checks and browser WebSocket API scenario. The suite covers byte-exact empty, NUL, multibyte and maximum-sized payloads, concurrent clients, a ping inside fragmented text, close, interrupted TCP, client admission, message and fragment budgets, oversized input and two-recipient broadcast.
