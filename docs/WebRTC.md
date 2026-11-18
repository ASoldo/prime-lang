# WebRTC for host Prime programs

Two browser tabs can play against one Prime process. The sample keeps the move count in Prime, replies to the player who moved, and keeps the count when a tab reloads. An optional host executable runs the interpreter or the emitted native game as a child. The child uses standard input and output. [Prime game](../workspace/demos/webrtc/game.prime) · [Prime API](../workspace/plugins/webrtc/webrtc.prime)

## Run the game

From the repository root, with the host Rust/LLVM toolchain and Go 1.24 or later installed, run the following commands.

```sh
cargo build --locked --bin prime-lang
(cd adapters/webrtc && go build -mod=readonly -o ../../target/prime-webrtc .)
target/prime-webrtc -- target/debug/prime-lang run workspace/demos/webrtc/game.prime
```

Open the printed `http://127.0.0.1:PORT` in two tabs on the same machine. Each tab displays its own `joined ID`. Send `move` from each, then send `stats` to read the shared move count and number of departed peers. Reload a tab to join with a new ID. Sending `bye` closes that tab's connection. Ctrl-C stops the service. Pass `-port 8080` before `--` to request a particular loopback port.

The same adapter can launch the native game.

```sh
target/debug/prime-lang build workspace/demos/webrtc/game.prime --name rtc_game
target/prime-webrtc -- ./.build.prime/rtc_game/rtc_game
```

## Signaling and transport

The browser creates one ordered, reliable, in-band-negotiated data channel labelled `prime`. It creates an SDP offer, sets the local description and waits for ICE gathering to complete. It posts its resulting `RTCSessionDescriptionInit` JSON object (`type`, `sdp`) to `POST /offer` with `Content-Type: application/json`. A successful HTTP 200 response contains the corresponding complete SDP answer. The browser applies the answer with `setRemoteDescription`. Both descriptions carry their candidates. A new connection uses a new offer. [Browser client](../adapters/webrtc/browser.js)

The host binds HTTP to IPv4 loopback and gathers only IPv4 UDP loopback candidates. Both ends use an empty ICE server list. Pion handles ICE connectivity checks, DTLS, SCTP and the WebRTC data-channel protocol. The HTTP handler checks the Host header and any browser Origin against the printed address. The demonstrated deployment is one machine. LAN operation would need different binding and candidate selection. Internet deployment would also need authenticated HTTPS signaling, a connectivity policy and tested STUN/TURN configuration. [Pion WebRTC v4.2.27](https://pkg.go.dev/github.com/pion/webrtc/v4@v4.2.27)

The included page uses the same browser API available to a same-origin application. Register the receiver when connecting to see messages Prime sends immediately upon opening.

```js
import { connectPrime } from "/browser.js";

const game = await connectPrime(({ data }) => console.log(data));
game.dc.send("move");
game.dc.send(new Uint8Array([0, 128, 255]));
// Later, when leaving the session
game.close();
```

## Prime event contract

Import `prime::webrtc::prelude::{*}` with the library declared in the program's package manifest. A single consumer calls `rtc_kind()`, `rtc_peer()` and `rtc_data()` in that order for every event. These return `Result[string, string]`, `Result[int64, string]` and `Result[string, string]`. Each event occupies exactly three LF-terminated lines, including the payload line when empty. The calls block. End of input ends the game loop. A game with an independent render loop can dedicate an input thread to these reads. [Example manifest](../workspace/demos/webrtc/prime.toml)

| Kind | Payload delivered by the adapter |
| --- | --- |
| `open` | Empty. The channel is ready for Prime to send immediately. |
| `text` | Single-line UTF-8 text, including the empty string. |
| `text64` | Padded standard base64 of UTF-8 text containing control characters or leading/trailing Unicode whitespace. This preserves NUL, line breaks and whitespace through both Prime input readers. |
| `bytes` | Padded standard base64 of the binary message, including empty data. |
| `audio`, `video` | Negotiated codec MIME type once per kind and peer, after Pion reads the first nonempty RTP payload of that kind. |
| `close` | Reason string, for example `channel-closed`, `prime-close`, `failed`, `invalid-message` or `event-backlog`. |

Peer IDs increase for the lifetime of the adapter. An opened peer gets one `open` before any payloads and at most one `close`. The child can address any open peer independently of the current event. After a peer departs, commands to its old ID are discarded. Refresh starts a new connection and the existing Prime process continues to own application state.

| Prime call | Single stdout record |
| --- | --- |
| `rtc_text(peer, data)` | `text ID DATA` |
| `rtc_text64(peer, encoded)` | `text64 ID BASE64` |
| `rtc_bytes(peer, encoded)` | `bytes ID BASE64` |
| `rtc_close(peer)` | `close ID` |

Only the command word and ID are split at spaces. For an empty payload the separating space after the ID is required. Use `rtc_text64` for control characters or multiline text. Use standard padded base64 for both encoded calls. `out` may emit a complete command directly, as the counter demo does with formatted values. Reserve stdout for these records and read stdin with an empty prompt. Malformed output from the program ends the adapter with an error. A successful send queues one ordered WebRTC message. Application-level delivery acknowledgments can be exchanged as ordinary messages.

## Bounds and lifecycle

- The host admits eight peers at a time, including unfinished negotiations. It rejects further offers with HTTP 503. Each offer body may contain up to 64 KiB. Reading the HTTP request and gathering the answer each have a five-second budget.
- The browser helper has a 12-second connection deadline. The host reclaims a peer that fails to open a supported channel within 15 seconds of admission. Pion uses a one-second ICE keepalive, a four-second disconnected timeout and an eight-second failed timeout.
- Application messages carry at most 8,192 decoded bytes in either direction. An oversized inbound message closes that peer. A second channel, wrong label or unreliable/unordered channel also closes its peer.
- When 128 events await the Prime process, admission pauses and a sender delivering another message is closed. Reserved queue space preserves peer lifecycle notifications. A three-second blocked write to the child's stdin fails the service. A browser with at least 256 KiB buffered for transmission is disconnected on the next send.
- SIGINT or SIGTERM closes signaling, peer connections and the child's stdin. The child has two seconds to finish on EOF before the adapter kills it. Child exit also closes the service. The API uses process EOF for service shutdown.

## Dependency and offline builds

The adapter is an independent Go module. It pins `github.com/pion/webrtc/v4 v4.2.27` and the selected transitive versions in `go.mod`, with checksums in `go.sum`. The ordinary Prime CLI, its Cargo lockfile and emitted applications build with their existing Rust toolchain. Go and Pion are needed only when building this adapter. [Module manifest](../adapters/webrtc/go.mod) · [Checksums](../adapters/webrtc/go.sum)

To prepare portable offline adapter sources, run these commands from the repository root while connected. Transfer the resulting directory to a machine with the matching Go toolchain already installed.

```sh
offline_src=$(mktemp -d)
cp adapters/webrtc/*.go adapters/webrtc/go.mod adapters/webrtc/go.sum \
  adapters/webrtc/browser.js adapters/webrtc/index.html "$offline_src/"
(cd "$offline_src" && go mod download && go mod verify && go mod vendor)
```

Build in the transferred directory. A prebuilt adapter executable for the target OS and architecture can also be copied directly.

```sh
cd "$offline_src"
GOTOOLCHAIN=local GOPROXY=off GOSUMDB=off go build -mod=vendor -o prime-webrtc .
```

For a separate offline Prime CLI build, prepare the repository Rust/LLVM toolchain and Cargo cache in advance with `cargo fetch --locked`, then run `cargo build --offline --locked --bin prime-lang`. The minimal CLI build uses `cargo build --offline --locked --no-default-features --bin prime-lang`.

## Reproduce the browser and lifecycle checks

With the host CLI and adapter built as above, install a matching Chromium and ChromeDriver and run these checks from the repository root. The Python harness uses the standard library and disposable browser profiles.

```sh
(cd adapters/webrtc && go test -mod=readonly -race ./...)
RUSTUP_TOOLCHAIN=$(rustup show active-toolchain | cut -d' ' -f1) \
  python3 adapters/webrtc/test_browser.py target/debug/prime-lang target/prime-webrtc
```

The Chromium test uses real loopback peer connections against both `prime-lang run` and the emitted native executable. It checks peer identity, moves shared across two tabs, Unicode and multiline text, empty messages, arbitrary binary bytes, the 8 KiB limit, tab refresh, Prime-initiated close and shutdown with a live peer. It also checks connection state and the selected ICE candidate through browser statistics. The bundled page must visibly display the fifth move after a form submission.

Two separate media sessions offer a synthetic Web Audio oscillator and a changing canvas capture. On Chromium 155.0.8059.84 for Linux x86-64, Prime reported `audio/opus` and `video/VP8` after receipt of RTP on each corresponding connection. Media support in this API is inbound RTP receipt and codec notification. Pixel/PCM access, playback and outbound media tracks are outside its scope. The test requires neither cameras nor audio devices.
