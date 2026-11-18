# Riftwell

Riftwell is a local browser arena for 2–99 falling-block wells. Each match runs one Prime ECS process. It owns gravity, the bag, collision, locking, garbage, elimination and final places. The browser paints its output and sends numbered keypresses. The same lobby accepts WebSocket and WebRTC players.

![Riftwell in Chromium. Seat 2 has received garbage, 39 of 99 wells remain alive.](riftwell.png)

## Run locally

Use Linux with the repository Rust/LLVM build prerequisites, Python 3.11+, and Go 1.24+ for the RTC adapter. Run the commands below from the repository root containing those components. If the components are still on their individual local branches, assemble the tested revisions in a disposable directory from the arena checkout first.

```sh
stage=$(mktemp -d)
git archive HEAD | tar -x -C "$stage"
git archive 583c6cf prime.toml workspace/demos/tetris workspace/demos/tetris_ecs workspace/demos/tetris_arena | tar -x -C "$stage"
git archive e3f15d9 sdk/websocket | tar -x -C "$stage"
git archive 5f85b66 adapters/webrtc | tar -x -C "$stage"
cd "$stage"
```

You can also point the engine entry and SDK paths at separate checkouts. The arena uses loopback networking and a private temporary Unix socket per adapter.

```sh
arena=workspace/demos/field_arena
python3 -m venv /tmp/riftwell-venv
. /tmp/riftwell-venv/bin/activate
pip install -r sdk/websocket/requirements.txt
(cd adapters/webrtc && go build -mod=readonly -o /tmp/riftwell-rtc .)
```

Build the checked-out CLI and choose the ECS executable. These commands start a mixed-transport match in the interpreter.

```sh
export CARGO_BUILD_JOBS=2
cargo build --locked --bin prime-lang
cli="$(realpath "${CARGO_TARGET_DIR:-target}/debug/prime-lang")"
entry=workspace/demos/tetris_arena/main.prime
python "$arena/arena.py" --ws-service sdk/websocket/service.py --rtc-bin /tmp/riftwell-rtc \
  --seats 8 --humans 2 --port 8000 -- "$cli" run "$entry"
```

To run it natively, stop the match, run `"$cli" build "$entry" --name riftwell`, and use `-- ./.build.prime/riftwell/riftwell` as the engine command.

Open the printed URL in one tab for WebSocket. In another tab or browser, open the same URL with `/?transport=rtc`. Select **Start match**. Left and right arrows move, up rotates and space drops. On-screen controls also work. Refresh the tab to resume its reserved seat. State and resumable identities last for one server process. A new tab without an identity claims a new human seat. After all reserved human seats are claimed, another fresh identity receives a lobby-full message.

The server assigns human seats in connection order and fills the other seats with deterministic bot inputs. `--seats 99 --humans 2` reserves two human seats and runs 97 simulated opponents. Set `--humans` equal to `--seats` to reserve all seats for players. The launcher sets the WebSocket adapter cap to the human-seat count. The RTC adapter admits up to 99 concurrent peers. Both adapters feed the same roster. Each process can start with only `--ws-service` or only `--rtc-bin`. The RTC option uses the SDK's own browser helper and loopback signaling endpoint through the same page origin.

## Session and snapshots

Both SDKs deliver three lines per peer event (kind, peer ID, payload) and accept `text PEER PAYLOAD` addressed replies. The local relay only moves bytes between the SDK child interface and the arena. Peer identity is the pair of adapter name and SDK peer ID. A browser sends `join UUID`, then `move SEQUENCE ACTION`. The peer binding determines the seat. Sequences start at one. The server acknowledges a move in the authoritative tick where Prime consumes it. An old or queued sequence returns the current snapshot. Gaps and concurrent claims on an active identity are rejected. The browser keeps its random bearer token and unacknowledged inputs in tab session storage.

Each complete snapshot carries every seat's visible board, score, lines, life flag, placement and acknowledged sequence, along with the match tick, winner and events. Messages use `F PART COUNT SHA256 TEXT` with zero-based part numbers and up to 2800 ASCII payload bytes per part. Each browser verifies the reassembled JSON against the SHA-256 before replacing its displayed state. The browser uses Prime’s four projected active cells and the 200 settled cells to display each well. The ordinary server waits 250 ms between ticks. With `--tick-ms 0`, each accepted move advances one tick for repeatable interactive tests.

## System checks

The browser check requires Chromium and the matching chromedriver on PATH. Install the SDK test requirements to obtain Selenium. It launches two separate Chromium processes per backend, negotiates a real WS and a real ordered RTC data channel into the same match, verifies snapshots, sends different keyboard moves, reloads a tab, checks a duplicate after reconnect, receives a garbage attack, plays through elimination to one survivor and saves a real browser screenshot. Its default roster is 99. The replay check drives 99 simulated seats with recorded action vectors and compares both ECS transcripts and all placements. The WS check also disconnects with an unacknowledged input while the real clock runs, reconnects and verifies the resulting position on both backends.

```sh
pip install -r sdk/websocket/requirements-test.txt
"$cli" build "$entry" --name riftwell
native="$PWD/.build.prime/riftwell/riftwell"
python "$arena/test_browser.py" --ws-service sdk/websocket/service.py --rtc-bin /tmp/riftwell-rtc \
  --prime "$cli" --entry "$entry" --native "$native" --screenshot /tmp/riftwell.png
python "$arena/test_replay.py" --prime "$cli" --entry "$entry" --native "$native" --replay /tmp/riftwell.json
```

### Recorded run

| Scenario on archiebald x86-64 | Result in the interpreter and native binary |
| --- | --- |
| Two Chromium 155 processes, one WS peer, one RTC peer and 97 bots | The browsers agreed on all 22 played ticks, including the seat-1 attack on tick 12, 98 eliminations and seat 1 winning. Final snapshot SHA-256 `42a84059ebf1f908ff848e67e38b274a6fe9fd59b427cdb4256908aab1a5d062`. |
| 99 deterministic bot input streams, zero transport connections and zero browser windows | 19 ticks, one attack, 98 eliminations, seat 1 winning. The complete ordered transcript including input acknowledgements and snapshots had SHA-256 `2caf8e759264c75d23722a9c008483dcb9490201589708e51f433f6021697bc5`. The [recorded replay](bots.replay.json) contains every input, event and placement. |
| Timed disconnect and resume, one WS client | The original seat recovered after disconnecting with pending input. Duplicate move 1 and subsequent move 2 produced two acknowledged inputs and the expected piece position. |
| 99 simultaneously connected WS clients, zero simulated bots and zero browser windows | Each connection claimed a distinct seat. The input from seat 99 changed only its own well and all 99 recipients agreed on the resulting snapshot. |
