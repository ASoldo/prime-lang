# A loopback REST resource with server-sent events

The [Prime program](../workspace/demos/rest_sse/rest_sse.prime) owns one integer reading and a revision counter. A successful assignment advances the counter exactly once, including when assigning the same value again. A rejected assignment preserves both fields. Each run starts at value 0 and revision 0. IDs are scoped to that process. Start a fresh EventSource when starting a new server.

The optional [Go adapter](../adapters/rest_sse) uses the standard library for HTTP/1.1 parsing, JSON and socket I/O. It binds only to IPv4 loopback. It runs either the interpreter or an emitted native binary as its child. The two commands below launch the same application on Linux. Go 1.22 or newer builds the adapter offline. The Prime CLI and the native host runtime retain their usual build requirements.

```sh
(cd adapters/rest_sse && GOTOOLCHAIN=local GOPROXY=off go build -trimpath -o /tmp/prime-rest .)
entry=workspace/demos/rest_sse/rest_sse.prime
/tmp/prime-rest --port 8080 -- prime-lang run "$entry"
# Stop with Ctrl-C before trying the native binary.
prime-lang build "$entry" --name rest_sse
/tmp/prime-rest --port 8080 -- ./.build.prime/rest_sse/rest_sse
```

Port 0 (the default) selects a free port. The adapter prints `LISTEN <port>` after the Prime startup handshake and bind. SIGINT or SIGTERM closes the listener and clients, sends STOP to Prime, waits for STOPPED and reaps the process. An unexpected worker exit or failed protocol exchange ends the session. The adapter targets Unix hosts and requires std for process I/O. The example does not target embedded/no_std.

## REST

```sh
curl -s http://127.0.0.1:8080/state
curl -s --json '{"value":42}' http://127.0.0.1:8080/state
curl -Ns http://127.0.0.1:8080/events
```

| Request | Result |
| --- | --- |
| GET /state | 200 and the current JSON object, e.g. `{"id":1,"value":42}` |
| POST /state | Requires application/json and exactly one case-sensitive `value` member with a signed 32-bit integer literal. Returns 200 and the committed state. |
| GET /events | 200 and a UTF-8 text/event-stream. GET accepts an empty body. |

Application errors have a JSON `error` string and the corresponding HTTP status. Invalid JSON, a wrong shape, duplicate keys, fractional numbers, a GET body or an invalid event cursor receive 400. Unknown paths receive 404. Unsupported methods receive 405 and Allow. Bodies over 1024 bytes receive 413. Unsupported media types, character sets and content encodings receive 415. Prime rejects values outside 0–1000 and mutations at revision 2147483647 with 422. A fifth concurrent subscriber receives 503. HTTP framing errors detected before dispatch follow Go's HTTP server behavior.

## EventSource

On a page served from the same origin (for example, open /state and use the browser console), subscribe and issue a POST from a second terminal.

```js
const source = new EventSource('/events');
for (const type of ['reset', 'promjena']) {
  source.addEventListener(type, e => console.log(e.type, e.lastEventId, JSON.parse(e.data)));
}
```

A new subscription begins with a `reset` snapshot. Each `promjena` event carries a decimal revision in `id` and a single UTF-8 JSON data line with `id`, `value` and `"label":"očitanje"`. The server retains the last 16 successful assignments. For a canonical decimal Last-Event-ID in range 0–2147483647 it replays retained revisions strictly greater than the cursor in commit order. A cursor equal to the latest revision waits for updates. A gap in history or a cursor ahead of the current revision yields one reset with the latest state. The same gap rule applies if a connected subscriber falls behind.

A stream rolls over after 16 events including its initial snapshot. Browser EventSource automatically reconnects with Last-Event-ID. A ten-second heartbeat comment keeps an idle stream active. Slow sockets have a two-second write deadline per batch. Each subscriber copies at most 16 events under the state lock and performs socket writes after releasing it. The four-subscriber limit leaves room for ordinary HTTP clients within the 32-connection cap. Excess connections are closed at accept. HTTP header reads have two seconds, whole requests have four seconds, idle keep-alive connections have five seconds, and Go's MaxHeaderBytes is 8192 (plus its parser buffer allowance).

## Prime transport boundary

The adapter sends one command at a time on stdin. Prime writes one newline-terminated reply on stdout. READY supplies the initial ID and value. GET returns STATE, SET followed by an integer line returns STATE or REJECT, and STOP returns STOPPED and exits. Both READY and STATE use the format `TAG id value`. The adapter holds its state lock across each command and publication so competing requests and replay observe commit order. A reply must fit its 4096-byte reader. Startup has ten seconds and a command reply has three seconds. The process pipes are trusted local transport. State and history are in memory.

## Regression

```sh
python3 scripts/test_rest_sse.py /absolute/path/to/prime-lang
```

The Linux check needs Go, Node 18+ and Chromium or Google Chrome. Use `--chromium /path/to/chrome` to select the binary. It builds the adapter with the Go module proxy disabled, starts the two backends on ephemeral ports, exercises invalid requests and concurrent updates with real HTTP clients, and tests automatic replay in headless Chromium while another client reads and writes the resource. Each session must exit cleanly and release the listener.
