# Run the Prime loopback web server

The [server](../workspace/demos/web_server/web_server.prime) reads a port and a connection limit from standard input. Give it port `0` to have the OS assign a free port, or choose an available port such as `8080`. It prints `LISTEN <port>` once it has bound to `127.0.0.1`.

Run from the repository root with a host std build of the CLI. This session accepts two connections.

```sh
entry=workspace/demos/web_server/web_server.prime
printf '8080\n2\n' | prime-lang run "$entry"
```

In a second terminal, issue the requests before the ten-second accept deadline.

```sh
curl --noproxy '*' --max-time 3 -i http://127.0.0.1:8080/health
curl --noproxy '*' --max-time 3 -i http://127.0.0.1:8080/elsewhere
```

To run the native version, build once and start the executable. Use the same two requests in either order. The native program opens its sockets when it runs.

```sh
prime-lang build workspace/demos/web_server/web_server.prime --name prime_web
printf '8080\n2\n' | ./.build.prime/prime_web/prime_web
```

## HTTP subset

The Prime server calls `tcp_next_frame("\r\n\r\n", limit)` to read one HTTP header block. An exact request line of `GET /health HTTP/1.0` or `GET /health HTTP/1.1` followed by CRLF receives `200 OK` and the body `ok\n`. All other complete header blocks receive `404 Not Found` and `not found\n`. Both responses specify the body length in bytes and close the connection. Header fields are ignored. Request bodies, persistent connections, pipelining, TLS and HTTP/2 are outside this development example.

The server can assemble four headers concurrently. For example, leave a connection open after sending `GET /elsewhere HTTP/1.1\r\n`, issue the health request on a second connection, then send the final `\r\n` on the first. The second connection receives its 200 before the first receives its 404. Prime selects the route and writes one response at a time.

Each admitted connection gets two seconds from acceptance to supply the entire header, including its terminator, in at most 4096 bytes. Receiving another fragment does not extend that deadline. EOF, a read error, expiry or exhausting the buffer without a terminator closes the connection without a response. When all four slots are occupied, additional connections wait in the OS listener backlog until a slot opens, subject to OS backlog limits. Writes have a separate two-second deadline.

Every admission counts towards the session limit of 1–256, including failed reads. On reaching the limit the server drains admitted clients, then closes the listener. When waiting for new clients it also exits after ten seconds of inactivity.

## Host TCP built-ins

The interpreter and the native ABI use the same loopback transport. Each server has a single listener and current client. The concurrent operation also holds partially assembled frames with their sockets. The example calls the operations on the main thread. The LSP exposes their signatures in completions, hover and signature help on the host. The checker reports `E0NS` for these operations in a no-std module or on an embedded target.

| Call | Result |
| --- | --- |
| `tcp_listen(port: int32) -> int32` | Bind an IPv4 loopback listener. Return the assigned port, or −1 on failure (including an invalid port or existing listener). |
| `tcp_accept() -> bool` | Close any previous client and wait up to ten seconds. Return true on accepting a client. |
| `tcp_next_frame(delimiter: string, limit: int32) -> bool` | On a valid call, close the current client and read from up to four connections until one has a complete frame. Return true with that client and its frame selected for `tcp_starts_with` and `tcp_write`. Return false on drain, idle timeout, listener failure or invalid arguments. |
| `tcp_receive_until(delimiter: string) -> bool` | Capture up to 4096 bytes through the first nonempty delimiter. Return false and clear the buffer on timeout, EOF before the delimiter, overflow or error. Bytes read past the delimiter are discarded. |
| `tcp_starts_with(prefix: string) -> bool` | Compare UTF-8 prefix bytes against the last successful receive on this connection. |
| `tcp_write(text: string) -> bool` | Write up to 4096 UTF-8 bytes. Return true when all bytes have been written. A failed write may have sent a prefix. |
| `tcp_disconnect()` | Drop the client and clear the receive buffer. |
| `tcp_close()` | Drop the listener, current and pending clients, buffers and admission count. |

The first valid `tcp_next_frame` call on a bound listener fixes its delimiter. It accepts a runtime string of 1–4096 UTF-8 bytes and matches the first occurrence, including across reads. The delimiter counts towards the 4096-byte frame limit and is included in the selected frame. Extra bytes from the same read are discarded. Pass the session's total admission limit (1–256) on each call. An empty, oversized or different delimiter, or an out-of-range limit, returns false immediately and preserves the selected client, pending frames, deadlines and budget. `tcp_disconnect()` releases only the current client. `tcp_close()` resets the delimiter and budget so a new listener can serve another protocol.

## Reproduce the loopback check

```sh
cargo build --locked --bin prime-lang
python3 scripts/test_web_server.py target/debug/prime-lang
```

The check uses ordinary loopback sockets and verifies the full responses and EOF in the interpreter and two runs of the same native executable. Each run covers a slow partial header alongside a successful health check, four occupied slots and a waiting fifth request, truncated and oversized headers, a partial request that expires despite receiving more bytes, 64 route requests, and process exit with the admission budget exhausted. It also checks argument and no-std diagnostics. The [telemetry probe](../scripts/tests/tcp_frames/telemetry.prime) reads its delimiter from stdin. It exercises two listener sessions, a multibyte delimiter split across reads, and attempts to change the delimiter while a frame is incomplete, in both the interpreter and the generated binary.

For JSON request bodies, concurrent REST clients and browser EventSource replay, see the [Prime REST and SSE example](RestSse.md).
