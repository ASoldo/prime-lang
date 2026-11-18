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

Prime reads one header block terminated by CRLF CRLF. An exact request line of `GET /health HTTP/1.0` or `GET /health HTTP/1.1` followed by CRLF receives `200 OK` and the body `ok\n`. All other complete header blocks receive `404 Not Found` and `not found\n`. Both responses specify the body length in bytes and close the connection. Header fields are ignored. Request bodies, persistent connections, pipelining, TLS and HTTP/2 are outside this development example.

The server handles one connection at a time and closes the listener after 1–256 accepted connections or ten seconds without a connection. Each accepted connection counts toward the limit, including a read failure. A complete header block must fit in 4096 bytes and arrive within two seconds of starting the read. The server closes incomplete requests without a response. Writes also have a two-second deadline.

## Host TCP built-ins

The interpreter and the native ABI use the same loopback transport. Each execution thread has a single listener, client and receive buffer. The example uses all three on the main thread. The checker reports `E0NS` for these operations in a no-std module or on an embedded target.

| Call | Result |
| --- | --- |
| `tcp_listen(port: int32) -> int32` | Bind an IPv4 loopback listener. Return the assigned port, or −1 on failure (including an invalid port or existing listener). |
| `tcp_accept() -> bool` | Close any previous client and wait up to ten seconds. Return true on accepting a client. |
| `tcp_receive_until(delimiter: string) -> bool` | Capture up to 4096 bytes through the first nonempty delimiter. Return false and clear the buffer on timeout, EOF before the delimiter, overflow or error. Bytes read past the delimiter are discarded. |
| `tcp_starts_with(prefix: string) -> bool` | Compare UTF-8 prefix bytes against the last successful receive on this connection. |
| `tcp_write(text: string) -> bool` | Write up to 4096 UTF-8 bytes. Return true when all bytes have been written. A failed write may have sent a prefix. |
| `tcp_disconnect()` | Drop the client and clear the receive buffer. |
| `tcp_close()` | Drop the listener, client and buffer. |

## Reproduce the loopback check

```sh
cargo build --locked --bin prime-lang
python3 scripts/test_web_server.py target/debug/prime-lang
```

The check reads an OS-assigned port from each server's terminal, sends ordinary HTTP/1.0 and HTTP/1.1 requests, checks the complete responses and EOF, and waits for process exit. It sends 64 requests to the interpreter, then runs the same native executable twice with different route orders, 64 requests each time. It also checks diagnostics for invalid argument types, wrong arity and no-std use.
