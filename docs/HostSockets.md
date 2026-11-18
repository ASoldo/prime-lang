# Host TCP and UDP sockets

Prime host programs can connect, listen and exchange arbitrary bytes through the same socket API in `run` and in executables produced by `build`. Use the default `std-builtins` feature and a std module with `Result[T,E]` in scope. Each call returns `Result[T,string]`. The table gives the success type. Operating-system errors, timeouts, invalid arguments and closed or mismatched handles return `Err(message)`. Error text can vary by OS.

| Operation | Success | Effect |
| --- | --- | --- |
| `net_tcp_listen(endpoint)` | `int32` | Open a listener. |
| `net_tcp_connect(endpoint, timeout_ms)` | `int32` | Open a connected TCP stream. |
| `net_tcp_accept(listener, timeout_ms)` | `int32` | Accept one independent stream. The listener can accept again. |
| `net_tcp_read(stream, output, limit, timeout_ms)` | `int32` | Replace the output slice with up to `limit` bytes and return its new length. Zero indicates EOF. |
| `net_tcp_write(stream, bytes, timeout_ms)` | `int32` | Return the number of bytes written. A short write commits that prefix. Send the remaining suffix in another call. |
| `net_udp_bind(endpoint)` | `int32` | Bind a UDP socket for sending and receiving. |
| `net_udp_send_to(socket, endpoint, bytes, timeout_ms)` | `int32` | Send one datagram and return its length. An empty slice sends an empty datagram. |
| `net_udp_recv_from(socket, output, limit, timeout_ms)` | `string` | Consume one datagram, replace the output slice, and return the sender's endpoint for that datagram. |
| `net_local_endpoint(handle)` | `string` | Look up the bound IP and port, including an OS-assigned port. |
| `net_peer_endpoint(stream)` | `string` | Look up the connected TCP peer. |
| `net_close(handle)` | `()` | Release the handle. Subsequent calls with that ID return an error. |

Supply endpoints as numeric strings such as `"127.0.0.1:9000"` or `"[::1]:9000"`. Bind port zero to let the OS choose a port. IPv6 availability follows the OS and interface configuration. A program can use the endpoint returned by a receive directly in a reply.

Handles, byte limits and timeout arguments use `int32`. Each opening operation returns a fresh positive ID, so closing a stale ID cannot affect a replacement socket. Closing a listener leaves accepted streams usable. An operation that already acquired a socket can finish under its own deadline after another thread closes the handle.

Payloads and receive buffers use `[]uint8`. A receive replaces the contents of the supplied slice, so aliases see the received bytes. Each receive limit must be between 1 and 65535 inclusive. An oversized UDP datagram is consumed and returns an error. Receive errors preserve the output slice. A successful empty UDP receive clears the slice and returns the sender. Each write accepts up to 65535 bytes, subject to the operating system's datagram size limit. TCP reads and writes can split a sequence of bytes at any position.

A positive `timeout_ms` bounds the socket wait for one operation. Reads return as soon as data is available. Code that loops over several operations must budget for each wait.

A UDP server can receive a binary message and reply to its sender with the same socket.

```prime
module echo;
enum Result[T, E] { Ok(T), Err(E) }

fn echo_once(socket: int32) -> Result[int32, string] {
  let mut []uint8 bytes = [];
  let string sender = net_udp_recv_from(socket, bytes, 1024, 1000)?;
  net_udp_send_to(socket, sender, bytes, 1000)
}

fn main() {
  match net_udp_bind("127.0.0.1:9000") {
    Ok(socket) => { out(echo_once(socket)); out(net_close(socket)); },
    Err(message) => out(message),
  }
}
```

The checker reports `E0NS` for these calls in no-std modules, with std built-ins disabled, and on the embedded targets including ESP32-C3 RV32IMC. The host LSP provides completion, hover and parameter signatures. The separate `tcp_*` demo operations retain their single-listener, loopback framing interface. [Demo server](LoopbackWebServer.md)

Run `python3 scripts/test_host_sockets.py /path/to/prime-lang` to exercise both backends against Python TCP and UDP peers on loopback. The fixture uses ephemeral ports, multiple live handles and both IP families when available. [Prime integration fixture](../scripts/tests/host_sockets/sockets.prime)
