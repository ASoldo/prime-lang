"""Route one published HTTP/1.1 port to the private page and WebSocket listeners."""
import asyncio
import contextlib
import ssl
from urllib.parse import urlsplit


def authority(origin):
    """Require the serialized origin a browser will send, including a non-default port."""
    url = urlsplit(origin)
    if url.scheme not in ("http", "https") or not url.hostname:
        raise ValueError("expected an http(s) origin")
    host = url.hostname
    if not host.isascii() or any(c not in "abcdefghijklmnopqrstuvwxyz0123456789-.:" for c in host):
        raise ValueError("expected an ASCII hostname or IP address")
    if ":" in host:
        host = f"[{host}]"
    if url.port == 0:
        raise ValueError("public port must be positive")
    if url.port is not None and url.port != {"http": 80, "https": 443}[url.scheme]:
        host += f":{url.port}"
    if origin != f"{url.scheme}://{host}":
        raise ValueError("use a canonical browser origin with no path, credentials or default port")
    return host


async def copy_stream(reader, writer):
    while data := await reader.read(65536):
        writer.write(data)
        await writer.drain()


async def serve_gateway(bind, port, page_port, ws_port, cert, key):
    tls = None
    if cert:
        tls = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
        tls.load_cert_chain(cert, key)

    async def route(client, reply):
        # Both upstreams parse their own headers and enforce their own limits.
        # The page closes each HTTP response. An upgrade stays on the WS upstream.
        try:
            line = await asyncio.wait_for(client.readline(), timeout=5)
            if not line:
                return
            dest = ws_port if ws_port and line == b"GET /ws HTTP/1.1\r\n" else page_port
            response, request = await asyncio.open_connection("127.0.0.1", dest)
            try:
                request.write(line)

                async def upload():
                    try:
                        await copy_stream(client, request)
                    finally:
                        if not request.is_closing():
                            request.write_eof()

                incoming = asyncio.create_task(upload())
                try:
                    await copy_stream(response, reply)
                finally:
                    incoming.cancel()
                    await asyncio.gather(incoming, return_exceptions=True)
            finally:
                request.close()
        except (OSError, TimeoutError, ValueError):
            pass
        finally:
            reply.close()
            with contextlib.suppress(OSError):
                await reply.wait_closed()

    return await asyncio.start_server(route, bind, port, ssl=tls, limit=8192)
