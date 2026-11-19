"""Serve loopback WebSockets on behalf of one long-lived Prime process."""
import argparse
import asyncio
import base64
from collections import deque
from http import HTTPStatus
import itertools
import signal
import sys
import time
import unicodedata

from websockets.asyncio.server import serve
from websockets.exceptions import ConnectionClosed

MAX_BYTES = 4096
MESSAGES_PER_SECOND = 256
MAX_FRAGMENTS = 64


def event_payload(parts):
    if isinstance(parts[0], bytes):
        return "bytes", base64.b64encode(b"".join(parts)).decode("ascii")
    value = "".join(parts)
    if value != value.strip() or any(unicodedata.category(ch) == "Cc" for ch in value):
        return "text64", base64.b64encode(value.encode()).decode("ascii")
    return "text", value


def parse_command(line):
    if not line.endswith(b"\n"):
        raise ValueError("missing command terminator")
    kind, _, rest = line[:-1].partition(b" ")
    peer, has_payload, data = rest.partition(b" ")
    if not peer.isdigit() or not 0 < int(peer) < 2**64:
        raise ValueError("expected positive decimal peer ID")
    if kind == b"close" and not has_payload:
        return int(peer), None
    if not has_payload or kind not in (b"text", b"bytes", b"text64"):
        raise ValueError("expected a text, text64, bytes or close command")
    if kind != b"text":
        decoded = base64.b64decode(data, validate=True)
        if base64.b64encode(decoded) != data:
            raise ValueError("expected canonical padded base64")
        data = decoded
    if len(data) > MAX_BYTES:
        raise ValueError("oversized payload")
    return int(peer), data if kind == b"bytes" else data.decode("utf-8")


class Service:
    def __init__(self, child, max_peers):
        self.child = child
        self.max_peers = max_peers
        self.clients = {}
        self.ids = itertools.count(1)
        self.input_lock = asyncio.Lock()
        self.stopped = asyncio.Event()
        self.error = None

    async def event(self, peer, kind, payload):
        async with self.input_lock:
            if self.stopped.is_set():
                return
            try:
                self.child.stdin.write(f"{kind}\n{peer}\n{payload}\n".encode())
                async with asyncio.timeout(3):
                    await self.child.stdin.drain()
            except (OSError, TimeoutError) as exc:
                self.error = f"Prime stdin: {exc!r}"
                self.stopped.set()

    async def commands(self):
        try:
            while line := await self.child.stdout.readline():
                peer, data = parse_command(line)
                ws = self.clients.get(peer)
                if ws is None:
                    continue
                try:
                    if data is None:
                        await ws.close(1000, "prime-close")
                    else:
                        async with asyncio.timeout(2):
                            await ws.send(data)
                except TimeoutError:
                    await ws.close(1013, "slow-receiver")
                except ConnectionClosed:
                    pass
            if not self.stopped.is_set():
                self.error = "Prime closed stdout"
        except (ValueError, OSError) as exc:
            self.error = f"Prime stdout: {exc!r}"
        finally:
            self.stopped.set()

    async def handle(self, ws):
        if len(self.clients) >= self.max_peers:
            await ws.close(1013, "client limit")
            return
        client_id = next(self.ids)
        self.clients[client_id] = ws
        try:
            await self.event(client_id, "open", "")
            arrivals = deque()
            while True:
                chunks = []
                async with asyncio.timeout(120):
                    async for part in ws.recv_streaming():
                        if len(chunks) == MAX_FRAGMENTS:
                            await ws.close(1008, "fragment limit")
                            return
                        chunks.append(part)
                now = time.monotonic()
                while arrivals and arrivals[0] <= now - 1:
                    arrivals.popleft()
                if len(arrivals) >= MESSAGES_PER_SECOND:
                    await ws.close(1008, "message rate")
                    return
                arrivals.append(now)
                kind, payload = event_payload(chunks)
                await self.event(client_id, kind, payload)
        except TimeoutError:
            await ws.close(1008, "receive timeout")
        except ConnectionClosed:
            pass
        finally:
            await self.event(client_id, "close", "prime-close" if ws.close_reason == "prime-close" else "peer-close")
            del self.clients[client_id]


async def stop_child(child):
    child.stdin.close()

    async def drain_output():
        # A full stdout pipe can keep asyncio's subprocess wait pending on exit.
        while await child.stdout.read(8192):
            pass

    draining = asyncio.create_task(drain_output())
    try:
        await asyncio.wait_for(child.wait(), 2)
    except TimeoutError:
        if child.returncode is None:
            child.kill()
        await child.wait()
    finally:
        draining.cancel()
        await asyncio.gather(draining, return_exceptions=True)


async def run(args):
    child = await asyncio.create_subprocess_exec(
        *args.command, stdin=asyncio.subprocess.PIPE, stdout=asyncio.subprocess.PIPE,
        limit=2 * MAX_BYTES + 32)
    service = Service(child, args.max_peers)
    loop = asyncio.get_running_loop()
    for sig in (signal.SIGINT, signal.SIGTERM):
        loop.add_signal_handler(sig, service.stopped.set)
    def check_host(connection, request):
        if args.browser_host and request.headers.get_all("Host") != [args.browser_host]:
            return connection.respond(HTTPStatus.FORBIDDEN, "Unexpected Host")

    try:
        async with serve(service.handle, "127.0.0.1", args.port, process_request=check_host,
                         origins=args.origin if args.browser_host else [None, *args.origin], compression=None,
                         max_size=MAX_BYTES, max_queue=4, write_limit=8192,
                         open_timeout=3, close_timeout=1, backlog=max(8, args.max_peers)) as server:
            print(f"LISTEN {server.sockets[0].getsockname()[1]}", flush=True)
            commands = asyncio.create_task(service.commands())
            stopped = asyncio.create_task(service.stopped.wait())
            exited = asyncio.create_task(child.wait())
            await asyncio.wait((stopped, exited), return_when=asyncio.FIRST_COMPLETED)
            if exited.done() and not service.stopped.is_set():
                service.error = f"Prime exited unexpectedly ({child.returncode})"
            service.stopped.set()
            commands.cancel()
            await asyncio.gather(commands, stopped, return_exceptions=True)
    finally:
        await stop_child(child)
    if service.error:
        sys.exit(service.error)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--port", default=0, type=int)
    parser.add_argument("--max-peers", type=int, default=4, help="application peer limit (1..256)")
    parser.add_argument("--origin", action="append", default=[],
                        help="allow an exact browser Origin (repeatable)")
    parser.add_argument("--browser-host", help="require this Host and an allowed Origin behind a gateway")
    parser.add_argument("command", nargs="+", help="Prime executable and arguments after --")
    args = parser.parse_args()
    if not 1 <= args.max_peers <= 256:
        parser.error("--max-peers must be in 1..256")
    asyncio.run(run(args))
