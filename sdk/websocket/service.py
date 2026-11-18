"""Serve loopback WebSockets on behalf of one long-lived Prime process."""
import argparse
import asyncio
import itertools
import re
import signal
import sys

from websockets.asyncio.server import serve
from websockets.exceptions import ConnectionClosed

MAX_CLIENTS = 4
MAX_BYTES = 4096
MAX_MESSAGES = 256
MAX_FRAGMENTS = 64
RESPONSE = re.compile(rb"(self|all|[1-9][0-9]{0,9}) (text|binary) ([0-9a-f]*)\n")


class Service:
    def __init__(self, child):
        self.child = child
        self.clients = {}
        self.ids = itertools.count(1)
        self.lock = asyncio.Lock()
        self.stopped = asyncio.Event()
        self.error = None

    async def exchange(self, client_id, kind, data):
        # Serializing input through delivery also orders broadcasts for all peers.
        async with self.lock:
            if self.stopped.is_set():
                return
            try:
                async with asyncio.timeout(5):
                    record = f"{client_id}\n{kind}\n{data.hex()}\n"
                    self.child.stdin.write(record.encode("ascii"))
                    await self.child.stdin.drain()
                    line = await self.child.stdout.readline()
                    if line == b"drop\n":
                        return
                    match = RESPONSE.fullmatch(line)
                    if not match or len(match[3]) > 2 * MAX_BYTES:
                        raise ValueError("invalid Prime response")
                    payload = bytes.fromhex(match[3].decode())
                    if match[2] == b"text":
                        payload = payload.decode("utf-8")
            except (ValueError, OSError, TimeoutError) as exc:
                self.error = f"Prime exchange failed: {exc!r}"
                self.stopped.set()
                return

            dest = match[1]
            if dest == b"all":
                peers = list(self.clients.values())
            else:
                peer = self.clients.get(client_id if dest == b"self" else int(dest))
                peers = [peer] if peer else []
            for peer in peers:
                try:
                    async with asyncio.timeout(2):
                        await peer.send(payload)
                except TimeoutError:
                    await peer.close(1013, "write timeout")
                except ConnectionClosed:
                    pass

    async def handle(self, ws):
        if len(self.clients) >= MAX_CLIENTS:
            await ws.close(1013, "client limit")
            return
        client_id = next(self.ids)
        self.clients[client_id] = ws
        try:
            await self.exchange(client_id, "open", b"")
            for _ in range(MAX_MESSAGES):
                chunks = []
                async with asyncio.timeout(120):
                    async for part in ws.recv_streaming():
                        if len(chunks) == MAX_FRAGMENTS:
                            await ws.close(1008, "fragment limit")
                            return
                        chunks.append(part)
                if isinstance(chunks[0], str):
                    kind, data = "text", "".join(chunks).encode("utf-8")
                else:
                    kind, data = "binary", b"".join(chunks)
                await self.exchange(client_id, kind, data)
            await ws.close(1008, "message budget")
        except TimeoutError:
            await ws.close(1008, "receive timeout")
        except ConnectionClosed:
            pass
        finally:
            del self.clients[client_id]
            await self.exchange(client_id, "close", b"")


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
    service = Service(child)
    loop = asyncio.get_running_loop()
    for sig in (signal.SIGINT, signal.SIGTERM):
        loop.add_signal_handler(sig, service.stopped.set)
    try:
        async with serve(service.handle, "127.0.0.1", args.port,
                         origins=[None, *args.origin], compression=None,
                         max_size=MAX_BYTES, max_queue=4, write_limit=8192,
                         open_timeout=3, close_timeout=1, backlog=8) as server:
            print(f"LISTEN {server.sockets[0].getsockname()[1]}", flush=True)
            stopped = asyncio.create_task(service.stopped.wait())
            exited = asyncio.create_task(child.wait())
            await asyncio.wait((stopped, exited), return_when=asyncio.FIRST_COMPLETED)
            if exited.done() and not service.stopped.is_set():
                service.error = f"Prime exited unexpectedly ({child.returncode})"
            service.stopped.set()
            stopped.cancel()
    finally:
        await stop_child(child)
    if service.error:
        sys.exit(service.error)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--port", default=0, type=int)
    parser.add_argument("--origin", action="append", default=[],
                        help="allow an exact browser Origin (repeatable)")
    parser.add_argument("command", nargs="+", help="Prime executable and arguments after --")
    asyncio.run(run(parser.parse_args()))
