"""Launch one Prime match with a local or published browser endpoint."""
import argparse
import asyncio
import contextlib
import http.client
import json
import os
from pathlib import Path
import signal
import sys
import tempfile
import threading
import uuid
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import urlsplit

from engine import Engine
from gateway import authority, serve_gateway
from match import Match

HERE = Path(__file__).resolve().parent
WEB = HERE / "web"
ASSETS = {"/": ("index.html", "text/html"), "/arena.js": ("arena.js", "text/javascript"),
          "/arena.css": ("arena.css", "text/css")}


class Page(BaseHTTPRequestHandler):
    timeout = 12

    def log_message(self, *_):
        pass

    def reply(self, status, data, mime):
        self.send_response(status)
        self.send_header("Content-Type", mime)
        self.send_header("Content-Length", str(len(data)))
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        self.wfile.write(data)

    def allowed(self, require_origin=False):
        origins = self.headers.get_all("Origin", [])
        return (self.headers.get_all("Host") == [self.server.public_host] and
                (origins == [self.server.origin] or (not origins and not require_origin)))

    def do_GET(self):
        if not self.allowed():
            return self.send_error(403)
        path = urlsplit(self.path).path
        if path == "/healthz":
            self.reply(200 if self.server.healthy() else 503, b"", "text/plain")
        elif path in ASSETS:
            name, mime = ASSETS[path]
            self.reply(200, (WEB / name).read_bytes(), mime)
        elif path == "/config":
            self.reply(200, json.dumps(self.server.config).encode(), "application/json")
        elif path == "/browser.js":
            self.rtc("GET", b"")
        else:
            self.send_error(404)

    def do_POST(self):
        if self.path != "/offer" or not self.allowed(require_origin=True):
            return self.send_error(403)
        try:
            length = int(self.headers.get("Content-Length", ""))
        except ValueError:
            return self.send_error(400)
        if not 0 < length <= 65536:
            return self.send_error(413)
        self.rtc("POST", self.rfile.read(length))

    def rtc(self, method, data):
        if self.server.rtc_port is None:
            return self.send_error(404)
        conn = http.client.HTTPConnection("127.0.0.1", self.server.rtc_port, timeout=12)
        try:
            # The SDK serves its own browser helper and handles all signaling and ICE.
            conn.request(method, self.path, body=data, headers={"Content-Type": "application/json"})
            response = conn.getresponse()
            self.reply(response.status, response.read(), response.getheader("Content-Type"))
        except OSError:
            self.send_error(502)
        finally:
            conn.close()


async def stop(process):
    if process.returncode is None:
        with contextlib.suppress(ProcessLookupError):
            process.terminate()
    try:
        await asyncio.wait_for(process.wait(), 5)
    except TimeoutError:
        process.kill()
        await process.wait()


async def serve(args):
    loop = asyncio.get_running_loop()
    finished = loop.create_future()

    def done(error=None):
        if not finished.done():
            finished.set_result(error)

    def monitor(task):
        if not task.cancelled():
            done(task.exception() or RuntimeError("Arena service exited"))

    for sig in (signal.SIGINT, signal.SIGTERM):
        loop.add_signal_handler(sig, done)
    async with contextlib.AsyncExitStack() as stack:
        tmp = Path(stack.enter_context(tempfile.TemporaryDirectory(prefix="prime-field-")))
        page = stack.enter_context(ThreadingHTTPServer(("127.0.0.1", 0 if args.public_origin else args.port), Page))
        origin = args.public_origin or f"http://127.0.0.1:{page.server_port}"
        page.origin = origin
        page.public_host = authority(origin)
        page.config = {"match": str(uuid.uuid4()), "ws": None, "rtc": bool(args.rtc_bin)}
        page.rtc_port = None
        engine = await Engine.launch(args.engine, args.seats, args.seed)
        stack.push_async_callback(engine.close)
        streams = {}
        processes = [engine.process]
        tasks = {asyncio.create_task(engine.process.wait())}
        page.healthy = lambda: not finished.done() and all(p.returncode is None for p in processes)

        async def send(peer, text):
            transport, number = peer
            writer = streams[transport]
            writer.write(f"text {number} {text}\n".encode())
            await asyncio.wait_for(writer.drain(), 10)

        clock = None if args.lockstep else args.tick_ms / 1000
        match = Match(engine, args.humans, clock, send)

        async def input_loop(transport, reader):
            while True:
                kind = await reader.readline()
                if not kind:
                    raise EOFError(f"{transport} relay closed")
                raw_id, payload = await reader.readline(), await reader.readline()
                if not raw_id or not payload:
                    raise EOFError(f"Incomplete {transport} event")
                await match.event((transport, int(raw_id)), kind.rstrip(b"\n").decode("ascii"),
                                  payload.rstrip(b"\n").decode("utf-8"))

        async def adapter(name, command):
            accepted = loop.create_future()
            def connected(reader, writer):
                if accepted.done():
                    writer.close()
                else:
                    accepted.set_result((reader, writer))
            socket_path = tmp / name
            listener = await asyncio.start_unix_server(connected, path=socket_path)
            stack.enter_context(contextlib.closing(listener))
            process = await asyncio.create_subprocess_exec(*command, "--", sys.executable,
                        HERE / "relay.py", socket_path, stdout=asyncio.subprocess.PIPE)
            processes.append(process)
            stack.push_async_callback(stop, process)
            line = (await asyncio.wait_for(process.stdout.readline(), 20)).decode().strip()
            if not line.startswith("LISTEN "):
                raise RuntimeError(f"{name} adapter failed to start: {line!r}")
            address = line.split(maxsplit=1)[1]
            reader, writer = await asyncio.wait_for(accepted, 10)
            streams[name] = writer
            stack.callback(writer.close)
            tasks.add(asyncio.create_task(input_loop(name, reader)))
            return int(address.rsplit(":", 1)[-1])

        try:
            ws_port = None
            if args.ws_service:
                command = [sys.executable, args.ws_service, "--port", "0", "--origin", origin,
                           "--max-peers", str(args.humans)]
                if args.public_origin:
                    command += ["--browser-host", page.public_host]
                ws_port = await adapter("ws", command)
                page.config["ws"] = ("ws" + origin[4:] + "/ws" if args.public_origin
                                     else f"ws://127.0.0.1:{ws_port}/")
            if args.rtc_bin:
                command = [args.rtc_bin.resolve(), "-port", "0"]
                if args.ice_udp_port:
                    command += ["-ice-udp-port", str(args.ice_udp_port)]
                    if args.ice_advertise_ip:
                        command += ["-ice-advertise-ip", args.ice_advertise_ip]
                page.rtc_port = await adapter("rtc", command)
            if args.public_origin:
                await stack.enter_async_context(await serve_gateway(
                    args.bind, args.port, page.server_port, ws_port, args.tls_cert, args.tls_key))
            if clock:
                tasks.add(asyncio.create_task(match.run_clock()))
            for task in tasks:
                task.add_done_callback(monitor)
            threading.Thread(target=page.serve_forever, daemon=True).start()
            stack.callback(page.shutdown)
            print(f"ARENA {origin}", flush=True)
            error = await finished
            if error:
                raise error
        finally:
            for task in tasks:
                task.cancel()
            await asyncio.gather(*tasks, return_exceptions=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--port", type=int, default=os.getenv("RIFTWELL_PORT", "8000"))
    parser.add_argument("--bind", default=os.getenv("RIFTWELL_BIND", "127.0.0.1"))
    parser.add_argument("--public-origin", default=os.getenv("RIFTWELL_PUBLIC_ORIGIN"),
                        help="exact browser origin for the single published HTTP/WS port")
    parser.add_argument("--tls-cert", default=os.getenv("RIFTWELL_TLS_CERT"))
    parser.add_argument("--tls-key", default=os.getenv("RIFTWELL_TLS_KEY"))
    parser.add_argument("--ice-udp-port", type=int, default=os.getenv("RIFTWELL_ICE_UDP_PORT", "0"))
    parser.add_argument("--ice-advertise-ip", default=os.getenv("RIFTWELL_ICE_ADVERTISE_IP"))
    parser.add_argument("--seats", type=int, default=8)
    parser.add_argument("--humans", type=int, default=2)
    parser.add_argument("--seed", type=int, default=17)
    clock = parser.add_mutually_exclusive_group()
    clock.add_argument("--tick-ms", type=int, default=250,
                       help="server tick interval, or 0 to advance once per accepted input")
    clock.add_argument("--lockstep", action="store_true",
                       help="wait for one numbered input from every live seat in an all-human match")
    parser.add_argument("--ws-service", type=Path, help="SDK sdk/websocket/service.py")
    parser.add_argument("--rtc-bin", type=Path, help="built SDK prime-webrtc executable")
    parser.add_argument("engine", nargs=argparse.REMAINDER, help="-- ENGINE [ARG ...]")
    args = parser.parse_args()
    if args.engine[:1] == ["--"]:
        args.engine.pop(0)
    if not args.engine or not (args.ws_service or args.rtc_bin):
        parser.error("provide an engine command and at least one adapter")
    if args.lockstep and args.humans != args.seats:
        parser.error("--lockstep requires one client per seat (--humans equal to --seats)")
    if not (2 <= args.seats <= 99 and 1 <= args.humans <= args.seats and 0 <= args.seed <= 2147483647
            and 0 <= args.tick_ms <= 60000):
        parser.error("invalid roster, seed or tick interval")
    if not (0 <= args.port <= 65535 and 0 <= args.ice_udp_port <= 65535):
        parser.error("ports must be in 0..65535")
    if args.bind != "127.0.0.1" and not args.public_origin:
        parser.error("--bind requires --public-origin")
    if args.public_origin:
        try:
            authority(args.public_origin)
        except ValueError as exc:
            parser.error(f"invalid --public-origin: {exc}")
    if bool(args.tls_cert) != bool(args.tls_key) or (args.tls_cert and
            not (args.public_origin or "").startswith("https://")):
        parser.error("direct TLS requires --tls-cert, --tls-key and an HTTPS --public-origin")
    if args.rtc_bin and args.ice_advertise_ip and not args.ice_udp_port:
        parser.error("--ice-advertise-ip requires --ice-udp-port")
    asyncio.run(serve(args))


if __name__ == "__main__":
    main()
