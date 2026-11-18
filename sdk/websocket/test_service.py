"""Run the same RFC6455 and Chromium checks against both Prime backends."""
import argparse
from contextlib import contextmanager, closing, ExitStack
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
import select
import shutil
import struct
import subprocess
import sys
import tempfile
import time
from threading import Thread

import websocket
from websocket import ABNF
from selenium import webdriver
from selenium.webdriver.chrome.service import Service as ChromeDriver

SDK = Path(__file__).resolve().parent
ENTRY = SDK / "echo.prime"


@contextmanager
def service(command, origin, *options):
    proc = subprocess.Popen([sys.executable, SDK / "service.py", "--origin", origin,
                             *options, "--", *command], stdout=subprocess.PIPE, text=True)
    try:
        assert select.select([proc.stdout], [], [], 15)[0], "missing listen announcement"
        prefix, port = proc.stdout.readline().split()
        assert prefix == "LISTEN"
        yield f"ws://127.0.0.1:{int(port)}/"
    finally:
        proc.terminate()
        try:
            status = proc.wait(12)
        except subprocess.TimeoutExpired:
            proc.kill()
            proc.wait()
            raise
        assert status == 0, f"adapter exit {status}"


def connect(url):
    return websocket.create_connection(url, timeout=8, suppress_origin=True)


def expect_close(ws, code):
    op, data = ws.recv_data()
    assert op == ABNF.OPCODE_CLOSE and struct.unpack("!H", data[:2])[0] == code, (op, data)
    ws.shutdown()  # recv_data sent the close acknowledgement; release the TCP socket.


def roundtrip(ws, value):
    if isinstance(value, bytes):
        ws.send_binary(value)
    else:
        ws.send(value)
    assert ws.recv() == value


def protocol(url):
    with ExitStack() as stack:
        a, b = [stack.enter_context(closing(connect(url))) for _ in range(2)]
        for value in ("", b"", " \nŽ\x00🙂\r ", b"\x00\xff\x00", bytes(range(256)) * 16):
            roundtrip(a, value)
        for value in ("t1:left", bytes([1]) + bytes(range(8)) * 25):
            roundtrip(a, value)
        a.send_frame(ABNF.create_frame(b"\xe2", ABNF.OPCODE_TEXT, fin=0))
        a.ping(b"\x00ping")
        assert a.recv_data(control_frame=True) == (ABNF.OPCODE_PONG, b"\x00ping")
        roundtrip(b, "while fragmented")
        a.send_frame(ABNF.create_frame(b"\x82\xac", ABNF.OPCODE_CONT))
        assert a.recv() == "€"

        a.send("both")
        assert a.recv() == b.recv() == "ready"

        c, d = [stack.enter_context(closing(connect(url))) for _ in range(2)]
        roundtrip(d, "fourth")
        with closing(connect(url)) as extra:
            expect_close(extra, 1013)
        c.sock.close()  # Interrupted TCP without a close frame.
        c.sock = None
        roundtrip(d, "after loss")
        d.send_close(1000)
        expect_close(d, 1000)
        b.send_binary(bytes(4097))
        expect_close(b, 1009)
        roundtrip(a, "still here")

    with closing(connect(url)) as client:
        for i in range(256):
            roundtrip(client, str(i))
        time.sleep(1.05)
        client.send("move")
        assert client.recv().startswith("state 1 by ")

    with closing(connect(url)) as client:
        # Emit one burst without a per-message round trip.
        for _ in range(257):
            client.send("rate")
        while True:
            opcode, data = client.recv_data()
            if opcode == ABNF.OPCODE_CLOSE:
                assert struct.unpack("!H", data[:2])[0] == 1008 and data[2:] == b"message rate"
                client.shutdown()
                break
            assert data == b"rate"

    with closing(connect(url)) as client:
        client.send_frame(ABNF.create_frame(b"", ABNF.OPCODE_BINARY, fin=0))
        for i in range(64):
            client.send_frame(ABNF.create_frame(b"", ABNF.OPCODE_CONT, fin=int(i == 63)))
        expect_close(client, 1008)


class BlankPage(BaseHTTPRequestHandler):
    def do_GET(self):
        self.send_response(200)
        self.send_header("Content-Type", "text/html")
        self.end_headers()
        self.wfile.write(b"<!doctype html><title>WebSocket test</title>")

    def log_message(self, *args):
        pass


def browser(driver, origin, url):
    driver.get(origin)
    result = driver.execute_async_script("""
        const [url, done] = arguments;
        const ws = new WebSocket(url);
        ws.binaryType = "arraybuffer";
        const seen = [];
        ws.onerror = () => done("WebSocket error");
        ws.onopen = () => ws.send("both");
        ws.onmessage = ({data}) => {
            if (typeof data === "string") {
                seen.push(data);
                ws.send(new Uint8Array([0, 255, 0]));
            } else {
                seen.push(Array.from(new Uint8Array(data)));
                ws.close(1000, "done");
            }
        };
        ws.onclose = e => done([seen, e.code, e.wasClean]);
    """, url)
    assert result == [["ready", [0, 255, 0]], 1000, True], result


def browser_connect(driver, url):
    opened = driver.execute_async_script("""
        const [url, done] = arguments;
        const ws = window.peer = new WebSocket(url);
        const pending = [], queue = [];
        window.readMessage = callback => {
            if (queue.length) callback(queue.shift());
            else pending.push(callback);
        };
        ws.onmessage = ({data}) => {
            if (pending.length) pending.shift()(data);
            else queue.push(data);
        };
        window.closedCode = new Promise(resolve => ws.onclose = e => resolve(e.code));
        ws.onopen = () => done(true);
        ws.onerror = () => done(false);
    """, url)
    assert opened


def browser_send(driver, value):
    driver.execute_script("window.peer.send(arguments[0])", value)


def browser_read(driver):
    return driver.execute_async_script("window.readMessage(arguments[0])")


def check_identity(message, active):
    label, peer, count, value = message.split()
    assert (label, count, int(value)) == ("peer", "active", active), message
    return int(peer)


def crowd(url, drivers):
    # One server and one Prime process. Two independent browser profiles plus 97 TCP peers.
    for driver in drivers:
        browser_connect(driver, url)
    first, second = drivers
    with ExitStack() as connections:
        clients = [connections.enter_context(closing(connect(url))) for _ in range(97)]
        for driver in drivers:
            browser_send(driver, "who")
        ids = [check_identity(browser_read(driver), 99) for driver in drivers]
        for client in clients:
            client.send("who")
        ids.extend(check_identity(client.recv(), 99) for client in clients)
        assert len(set(ids)) == 99
        with closing(connect(url)) as overflow:
            expect_close(overflow, 1013)

        for move, actor in enumerate(drivers, start=1):
            browser_send(actor, "move")
            expected = f"state {move} by {ids[move - 1]}"
            assert [browser_read(driver) for driver in drivers] == [expected] * 2
            assert all(client.recv() == expected for client in clients)

        browser_send(first, "bye")
        assert first.execute_async_script("window.closedCode.then(arguments[0])") == 1000
        wait_for_active(clients[0], 98)
        browser_connect(first, url)
        browser_send(first, "who")
        replacement = check_identity(browser_read(first), 99)
        assert replacement not in ids
        browser_send(first, "move")
        expected = f"state 3 by {replacement}"
        assert [browser_read(driver) for driver in drivers] == [expected] * 2
        assert all(client.recv() == expected for client in clients)
        for driver in drivers:
            driver.execute_script("window.peer.close(1000)")
            assert driver.execute_async_script("window.closedCode.then(arguments[0])") == 1000
    with closing(connect(url)) as last:
        wait_for_active(last, 1)


def wait_for_active(client, count):
    deadline = time.monotonic() + 5
    while time.monotonic() < deadline:
        client.send("who")
        message = client.recv()
        if message.endswith(f" active {count}"):
            return
        time.sleep(0.02)
    raise AssertionError(f"expected {count} active peers; received {message!r}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("prime", type=Path)
    parser.add_argument("--chromium", default=shutil.which("chromium") or shutil.which("google-chrome"))
    args = parser.parse_args()
    if not args.chromium:
        parser.error("install Chromium or pass --chromium")
    prime = args.prime.resolve()
    options = webdriver.ChromeOptions()
    options.binary_location = args.chromium
    options.add_argument("--headless")
    options.add_argument("--disable-dev-shm-usage")
    with (tempfile.TemporaryDirectory() as scratch,
          ThreadingHTTPServer(("127.0.0.1", 0), BlankPage) as httpd,
          webdriver.Chrome(service=ChromeDriver(shutil.which("chromedriver")),
                           options=options) as driver,
          webdriver.Chrome(service=ChromeDriver(shutil.which("chromedriver")),
                           options=options) as other):
        origin = f"http://127.0.0.1:{httpd.server_port}"
        Thread(target=httpd.serve_forever, daemon=True).start()
        for session in (driver, other):
            session.set_script_timeout(12)
            session.get(origin)
        try:
            subprocess.run([prime, "build", ENTRY, "--name", "ws_echo"], cwd=scratch, check=True)
            for mode, cmd in [
                ("interpreter", [prime, "run", ENTRY]),
                ("native", [Path(scratch) / ".build.prime/ws_echo/ws_echo"]),
            ]:
                with service(cmd, origin) as url:
                    protocol(url)
                    browser(driver, origin, url)
                with service(cmd, origin, "--max-peers", "99") as url:
                    crowd(url, (driver, other))
                print(f"{mode}: RFC6455, 99 peers (two Chromium instances), "
                      "targeting, broadcast, reconnect and cleanup passed", flush=True)
        finally:
            httpd.shutdown()


if __name__ == "__main__":
    main()
