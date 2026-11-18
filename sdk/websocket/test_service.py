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
from threading import Thread

import websocket
from websocket import ABNF
from selenium import webdriver
from selenium.webdriver.chrome.service import Service as ChromeDriver

SDK = Path(__file__).resolve().parent
ENTRY = SDK / "echo.prime"


@contextmanager
def service(command, origin):
    proc = subprocess.Popen([sys.executable, SDK / "service.py", "--origin", origin,
                             "--", *command], stdout=subprocess.PIPE, text=True)
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
        expect_close(client, 1008)

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


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("prime", type=Path)
    prime = parser.parse_args().prime.resolve()
    options = webdriver.ChromeOptions()
    options.binary_location = shutil.which("chromium")
    options.add_argument("--headless")
    options.add_argument("--disable-dev-shm-usage")
    with (tempfile.TemporaryDirectory() as scratch,
          ThreadingHTTPServer(("127.0.0.1", 0), BlankPage) as httpd,
          webdriver.Chrome(service=ChromeDriver(shutil.which("chromedriver")),
                           options=options) as driver):
        origin = f"http://127.0.0.1:{httpd.server_port}"
        Thread(target=httpd.serve_forever, daemon=True).start()
        driver.set_script_timeout(12)
        try:
            subprocess.run([prime, "build", ENTRY, "--name", "ws_echo"], cwd=scratch, check=True)
            for mode, cmd in [
                ("interpreter", [prime, "run", ENTRY]),
                ("native", [Path(scratch) / ".build.prime/ws_echo/ws_echo"]),
            ]:
                with service(cmd, origin) as url:
                    protocol(url)
                    browser(driver, origin, url)
                    print(f"{mode}: independent client and Chromium passed", flush=True)
        finally:
            httpd.shutdown()


if __name__ == "__main__":
    main()
