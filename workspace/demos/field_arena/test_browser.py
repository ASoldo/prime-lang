"""Drive two independent Chromium processes through the live arena UI."""
import argparse
import base64
import contextlib
import hashlib
import json
from pathlib import Path
import selectors
import signal
import shutil
import subprocess
import sys
import uuid
from urllib.request import urlopen
import zlib

from selenium import webdriver
from selenium.webdriver import ActionChains, Keys
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait

HERE = Path(__file__).resolve().parent


def browser():
    options = webdriver.ChromeOptions()
    for flag in ["--headless", "--disable-dev-shm-usage", "--window-size=1100,1040",
                 "--no-proxy-server", "--no-first-run"]:
        options.add_argument(flag)
    client = webdriver.Chrome(service=Service(shutil.which("chromedriver")), options=options)
    # Observe the actual transport in order to check retransmission and invalid input.
    client.execute_cdp_cmd("Page.addScriptToEvaluateOnNewDocument", {"source": """
        for (const type of [WebSocket, RTCDataChannel]) {
          const original = type.prototype.send;
          type.prototype.send = function(data) {
            window.wire = this;
            return original.call(this, data);
          };
        }
    """})
    return client


def wait(driver, condition):
    return WebDriverWait(driver, 25, poll_frequency=.05).until(condition)


def ui(driver, id_):
    return driver.find_element(By.ID, id_).text


def at(driver, tick):
    wait(driver, lambda d: ui(d, "tick") == f"Tick {tick}")
    return driver.find_element(By.ID, "frameHash").get_attribute("title")


def key(driver, symbol):
    ActionChains(driver).send_keys(symbol).perform()


def frame(driver):
    return driver.execute_script(
        "return document.querySelector('#well').toDataURL('image/png')")


def connected(driver, seat):
    wait(driver, lambda d: ui(d, "player") == f"Seat {seat}" and ui(d, "linkState") == "Connected")


@contextlib.contextmanager
def server(args, engine, interval, seats, humans=2):
    with contextlib.ExitStack() as stack:
        command = [sys.executable, str(HERE / "arena.py"), "--port", "0", "--tick-ms", str(interval),
                   "--seats", str(seats), "--humans", str(humans), "--seed", "137"]
        if args.ws_service:
            command += ["--ws-service", str(args.ws_service)]
        if args.rtc_bin:
            command += ["--rtc-bin", str(args.rtc_bin)]
        host = subprocess.Popen(command + ["--", *engine], stdout=subprocess.PIPE, text=True)
        def stop():
            host.send_signal(signal.SIGTERM)
            try:
                code = host.wait(10)
            except subprocess.TimeoutExpired:
                host.kill()
                host.wait()
                raise
            if code:
                raise RuntimeError(f"arena exited with {code}")
        stack.callback(stop)
        with selectors.DefaultSelector() as ready:
            ready.register(host.stdout, selectors.EVENT_READ)
            if not ready.select(45):
                raise TimeoutError("arena startup")
        line = host.stdout.readline().strip()
        if not line.startswith("ARENA "):
            raise ValueError(f"arena failed to start: {line!r}")
        url = line.split()[1]
        yield url


def scenario(args, engine, screenshot):
    with contextlib.ExitStack() as stack:
        url = stack.enter_context(server(args, engine, 0, args.seats))
        left = stack.enter_context(browser())
        right = stack.enter_context(browser())
        left.get(url + ("/" if args.ws_service else "/?transport=rtc"))
        connected(left, 1)
        right.get(url + ("/?transport=rtc" if args.rtc_bin else "/"))
        connected(right, 2)
        assert left.session_id != right.session_id
        assert left.execute_script("return wire.constructor.name") == ("WebSocket" if args.ws_service else "RTCDataChannel")
        assert right.execute_script("return wire.constructor.name") == ("RTCDataChannel" if args.rtc_bin else "WebSocket")
        left.find_element(By.ID, "start").click()
        wait(left, lambda d: ui(d, "phase") == "Match in play")
        assert frame(left) == frame(right)
        hashes = []
        def confirm(tick):
            a, b = at(left, tick), at(right, tick)
            assert a and len(a) == 64 and a == b, (tick, a, b)
            hashes.append(a)
        key(left, Keys.ARROW_LEFT)
        confirm(1)
        assert frame(left) != frame(right)
        key(right, Keys.ARROW_RIGHT)
        confirm(2)
        # A claimed seat ID is outside the wire grammar. The server binds the sender.
        right.execute_script("wire.send('move 2 4 0')")
        wait(right, lambda d: ui(d, "linkState") == "Unknown command")
        right.execute_script("wire.send('move 4 1')")
        wait(right, lambda d: ui(d, "linkState") == "Sequence gap or queue full")
        # Rebind the original tab identity over a fresh transport.
        old_board = frame(right)
        right.refresh()
        connected(right, 2)
        assert at(right, 2) == hashes[-1] and frame(right) == old_board
        replayed = right.execute_async_script("""
            const done = arguments[arguments.length - 1];
            wire.addEventListener("message", e => done(e.data), {once: true});
            wire.send("move 1 2");
        """)
        fields = replayed.split(" ", 4)
        assert fields[0] == "Z" and fields[3] == hashes[-1], "duplicate changed compressed snapshot"
        # Complete the shared seed-137 clear, allowing for seat 2's tick-2 move.
        script = [Keys.ARROW_LEFT] * 3 + [Keys.SPACE] * 2 + [Keys.ARROW_RIGHT] * 4 + [Keys.SPACE]
        for tick, symbol in enumerate(script, 3):
            key(left, symbol)
            confirm(tick)
        assert "Seat 1 attacked seat 2" in ui(right, "events")
        assert ui(left, "lines") == "1" and ui(left, "score") == "100"
        assert left.execute_script("return document.querySelectorAll('.opponent').length") == args.seats - 1
        tick = 12
        while not ui(left, "phase").startswith("Winner"):
            tick += 1
            assert tick <= 100, "match did not finish"
            if ui(right, "phase").startswith("Eliminated"):
                key(left, Keys.ARROW_LEFT if tick % 2 else Keys.ARROW_RIGHT)
            else:
                key(right, Keys.SPACE)
            confirm(tick)
            if tick == 16 and screenshot:
                assert right.save_screenshot(str(screenshot))
        assert ui(right, "phase") == ui(left, "phase") == "Winner · seat 1"
        assert ui(left, "alive") == "1/" + str(args.seats)
        assert "eliminated" in ui(right, "events")
        return hashes


def snapshot(ws, encoding="F"):
    parts = []
    while True:
        kind, index, count, digest, data = ws.recv(timeout=10).split(" ", 4)
        assert kind == encoding and int(index) == len(parts)
        parts.append(data)
        if len(parts) == int(count):
            payload = "".join(parts).encode("ascii")
            if kind == "Z":
                payload = zlib.decompress(base64.b64decode(payload, validate=True))
            assert hashlib.sha256(payload).hexdigest() == digest
            return json.loads(payload)


def clocked_resume(args, engine):
    from websockets.sync.client import connect

    def columns(state):
        return sorted(i % 10 for i, cell in enumerate(state["seats"][0][0]) if cell != "0")

    with server(args, engine, 350, 2) as origin:
        with urlopen(origin + "/config", timeout=5) as page:
            endpoint = json.load(page)["ws"]
        token = str(uuid.uuid4())
        with connect(endpoint, origin=origin) as first:
            first.send("join " + token)
            assert first.recv(timeout=5) == "seat 0"
            initial = snapshot(first)
            first.send("start")
            first.send("move 1 1")
            first.send("move 1 1")
            # Disconnect with an unacknowledged input. The independent clock keeps running.
        with connect(endpoint, origin=origin) as resumed:
            resumed.send("join " + token)
            assert resumed.recv(timeout=5) == "seat 0"
            snapshot(resumed)
            resumed.send("move 1 1")
            resumed.send("move 2 2")
            state = snapshot(resumed)
            while state["seats"][0][5] < 2:
                assert state["tick"] < 20, "clock failed to apply pending inputs"
                state = snapshot(resumed)
            assert state["tick"] >= 2 and columns(state) == columns(initial)


def full_roster(args, engine):
    from websockets.sync.client import connect

    with contextlib.ExitStack() as stack:
        origin = stack.enter_context(server(args, engine, 0, 99, 99))
        with urlopen(origin + "/config", timeout=5) as page:
            endpoint = json.load(page)["ws"]
        peers = []
        for i in range(99):
            peer = stack.enter_context(connect(endpoint, origin=origin))
            encoding = "Z" if i % 2 else "F"
            peer.send("join " + str(uuid.uuid4()) + (" zlib" if encoding == "Z" else ""))
            assert peer.recv(timeout=5) == f"seat {i}"
            initial = snapshot(peer, encoding)
            peers.append((peer, encoding))
        peers[-1][0].send("start")
        peers[-1][0].send("move 1 1")
        common = None
        for peer, encoding in peers:
            assert snapshot(peer, encoding)["phase"] == "playing"
            state = snapshot(peer, encoding)
            assert state["tick"] == 1 and state["bots"] == 0
            assert state["seats"][:-1] == initial["seats"][:-1]
            assert state["seats"][-1][0] != initial["seats"][-1][0] and state["seats"][-1][5] == 1
            if common is not None:
                assert state == common
            common = state


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ws-service", type=Path)
    parser.add_argument("--rtc-bin", type=Path)
    parser.add_argument("--prime", required=True)
    parser.add_argument("--entry", required=True)
    parser.add_argument("--native", required=True)
    parser.add_argument("--screenshot", type=Path)
    parser.add_argument("--seats", type=int, default=99)
    args = parser.parse_args()
    if not (args.ws_service or args.rtc_bin):
        parser.error("provide at least one adapter")
    interp = scenario(args, [args.prime, "run", args.entry], None)
    native = scenario(args, [args.native], args.screenshot)
    assert interp == native, (interp, native)
    if args.ws_service:
        for command in ([args.prime, "run", args.entry], [args.native]):
            clocked_resume(args, command)
            full_roster(args, command)
        print("Both backends passed timed reconnect plus 99 simultaneous WS players, 0 bots and 0 browser windows.")
    print(f"2 Chromium processes per backend, 2 transport peers, {args.seats} seats ({args.seats - 2} bots), "
          f"{len(native)} matching frames; final sha256 {native[-1]}")
