#!/usr/bin/env python3
"""Exercise real Chromium peer connections against Prime run and native on loopback."""
import argparse
import contextlib
import json
import os
from pathlib import Path
import re
import select
import shutil
import signal
import socket
import subprocess
import tempfile
import time
from urllib.error import HTTPError, URLError
from urllib.request import Request, build_opener, ProxyHandler

ROOT = Path(__file__).resolve().parents[2]
ENTRY = ROOT / "workspace/demos/webrtc/game.prime"
HTTP = build_opener(ProxyHandler({}))


def request(method, url, value=None, timeout=25):
    data = json.dumps(value).encode() if value is not None else None
    req = Request(url, data=data, method=method, headers={"Content-Type": "application/json"})
    try:
        with HTTP.open(req, timeout=timeout) as response:
            return json.load(response)
    except HTTPError as error:
        raise RuntimeError(f"{method} {url}: {error.code} {error.read().decode()}") from error


def stop(process):
    if process.poll() is None:
        process.terminate()
        try:
            process.wait(timeout=6)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait()
            raise


@contextlib.contextmanager
def service(adapter, cwd, command, interrupt):
    child = subprocess.Popen([adapter, "--", *command], cwd=cwd, stdout=subprocess.PIPE,
                             text=True, start_new_session=True)
    try:
        if not select.select([child.stdout], [], [], 8)[0]:
            raise RuntimeError("adapter failed to announce a port")
        line = child.stdout.readline().strip()
        if not re.fullmatch(r"LISTEN http://127\.0\.0\.1:\d+", line):
            raise RuntimeError(f"unexpected startup: {line!r}")
        yield line.removeprefix("LISTEN ")
    finally:
        if interrupt and child.poll() is None:
            os.killpg(child.pid, signal.SIGINT)
            try:
                child.wait(timeout=6)
            finally:
                stop(child)
        else:
            stop(child)
        child.stdout.close()
    assert child.returncode == 0, child.returncode


class Browser:
    def __init__(self, session):
        self.session = session

    def post(self, path, data):
        return request("POST", self.session + path, data)["value"]

    def js(self, script, *args):
        body = (
            "const done = arguments[arguments.length - 1]; const a = arguments[0];"
            "(async () => {" + script + "})().then(value => done({value}),"
            "error => done({error: String(error.stack || error)}));"
        )
        result = self.post("/execute/async", {"script": body, "args": [args]})
        if "error" in result:
            raise AssertionError(result["error"])
        return result.get("value")

    def switch(self, handle):
        self.post("/window", {"handle": handle})

    def tab(self):
        handle = self.post("/window/new", {"type": "tab"})["handle"]
        self.switch(handle)
        return handle


@contextlib.contextmanager
def chromium(binary, driver):
    with socket.socket() as sock:
        sock.bind(("127.0.0.1", 0))
        port = sock.getsockname()[1]
    with tempfile.TemporaryDirectory() as profile:
        process = subprocess.Popen([driver, f"--port={port}"], stdout=subprocess.DEVNULL)
        base = f"http://127.0.0.1:{port}"
        try:
            deadline = time.monotonic() + 5
            while True:
                try:
                    request("GET", base + "/status", timeout=1)
                    break
                except URLError:
                    if process.poll() is not None or time.monotonic() > deadline:
                        raise
                    time.sleep(0.05)
            caps = {"browserName": "chrome", "goog:chromeOptions": {
                "binary": binary, "args": ["--headless", "--disable-gpu", "--disable-dev-shm-usage",
                    "--autoplay-policy=no-user-gesture-required", "--disable-background-timer-throttling",
                    "--disable-renderer-backgrounding", f"--user-data-dir={profile}"]}}
            session = request("POST", base + "/session", {"capabilities": {"alwaysMatch": caps}})["value"]
            print("Chromium", session["capabilities"]["browserVersion"], flush=True)
            url = base + "/session/" + session["sessionId"]
            try:
                browser = Browser(url)
                browser.post("/timeouts", {"script": 20000})
                yield browser
            finally:
                request("DELETE", url)
        finally:
            stop(process)


# Use a same-origin document without the interactive page's automatic connection.
# The public JS helper installs onmessage before the channel opens.
CONNECT = """
const {connectPrime} = await import("/browser.js");
window.inbox = [];
window.take = async () => {
  const start = performance.now();
  while (!inbox.length) {
    if (performance.now() - start > 5000) throw Error("Prime response timed out");
    await new Promise(r => setTimeout(r, 20));
  }
  return inbox.shift();
};
const tracks = [];
if (a[0] === "audio") {
  const audio = new AudioContext();
  const destination = audio.createMediaStreamDestination();
  const osc = audio.createOscillator();
  osc.connect(destination); osc.start(); await audio.resume();
  tracks.push(...destination.stream.getAudioTracks());
  window.synthetic = {audio, osc, destination};
}
if (a[0] === "video") {
  const canvas = document.createElement("canvas");
  canvas.width = canvas.height = 64;
  const ctx = canvas.getContext("2d");
  let i = 0;
  setInterval(() => {
    ctx.fillStyle = ++i % 2 ? "red" : "blue"; ctx.fillRect(0, 0, 64, 64);
  }, 50);
  tracks.push(...canvas.captureStream(20).getVideoTracks());
}
window.rtc = await connectPrime(({data}) => inbox.push(
  typeof data === "string" ? data : Array.from(new Uint8Array(data))), tracks);
return await take();
"""


def connect(browser, origin, media=""):
    browser.post("/url", {"url": origin + "/browser.js"})
    welcome = browser.js(CONNECT, media)
    assert re.fullmatch(r"joined [1-9][0-9]*", welcome), welcome
    return int(welcome.split()[1])


def roundtrip(browser, value, expected):
    got = browser.js("rtc.dc.send(Array.isArray(a[0]) ? Uint8Array.from(a[0]) : a[0]);"
                     "return await take();", value)
    assert got == expected, (got, expected)


def stats_after_close(browser, moves, closed):
    expected = f"moves {moves} closed {closed}"
    deadline = time.monotonic() + 13
    while True:
        value = browser.js('rtc.dc.send("stats"); return await take()')
        if value == expected:
            return
        if time.monotonic() >= deadline:
            raise AssertionError((value, expected))
        time.sleep(0.1)


def expect_closed(browser):
    browser.js("""
      if (rtc.dc.readyState === "closed") return;
      await new Promise((resolve, reject) => {
        const timer = setTimeout(() => reject(Error("channel close timed out")), 10000);
        rtc.dc.addEventListener("close", () => { clearTimeout(timer); resolve(); }, { once: true });
      });
    """)


def data_check(browser, origin):
    a = browser.tab()
    first = connect(browser, origin)
    transport = browser.js("""
      const stats = await rtc.pc.getStats();
      const dtls = [...stats.values()].find(r => r.type === "transport" && r.selectedCandidatePairId);
      const pair = stats.get(dtls.selectedCandidatePairId);
      const remote = stats.get(pair.remoteCandidateId);
      return [dtls.dtlsState, rtc.pc.sctp.state, pair.state, remote.protocol, remote.address];
    """)
    assert transport == ["connected", "connected", "succeeded", "udp", "127.0.0.1"], transport
    print("pass: connected DTLS/SCTP, selected UDP candidate 127.0.0.1", flush=True)
    b = browser.tab()
    second = connect(browser, origin)
    assert first != second
    browser.switch(a)
    roundtrip(browser, "move", "moves 1")
    browser.switch(b)
    roundtrip(browser, "move", "moves 2")
    for text in ("", "igra 🎮", " \tline\nnext\r\n\0 "):
        roundtrip(browser, text, text)
    for data in ([], [0, 13, 10, 127, 128, 255], list(range(256)) * 32):
        roundtrip(browser, data, data)
    browser.switch(a)
    browser.post("/refresh", {})
    browser.switch(b)
    stats_after_close(browser, 2, 1)
    browser.switch(a)
    third = connect(browser, origin)
    assert third > max(first, second)
    roundtrip(browser, "move", "moves 3")
    browser.js('rtc.dc.send("bye");')
    expect_closed(browser)
    browser.switch(b)
    stats_after_close(browser, 3, 2)
    # One invalid client must leave the other peer and the Prime game running.
    browser.switch(a)
    connect(browser, origin)
    browser.js("rtc.dc.send(new Uint8Array(8193));")
    browser.switch(b)
    stats_after_close(browser, 3, 3)
    roundtrip(browser, "move", "moves 4")
    print("pass: two tabs, UTF-8 and binary, page refresh, Prime close, size limit, shared moves 1..4", flush=True)
    return b


def page_check(browser, origin):
    browser.tab()
    browser.post("/url", {"url": origin})
    result = browser.js("""
      const button = document.querySelector("button");
      const log = document.querySelector("pre");
      async function wait(test) {
        const end = performance.now() + 6000;
        while (!test()) {
          if (performance.now() > end) throw Error("page did not show a response: " + log.innerText);
          await new Promise(r => setTimeout(r, 25));
        }
      }
      await wait(() => !button.disabled && log.innerText.includes("joined "));
      document.querySelector("input").value = "move";
      button.click();
      await wait(() => log.innerText.includes("moves "));
      return log.innerText;
    """)
    assert "moves 5" in result.splitlines(), result
    print("pass: bundled page displayed the Prime response moves 5", flush=True)


def media_check(browser, origin, kind):
    browser.tab()
    connect(browser, origin, kind)
    codec = browser.js("return await take()")
    assert codec.lower().startswith("media " + kind + "/"), codec
    browser.js("rtc.close();")
    print(f"pass: synthetic {kind} RTP received by Pion, Prime reported {codec}", flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("prime", type=Path)
    parser.add_argument("adapter", type=Path)
    parser.add_argument("--chromium", default=shutil.which("chromium"))
    parser.add_argument("--driver", default=shutil.which("chromedriver"))
    args = parser.parse_args()
    if not args.chromium or not args.driver:
        parser.error("install matching Chromium and chromedriver")
    cli, adapter = args.prime.resolve(), args.adapter.resolve()
    with tempfile.TemporaryDirectory() as work:
        subprocess.run([cli, "build", ENTRY, "--name", "rtc_game"], cwd=work, check=True, timeout=300)
        native = Path(work) / ".build.prime/rtc_game/rtc_game"
        for label, command in [("run", [cli, "run", ENTRY]), ("native", [native])]:
            print(label, flush=True)
            with chromium(args.chromium, args.driver) as browser:
                with service(adapter, work, command, interrupt=label == "native") as origin:
                    survivor = data_check(browser, origin)
                    for kind in ("audio", "video"):
                        media_check(browser, origin, kind)
                    page_check(browser, origin)
                    browser.switch(survivor)
                    roundtrip(browser, "move", "moves 6")
                expect_closed(browser)
                how = "terminal SIGINT" if label == "native" else "SIGTERM"
                print(f"pass: {how} closed a live browser channel and Prime exited", flush=True)


if __name__ == "__main__":
    main()
