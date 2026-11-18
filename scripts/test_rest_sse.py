#!/usr/bin/env python3
"""Exercise the same Prime state machine in run and native with real HTTP/1.1 clients."""
import argparse
from concurrent.futures import ThreadPoolExecutor
from contextlib import ExitStack, contextmanager
import http.client
import json
import os
from pathlib import Path
import select
import shutil
import socket
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[1]
ENTRY = ROOT / "workspace/demos/rest_sse/rest_sse.prime"


@contextmanager
def response(port, method="GET", path="/state", body=None, headers=None):
    conn = http.client.HTTPConnection("127.0.0.1", port, timeout=4)
    try:
        conn.request(method, path, body, headers or {})
        with conn.getresponse() as reply:
            yield reply
    finally:
        conn.close()


def request(port, status=200, **kwargs):
    with response(port, **kwargs) as reply:
        data = json.load(reply)
        assert reply.status == status, (reply.status, data)
        if status >= 400:
            assert data == {"error": reply.reason}
        if status == 405:
            assert "GET" in reply.getheader("Allow").split(", ")
        return data


def set_value(port, value):
    return request(port, method="POST", body=json.dumps({"value": value}),
                   headers={"Content-Type": "application/json"})


def event(stream):
    fields = {}
    while True:
        line = stream.readline(2048)
        assert line.endswith(b"\n"), line
        line = line.decode("utf-8").strip()
        if not line and fields:
            data = json.loads(fields["data"])
            assert data["label"] == "očitanje" and str(data["id"]) == fields["id"]
            return fields["event"], data["id"], data["value"]
        if line and not line.startswith(":"):
            key, value = line.split(": ", 1)
            fields[key] = value


def exercise(port):
    assert request(port) == {"id": 0, "value": 0}
    for body, code in [('{"value":1.5}', 400), ('{"VALUE":1}', 400), ('{}', 400),
                       ('{"value":-1}', 422), ('{"value":1001}', 422), ('1', 400),
                       ('{"value":0,"value":1}', 400), ('{"value":7}[]', 400),
                       ('{"value":true}', 400), ('{"value":1', 400), (' ' * 1025, 413)]:
        request(port, code, method="POST", body=body, headers={"Content-Type": "application/json"})
    request(port, 405, method="PUT")
    request(port, 405, method="POST", path="/events")
    request(port, 404, path="/absent")
    request(port, 415, method="POST", body='{"value":1}')
    request(port, 400, body="x")
    request(port, 400, path="/events", headers={"Last-Event-ID": "NaN"})

    with ExitStack() as stack:
        streams = [stack.enter_context(response(port, path="/events")) for _ in range(4)]
        for stream in streams:
            assert stream.status == 200 and event(stream) == ("reset", 0, 0)
        request(port, 503, path="/events")
        # Leave four streams unread while requests overlap on independent sockets.
        with ThreadPoolExecutor(max_workers=8) as pool:
            results = list(pool.map(lambda v: set_value(port, v), range(24)))
        assert sorted(item["id"] for item in results) == list(range(1, 25))
        assert [item["value"] for item in results] == list(range(24))
        latest = max(results, key=lambda item: item["id"])
        assert request(port) == latest
    by_id = {item["id"]: item["value"] for item in results}
    for cursor, expected in [("20", [("promjena", i, by_id[i]) for i in range(21, 25)]),
                             ("0", [("reset", 24, latest["value"])]),
                             ("900", [("reset", 24, latest["value"])])]:
        with response(port, path="/events", headers={"Last-Event-ID": cursor}) as stream:
            assert [event(stream) for _ in expected] == expected


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("cli", type=Path)
    parser.add_argument("--chromium", default=None, help="Chromium executable (auto-detected by default)")
    args = parser.parse_args()
    chrome = args.chromium or shutil.which("chromium") or shutil.which("google-chrome")
    if not chrome:
        parser.error("provide --chromium for the EventSource regression")
    cli = args.cli.resolve(strict=True)
    env = dict(os.environ, GOPROXY="off", GOTOOLCHAIN="local", CARGO_NET_OFFLINE="true")
    for name in ("PRIME_TEST_INPUTS", "PRIME_TEST_INPUTS_FILE"):
        env.pop(name, None)
    with tempfile.TemporaryDirectory() as temp:
        cwd = Path(temp)
        adapter = cwd / "adapter"
        subprocess.run(["go", "build", "-trimpath", "-o", adapter, "."], cwd=ROOT / "adapters/rest_sse",
                       env=env, check=True, timeout=180)
        subprocess.run([cli, "build", ENTRY, "--name", "api"], cwd=cwd, env=env, check=True, timeout=600)
        for name, command in [("run", [cli, "run", ENTRY]), ("native", [cwd / ".build.prime/api/api"])]:
            server = subprocess.Popen([adapter, "--", *command], cwd=cwd, env=env, stdout=subprocess.PIPE)
            try:
                assert select.select([server.stdout], [], [], 15)[0], "startup timed out"
                line = server.stdout.readline().split()
                assert len(line) == 2 and line[0] == b"LISTEN", line
                port = int(line[1])
                exercise(port)
                subprocess.run(["node", ROOT / "scripts/tests/rest_sse/eventsource.mjs", chrome,
                                cwd / name, f"http://127.0.0.1:{port}"], check=True, timeout=40)
                server.terminate()
                assert server.wait(timeout=6) == 0
                with socket.socket() as probe:
                    assert probe.connect_ex(("127.0.0.1", port)) != 0
                print(f"{name}: concurrent REST, validation, replay, Chromium reconnect, clean stop passed")
            finally:
                if server.poll() is None:
                    server.terminate()
                    try:
                        server.wait(timeout=6)
                    except subprocess.TimeoutExpired:
                        server.kill()
                        server.wait()
                server.stdout.close()


if __name__ == "__main__":
    main()
