#!/usr/bin/env python3
"""Exercise the ordinary Prime CLI and generated executable on IPv4 loopback."""
import argparse
import contextlib
import errno
import json
import os
from pathlib import Path
import pty
import re
import select
import socket
import subprocess
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]
ENTRY = ROOT / "workspace/demos/web_server/web_server.prime"
COUNT = 64


def listening_port(master):
    output = bytearray()
    deadline = time.monotonic() + 8
    while time.monotonic() < deadline:
        ready, _, _ = select.select([master], [], [], max(0, deadline - time.monotonic()))
        if not ready:
            break
        try:
            data = os.read(master, 4096)
        except OSError as exc:
            if exc.errno != errno.EIO:
                raise
            break
        if not data:
            break
        output.extend(data)
        bound = re.search(rb"LISTEN (\d+)\r?\n", output)
        if bound:
            return int(bound[1])
    raise RuntimeError(f"server did not announce its port: {output!r}")


def connect(port):
    return socket.create_connection(("127.0.0.1", port), timeout=4)


def expect_response(conn, path):
    status, body = (b"200 OK", b"ok\n") if path == "/health" else (b"404 Not Found", b"not found\n")
    expected = (b"HTTP/1.1 " + status + b"\r\nContent-Length: " + str(len(body)).encode()
                + b"\r\nConnection: close\r\n\r\n" + body)
    response = bytearray()
    while len(response) <= len(expected):
        data = conn.recv(1024)
        if not data:
            break
        response.extend(data)
    if response != expected:
        raise RuntimeError(f"{path}: expected {expected!r}, received {response!r}")


def request(port, path, version):
    with connect(port) as conn:
        headers = f"GET {path} HTTP/{version}\r\nHost: localhost\r\n"
        # Send a complete, ordinary header block across the 512-byte read boundary.
        padded = headers + "X-Note: " + "a" * (512 - len(headers) - len("X-Note: ") - 2)
        conn.sendall((padded + "\r\n\r\n").encode())
        expect_response(conn, path)


def overlap(port):
    with connect(port) as slow:
        slow.sendall(b"GET /missing HTTP/1.1\r\nHost: localhost\r\n")
        request(port, "/health", "1.1")
        slow.sendall(b"\r\n")
        expect_response(slow, "/missing")


def fill_and_reuse(port):
    with contextlib.ExitStack() as stack:
        clients = [stack.enter_context(connect(port)) for _ in range(4)]
        for conn in clients:
            conn.sendall(b"GET /missing HTTP/1.1\r\n")
        with connect(port) as queued:
            queued.sendall(b"GET /health HTTP/1.0\r\n\r\n")
            if select.select([queued], [], [], 0.15)[0]:
                raise RuntimeError("fifth request completed while all four slots were occupied")
            clients[0].sendall(b"\r\n")
            expect_response(clients[0], "/missing")
            expect_response(queued, "/health")
        for conn in clients[1:]:
            conn.sendall(b"\r\n")
            expect_response(conn, "/missing")


def expire_partial(port):
    with connect(port) as conn:
        conn.sendall(b"GET /health HTTP/1.1\r\nX-Partial: ")
        # Activity near the deadline must not buy another two seconds.
        time.sleep(1.1)
        conn.sendall(b"x")
        conn.settimeout(1.6)
        if conn.recv(1):
            raise RuntimeError("incomplete timed-out header received a response")


def exercise(command, cwd, env, paths):
    # A terminal makes the CLI's port announcement visible before server exit.
    master, slave = pty.openpty()
    with contextlib.ExitStack() as stack:
        stack.callback(os.close, master)
        stack.callback(os.close, slave)
        server = subprocess.Popen(command, cwd=cwd, env=env, stdin=subprocess.PIPE,
                                  stdout=slave, stderr=slave)
        try:
            server.stdin.write(f"0\n{len(paths) + 10}\n".encode())
            server.stdin.close()
            port = listening_port(master)
            overlap(port)
            with connect(port) as conn:
                conn.sendall(b"GET /health HTTP/1.1")
                conn.shutdown(socket.SHUT_WR)
                if conn.recv(1):
                    raise RuntimeError("truncated header received a response")
            with connect(port) as conn:
                conn.sendall(b"x" * 4096)
                if conn.recv(1):
                    raise RuntimeError("oversized header received a response")
            for i, path in enumerate(paths):
                request(port, path, "1.0" if i % 2 else "1.1")
            expire_partial(port)
            fill_and_reuse(port)
            if server.wait(timeout=3) != 0:
                raise RuntimeError(f"server exited with {server.returncode}")
        finally:
            server.kill()
            server.wait()


def check_rejections(cli, cwd, env):
    project = cwd / "invalid"
    project.mkdir()
    manifest = project / "prime.toml"
    manifest.write_text('manifest_version = "3"\n[module]\nname = "probe"\npath = "main.prime"\n')
    entry = project / "main.prime"
    for call, expected in [('tcp_accept(1)', 'expects 0 arguments'),
                           ('tcp_write(42)', 'string'),
                           ('tcp_next_header()', 'expects 1 argument'),
                           ('tcp_next_header("bad")', 'int32'),
                           ('tcp_next_header(0)', 'requires the host std runtime'),
                           ('tcp_listen(0)', 'requires the host std runtime')]:
        if call == 'tcp_next_header(0)':
            with manifest.open('a') as output:
                output.write('no_std = true\n')
        entry.write_text(f"module probe;\nfn main() {{ {call}; }}\n")
        result = subprocess.run([cli, "check", entry, "--format", "json"], cwd=project,
                                env=env, capture_output=True, text=True, timeout=10)
        diagnostics = json.loads(result.stdout)["diagnostics"]
        if result.returncode == 0 or not any(
                expected in d["message"] and (expected != "requires the host std runtime" or d["code"] == "E0NS")
                for d in diagnostics
        ):
            raise RuntimeError(f"{call}: {result.stdout}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("cli", type=Path, help="path to prime-lang")
    cli = parser.parse_args().cli.resolve(strict=True)
    env = os.environ.copy()
    env.pop("PRIME_TEST_INPUTS", None)
    env.pop("PRIME_TEST_INPUTS_FILE", None)
    paths = (["/health", "/missing", "/health?x=1", "/health"] * (COUNT // 4))
    with tempfile.TemporaryDirectory() as tmp:
        cwd = Path(tmp)
        check_rejections(cli, cwd, env)
        subprocess.run([cli, "check", ENTRY, "--format", "json"], cwd=cwd, env=env,
                       check=True, timeout=15)
        exercise([cli, "run", ENTRY], cwd, env, paths)
        subprocess.run([cli, "build", ENTRY, "--name", "prime_web"], cwd=cwd, env=env,
                       stdin=subprocess.DEVNULL, check=True, timeout=600)
        binary = cwd / ".build.prime/prime_web/prime_web"
        exercise([binary], cwd, env, paths)
        exercise([binary], cwd, env, paths[1:] + paths[:1])
    print(f"Passed overlap, saturation, reuse, expiry and {COUNT} route requests per run; interpreter and two runs of one native binary exited cleanly.")


if __name__ == "__main__":
    main()
