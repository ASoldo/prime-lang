#!/usr/bin/env python3
"""Exercise the host socket API against Python's TCP and UDP implementations."""
import argparse
import json
import os
import re
from pathlib import Path
import socket
import subprocess
import tempfile
import time

ENTRY = Path(__file__).resolve().parent / "tests/host_sockets/sockets.prime"


def endpoint(host, port):
    return f"[{host}]:{port}" if ":" in host else f"{host}:{port}"


def receive(sock, size):
    data = bytearray()
    while len(data) < size:
        part = sock.recv(size - len(data))
        if not part:
            raise EOFError(bytes(data))
        data.extend(part)
    return data


def exercise(command, directory, host, protocol):
    ready = directory / "ready"
    ready.unlink(missing_ok=True)
    family = socket.AF_INET6 if ":" in host else socket.AF_INET
    tcp = protocol == "tcp"
    with socket.socket(family, socket.SOCK_STREAM if tcp else socket.SOCK_DGRAM) as server:
        server.settimeout(4)
        server.bind((host, 0))
        if tcp:
            server.listen()
        env = os.environ.copy()
        env.pop("PRIME_TEST_INPUTS", None)
        env.pop("PRIME_TEST_INPUTS_FILE", None)
        with subprocess.Popen(command, env=env, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                              stderr=subprocess.STDOUT, cwd=directory, text=True) as proc:
            try:
                dest = endpoint(host, server.getsockname()[1])
                proc.stdin.write(f"{1 if tcp else 2}\n{endpoint(host, 0)}\n{dest}\n")
                proc.stdin.close()
                deadline = time.monotonic() + 4
                while time.monotonic() < deadline and proc.poll() is None:
                    if ready.exists() and ready.stat().st_size:
                        break
                    time.sleep(0.005)
                assert ready.exists(), "missing bound endpoint"
                ip, port = ready.read_text().rsplit(":", 1)
                assert ip.strip("[]") == host
                bound = (host, int(port))
                with socket.socket(family, server.type) as client:
                    client.settimeout(4)
                    if tcp:
                        client.connect(bound)
                        conn, _ = server.accept()
                        with conn:
                            conn.settimeout(4)
                            assert receive(conn, 2) == b"\x00\xff"
                            client.sendall(b"\xf8")
                            client.shutdown(socket.SHUT_WR)
                            assert receive(conn, 1) == b"\xf8"
                            conn.sendall(b"\xfe")
                            assert receive(client, 1) == b"\xfe"
                            assert conn.recv(1) == client.recv(1) == b""
                    else:
                        data, source = server.recvfrom(8)
                        assert data == b"\x00\xff"
                        assert source[:2] != bound
                        server.sendto(b"\xff\x00", source)
                        assert server.recvfrom(8) == (b"\xff\x00", source)
                        for data in [b"oversize", b"\xfc\x00", b""]:
                            client.sendto(data, bound)
                        for data in [b"\xfc\x00", b""]:
                            reply, source = client.recvfrom(8)
                            assert (reply, source[:2]) == (data, bound)
                assert proc.wait(timeout=4) == 0
                output = proc.stdout.read()
                assert output.strip() == "PASS", output
            except BaseException:
                proc.kill()
                proc.wait()
                print(proc.stdout.read(), flush=True)
                raise


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("cli", type=Path)
    cli = parser.parse_args().cli.resolve(strict=True)
    with tempfile.TemporaryDirectory() as tmp:
        directory = Path(tmp)
        subprocess.run([cli, "check", ENTRY], cwd=directory, check=True)
        expected = set(re.findall(r"\b(net_\w+)\(", ENTRY.read_text()))
        for target in ["riscv32imc-unknown-none-elf", "xtensa-esp32-none-elf"]:
            rv = subprocess.run([cli, "check", ENTRY, "--format", "json"], cwd=directory,
                                capture_output=True, env={**os.environ, "PRIME_TARGET": target})
            diagnostics = json.loads(rv.stdout)["diagnostics"]
            rejected = {name for name in expected if any(
                d["code"] == "E0NS" and f"`{name}`" in d["message"] for d in diagnostics
            )}
            assert rv.returncode and rejected == expected, diagnostics
            print(f"{target} E0NS for all {len(rejected)} socket operations", flush=True)
        invalid = directory / "invalid.prime"
        invalid.write_text(
            'module invalid; fn main() { net_tcp_connect(42); net_udp_bind(7); '
            'let []int32 words = [1]; net_tcp_write(1, words, 10); }'
        )
        rv = subprocess.run([cli, "check", invalid, "--format", "json"], cwd=directory,
                            capture_output=True)
        messages = [d["message"] for d in json.loads(rv.stdout)["diagnostics"]]
        assert rv.returncode and any("expects 2 arguments" in m for m in messages), messages
        assert any("string" in m for m in messages), messages
        assert any("uint8" in m for m in messages), messages
        restricted = directory / "restricted"
        restricted.mkdir()
        (restricted / "prime.toml").write_text(
            'manifest_version = "3"\n[module]\nname = "local"\npath = "local.prime"\nno_std = true\n'
        )
        source = restricted / "local.prime"
        source.write_text('module local; fn main() { net_udp_bind("127.0.0.1:0"); }')
        rv = subprocess.run([cli, "check", source, "--format", "json"], cwd=restricted,
                            capture_output=True)
        check = json.loads(rv.stdout)
        assert rv.returncode and check["schema_version"] == 1, check
        assert any(d["code"] == "E0NS" for d in check["diagnostics"]), check
        subprocess.run([cli, "build", ENTRY, "--name", "sockets"], cwd=directory,
                       stdin=subprocess.DEVNULL, check=True, timeout=600)
        binary = directory / ".build.prime/sockets/sockets"
        hosts = ["127.0.0.1"]
        try:
            with socket.socket(socket.AF_INET6) as probe:
                probe.bind(("::1", 0))
        except OSError as exc:
            print(f"IPv6 loopback unavailable: {exc}")
        else:
            hosts.append("::1")
        for mode, command in [("run", [cli, "run", ENTRY]), ("native", [binary])]:
            for host in hosts:
                for proto in ["tcp", "udp"]:
                    exercise(command, directory, host, proto)
                    print(f"{mode} {host} {proto} passed", flush=True)


if __name__ == "__main__":
    main()
