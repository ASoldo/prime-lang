"""Play a complete all-client match through the loopback Riftwell launcher."""
import argparse
import asyncio
import base64
import contextlib
import hashlib
import json
import os
from pathlib import Path
import shutil
import socket
import statistics
import struct
import sys
import time
from urllib.request import urlopen
import uuid
import zlib

from websockets.asyncio.client import connect

LAUNCHER = Path(__file__).resolve().parents[3] / "scripts" / "play_riftwell.sh"
SEATS = 99


@contextlib.asynccontextmanager
async def launch(cli, mode):
    args = ["--port", "0", "--seats", "99", "--humans", "99", "--seed", "137", "--lockstep"]
    if mode == "interpreter":
        args.append("--interpret")
    proc = await asyncio.create_subprocess_exec(LAUNCHER, *args,
        env={**os.environ, "PRIME_BIN": cli}, stdout=asyncio.subprocess.PIPE)
    try:
        async with asyncio.timeout(180):
            while line := await proc.stdout.readline():
                if line.startswith(b"ARENA "):
                    break
            else:
                raise RuntimeError(f"launcher exited with {await proc.wait()}")
        origin = line.decode().split()[1]
        yield origin
    finally:
        if proc.returncode is None:
            proc.terminate()
        await asyncio.wait_for(proc.wait(), 15)
        if proc.returncode != 0:
            raise RuntimeError(f"launcher exited with {proc.returncode}")


async def receive(ws, expected_format):
    parts = []
    async with asyncio.timeout(90):
        while True:
            kind, index, count, digest, part = (await ws.recv()).split(" ", 4)
            assert kind == expected_format and int(index) == len(parts)
            if not parts:
                key = (kind, count, digest)
            assert key == (kind, count, digest) and 0 < int(count) <= 99
            parts.append(part)
            if len(parts) == int(count):
                break
    payload = "".join(parts).encode("ascii")
    if kind == "Z":
        payload = zlib.decompress(base64.b64decode(payload, validate=True))
    assert hashlib.sha256(payload).hexdigest() == digest
    return json.loads(payload), digest


async def all_agree(peers, fmt):
    results = await asyncio.gather(*(receive(ws, fmt) for ws in peers))
    first = results[0]
    assert all(result == first for result in results[1:]), "clients reconstructed different snapshots"
    return first


def tcp_bytes(peers):
    # Linux UAPI struct tcp_info uses byte offsets 128 and 200 for these two u64s.
    # These per-socket kernel counters include the WS framing and client masking.
    tx = rx = 0
    for ws in peers:
        sock = ws.transport.get_extra_info("socket")
        info = sock.getsockopt(socket.IPPROTO_TCP, socket.TCP_INFO, 208)
        rx += struct.unpack_from("=Q", info, 128)[0]  # tcpi_bytes_received
        tx += struct.unpack_from("=Q", info, 200)[0]  # tcpi_bytes_sent
    return tx, rx


async def play(origin, mode, fmt):
    with urlopen(origin + "/config", timeout=5) as page:
        endpoint = json.load(page)["ws"]
    async with contextlib.AsyncExitStack() as stack:
        peers = []
        for slot in range(SEATS):
            ws = await stack.enter_async_context(connect(endpoint, origin=origin, compression=None,
                                                         ping_interval=None, proxy=None))
            await ws.send("join " + str(uuid.uuid4()) + (" zlib" if fmt == "Z" else ""))
            assert await asyncio.wait_for(ws.recv(), 10) == f"seat {slot}"
            await receive(ws, fmt)
            peers.append(ws)
        await peers[0].send("start")
        state, initial = await all_agree(peers, fmt)
        assert state["tick"] == 0 and state["alive"] == SEATS and state["bots"] == 0
        path, elapsed = [initial], []
        tx0, rx0 = tcp_bytes(peers)
        t0 = time.perf_counter()
        while state["phase"] == "playing":
            start = time.perf_counter()
            tick = state["tick"] + 1
            assert tick <= 100, "match failed to finish"
            active = [i for i, seat in enumerate(state["seats"]) if seat[3]]
            await asyncio.gather(*(peers[i].send(f"move {tick} {0 if i == SEATS - 1 else 4}") for i in active))
            state, digest = await all_agree(peers, fmt)
            assert state["tick"] == tick and all(state["seats"][i][5] == tick for i in active)
            path.append(digest)
            elapsed.append((time.perf_counter() - start) * 1000)
        total = time.perf_counter() - t0
        tx1, rx1 = tcp_bytes(peers)
        ranks = [seat[4] for seat in state["seats"]]
        assert state["phase"] == "finished" and state["alive"] == 1 and state["winner"] == SEATS - 1
        assert ranks == list(range(SEATS, 0, -1)), ranks
        print(f"{mode} {fmt}: ticks={len(elapsed)} play_s={total:.3f} mean_ms={statistics.mean(elapsed):.2f} "
              f"median_ms={statistics.median(elapsed):.2f} max_ms={max(elapsed):.2f} "
              f"play_tx={tx1-tx0} play_rx={rx1-rx0} through_final_tx={tx1} through_final_rx={rx1}",
              flush=True)
        return path, ranks


async def verify(args):
    if args.origin:
        path, _ = await play(args.origin, "published", "Z")
        expected_digest = "dd9e188c63e410a64633b438737161a634b49c8fb2d36ccb7f3aaf4bbfa59960"
        assert path[-1] == expected_digest, path[-1]
        print(f"99 connected players agreed on all ten ticks and ranks 99..1, final_sha256={path[-1]}")
        return
    expected = None
    for fmt in (["F", "Z"] if args.compare_plain else ["Z"]):
        for mode in ("native", "interpreter"):
            async with launch(args.prime, mode) as origin:
                result = await play(origin, mode, fmt)
            if expected is not None:
                assert result == expected, "backend or encoding changed authoritative replay or standings"
            else:
                expected = result
    print(f"99 WS connections, 99 input clients, 0 simulated bots, 0 browser windows; "
          f"all recipients agreed each tick; winner seat 99, ranks by seat 99..1; final_sha256={expected[0][-1]}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    source = parser.add_mutually_exclusive_group()
    source.add_argument("--origin", help="play a prestarted seed-137, 99-human lockstep match")
    source.add_argument("--prime", default=os.environ.get("PRIME_BIN", "prime-lang"))
    parser.add_argument("--compare-plain", action="store_true", help="also measure the plain snapshot protocol")
    args = parser.parse_args()
    if sys.platform != "linux":
        parser.error("requires Linux TCP_INFO")
    if args.origin and args.compare_plain:
        parser.error("each encoding requires a fresh lobby")
    if not args.origin:
        cli = shutil.which(args.prime)
        if not cli:
            parser.error("requires prime-lang on PATH or --prime")
        args.prime = str(Path(cli).resolve())
    asyncio.run(verify(args))
