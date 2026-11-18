#!/usr/bin/env python3
"""Measure concurrent RTC channels, sequenced Prime replies and relay/media return on loopback."""
import argparse
import contextlib
import json
from pathlib import Path
import select
import shutil
import subprocess
import tempfile
import time

from test_browser import ENTRY, chromium, connect, expect_closed, roundtrip, service, stop

COUNTER = Path(__file__).parent.resolve() / "testdata/counter/counter.prime"


@contextlib.contextmanager
def relay(binary, work):
    config = Path(work) / "private-ice.json"
    process = subprocess.Popen([binary, config], stdout=subprocess.PIPE, text=True)
    try:
        if not select.select([process.stdout], [], [], 8)[0] or process.stdout.readline() != "READY\n":
            raise RuntimeError("local TURN fixture did not start")
        assert config.stat().st_mode & 0o777 == 0o600
        yield config
    finally:
        started = time.monotonic()
        stop(process)
        process.stdout.close()
        print(f"TURN fixture exit in {time.monotonic()-started:.3f}s", flush=True)
    assert process.returncode == 0 and not config.exists()


def relay_media(browser, origin, config, kind, moves):
    browser.tab()
    connect(browser, origin, kind, config, return_media=True)
    codec = browser.js("return await take()")
    assert codec.lower() == "media " + {"audio": "audio/opus", "video": "video/vp8"}[kind], codec
    roundtrip(browser, "move", f"moves {moves}")
    roundtrip(browser, [0, 255, 42], [0, 255, 42])
    report = browser.js("""
      const kind = a[0];
      const measure = async () => {
        const stats = await rtc.pc.getStats();
        const dtls = [...stats.values()].find(r => r.type === "transport" && r.selectedCandidatePairId);
        const pair = stats.get(dtls.selectedCandidatePairId);
        const source = stats.get(pair.localCandidateId), dest = stats.get(pair.remoteCandidateId);
        const inbound = [...stats.values()].find(r => r.type === "inbound-rtp" && r.kind === kind);
        const outbound = [...stats.values()].find(r => r.type === "outbound-rtp" && r.kind === kind);
        return {pair: [source.candidateType, dest.candidateType], state: [dtls.dtlsState, pair.state],
          rx: inbound?.packetsReceived ?? 0, tx: outbound?.packetsSent ?? 0,
          rxBytes: inbound?.bytesReceived ?? 0, txBytes: outbound?.bytesSent ?? 0,
          decoded: (kind === "video" ? inbound?.framesDecoded : inbound?.totalSamplesReceived) ?? 0,
          codec: stats.get(inbound?.codecId)?.mimeType};
      };
      const first = await measure();
      const end = performance.now() + 9000;
      while (performance.now() < end) {
        await new Promise(r => setTimeout(r, 250));
        const next = await measure();
        if (next.rx > first.rx && next.tx > first.tx && next.decoded > first.decoded) return next;
      }
      throw Error("RTP return did not advance: " + JSON.stringify(await measure()));
    """, kind)
    assert report["pair"] == ["relay", "relay"] and report["state"] == ["connected", "succeeded"], report
    assert report["rxBytes"] > 0 and report["txBytes"] > 0 and codec.lower().endswith(report["codec"].lower()), report
    print("pass: selected TURN pair and returned " + kind, json.dumps(report), flush=True)


JOIN = """
const {connectPrime} = await import("/browser.js");
window.clients ??= [];
await Promise.all(Array.from({length: a[0]}, async () => {
  let waiter, queued;
  const next = () => {
    if (queued !== undefined) { const m = queued; queued = undefined; return Promise.resolve(m); }
    return new Promise((resolve, reject) => {
      const t = setTimeout(() => {waiter = undefined; reject(Error("client response timed out"));}, 8000);
      waiter = m => { clearTimeout(t); waiter = undefined; resolve(m); };
    });
  };
  const rtc = await connectPrime(({data}) => { if (waiter) waiter(data); else queued = data; });
  const joined = await next();
  if (!/^joined [1-9][0-9]*$/.test(joined)) throw Error("missing peer ID");
  clients.push({rtc, next, id: Number(joined.slice(7))});
}));
return clients.length;
"""


def capacity(browser, origin, peers):
    tabs = []
    started = time.monotonic()
    for count in ((peers + 1) // 2, peers // 2):
        tabs.append(browser.tab())
        browser.post("/url", {"url": origin + "/browser.js"})
        for base in range(0, count, 8):
            assert browser.js(JOIN, min(count-base, 8)) == min(base+8, count)
    ids, snapshots = set(), set()
    for tab in tabs:
        browser.switch(tab)
        results = browser.js("""
          return await Promise.all(clients.map(async ({rtc, next, id}) => {
            if (rtc.pc.connectionState !== "connected" || rtc.pc.sctp.state !== "connected" ||
                rtc.dc.readyState !== "open") throw Error("peer closed");
            rtc.dc.send("1");
            return [id, await next()];
          }));
        """)
        for peer, snapshot in results:
            assert peer not in ids
            ids.add(peer)
            prefix, seq, count = snapshot.split()
            assert (prefix, seq) == ("snapshot", "1")
            snapshots.add(int(count))
    assert len(ids) == peers and snapshots == set(range(1, peers+1))
    # Observe all connections again after the final tab has exchanged data.
    for tab in tabs:
        browser.switch(tab)
        assert browser.js('return clients.every(c => c.rtc.dc.readyState === "open")')
    print(f"pass: {peers} simultaneously open real Chromium RTCDataChannels across two tabs; "
          f"{peers} per-peer snapshots in {time.monotonic()-started:.2f}s", flush=True)
    # Run paced request/response batches in JS. No cumulative per-peer admission budget.
    for start in range(2, 302, 50):
        result = browser.js("""
          const {rtc, next} = clients[0];
          let last;
          for (let seq = a[0]; seq < a[0]+50; seq++) {
            rtc.dc.send(String(seq)); last = await next();
            if (last !== "snapshot " + seq + " " + (a[1]+seq-1)) throw Error("wrong snapshot: " + last);
          }
          return last;
        """, start, peers)
    print("pass: 301 numbered moves on one peer, final Prime reply", result, flush=True)
    return tabs


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("prime", type=Path)
    p.add_argument("adapter", type=Path)
    p.add_argument("turn_fixture", type=Path)
    p.add_argument("--peers", type=int, default=99, choices=range(2, 100), metavar="2..99")
    p.add_argument("--chromium", default=shutil.which("chromium"))
    p.add_argument("--driver", default=shutil.which("chromedriver"))
    args = p.parse_args()
    cli, adapter = args.prime.resolve(), args.adapter.resolve()
    with tempfile.TemporaryDirectory() as work:
        for name, entry in [("rtc_game", ENTRY), ("rtc_counter", COUNTER)]:
            subprocess.run([cli, "build", entry, "--name", name], cwd=work, check=True, timeout=300)
        for label in ("run", "native"):
            def command(entry, name):
                if label == "run":
                    return [cli, "run", entry]
                return [Path(work) / ".build.prime" / name / name]
            print(label, flush=True)
            with chromium(args.chromium, args.driver) as browser:
                with relay(args.turn_fixture.resolve(), work) as config:
                    with service(adapter, work, command(ENTRY, "rtc_game"), False, config) as origin:
                        for moves, kind in enumerate(("audio", "video"), 1):
                            relay_media(browser, origin, json.loads(config.read_text()), kind, moves)
                        started = time.monotonic()
                    expect_closed(browser)
                    print(f"pass: adapter SIGTERM and observed relay channel close in {time.monotonic()-started:.3f}s", flush=True)
            with chromium(args.chromium, args.driver) as browser:
                with service(adapter, work, command(COUNTER, "rtc_counter"), True) as origin:
                    tabs = capacity(browser, origin, args.peers)
                    started = time.monotonic()
                for tab in tabs:
                    browser.switch(tab)
                    browser.js("""
                      const end = performance.now() + 10000;
                      while (!clients.every(c => c.rtc.dc.readyState === "closed")) {
                        if (performance.now() > end) throw Error("some RTC channels remained open");
                        await new Promise(r => setTimeout(r, 20));
                      }
                    """)
                print(f"pass: terminal SIGINT and {args.peers} observed channel closes in "
                      f"{time.monotonic()-started:.3f}s", flush=True)


if __name__ == "__main__":
    main()
