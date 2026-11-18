"""Seat leases, sequenced input and presentation of one Prime-owned match."""
import asyncio
import hashlib
import json
import re
from collections import deque
from dataclasses import dataclass, field

TOKEN = re.compile(r"join ([0-9a-f]{8}-(?:[0-9a-f]{4}-){3}[0-9a-f]{12})")
MOVE = re.compile(r"move ([1-9][0-9]{0,8}) ([1-4])")


@dataclass
class Lease:
    token: str
    peer: tuple | None = None
    ack: int = 0
    pending: deque = field(default_factory=deque)


class Match:
    def __init__(self, engine, humans, clock, send):
        self.engine = engine
        self.humans = humans
        self.clock = clock
        self.send = send
        self.leases = []
        self.by_peer = {}
        self.lock = asyncio.Lock()
        self.started = False
        self.log = deque(maxlen=12)

    def snapshot(self):
        frame = self.engine.frame
        phase = "finished" if frame.winner is not None else "playing" if self.started else "lobby"
        seats = [[*seat, self.leases[i].ack if i < len(self.leases) else 0]
                 for i, seat in enumerate(frame.seats)]
        return dict(tick=frame.tick, alive=frame.alive, winner=frame.winner, seats=seats,
                    phase=phase, bots=len(seats) - self.humans, log=list(self.log), events=frame.events)

    async def publish(self, peers):
        payload = json.dumps(self.snapshot(), separators=(",", ":"), ensure_ascii=True)
        digest = hashlib.sha256(payload.encode()).hexdigest()
        parts = [payload[i:i + 2800] for i in range(0, len(payload), 2800)]
        for peer in peers:
            for i, part in enumerate(parts):
                await self.send(peer, f"F {i} {len(parts)} {digest} {part}")

    async def join(self, peer, token):
        current = self.by_peer.get(peer)
        for i, lease in enumerate(self.leases):
            if lease.token == token:
                if (lease.peer and lease.peer != peer) or current not in (None, i):
                    return await self.send(peer, "error Seat already connected")
                break
        else:
            if current is not None or len(self.leases) == self.humans:
                return await self.send(peer, "error No available human seat")
            i = len(self.leases)
            lease = Lease(token)
            self.leases.append(lease)
        self.by_peer[peer] = i
        lease.peer = peer
        await self.send(peer, f"seat {i}")
        await self.publish([peer])

    async def event(self, peer, kind, payload):
        async with self.lock:
            if kind == "close":
                i = self.by_peer.pop(peer, None)
                if i is not None:
                    self.leases[i].peer = None
                return
            if kind != "text":
                return
            join = TOKEN.fullmatch(payload)
            if join:
                return await self.join(peer, join[1])
            if peer not in self.by_peer:
                return await self.send(peer, "error Join required")
            if payload == "start" and not self.started:
                self.started = True
                return await self.publish(self.by_peer)
            move = MOVE.fullmatch(payload)
            if not move:
                return await self.send(peer, "error Unknown command")
            seat = self.by_peer[peer]
            lease = self.leases[seat]
            seq, action = map(int, move.groups())
            tail = lease.pending[-1][0] if lease.pending else lease.ack
            if seq <= tail:
                # Lost ack or retransmission, including a move already in the tick queue.
                return await self.publish([peer])
            if seq != tail + 1 or len(lease.pending) >= 16:
                return await self.send(peer, "error Sequence gap or queue full")
            if not self.started or self.engine.frame.winner is not None or not self.engine.frame.seats[seat][3]:
                return await self.send(peer, "error Seat cannot move")
            lease.pending.append((seq, action))
            if self.clock == 0:
                await self.advance()

    async def advance(self):
        # The engine alone decides gravity, collisions, attacks, KO order and victory.
        frame = self.engine.frame
        actions = [0] * len(frame.seats)
        for i, lease in enumerate(self.leases):
            if lease.pending:
                actions[i] = lease.pending[0][1]
        for i in range(self.humans, len(actions)):
            actions[i] = bot_action(i, frame.tick + 1)
        await self.engine.step(actions)
        for i, lease in enumerate(self.leases):
            if lease.pending and frame.seats[i][3]:
                lease.ack = lease.pending.popleft()[0]
            if not self.engine.frame.seats[i][3] or self.engine.frame.winner is not None:
                lease.pending.clear()
        for event in self.engine.frame.events:
            if event[0] == "G2":
                _, tick, attacker, target, hole = event
                message = f"Seat {attacker + 1} attacked seat {target + 1} · hole {hole + 1}"
            else:
                _, tick, seat, rank = event
                message = f"Seat {seat + 1} eliminated · place {rank}"
            self.log.appendleft(f"T{tick}  {message}")
        await self.publish(self.by_peer)

    async def run_clock(self):
        while True:
            await asyncio.sleep(self.clock)
            async with self.lock:
                if self.started and self.engine.frame.winner is None:
                    await self.advance()


def bot_action(seat, tick):
    """Deterministic input source with no board model or prediction."""
    phase = (tick + seat * 3) % 7
    return 4 if phase < 4 else (1, 2, 3)[(phase + seat) % 3]
