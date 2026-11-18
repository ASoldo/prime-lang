"""Drive one long-lived Prime ECS process. All simulation state comes from its stdout."""
import asyncio
import contextlib
from dataclasses import dataclass
import os


@dataclass
class Frame:
    tick: int
    seats: list
    alive: int
    winner: int | None
    events: list


class Engine:
    def __init__(self, process, size):
        self.process = process
        self.size = size
        self.frame = None
        self.raw = b""

    @classmethod
    async def launch(cls, command, seats, seed):
        env = os.environ.copy()
        env.pop("PRIME_TEST_INPUTS", None)
        env.pop("PRIME_TEST_INPUTS_FILE", None)
        process = await asyncio.create_subprocess_exec(*command, env=env,
            stdin=asyncio.subprocess.PIPE, stdout=asyncio.subprocess.PIPE)
        engine = cls(process, seats)
        try:
            await engine.initialize(seed)
            return engine
        except BaseException:
            await engine.close()
            raise

    async def write(self, data):
        self.process.stdin.write(data.encode("ascii"))
        await self.process.stdin.drain()

    async def close(self):
        if self.process.returncode is not None:
            return
        self.process.stdin.close()
        with contextlib.suppress(BrokenPipeError, ConnectionResetError):
            await self.process.stdin.wait_closed()
        try:
            await asyncio.wait_for(self.process.wait(), 3)
        except TimeoutError:
            self.process.kill()
            await self.process.wait()

    async def initialize(self, seed):
        await self.write(f"{seed}\n{self.size}\n-2\n")
        await self.receive(0, [])

    async def step(self, actions):
        if len(actions) != self.size or any(action not in range(5) for action in actions):
            raise ValueError("invalid input vector")
        tick = self.frame.tick + 1
        moving = [i for i, action in enumerate(actions) if action and self.frame.seats[i][3]]
        records = [tick * 10000 + (i + 1) * 10 + actions[i] for i in moving]
        records.extend((tick * 10000, -2))
        await self.write("".join(f"{n}\n" for n in records))
        await self.receive(tick, moving)

    async def receive(self, tick, moving):
        seats, events, raw = [], [], bytearray()
        async def read():
            line = await self.process.stdout.readline()
            if not line:
                raise EOFError("Prime ECS stopped during a frame")
            raw.extend(line)
            return line.decode("ascii").split()

        async with asyncio.timeout(90):
            for i in moving:
                record = await read()
                if record != ["Q2", str(tick), str(i)]:
                    raise ValueError(f"unexpected input acknowledgement {record}")
            while True:
                record = await read()
                if len(record) < 2 or record[1] != str(tick):
                    raise ValueError(f"unexpected engine event {record}")
                if len(record) == 4 and record[0] == "T2":
                    alive, winner = map(int, record[2:])
                    break
                if len(record) == 5 and record[0] == "G2":
                    events.append(["G2", tick, *map(int, record[2:])])
                elif len(record) == 4 and record[0] == "X2":
                    events.append(["X2", tick, *map(int, record[2:])])
                else:
                    raise ValueError(f"unexpected engine event {record}")
            for i in range(self.size):
                record = await read()
                if len(record) != 19 or record[:3] != ["V2", str(tick), str(i)]:
                    raise ValueError(f"invalid Prime seat record {record}")
                kind, lines, score, live, rank = map(int, [record[j] for j in (3, 7, 8, 9, 14)])
                cells = list(record[13])
                if len(cells) != 200 or not all(c in "012345678" for c in cells):
                    raise ValueError("invalid settled board")
                # The four offsets come from Prime's geometry including rotation and kicks.
                if live:
                    for cell in map(int, record[15:]):
                        if cell == -1:
                            continue
                        if not 0 <= cell < 200:
                            raise ValueError("invalid Prime cell offset")
                        cells[cell] = str(kind)
                seats.append(["".join(cells), lines, score, live, rank])
            if await read() != ["Z2", str(tick)]:
                raise ValueError("missing Prime snapshot terminator")
        self.frame = Frame(tick, seats, alive, winner if winner >= 0 else None, events)
        self.raw = bytes(raw)
