"""Generate 99 input streams, replay them in native Prime and compare every frame."""
import argparse
import asyncio
import hashlib
import json
from pathlib import Path

from engine import Engine
from match import bot_action


async def play(command, inputs=None):
    engine = await Engine.launch(command, 99, 137)
    transcript = hashlib.sha256()
    transcript.update(engine.raw)
    frames, events = [], []
    try:
        # The ECS clear fixture exercises a real garbage attack in a full roster.
        opening = [1, 1, 1, 1, 4, 4, 2, 2, 2, 2, 4]
        while engine.frame.winner is None:
            tick = engine.frame.tick + 1
            if tick > 300:
                raise AssertionError("match failed to finish")
            if inputs is None:
                actions = [bot_action(i, tick) for i in range(99)]
                if tick <= len(opening):
                    actions[0] = opening[tick - 1]
            else:
                actions = inputs[tick - 1]
            frames.append(actions)
            await engine.step(actions)
            current = engine.frame
            transcript.update(engine.raw)
            events.extend(current.events)
        ranks = [seat[4] for seat in engine.frame.seats]
        assert sorted(ranks) == list(range(1, 100)), ranks
        assert ranks[engine.frame.winner] == 1 and engine.frame.alive == 1
        if inputs is not None:
            assert len(inputs) == len(frames)
        return frames, ranks, events, transcript.hexdigest()
    finally:
        await engine.close()


async def verify(args):
    frames, ranks, events, digest = await play([args.prime, "run", args.entry])
    other = await play([args.native], frames)
    assert (frames, ranks, events, digest) == other
    attacks = sum(event[0] == "G2" for event in events)
    assert attacks > 0 and sum(event[0] == "X2" for event in events) == 98
    replay = {"seed": 137, "seats": 99, "actions": frames, "rank_by_seat": ranks,
              "events": events, "state_sha256": digest}
    recorded = Path(__file__).with_name("bots.replay.json")
    assert replay == json.loads(recorded.read_text()), "recorded bot replay changed"
    if args.replay:
        args.replay.write_text(json.dumps(replay, separators=(",", ":")) + "\n")
    print(f"99 simulated slots, 0 transport peers, 0 browser windows; {len(frames)} ticks, "
          f"attacks={attacks}, 98 eliminations, winner seat {ranks.index(1) + 1}, ranks 1..99; ECS transcript sha256 {digest}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--prime", required=True)
    parser.add_argument("--entry", required=True)
    parser.add_argument("--native", required=True)
    parser.add_argument("--replay", type=Path, help="write accepted inputs, events and placement")
    asyncio.run(verify(parser.parse_args()))
