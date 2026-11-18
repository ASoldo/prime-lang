# Prime Tetris arena

One Prime process owns a match of 2–99 players. Each seat has its own
200-cell well, seven-bag, active piece, score, alive flag and rank. All
bags start from the match seed. The arena uses the same board geometry,
rotation kicks, drop, line clear and shuffle functions as graphical Tetris.
[Shared rules](../tetris/rules.prime) · [ECS components](../tetris_ecs/ecs.prime)

## Run and verify

From the repository root on a Rust/LLVM host:

```sh
cargo build --locked --bin prime-lang
cli="${CARGO_TARGET_DIR:-target}/debug/prime-lang"
demo=workspace/demos/tetris_arena
"$cli" build "$demo/main.prime" --name tetris_arena
native=.build.prime/tetris_arena/tetris_arena
python3 "$demo/verify.py" --chain > /tmp/tetris-chain.txt
"$cli" run "$demo/main.prime" < /tmp/tetris-chain.txt
"$native" < /tmp/tetris-chain.txt
python3 "$demo/verify.py" "$cli" "$native"
```

The last command is the CI stress check. It compares full transcripts
across both backends and against golden SHA-256 values. In the 99-seat
case all players lock real pieces, seat 0 finishes 99th, and tick 15
has 98 survivors. Reversing submission order produces the same final
snapshots. Use `--bots` to emit the 99-seat input log.

The `--final` fixture plays a match through victory. With seed 137,
seats 0–97 hard-drop each tick and seat 98 waits. All 99 seats survive
tick 9. On tick 10 identical stacks block 98 spawns; ascending simulation
order eliminates seat 0 first (rank 99) and seat 97 last (rank 2).
Seat 98 wins. The check requires every rank 1–99 exactly once and compares
the full event stream and snapshots after replaying both submission orders
on both backends. The SHA-256 of the canonical transcript is
`0de9a45d253197e7c35a37fe8538468cbf3cfb8771884e639d7f95c81cfc3d34`.
The 99 final V2 lines alone, including their LF terminators, hash to
`455d5e81a1a239c1bf396b644a2502bd2d380d729f9dad4eaa4a2a173a78b02b`.

In the two-seat chain, gravity brings seat 1's J to row 19, while
seat 0 places J, I and L across the floor. At tick 73 seat 0 clears a
line for 100 points. The garbage row overlaps seat 1's falling piece;
the engine emits `G2 73 0 1 1`, `X2 73 1 2`, then `T2 73 1 0`.
The winner's bottom row is `6000000007`, the loser's is `8088888888`.

## INPUT v2

Write decimal integers to stdin, one per LF-terminated line. First send
a seed (0..2147483647), then the roster size (2..99). Read `T2 0 N -1`
on success or `E2 init` on failure. The bridge owns identity: bind
each connection to a seat and match instance, and derive the seat
field server-side. IDs never recycle within a match, including after
elimination. Expire these mappings when starting another match.

| Input word | Effect | Output |
| --- | --- | --- |
| `t*10000 + (id+1)*10 + a` | Queue action for the next tick, once per live seat. | `Q2 t id` |
| `t*10000` | Commit the next tick. Seats without an action wait. | Zero or more G2/X2 events, then T2 |
| `-100-id` | Inspect one seat. | One V2 then Z2 |
| `-2` | Inspect the roster in ID order. | N separate V2 lines then Z2 |
| `-1` | End the stream. | Process exits |

IDs run from 0 to N−1, ticks from 1 to 99999. Action codes are 0 wait,
1 left, 2 right, 3 clockwise and 4 hard drop. Tick is the sequence number
for each seat. Rejecting a repeated, stale, future or invalid command
emits `E2 currentTick` without mutating simulation or queued actions.
After victory the process still serves snapshots. EOF or an unparsable
word ends the command stream.

For example, lines `137`, `2`, `10014`, `10000`, `-100`, `-1`
start a match, queue a hard drop for seat 0, commit tick 1, inspect
seat 0 and exit. The reply stream starts with `T2 0 2 -1`,
`Q2 1 0`, `T2 1 2 -1` and ends with `Z2 1`.

## Tick, garbage and ranking

Process live seats in ascending ID order. Each seat moves or rotates
before gravity, falls one cell every fourth tick, and settles immediately
on hard drop. A blocked spawn eliminates the seat. After a successful
spawn, each line cleared sends one garbage row to the next live seat in
cyclic ID order. Process attacks immediately and retarget each subsequent
row if the previous target died. The hole column is
`(seed % 10 + tick + sender + target + zeroBasedAttackRow) % 10`.

The target's locked board shifts up one row and receives the garbage at
the bottom. The falling piece keeps its coordinates. Pushing a locked
cell off the top or overlapping the active piece eliminates the target.
Send `G2 tick attacker target hole` for every row. Send `X2 tick seat rank`
for each elimination. Rank is the number of survivors immediately before
that elimination. The fixed seat and attack order breaks same-tick ties.
Stop simulation once one survivor remains; it takes rank 1. At the
99999-tick limit, score decides among remaining seats and lower ID wins
a score tie. `T2 tick aliveCount winnerId` ends each committed tick.
Winner ID is −1 while play continues.

## SNAPSHOT v2 and recovery

Each V2 record is an ASCII line with space-delimited fields:

```text
V2 tick id kind rotation x y lines score alive rng bagIndex bagDigits boardDigits rank cell0 cell1 cell2 cell3
```

`alive` is 0/1. Rank is 0 while undecided. The seven bag digits list
the current shuffle, and `bagIndex` counts consumed entries. The fixed
200 board digits encode settled cells top-to-bottom, left-to-right,
0 empty, 1–7 tetromino kind, 8 garbage. The last four integers project
the active piece onto the board; use `row = cell / 10` (integer division)
and `column = cell % 10`. Ignore −1 cells. Dead seats have four −1s.
A snapshot batch terminates with `Z2 tick`. Only explicit inspection
sends boards; an ordinary tick ends with a short summary.

Persist seed, roster size and accepted inputs/advances. Capture committed
checkpoints immediately after T2. For reconnect, request the chosen
seat or full roster, then resume from its tick. To restore authority,
recreate the process and replay the journal, comparing ordered V2 records
at checkpoints; replay any acknowledged, still-pending actions after
the last commit. V2 describes committed state. The engine has no
snapshot-import command. The browser bridge can forward individual
records without assembling a 99-board transport message.
