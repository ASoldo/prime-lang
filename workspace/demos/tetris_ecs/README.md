# Two-player headless Tetris

From the repository root on a Rust/LLVM host:

```sh
cargo build --locked --bin prime-lang
cli="${CARGO_TARGET_DIR:-target}/debug/prime-lang"
demo=workspace/demos/tetris_ecs
"$cli" run "$demo/main.prime" < "$demo/clear.replay"
"$cli" build "$demo/main.prime" --name tetris_ecs
native=.build.prime/tetris_ecs/tetris_ecs
"$native" < "$demo/clear.replay"
python3 "$demo/verify.py" "$cli" "$native"
```

The seed-137 fixture deals J, I, L to each seat. Seat 0 moves J against
the left wall, locks the three pieces across the well and clears the bottom
row on tick 11. The remaining bottom row reads `6000000007`. Seat 1 rotates
its J and drops it at a different position. Verification checks the full
24-record transcript against a golden SHA-256, compares both backends,
then injects a duplicate, a missing tick and an invalid action and
checks that replay continues from the last accepted state.

## Match and tick

One process owns one match. Entity IDs 0 and 1 are row indices in a
fixed, two-seat roster. Each row owns an active piece, score, line count,
alive flag and seven-bag cursor/seed. Each entity also owns its 10×20
well and independent bag. Both start with the same shuffle. An ID stays
allocated after top-out and expires when the process exits. A transport
must bind its client-to-seat mapping and input queue to the same match
instance and discard that mapping when creating a new match.

The driver applies a complete pair of actions in seat order, then emits
both states. Each live seat applies its action before gravity. Gravity
attempts one downward cell on every fourth tick. A hard drop settles
immediately. Clockwise rotation checks the current column, one left and
one right. Settlement locks four cells, compacts full rows, updates
score and level, draws the next piece and checks spawn collision.
A topped-out seat remains frozen while the other continues. The engine
uses the graphical game's geometry, collision, locking, row clearing,
scoring and seven-bag functions in [rules.prime](../tetris/rules.prime).
The headless ruleset has immediate clears and fixed gravity. Hold,
preview, presentation and wall-clock input belong to the graphical demo.

## Input and state v1

Use ASCII decimal integers, one per LF-terminated stdin line. Start with
a seed from 0 through 2147483647. For each tick send
`100 * tick + 10 * action0 + action1`. Ticks run consecutively from 1
through 999999. Action codes are 0 wait, 1 left, 2 right, 3 clockwise and
4 hard drop. For example, `113` moves seat 0 left and rotates seat 1
on tick 1. Send `-1` to end the stream. Invalid seed values produce
`E1 seed`. A duplicate, gap or out-of-range action produces
`E1 frame LAST_ACCEPTED_TICK` and leaves both seats unchanged. EOF or
an unparsable frame ends the match.

On initialization and after every accepted tick, stdout yields two lines
in ascending ID order. Split on ASCII spaces:

```text
S1 tick id kind rotation x y lines score alive rng bagIndex bagDigits boardDigits
```

Coordinates start at the upper left. `x,y` locate the piece origin used
by the shared geometry functions. `alive` is 0 or 1. `bagDigits` contains
seven piece kinds (1–7) in draw order. `bagIndex` counts consumed entries
and 7 triggers a shuffle on the next draw. `rng` holds the seed for that
shuffle. `boardDigits` has exactly 200 digits in row-major order, including
leading zeroes for empty cells. It stores locked cells. A renderer
overlays the active piece when the seat is alive.

For lockstep networking, assemble the two seat actions for a tick and
send exactly one frame to each replica. Persist the seed and accepted
frames. Treat the pair of S1 records for the same tick as one checkpoint.
On a gap, request the missing frames. On divergent output or restart,
launch a fresh engine with the original seed, replay the accepted prefix
and compare both state lines byte for byte at the agreed tick before
resuming live input. Snapshot import and network transport are outside
this demo.
