# prime::diagram

Prime-native diagram plugin/framework for loading diagram data at runtime and
rendering SVG or PNG output. The renderer is designed for runtime pipeline and
node-graph views: typed ports, colored node kinds, connected wires, and
content-fitted export bounds.

## Runtime Data Format

```text
title=Prime Runtime Diagram Pipeline
node=load|Begin Load|event|flow|runtime source|0
port=load|out|exec|flow
port=load|out|payload|data
node=parse|Parse Model|function|flow|dynamic schema|1
port=parse|in|exec|flow
port=parse|in|payload|data
port=parse|out|model|object
edge=load|parse|exec|exec|exec
edge=load|parse|data|payload|payload
```

Supported records:

- `title=<text>`
- `node=<id>|<label>|<kind>|<lane>|<detail>|<column>`
- `port=<node-id>|<in|out>|<name>|<kind>`
- `edge=<from-id>|<to-id>|<label>|<from-port>|<to-port>`

`lane` is optional and accepts values such as `flow`, `data`, or `async`.
`column` is optional and pins a node to a left-to-right layout column.
`kind` controls the node header color; common values are `event`, `function`,
`async`, and `output`.

The parser validates graph integrity before returning a diagram: duplicate node
ids, unknown edge endpoints, invalid port directions, and edges that target
missing named ports return `Err(string)`.

## API

Import the prelude:

```prime
import prime::diagram::prelude::{*};
```

Primary functions:

- `load_diagram(path) -> Result[Diagram, string]`
- `parse_diagram(text) -> Result[Diagram, string]`
- `validate_diagram(&diagram) -> Result[(), string]`
- `diagram_summary(&diagram) -> string`
- `render_svg(&diagram) -> string`
- `render_png(&diagram) -> []uint8`
- `write_svg(&diagram, path) -> Result[(), string]`
- `write_png(&diagram, path) -> Result[(), string]`
- `load_and_write(input, svg, png) -> Result[(), string]`

Run the demo:

```sh
cargo run -- run workspace/plugins/diagram_demo/main.prime
```

The SVG output uses scalable text and curved wires. The PNG output is generated
directly in Prime with a compact bitmap font, so it works without a browser or
native image dependency.

The repo architecture map lives at `prime_lang_architecture.pdiag`; the demo
writes its SVG next to the runtime pipeline outputs.
