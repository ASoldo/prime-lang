"""Validate the installed CLI's own reports against its published JSON Schemas."""
import json
from pathlib import Path
import subprocess
import sys
import tempfile

from jsonschema import Draft202012Validator


def main(cli):
    def validator(name):
        schema = json.loads(subprocess.check_output([cli, "schema", name]))
        Draft202012Validator.check_schema(schema)
        return Draft202012Validator(schema)

    check = validator("check")
    trace = validator("trace")
    with tempfile.TemporaryDirectory() as directory:
        entry = Path(directory) / "sample.prime"
        for body, outcome in [("out(42);", "ok"), ("out(unknown);", "error")]:
            entry.write_text("module sample; fn main() { " + body + " }", encoding="utf-8")
            result = subprocess.run([cli, "check", entry, "--format", "json"], capture_output=True)
            report = json.loads(result.stdout)
            check.validate(report)
            assert result.returncode == int(outcome == "error") and report["outcome"] == outcome

        entry.write_text("module sample; fn ratio(x: int32) -> int32 { 42 / x }\n"
                         "fn main() { out(ratio(0)); }", encoding="utf-8")
        run = subprocess.run([cli, "run", "--trace", entry], capture_output=True)
        events = [json.loads(line) for line in run.stderr.splitlines()]
        assert run.returncode == 1 and [e["event"] for e in events] == ["call", "call", "runtime_error"]
        for event in events:
            trace.validate(event)
        assert len(events[-1]["frames"]) == 2
        assert not trace.is_valid({"schema_version": 1, "event": "runtime_error", "message": "missing frames"})
    print("Validated check reports and trace records against their published schemas.")


if __name__ == "__main__":
    main(str(Path(sys.argv[1]).resolve(strict=True)))
