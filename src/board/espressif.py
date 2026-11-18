"""esptool adapter embedded in the CLI. Only the final report goes to stdout."""
import contextlib
import hashlib
import json
import sys


def probe(port, image=None, baud=None):
    if sys.version_info < (3, 11):
        raise RuntimeError("board commands require Python 3.11 or newer")
    import esptool
    from esptool import cmds
    from esptool.util import flash_size_bytes

    with cmds.detect_chip(port=port) as esp:
        try:
            if image:
                esp = cmds.run_stub(esp)
                esp.change_baud(int(baud))
            cmds.attach_flash(esp)
            size = flash_size_bytes(cmds.detect_flash_size(esp))
            if not size:
                raise ValueError("device did not report a recognized SPI NOR flash size")
            report = {
                "schema_version": 1,
                "port": port,
                "chip": esp.CHIP_NAME,
                "description": esp.get_chip_description(),
                "mac": bytes(esp.read_mac()).hex(":"),
                "flash_bytes": size,
                "esptool_version": esptool.__version__,
            }
            if image:
                cmds.read_flash(esp, 0, size, output=image, no_progress=True)
                with open(image, "rb") as data:
                    cmds.verify_flash(esp, [(0, data)])
                    data.seek(0)
                    report["sha256"] = hashlib.file_digest(data, "sha256").hexdigest()
            return report
        finally:
            cmds.reset_chip(esp, "hard-reset")


if __name__ == "__main__":
    try:
        with contextlib.redirect_stdout(sys.stderr):
            result = probe(*sys.argv[1:])
        print(json.dumps(result))
    except ImportError as error:
        sys.exit(f"Python 3.11+ and Espressif esptool 5.x are required: {error}")
    except Exception as error:
        sys.exit(str(error))
