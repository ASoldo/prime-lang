# Classic ESP32 UART acceptance test

Use the classic Xtensa ESP32 with the existing `workspace/demos/no_std_parity/main.prime` firmware. From the repository root, a host with the Espressif toolchain and IDF linker scripts can build the image with the following command. [Toolchain setup](../README.md#embedded--esp32-xtensa-quickstart)

```sh
prime-lang build workspace/demos/no_std_parity/main.prime --name esp32_parity --no-flash
```

## On the board

With the demo already installed, close other serial monitors. On Linux, substitute the board’s UART0 port below. When the script prints `UART ready`, press EN once. The receiver uses 115200 baud, 8N1 and listens for the entire 15-second window, including time after the final marker.

```sh
python3 scripts/check_esp32_parity.py --port /dev/ttyUSB0 --timeout 15
```

For a saved capture, use the same validator with `--log path/to/board.uart`. Keep raw LF or CRLF lines from a single run, including boot messages and the observation tail. The validator reads to EOF. It reports the first mismatch, an incomplete run, a panic diagnostic or an unexpected reboot with exit status 1. The Xtensa Rust panic handler spins silently, so a halt during a probe can surface as a deadline waiting for its next line.

## GPIO readback and deadlines

On a classic ESP32 development board, leave GPIO2 free of external drivers. Build the board I/O probe and install its application using the board's bootloader and partition layout. The CLI's default flash operation updates the application. A fully erased board also needs a matching bootloader and partition table. [Flash options](EmbeddedPipeline.md)

```sh
prime-lang build workspace/demos/no_std_parity/board_io.prime --name board_io --no-flash
python3 scripts/check_esp32_parity.py --probe board-io --port /dev/ttyUSB0
```

The program drives GPIO2 low, high and low again, sampling the physical pad after each change. The validator expects `0, 1, 0`. It also checks that a 50 ms task times out at 5 ms, an already completed timer succeeds and an empty open channel times out. Replay a captured run with `--probe board-io --log board-io.uart`. [Probe source](../workspace/demos/no_std_parity/board_io.prime) · [Espressif GPIO reference](https://docs.espressif.com/projects/esp-idf/en/stable/esp32/api-reference/peripherals/gpio.html)

On an ESP32-D0WD-V3 revision 3.0 with a CP2102 UART bridge, this probe originally read zero after driving GPIO2 high. After the GPIO runtime fix the same board returned all eight expected lines. The test fixture preserves the boot and application output with line endings and trailing whitespace normalized. [Physical UART capture](../scripts/tests/fixtures/esp32_d0wd_v3_board_io.uart)

## Two-slot cancellation token probe

On the classic ESP32, build the probe with two runtime slots. The program keeps one token alive while it repeatedly allocates the second slot, checks that a third allocation is cancelled, and observes cancellation of the second token through its alias. The last check allocates the second slot once more. [Probe source](../workspace/demos/no_std_parity/token_owners.prime)

```sh
PRIME_RT_CANCEL_TOKEN_SLOTS=2 prime-lang build workspace/demos/no_std_parity/token_owners.prime --name token_owners --no-flash
```

After installing the application with the normal ESP32 bootloader and partition table, capture UART0 at 115200 baud, 8N1. A successful run prints the following lines.

```text
token owners start
token owners shared 100/100 full 100/100
token owners PASS
```

## Offline check

The first fixture is a synthetic transcript of the eleven required lines, in order. It should exit 0. The second is the application output from an Espressif QEMU run of `c825d5f`, saved with LF line endings. It should exit 1 at `await_timeout parity err`, where the firmware printed `2000505063154167820296574927073` in place of `timeout`. Both commands run without a board.

```sh
python3 scripts/check_esp32_parity.py --log scripts/tests/fixtures/no_std_parity_expected.uart --timeout 3
python3 scripts/check_esp32_parity.py --log scripts/tests/fixtures/no_std_parity_c825d5f_qemu.uart --timeout 3
python3 scripts/tests/test_esp32_parity.py -v
```

## Boot the image in Espressif QEMU

Use a matching classic ESP32 bootloader and partition table with a single factory app at `0x10000`. The script uses `esptool` to assemble a temporary flash image, boots it in the emulator and checks its UART capture with the same validator. Put Espressif’s `qemu-system-xtensa` on `PATH` or set `QEMU_SYSTEM_XTENSA` to the executable. Set `QEMU_DATA_DIR` if its ROM files are in a separate data directory.

```sh
scripts/test_esp32_qemu.sh BOOTLOADER.bin PARTITIONS.bin PARITY_APP.bin
```

## What the UART can establish

The Xtensa print path writes literals and interpolated values in pieces using ROM `ets_printf`, then emits a newline. Expect guard values 0 and 1, the async join marker, `Ok(7)`, `Some(3)`, the joined value 11, then the three guarded wait errors. The async join probe discards its computed integer and prints only the marker. [Demo](../workspace/demos/no_std_parity/main.prime) · [Print lowering](../src/language/compiler/runtime.rs) · [Xtensa ABI](../src/runtime/abi.rs)

At `c825d5f`, the generated Xtensa IR constructs the strings `timeout`, `cancelled`, `timeout` for those errors. In each corresponding match arm it calls `prime_value_as_int` and `prime_int_to_string` on the extracted string handle. The ABI reads that handle as an integer. The QEMU capture contains three numeric payloads and the final marker. Acceptance requires all three literal error strings. Physical UART and reset behavior require a run on the board. [Guarded wait lowering](../src/language/compiler/emit.rs)
