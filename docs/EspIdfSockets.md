# TCP and UDP on a classic ESP32

The SDK example runs a Prime program on the ESP32's two local TCP endpoints and two UDP endpoints. All four use `127.0.0.1`. The IDF component starts ESP-NETIF/lwIP, then calls the `main` symbol from the Prime-generated Xtensa object. Prime selects the payloads, opens the sockets, reads the actual bytes, computes an acknowledgement, sends it back and checks it. Both sides run on the chip. The project requires neither Wi-Fi initialization nor WLAN credentials. [Prime application](../workspace/demos/esp32_idf_net/main.prime) · [ESP-IDF lwIP documentation](https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-guides/lwip.html)

## Build the three images

The verified configuration uses Espressif's ESP-IDF `v6.1-dev-918-gb111c84f9e` at commit `b111c84f9e02b1f07614c82029deec6a4d81fcc6` with its submodules (esp-lwIP `fd432e4ee2cfb7f7f1c7eb7227e0173412e7b84e`), Xtensa GCC `esp-15.2.0_20250929`, Espressif LLVM `esp-20.1.1_20250829`, and esptool 5.6.0. Install the IDF tools and Python dependencies for that checkout. The host CLI uses the repository's Rust toolchain and Cargo.lock. In the commands below, set `ESP_LLC` to the actual Espressif `llc` executable. Its sibling `clang` supplies the Xtensa wide-integer ABI used by Prime's scalar display functions. [Pinned SDK](https://github.com/espressif/esp-idf/tree/b111c84f9e02b1f07614c82029deec6a4d81fcc6) · [Espressif LLVM release](https://github.com/espressif/llvm-project/releases/tag/esp-20.1.1_20250829)

From the Prime repository root, use a fresh build directory and the example's default configuration.

```sh
export CARGO_BUILD_JOBS=2
cargo build --locked --bin prime-lang
cli="$PWD/target/debug/prime-lang"
project="$PWD/workspace/demos/esp32_idf_net"
build="$PWD/.build.prime/esp32-idf-net"
ESP_LLC="$HOME/.espressif/tools/esp-clang/esp-clang/bin/llc"
. "$HOME/esp/esp-idf/export.sh"

"$cli" check "$project/main.prime" --format json
idf.py -C "$project" -B "$build" -DIDF_TARGET=esp32 \
  -DSDKCONFIG="$build/sdkconfig" -DPRIME_CLI="$cli" -DPRIME_ESP_LLC="$ESP_LLC" build
cat "$build/flash_args"
```

CMake invokes `prime-lang build main.prime --emit-object FILE --no-flash` whenever the Prime source, manifest or CLI changes. This CLI mode writes one relocatable object atomically. The embedding project supplies the entry point and definitions for the imported runtime symbols. The SDK example links the object at `esp-idf/main/idf_net.o` into its normal IDF ELF.

| Offset | Image under the build directory | Purpose |
| --- | --- | --- |
| `0x1000` | `bootloader/bootloader.bin` | Classic ESP32 second-stage bootloader |
| `0x8000` | `partition_table/partition-table.bin` | Single factory app table |
| `0x10000` | `prime_idf_net.bin` | Prime + FreeRTOS + ESP-NETIF + lwIP |

Flash settings are DIO, 40 MHz, 4 MiB. The table allocates NVS at `0x9000` (24 KiB), PHY init at `0xf000` (4 KiB) and the factory app at `0x10000` (1 MiB). IDF checks the app against the partition during the build. Use the generated `flasher_args.json` and `flash_args` for the exact build being provisioned.

## Execution and evidence

Install Espressif QEMU `esp_develop_9.2.2_20250817` from the pinned SDK tool manifest. With `esptool` and `qemu-system-xtensa` on PATH, run the following command. `QEMU_SYSTEM_XTENSA` can also point to the emulator executable. The script merges the three images into a temporary 4 MiB flash, boots the Xtensa CPU with the emulated network card disabled and checks for one complete run. [Espressif QEMU setup](https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-guides/tools/qemu.html)

```sh
./scripts/test_esp32_idf_net_qemu.sh "$build"
```

Two clean IDF build directories produced byte-identical bootloader, partition table and application images. Both Espressif QEMU and a physical ESP32-D0WD-V3 rev3 produced the following four UART lines. The board ran the same 258896-byte factory application from the integrated compiler (SHA-256 `5eadeeee2558db2af4e99a118b9c33bab4d0870b0525122e3d4f377349f22feb`) at 115200 baud. The test restored and verified the board’s original 4 MiB flash image after recording the result.

```text
PRIME_IDF_BEGIN
PRIME_IDF_TCP bytes=7 sum=764 ack=252
PRIME_IDF_UDP bytes=6 sum=616 ack=104
PRIME_IDF_PASS
```

The TCP request is `00 50 72 69 6d 65 ff`. Prime retrieves it through seven one-byte reads. The receiver returns `fc`, the low byte of the received sum. The UDP request is one datagram `55 44 50 00 80 ff`, followed by one acknowledgement datagram `68` to the observed sender. Prime verifies the bytes and both UDP source ports. An empty UDP receive first exercises the timeout path. A transfer or data mismatch produces `PRIME_IDF_FAIL`. A separate fault-injection build swapped the first two TCP bytes before sending. The counts, sums and acknowledgement bytes stayed the same, and Prime reported failure on the byte comparison.

## Socket contract

The checker enables the component built-ins for `target = "xtensa-esp32-espidf"`, `platform = "esp-idf"` and a module with `no_std = false`. The host interpreter reports that execution requires the linked SDK component. Existing bare-metal projects use the embedded runtime and its startup. Host sockets use operating-system endpoints and dynamically allocated buffers. The SDK keeps the same listen/connect/accept/read/write and bind/send-to/receive-from operations with integer handles, byte counts and positive timeouts in milliseconds.

The SDK subset fixes the address at IPv4 loopback and accepts numeric ports. Port zero on bind/listen asks lwIP to choose a port. All results are signed `int32`, with negative errno on failure. One FreeRTOS task owns the adapter and its six handle slots. Closed IDs stay invalid even when a descriptor is reused. [C ABI declarations](../workspace/demos/esp32_idf_net/main/prime_idf_socket.h)

| Prime call | Successful result |
| --- | --- |
| `idf_tcp_listen(port)`, `idf_tcp_connect(port, ms)`, `idf_tcp_accept(listener, ms)`, `idf_udp_bind(port)` | New positive handle |
| `idf_local_port(handle)` | Bound port |
| `idf_tcp_write(stream, bytes, ms)`, `idf_udp_send_to(socket, port, bytes, ms)` | Actual byte count. A stream write can be partial. The component borrows a compiler-sized `[]uint8` for the call. |
| `idf_tcp_read(stream, limit, ms)`, `idf_udp_recv_from(socket, limit, ms)` | Actual received length. A stream returns zero at EOF. A datagram can be empty. |
| `idf_rx_byte(handle, index)` | Unsigned byte from the latest successful receive on this handle |
| `idf_sender_port(socket)` | Source port of the latest successful UDP receive |
| `idf_close(handle)` | Zero |

Each handle has a 256-byte receive buffer. The receive limit ranges from 1 through 256. A new receive clears the previous length and sender. An oversized datagram is consumed and reported as `-EMSGSIZE`. The component retains scalar and string handles for this single Prime invocation and releases their storage when it returns.
