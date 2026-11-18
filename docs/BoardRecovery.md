# Identify and back up an Espressif board

Use an explicit serial port. These commands temporarily reset the board into its ROM bootloader and reset it again before disconnecting. The running application is interrupted. Inspection reads chip information and flash geometry. Backup reads the complete detected SPI NOR flash and checks it against the device.

The installed Prime CLI includes the adapter. It needs Python 3.11 or newer with the Espressif esptool 5.x package installed in the same interpreter. Select that interpreter with `PRIME_ESP_PYTHON` if it differs from `python3`. Once installed, these operations work offline. Close serial monitors before connecting. [Espressif scripting API](https://docs.espressif.com/projects/esptool/en/latest/esp32/esptool/scripting.html)

```sh
port=/dev/ttyUSB0
prime-lang board inspect --port "$port"
prime-lang board backup --port "$port" --out original.bin > original.json
prime-lang schema board
```

Use a fresh image name and receipt name. Shell redirection opens the receipt before Prime starts. On success the JSON on stdout identifies the chip, revision description, MAC, serial port, flash size in bytes, esptool version and a Prime build target where available. A backup also reports the image path and its SHA-256. Progress and errors go to stderr. A nonzero exit indicates failure. The CLI publishes a new image after SDK verification and leaves an existing image intact, including if another process creates the destination during the read. Store the image with its receipt. It can contain provisioning data belonging to this particular board.

The connection starts at 115200 baud. Backup uses a RAM stub with a default transfer rate of 460800. For an unreliable connection use `--baud 115200`. If flash size detection or verification fails, inspect the SDK error and keep any previous backup. The snapshot covers SPI flash. eFuses, peripheral state and data restricted by the device are outside its scope.

For a classic ESP32, build the [board I/O probe](../workspace/demos/no_std_parity/board_io.prime) with the [ESP-IDF linker configuration](EmbeddedPipeline.md) and `--no-flash`. The [UART acceptance procedure](BoardTesting.md) lists the GPIO and timer results to look for. Firmware, bootloader and partition layout must match the identified chip and board. [Espressif flash commands](https://docs.espressif.com/projects/esptool/en/latest/esp32/esptool/basic-commands.html)
