#!/usr/bin/env bash
# Boot the no_std_parity application with a classic ESP32 bootloader and partition table.
set -euo pipefail
if [[ $# -ne 3 ]]; then
  echo 'Usage: test_esp32_qemu.sh BOOTLOADER.bin PARTITIONS.bin PARITY_APP.bin' >&2
  exit 2
fi
here=$(cd "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT

# These offsets match the ESP-IDF single factory application layout for classic ESP32.
esptool --chip esp32 merge-bin --pad-to-size 4MB --output "$work/flash.bin" \
  0x1000 "$1" 0x8000 "$2" 0x10000 "$3"

qemu=( "${QEMU_SYSTEM_XTENSA:-qemu-system-xtensa}" )
if [[ -n ${QEMU_DATA_DIR:-} ]]; then qemu+=(-L "$QEMU_DATA_DIR"); fi
status=0
timeout --kill-after=2s 15s "${qemu[@]}" -machine esp32 -nographic \
  -monitor none -serial "file:$work/uart.log" \
  -drive "file=$work/flash.bin,if=mtd,format=raw" || status=$?
# The firmware remains on the board after printing the completion marker.
if [[ $status -ne 124 ]]; then
  echo "QEMU exited before the observation window completed (status $status)" >&2
  exit 1
fi
if ! python3 "$here/check_esp32_parity.py" --log "$work/uart.log"; then
  cat "$work/uart.log" >&2
  exit 1
fi
