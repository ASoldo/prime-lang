#!/usr/bin/env bash
# Exercise the SDK flash images in Espressif QEMU with the emulated NIC disabled.
set -eu -o pipefail
if [[ $# -ne 1 ]]; then
    echo 'usage: test_esp32_idf_net_qemu.sh IDF_BUILD_DIR' >&2
    exit 2
fi
build=$(realpath "$1")
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
(
    cd "$build"
    esptool --chip esp32 merge-bin --pad-to-size 4MB -o "$work/flash.bin" @flash_args
)
qemu=("${QEMU_SYSTEM_XTENSA:-qemu-system-xtensa}")
if [[ -n ${QEMU_DATA_DIR:-} ]]; then qemu+=(-L "$QEMU_DATA_DIR"); fi
status=0
timeout --kill-after=2s 12s "${qemu[@]}" -M esp32 -nic none -nographic -monitor none \
    -serial "file:$work/uart.log" -drive "file=$work/flash.bin,if=mtd,format=raw" || status=$?
if [[ $status != 124 ]]; then
    echo "QEMU exited early ($status)" >&2
    exit 1
fi
tr -d '\r' < "$work/uart.log" | awk '/^PRIME_IDF_/' > "$work/actual"
diff -u - "$work/actual" <<'UART'
PRIME_IDF_BEGIN
PRIME_IDF_TCP bytes=7 sum=764 ack=252
PRIME_IDF_UDP bytes=6 sum=616 ack=104
PRIME_IDF_PASS
UART
if ! grep -Fq 'Returned from app_main()' "$work/uart.log" || grep -Eq 'Guru Meditation|abort\(\) was called' "$work/uart.log"; then
    cat "$work/uart.log" >&2
    exit 1
fi
cat "$work/actual"
