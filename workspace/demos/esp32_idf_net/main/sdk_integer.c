// Only this file requires Espressif Clang's extended Xtensa i128 ABI.
#include <stddef.h>
#include <stdint.h>

extern void *prime_sdk_alloc(size_t size);
extern void *prime_string_new(const uint8_t *data, size_t len);

void *prime_int_new(__int128 value)
{
    void *p = prime_sdk_alloc(sizeof(value));
    __builtin_memcpy(p, &value, sizeof(value));
    return p;
}

void *prime_int_to_string(__int128 value)
{
    // Divide base-256 digits by ten. This also covers INT128_MIN and requires
    // only 32-bit arithmetic on an SDK without compiler-rt's 128-bit division.
    unsigned __int128 magnitude = value < 0 ? 0 - (unsigned __int128)value : (unsigned __int128)value;
    uint8_t little[16];
    __builtin_memcpy(little, &magnitude, sizeof(little));
    uint8_t digits[40];
    size_t pos = sizeof(digits);
    unsigned used = sizeof(little);
    do {
        unsigned rem = 0;
        for (unsigned i = used; i > 0; i--) {
            unsigned v = rem * 256 + little[i - 1];
            little[i - 1] = v / 10;
            rem = v % 10;
        }
        digits[--pos] = '0' + rem;
        while (used && little[used - 1] == 0) used--;
    } while (used);
    if (value < 0) digits[--pos] = '-';
    return prime_string_new(digits + pos, sizeof(digits) - pos);
}
