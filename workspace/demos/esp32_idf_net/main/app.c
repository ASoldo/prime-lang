#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "esp_netif.h"

// Scalar/string handles live until the single Prime invocation returns.
struct allocation { struct allocation *next; };
static struct allocation *allocations;

void *prime_sdk_alloc(size_t size)
{
    struct allocation *p = malloc(sizeof(*p) + size);
    if (!p) abort();
    p->next = allocations;
    allocations = p;
    return p + 1;
}

struct text { size_t len; uint8_t data[]; };

void *prime_string_new(const uint8_t *bytes, size_t len)
{
    struct text *s = prime_sdk_alloc(sizeof(*s) + len);
    s->len = len;
    memcpy(s->data, bytes, len);
    return s;
}

void prime_print(const struct text *s)
{
    fwrite(s->data, 1, s->len, stdout);
}

void prime_integer_check(bool ok, const uint8_t *message, size_t len)
{
    if (!ok) {
        printf("PRIME_IDF_FAIL integer %.*s\n", (int)len, (const char *)message);
        abort();
    }
}

extern int main(void); // Relocatable Prime object.

void app_main(void)
{
    ESP_ERROR_CHECK(esp_netif_init());
    main();
    fflush(stdout);
    while (allocations) {
        struct allocation *next = allocations->next;
        free(allocations);
        allocations = next;
    }
}
