#pragma once
#include <stdint.h>

// IPv4 loopback. One calling task. Positive handles, negative errno on failure.
int32_t prime_idf_tcp_listen(int32_t port);
int32_t prime_idf_tcp_connect(int32_t port, int32_t timeout_ms);
int32_t prime_idf_tcp_accept(int32_t listener, int32_t timeout_ms);
int32_t prime_idf_udp_bind(int32_t port);
int32_t prime_idf_local_port(int32_t handle);
int32_t prime_idf_close(int32_t handle);

// Borrow the payload through the call. Return the actual number of bytes written.
int32_t prime_idf_tcp_write(int32_t stream, const uint8_t *data, int32_t len, int32_t timeout_ms);
int32_t prime_idf_udp_send_to(int32_t socket, int32_t port, const uint8_t *data, int32_t len, int32_t timeout_ms);

// A receive replaces this handle's previous buffer. Limit is 1..256 inclusive.
int32_t prime_idf_tcp_read(int32_t stream, int32_t limit, int32_t timeout_ms);
int32_t prime_idf_udp_recv_from(int32_t socket, int32_t limit, int32_t timeout_ms);
int32_t prime_idf_rx_byte(int32_t handle, int32_t index);
int32_t prime_idf_sender_port(int32_t socket);
