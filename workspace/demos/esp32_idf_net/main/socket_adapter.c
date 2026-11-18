// Single-task IPv4 loopback socket component. Handles stay invalid after close.
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include "prime_idf_socket.h"
#include <sys/select.h>
#include <sys/socket.h>
#include <unistd.h>

#define SOCKET_SLOTS 6
#define RX_CAPACITY 256

enum kind { LISTENER, STREAM, DATAGRAM };
struct socket_slot {
    int32_t id;
    int fd;
    enum kind type;
    int rx_len;
    int sender_port;
    uint8_t rx[RX_CAPACITY + 1];
};

static struct socket_slot slots[SOCKET_SLOTS];
static int32_t last_id;

static struct socket_slot *lookup(int32_t id)
{
    for (int i = 0; id > 0 && i < SOCKET_SLOTS; i++) {
        if (slots[i].id == id) return &slots[i];
    }
    return NULL;
}

static int32_t adopt(int fd, enum kind type)
{
    int error = EMFILE;
    for (int i = 0; i < SOCKET_SLOTS && last_id < INT32_MAX; i++) {
        if (slots[i].id != 0) continue;
        if (fcntl(fd, F_SETFL, O_NONBLOCK) == -1) {
            error = errno;
            break;
        }
        slots[i] = (struct socket_slot){ .id = ++last_id, .fd = fd, .type = type };
        return last_id;
    }
    close(fd);
    return -error;
}

static struct sockaddr_in endpoint(int32_t port)
{
    return (struct sockaddr_in){
        .sin_family = AF_INET, .sin_port = htons((uint16_t)port),
        .sin_addr.s_addr = htonl(INADDR_LOOPBACK)
    };
}

static int wait_ready(int fd, int write_ready, int32_t ms)
{
    if (ms <= 0 || fd < 0 || fd >= FD_SETSIZE) return -EINVAL;
    fd_set fds;
    FD_ZERO(&fds);
    FD_SET(fd, &fds);
    struct timeval tv = { .tv_sec = ms / 1000, .tv_usec = (ms % 1000) * 1000 };
    int rc = select(fd + 1, write_ready ? NULL : &fds, write_ready ? &fds : NULL, NULL, &tv);
    return rc > 0 ? 0 : rc == 0 ? -ETIMEDOUT : -errno;
}

static int32_t bind_socket(enum kind type, int32_t port)
{
    if (port < 0 || port > UINT16_MAX) return -EINVAL;
    int fd = socket(AF_INET, type == DATAGRAM ? SOCK_DGRAM : SOCK_STREAM, 0);
    if (fd < 0) return -errno;
    struct sockaddr_in addr = endpoint(port);
    if (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) < 0 ||
        (type == LISTENER && listen(fd, 1) < 0)) {
        int error = errno;
        close(fd);
        return -error;
    }
    return adopt(fd, type);
}

int32_t prime_idf_tcp_listen(int32_t port) { return bind_socket(LISTENER, port); }
int32_t prime_idf_udp_bind(int32_t port) { return bind_socket(DATAGRAM, port); }

int32_t prime_idf_close(int32_t id)
{
    struct socket_slot *s = lookup(id);
    if (!s) return -EBADF;
    s->id = 0;
    return close(s->fd) == 0 ? 0 : -errno;
}

int32_t prime_idf_local_port(int32_t id)
{
    struct socket_slot *s = lookup(id);
    if (!s) return -EBADF;
    struct sockaddr_in addr;
    socklen_t len = sizeof(addr);
    if (getsockname(s->fd, (struct sockaddr *)&addr, &len) != 0) return -errno;
    return ntohs(addr.sin_port);
}

int32_t prime_idf_tcp_connect(int32_t port, int32_t ms)
{
    if (port <= 0 || port > UINT16_MAX || ms <= 0) return -EINVAL;
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) return -errno;
    int32_t id = adopt(fd, STREAM);
    if (id < 0) return id;
    struct sockaddr_in addr = endpoint(port);
    if (connect(fd, (struct sockaddr *)&addr, sizeof(addr)) == 0) return id;
    int rc = -errno;
    if (rc == -EINPROGRESS) {
        rc = wait_ready(fd, 1, ms);
        if (rc == 0) {
            int error = 0;
            socklen_t len = sizeof(error);
            rc = getsockopt(fd, SOL_SOCKET, SO_ERROR, &error, &len) < 0 ? -errno : -error;
        }
    }
    if (rc == 0) return id;
    prime_idf_close(id);
    return rc;
}

int32_t prime_idf_tcp_accept(int32_t id, int32_t ms)
{
    struct socket_slot *s = lookup(id);
    if (!s || s->type != LISTENER) return -EBADF;
    int rc = wait_ready(s->fd, 0, ms);
    if (rc < 0) return rc;
    int fd = accept(s->fd, NULL, NULL);
    return fd < 0 ? -errno : adopt(fd, STREAM);
}

int32_t prime_idf_tcp_write(int32_t id, const uint8_t *data, int32_t len, int32_t ms)
{
    struct socket_slot *s = lookup(id);
    if (!s || s->type != STREAM) return -EBADF;
    if (len < 0) return -EINVAL;
    int rc = wait_ready(s->fd, 1, ms);
    if (rc < 0) return rc;
    rc = send(s->fd, data, len, 0);
    return rc < 0 ? -errno : rc;
}

int32_t prime_idf_udp_send_to(int32_t id, int32_t port, const uint8_t *data, int32_t len, int32_t ms)
{
    struct socket_slot *s = lookup(id);
    if (!s || s->type != DATAGRAM) return -EBADF;
    if (port <= 0 || port > UINT16_MAX || len < 0) return -EINVAL;
    int rc = wait_ready(s->fd, 1, ms);
    if (rc < 0) return rc;
    struct sockaddr_in dest = endpoint(port);
    rc = sendto(s->fd, data, len, 0, (struct sockaddr *)&dest, sizeof(dest));
    return rc < 0 ? -errno : rc;
}

static int32_t receive(int32_t id, enum kind type, int32_t limit, int32_t ms)
{
    struct socket_slot *s = lookup(id);
    if (!s || s->type != type) return -EBADF;
    s->rx_len = 0;
    s->sender_port = 0;
    if (limit <= 0 || limit > RX_CAPACITY) return -EINVAL;
    int rc = wait_ready(s->fd, 0, ms);
    if (rc < 0) return rc;
    if (type == STREAM) {
        rc = recv(s->fd, s->rx, limit, 0);
    } else {
        struct sockaddr_in peer;
        socklen_t len = sizeof(peer);
        rc = recvfrom(s->fd, s->rx, limit + 1, 0, (struct sockaddr *)&peer, &len);
        if (rc > limit) return -EMSGSIZE;
        if (rc >= 0) s->sender_port = ntohs(peer.sin_port);
    }
    if (rc < 0) return -errno;
    s->rx_len = rc;
    return rc;
}

int32_t prime_idf_tcp_read(int32_t id, int32_t limit, int32_t ms) { return receive(id, STREAM, limit, ms); }
int32_t prime_idf_udp_recv_from(int32_t id, int32_t limit, int32_t ms) { return receive(id, DATAGRAM, limit, ms); }

int32_t prime_idf_rx_byte(int32_t id, int32_t index)
{
    struct socket_slot *s = lookup(id);
    if (!s) return -EBADF;
    if (index < 0 || index >= s->rx_len) return -ERANGE;
    return s->rx[index];
}

int32_t prime_idf_sender_port(int32_t id)
{
    struct socket_slot *s = lookup(id);
    if (!s || s->type != DATAGRAM) return -EBADF;
    return s->sender_port ? s->sender_port : -ENOTCONN;
}
