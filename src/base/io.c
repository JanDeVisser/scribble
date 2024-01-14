/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <arpa/inet.h>
#include <errno.h>
#include <netdb.h>
#include <netinet/in.h>
#include <poll.h>
#include <stddef.h>
#include <sys/fcntl.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>

#define STATIC_ALLOCATOR
#include <allocate.h>
#include <io.h>
#include <mem.h>

#define BUF_SZ 4096
#define NO_SOCKET ((socket_t) -1)

typedef struct {
    int    fd;
    char   buffer[BUF_SZ];
    size_t start;
    size_t num;
} Socket;

FREE_LIST(Socket, socket);
FREE_LIST_IMPL(Socket, socket);

static Socket **s_sockets = NULL;
static size_t   s_num_sockets = 0;
static size_t   s_cap_sockets = 0;

socket_t socket_allocate(int fd)
{
    if (s_sockets == NULL) {
        s_num_sockets = 0;
        s_cap_sockets = 4;
        s_sockets = allocate_array(Socket *, s_cap_sockets);
    }
    for (size_t ix = 0; ix < s_num_sockets; ++ix) {
        if (s_sockets[ix]->fd == -1) {
            s_sockets[ix]->fd = fd;
            s_sockets[ix]->num = 0;
            s_sockets[ix]->start = 0;
            return ix;
        }
    }
    if (s_num_sockets + 1 >= s_cap_sockets) {
        s_cap_sockets *= 2;
        Socket **new_sockets = allocate_array(Socket *, s_cap_sockets);
        memmove(new_sockets, s_sockets, s_num_sockets * sizeof(Socket *));
    }
    socket_t ret = s_num_sockets++;
    s_sockets[ret] = allocate_new(Socket);
    s_sockets[ret]->fd = fd;
    return ret;
}

void socket_close(socket_t socket)
{
    close(s_sockets[socket]->fd);
    s_sockets[socket]->fd = -1;
}

ErrorOrInt fd_make_nonblocking(int fd)
{
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags < 0) {
        ERROR(Int, IOError, 0, "Cannot get file descriptor flags: %s", strerror(errno));
    }
    flags = flags | O_NONBLOCK;
    if (fcntl(fd, F_SETFL, flags) < 0) {
        ERROR(Int, IOError, 0, "Cannot make file descriptor non-blocking: %s", strerror(errno));
    }
    RETURN(Int, fd);
}

ErrorOrSockAddrIn tcpip_address_resolve(StringView ip_address)
{
    struct addrinfo hints, *res, *res0;
    int             error;

    memset(&hints, 0, sizeof(hints));
    hints.ai_family = PF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    if ((error = getaddrinfo(sv_cstr(ip_address), NULL, &hints, &res0)) != 0) {
        ERROR(SockAddrIn, IOError, 0, "Error resolving IP address '%.*s': %s",
            SV_ARG(ip_address), gai_strerror(error));
    }
    if (!res0) {
        ERROR(SockAddrIn, IOError, 0, "Could not resolve IP address '%.*s': %s",
            SV_ARG(ip_address), gai_strerror(error));
    }
    struct sockaddr_in addr;
    for (res = res0; res; res = res->ai_next) {
        if (res->ai_family == AF_INET && res->ai_socktype == SOCK_STREAM) {
            assert(res->ai_addrlen == sizeof(struct sockaddr_in));
            memcpy(&addr, res->ai_addr, sizeof(struct sockaddr_in));
            freeaddrinfo(res0);
            RETURN(SockAddrIn, addr);
        }
    }
    freeaddrinfo(res0);
    ERROR(SockAddrIn, IOError, 0, "Could not resolve address '%.*s' to an IP address", SV_ARG(ip_address));
}

ErrorOrSocket unix_socket_listen(StringView socket_name)
{
    int const listen_fd = socket(PF_LOCAL, SOCK_STREAM, 0);
    if (listen_fd < 0) {
        ERROR(Socket, IOError, 0, "Cannot create socket: %s", strerror(errno));
    }

    struct sockaddr_un sock_addr = { 0 };
    sock_addr.sun_family = AF_LOCAL;
    if (socket_name.length >= sizeof(sock_addr.sun_path)) {
        ERROR(Socket, IOError, 0, "Local socket name '%.*s' too long: %zu >= %zu",
            SV_ARG(socket_name), socket_name.length, sizeof(sock_addr.sun_path));
    }
    memcpy(sock_addr.sun_path, socket_name.ptr, socket_name.length);
    sock_addr.sun_path[socket_name.length] = '\0';
    size_t serv_size = offsetof(struct sockaddr_un, sun_path) + socket_name.length + 1;
    if (bind(listen_fd, (struct sockaddr *) &sock_addr, serv_size) < 0) {
        ERROR(Socket, IOError, 0, "Cannot bind to local socket '%.*s': %s", SV_ARG(socket_name), strerror(errno));
    }
    if (listen(listen_fd, 1) < 0) {
        ERROR(Socket, IOError, 0, "Cannot listen on local socket '%.*s': %s", SV_ARG(socket_name), strerror(errno));
    }
    RETURN(Socket, socket_allocate(listen_fd));
}

ErrorOrSocket tcpip_socket_listen(StringView ip_address, int port)
{
    NYI();
}

ErrorOrSocket socket_accept(socket_t socket)
{
    int conn_fd = accept(s_sockets[socket]->fd, NULL, NULL);
    if (conn_fd < 0) {
        ERROR(Socket, IOError, 0, "Cannot accept connection on local socket: %s", strerror(errno));
    }
    TRY_TO(Int, Socket, fd_make_nonblocking(conn_fd));
    RETURN(Socket, socket_allocate(conn_fd));
}

ErrorOrSocket unix_socket_connect(StringView socket_name)
{
    int conn_fd = socket(PF_LOCAL, SOCK_STREAM, 0);
    if (conn_fd < 0) {
        ERROR(Socket, IOError, 0, "Cannot create socket: %s", strerror(errno));
    }

    struct sockaddr_un sock_addr = { 0 };
    sock_addr.sun_family = AF_LOCAL;
    if (socket_name.length >= sizeof(sock_addr.sun_path)) {
        ERROR(Socket, IOError, 0, "Local socket name '%.*s' too long: %zu >= %zu",
            SV_ARG(socket_name), socket_name.length, sizeof(sock_addr.sun_path));
    }
    memcpy(sock_addr.sun_path, socket_name.ptr, socket_name.length);
    sock_addr.sun_path[socket_name.length] = '\0';
    size_t sock_addr_size = offsetof(struct sockaddr_un, sun_path) + socket_name.length + 1;
    if (connect(conn_fd, (struct sockaddr *) &sock_addr, sock_addr_size) < 0) {
        ERROR(Socket, IOError, 0, "Cannot connect to local socket '%.*s': %s", SV_ARG(socket_name), strerror(errno));
    }
    TRY_TO(Int, Socket, fd_make_nonblocking(conn_fd));
    RETURN(Socket, socket_allocate(conn_fd));
}

ErrorOrSocket tcpip_socket_connect(StringView ip_address, int port)
{
    int conn_fd;
    if ((conn_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
        ERROR(Socket, IOError, 0, "Cannot create socket: %s", strerror(errno));
    }

    struct sockaddr_in server_address = TRY_TO(SockAddrIn, Socket, tcpip_address_resolve(ip_address));
    server_address.sin_family = AF_INET;
    server_address.sin_port = htons(port);

    if (connect(conn_fd, (struct sockaddr *) &server_address, sizeof(server_address)) < 0) {
        ERROR(Socket, IOError, 0, "Cannot connect to TCP/IP address '%.*s:%d': %s",
            SV_ARG(ip_address), port, strerror(errno));
    }
    TRY_TO(Int, Socket, fd_make_nonblocking(conn_fd));
    RETURN(Socket, socket_allocate(conn_fd));
}

ErrorOrSize read_available_bytes(Socket *s)
{
    char    buffer[BUF_SZ];
    ssize_t bytes_read = read(s->fd, buffer, BUF_SZ - s->num);
    if (bytes_read < 0) {
        if (errno != EAGAIN) {
            ERROR(Size, HttpError, 0, "Failed to read from socket: %d: %s", errno, strerror(errno));
        }
        RETURN(Size, 0);
    }
    for (size_t ix = 0, buf_ix = (s->start + s->num) % BUF_SZ; ix < bytes_read; ++ix, ++s->num, buf_ix = (buf_ix + 1) % BUF_SZ) {
        s->buffer[buf_ix] = buffer[ix];
    }
    RETURN(Size, bytes_read);
}

ErrorOrSize socket_fill_buffer(Socket *s)
{
    TRY(Size, read_available_bytes(s));
    if (s->num > 0) {
        RETURN(Size, s->num);
    }
    struct pollfd pollfds[] = {
        { .fd = s->fd, .events = POLLIN, .revents = 0 },
    };
    while (poll(pollfds, 1, -1) < 0) { }
    assert(pollfds[0].revents & POLLIN);
    TRY(Size, read_available_bytes(s));
    assert(s->num > 0);
    RETURN(Size, s->num);
}

ErrorOrStringView socket_read(socket_t socket, size_t count)
{
    Socket       *s = s_sockets[socket];
    StringBuilder out = sb_create();

    do {
        TRY_TO(Size, StringView, socket_fill_buffer(s));
        if (!count && (s->num > 0)) {
            RETURN(StringView, sv_null());
        }
        for (; s->num > 0 && count > 0; s->start = (s->start + 1) % BUF_SZ, --s->num, --count) {
            int ch = s->buffer[s->start];
            sb_append_char(&out, ch);
        }
    } while (count > 0);
    RETURN(StringView, out.view);
}

ErrorOrStringView socket_readln(socket_t socket)
{
    Socket       *s = s_sockets[socket];
    StringBuilder out = sb_create();
    while (true) {
        TRY_TO(Size, StringView, socket_fill_buffer(s));
        for (; s->num > 0; s->start = (s->start + 1) % BUF_SZ, --s->num) {
            int ch = s->buffer[s->start];
            switch (ch) {
            case '\r':
                break;
            case '\n':
                s->start = (s->start + 1) % BUF_SZ;
                --s->num;
                trace(CAT_IPC, "socket_readln: '%.*s'", SB_ARG(out));
                RETURN(StringView, out.view);
            default:
                sb_append_char(&out, ch);
                break;
            }
        }
    }
}

ErrorOrSize socket_write(socket_t socket, char const *buffer, size_t num)
{
    Socket *s = s_sockets[socket];
    ssize_t written = write(s->fd, buffer, num);
    if (written < 0) {
        ERROR(Size, IOError, 0, "Error writing to socket: %s", strerror(errno));
    }
    if (written < num) {
        ERROR(Size, IOError, 0, "Incomplete write to socket: %d < %d", written, num);
    }
    RETURN(Size, written);
}

ErrorOrSize socket_writeln(socket_t socket, StringView sv)
{
    TRY(Size, socket_write(socket, sv.ptr, sv.length));
    char eol = '\n';
    TRY(Size, socket_write(socket, &eol, 1));
    RETURN(Size, sv.length + 1);
}

ErrorOrChar read_file_by_name(char const *file_name)
{
    int fd = open(file_name, O_RDONLY);
    if (fd < 0) {
        ERROR(Char, IOError, errno, "Could not open file");
    }
    ErrorOrChar ret = read_file(fd);
    close(fd);
    return ret;
}

ErrorOrChar read_file_at(int dir_fd, char const *file_name)
{
    int fd = openat(dir_fd, file_name, O_RDONLY);
    if (fd < 0) {
        ERROR(Char, IOError, errno, "Could not open file");
    }
    ErrorOrChar ret = read_file(fd);
    close(fd);
    return ret;
}

ErrorOrChar read_file(int fd)
{
    struct stat sb;
    if (fstat(fd, &sb) < 0) {
        ERROR(Char, IOError, errno, "Could not fstat file");
    }
    size_t sz = sb.st_size;
    char  *buffer = mem_allocate(sz + 1);
    if (!buffer) {
        ERROR(Char, OutOfMemory, errno, "Out-of-memory allocating file buffer");
    }
    buffer[sz] = 0;
    size_t read_chars = read(fd, buffer, sz);
    if (read_chars < sz) {
        ERROR(Char, IOError, errno, "Could not read file");
    }
    RETURN(Char, buffer);
}
