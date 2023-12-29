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

#include <io.h>
#include <mem.h>

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

ErrorOrInt unix_socket_listen(StringView socket_name)
{
    int const listen_fd = socket(PF_LOCAL, SOCK_STREAM, 0);
    if (listen_fd < 0) {
        ERROR(Int, IOError, 0, "Cannot create socket: %s", strerror(errno));
    }

    struct sockaddr_un sock_addr = { 0 };
    sock_addr.sun_family = AF_LOCAL;
    if (socket_name.length >= sizeof(sock_addr.sun_path)) {
        ERROR(Int, IOError, 0, "Local socket name '%.*s' too long: %zu >= %zu",
            SV_ARG(socket_name), socket_name.length, sizeof(sock_addr.sun_path));
    }
    memcpy(sock_addr.sun_path, socket_name.ptr, socket_name.length);
    sock_addr.sun_path[socket_name.length] = '\0';
    size_t serv_size = offsetof(struct sockaddr_un, sun_path) + socket_name.length + 1;
    if (bind(listen_fd, (struct sockaddr *) &sock_addr, serv_size) < 0) {
        ERROR(Int, IOError, 0, "Cannot bind to local socket '%.*s': %s", SV_ARG(socket_name), strerror(errno));
    }
    if (listen(listen_fd, 1) < 0) {
        ERROR(Int, IOError, 0, "Cannot listen on local socket '%.*s': %s", SV_ARG(socket_name), strerror(errno));
    }
    RETURN(Int, listen_fd);
}

ErrorOrInt tcpip_socket_listen(StringView ip_address, int port)
{
    NYI();
}

ErrorOrInt socket_accept(int socket_fd)
{
    int conn_fd = accept(socket_fd, NULL, NULL);
    if (conn_fd < 0) {
        ERROR(Int, IOError, 0, "Cannot accept connection on local socket: %s", strerror(errno));
    }
    TRY(Int, fd_make_nonblocking(conn_fd));
    RETURN(Int, conn_fd);
}

ErrorOrInt unix_socket_connect(StringView socket_name)
{
    int conn_fd = socket(PF_LOCAL, SOCK_STREAM, 0);
    if (conn_fd < 0) {
        ERROR(Int, IOError, 0, "Cannot create socket: %s", strerror(errno));
    }

    struct sockaddr_un sock_addr = { 0 };
    sock_addr.sun_family = AF_LOCAL;
    if (socket_name.length >= sizeof(sock_addr.sun_path)) {
        ERROR(Int, IOError, 0, "Local socket name '%.*s' too long: %zu >= %zu",
            SV_ARG(socket_name), socket_name.length, sizeof(sock_addr.sun_path));
    }
    memcpy(sock_addr.sun_path, socket_name.ptr, socket_name.length);
    sock_addr.sun_path[socket_name.length] = '\0';
    size_t sock_addr_size = offsetof(struct sockaddr_un, sun_path) + socket_name.length + 1;
    if (connect(conn_fd, (struct sockaddr *) &sock_addr, sock_addr_size) < 0) {
        ERROR(Int, IOError, 0, "Cannot connect to local socket '%.*s': %s", SV_ARG(socket_name), strerror(errno));
    }
    TRY(Int, fd_make_nonblocking(conn_fd));
    RETURN(Int, conn_fd);
}

ErrorOrInt tcpip_socket_connect(StringView ip_address, int port)
{
    int conn_fd;
    if ((conn_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
        ERROR(Int, IOError, 0, "Cannot create socket: %s", strerror(errno));
    }

    struct sockaddr_in server_address = TRY_TO(SockAddrIn, Int, tcpip_address_resolve(ip_address));
    server_address.sin_family = AF_INET;
    server_address.sin_port = htons(port);

    if (connect(conn_fd, (struct sockaddr *) &server_address, sizeof(server_address)) < 0) {
        ERROR(Int, IOError, 0, "Cannot connect to TCP/IP address '%.*s:%d': %s",
            SV_ARG(ip_address), port, strerror(errno));
    }
    TRY(Int, fd_make_nonblocking(conn_fd));
    RETURN(Int, conn_fd);
}

ErrorOrStringView socket_read(int fd)
{
    StringBuilder sb = { 0 };
    char          buffer[1024];
    size_t        total = 0;
    struct pollfd pollfds[] = {
        { .fd = fd, .events = POLLIN, .revents = 0 },
    };
    while (poll(pollfds, 1, -1) < 0) {
        fprintf(stderr, "poll() interupted\n");
    }
    assert(pollfds[0].revents & POLLIN);
    while (true) {
        ssize_t bytes_read = read(fd, buffer, sizeof(buffer));
        if (bytes_read < 0) {
            if (total == 0) {
                ERROR(StringView, HttpError, 0, "Failed to read from socket: %d: %s", errno, strerror(errno));
            } else {
                break;
            }
        }
        total += bytes_read;
        sb_append_chars(&sb, buffer, bytes_read);
        if (bytes_read < sizeof(buffer)) {
            break;
        }
    }
    RETURN(StringView, sb.view);
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
