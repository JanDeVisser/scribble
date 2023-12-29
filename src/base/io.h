/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <netinet/in.h>

#include <error_or.h>
#include <sv.h>

#ifndef __IO_H__
#define __IO_H__

ErrorOr(SockAddrIn, struct sockaddr_in);

ErrorOrSockAddrIn tcpip_address_resolve(StringView ip_address);
ErrorOrInt        fd_make_nonblocking(int fd);
ErrorOrInt        unix_socket_listen(StringView socket_name);
ErrorOrInt        tcpip_socket_listen(StringView ip_address, int port);
ErrorOrInt        socket_accept(int socket_fd);
ErrorOrInt        unix_socket_connect(StringView socket_name);
ErrorOrInt        tcpip_socket_connect(StringView ip_address, int port);
ErrorOrStringView socket_read(int fd);
ErrorOrChar       read_file_by_name(char const *file_name);
ErrorOrChar       read_file_at(int dir_fd, char const *file_name);
ErrorOrChar       read_file(int fd);

#endif /* __IO_H__ */
