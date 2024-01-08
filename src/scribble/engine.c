/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stddef.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

#define STATIC_ALLOCATOR
#include <allocate.h>
#include <datum.h>
#include <engine.h>
#include <execute.h>
#include <http.h>
#include <io.h>
#include <process.h>

ErrorOrSocket engine_start_backend()
{
    StringView     path = sv_printf("/tmp/scribble-engine-%zu", getpid());
    socket_t const listen_fd = MUST(Socket, unix_socket_listen(path));
    TRY_TO(Int, Socket, execute(sv_from("scribble-backend"), sv_cstr(path)));
    return socket_accept(listen_fd);
}

BackendConnection engine_initialize_backend(StringView path)
{
    socket_t          conn_fd = MUST(Socket, unix_socket_connect(path));
    BackendConnection conn = { 0 };
    conn.fd = conn_fd;
    conn.context = NULL;
    conn.socket = path;
    return conn;
}
