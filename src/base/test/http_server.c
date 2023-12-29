/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ctype.h>
#include <errno.h>
#include <netinet/in.h>
#include <stddef.h>
#include <sys/fcntl.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

#include <fs.h>
#include <http.h>
#include <io.h>
#include <process.h>

int main(int argc, char **argv)
{
    StringView path = sv_printf("/tmp/http-test-%zu", getpid());
    int const  listen_fd = MUST(Int, unix_socket_listen(path));
    printf("[S] Listening to socket\n");

    Process *client = process_create(sv_from("http_client"), sv_cstr(path));
    MUST(Int, process_start(client));
    printf("[S] Started client, pid = %d\n", client->pid);

    int const conn_fd = MUST(Int, socket_accept(listen_fd));
    printf("[S] Got client connection\n");

    while (true) {
        printf("[S] Waiting for request\n");
        HttpRequest request = MUST(HttpRequest, http_request_receive(conn_fd));
        printf("[S] Got %.*s\n", SV_ARG(request.url));
        if (sv_eq_cstr(request.url, "/hello")) {
            HttpResponse response = { 0 };
            response.status = HTTP_STATUS_HELLO;
            http_response_send(conn_fd, &response);
            continue;
        }
        if (sv_eq_cstr(request.url, "/goodbye")) {
            HttpResponse response = { 0 };
            response.status = HTTP_STATUS_OK;
            http_response_send(conn_fd, &response);
            break;
        }
        HttpResponse response = { 0 };
        response.status = HTTP_STATUS_NOT_FOUND;
        http_response_send(conn_fd, &response);
    }
    close(conn_fd);
    close(listen_fd);
    MUST(Int, fs_unlink(path));
    return 0;
}
