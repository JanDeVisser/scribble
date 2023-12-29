/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ctype.h>
#include <errno.h>
#include <netinet/in.h>
#include <stddef.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

#include <http.h>
#include <io.h>

void send_message(int fd, char const *url)
{
    printf("[C] Sending %s... ", url);
    HttpRequest request = { 0 };
    request.method = HTTP_METHOD_GET;
    request.url = sv_from(url);
    http_request_send(fd, &request);
    printf("Sent\n");

    HttpResponse response = MUST(HttpResponse, http_response_receive(fd));
    printf("[C] Got back %s\n", http_status_to_string(response.status));
}

int main(int argc, char **argv)
{
    printf("[C] Started http_client\n");
    int conn_fd = MUST(Int, unix_socket_connect(sv_from(argv[1])));
    printf("[C] Connected to local socket\n");

    send_message(conn_fd, "/hello");
    send_message(conn_fd, "/goodbye");
    close(conn_fd);
    return 0;
}
