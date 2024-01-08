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
#include <options.h>

void send_message(socket_t socket, char const *url, HttpStatus expect)
{
    printf("[C] Sending %s... ", url);
    HttpRequest request = { 0 };
    request.method = HTTP_METHOD_GET;
    request.url = sv_from(url);
    http_request_send(socket, &request);
    printf("Sent\n");

    HttpResponse response = MUST(HttpResponse, http_response_receive(socket));
    printf("[C] Got back '%s'\n", http_status_to_string(response.status));
    if (response.status != expect) {
        fatal("Expected '%s'", http_status_to_string(expect));
    }
}

int main(int argc, char **argv)
{
    set_option(sv_from("trace"), sv_from("IPC"));
    log_init();
    printf("[C] Started http_client\n");
    socket_t socket = MUST(Socket, unix_socket_connect(sv_from(argv[1])));
    printf("[C] Connected to local socket\n");

    send_message(socket, "/hello", HTTP_STATUS_HELLO);
    send_message(socket, "/work", HTTP_STATUS_NOW_CLIENT);

    while (true) {
        printf("[C] Waiting for callback\n");
        HttpRequest request = MUST(HttpRequest, http_request_receive(socket));
        printf("[C] Got %.*s\n", SV_ARG(request.url));
        if (sv_eq_cstr(request.url, "/callback")) {
            HttpResponse response = { 0 };
            response.status = HTTP_STATUS_OK;
            http_response_send(socket, &response);
            break;
        }
        HttpResponse response = { 0 };
        response.status = HTTP_STATUS_NOT_FOUND;
        http_response_send(socket, &response);
    }

    send_message(socket, "/goodbye", HTTP_STATUS_OK);
    socket_close(socket);
    return 0;
}
