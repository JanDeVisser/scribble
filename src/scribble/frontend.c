/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <engine.h>
#include <http.h>
#include <json.h>
#include <sv.h>
#include <options.h>

int main(int argc, char **argv)
{
    char  *program_dir_or_file = NULL;
    int    scribble_param_count = 0;
    char **scribble_params = NULL;

    for (int ix = 1; ix < argc; ++ix) {
        if (!program_dir_or_file) {
            if (!strncmp(argv[ix], "--", 2) && (strlen(argv[ix]) > 2)) {
                StringView  option = sv_from(argv[ix] + 2);
                StringView  value = sv_from("true");
                char const *equals = strchr(argv[ix] + 2, '=');
                if (equals) {
                    option = (StringView) { argv[ix] + 2, equals - argv[ix] - 2 };
                    value = sv_from(equals + 1);
                }
                set_option(option, value);
            } else {
                program_dir_or_file = argv[ix];
            }
        } else {
            scribble_param_count = argc - ix;
            scribble_params = argv + ix;
        }
    }
    set_option(sv_from("scribble-dir"), sv_from(SCRIBBLE_DIR));
    log_init();

    JSONValue config = json_object();
    json_set(&config, "debug_intermediate", json_bool(true));
    json_set_cstr(&config, "target", program_dir_or_file);
    socket_t conn_fd = MUST(Socket, engine_start_backend());

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
        if (sv_eq_cstr(request.url, "/bootstrap/config")) {
            HttpResponse response = { 0 };
            response.status = HTTP_STATUS_OK;
            response.body = json_encode(config);
            http_response_send(conn_fd, &response);
            continue;
        }
        if (sv_eq_cstr(request.url, "/work")) {
            HttpResponse response = { 0 };
            response.status = HTTP_STATUS_NOW_CLIENT;
            http_response_send(conn_fd, &response);
            printf("[S] Sending /callback... ");
            http_get_request(conn_fd, sv_from("/callback"), (StringList) {0});
            printf("Sent\n");
            HttpResponse callback_response = MUST(HttpResponse, http_response_receive(conn_fd));
            printf("[S] Got back %s\n", http_status_to_string(response.status));
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
    socket_close(conn_fd);
}
