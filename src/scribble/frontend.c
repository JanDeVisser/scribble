/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <unistd.h>

#include <engine.h>
#include <http.h>
#include <json.h>
#include <options.h>
#include <process.h>
#include <sv.h>

extern ErrorOrSocket start_backend_thread();

ErrorOrSocket start_backend_process()
{
    StringView     path = sv_printf("/tmp/scribble-engine-%d", getpid());
    socket_t const listen_fd = MUST(Socket, unix_socket_listen(path));
    Process       *client = process_create(sv_from("scribble-backend"), sv_cstr(path));
    MUST(Int, process_start(client));
    trace(CAT_IPC, "Started client, pid = %d\n", client->pid);
    return socket_accept(listen_fd);
}

void handle_parser_message(socket_t socket, HttpRequest request)
{
    if (sv_eq_cstr(request.url, "/parser/start")) {
        printf("[parser] started\n");
        HTTP_RESPONSE_OK(socket);
        return;
    }
    if (sv_eq_cstr(request.url, "/parser/done")) {
        printf("[parser] done\n");
        HTTP_RESPONSE_OK(socket);
        return;
    }
    if (sv_eq_cstr(request.url, "/parser/info")) {
        JSONValue msg = MUST(JSONValue, json_decode(request.body));
        printf("[parser] %.*s\n", SV_ARG(msg.string));
        HTTP_RESPONSE_OK(socket);
        return;
    }
    if (sv_eq_cstr(request.url, "/parser/node")) {
        JSONValue node = MUST(JSONValue, json_decode(request.body));
        printf("[parser] node: %.*s\n", SV_ARG(json_get_string(&node, "type", sv_null())));
        HTTP_RESPONSE_OK(socket);
        return;
    }
    HTTP_RESPONSE(socket, HTTP_STATUS_NOT_FOUND);
}

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
    set_option(sv_from("trace"), sv_from("IPC"));
    log_init();

    JSONValue config = json_object();
    JSONValue stages = json_array();
    JSONValue stage = json_object();
    json_set_cstr(&stage, "name", "parse");
    json_set_cstr(&stage, "target", program_dir_or_file);
    json_set(&stage, "debug", json_bool(true));
    json_append(&stages, stage);
    stage = json_object();
    json_set_cstr(&stage, "name", "bind");
    json_set(&stage, "debug", json_bool(true));
    json_append(&stages, stage);
    stage = json_object();
    json_set_cstr(&stage, "name", "ir");
    json_set(&stage, "debug", json_bool(true));
    json_append(&stages, stage);
    json_set_cstr(&stage, "name", "generate");
    json_set(&stage, "debug", json_bool(true));
    json_append(&stages, stage);
    json_set(&config, "stages", stages);

#ifndef SCRIBBLE_THREADED_BACKEND
    socket_t conn_fd = MUST(Socket, start_backend_process());
#else
    socket_t conn_fd = MUST(Socket, start_backend_thread());
#endif /* SCRIBBLE_THREADED_BACKEND */


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
        if (sv_startswith(request.url, sv_from("/parser/"))) {
            handle_parser_message(conn_fd, request);
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
