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
        StringView type = json_get_string(&node, "type", sv_null());
        StringView name = json_get_string(&node, "name", sv_null());
        printf("[parser] %.*s %.*s\n", SV_ARG(type), SV_ARG(name));
        HTTP_RESPONSE_OK(socket);
        return;
    }
    HTTP_RESPONSE(socket, HTTP_STATUS_NOT_FOUND);
}

void handle_bind_message(socket_t socket, HttpRequest request)
{
    if (sv_eq_cstr(request.url, "/bind/start")) {
        printf("[bind] started\n");
        HTTP_RESPONSE_OK(socket);
        return;
    }
    if (sv_eq_cstr(request.url, "/bind/done")) {
        printf("[bind] done\n");
        HTTP_RESPONSE_OK(socket);
        return;
    }
    if (sv_eq_cstr(request.url, "/bind/info")) {
        JSONValue msg = MUST(JSONValue, json_decode(request.body));
        printf("[bind] %.*s\n", SV_ARG(msg.string));
        HTTP_RESPONSE_OK(socket);
        return;
    }
    if (sv_eq_cstr(request.url, "/bind/syntaxnode")) {
        JSONValue  node = MUST(JSONValue, json_decode(request.body));
        StringView type = json_get_string(&node, "nodetype", sv_null());
        StringView name = json_get_string(&node, "name", sv_null());
        printf("[bind] %.*s %.*s\n", SV_ARG(type), SV_ARG(name));
        HTTP_RESPONSE_OK(socket);
        return;
    }
    if (sv_eq_cstr(request.url, "/bind/boundnode")) {
        JSONValue  node = MUST(JSONValue, json_decode(request.body));
        StringView node_type = json_get_string(&node, "nodetype", sv_null());
        StringView name = json_get_string(&node, "name", sv_null());
        StringView type = json_get_string(&node, "type", sv_null());
        printf("[bind] %.*s %.*s : %.*s\n", SV_ARG(node_type), SV_ARG(name), SV_ARG(type));
        HTTP_RESPONSE_OK(socket);
        return;
    }
    if (sv_eq_cstr(request.url, "/bind/error")) {
        JSONValue iter_stats = MUST(JSONValue, json_decode(request.body));
        int       iteration = json_get_int(&iter_stats, "iteration", 1);
        int       warnings = json_get_int(&iter_stats, "warnings", 0);
        int       total_warnings = json_get_int(&iter_stats, "total_warnings", 0);
        int       errors = json_get_int(&iter_stats, "errors", 0);
        int       unbound = json_get_int(&iter_stats, "unbound", 0);
        printf("[bind] Iteration %d: %d warning(s), %d total warning(s), %d error(s), %d unbound node(s)\n", iteration, warnings, total_warnings, errors, unbound);
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
        trace(CAT_IPC, "[S] Waiting for request");
        HttpRequest request = MUST(HttpRequest, http_request_receive(conn_fd));
        trace(CAT_IPC, "[S] Got %.*s", SV_ARG(request.url));
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
        if (sv_startswith(request.url, sv_from("/bind/"))) {
            handle_bind_message(conn_fd, request);
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
