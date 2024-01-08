/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <unistd.h>

#include <arch/arm64/arm64.h>
#include <binder.h>
#include <config.h>
#include <engine.h>
#include <execute.h>
#include <fs.h>
#include <graph.h>
#include <http.h>
#include <intermediate.h>
#include <options.h>
#include <parser.h>
#include <type.h>

void shutdown_backend(BackendConnection *conn, int code)
{
    socket_close(conn->fd);
    fs_unlink(conn->socket);
}

void exit_backend(BackendConnection *conn, StringView error)
{
    HttpRequest request = { 0 };
    request.method = HTTP_METHOD_POST;
    request.url = sv_from("/goodbye");
    request.body = sv_printf("Error: %.*s", SV_ARG(error));
    http_request_send(conn->fd, &request);
    shutdown_backend(conn, 1);
}

bool bootstrap_backend(BackendConnection *conn)
{
    if (http_get_message(conn->fd, sv_from("/hello"), (StringList) { 0 }) != HTTP_STATUS_HELLO) {
        fatal("/hello failed");
    }
    conn->config = HTTP_GET_REQUEST_MUST(conn->fd, "/bootstrap/config", (StringList) { 0 });
    printf("[C] Got config\n");
    return true;
}

ErrorOrInt compile_program(BackendConnection *conn)
{
    bool          debug = json_get_bool(&conn->config, "debug_parser", false);

    if (debug && http_post_message(conn->fd, sv_from("/parser/start"), (JSONValue) { 0 }) != HTTP_STATUS_OK) {
        fatal("/parser/start failed");
    }
    ParserContext parse_result = parse(conn);
    if (parse_result.first_error) {
        JSONValue errors = json_array();
        for (ScribbleError *err = parse_result.first_error; err; err = err->next) {
            JSONValue error = json_object();
            json_set(&error, "message", json_string(err->message));
            JSONValue loc = json_object();
            json_set(&loc, "file", json_string(err->token.loc.file));
            json_set(&loc, "line", json_int(err->token.loc.line));
            json_set(&loc, "column", json_int(err->token.loc.column));
            json_set(&error, "location", loc);
            json_append(&errors, error);
        }
        HTTP_POST_MUST(conn->fd, "/parser/errors", errors);
        exit(1);
    }
    if (debug) {
        HTTP_GET_MUST(conn->fd, "/parser/done", (StringList) { 0 });
    }

    if (json_get_bool(&conn->config, "graph", false)) {
        graph_program(parse_result.program);
    }
    BoundNode *ast = bind_program(conn, parse_result.program);
    IRProgram  ir = generate(conn, ast);
    if (sv_eq_cstr(json_get_string(&conn->config, "mode", sv_from("compile")), "emulate")) {
        return execute(conn, ir);
    }
    return output_arm64(conn, &ir);
}

int main(int argc, char **argv)
{
    if (argc != 2) {
        exit(1);
    }
    set_option(sv_from("trace"), sv_from("IPC"));
    log_init();
    type_registry_init();
    StringView socket_path = sv_from(argv[1]);

    BackendConnection conn = engine_initialize_backend(socket_path);

    bootstrap_backend(&conn);
    compile_program(&conn);

    return 0;
}
