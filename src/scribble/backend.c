/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <pthread.h>
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

void compile_program(BackendConnection *conn)
{
    JSONValue stages = MUST_OPTIONAL(JSONValue, json_get(&conn->config, "stages"));
    assert(stages.type == JSON_TYPE_ARRAY);
    SyntaxNode *program = NULL;
    BoundNode  *ast = NULL;
    IRProgram   ir = { 0 };
    for (size_t ix = 0; ix < json_len(&stages); ++ix) {
        JSONValue  stage = MUST_OPTIONAL(JSONValue, json_at(&stages, ix));
        StringView name = json_get_string(&stage, "name", sv_null());
        if (sv_eq_cstr(name, "parse")) {
            ParserContext parse_result = parse(conn, stage);
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
            if (json_get_bool(&stage, "graph", false)) {
                graph_program(parse_result.program);
            }
            program = parse_result.program;
            continue;
        }
        if (sv_eq_cstr(name, "bind")) {
            assert(program != NULL);
            bool debug = json_get_bool(&stage, "debug", false);
            if (debug) {
                HTTP_GET_MUST(conn->fd, "/bind/start", sl_create());
            }
            ast = bind_program(conn, program);
            if (debug) {
                HTTP_GET_MUST(conn->fd, "/bind/done", sl_create());
            }
            if (json_get_bool(&stage, "graph", false)) {
                graph_ast(0, ast);
            }
        }
        if (sv_eq_cstr(name, "ir")) {
            assert(ast != NULL);
            bool debug = json_get_bool(&stage, "debug", false);
            if (debug) {
                HTTP_GET_MUST(conn->fd, "/ir/start", sl_create());
            }
            ir = generate(conn, ast);
            if (debug) {
                HTTP_GET_MUST(conn->fd, "/ir/done", sl_create());
            }
        }
        if (sv_eq_cstr(name, "generate")) {
            assert(ir.obj_type == OT_PROGRAM);
            bool debug = json_get_bool(&stage, "debug", false);
            if (debug) {
                HTTP_GET_MUST(conn->fd, "/generate/start", sl_create());
            }
            output_arm64(conn, &ir);
            if (debug) {
                HTTP_GET_MUST(conn->fd, "/generate/done", sl_create());
            }
        }
        if (sv_eq_cstr(name, "emulate")) {
            assert(ir.obj_type == OT_PROGRAM);
            bool debug = json_get_bool(&stage, "debug", false);
            if (debug) {
                HTTP_GET_MUST(conn->fd, "/emulate/start", sl_create());
            }
            execute(conn, ir);
            if (debug) {
                HTTP_GET_MUST(conn->fd, "/emulate/done", sl_create());
            }
        }
    }
}

extern int backend_main(StringView path)
{
    socket_t          conn_fd = MUST(Socket, unix_socket_connect(path));
    BackendConnection conn = { 0 };

    conn.fd = conn_fd;
    conn.context = NULL;
    conn.socket = path;

    if (http_get_message(conn.fd, sv_from("/hello"), (StringList) { 0 }) != HTTP_STATUS_HELLO) {
        fatal("/hello failed");
    }
    conn.config = HTTP_GET_REQUEST_MUST(conn.fd, "/bootstrap/config", (StringList) { 0 });
    printf("[C] Got config\n");

    compile_program(&conn);

    return 0;
}

void *backend_main_wrapper(void *path)
{
    backend_main((StringView) { (char const *) path, strlen(path) });
    return NULL;
}

ErrorOrSocket start_backend_thread()
{
    StringView     path = sv_printf("/tmp/scribble-engine-%d", getpid());
    socket_t const listen_fd = MUST(Socket, unix_socket_listen(path));
    pthread_t      thread;
    int            ret;

    if ((ret = pthread_create(&thread, NULL, backend_main_wrapper, (void *) path.ptr)) != 0) {
        fatal("Could not start backend thread: %s", strerror(ret));
    }
    trace(CAT_IPC, "Started client thread");
    return socket_accept(listen_fd);
}
