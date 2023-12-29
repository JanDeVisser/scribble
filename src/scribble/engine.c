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

#define STATIC_ALLOCATOR
#include <allocate.h>
#include <datum.h>
#include <engine.h>
#include <execute.h>
#include <http.h>
#include <io.h>
#include <process.h>

bool engine_bootstrap_processor(EngineConnection *conn, ExecutionMessage msg);
bool engine_parse_processor(EngineConnection *conn, ExecutionMessage msg);
bool engine_bind_processor(EngineConnection *conn, ExecutionMessage msg);
bool engine_intermediate_processor(EngineConnection *conn, ExecutionMessage msg);
bool engine_generate_processor(EngineConnection *conn, ExecutionMessage msg);
bool engine_execute_processor(EngineConnection *conn, ExecutionMessage msg);
bool bootstrap_command_processor(EngineConnection *conn);

// FREE_LIST_IMPL(ObserverStack, observer_stack)

EngineStageExecutor s_executors[(int) ES_MAX] = {
    [ES_INIT] = engine_bootstrap_processor,
    [ES_PARSE] = engine_parse_processor,
    [ES_BIND] = engine_bind_processor,
    [ES_INTERMEDIATE] = engine_intermediate_processor,
    [ES_GENERATE] = engine_generate_processor,
    [ES_EXECUTE] = engine_execute_processor,
    [ES_INTERPRET] = engine_execute_processor,
};

bool engine_callback(EngineConnection *conn, ExecutionMessage msg)
{
    if (!conn->config[conn->stage].debug) {
        return true;
    }
    return s_executors[conn->stage](conn, msg);
}

bool bootstrap_command_processor(EngineConnection *conn)
{
    HttpRequest request = { 0 };
    request.method = HTTP_METHOD_GET;
    request.url = sv_from("/hello");
    http_request_send(conn->fd, &request);
    sv_free(request.request);

    HttpResponse response = MUST(HttpResponse, http_response_receive(conn->fd));
    if (response.status != HTTP_STATUS_OK) {
        fatal("/hello failed");
    }

    request = (HttpRequest) { 0 };
    request.method = HTTP_METHOD_GET;
    request.url = sv_from("/target");
    http_request_send(conn->fd, &request);
    sv_free(request.request);

    response = MUST(HttpResponse, http_response_receive(conn->fd));
    if (response.status != HTTP_STATUS_OK) {
        fatal("/target failed");
    }
    conn->target = response.body;

    request = (HttpRequest) { 0 };
    request.method = HTTP_METHOD_GET;
    request.url = sv_from("/target");
    http_request_send(conn->fd, &request);
    sv_free(request.request);
    return true;
}

bool engine_bootstrap_processor(EngineConnection *conn, ExecutionMessage msg)
{
    switch (msg.type) {
    case EMT_STAGE_INIT: {
        for (size_t ix = 0; ix < (size_t) ES_MAX; ++ix) {
            conn->config[ix].debug = false;
            conn->config[ix].execute = true;
        }
        conn->config[ES_EXECUTE].execute = false;
        conn->config[ES_INTERPRET].execute = false;
        conn->context = NULL;
        return true;
    }
    case EMT_STAGE_START: {
        return bootstrap_command_processor(conn);
    }
    default:
        return true;
    }
}

void engine_start_stage(EngineConnection *conn, EngineStage stage)
{
    if (stage != ES_INIT) {
        s_executors[conn->stage](conn, (ExecutionMessage) { .type = EMT_STAGE_END, .payload = NULL });
    }
    conn->stage = stage;
    s_executors[conn->stage](conn, (ExecutionMessage) { .type = EMT_STAGE_INIT, .payload = NULL });
    s_executors[conn->stage](conn, (ExecutionMessage) { .type = EMT_STAGE_START, .payload = NULL });
}

ErrorOrInt engine_start_backend()
{
    StringView path = sv_printf("/tmp/scribble-engine-%zu", getpid());
    int const  listen_fd = MUST(Int, unix_socket_listen(path));
    TRY(Int, execute(sv_from("scribble-backend"), sv_cstr(path)));
    return socket_accept(listen_fd);
}

void engine_initialize_backend(StringView path)
{
    int              conn_fd = MUST(Int, unix_socket_connect(path));
    EngineConnection conn = { 0 };
    conn.fd = conn_fd;
    conn.context = NULL;
    engine_start_stage(&conn, ES_INIT);
}
