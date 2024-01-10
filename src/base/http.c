/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <errno.h>
#include <poll.h>
#include <sys/socket.h>
#include <unistd.h>

#define STATIC_ALLOCATOR
#include <allocate.h>
#include <http.h>
#include <io.h>

FREE_LIST_IMPL(HttpRequest, http_request)
FREE_LIST_IMPL(HttpResponse, http_response)

DA_IMPL(HttpHeader)

void http_request_free(HttpRequest *request)
{
    sv_free(request->request);
}

ErrorOrInt http_request_send(socket_t socket, HttpRequest *request)
{
    StringBuilder sb = { 0 };

    sb_printf(&sb, "%s ", http_method_to_string(request->method));
    sb_append_sv(&sb, request->url);
    if (request->params.size > 0) {
        sb_append_char(&sb, '?');
        StringView param_sv = sl_join(&request->params, sv_from("&"));
        sb_append_sv(&sb, param_sv);
        sv_free(param_sv);
    }
    sb_append_cstr(&sb, " HTTP/1.1\r\n");
    for (size_t ix = 0; ix < request->headers.size; ++ix) {
        sb_append_sv(&sb, request->headers.elements[ix].name);
        sb_append_cstr(&sb, ": ");
        sb_append_sv(&sb, request->headers.elements[ix].value);
        sb_append_cstr(&sb, "\r\n");
    }
    if (!sv_empty(request->body)) {
        sb_printf(&sb, "Content-Length: %zu\r\n", request->body.length);
    }
    sb_append_cstr(&sb, "\r\n");
    if (!sv_empty(request->body)) {
        sb_append_sv(&sb, request->body);
    }
    TRY_TO(Size, Int, socket_write(socket, sb.view.ptr, sb.view.length));
    RETURN(Int, 0);
}

ErrorOrHttpRequest http_request_receive(socket_t socket)
{
    HttpRequest   ret = { 0 };
    StringBuilder sb = { 0 };
    ret.body = sv_null();
    StringView line = TRY_TO(StringView, HttpRequest, socket_readln(socket));
    trace(CAT_IPC, "http_request_receive start");
    sb_append_sv(&sb, line);
    sv_free(line);
    line = sb.view;

    StringList status_fields = sv_split(line, sv_from(" "));
    if (status_fields.size != 3) {
        ERROR(HttpRequest, HttpError, 0, "Invalid HTTP request; malformed start line '%.*s' ", SV_ARG(line));
    }
    if (!sv_startswith(status_fields.strings[2], sv_from("HTTP/"))) {
        ERROR(HttpRequest, HttpError, 0, "Invalid HTTP request; malformed start line '%.*s' ", SV_ARG(line));
    }
    ret.method = http_method_from_string(status_fields.strings[0]);
    if (ret.method == HTTP_METHOD_UNKNOWN) {
        ERROR(HttpRequest, HttpError, 0, "Invalid HTTP request; unknown method '%s' ", http_method_to_string(ret.method));
    }
    trace(CAT_IPC, "Method: %s", http_method_to_string(ret.method));

    ret.url = sv_copy(status_fields.strings[1]);
    int qmark_ix;
    if ((qmark_ix = sv_first(ret.url, '?')) > 0) {
        StringView params = (StringView) { ret.url.ptr, ret.url.length - qmark_ix - 1 };
        ret.url.length = qmark_ix;
        ret.params = sv_split(params, sv_from("&"));
    }

    size_t content_length = 0;
    while (true) {
        line = TRY_TO(StringView, HttpRequest, socket_readln(socket));
        sb_append_sv(&sb, line);

        if (sv_empty(line)) {
            break;
        }

        StringList header_fields = sv_split(line, sv_from(": "));
        if (header_fields.size != 2) {
            ERROR(HttpRequest, HttpError, 0, "Malformed header line '%.*s'", SV_ARG(line));
        }
        HttpHeader header = { .name = header_fields.strings[0], .value = header_fields.strings[1] };
        da_append_HttpHeader(&ret.headers, header);
        trace(CAT_IPC, "Request Header: %.*s: %.*s", SV_ARG(header.name), SV_ARG(header.name));
        if (sv_eq_ignore_case_cstr(header.name, "Content-Length")) {
            IntegerParseResult content_length_maybe = sv_parse_u64(header.value);
            if (content_length_maybe.success) {
                content_length = content_length_maybe.integer.u64;
                trace(CAT_IPC, "Content length: %zu", content_length);
            }
        }
    }

    if (content_length) {
        size_t     len = sb.view.length;
        StringView body = TRY_TO(StringView, HttpRequest, socket_read(socket, content_length));
        sb_append_sv(&sb, body);
        ret.body = (StringView) { .ptr = sb.view.ptr + len, .length = body.length };
        trace(CAT_IPC, "Read Request Body:\n%.*s\n", SV_ARG(body));
        sv_free(body);
    }
    ret.request = sb.view;
    trace(CAT_IPC, "http_request_receive done");
    RETURN(HttpRequest, ret);
}

void http_response_free(HttpResponse *response)
{
    sv_free(response->response);
}

ErrorOrInt http_response_send(socket_t socket, HttpResponse *response)
{
    StringBuilder sb = { 0 };

    sb_printf(&sb, "HTTP/1.1 %d %s\r\n", (int) response->status, http_status_to_string(response->status));
    for (size_t ix = 0; ix < response->headers.size; ++ix) {
        sb_append_sv(&sb, response->headers.elements[ix].name);
        sb_append_cstr(&sb, ": ");
        sb_append_sv(&sb, response->headers.elements[ix].value);
        sb_append_cstr(&sb, "\r\n");
    }
    if (!sv_empty(response->body)) {
        sb_printf(&sb, "Content-Length: %zu\r\n", response->body.length);
    }
    sb_append_cstr(&sb, "\r\n");
    if (!sv_empty(response->body)) {
        sb_append_sv(&sb, response->body);
    }
    printf("Sending response\n%.*s", SV_ARG(sb.view));
    TRY_TO(Size, Int, socket_write(socket, sb.view.ptr, sb.view.length));
    RETURN(Int, 0);
}

ErrorOrHttpResponse http_response_receive(socket_t socket)
{
    HttpResponse  ret = { 0 };
    StringBuilder sb = { 0 };
    ret.status = HTTP_STATUS_UNKNOWN;
    ret.body = sv_null();
    StringView line = TRY_TO(StringView, HttpResponse, socket_readln(socket));
    trace(CAT_IPC, "http_response_receive start");
    sb_append_sv(&sb, line);
    sv_free(line);
    line = sb.view;

    StringList status_fields = sv_split(line, sv_from(" "));
    if (status_fields.size != 3) {
        ERROR(HttpResponse, HttpError, 0, "Invalid HTTP response: malformed status line '%.*s'", SV_ARG(line));
    }
    if (!sv_startswith(status_fields.strings[0], sv_from("HTTP/"))) {
        ERROR(HttpResponse, HttpError, 0, "Invalid HTTP response: Invalid protocol '%.*s'", SV_ARG(status_fields.strings[0]));
    }
    ret.status = http_status_from_code(status_fields.strings[1]);
    if (ret.status == HTTP_STATUS_UNKNOWN) {
        ERROR(HttpResponse, HttpError, 0, "Invalid HTTP response: Unknow status '%s'", http_status_to_string(ret.status));
    }
    trace(CAT_IPC, "Status: %s", http_status_to_string(ret.status));

    size_t content_length = 0;
    while (true) {
        line = TRY_TO(StringView, HttpResponse, socket_readln(socket));
        sb_append_sv(&sb, line);

        if (sv_empty(line)) {
            trace(CAT_IPC, "End of headers");
            break;
        }

        StringList header_fields = sv_split(line, sv_from(": "));
        if (header_fields.size != 2) {
            ERROR(HttpResponse, HttpError, 0, "Malformed header line '%.*s'", SV_ARG(line));
        }
        HttpHeader header = { .name = header_fields.strings[0], .value = header_fields.strings[1] };
        da_append_HttpHeader(&ret.headers, header);
        trace(CAT_IPC, "Response Header: %.*s: %.*s", SV_ARG(header.name), SV_ARG(header.value));
        if (sv_eq_ignore_case_cstr(header.name, "Content-Length")) {
            IntegerParseResult content_length_maybe = sv_parse_u64(header.value);
            if (content_length_maybe.success) {
                content_length = content_length_maybe.integer.u64;
                trace(CAT_IPC, "Content length: %zu", content_length);
            }
        }
    }

    if (content_length) {
        size_t     len = sb.view.length;
        StringView body = TRY_TO(StringView, HttpResponse, socket_read(socket, content_length));
        trace(CAT_IPC, "Read Response Body:\n%.*s\n", SV_ARG(body));
        sb_append_sv(&sb, body);
        sv_free(body);
        ret.body = (StringView) { .ptr = sb.view.ptr + len, .length = content_length };
    }
    ret.response = sb.view;
    trace(CAT_IPC, "http_response_receive done");
    RETURN(HttpResponse, ret);
}

HttpResponse http_get_request(socket_t socket, StringView url, StringList params)
{
    HttpRequest request = { 0 };
    request.method = HTTP_METHOD_GET;
    request.url = url;
    request.params = params;
    printf("http_get_request(%.*s)\n", SV_ARG(url));
    http_request_send(socket, &request);
    sv_free(request.request);
    printf("http_get_request(%.*s) - waiting for response\n", SV_ARG(url));
    return MUST(HttpResponse, http_response_receive(socket));
}

HttpResponse http_post_request(socket_t socket, StringView url, JSONValue body)
{
    HttpRequest request = { 0 };
    request.method = HTTP_METHOD_POST;
    request.url = url;
    if (body.type != JSON_TYPE_NULL) {
        request.body = json_encode(body);
    }
    http_request_send(socket, &request);
    sv_free(request.request);
    HttpResponse response = MUST(HttpResponse, http_response_receive(socket));
    return MUST(HttpResponse, http_response_receive(socket));
}

HttpStatus http_get_message(socket_t socket, StringView url, StringList params)
{
    HttpResponse response = http_get_request(socket, url, params);
    return response.status;
}

HttpStatus http_post_message(socket_t socket, StringView url, JSONValue body)
{
    HttpResponse response = http_post_request(socket, url, body);
    return response.status;
}

#ifdef HTTP_TEST

#include <arpa/inet.h>

int main(int argc, char **argv)
{
    socket_t client_fd = MUST(Socket, tcpip_socket_connect(sv_from("www.google.com"), 80));

    HttpRequest req = { 0 };
    req.method = HTTP_METHOD_GET;
    req.url = sv_from("/");
    http_request_send(client_fd, &req);

    HttpResponse const resp = MUST(HttpResponse, http_response_receive(client_fd));
    printf("Status: %s\n", http_status_to_string(resp.status));
    for (size_t ix = 0; ix < resp.headers.size; ++ix) {
        printf("%.*s (%zu): %.*s (%zu)\n",
            SV_ARG(resp.headers.elements[ix].name),
            resp.headers.elements[ix].name.length,
            SV_ARG(resp.headers.elements[ix].value),
            resp.headers.elements[ix].value.length);
    }
    printf("\n");
    socket_close(client_fd);
    return 0;
}

#endif /* HTTP_TEST */
