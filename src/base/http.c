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

ErrorOrInt http_request_send(int fd, HttpRequest *request)
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
    sb_append_cstr(&sb, "\r\n");
    if (!sv_empty(request->body)) {
        sb_append_sv(&sb, request->body);
    }
    if (send(fd, sb.view.ptr, sb.view.length, 0) != sb.view.length) {
        ERROR(Int, HttpError, 0, "Failed to write to socket: %s", strerror(errno));
    }
    RETURN(Int, 0);
}

ErrorOrHttpRequest http_request_receive(int fd)
{
    HttpRequest ret = { 0 };
    ret.request = TRY_TO(StringView, HttpRequest, socket_read(fd));
    ret.body = sv_null();
    StringView req = ret.request;
    StringView start_line = sv_chop_to_delim(&req, sv_from("\r\n"));

    StringList status_fields = sv_split(start_line, sv_from(" "));
    if (status_fields.size != 3) {
        ERROR(HttpRequest, HttpError, 0, "Invalid HTTP request; malformed start line '%.*s' ", SV_ARG(start_line));
    }
    if (!sv_startswith(status_fields.strings[2], sv_from("HTTP/"))) {
        ERROR(HttpRequest, HttpError, 0, "Invalid HTTP request; malformed start line '%.*s' ", SV_ARG(start_line));
    }
    ret.method = http_method_from_string(status_fields.strings[0]);
    if (ret.method == HTTP_METHOD_UNKNOWN) {
        ERROR(HttpRequest, HttpError, 0, "Invalid HTTP request; unknown method '%s' ", http_method_to_string(ret.method));
    }
    ret.url = status_fields.strings[1];
    int qmark_ix;
    if ((qmark_ix = sv_first(ret.url, '?')) > 0) {
        StringView params = (StringView) { ret.url.ptr, ret.url.length - qmark_ix - 1 };
        ret.url.length = qmark_ix;
        ret.params = sv_split(params, sv_from("&"));
    }

    if (sv_empty(req)) {
        RETURN(HttpRequest, ret);
    }
    StringView header_lines = sv_chop_to_delim(&req, sv_from("\r\n\r\n"));
    ret.body = req;

    StringList headers = sv_split(header_lines, sv_from("\r\n"));
    for (size_t ix = 0; ix < headers.size; ++ix) {
        StringList header_fields = sv_split(headers.strings[ix], sv_from(": "));
        if (header_fields.size != 2) {
            continue;
        }
        HttpHeader header = { .name = header_fields.strings[0], .value = header_fields.strings[1] };
        da_append_HttpHeader(&ret.headers, header);
    }
    RETURN(HttpRequest, ret);
}

void http_response_free(HttpResponse *response)
{
    sv_free(response->response);
}

ErrorOrInt http_response_send(int fd, HttpResponse *response)
{
    StringBuilder sb = { 0 };

    sb_printf(&sb, "HTTP/1.1 %d %s\n", (int) response->status, http_status_to_string(response->status));
    for (size_t ix = 0; ix < response->headers.size; ++ix) {
        sb_append_sv(&sb, response->headers.elements[ix].name);
        sb_append_cstr(&sb, ": ");
        sb_append_sv(&sb, response->headers.elements[ix].value);
        sb_append_cstr(&sb, "\r\n");
    }
    sb_append_cstr(&sb, "\r\n");
    if (!sv_empty(response->body)) {
        sb_append_sv(&sb, response->body);
    }
    if (send(fd, sb.view.ptr, sb.view.length, 0) != sb.view.length) {
        ERROR(Int, HttpError, 0, "Failed to write to socket: %s", strerror(errno));
    }
    RETURN(Int, 0);
}

ErrorOrHttpResponse http_response_receive(int fd)
{
    HttpResponse ret = { 0 };
    ret.status = HTTP_STATUS_UNKNOWN;
    ret.response = TRY_TO(StringView, HttpResponse, socket_read(fd));
    ret.body = sv_null();
    StringView resp = ret.response;
    StringView status_line = sv_chop_to_delim(&resp, sv_from("\r\n"));

    StringList status_fields = sv_split(status_line, sv_from(" "));
    if (status_fields.size != 3) {
        ERROR(HttpResponse, HttpError, 0, "Invalid HTTP response: malformed status line '%.*s'", SV_ARG(status_line));
    }
    if (!sv_startswith(status_fields.strings[0], sv_from("HTTP/"))) {
        ERROR(HttpResponse, HttpError, 0, "Invalid HTTP response: Invalid protocol '%.*s'", SV_ARG(status_fields.strings[0]));
    }
    ret.status = http_status_from_code(status_fields.strings[1]);
    if (ret.status == HTTP_STATUS_UNKNOWN) {
        ERROR(HttpResponse, HttpError, 0, "Invalid HTTP response: Unknow status '%s'", http_status_to_string(ret.status));
    }

    if (sv_empty(resp)) {
        RETURN(HttpResponse, ret);
    }

    StringView header_lines = sv_chop_to_delim(&resp, sv_from("\r\n\r\n"));
    ret.body = resp;
    StringList headers = sv_split(header_lines, sv_from("\r\n"));
    for (size_t ix = 0; ix < headers.size; ++ix) {
        StringList header_fields = sv_split(headers.strings[ix], sv_from(": "));
        if (header_fields.size != 2) {
            continue;
        }
        HttpHeader header = { .name = header_fields.strings[0], .value = header_fields.strings[1] };
        da_append_HttpHeader(&ret.headers, header);
    }
    RETURN(HttpResponse, ret);
}

#ifdef HTTP_TEST

#include <arpa/inet.h>

int main(int argc, char **argv)
{
    int client_fd = MUST(Int, tcpip_socket_connect(sv_from("www.google.com"), 80));

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
    close(client_fd);
    return 0;
}

#endif /* HTTP_TEST */
