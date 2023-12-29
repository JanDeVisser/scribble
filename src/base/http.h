/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <error_or.h>
#include <sv.h>

#ifndef __HTTP_H__
#define __HTTP_H__

#define HTTPMETHODS(S) \
    S(UNKNOWN)         \
    S(GET)             \
    S(POST)            \
    S(PUT)             \
    S(DELETE)

typedef enum {
#undef HTTPMETHOD
#define HTTPMETHOD(m) HTTP_METHOD_##m,
    HTTPMETHODS(HTTPMETHOD)
#undef HTTPMETHOD
} HttpMethod;

inline static char const *http_method_to_string(HttpMethod method)
{
    switch (method) {
#undef HTTPMETHOD
#define HTTPMETHOD(m)     \
    case HTTP_METHOD_##m: \
        return #m;
        HTTPMETHODS(HTTPMETHOD)
#undef HTTPMETHOD
    default:
        UNREACHABLE();
    }
}

static inline HttpMethod http_method_from_string(StringView method)
{
#undef HTTPMETHOD
#define HTTPMETHOD(m)           \
    if (sv_eq_cstr(method, #m)) \
        return HTTP_METHOD_##m;
    HTTPMETHODS(HTTPMETHOD)
#undef HTTPMETHOD
    return HTTP_METHOD_UNKNOWN;
}

#define HTTPSTATUSES(S)           \
    S(UNKNOWN, 0)                 \
    S(OK, 200)                    \
    S(CREATED, 201)               \
    S(ACCEPTED, 202)              \
    S(NO_CONTENT, 204)            \
    S(HELLO, 290)                 \
    S(MOVED_PERMANENTLY, 301)     \
    S(FOUND, 302)                 \
    S(NOT_MODIFIED, 304)          \
    S(BAD_REQUEST, 400)           \
    S(UNAUTHORIZED, 401)          \
    S(FORBIDDEN, 403)             \
    S(NOT_FOUND, 404)             \
    S(METHOD_NOT_ALLOWED, 405)    \
    S(INTERNAL_SERVER_ERROR, 500) \
    S(NOT_IMPLEMENTED, 501)       \
    S(BAD_GATEWAY, 502)           \
    S(SERVICE_UNAVAILABLE, 503)

typedef enum {
#undef HTTPSTATUS
#define HTTPSTATUS(s, c) HTTP_STATUS_##s = (c),
    HTTPSTATUSES(HTTPSTATUS)
#undef HTTPSTATUS
} HttpStatus;

static inline char const *http_status_to_string(HttpStatus status)
{
    switch (status) {
#undef HTTPSTATUS
#define HTTPSTATUS(s, c)  \
    case HTTP_STATUS_##s: \
        return #s;
        HTTPSTATUSES(HTTPSTATUS)
#undef HTTPSTATUS
    default:
        UNREACHABLE();
    }
}

static inline HttpStatus http_status_from_code(StringView code)
{
#undef HTTPSTATUS
#define HTTPSTATUS(s, c)      \
    if (sv_eq_cstr(code, #c)) \
        return HTTP_STATUS_##s;
    HTTPSTATUSES(HTTPSTATUS)
#undef HTTPSTATUS
    return HTTP_STATUS_UNKNOWN;
}

typedef struct {
    StringView name;
    StringView value;
} HttpHeader;

DA(HttpHeader)
typedef DA_HttpHeader HttpHeaders;

typedef struct {
    StringView  request;
    HttpMethod  method;
    StringView  url;
    HttpHeaders headers;
    StringList  params;
    StringView  body;
} HttpRequest;

ErrorOr(HttpRequest, HttpRequest);

typedef struct {
    StringView  response;
    HttpStatus  status;
    HttpHeaders headers;
    StringView  body;
} HttpResponse;

ErrorOr(HttpResponse, HttpResponse);

void                http_request_free(HttpRequest *request);
ErrorOrInt          http_request_send(int fd, HttpRequest *request);
ErrorOrHttpRequest  http_request_receive(int fd);
void                http_response_free(HttpResponse *response);
ErrorOrInt          http_response_send(int fd, HttpResponse *response);
ErrorOrHttpResponse http_response_receive(int fd);

#endif /* __HTTP_H__ */
