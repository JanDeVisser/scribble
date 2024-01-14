/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <model/error.h>

DA_IMPL(ScribbleError);

char const *ErrorKind_name(ScribbleErrorKind kind)
{
    switch (kind) {
#undef ERRORKIND
#define ERRORKIND(K) \
    case SEK_##K:    \
        return #K;
        ERRORKINDS(ERRORKIND)
#undef ERRORKIND
    default:
        UNREACHABLE();
    }
}

ScribbleErrorKind ErrorKind_from_string(StringView kind)
{
#undef ERRORKIND
#define ERRORKIND(K)                      \
    if (sv_eq_ignore_case_cstr(kind, #K)) \
        return SEK_##K;
    ERRORKINDS(ERRORKIND)
#undef ERRORKIND
    fatal("Unrecognized error kind '%.*s'", SV_ARG(kind));
}

JSONValue scribble_error_to_json(ScribbleError *error)
{
    JSONValue e = json_object();
    json_set(&e, "kind", json_string(sv_from(ErrorKind_name(error->kind))));
    json_set(&e, "message", json_string(error->message));
    json_set(&e, "token", token_to_json(error->token));
    json_set(&e, "notes", scribble_errors_to_json(&error->notes));
    return e;
}

ScribbleError scribble_error_from_json(JSONValue error)
{
    assert(error.type == JSON_TYPE_OBJECT);
    return (ScribbleError) {
        .kind = ErrorKind_from_string(json_get_string(&error, "kind", sv_from("SYNTAX"))),
        .message = json_get_string(&error, "message", sv_null()),
        .token = token_from_json(MUST_OPTIONAL(JSONValue, json_get(&error, "token"))),
        .notes = scribble_errors_from_json(MUST_OPTIONAL(JSONValue, json_get(&error, "notes"))),
    };
}

JSONValue scribble_errors_to_json(ScribbleErrors *errors)
{
    JSONValue ret = json_array();
    for (size_t ix = 0; ix < errors->size; ++ix) {
        json_append(&ret, scribble_error_to_json(da_element_ScribbleError(errors, ix)));
    }
    return ret;
}

ScribbleErrors scribble_errors_from_json(JSONValue errors)
{
    assert(errors.type == JSON_TYPE_ARRAY);
    ScribbleErrors ret = { 0 };
    for (size_t ix = 0; ix < json_len(&errors); ++ix) {
        da_append_ScribbleError(&ret, scribble_error_from_json(MUST_OPTIONAL(JSONValue, json_at(&errors, ix))));
    }
    return ret;
}
