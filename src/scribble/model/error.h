/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#ifndef __MODEL_ERROR_H__
#define __MODEL_ERROR_H__

#include <json.h>
#include <model/token.h>

#define ERRORKINDS(S) \
    S(SYNTAX)         \
    S(SEMANTIC)

typedef enum scribble_error_kind {
#undef ERRORKIND
#define ERRORKIND(K) SEK_##K,
    ERRORKINDS(ERRORKIND)
#undef ERRORKIND
} ScribbleErrorKind;

DA_VOID_WITH_NAME(ScribbleError, ScribbleErrors);

typedef struct {
    ScribbleErrorKind kind;
    Token             token;
    StringView        message;
    ScribbleErrors    notes;
} ScribbleError;

DA_FUNCTIONS(ScribbleError);

extern char const       *ErrorKind_name(ScribbleErrorKind kind);
extern ScribbleErrorKind ErrorKind_from_string(StringView kind);
extern JSONValue         scribble_errors_to_json(ScribbleErrors *errors);
extern ScribbleErrors    scribble_errors_from_json(JSONValue errors);

#endif /* __MODEL_ERROR_H__ */
