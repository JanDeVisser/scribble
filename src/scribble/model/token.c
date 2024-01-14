/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <model/token.h>

char const *TokenKind_name(TokenKind kind)
{
    switch (kind) {
#undef TOKENKIND_ENUM
#define TOKENKIND_ENUM(kind) \
    case TK_##kind:          \
        return #kind;
        TOKENKINDS(TOKENKIND_ENUM)
#undef TOKENKIND_ENUM
    default:
        UNREACHABLE();
    }
}

TokenKind TokenKind_from_string(StringView kind)
{
#undef TOKENKIND
#define TOKENKIND(K)                      \
    if (sv_eq_ignore_case_cstr(kind, #K)) \
        return TK_##K;
    TOKENKINDS(TOKENKIND)
#undef TOKENKIND
    fatal("Unrecognized token kind '%.*s'", SV_ARG(kind));
}

char const *TokenCode_name(int code)
{
    static char buffer[2];
    if (code < TC_COUNT) {
        switch ((TokenCode) code) {
#undef TOKENCODE_ENUM
#define TOKENCODE_ENUM(code) \
    case code:               \
        return #code;
            TOKENCODES(TOKENCODE_ENUM)
#undef TOKENCODE_ENUM
        default:
            return "Unknown";
        }
    }
    switch (code) {
#undef KEYWORD_ENUM
#define KEYWORD_ENUM(keyword, text, code) \
    case TC_COUNT + code:                 \
        return "KW_" #keyword;
        KEYWORDS(KEYWORD_ENUM)
#undef KEYWORD_ENUM
    default: {
        buffer[0] = (char) code;
        buffer[1] = 0;
        return buffer;
    }
    }
}

char const *Keyword_text(KeywordCode code)
{
    switch (code) {
#undef KEYWORD_ENUM
#define KEYWORD_ENUM(keyword, text, code) \
    case KW_##keyword:                    \
        return #text;
        KEYWORDS(KEYWORD_ENUM)
#undef KEYWORD_ENUM
    default:
        UNREACHABLE();
    }
}

JSONValue location_to_json(Location location)
{
    JSONValue ret = json_object();
    json_set_string(&ret, "file", location.file);
    json_set_int(&ret, "line", location.line);
    json_set_int(&ret, "column", location.column);
    return ret;
}

Location location_from_json(JSONValue location)
{
    assert(location.type == JSON_TYPE_OBJECT);
    return (Location) {
        .file = json_get_string(&location, "file", sv_null()),
        .line = json_get_int(&location, "line", 0),
        .column = json_get_int(&location, "column", 0),
    };
}

JSONValue token_to_json(Token token)
{
    JSONValue ret = json_object();
    json_set_string(&ret, "text", token.text);
    json_set_cstr(&ret, "kind", TokenKind_name(token.kind));
    json_set_int(&ret, "code", token.code);
    json_set(&ret, "location", location_to_json(token.loc));
    return ret;
}

Token token_from_json(JSONValue token)
{
    assert(token.type == JSON_TYPE_OBJECT);
    return (Token) {
        .text = json_get_string(&token, "text", sv_null()),
        .kind = TokenKind_from_string(json_get_string(&token, "kind", sv_from("UNKNOWN"))),
        .code = json_get_int(&token, "code", TC_NONE),
        .loc = location_from_json(MUST_OPTIONAL(JSONValue, json_get(&token, "location"))),
    };
}

Token token_merge(Token t1, Token t2, TokenKind kind, int code)
{
    Token ret = { 0 };
    if (t1.text.ptr < t2.text.ptr) {
        ret = t1;
        ret.text.length = (t2.text.ptr - t1.text.ptr) + t2.text.length;
    } else {
        ret = t2;
        ret.text.length = (t1.text.ptr - t2.text.ptr) + t1.text.length;
    }
    ret.kind = kind;
    ret.code = code;
    return ret;
}
