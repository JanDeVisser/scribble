/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#ifndef __TOKEN_H__
#define __TOKEN_H__

#include <json.h>
#include <sv.h>

typedef struct _location {
    StringView file;
    size_t     line;
    size_t     column;
} Location;

#define LOC_SPEC "%.*s:%zu:%zu: "
#define LOC_ARG(loc) (int) loc.file.length, loc.file.ptr, loc.line, loc.column

#define TOKENKINDS(S)   \
    S(UNKNOWN)       \
    S(END_OF_FILE)   \
    S(SYMBOL)        \
    S(KEYWORD)       \
    S(IDENTIFIER)    \
    S(NUMBER)        \
    S(QUOTED_STRING) \
    S(COMMENT)       \
    S(WHITESPACE)    \
    S(PROGRAM)       \
    S(MODULE)

typedef enum {
#undef TOKENKIND_ENUM
#define TOKENKIND_ENUM(kind) TK_##kind,
    TOKENKINDS(TOKENKIND_ENUM)
#undef TOKENKIND_ENUM
} TokenKind;

#define TOKENCODES(S)                       \
    S(TC_NONE)                              \
    S(TC_IDENTIFIER)                        \
    S(TC_INTEGER)                           \
    S(TC_DECIMAL)                           \
    S(TC_HEXNUMBER)                         \
    S(TC_BINARYNUMBER)                      \
    S(TC_SINGLE_QUOTED_STRING)              \
    S(TC_DOUBLE_QUOTED_STRING)              \
    S(TC_BACK_QUOTED_STRING)                \
    S(TC_UNTERMINATED_SINGLE_QUOTED_STRING) \
    S(TC_UNTERMINATED_DOUBLE_QUOTED_STRING) \
    S(TC_UNTERMINATED_BACK_QUOTED_STRING)   \
    S(TC_END_OF_LINE_COMMENT)               \
    S(TC_BLOCK_COMMENT)                     \
    S(TC_UNTERMINATED_BLOCK_COMMENT)        \
    S(TC_NEWLINE)                           \
    S(TC_WHITESPACE)

typedef enum {
#undef TOKENCODE_ENUM
#define TOKENCODE_ENUM(code) code,
    TOKENCODES(TOKENCODE_ENUM)
#undef TOKENCODE_ENUM
        TC_COUNT,
} TokenCode;

typedef struct {
    StringView keyword;
    int        code;
} Keyword;

// clang-format off
#define KEYWORDS(S)                \
    S(AS, as, 0)                   \
    S(BREAK, break, 1)             \
    S(CONST, const, 2)             \
    S(CONTINUE, continue, 3)       \
    S(ELIF, elif, 4)               \
    S(ELSE, else, 5)               \
    S(ENUM, enum, 6)               \
    S(ERROR, error, 7)             \
    S(FOR, for, 8)                 \
    S(FUNC, func, 9)               \
    S(IF, if, 10)                  \
    S(IMPORT, import, 11)          \
    S(IN, in, 12)                  \
    S(LOOP, loop, 13)              \
    S(MATCH, match, 14)            \
    S(RETURN, return, 15)          \
    S(STRUCT, struct, 16)          \
    S(VAR, var, 17)                \
    S(VARIANT, variant, 18)        \
    S(WHILE, while, 19)            \
    S(TRUE, true, 20)              \
    S(FALSE, false, 21)            \
    S(ASSIGN_BITWISE_AND, &=, 22)  \
    S(ASSIGN_BITWISE_OR, |=, 23)   \
    S(ASSIGN_BITWISE_XOR, ^=, 24)  \
    S(ASSIGN_SHIFT_LEFT, <<=, 25)  \
    S(ASSIGN_SHIFT_RIGHT, >>=, 26) \
    S(BINARY_DECREMENT, -=, 27)    \
    S(BINARY_INCREMENT, +=, 28)    \
    S(ASSIGN_MULTIPLY, *=, 29)     \
    S(ASSIGN_DIVIDE, /=, 30)       \
    S(ASSIGN_MODULO, %=, 31)       \
    S(BIT_SHIFT_LEFT, <<, 32)      \
    S(BIT_SHIFT_RIGHT, >>, 33)     \
    S(EQUALS, ==, 34)              \
    S(GREATER_EQUALS, >=, 35)      \
    S(LESS_EQUALS, <=, 36)         \
    S(LOGICAL_AND, &&, 37)         \
    S(LOGICAL_OR, ||, 38)          \
    S(NOT_EQUALS, !=, 39)          \
    S(RANGE, .., 40)               \
    S(FUNC_BINDING, ->, 41)        \
    S(MACRO_BINDING, =>, 42)       \
    S(UNARY_DECREMENT, --, 43)     \
    S(UNARY_INCREMENT, ++, 44)
// clang-format on

typedef enum {
#undef KEYWORD_ENUM
#define KEYWORD_ENUM(keyword, text, code) KW_##keyword = TC_COUNT + code,
    KEYWORDS(KEYWORD_ENUM)
#undef KEYWORD_ENUM
        KW_MAX,
} KeywordCode;

#define KW_COUNT (KW_MAX - TC_COUNT)

typedef struct {
    StringView text;
    TokenKind  kind;
    int        code;
    Location   loc;
} Token;

ERROR_OR_ALIAS(Token, Token)
OPTIONAL(Token)

#define TOKEN_SPEC "%.*s:%zu:%zu: %s %s [%.*s]:%zu"
#define TOKEN_ARG(t) (int) t.loc.file.length, t.loc.file.ptr, t.loc.line, t.loc.column, TokenKind_name(t.kind), TokenCode_name(t.code), (int) t.text.length, t.text.ptr, t.text.length

extern char const *TokenKind_name(TokenKind kind);
extern TokenKind   TokenKind_from_string(StringView kind);
extern char const *TokenCode_name(int code);
extern char const *Keyword_text(KeywordCode code);
extern JSONValue   location_to_json(Location location);
extern Location    location_from_json(JSONValue location);
extern JSONValue   token_to_json(Token token);
extern Token       token_from_json(JSONValue token);
extern Token       token_merge(Token t1, Token t2, TokenKind kind, int code);

#define token_matches_kind(t, k) ((t).kind == k)
#define token_matches(t, k, c) (token_matches_kind((t), (k)) && (t).code == c)

#define TOKEN_LOC_ARG(token) LOC_ARG(token.loc)

#endif /* __TOKEN_H__ */
