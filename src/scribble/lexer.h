/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ctype.h>

#include <error_or.h>
#include <log.h>
#include <sv.h>

#ifndef __LEXER_H__
#define __LEXER_H__

typedef struct _location {
    StringView file;
    size_t     line;
    size_t     column;
} Location;

#define LOC_SPEC "%.*s:%zu:%zu: "
#define LOC_ARG(loc) (int) loc.file.length, loc.file.ptr, loc.line, loc.column

#define TOKENKINDS(S)   \
    S(TK_UNKNOWN)       \
    S(TK_END_OF_FILE)   \
    S(TK_SYMBOL)        \
    S(TK_KEYWORD)       \
    S(TK_IDENTIFIER)    \
    S(TK_NUMBER)        \
    S(TK_QUOTED_STRING) \
    S(TK_COMMENT)       \
    S(TK_WHITESPACE)    \
    S(TK_PROGRAM)       \
    S(TK_MODULE)

typedef enum {
#undef TOKENKIND_ENUM
#define TOKENKIND_ENUM(kind) kind,
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

#define KEYWORDS(S)                \
    S(BREAK, break, 0)             \
    S(CONST, const, 1)             \
    S(CONTINUE, continue, 2)       \
    S(ELIF, elif, 3)               \
    S(ELSE, else, 4)               \
    S(ERROR, error, 5)             \
    S(FOR, for, 6)                 \
    S(FUNC, func, 7)               \
    S(IF, if, 8)                   \
    S(IMPORT, import, 9)           \
    S(IN, in, 10)                  \
    S(LOOP, loop, 11)              \
    S(MATCH, match, 12)            \
    S(RETURN, return, 13)          \
    S(STRUCT, struct, 14)          \
    S(VAR, var, 15)                \
    S(WHILE, while, 16)            \
    S(TRUE, true, 17)              \
    S(FALSE, false, 18)            \
    S(ASSIGN_BITWISE_AND, &=, 19)  \
    S(ASSIGN_BITWISE_OR, |=, 20)   \
    S(ASSIGN_BITWISE_XOR, ^=, 21)  \
    S(ASSIGN_SHIFT_LEFT, <<=, 22)  \
    S(ASSIGN_SHIFT_RIGHT, >>=, 23) \
    S(BINARY_DECREMENT, -=, 24)    \
    S(BINARY_INCREMENT, +=, 25)    \
    S(BIT_SHIFT_LEFT, <<, 26)      \
    S(BIT_SHIFT_RIGHT, >>, 27)     \
    S(EQUALS, ==, 28)              \
    S(GREATER_EQUALS, >=, 29)      \
    S(LESS_EQUALS, <=, 30)         \
    S(LOGICAL_AND, &&, 31)         \
    S(LOGICAL_OR, ||, 32)          \
    S(NOT_EQUALS, !=, 33)          \
    S(RANGE, .., 34)               \
    S(FUNC_BINDING, ->, 35)        \
    S(MACRO_BINDING, = >, 36)      \
    S(UNARY_DECREMENT, --, 37)     \
    S(UNARY_INCREMENT, ++, 38)

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

ErrorOr(Token, Token)

#define TOKEN_SPEC "%.*s:%zu:%zu: %s %s [%.*s]:%zu"
#define TOKEN_ARG(t) (int) t.loc.file.length, t.loc.file.ptr, t.loc.line, t.loc.column, TokenKind_name(t.kind), TokenCode_name(t.code), (int) t.text.length, t.text.ptr, t.text.length

    typedef struct _source {
    Location        loc;
    StringView      source;
    struct _source *prev;
} Source;

typedef struct {
    bool    skip_whitespace;
    Source *sources;
    Token   current;
} Lexer;

extern char const *TokenKind_name(TokenKind kind);
extern char const *TokenCode_name(int code);

#define token_matches_kind(t, k) ((t).kind == k)
#define token_matches(t, k, c) (token_matches_kind((t), (k)) && (t).code == c)

extern StringView   lexer_source(Lexer *lexer);
extern Location     lexer_current_location(Lexer *lexer);
extern void         lexer_push_source(Lexer *lexer, StringView source, StringView name);
extern void         lexer_pop_source(Lexer *lexer);
extern void         lexer_advance_source(Lexer *lexer, size_t num);
extern Token        lexer_peek(Lexer *lexer);
extern Token        lexer_next(Lexer *lexer);
extern Token        lexer_lex(Lexer *lexer);
extern ErrorOrToken lexer_expect(Lexer *lexer, TokenKind kind, TokenCode code, char const *msg, ...);
extern bool         lexer_next_matches(Lexer *lexer, TokenKind kind, TokenCode code);

#define TOKEN_LOC_ARG(token) LOC_ARG(token.loc)
#define LEXER_LOC_ARG(lexer) LOC_ARG(lexer->sources->loc)

#endif /* __LEXER_H__ */
