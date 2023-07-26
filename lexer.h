/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ctype.h>

#include <log.h>
#include <sv.h>

#ifndef __LEXER_H__
#define __LEXER_H__

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
    StringView  keyword;
    int         code;
} Keyword;

#define KEYWORDS(S)                \
    S(BREAK, break, 0)             \
    S(CONST, const, 1)             \
    S(CONTINUE, continue, 2)       \
    S(ELIF, elif, 3)               \
    S(ELSE, else, 4)               \
    S(ERROR, error, 5)             \
    S(FUNC, func, 6)               \
    S(IF, if, 7)                   \
    S(LOOP, loop, 8)               \
    S(MATCH, match, 9)             \
    S(RETURN, return, 10)           \
    S(VAR, var, 11)                \
    S(WHILE, while, 12)            \
    S(ASSIGN_BITWISE_AND, &=, 13)  \
    S(ASSIGN_BITWISE_OR, |=, 14)   \
    S(ASSIGN_BITWISE_XOR, ^=, 15)  \
    S(ASSIGN_SHIFT_LEFT, <<=, 16)  \
    S(ASSIGN_SHIFT_RIGHT, >>=, 17) \
    S(BINARY_DECREMENT, -=, 18)    \
    S(BINARY_INCREMENT, +=, 19)    \
    S(BIT_SHIFT_LEFT, <<, 20)      \
    S(BIT_SHIFT_RIGHT, >>, 21)     \
    S(EQUALS, ==, 22)              \
    S(GREATER_EQUALS, >=, 23)      \
    S(LESS_EQUALS, <=, 24)         \
    S(LOGICAL_AND, &&, 25)         \
    S(LOGICAL_OR, ||, 26)          \
    S(NOT_EQUALS, !=, 27)          \
    S(RANGE, .., 28)               \
    S(RETURN_TYPES, ->, 29)        \
    S(UNARY_DECREMENT, --, 30)     \
    S(UNARY_INCREMENT, ++, 31)

typedef enum {
#undef KEYWORD_ENUM
#define KEYWORD_ENUM(keyword, text, code) KW_##keyword = TC_COUNT + code,
    KEYWORDS(KEYWORD_ENUM)
#undef KEYWORD_ENUM
        KW_MAX,
} KeywordCode;

#define KW_COUNT (KW_MAX - TC_COUNT)

typedef struct {
    size_t     pos;
    StringView text;
    TokenKind  kind;
    int        code;
} Token;

#define TOKEN_SPEC "%s %s [%.*s]:%zu"
#define TOKEN_ARG(t) TokenKind_name(t.kind), \
                     TokenCode_name(t.code), \
                     (int) t.text.length,    \
                     t.text.ptr,             \
                     t.text.length

typedef struct {
    char const *buffer;
    bool        skip_whitespace;
    StringView  tail;
    Token       current;
    size_t      ptr;
} Lexer;

extern char const *TokenKind_name(TokenKind kind);
extern char const *TokenCode_name(int code);
extern Token       token_merge(Token t1, Token t2);
extern Token       token_merge3(Token t1, Token t2, Token t3);

#define token_matches_kind(t, k) ((t).kind == k)
#define token_matches(t, k, c) (token_matches_kind((t), (k)) && (t).code == c)

extern Token lexer_peek(Lexer *lexer);
extern Token lexer_next(Lexer *lexer);
extern Token lexer_lex(Lexer *lexer);

#endif /* __LEXER_H__ */
