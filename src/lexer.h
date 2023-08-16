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
    S(FUNC, func, 6)               \
    S(IF, if, 7)                   \
    S(LOOP, loop, 8)               \
    S(MATCH, match, 9)             \
    S(RETURN, return, 10)          \
    S(STRUCT, struct, 11)          \
    S(VAR, var, 12)                \
    S(WHILE, while, 13)            \
    S(TRUE, true, 14)              \
    S(FALSE, false, 15)            \
    S(ASSIGN_BITWISE_AND, &=, 16)  \
    S(ASSIGN_BITWISE_OR, |=, 17)   \
    S(ASSIGN_BITWISE_XOR, ^=, 18)  \
    S(ASSIGN_SHIFT_LEFT, <<=, 19)  \
    S(ASSIGN_SHIFT_RIGHT, >>=, 20) \
    S(BINARY_DECREMENT, -=, 21)    \
    S(BINARY_INCREMENT, +=, 22)    \
    S(BIT_SHIFT_LEFT, <<, 23)      \
    S(BIT_SHIFT_RIGHT, >>, 24)     \
    S(EQUALS, ==, 25)              \
    S(GREATER_EQUALS, >=, 26)      \
    S(LESS_EQUALS, <=, 27)         \
    S(LOGICAL_AND, &&, 28)         \
    S(LOGICAL_OR, ||, 29)          \
    S(NOT_EQUALS, !=, 30)          \
    S(RANGE, .., 31)               \
    S(FUNC_BINDING, ->, 32)        \
    S(UNARY_DECREMENT, --, 33)     \
    S(UNARY_INCREMENT, ++, 34)

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
} Token;

#define TOKEN_SPEC "%s %s [%.*s]:%zu: "
#define TOKEN_ARG(t) TokenKind_name(t.kind), \
                     TokenCode_name(t.code), \
                     (int) t.text.length,    \
                     t.text.ptr,             \
                     t.text.length

typedef struct source_stack {
    StringView           source;
    struct source_stack *prev;
} SourceStackEntry;

typedef struct {
    bool              skip_whitespace;
    SourceStackEntry *sources;
    Token             current;
} Lexer;

extern char const *TokenKind_name(TokenKind kind);
extern char const *TokenCode_name(int code);

#define token_matches_kind(t, k) ((t).kind == k)
#define token_matches(t, k, c) (token_matches_kind((t), (k)) && (t).code == c)

extern StringView lexer_source(Lexer *lexer);
extern void       lexer_update_source(Lexer *lexer, StringView new_source);
extern void       lexer_push_source(Lexer *lexer, StringView source);
extern void       lexer_pop_source(Lexer *lexer);
extern Token      lexer_peek(Lexer *lexer);
extern Token      lexer_next(Lexer *lexer);
extern Token      lexer_lex(Lexer *lexer);

#endif /* __LEXER_H__ */
