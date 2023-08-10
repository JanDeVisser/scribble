/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include "lexer.h"

char const *TokenKind_name(TokenKind kind)
{
    switch (kind) {
#undef TOKENKIND_ENUM
#define TOKENKIND_ENUM(kind) \
    case kind:               \
        return #kind;
        TOKENKINDS(TOKENKIND_ENUM)
#undef TOKENKIND_ENUM
    default:
        UNREACHABLE();
    }
}

char const *TokenCode_name(int code)
{
    static char buffer[80];
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
#define KEYWORD_ENUM(keyword, text, code)        \
    case TC_COUNT + code:                        \
        snprintf(buffer, 80, "KW_%s", #keyword); \
        return buffer;
        KEYWORDS(KEYWORD_ENUM)
#undef KEYWORD_ENUM
    default: {
        snprintf(buffer, 80, "%c", code);
        return buffer;
    }
    }
}

Token token_merge(Token t1, Token t2)
{
    size_t len = (t2.pos + sv_length(t2.text)) - t1.pos;
    Token  ret = { t1.pos, { t1.text.ptr, len }, { t1.text.ptr, len }, t1.kind, t1.code };
    return ret;
}

Token token_merge3(Token t1, Token t2, Token t3)
{
    return token_merge(token_merge(t1, t2), t3);
}

Token scan_number(char const *buffer, size_t pos)
{
    int       ix = 0;
    TokenCode code = TC_INTEGER;
    while (true) {
        char ch = buffer[ix];
        if (!isdigit(ch) && ((ch != '.') || (code == TC_DECIMAL))) {
            // FIXME lex '1..10' as '1', '..', '10'. It will now lex as '1.', '.', '10'
            Token ret = { pos, { buffer, ix }, { buffer, ix }, TK_NUMBER, code };
            return ret;
        }
        if (ch == '.') {
            code = TC_DECIMAL;
        }
        ++ix;
    }
}

Token lexer_peek(Lexer *lexer)
{
    static Keyword s_keywords[KW_COUNT] = { 0 };
    if (s_keywords[0].keyword.length == 0) {
#undef KEYWORD_ENUM
#define KEYWORD_ENUM(kw, text, c)           \
    s_keywords[c].keyword = sv_from(#text); \
    s_keywords[c].code = KW_##kw;
        KEYWORDS(KEYWORD_ENUM)
#undef KEYWORD_ENUM
    }
#undef KEYWORD_ENUM

    if (lexer->current.kind != TK_UNKNOWN) {
        return lexer->current;
    }
    char const *buffer = lexer->tail.ptr;
    char        first = *buffer;
    switch (first) {
    case '\0': {
        Token ret = { lexer->ptr, { buffer, 0 }, { buffer, 0 }, TK_END_OF_FILE, TC_NONE };
        lexer->current = ret;
        return lexer->current;
    }
    case '\n': {
        Token ret = { lexer->ptr, { buffer, 1 }, { buffer, 1 }, TK_WHITESPACE, TC_NEWLINE };
        lexer->current = ret;
        return lexer->current;
    }
    case '\'':
    case '"':
    case '`': {
        size_t ix = 1;
        for (; buffer[ix] && buffer[ix] != first; ++ix)
            ;
        TokenCode code;
        switch (first) {
        case '"':
            code = TC_DOUBLE_QUOTED_STRING;
            break;
        case '\'':
            code = TC_SINGLE_QUOTED_STRING;
            break;
        case '`':
            code = TC_BACK_QUOTED_STRING;
            break;
        default:
            UNREACHABLE();
        }
        if (!buffer[ix]) {
            code += (TC_UNTERMINATED_DOUBLE_QUOTED_STRING - TC_DOUBLE_QUOTED_STRING);
        }
        Token ret = { lexer->ptr, { buffer, ix+1 }, { buffer + 1, ix - 1}, TK_QUOTED_STRING, code };
        lexer->current = ret;
        return lexer->current;
    }
    case '/':
        switch (buffer[1]) {
        case '/': {
            size_t ix = 2;
            for (; buffer[ix] && buffer[ix] != '\n'; ++ix)
                ;
            Token ret = { lexer->ptr, { buffer, ix }, { buffer, ix }, TK_COMMENT, TC_END_OF_LINE_COMMENT };
            lexer->current = ret;
            return lexer->current;
        }
        case '*': {
            if (!buffer[2]) {
                Token ret = { lexer->ptr, { buffer, 2 }, { buffer, 2 }, TK_COMMENT, TC_UNTERMINATED_BLOCK_COMMENT };
                lexer->current = ret;
                return lexer->current;
            }
            size_t ix = 3;
            for (; buffer[ix] && buffer[ix - 1] != '*' && buffer[ix] != '/'; ++ix)
                ;
            Token ret = { lexer->ptr, { buffer, ix }, { buffer, ix }, TK_COMMENT,
                (buffer[ix]) ? TC_UNTERMINATED_BLOCK_COMMENT : TC_BLOCK_COMMENT };
            lexer->current = ret;
            return lexer->current;
        }
        default:
            break;
        }
    default:
        break;
    }
    if (isspace(first)) {
        size_t ix = 0;
        for (; isspace(buffer[ix]) && buffer[ix] != '\n'; ++ix)
            ;
        Token ret = { lexer->ptr, { buffer, ix }, { buffer, ix }, TK_WHITESPACE, TC_WHITESPACE };
        lexer->current = ret;
        return lexer->current;
    }
    if (isdigit(first)) {
        Token ret = scan_number(buffer, lexer->ptr);
        lexer->current = ret;
        return lexer->current;
    }
    if (isalpha(first) || first == '_') {
        size_t ix = 0;
        for (; isalnum(buffer[ix]) || buffer[ix] == '_'; ++ix)
            ;
        for (int kw = 0; kw < KW_COUNT; ++kw) {
            if ((sv_length(s_keywords[kw].keyword) == ix) && sv_startswith(lexer->tail, s_keywords[kw].keyword)) {
                Token ret = { lexer->ptr, { buffer, ix }, { buffer, ix }, TK_KEYWORD, s_keywords[kw].code };
                lexer->current = ret;
                return lexer->current;
            }
        }
        Token ret = { lexer->ptr, { buffer, ix }, { buffer, ix }, TK_IDENTIFIER, TC_IDENTIFIER };
        lexer->current = ret;
        return lexer->current;
    }
    {
        int matched = -1;
        for (int kw = 0; kw < KW_COUNT; ++kw) {
            if (sv_startswith(lexer->tail, s_keywords[kw].keyword)) {
                if ((matched < 0) || (sv_length(s_keywords[kw].keyword) > sv_length(s_keywords[matched].keyword))) {
                    matched = kw;
                }
            }
        }
        if (matched >= 0) {
            Token ret = { lexer->ptr, { buffer, sv_length(s_keywords[matched].keyword) },
                { buffer, sv_length(s_keywords[matched].keyword) }, TK_KEYWORD, TC_COUNT + matched };
            lexer->current = ret;
            return lexer->current;
        }
    }
    Token ret = { lexer->ptr, { buffer, 1 }, { buffer, 1 },TK_SYMBOL, (int) first };
    lexer->current = ret;
    return lexer->current;
}

Token lexer_next(Lexer *lexer)
{
    Token token;
    for (token = lexer_peek(lexer); token.kind != TK_END_OF_FILE; token = lexer_peek(lexer)) {
        if (!lexer->skip_whitespace || token.kind != TK_WHITESPACE) {
            return token;
        }
        lexer_lex(lexer);
    }
    return token;
}

Token lexer_lex(Lexer *lexer)
{
    if (lexer->current.kind == TK_UNKNOWN) {
        lexer_next(lexer);
    }
    lexer->tail = sv_chop(lexer->tail, lexer->current.text.length);
    lexer->ptr += lexer->current.text.length;
    Token ret = lexer->current;
    lexer->current.text.ptr = lexer->tail.ptr;
    lexer->current.text.length = 0;
    lexer->current.kind = TK_UNKNOWN;
    lexer->current.code = TC_NONE;
    return ret;
}
