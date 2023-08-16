/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <error.h>
#define STATIC_ALLOCATOR
#include <allocate.h>
#include <io.h>
#include <lexer.h>

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

Token scan_number(char const *buffer)
{
    int       ix = 0;
    TokenCode code = TC_INTEGER;
    while (true) {
        char ch = buffer[ix];
        if (!isdigit(ch) && ((ch != '.') || (code == TC_DECIMAL))) {
            // FIXME lex '1..10' as '1', '..', '10'. It will now lex as '1.', '.', '10'
            Token ret = { { buffer, ix }, TK_NUMBER, code };
            return ret;
        }
        if (ch == '.') {
            code = TC_DECIMAL;
        }
        ++ix;
    }
}

StringView lexer_source(Lexer *lexer)
{
    if (!lexer->sources) {
        return sv_null();
    }
    return lexer->sources->source;
}

void lexer_update_source(Lexer *lexer, StringView new_source)
{
    if (lexer->sources) {
        lexer->sources->source = new_source;
    }
}

void lexer_push_source(Lexer *lexer, StringView source)
{
    SourceStackEntry *entry = allocate_new(SourceStackEntry);
    entry->source = source;
    entry->prev = lexer->sources;
    lexer->sources = entry;
    lexer->current.text = sv_null();
    lexer->current.kind = TK_UNKNOWN;
    lexer->current.code = TC_NONE;
}

void lexer_pop_source(Lexer *lexer)
{
    if (lexer->sources) {
        lexer->sources = lexer->sources->prev;
    }
    lexer->current.text = sv_null();
    lexer->current.kind = TK_UNKNOWN;
    lexer->current.code = TC_NONE;
}

typedef void (*DirectiveHandler)(StringView directive, StringView source, Lexer *lexer);

typedef struct directive_map {
    char const      *directive;
    DirectiveHandler handler;
} DirectiveMap;

static void directive_handle_include(StringView directive, StringView source, Lexer *lexer);

static DirectiveMap s_directives[] = {
    { "include", directive_handle_include },
    { NULL, NULL },
};

void directive_handle_include(StringView directive, StringView source, Lexer *lexer)
{
    size_t ix = 0;
    while (!isspace(source.ptr[ix])) ++ix;
    StringView file = { source.ptr, ix };
    char *name = allocate(sv_length(file) + 1);
    memcpy(name, file.ptr, file.length);
    name[file.length] = 0;
    MUST(Char, char *, buffer, read_file_by_name(name));
    lexer_update_source(lexer, (StringView) { source.ptr + ix, sv_length(source) - ix });
    lexer_push_source(lexer, (StringView) { buffer, strlen(buffer) });
}

void directive_handle(StringView directive, StringView source, Lexer *lexer)
{
    for (int ix = 0; s_directives[ix].directive; ++ix) {
        if (sv_eq_cstr(directive, s_directives[ix].directive)) {
            s_directives[ix].handler(directive, source, lexer);
            return;
        }
    }
    fatal("Unrecognized preprocessor directive '" SV_SPEC "'", SV_ARG(directive));
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
    StringView  source = lexer_source(lexer);
    char const *buffer = source.ptr;
    char        first = *buffer;
    switch (first) {
    case '\0': {
        Token ret = { { buffer, 0 }, TK_END_OF_FILE, TC_NONE };
        lexer->current = ret;
        return lexer->current;
    }
    case '\n': {
        Token ret = { { buffer, 1 }, TK_WHITESPACE, TC_NEWLINE };
        lexer->current = ret;
        return lexer->current;
    }
    case '\'':
    case '"':
    case '`': {
        size_t ix = 1;
        for (; buffer[ix] && (buffer[ix] != first || buffer[ix - 1] == '\\'); ++ix)
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
        Token ret = { { buffer, ix + 1 }, TK_QUOTED_STRING, code };
        lexer->current = ret;
        return lexer->current;
    }
    case '/':
        switch (buffer[1]) {
        case '/': {
            size_t ix = 2;
            for (; buffer[ix] && buffer[ix] != '\n'; ++ix)
                ;
            Token ret = { { buffer, ix }, TK_COMMENT, TC_END_OF_LINE_COMMENT };
            lexer->current = ret;
            return lexer->current;
        }
        case '*': {
            if (!buffer[2]) {
                Token ret = { { buffer, 2 }, TK_COMMENT, TC_UNTERMINATED_BLOCK_COMMENT };
                lexer->current = ret;
                return lexer->current;
            }
            size_t ix = 3;
            for (; buffer[ix] && buffer[ix - 1] != '*' && buffer[ix] != '/'; ++ix)
                ;
            Token ret = { { buffer, ix }, TK_COMMENT,
                (buffer[ix]) ? TC_UNTERMINATED_BLOCK_COMMENT : TC_BLOCK_COMMENT };
            lexer->current = ret;
            return lexer->current;
        }
        default:
            break;
        }
    case '#': {
        size_t directive_start = 1;
        while (buffer[directive_start] && isspace(buffer[directive_start]))
            ++directive_start;
        size_t directive_end = directive_start;
        while (buffer[directive_end] && isalpha(buffer[directive_end]))
            ++directive_end;
        if (directive_end > directive_start) {
            StringView directive = { buffer + directive_start, directive_end - directive_start };
            size_t text_start = directive_end;
            while (buffer[text_start] && isspace(buffer[text_start]))
                ++text_start;
            directive_handle(directive, (StringView) { buffer + text_start }, lexer);
            return lexer_peek(lexer);

//                size_t     ix = directive_end;
//                StringView text = { buffer + directive_end, ix - directive_end };
//            while ((ix < (sv_length(source) - 4)) && !sv_endswith(text, sv_from("#end"))) {
//                ++ix;
//                text = (StringView) { buffer + directive_end, ix - directive_end };
//            }
//            if (sv_endswith(text, sv_from("#end"))) {
//                directive_handle(directive, (StringView) { text.ptr, text.length - 4 });
//            }
        }
    }
    default:
        break;
    }
    if (isspace(first)) {
        size_t ix = 0;
        for (; isspace(buffer[ix]) && buffer[ix] != '\n'; ++ix)
            ;
        Token ret = { { buffer, ix }, TK_WHITESPACE, TC_WHITESPACE };
        lexer->current = ret;
        return lexer->current;
    }
    if (isdigit(first)) {
        Token ret = scan_number(buffer);
        lexer->current = ret;
        return lexer->current;
    }
    if (isalpha(first) || first == '_') {
        size_t ix = 0;
        for (; isalnum(buffer[ix]) || buffer[ix] == '_'; ++ix)
            ;
        for (int kw = 0; kw < KW_COUNT; ++kw) {
            if ((sv_length(s_keywords[kw].keyword) == ix) && sv_startswith(source, s_keywords[kw].keyword)) {
                Token ret = { { buffer, ix }, TK_KEYWORD, s_keywords[kw].code };
                lexer->current = ret;
                return lexer->current;
            }
        }
        Token ret = { { buffer, ix }, TK_IDENTIFIER, TC_IDENTIFIER };
        lexer->current = ret;
        return lexer->current;
    }
    {
        int matched = -1;
        for (int kw = 0; kw < KW_COUNT; ++kw) {
            if (sv_startswith(source, s_keywords[kw].keyword)) {
                if ((matched < 0) || (sv_length(s_keywords[kw].keyword) > sv_length(s_keywords[matched].keyword))) {
                    matched = kw;
                }
            }
        }
        if (matched >= 0) {
            Token ret = { { buffer, sv_length(s_keywords[matched].keyword) }, TK_KEYWORD, TC_COUNT + matched };
            lexer->current = ret;
            return lexer->current;
        }
    }
    Token ret = { { buffer, 1 }, TK_SYMBOL, (int) first };
    lexer->current = ret;
    return lexer->current;
}

Token lexer_next(Lexer *lexer)
{
    Token token;
    while (lexer->sources) {
        for (token = lexer_peek(lexer); token.kind != TK_END_OF_FILE; token = lexer_peek(lexer)) {
            if (!lexer->skip_whitespace || token.kind != TK_WHITESPACE) {
                return token;
            }
            lexer_lex(lexer);
        }
        lexer_pop_source(lexer);
    }
    return token;
}

Token lexer_lex(Lexer *lexer)
{
    if (lexer->current.kind == TK_UNKNOWN) {
        lexer_next(lexer);
    }
    StringView source = lexer_source(lexer);
    lexer_update_source(lexer, sv_chop(source, lexer->current.text.length));
    Token ret = lexer->current;
    lexer->current.text.ptr = source.ptr;
    lexer->current.text.length = 0;
    lexer->current.kind = TK_UNKNOWN;
    lexer->current.code = TC_NONE;
    return ret;
}
