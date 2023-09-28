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

Location lexer_current_location(Lexer *lexer)
{
    Location ret = { 0 };
    if (lexer->sources) {
        ret = lexer->sources->loc;
    }
    return ret;
}

void lexer_advance_source(Lexer *lexer, size_t num)
{
    if (lexer->sources) {
        Source *src = lexer->sources;
        if (src->source.length < num) {
            num = src->source.length;
        }
        for (size_t ix = 0; ix < num; ++ix) {
            if (src->source.ptr[ix] == '\n') {
                ++src->loc.line;
                src->loc.column = 1;
            } else {
                ++src->loc.column;
            }
        }
        src->source.ptr += num;
        src->source.length -= num;
        if (!src->source.length) {
            src->source.ptr = NULL;
        }
    }
}

void lexer_push_source(Lexer *lexer, StringView source, StringView name)
{
    Source *entry = allocate_new(Source);
    entry->source = source;
    entry->loc.file = name;
    entry->loc.line = 1;
    entry->loc.column = 1;
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

typedef void (*DirectiveHandler)(StringView directive, Lexer *lexer);

typedef struct directive_map {
    char const      *directive;
    DirectiveHandler handler;
} DirectiveMap;

static void directive_handle_include(StringView directive, Lexer *lexer);

static DirectiveMap s_directives[] = {
    { "include", directive_handle_include },
    { NULL, NULL },
};

void directive_handle_include(StringView directive, Lexer *lexer)
{
    StringView source = lexer_source(lexer);
    size_t     ix = 0;
    while (!isspace(source.ptr[ix]))
        ++ix;
    StringView file = { source.ptr, ix };
    char      *name = allocate(sv_length(file) + 1);
    memcpy(name, file.ptr, file.length);
    name[file.length] = 0;
    MUST(Char, char *, buffer, read_file_by_name(name));
    lexer_advance_source(lexer, ix);
    lexer_push_source(lexer, (StringView) { buffer, strlen(buffer) }, sv_from(name));
}

Token directive_handle(Lexer *lexer)
{
    char const *buffer = lexer_source(lexer).ptr;
    size_t      directive_start = 0;
    size_t      text_start = 0;

    while (buffer[directive_start] && isspace(buffer[directive_start]))
        ++directive_start;
    lexer_advance_source(lexer, directive_start);
    buffer = lexer_source(lexer).ptr;
    size_t directive_end = 0;
    while (buffer[directive_end] && isalpha(buffer[directive_end]))
        ++directive_end;
    if (directive_end == 0) {
        if (buffer[0]) {
            StringView directive = { buffer, 1 };
            fatal(LOC_SPEC "No preprocessor directive after '#' but '" SV_SPEC "'", SV_ARG(directive));
        } else {
            fatal(LOC_SPEC "'#' character cannot end source");
        }
    }
    StringView directive = { buffer, directive_end };
    for (int ix = 0; s_directives[ix].directive; ++ix) {
        if (sv_eq_cstr(directive, s_directives[ix].directive)) {
            lexer_advance_source(lexer, directive_end);
            buffer = lexer_source(lexer).ptr;
            while (buffer[text_start] && isspace(buffer[text_start]))
                ++text_start;
            lexer_advance_source(lexer, text_start);
            s_directives[ix].handler(directive, lexer);
            return lexer_peek(lexer);
        }
    }
    fatal(LOC_SPEC "Unrecognized preprocessor directive '" SV_SPEC "'", LOC_ARG(lexer->sources->loc), SV_ARG(directive));
}

Token lexer_set_current(Lexer *lexer, Token token)
{
    if (lexer->sources) {
        token.loc = lexer->sources->loc;
    }
    lexer->current = token;
    return lexer->current;
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
    if (!buffer || !buffer[0]) {
        return lexer_set_current(lexer, (Token) { { buffer, 0 }, TK_END_OF_FILE, TC_NONE });
    }
    switch (buffer[0]) {
    case '\n': {
        return lexer_set_current(lexer, (Token) { { buffer, 1 }, TK_WHITESPACE, TC_NEWLINE });
    }
    case ' ':
    case '\t': {
        size_t ix = 0;
        for (; buffer[ix] == ' ' || buffer[ix] == '\t'; ++ix)
            ;
        return lexer_set_current(lexer, (Token) { { buffer, ix }, TK_WHITESPACE, TC_WHITESPACE });
    }
    case '\'':
    case '"':
    case '`': {
        size_t ix = 1;
        for (; buffer[ix] && (buffer[ix] != buffer[0] || buffer[ix - 1] == '\\'); ++ix)
            ;
        TokenCode code;
        switch (buffer[0]) {
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
        return lexer_set_current(lexer, (Token) { { buffer, ix + 1 }, TK_QUOTED_STRING, code });
    }
    case '/':
        switch (buffer[1]) {
        case '/': {
            size_t ix = 2;
            for (; buffer[ix] && buffer[ix] != '\n'; ++ix)
                ;
            return lexer_set_current(lexer, (Token) { { buffer, ix + 1 }, TK_COMMENT, TC_END_OF_LINE_COMMENT });
        }
        case '*': {
            if (!buffer[2]) {
                return lexer_set_current(lexer, (Token) { { buffer, 2 }, TK_COMMENT, TC_UNTERMINATED_BLOCK_COMMENT });
            }
            size_t ix = 3;
            for (; buffer[ix] && buffer[ix - 1] != '*' && buffer[ix] != '/'; ++ix)
                ;
            return lexer_set_current(lexer, (Token) { { buffer, ix + 1 }, TK_COMMENT, (buffer[ix]) ? TC_UNTERMINATED_BLOCK_COMMENT : TC_BLOCK_COMMENT });
        }
        default:
            break;
        }
    case '#': {
        lexer_advance_source(lexer, 1);
        return lexer_set_current(lexer, directive_handle(lexer));
    }
    default:
        break;
    }
    if (isspace(buffer[0])) {
        size_t ix = 0;
        for (; isspace(buffer[ix]) && buffer[ix] != '\n'; ++ix)
            ;
        return lexer_set_current(lexer, (Token) { { buffer, ix + 1 }, TK_WHITESPACE, TC_WHITESPACE });
    }
    if (isdigit(buffer[0])) {
        return lexer_set_current(lexer, (Token) scan_number(buffer));
    }
    if (isalpha(buffer[0]) || buffer[0] == '_') {
        size_t ix = 0;
        for (; isalnum(buffer[ix]) || buffer[ix] == '_'; ++ix)
            ;
        for (int kw = 0; kw < KW_COUNT; ++kw) {
            if ((sv_length(s_keywords[kw].keyword) == ix) && sv_startswith(source, s_keywords[kw].keyword)) {
                return lexer_set_current(lexer, (Token) { { buffer, ix }, TK_KEYWORD, s_keywords[kw].code });
            }
        }
        return lexer_set_current(lexer, (Token) { { buffer, ix }, TK_IDENTIFIER, TC_IDENTIFIER });
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
            return lexer_set_current(lexer, (Token) { { buffer, sv_length(s_keywords[matched].keyword) }, TK_KEYWORD, TC_COUNT + matched });
        }
    }
    return lexer_set_current(lexer, (Token) { { buffer, 1 }, TK_SYMBOL, (int) buffer[0] });
}

Token lexer_next(Lexer *lexer)
{
    Token token;
    while (lexer->sources) {
        for (token = lexer_peek(lexer); token.kind != TK_END_OF_FILE; token = lexer_peek(lexer)) {
            if (!(lexer->skip_whitespace && token.kind == TK_WHITESPACE) && (token.kind != TK_COMMENT)) {
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
    lexer_advance_source(lexer, lexer->current.text.length);
    Token ret = lexer->current;
    lexer->current.text.ptr = lexer_source(lexer).ptr;
    lexer->current.text.length = 0;
    lexer->current.kind = TK_UNKNOWN;
    lexer->current.code = TC_NONE;
    return ret;
}

Token lexer_expect(Lexer *lexer, TokenKind kind, TokenCode code, char const *msg, ...)
{
    Token ret = lexer_next(lexer);
    if (!token_matches(ret, kind, code)) {
        va_list args;
        va_start(args, msg);
        StringView formatted = sv_vprintf(msg, args);
        fatal(LOC_SPEC "%.*s", LEXER_LOC_ARG(lexer), SV_ARG(formatted));
    }
    return lexer_lex(lexer);
}

bool lexer_next_matches(Lexer *lexer, TokenKind kind, TokenCode code)
{
    Token next = lexer_next(lexer);
    return token_matches(next, kind, code);
}
