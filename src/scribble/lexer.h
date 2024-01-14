/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#ifndef __LEXER_H__
#define __LEXER_H__

#include <ctype.h>

#include <sv.h>

#include <model/token.h>

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

#define LEXER_LOC_ARG(lexer) LOC_ARG(lexer->sources->loc)

#endif /* __LEXER_H__ */
