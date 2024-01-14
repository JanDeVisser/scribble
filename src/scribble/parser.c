/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <dirent.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>

#define STATIC_ALLOCATOR
#include <allocate.h>
#include <engine.h>
#include <error_or.h>
#include <fn.h>
#include <fs.h>
#include <http.h>
#include <io.h>
#include <json.h>
#include <lexer.h>
#include <log.h>
#include <parser.h>
#include <sv.h>
#include <type.h>

#include <model/error.h>

static void        parser_context_add_verror(ParserContext *ctx, Token token, char const *msg, va_list args);
static void        parser_context_add_error(ParserContext *ctx, Token token, char const *msg, ...);
static void        parser_context_add_vnote(ParserContext *ctx, Token token, char const *msg, va_list args);
static void        parser_context_add_note(ParserContext *ctx, Token token, char const *msg, ...);
static SyntaxNode *parse_expression(ParserContext *ctx);
static SyntaxNode *parse_expression_1(ParserContext *ctx, SyntaxNode *lhs, int min_precedence);
static SyntaxNode *parse_primary_expression(ParserContext *ctx);
static bool        parse_expression_list(ParserContext *ctx, SyntaxNode **dst, char end);
static SyntaxNode *parse_statement(ParserContext *ctx);
static SyntaxNode *parse_type(ParserContext *ctx);
static SyntaxNode *parse_import(ParserContext *ctx);
static SyntaxNode *parse_module(ParserContext *ctx, StringView buffer, StringView name);
static void        parser_debug_info(ParserContext *ctx, char const *fmt, ...);
static void        parser_debug_node(ParserContext *ctx, SyntaxNode *node);

size_t next_index()
{
    static size_t counter = 0;
    return counter++;
}

SyntaxNode *syntax_node_make(ParserContext *ctx, SyntaxNodeType type, StringView name, Token token)
{
    SyntaxNode *node = (SyntaxNode *) allocate(sizeof(SyntaxNode));
    node->type = type;
    node->name = name;
    node->next = NULL;
    node->index = next_index();
    node->token = token;
    if (ctx && ctx->debug) {
        parser_debug_node(ctx, node);
    }
    return node;
}

void parser_context_add_verror(ParserContext *ctx, Token token, char const *msg, va_list args)
{
    da_append_ScribbleError(
        &ctx->errors,
        (ScribbleError) {
            .kind = SEK_SYNTAX,
            .token = token,
            .message = sv_vprintf(msg, args),
        });
}

void parser_context_add_error(ParserContext *ctx, Token token, char const *msg, ...)
{
    va_list args;

    va_start(args, msg);
    parser_context_add_verror(ctx, token, msg, args);
    va_end(args);
}

void parser_context_add_vnote(ParserContext *ctx, Token token, char const *msg, va_list args)
{
    assert(ctx->errors.size > 0);
    ScribbleError *last = da_element_ScribbleError(&ctx->errors, ctx->errors.size - 1);
    da_append_ScribbleError(
        &last->notes,
        (ScribbleError) {
            .kind = last->kind,
            .token = token,
            .message = sv_vprintf(msg, args),
        });
}

void parser_context_add_note(ParserContext *ctx, Token token, char const *msg, ...)
{
    va_list args;

    va_start(args, msg);
    parser_context_add_vnote(ctx, token, msg, args);
    va_end(args);
}

bool parser_context_token_is_error(ParserContext *ctx, ErrorOrToken token_maybe)
{
    if (ErrorOrToken_is_error(token_maybe)) {
        parser_context_add_error(ctx, lexer_lex(ctx->lexer), token_maybe.error.message);
        return true;
    }
    return false;
}

ErrorOrToken parser_context_match(ParserContext *ctx, Token token, TokenKind kind, TokenCode code)
{
    if (!token_matches(token, kind, code)) {
        StringView msg;
        switch (kind) {
        case TK_SYMBOL:
            msg = sv_printf("Expected '%c'", (char) code);
            break;
        case TK_KEYWORD:
            msg = sv_printf("Expected keyword '%s'", Keyword_text((KeywordCode) code));
            break;
        case TK_QUOTED_STRING:
            switch (code) {
            case TC_DOUBLE_QUOTED_STRING:
                msg = sv_printf("Expected double-quoted string");
                break;
            case TC_SINGLE_QUOTED_STRING:
                msg = sv_printf("Expected single-quoted string");
                break;
            case TC_BACK_QUOTED_STRING:
                msg = sv_printf("Expected backquoted string");
                break;
            default:
                UNREACHABLE();
            }
        default:
            msg = sv_printf("Expected '%s'", TokenKind_name(kind));
            break;
        }
        parser_context_add_error(ctx, token, sv_cstr(msg));
        ERROR(Token, ParserError, 0, sv_cstr(msg));
    }
    RETURN(Token, token);
}

bool parser_context_expect(ParserContext *ctx, TokenKind kind, TokenCode code)
{
    return ErrorOrToken_has_value(parser_context_match(ctx, lexer_next(ctx->lexer), kind, code));
}

ErrorOrToken parser_context_expect_token(ParserContext *ctx, TokenKind kind, TokenCode code)
{
    Token ret = TRY(Token, parser_context_match(ctx, lexer_next(ctx->lexer), kind, code));
    lexer_lex(ctx->lexer);
    RETURN(Token, ret);
}

ErrorOrToken parser_context_expect_identifier(ParserContext *ctx)
{
    return parser_context_expect_token(ctx, TK_IDENTIFIER, TC_IDENTIFIER);
}

bool parser_context_expect_and_discard(ParserContext *ctx, TokenKind kind, TokenCode code)
{
    bool ret = parser_context_expect(ctx, kind, code);
    if (ret) {
        lexer_lex(ctx->lexer);
    }
    return ret;
}

bool parser_context_expect_symbol(ParserContext *ctx, char symbol)
{
    return parser_context_expect_and_discard(ctx, TK_SYMBOL, symbol);
}

bool parser_context_expect_keyword(ParserContext *ctx, KeywordCode keyword)
{
    return parser_context_expect_and_discard(ctx, TK_KEYWORD, (TokenCode) keyword);
}

#define EXPECT_SYMBOL_OR(ctx, symbol, ret)                    \
    do {                                                      \
        if (!parser_context_expect_symbol((ctx), (symbol))) { \
            return (ret);                                     \
        }                                                     \
    } while (0)

#define EXPECT_SYMBOL(ctx, symbol) EXPECT_SYMBOL_OR(ctx, symbol, NULL)

#define EXPECT_KEYWORD(ctx, kw)                            \
    do {                                                   \
        if (!parser_context_expect_keyword((ctx), (kw))) { \
            return NULL;                                   \
        }                                                  \
    } while (0)

#define SKIP_SEMICOLON(ctx) EXPECT_SYMBOL((ctx), ';')

bool parser_context_accept(ParserContext *ctx, TokenKind kind, TokenCode code)
{
    Token next = lexer_next(ctx->lexer);
    return token_matches(next, kind, code);
}

OptionalToken parser_context_accept_token(ParserContext *ctx, TokenKind kind, TokenCode code)
{
    Token ret = lexer_next(ctx->lexer);
    if (token_matches(ret, kind, code)) {
        lexer_lex(ctx->lexer);
        return OptionalToken_create(ret);
    }
    return OptionalToken_empty();
}

bool parser_context_accept_and_discard(ParserContext *ctx, TokenKind kind, TokenCode code)
{
    bool ret = parser_context_accept(ctx, kind, code);
    if (ret) {
        lexer_lex(ctx->lexer);
    }
    return ret;
}

bool parser_context_accept_symbol(ParserContext *ctx, char symbol)
{
    return parser_context_accept_and_discard(ctx, TK_SYMBOL, symbol);
}

bool parser_context_accept_keyword(ParserContext *ctx, KeywordCode keyword)
{
    return parser_context_accept_and_discard(ctx, TK_KEYWORD, (TokenCode) keyword);
}

#define ACCEPT_SYMBOL_OR(ctx, symbol, ret)                    \
    do {                                                      \
        if (!parser_context_accept_symbol((ctx), (symbol))) { \
            return (ret);                                     \
        }                                                     \
    } while (0)
#define ACCEPT_SYMBOL_AND(ctx, symbol, ret)                  \
    do {                                                     \
        if (parser_context_accept_symbol((ctx), (symbol))) { \
            return (ret);                                    \
        }                                                    \
    } while (0)
#define ACCEPT_KEYWORD_OR(ctx, kw, ret)                    \
    do {                                                   \
        if (!parser_context_accept_keyword((ctx), (kw))) { \
            return (ret);                                  \
        }                                                  \
    } while (0)
#define ACCEPT_KEYWORD_AND(ctx, kw, ret)                  \
    do {                                                  \
        if (parser_context_accept_keyword((ctx), (kw))) { \
            return (ret);                                 \
        }                                                 \
    } while (0)

static OperatorMapping s_operator_mapping[] = {
#undef ENUM_BINARY_OPERATOR
#define ENUM_BINARY_OPERATOR(op, a, p, k, c, cl) { OP_##op, true, k, (TokenCode) c, (char) cl, p },
    BINARY_OPERATORS(ENUM_BINARY_OPERATOR)
#undef ENUM_BINARY_OPERATOR
#undef ENUM_UNARY_OPERATOR
#define ENUM_UNARY_OPERATOR(op, k, c) { OP_##op, false, k, (TokenCode) c, (char) 0, -1 },
        UNARY_OPERATORS(ENUM_UNARY_OPERATOR)
#undef ENUM_BINARY_OPERATOR
            { OP_COUNT, false, TK_UNKNOWN, TC_NONE, -1 }
};

OperatorMapping operator_for_token(Token token, bool binary)
{
    for (int ix = 0; s_operator_mapping[ix].operator!= OP_COUNT; ++ix) {
        if (token_matches(token, s_operator_mapping[ix].token_kind, s_operator_mapping[ix].token_code) && s_operator_mapping[ix].binary == binary) {
            return s_operator_mapping[ix];
        }
    }
    return s_operator_mapping[0];
}

/*
 * Precedence climbing method (https://en.wikipedia.org/wiki/Operator-precedence_parser):
 *
 * parse_expression()
 *    return parse_expression_1(parse_primary(), 0)
 *
 * parse_expression_1(lhs, min_precedence)
 *    lookahead := peek next token
 *    while lookahead is a binary operator whose precedence is >= min_precedence
 *      *op := lookahead
 *      advance to next token
 *      rhs := parse_primary ()
 *      lookahead := peek next token
 *      while lookahead is a binary operator whose precedence is greater
 *              than op's, or a right-associative operator
 *              whose precedence is equal to op's
 *        rhs := parse_expression_1 (rhs, precedence of op + 1)
 *        lookahead := peek next token
 *      lhs := the result of applying op with operands lhs and rhs
 *    return lhs
 */
SyntaxNode *parse_expression(ParserContext *ctx)
{
    SyntaxNode *primary = parse_primary_expression(ctx);
    if (!primary)
        return NULL;
    return parse_expression_1(ctx, primary, 0);
}

SyntaxNode *parse_expression_1(ParserContext *ctx, SyntaxNode *lhs, int min_precedence)
{
    Token           lookahead = lexer_next(ctx->lexer);
    OperatorMapping op = operator_for_token(lookahead, true);
    while (op.binary && op.precedence >= min_precedence) {
        lexer_lex(ctx->lexer);

        SyntaxNode *rhs = NULL;
        int         prec = op.precedence;
        if (op.operator== OP_CAST) {
            rhs = parse_type(ctx);
        } else {
            rhs = parse_primary_expression(ctx);
        }
        lookahead = lexer_next(ctx->lexer);
        OperatorMapping op_1 = operator_for_token(lookahead, true);
        while (op_1.binary && op_1.precedence > prec) {
            rhs = parse_expression_1(ctx, rhs, prec + 1);
            if (!rhs) {
                return NULL;
            }
            lookahead = lexer_next(ctx->lexer);
            op_1 = operator_for_token(lookahead, true);
        }
        if (op.operator== OP_SUBSCRIPT) {
            EXPECT_SYMBOL_OR(ctx, ']', NULL);
        }
        if (op.operator== OP_TERNARY) {
            if (rhs->type != SNT_BINARYEXPRESSION || rhs->binary_expr.operator!= OP_TERNARY_ELSE) {
                parser_context_add_error(ctx, rhs->token, "Expected ':' in ternary expression");
                return NULL;
            }
        }
        SyntaxNode *expr = syntax_node_make(ctx, SNT_BINARYEXPRESSION, sv_from(Operator_name(op.operator)), lhs->token);
        expr->binary_expr.lhs = lhs;
        expr->binary_expr.rhs = rhs;
        expr->binary_expr.operator= op.operator;
        lhs = expr;
        lookahead = lexer_next(ctx->lexer);
        op = operator_for_token(lookahead, true);
    }
    return lhs;
}

SyntaxNode *parse_primary_expression(ParserContext *ctx)
{
    Token token = lexer_next(ctx->lexer);
    switch (token.kind) {
    case TK_IDENTIFIER: {
        lexer_lex(ctx->lexer);
        SyntaxNode  *var = syntax_node_make(ctx, SNT_VARIABLE, token.text, token);
        SyntaxNode **name_part = &var->variable.subscript;
        while (parser_context_accept_symbol(ctx, '.')) {
            token = TRY_OR_NULL(Token, parser_context_expect_identifier(ctx));
            *name_part = syntax_node_make(ctx, SNT_VARIABLE, token.text, token);
            name_part = &(*name_part)->variable.subscript;
        }
        ACCEPT_SYMBOL_OR(ctx, '(', var);
        SyntaxNode *call = syntax_node_make(ctx, SNT_FUNCTION_CALL, var->name, var->token);
        var->type = SNT_FUNCTION;
        call->call.function = var;
        call->call.discard_result = false;
        if (!parse_expression_list(ctx, &call->call.arguments, ')')) {
            return NULL;
        }
        return call;
    }
    case TK_NUMBER: {
        lexer_lex(ctx->lexer);
        switch (token.code) {
        case TC_INTEGER: {
            SyntaxNode *ret = syntax_node_make(ctx, SNT_INTEGER, token.text, token);
            ret->integer.type = I32;
            Token type = lexer_next(ctx->lexer);
            if (token_matches_kind(type, TK_IDENTIFIER)) {
                char const *cstr = sv_cstr(type.text);
                IntegerType integer_type = IntegerType_from_name(cstr);
                if (cstr != type.text.ptr) {
                    free_buffer((char *) cstr);
                }
                if (integer_type != IU_NO_SUCH_TYPE) {
                    lexer_lex(ctx->lexer);
                    ret->integer.type = integer_type;
                }
            }
            return ret;
        }
        case TC_HEXNUMBER: {
            SyntaxNode *ret = syntax_node_make(ctx, SNT_INTEGER, token.text, token);
            ret->integer.type = (IntegerType) align_at(4 * (token.text.length - 2), 8);
            return ret;
        }
        case TC_DECIMAL: {
            return syntax_node_make(ctx, SNT_DECIMAL, token.text, token);
        }
        default:
            UNREACHABLE();
        }
    }
    case TK_QUOTED_STRING:
        switch (token.code) {
        case TC_DOUBLE_QUOTED_STRING:
            lexer_lex(ctx->lexer);
            return syntax_node_make(ctx, SNT_STRING, sv_decode_quoted_str(token.text), token);
        default:
            return NULL;
        }
    case TK_KEYWORD:
        switch (token.code) {
        case KW_TRUE:
        case KW_FALSE:
            lexer_lex(ctx->lexer);
            return syntax_node_make(ctx, SNT_BOOL, token.text, token);
        default:
            return NULL;
        }
    case TK_SYMBOL:
        switch (token.code) {
        case '(': {
            lexer_lex(ctx->lexer);
            SyntaxNode *ret = parse_expression(ctx);
            if (!parser_context_expect_symbol(ctx, ')')) {
                return NULL;
            }
            return ret;
        }
        case '{': {
            lexer_lex(ctx->lexer);
            SyntaxNode *ret = syntax_node_make(ctx, SNT_COMPOUND, token.text, token);
            if (!parse_expression_list(ctx, &ret->compound_expr.expressions, '}')) {
                return NULL;
            }
            return ret;
        }
        default: {
            OperatorMapping op = operator_for_token(token, false);
            if (op.operator!= OP_INVALID) {
                lexer_lex(ctx->lexer);
                SyntaxNode *operand = parse_primary_expression(ctx);
                if (operand) {
                    SyntaxNode *unary_expr = syntax_node_make(ctx, SNT_UNARYEXPRESSION, token.text, token);
                    unary_expr->unary_expr.operand = operand;
                    unary_expr->unary_expr.operator= op.operator;
                    return unary_expr;
                }
            }
            return NULL;
        }
        }
    default:
        return NULL;
    }
}

bool parse_expression_list(ParserContext *ctx, SyntaxNode **dst, char end)
{
    ACCEPT_SYMBOL_AND(ctx, end, true);
    while (true) {
        SyntaxNode *arg = parse_expression(ctx);
        if (!arg) {
            return false;
        }
        (*dst) = arg;
        dst = &(*dst)->next;
        ACCEPT_SYMBOL_AND(ctx, end, true);
        EXPECT_SYMBOL_OR(ctx, ',', false);
    }
}

SyntaxNode *parse_identifier(ParserContext *ctx)
{
    Token        token = lexer_lex(ctx->lexer);
    SyntaxNode  *var = syntax_node_make(ctx, SNT_VARIABLE, token.text, token);
    SyntaxNode **name_part = &var->variable.subscript;
    SyntaxNode  *ret = NULL;
    while (parser_context_accept_symbol(ctx, '.')) {
        token = TRY_OR_NULL(Token, parser_context_expect_identifier(ctx));
        *name_part = syntax_node_make(ctx, SNT_VARIABLE, token.text, token);
        name_part = &(*name_part)->variable.subscript;
    }
    if (parser_context_accept_symbol(ctx, '(')) {
        ret = syntax_node_make(ctx, SNT_FUNCTION_CALL, var->name, var->token);
        var->type = SNT_FUNCTION;
        ret->call.function = var;
        ret->call.discard_result = true;
        if (!parse_expression_list(ctx, &ret->call.arguments, ')')) {
            return NULL;
        }
    } else if (parser_context_accept_symbol(ctx, '=')) {
        ret = syntax_node_make(ctx, SNT_ASSIGNMENT, var->name, var->token);
        ret->assignment.variable = var;
        ret->assignment.expression = parse_expression(ctx);
        if (ret->assignment.expression == NULL) {
            return NULL;
        }
    } else if (parser_context_accept_symbol(ctx, ':')) {
        ret = syntax_node_make(ctx, SNT_LABEL, var->name, var->token);
        return ret;
    }
    SKIP_SEMICOLON(ctx);
    return ret;
}

SyntaxNode *parse_variable_declaration(ParserContext *ctx, bool is_const)
{
    Token       var = lexer_lex(ctx->lexer);
    SyntaxNode *type = NULL;

    Token       ident = TRY_OR_NULL(Token, parser_context_expect_token(ctx, TK_IDENTIFIER, TC_IDENTIFIER));
    SyntaxNode *ret = syntax_node_make(ctx, SNT_VARIABLE_DECL, ident.text, var);
    ret->variable_decl.variable = syntax_node_make(ctx, SNT_VARIABLE, ident.text, ident);
    if (parser_context_accept_symbol(ctx, ':')) {
        type = parse_type(ctx);
    }
    ret->variable_decl.var_type = type;
    if (parser_context_accept_symbol(ctx, '=')) {
        ret->variable_decl.init_expr = parse_expression(ctx);
        if (ret->variable_decl.init_expr == NULL) {
            return NULL;
        }
    } else if (is_const) {
        parser_context_add_error(ctx, ident, "'const' declaration without initializer expression");
        return NULL;
    }
    ret->variable_decl.is_const = is_const;
    SKIP_SEMICOLON(ctx);
    return ret;
}

SyntaxNode *parse_if(ParserContext *ctx)
{
    Token       token = lexer_lex(ctx->lexer);
    SyntaxNode *expr = parse_expression(ctx);
    if (!expr) {
        parser_context_add_error(ctx, token, "Expected condition in 'if' statement");
        return NULL;
    }
    SyntaxNode *if_true = parse_statement(ctx);
    if (!if_true) {
        parser_context_add_error(ctx, token, "Expected 'true' branch for 'if' statement");
        return NULL;
    }
    SyntaxNode *if_false = NULL;
    if (parser_context_accept_and_discard(ctx, TK_KEYWORD, (TokenCode) KW_ELSE)) {
        if_false = parse_statement(ctx);
    }
    SyntaxNode *ret = syntax_node_make(ctx, SNT_IF, sv_from("if"), token);
    ret->if_statement.condition = expr;
    ret->if_statement.if_true = if_true;
    ret->if_statement.if_false = if_false;
    return ret;
}

SyntaxNode *parse_for(ParserContext *ctx)
{
    Token token = lexer_lex(ctx->lexer);
    Token variable = TRY_OR_NULL(Token, parser_context_expect_identifier(ctx));
    EXPECT_KEYWORD(ctx, KW_IN);
    SyntaxNode *range = parse_expression(ctx);
    if (!range) {
        parser_context_add_error(ctx, token, "Expected range expression in 'for' statement");
        return NULL;
    }
    SyntaxNode *stmt = parse_statement(ctx);
    if (!stmt) {
        parser_context_add_error(ctx, token, "Expected statement for 'for' loop");
        return NULL;
    }
    SyntaxNode *ret = syntax_node_make(ctx, SNT_FOR, sv_from("$for"), token);
    ret->for_statement.variable = variable.text;
    ret->for_statement.range = range;
    ret->for_statement.statement = stmt;
    return ret;
}

SyntaxNode *parse_return(ParserContext *ctx)
{
    Token       token = lexer_lex(ctx->lexer);
    SyntaxNode *ret = syntax_node_make(ctx, SNT_RETURN, sv_from("return"), token);
    ACCEPT_SYMBOL_AND(ctx, ';', ret);
    ret->return_stmt.expression = parse_expression(ctx);
    if (ret->return_stmt.expression == NULL) {
        return NULL;
    }
    SKIP_SEMICOLON(ctx);
    return ret;
}

SyntaxNode *parse_while(ParserContext *ctx)
{
    Token       token = lexer_lex(ctx->lexer);
    SyntaxNode *expr = parse_expression(ctx);
    if (!expr) {
        parser_context_add_error(ctx, token, "Expected condition in 'while' statement");
        return NULL;
    }
    SyntaxNode *stmt = parse_statement(ctx);
    if (!stmt) {
        parser_context_add_error(ctx, token, "Expected statement for 'while' loop");
        return NULL;
    }
    SyntaxNode *ret = syntax_node_make(ctx, SNT_WHILE, sv_from("$while"), token);
    ret->while_statement.condition = expr;
    ret->while_statement.statement = stmt;
    return ret;
}

SyntaxNode *parse_loop(ParserContext *ctx)
{
    Token       token = lexer_lex(ctx->lexer);
    SyntaxNode *stmt = parse_statement(ctx);
    if (!stmt) {
        parser_context_add_error(ctx, token, "Expected statement for loop");
        return NULL;
    }
    SyntaxNode *ret = syntax_node_make(ctx, SNT_LOOP, sv_from("$loop"), token);
    ret->block.statements = stmt;
    return ret;
}

SyntaxNode *parse_block(ParserContext *ctx)
{
    Token        token = lexer_lex(ctx->lexer);
    SyntaxNode  *ret = syntax_node_make(ctx, SNT_BLOCK, sv_from("block"), token);
    SyntaxNode **dst = &ret->block.statements;
    while (true) {
        ACCEPT_SYMBOL_AND(ctx, '}', ret);
        if (parser_context_accept_and_discard(ctx, TK_END_OF_FILE, TC_NONE)) {
            parser_context_add_error(ctx, token, "Expected '}' to close block");
            return NULL;
        }
        SyntaxNode *stmt = parse_statement(ctx);
        if (stmt == NULL) {
            return NULL;
        }
        *dst = stmt;
        dst = &stmt->next;
    }
}

SyntaxNode *parse_statement(ParserContext *ctx)
{
    Token       token = lexer_next(ctx->lexer);
    SyntaxNode *ret;
    switch (token.kind) {
    case TK_SYMBOL: {
        switch (token.code) {
        case '{':
            return parse_block(ctx);
        default:
            parser_context_add_error(ctx, token, "Unexpected symbol '%c'", (char) token.code);
            return NULL;
        }
    }
    case TK_KEYWORD: {
        switch (token.code) {
        case KW_BREAK:
        case KW_CONTINUE: {
            Token stmt_token = lexer_lex(ctx->lexer);
            token = TRY_OR_NULL(Token, parser_context_expect_identifier(ctx));
            ret = syntax_node_make(ctx, (stmt_token.code == KW_BREAK) ? SNT_BREAK : SNT_CONTINUE, token.text, stmt_token);
            SKIP_SEMICOLON(ctx);
        } break;
        case KW_CONST:
            ret = parse_variable_declaration(ctx, true);
            break;
        case KW_IF:
            ret = parse_if(ctx);
            break;
        case KW_FOR:
            ret = parse_for(ctx);
            break;
        case KW_LOOP:
            ret = parse_loop(ctx);
            break;
        case KW_RETURN:
            ret = parse_return(ctx);
            break;
        case KW_VAR:
            ret = parse_variable_declaration(ctx, false);
            break;
        case KW_WHILE:
            ret = parse_while(ctx);
            break;
        default:
            NYI("Keywords");
        }
        break;
    }
    case TK_IDENTIFIER:
        ret = parse_identifier(ctx);
        break;
    default:
        parser_context_add_error(ctx, token, "Unexpected token '%.*s'", SV_ARG(token.text));
        return NULL;
    }
    return ret;
}

bool parse_type_descr(ParserContext *ctx, Token type_name, TypeDescr *target)
{
    target->name = type_name.text;
    if (parser_context_accept_symbol(ctx, '<')) {
        while (true) {
            Token      token = TRY_OR_FALSE(Token, parser_context_expect_identifier(ctx));
            TypeDescr *component = allocate_new(TypeDescr);
            DIA_APPEND(TypeDescr *, target, component);
            if (!parse_type_descr(ctx, token, component)) {
                return false;
            }
            if (parser_context_accept_symbol(ctx, '>')) {
                return true;
            }
            if (!parser_context_expect_symbol(ctx, ',')) {
                return false;
            }
        }
    }
    return true;
}

SyntaxNode *parse_type(ParserContext *ctx)
{
    Token       type_name = TRY_OR_NULL(Token, parser_context_expect_identifier(ctx));
    SyntaxNode *ret = syntax_node_make(ctx, SNT_TYPE, type_name.text, type_name);
    if (!parse_type_descr(ctx, type_name, &ret->type_descr)) {
        return NULL;
    }
    return ret;
}

SyntaxNode *parse_param(ParserContext *ctx)
{
    Token name = TRY_OR_NULL(Token, parser_context_expect_identifier(ctx));
    EXPECT_SYMBOL(ctx, ':');
    SyntaxNode *type = parse_type(ctx);
    if (!type) {
        return NULL;
    }
    SyntaxNode *param = syntax_node_make(ctx, SNT_PARAMETER, name.text, name);
    param->parameter.parameter_type = type;
    return param;
}

SyntaxNode *parse_parameters(ParserContext *ctx, SyntaxNode *func)
{
    if (!parser_context_expect_symbol(ctx, '(')) {
        return NULL;
    }
    if (parser_context_accept_symbol(ctx, ')')) {
        return func;
    }
    SyntaxNode *last_param = NULL;
    while (true) {
        SyntaxNode *param = parse_param(ctx);
        if (!param) {
            return NULL;
        }
        if (!last_param) {
            func->function.parameter = param;
        } else {
            last_param->next = param;
        }
        last_param = param;
        if (parser_context_accept_symbol(ctx, ')')) {
            return func;
        }
        EXPECT_SYMBOL(ctx, ',');
    }
}

SyntaxNode *parse_return_types(ParserContext *ctx, SyntaxNode *func)
{
    if (!parser_context_expect(ctx, TK_SYMBOL, ':')) {
        return NULL;
    }
    lexer_lex(ctx->lexer);
    func->function.return_type = parse_type(ctx);
    if (!func->function.return_type) {
        return NULL;
    }
    ACCEPT_SYMBOL_OR(ctx, '/', func);
    func->function.error_type = parse_type(ctx);
    if (!func->function.error_type) {
        return NULL;
    }
    return func;
}

SyntaxNode *parse_function_decl(ParserContext *ctx)
{
    lexer_lex(ctx->lexer);
    Token       token = TRY_OR_NULL(Token, parser_context_expect_identifier(ctx));
    SyntaxNode *func = syntax_node_make(ctx, SNT_FUNCTION, token.text, token);
    if (parse_parameters(ctx, func) == NULL) {
        return NULL;
    }
    if (parse_return_types(ctx, func) == NULL) {
        return NULL;
    }
    return func;
}

SyntaxNode *parse_function(ParserContext *ctx)
{
    SyntaxNode *func = parse_function_decl(ctx);
    if (!func) {
        return NULL;
    }
    if (parser_context_accept_symbol(ctx, '{')) {
        SyntaxNode *impl = syntax_node_make(ctx, SNT_FUNCTION_IMPL, func->name, func->token);
        func->function.function_impl = impl;
        SyntaxNode *last_stmt = NULL;
        while (true) {
            if (parser_context_accept_symbol(ctx, '}')) {
                return func;
            }
            SyntaxNode *stmt = parse_statement(ctx);
            if (!stmt) {
                while (!parser_context_accept_symbol(ctx, ';') && !parser_context_accept_symbol(ctx, '}') && !parser_context_accept_and_discard(ctx, TK_END_OF_FILE, TC_NONE)) {
                    lexer_lex(ctx->lexer);
                }
                continue;
            }
            if (last_stmt == NULL) {
                impl->function_impl.statements = stmt;
            } else {
                last_stmt->next = stmt;
            }
            last_stmt = stmt;
        }
    }
    if (parser_context_accept_and_discard(ctx, TK_KEYWORD, (TokenCode) KW_FUNC_BINDING)) {
        Token const token = TRY_OR_NULL(Token, parser_context_expect_token(ctx, TK_QUOTED_STRING, TC_DOUBLE_QUOTED_STRING));
        SKIP_SEMICOLON(ctx);
        func->function.function_impl = syntax_node_make(ctx,
            SNT_NATIVE_FUNCTION,
            (StringView) { token.text.ptr + 1, token.text.length - 2 },
            token);
        return func;
    }
    if (parser_context_accept_and_discard(ctx, TK_KEYWORD, (TokenCode) KW_MACRO_BINDING)) {
        Token const token = TRY_OR_NULL(Token, parser_context_expect_token(ctx, TK_QUOTED_STRING, TC_DOUBLE_QUOTED_STRING));
        SKIP_SEMICOLON(ctx);
        func->function.function_impl = syntax_node_make(ctx,
            SNT_MACRO,
            (StringView) { token.text.ptr + 1, token.text.length - 2 },
            token);
        return func;
    }
    parser_context_add_error(ctx, func->token, "Expected '{', '->', or '=>' after function declaration");
    return NULL;
}

SyntaxNode *parse_enum_def(ParserContext *ctx)
{
    lexer_lex(ctx->lexer);
    Token       ident = TRY_OR_NULL(Token, parser_context_expect_identifier(ctx));
    SyntaxNode *underlying_type = NULL;
    if (parser_context_accept_symbol(ctx, ':')) {
        if ((underlying_type = parse_type(ctx)) == NULL) {
            return NULL;
        }
    }
    EXPECT_SYMBOL(ctx, '{');
    SyntaxNode *enum_node = syntax_node_make(ctx, SNT_ENUMERATION, ident.text, ident);
    enum_node->enumeration.underlying_type = underlying_type;
    SyntaxNode **value = &enum_node->enumeration.values;
    while (!parser_context_accept_symbol(ctx, '}')) {
        Token value_name = TRY_OR_NULL(Token, parser_context_expect_token(ctx, TK_IDENTIFIER, TC_IDENTIFIER));
        *value = syntax_node_make(ctx, SNT_ENUM_VALUE, value_name.text, value_name);
        SyntaxNode *underlying_value = NULL;
        if (parser_context_accept_symbol(ctx, '=')) {
            if ((underlying_value = parse_expression(ctx)) == NULL) {
                return NULL;
            }
        }
        (*value)->enum_value.underlying_value = underlying_value;
        if (parser_context_accept_and_discard(ctx, TK_SYMBOL, '}')) {
            break;
        }
        EXPECT_SYMBOL(ctx, ',');
        value = &(*value)->next;
    }
    SKIP_SEMICOLON(ctx);
    return enum_node;
}

SyntaxNode *parse_struct_def(ParserContext *ctx)
{
    lexer_lex(ctx->lexer);
    Token ident = TRY_OR_NULL(Token, parser_context_expect_identifier(ctx));
    EXPECT_SYMBOL(ctx, '{');
    SyntaxNode  *strukt = syntax_node_make(ctx, SNT_STRUCT, ident.text, ident);
    SyntaxNode **comp = &strukt->struct_def.components;
    while (!parser_context_accept_symbol(ctx, '}')) {
        Token comp_name = TRY_OR_NULL(Token, parser_context_expect_identifier(ctx));
        EXPECT_SYMBOL(ctx, ':');
        *comp = syntax_node_make(ctx, SNT_TYPE_COMPONENT, comp_name.text, comp_name);
        if (((*comp)->parameter.parameter_type = parse_type(ctx)) == NULL) {
            return NULL;
        }
        if (parser_context_accept(ctx, TK_SYMBOL, '}')) {
            break;
        }
        EXPECT_SYMBOL(ctx, ',');
        comp = &(*comp)->next;
    }
    SKIP_SEMICOLON(ctx);
    return strukt;
}

SyntaxNode *parse_variant_def(ParserContext *ctx)
{
    lexer_lex(ctx->lexer);
    Token       ident = TRY_OR_NULL(Token, parser_context_expect_token(ctx, TK_IDENTIFIER, TC_IDENTIFIER));
    SyntaxNode *underlying_type = NULL;
    if (parser_context_accept_symbol(ctx, ':')) {
        underlying_type = parse_type(ctx);
    }
    EXPECT_SYMBOL(ctx, '{');
    SyntaxNode *variant_node = syntax_node_make(ctx, SNT_VARIANT, ident.text, ident);
    variant_node->variant_def.underlying_type = underlying_type;
    SyntaxNode **value = &variant_node->variant_def.options;
    while (!parser_context_accept_symbol(ctx, '}')) {
        Token option_name = TRY_OR_NULL(Token, parser_context_expect_identifier(ctx));
        *value = syntax_node_make(ctx, SNT_VARIANT_OPTION, option_name.text, option_name);
        SyntaxNode *underlying_value = NULL;
        SyntaxNode *payload_type = NULL;
        if (parser_context_accept_symbol(ctx, '(')) {
            if ((payload_type = parse_type(ctx)) == NULL) {
                return NULL;
            }
            EXPECT_SYMBOL(ctx, ')');
        }
        if (parser_context_accept_symbol(ctx, '=')) {
            if ((underlying_value = parse_expression(ctx)) == NULL) {
                return NULL;
            }
        }
        (*value)->variant_option.underlying_value = underlying_value;
        (*value)->variant_option.payload_type = payload_type;
        if (parser_context_accept_symbol(ctx, '}')) {
            break;
        }
        EXPECT_SYMBOL(ctx, ',');
        value = &(*value)->next;
    }
    SKIP_SEMICOLON(ctx);
    return variant_node;
}

SyntaxNode *import_package(ParserContext *ctx, Token token, StringView path)
{
    StringView name = sv_replace(path, sv_from("/"), sv_from("."));
    StringView file_name = sv_printf("%.*s.scribble", SV_ARG(path));
    if (!fs_file_exists(file_name)) {
        StringView scribble_dir = sv_from(getenv("SCRIBBLE_DIR"));
        if (sv_empty(scribble_dir) || !fs_file_exists(scribble_dir)) {
            scribble_dir = sv_from(SCRIBBLE_DIR);
        }
        if (sv_empty(scribble_dir) || !fs_file_exists(scribble_dir)) {
            scribble_dir = sv_from("/usr/local/scribble");
        }
        if (sv_empty(scribble_dir) || !fs_file_exists(scribble_dir)) {
            fatal("Broken scribble installation");
        }
        file_name = sv_printf("%.*s/share/%.*s", SV_ARG(scribble_dir), SV_ARG(file_name));
    }
    if (!fs_file_exists(file_name)) {
        parser_context_add_error(ctx, token, "Could not find import '%.*s'", SV_ARG(path));
        return NULL;
    }
    SyntaxNode *import = syntax_node_make(ctx, SNT_IMPORT, name, token);
    ErrorOrChar buffer_maybe = read_file_by_name(sv_cstr(file_name));
    if (ErrorOrChar_is_error(buffer_maybe)) {
        parser_context_add_error(ctx, token, "Could not read import '%.*s'", SV_ARG(path));
        parser_context_add_note(ctx, token, buffer_maybe.error.message);
        return NULL;
    }
    char       *buffer = buffer_maybe.value;
    SyntaxNode *module = parse_module(ctx, sv_from(buffer), name);
    import->next = ctx->program->program.imports;
    ctx->program->program.imports = import;
    module->next = import->import.modules;
    import->import.modules = module;
    return import;
}

SyntaxNode *parse_import(ParserContext *ctx)
{
    Token      token = lexer_lex(ctx->lexer);
    Token      name = TRY_OR_NULL(Token, parser_context_expect_token(ctx, TK_QUOTED_STRING, TC_DOUBLE_QUOTED_STRING));
    StringView path = sv_decode_quoted_str(name.text);
    SKIP_SEMICOLON(ctx);
    return import_package(ctx, token, path);
}

SyntaxNode *parse_module(ParserContext *ctx, StringView buffer, StringView name)
{
    Token       token = { name, TK_MODULE, TC_NONE };
    SyntaxNode *module = syntax_node_make(ctx, SNT_MODULE, name, token);
    Lexer       lexer = { 0 };

    parser_debug_info(ctx, "Compiling '%.*s'", SV_ARG(name));
    lexer.skip_whitespace = true;
    ctx->lexer = &lexer;
    lexer_push_source(&lexer, buffer, name);
    SyntaxNode *last_statement = NULL;
    while (true) {
        token = lexer_next(&lexer);

        SyntaxNode *statement = NULL;
        if (token_matches(token, TK_KEYWORD, KW_FUNC)) {
            statement = parse_function(ctx);
            if (statement) {
                trace(CAT_PARSE, "Function '%.*s' parsed", SV_ARG(statement->name));
            }
        } else if (token_matches(token, TK_KEYWORD, KW_ENUM)) {
            statement = parse_enum_def(ctx);
        } else if (token_matches(token, TK_KEYWORD, KW_STRUCT)) {
            statement = parse_struct_def(ctx);
        } else if (token_matches(token, TK_KEYWORD, KW_VARIANT)) {
            statement = parse_variant_def(ctx);
        } else if (token_matches(token, TK_KEYWORD, KW_IMPORT)) {
            statement = parse_import(ctx);
        } else if (token_matches(token, TK_KEYWORD, KW_CONST)) {
            statement = parse_variable_declaration(ctx, true);
        } else if (token_matches(token, TK_KEYWORD, KW_VAR)) {
            statement = parse_variable_declaration(ctx, false);
        } else if (token_matches_kind(token, TK_END_OF_FILE)) {
            return module;
        } else {
            parser_context_add_error(ctx, token, "Only 'import', 'func', 'var', 'const', and 'struct' are allowed on the top level of files, '%.*s' is not", SV_ARG(token.text));
            while (true) {
                lexer_lex(&lexer);
                token = lexer_next(&lexer);
                if (token_matches(token, TK_KEYWORD, KW_FUNC) || token_matches(token, TK_KEYWORD, KW_STRUCT) || token_matches(token, TK_KEYWORD, KW_VAR) || token_matches(token, TK_KEYWORD, KW_CONST)) {
                    break;
                }
                if (token_matches_kind(token, TK_END_OF_FILE)) {
                    return module;
                }
            }
        }
        if (statement) {
            if (!last_statement) {
                module->block.statements = statement;
            } else {
                last_statement->next = statement;
            }
            last_statement = statement;
        }
    }
    return module;
}

SyntaxNode *parse_module_file(ParserContext *ctx, int dir_fd, char const *file)
{
    char *buffer = TRY_OR_NULL(Char, read_file_at(dir_fd, file));
    return parse_module(ctx, sv_from(buffer), fn_barename(sv_copy_cstr(file)));
}

void parser_debug_info(ParserContext *ctx, char const *fmt, ...)
{
    if (!ctx->debug)
        return;
    va_list args;
    va_start(args, fmt);
    HTTP_POST_MUST(ctx->frontend, "/parser/info", json_string(sv_vprintf(fmt, args)));
    va_end(args);
}

void parser_debug_node(ParserContext *ctx, SyntaxNode *node)
{
    if (!ctx || !ctx->debug)
        return;
    JSONValue n = json_object();
    json_set_string(&n, "name", node->name);
    json_set_cstr(&n, "type", SyntaxNodeType_name(node->type));
    JSONValue loc = json_object();
    json_set_string(&loc, "filename", node->token.loc.file);
    json_set_int(&loc, "line", node->token.loc.line);
    json_set_int(&loc, "column", node->token.loc.column);
    json_set(&n, "location", loc);
    HTTP_POST_MUST(ctx->frontend, "/parser/node", n);
}

ParserContext parse(BackendConnection *conn, JSONValue config)
{
    StringView dir_or_file = json_get_string(&config, "target", sv_from("."));

    ParserContext ret = { 0 };
    ret.frontend = conn->fd;
    ret.debug = json_get_bool(&config, "debug", false);
    ret.source_name = sv_copy(dir_or_file);

    if (ret.debug) {
        HTTP_POST_MUST(conn->fd, "/parser/start", MUST_OPTIONAL(JSONValue, json_get(&config, "target")));
        char cwd[256];
        getcwd(cwd, 256);
        parser_debug_info(&ret, "CWD: %s dir: %.*s", cwd, SV_ARG(dir_or_file));
    }

    Token token = { ret.source_name, TK_PROGRAM, TC_NONE };
    ret.program = syntax_node_make(&ret, SNT_PROGRAM, fn_barename(ret.source_name), token);
    import_package(&ret, token, sv_from("std"));

    char const *dir_cstr = sv_cstr(ret.source_name);
    DIR        *dir = opendir(dir_cstr);
    if (dir == NULL) {
        if (errno == ENOTDIR) {
            dir = opendir(".");
            if (dir == NULL) {
                fatal("Could not open current directory");
            }
            SyntaxNode *module = parse_module_file(&ret, dirfd(dir), dir_cstr);
            module->next = ret.program->program.modules;
            ret.program->program.modules = module;
            closedir(dir);
            if (ret.debug) {
                HTTP_GET_MUST(conn->fd, "/parser/done", sl_create());
            }
            return ret;
        }
        fatal("Could not open directory '%.*s'", SV_ARG(dir_or_file));
    }

    struct dirent *dp;
    while ((dp = readdir(dir)) != NULL) {
#ifdef HAVE_DIRENT_D_NAMLEN
        if ((dp->d_namlen > 8) && strcmp(dp->d_name + (dp->d_namlen - 9), ".scribble") == 0) {
#else
        size_t namlen = strlen(dp->d_name);
        if ((namlen > 8) && strcmp(dp->d_name + (namlen - 9), ".scribble") == 0) {
#endif
            SyntaxNode *module = parse_module_file(&ret, dirfd(dir), dp->d_name);
            if (!module) {
                continue;
            }
            module->next = ret.program->program.modules;
            ret.program->program.modules = module;
        }
    }
    closedir(dir);
    if (ret.debug) {
        HTTP_GET_MUST(conn->fd, "/parser/done", sl_create());
    }
    return ret;
}
