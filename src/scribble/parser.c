/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <dirent.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>

#include <error_or.h>
#include <fn.h>
#include <fs.h>
#include <io.h>
#include <lexer.h>
#include <log.h>
#include <options.h>
#include <parser.h>
#include <sv.h>

#define STATIC_ALLOCATOR
#include <allocate.h>

static SyntaxNode *syntax_node_make(SyntaxNodeType type, StringView name, Token token);
static SyntaxNode *parse_expression(ParserContext *ctx);
static SyntaxNode *parse_expression_1(ParserContext *ctx, SyntaxNode *lhs, int min_precedence);
static SyntaxNode *parse_primary_expression(ParserContext *ctx);
static void        parse_arguments(ParserContext *ctx, SyntaxNode *node, char start, char end);
static SyntaxNode *parse_statement(ParserContext *ctx);
static SyntaxNode *parse_type(ParserContext *ctx);
static SyntaxNode *parse_import(ParserContext *ctx);
static SyntaxNode *parse_module(ParserContext *ctx, StringView buffer, StringView name);

char const *SyntaxNodeType_name(SyntaxNodeType type)
{
    switch (type) {
#undef SYNTAXNODETYPE_ENUM
#define SYNTAXNODETYPE_ENUM(type) \
    case SNT_##type:              \
        return "SNT_" #type;
        SYNTAXNODETYPES(SYNTAXNODETYPE_ENUM)
#undef SYNTAXNODETYPE_ENUM
    default:
        UNREACHABLE();
    }
}

size_t next_index()
{
    static size_t counter = 0;
    return counter++;
}

SyntaxNode *syntax_node_make(SyntaxNodeType type, StringView name, Token token)
{
    SyntaxNode *node = (SyntaxNode *) allocate(sizeof(SyntaxNode));
    node->type = type;
    node->name = name;
    node->next = NULL;
    node->index = next_index();
    node->token = token;
    return node;
}

void parser_context_add_error(ParserContext *ctx, Token token, StringView msg)
{
    ScribbleError *err = allocate_new(ScribbleError);
    err->token = token;
    err->kind = SEK_SYNTAX;
    err->message = msg;
    if (!ctx->first_error) {
        assert(!ctx->last_error);
        ctx->first_error = ctx->last_error = err;
    } else {
        assert(ctx->last_error);
        ctx->last_error->next = err;
        ctx->last_error = err;
    }
    printf(LOC_SPEC SV_SPEC "\n", LOC_ARG(err->token.loc), SV_ARG(err->message));
}

bool parser_context_token_is_error(ParserContext *ctx, ErrorOrToken token_maybe)
{
    if (ErrorOrToken_is_error(token_maybe)) {
        parser_context_add_error(ctx, lexer_lex(ctx->lexer), sv_from(token_maybe.error.message));
        return true;
    }
    return false;
}

bool parser_context_match(ParserContext *ctx, Token token, TokenKind kind, TokenCode code)
{
    if (!token_matches(token, kind, code)) {
        StringView msg = { 0 };
        switch (token.kind) {
        case TK_SYMBOL:
            msg = sv_printf("Expected '%c'", (char) code);
            break;
        case TK_KEYWORD:
            msg = sv_printf("Expected keyword '%s'", TokenCode_name(code));
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
        parser_context_add_error(ctx, token, msg);
        return false;
    }
    return true;
}

bool parser_context_expect(ParserContext *ctx, TokenKind kind, TokenCode code)
{
    return parser_context_match(ctx, lexer_next(ctx->lexer), kind, code);
}

bool parser_context_expect_and_discard(ParserContext *ctx, TokenKind kind, TokenCode code)
{
    bool ret = parser_context_expect(ctx, kind, code);
    if (ret) {
        lexer_lex(ctx->lexer);
    }
    return ret;
}

bool parser_context_accept(ParserContext *ctx, TokenKind kind, TokenCode code)
{
    return token_matches(lexer_next(ctx->lexer), kind, code);
}

bool parser_context_accept_and_discard(ParserContext *ctx, TokenKind kind, TokenCode code)
{
    bool ret = parser_context_accept(ctx, kind, code);
    if (ret) {
        lexer_lex(ctx->lexer);
    }
    return ret;
}

void parse_arguments(ParserContext *ctx, SyntaxNode *node, char start, char end)
{
    if (!parser_context_expect_and_discard(ctx, TK_SYMBOL, start)) {
        return;
    }
    Token token = lexer_next(ctx->lexer);
    if (token_matches(token, TK_SYMBOL, end)) {
        lexer_lex(ctx->lexer);
        return;
    }
    SyntaxNode *last_arg = NULL;
    while (true) {
        SyntaxNode *arg = parse_expression(ctx);
        if (!arg) {
            return;
        }
        if (!last_arg) {
            node->arguments.argument = arg;
        } else {
            last_arg->next = arg;
        }
        last_arg = arg;
        token = lexer_lex(ctx->lexer);
        if (token_matches(token, TK_SYMBOL, end)) {
            return;
        }
        if (!parser_context_match(ctx, token, TK_SYMBOL, ',')) {
            return;
        }
    }
}

char *Operator_name(Operator op)
{
    switch (op) {
#undef ENUM_BINARY_OPERATOR
#define ENUM_BINARY_OPERATOR(op, a, p, k, c) \
    case OP_##op:                            \
        return #op;
        BINARY_OPERATORS(ENUM_BINARY_OPERATOR)
#undef ENUM_BINARY_OPERATOR
#undef ENUM_UNARY_OPERATOR
#define ENUM_UNARY_OPERATOR(op, k, c) \
    case OP_##op:                     \
        return #op;
        UNARY_OPERATORS(ENUM_UNARY_OPERATOR)
#undef ENUM_UNARY_OPERATOR
    default:
        fatal("Unknown Operator '{}'", (int) op);
    }
}

static OperatorMapping s_operator_mapping[] = {
#undef ENUM_BINARY_OPERATOR
#define ENUM_BINARY_OPERATOR(op, a, p, k, c) { OP_##op, true, k, (TokenCode) c, p },
    BINARY_OPERATORS(ENUM_BINARY_OPERATOR)
#undef ENUM_BINARY_OPERATOR
#undef ENUM_UNARY_OPERATOR
#define ENUM_UNARY_OPERATOR(op, k, c) { OP_##op, false, k, (TokenCode) c, -1 },
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

bool skip_semicolon(ParserContext *ctx)
{
    return parser_context_expect_and_discard(ctx, TK_SYMBOL, ';');
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
    SyntaxNode *ret = parse_expression_1(ctx, primary, 0);
    Token       token = lexer_next(ctx->lexer);
    if (token_matches(token, TK_SYMBOL, ')')) {
        return ret;
    }
    if (token_matches(token, TK_SYMBOL, '?')) {
        lexer_lex(ctx->lexer);
        SyntaxNode *if_true = parse_expression(ctx);
        if (!if_true) {
            return NULL;
        }
        parser_context_expect_and_discard(ctx, TK_SYMBOL, ':');
        SyntaxNode *if_false = parse_expression(ctx);
        if (!if_false) {
            return NULL;
        }
        SyntaxNode *ternary = syntax_node_make(SNT_TERNARYEXPRESSION, sv_from(""), primary->token);
        ternary->ternary_expr.condition = ret;
        ternary->ternary_expr.if_true = if_true;
        ternary->ternary_expr.if_false = if_false;
        return ternary;
    }
    return ret;
}

SyntaxNode *parse_expression_1(ParserContext *ctx, SyntaxNode *lhs, int min_precedence)
{
    Token           lookahead = lexer_next(ctx->lexer);
    OperatorMapping op = operator_for_token(lookahead, true);
    while (op.binary && op.precedence >= min_precedence) {
        lexer_lex(ctx->lexer);

        SyntaxNode *rhs = parse_primary_expression(ctx);
        int         prec = op.precedence;

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
            parser_context_expect_and_discard(ctx, TK_SYMBOL, ']');
        }
        SyntaxNode *expr = syntax_node_make(SNT_BINARYEXPRESSION, sv_from(""), lhs->token);
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
        Token next = lexer_next(ctx->lexer);
        if (token_matches(next, TK_SYMBOL, '(')) {
            SyntaxNode *call = syntax_node_make(SNT_FUNCTION_CALL, token.text, token);
            parse_arguments(ctx, call, '(', ')');
            return call;
        } else {
            StringBuilder sb = sb_create();
            SyntaxNode   *var = syntax_node_make(SNT_VARIABLE, token.text, token);
            SyntaxNode  **name_part = &var->variable.names;
            while (true) {
                *name_part = syntax_node_make(SNT_NAME, token.text, token);
                sb_append_sv(&sb, token.text);
                name_part = &(*name_part)->next;
                if (!lexer_next_matches(ctx->lexer, TK_SYMBOL, '.')) {
                    var->name = sb.view;
                    return var;
                }
                lexer_lex(ctx->lexer);
                if (!parser_context_expect(ctx, TK_IDENTIFIER, TC_IDENTIFIER)) {
                    var->name = sb.view;
                    return var;
                }
                token = lexer_lex(ctx->lexer);
                sb_append_cstr(&sb, ".");
            }
        }
    }
    case TK_NUMBER: {
        lexer_lex(ctx->lexer);
        switch (token.code) {
        case TC_INTEGER: {
            SyntaxNode *ret = syntax_node_make(SNT_INTEGER, token.text, token);
            ret->integer.un_signed = false;
            ret->integer.width = 32;
            Token type = lexer_next(ctx->lexer);
            if (token_matches_kind(type, TK_IDENTIFIER)) {
                if (sv_eq_cstr(type.text, "u8") || sv_eq_cstr(type.text, "i8") || sv_eq_cstr(type.text, "u16") || sv_eq_cstr(type.text, "i16") || sv_eq_cstr(type.text, "u32") || sv_eq_cstr(type.text, "i32") || sv_eq_cstr(type.text, "u64") || sv_eq_cstr(type.text, "i64")) {
                    lexer_lex(ctx->lexer);
                    ret->integer.un_signed = type.text.ptr[0] == 'u';
                    if (type.text.ptr[1] == '8') {
                        ret->integer.width = 8;
                    } else if (type.text.ptr[1] == '1') {
                        ret->integer.width = 16;
                    } else if (type.text.ptr[1] == '3') {
                        ret->integer.width = 32;
                    } else {
                        ret->integer.width = 64;
                    }
                }
            }
            return ret;
        }
        case TC_HEXNUMBER: {
            SyntaxNode *ret = syntax_node_make(SNT_INTEGER, token.text, token);
            ret->integer.un_signed = true;
            ret->integer.width = 4 * (token.text.length - 2);
            if (ret->integer.width % 8) {
                ret->integer.width += 8 - (ret->integer.width % 8);
            }
            return ret;
        }
        case TC_DECIMAL: {
            return syntax_node_make(SNT_DECIMAL, token.text, token);
        }
        default:
            UNREACHABLE();
        }
    }
    case TK_QUOTED_STRING:
        switch (token.code) {
        case TC_DOUBLE_QUOTED_STRING:
            lexer_lex(ctx->lexer);
            return syntax_node_make(SNT_STRING, sv_decode_quoted_str(token.text), token);
        default:
            return NULL;
        }
    case TK_KEYWORD:
        switch (token.code) {
        case KW_TRUE:
        case KW_FALSE:
            lexer_lex(ctx->lexer);
            return syntax_node_make(SNT_BOOL, token.text, token);
        default:
            return NULL;
        }
    case TK_SYMBOL:
        switch (token.code) {
        case '(': {
            lexer_lex(ctx->lexer);
            SyntaxNode *ret = parse_expression(ctx);
            parser_context_expect_and_discard(ctx, TK_SYMBOL, ')');
            return ret;
        }
        default: {
            OperatorMapping op = operator_for_token(token, false);
            if (op.operator!= OP_INVALID) {
                lexer_lex(ctx->lexer);
                SyntaxNode *operand = parse_expression(ctx);
                if (operand) {
                    SyntaxNode *unary_expr = syntax_node_make(SNT_UNARYEXPRESSION, token.text, token);
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

SyntaxNode *parse_variable_declaration(ParserContext *ctx, bool is_const)
{
    Token       var = lexer_lex(ctx->lexer);
    Token       ident = lexer_lex(ctx->lexer);
    Token       t = { 0 };
    Token       token;
    SyntaxNode *type = NULL;
    SyntaxNode *expr = NULL;

    if (!parser_context_match(ctx, ident, TK_IDENTIFIER, TC_IDENTIFIER)) {
        return NULL;
    }
    token = lexer_next(ctx->lexer);
    if (token_matches(token, TK_SYMBOL, ':')) {
        lexer_lex(ctx->lexer);
        type = parse_type(ctx);
    }
    token = lexer_next(ctx->lexer);
    if (token_matches(token, TK_SYMBOL, '=')) {
        lexer_lex(ctx->lexer);
        token = lexer_next(ctx->lexer);
        if (token_matches(token, TK_SYMBOL, '{')) {
            expr = syntax_node_make(SNT_COMPOUND_INITIALIZER, ident.text, t);
            parse_arguments(ctx, expr, '{', '}');
        } else {
            expr = parse_expression(ctx);
        }
    } else if (is_const) {
        parser_context_add_error(ctx, token, sv_from("'const' declaration without initializer expression"));
        return NULL;
    }
    SyntaxNode *ret = syntax_node_make(SNT_VARIABLE_DECL, ident.text, var);
    ret->variable_decl.var_type = type;
    ret->variable_decl.init_expr = expr;
    ret->variable_decl.is_const = is_const;
    if (skip_semicolon(ctx)) {
        return ret;
    }
    return NULL;
}

SyntaxNode *parse_if(ParserContext *ctx)
{
    Token       token = lexer_lex(ctx->lexer);
    SyntaxNode *expr = parse_expression(ctx);
    if (!expr) {
        parser_context_add_error(ctx, token, sv_from("Expected condition in 'if' statement"));
        return NULL;
    }
    SyntaxNode *if_true = parse_statement(ctx);
    if (!if_true) {
        parser_context_add_error(ctx, token, sv_from("Expected 'true' branch for 'if' statement"));
        return NULL;
    }
    SyntaxNode *if_false = NULL;
    token = lexer_next(ctx->lexer);
    if (token_matches(token, TK_KEYWORD, KW_ELSE)) {
        lexer_lex(ctx->lexer);
        if_false = parse_statement(ctx);
    }
    SyntaxNode *ret = syntax_node_make(SNT_IF, sv_from("if"), token);
    ret->if_statement.condition = expr;
    ret->if_statement.if_true = if_true;
    ret->if_statement.if_false = if_false;
    return ret;
}

SyntaxNode *parse_for(ParserContext *ctx)
{
    Token token = lexer_lex(ctx->lexer);
    if (!parser_context_expect(ctx, TK_IDENTIFIER, TC_IDENTIFIER)) {
        return NULL;
    }
    Token variable = lexer_lex(ctx->lexer);
    if (!parser_context_expect(ctx, TK_KEYWORD, (TokenCode) KW_IN)) {
        return NULL;
    }
    lexer_lex(ctx->lexer);
    SyntaxNode *range = parse_expression(ctx);
    if (!range) {
        parser_context_add_error(ctx, token, sv_from("Expected range expression in 'for' statement"));
        return NULL;
    }
    SyntaxNode *stmt = parse_statement(ctx);
    if (!stmt) {
        parser_context_add_error(ctx, token, sv_from("Expected statement for 'for' loop"));
        return NULL;
    }
    SyntaxNode *ret = syntax_node_make(SNT_FOR, sv_from("$for"), token);
    ret->for_statement.variable = variable.text;
    ret->for_statement.range = range;
    ret->for_statement.statement = stmt;
    return ret;
}

SyntaxNode *parse_return(ParserContext *ctx)
{
    Token token;
    lexer_lex(ctx->lexer);
    token = lexer_next(ctx->lexer);
    SyntaxNode *expr = NULL;
    if (token_matches(token, TK_SYMBOL, ';')) {
        lexer_lex(ctx->lexer);
    } else {
        expr = parse_expression(ctx);
        if (!skip_semicolon(ctx)) {
            return NULL;
        }
    }
    SyntaxNode *ret = syntax_node_make(SNT_RETURN, sv_from("return"), token);
    ret->return_stmt.expression = expr;
    return ret;
}

SyntaxNode *parse_while(ParserContext *ctx)
{
    Token       token = lexer_lex(ctx->lexer);
    SyntaxNode *expr = parse_expression(ctx);
    if (!expr) {
        parser_context_add_error(ctx, token, sv_from("Expected condition in 'while' statement"));
        return NULL;
    }
    SyntaxNode *stmt = parse_statement(ctx);
    if (!stmt) {
        parser_context_add_error(ctx, token, sv_from("Expected statement for 'while' loop"));
        return NULL;
    }
    SyntaxNode *ret = syntax_node_make(SNT_WHILE, sv_from("$while"), token);
    ret->while_statement.condition = expr;
    ret->while_statement.statement = stmt;
    return ret;
}

SyntaxNode *parse_loop(ParserContext *ctx)
{
    Token       token = lexer_lex(ctx->lexer);
    SyntaxNode *stmt = parse_statement(ctx);
    if (!stmt) {
        parser_context_add_error(ctx, token, sv_from("Expected statement for loop"));
        return NULL;
    }
    SyntaxNode *ret = syntax_node_make(SNT_LOOP, sv_from("$loop"), token);
    ret->block.statements = stmt;
    return ret;
}

SyntaxNode *parse_block(ParserContext *ctx)
{
    Token        token = lexer_lex(ctx->lexer);
    SyntaxNode  *ret = syntax_node_make(SNT_BLOCK, sv_from("block"), token);
    SyntaxNode **dst = &ret->block.statements;
    while (true) {
        token = lexer_next(ctx->lexer);
        if (token_matches(token, TK_SYMBOL, '}')) {
            lexer_lex(ctx->lexer);
            return ret;
        }
        if (token.kind == TK_END_OF_FILE) {
            parser_context_add_error(ctx, token, sv_from("Expected '}' to close block"));
            return NULL;
        }
        SyntaxNode *stmt = parse_statement(ctx);
        if (stmt != NULL) {
            *dst = stmt;
            dst = &stmt->next;
        }
    }
}

SyntaxNode *parse_identifier(ParserContext *ctx)
{
    Token       token = lexer_lex(ctx->lexer);
    StringView  name = token.text;
    Token       t = lexer_next(ctx->lexer);
    SyntaxNode *ret;
    switch (t.kind) {
    case TK_SYMBOL:
        switch (t.code) {
        case '=': {
            lexer_lex(ctx->lexer);
            SyntaxNode *expression;
            t = lexer_next(ctx->lexer);
            if (token_matches(t, TK_SYMBOL, '{')) {
                expression = syntax_node_make(SNT_COMPOUND_INITIALIZER, name, t);
                parse_arguments(ctx, expression, '{', '}');
            } else {
                expression = parse_expression(ctx);
            }
            ret = syntax_node_make(SNT_ASSIGNMENT, name, token);
            ret->assignment.expression = expression;
        } break;
        case '(': {
            ret = syntax_node_make(SNT_PROCEDURE_CALL, name, token);
            parse_arguments(ctx, ret, '(', ')');
        } break;
        case ':': {
            ret = syntax_node_make(SNT_LABEL, name, token);
            lexer_lex(ctx->lexer);
            return ret;
        }
        default:
            parser_context_add_error(ctx, token, sv_from("Expected '=', '(', or ':' after identifier"));
            return NULL;
        }
        break;
    default:
        parser_context_add_error(ctx, token, sv_from("Expected '=', '(', or ':' after identifier"));
        return NULL;
    }
    if (!skip_semicolon(ctx)) {
        return NULL;
    }
    return ret;
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
            parser_context_add_error(ctx, token, sv_printf("Unexpected symbol '%c'", (char) token.code));
            return NULL;
        }
    }
    case TK_KEYWORD: {
        switch (token.code) {
        case KW_BREAK:
        case KW_CONTINUE: {
            Token stmt_token = lexer_lex(ctx->lexer);
            if (!parser_context_expect(ctx, TK_IDENTIFIER, TC_IDENTIFIER)) {
                return NULL;
            }
            token = lexer_next(ctx->lexer);
            ret = syntax_node_make((stmt_token.code == KW_BREAK) ? SNT_BREAK : SNT_CONTINUE, token.text, stmt_token);
            if (!skip_semicolon(ctx)) {
                return NULL;
            }
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
        parser_context_add_error(ctx, token, sv_printf("Unexpected token '%.*s'", SV_ARG(token.text)));
        return NULL;
    }
    return ret;
}

bool parse_type_descr(ParserContext *ctx, TypeDescr *target)
{
    Token token = lexer_lex(ctx->lexer);
    target->name = token.text;
    token = lexer_next(ctx->lexer);
    if (token_matches(token, TK_SYMBOL, '<')) {
        while (true) {
            lexer_lex(ctx->lexer);
            token = lexer_next(ctx->lexer);
            if (!token_matches(token, TK_IDENTIFIER, TC_IDENTIFIER)) {
                return false;
            }
            TypeDescr *component = allocate_new(TypeDescr);
            DIA_APPEND(TypeDescr *, target, component)
            if (!parse_type_descr(ctx, component)) {
                return false;
            }
            token = lexer_next(ctx->lexer);
            if (token_matches(token, TK_SYMBOL, '>')) {
                lexer_lex(ctx->lexer);
                return true;
            }
            if (!token_matches(token, TK_SYMBOL, ',')) {
                return false;
            }
        }
    }
    return true;
}

SyntaxNode *parse_type(ParserContext *ctx)
{
    if (!parser_context_expect(ctx, TK_IDENTIFIER, TC_IDENTIFIER)) {
        return NULL;
    }
    Token       token = lexer_next(ctx->lexer);
    SyntaxNode *ret = syntax_node_make(SNT_TYPE, token.text, token);
    if (!parse_type_descr(ctx, &ret->type_descr)) {
        return NULL;
    }
    return ret;
}

SyntaxNode *parse_param(ParserContext *ctx, StringView name)
{
    Token token = lexer_lex(ctx->lexer);
    if (!parser_context_expect_and_discard(ctx, TK_SYMBOL, ':')) {
        return NULL;
    }
    SyntaxNode *type = parse_type(ctx);
    if (!type) {
        return NULL;
    }
    SyntaxNode *param = syntax_node_make(SNT_PARAMETER, name, token);
    param->parameter.parameter_type = type;
    return param;
}

SyntaxNode *parse_parameters(ParserContext *ctx, SyntaxNode *func)
{
    if (!parser_context_expect_and_discard(ctx, TK_SYMBOL, '(')) {
        return NULL;
    }
    SyntaxNode *last_param = NULL;
    while (true) {
        Token token = lexer_next(ctx->lexer);
        switch (token.kind) {
        case TK_IDENTIFIER: {
            SyntaxNode *param = parse_param(ctx, token.text);
            if (!param) {
                return NULL;
            }
            if (!last_param) {
                func->function.parameter = param;
            } else {
                last_param->next = param;
            }
            last_param = param;
        } break;
        case TK_SYMBOL: {
            switch (token.code) {
            case ')':
                lexer_lex(ctx->lexer);
                return func;
            case ',':
                if (last_param == NULL) {
                    parser_context_add_error(ctx, token, sv_from("Expected parameter or ')'"));
                    return NULL;
                }
                lexer_lex(ctx->lexer);
                break;
            default:
                if (last_param == NULL) {
                    parser_context_add_error(ctx, token, sv_from("Expected parameter or ')'"));
                    return NULL;
                } else {
                    parser_context_add_error(ctx, token, sv_from("Expected ',' or ')' in parameter list"));
                    return NULL;
                }
            }
        } break;
        default:
            parser_context_add_error(ctx, token, sv_from("Expected ',' or ')' in parameter list"));
            return NULL;
        }
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
    Token token = lexer_next(ctx->lexer);
    if (!token_matches(token, TK_SYMBOL, '/')) {
        return func;
    }
    lexer_lex(ctx->lexer);
    func->function.error_type = parse_type(ctx);
    if (!func->function.error_type) {
        return NULL;
    }
    return func;
}

SyntaxNode *parse_function_decl(ParserContext *ctx)
{
    lexer_lex(ctx->lexer);
    if (!parser_context_expect(ctx, TK_IDENTIFIER, TC_IDENTIFIER)) {
        return NULL;
    }
    Token       token = lexer_lex(ctx->lexer);
    SyntaxNode *func = syntax_node_make(SNT_FUNCTION, token.text, token);
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
    Token token = lexer_lex(ctx->lexer);
    if (token_matches(token, TK_SYMBOL, '{')) {
        SyntaxNode *impl = syntax_node_make(SNT_FUNCTION_IMPL, func->name, token);
        func->function.function_impl = impl;
        SyntaxNode *last_stmt = NULL;
        while (true) {
            token = lexer_next(ctx->lexer);
            if (token_matches(token, TK_SYMBOL, '}')) {
                lexer_lex(ctx->lexer);
                return func;
            }
            if (token_matches_kind(token, TK_END_OF_FILE)) {
                parser_context_add_error(ctx, token, sv_from("Expected '}' to end function definition"));
                return NULL;
            }
            SyntaxNode *stmt = parse_statement(ctx);
            if (!stmt) {
                return NULL;
            }
            if (last_stmt == NULL) {
                impl->function_impl.statements = stmt;
            } else {
                last_stmt->next = stmt;
            }
            last_stmt = stmt;
        }
    } else if (token_matches(token, TK_KEYWORD, KW_FUNC_BINDING)) {
        if (!parser_context_expect(ctx, TK_QUOTED_STRING, TC_DOUBLE_QUOTED_STRING)) {
            return NULL;
        }
        token = lexer_lex(ctx->lexer);
        if (!skip_semicolon(ctx)) {
            return NULL;
        }
        func->function.function_impl = syntax_node_make(
            SNT_NATIVE_FUNCTION,
            (StringView) { token.text.ptr + 1, token.text.length - 2 },
            token);
        return func;
    } else if (token_matches(token, TK_KEYWORD, KW_MACRO_BINDING)) {
        if (!parser_context_expect(ctx, TK_QUOTED_STRING, TC_DOUBLE_QUOTED_STRING)) {
            return NULL;
        }
        token = lexer_lex(ctx->lexer);
        if (!skip_semicolon(ctx)) {
            return NULL;
        }
        func->function.function_impl = syntax_node_make(
            SNT_MACRO,
            (StringView) { token.text.ptr + 1, token.text.length - 2 },
            token);
        return func;
    } else {
        parser_context_add_error(ctx, token, sv_from("Expected '{' or '->' after function declaration"));
        return NULL;
    }
}

SyntaxNode *parse_enum_def(ParserContext *ctx)
{
    lexer_lex(ctx->lexer);
    Token ident = lexer_next(ctx->lexer);
    if (!token_matches(ident, TK_IDENTIFIER, TC_IDENTIFIER)) {
        parser_context_add_error(ctx, ident, sv_from("Expected 'struct' to be followed by type name"));
        return NULL;
    }
    lexer_lex(ctx->lexer);
    SyntaxNode *underlying_type = NULL;
    if (parser_context_accept_and_discard(ctx, TK_SYMBOL, ':')) {
        underlying_type = parse_type(ctx);
    }
    if (!parser_context_expect_and_discard(ctx, TK_SYMBOL, '{')) {
        return NULL;
    }
    SyntaxNode *enum_node = syntax_node_make(SNT_ENUMERATION, ident.text, ident);
    enum_node->enumeration.underlying_type = underlying_type;
    SyntaxNode **value = &enum_node->enumeration.values;
    while (true) {
        if (!parser_context_expect(ctx, TK_IDENTIFIER, TC_IDENTIFIER)) {
            return NULL;
        }
        Token value_name = lexer_lex(ctx->lexer);
        *value = syntax_node_make(SNT_ENUM_VALUE, value_name.text, value_name);
        SyntaxNode *underlying_value = NULL;
        if (parser_context_accept_and_discard(ctx, TK_SYMBOL, '=')) {
            underlying_value = parse_expression(ctx);
        }
        (*value)->enum_value.underlying_value = underlying_value;
        if (parser_context_accept_and_discard(ctx, TK_SYMBOL, '}')) {
            break;
        }
        if (!parser_context_expect_and_discard(ctx, TK_SYMBOL, ',')) {
            break;
        }
        value = &(*value)->next;
    }
    if (!skip_semicolon(ctx)) {
        return NULL;
    }
    return enum_node;
}

SyntaxNode *parse_struct_def(ParserContext *ctx)
{
    lexer_lex(ctx->lexer);
    Token ident = lexer_next(ctx->lexer);
    if (!token_matches(ident, TK_IDENTIFIER, TC_IDENTIFIER)) {
        parser_context_add_error(ctx, ident, sv_from("Expected 'struct' to be followed by type name"));
        return NULL;
    }
    lexer_lex(ctx->lexer);
    if (!parser_context_expect_and_discard(ctx, TK_SYMBOL, '{')) {
        return NULL;
    }
    SyntaxNode *strukt = syntax_node_make(SNT_STRUCT, ident.text, ident);
    SyntaxNode *last_component = NULL;
    while (true) {
        Token comp = lexer_lex(ctx->lexer);
        switch (comp.kind) {
        case TK_IDENTIFIER: {
            if (!parser_context_expect_and_discard(ctx, TK_SYMBOL, ':')) {
                return NULL;
            }
            SyntaxNode *component = syntax_node_make(SNT_TYPE_COMPONENT, comp.text, comp);
            component->parameter.parameter_type = parse_type(ctx);
            if (component->parameter.parameter_type == NULL) {
                return NULL;
            }
            if (!last_component) {
                strukt->struct_def.components = component;
            } else {
                last_component->next = component;
            }
            last_component = component;
        } break;
        case TK_SYMBOL: {
            switch (comp.code) {
            case '}':
                return strukt;
            case ';':
                if (last_component == NULL) {
                    parser_context_add_error(ctx, ident, sv_from("Expected struct component or '}'"));
                    return NULL;
                }
                break;
            default:
                parser_context_add_error(ctx, ident, sv_from("Expected struct component or '}'"));
                return NULL;
            }
        } break;
        default:
            parser_context_add_error(ctx, ident, sv_from("Expected struct component or '}'"));
            return NULL;
        }
    }
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
        parser_context_add_error(ctx, token, sv_printf("Could not find import '%.*s'", SV_ARG(path)));
        return NULL;
    }
    SyntaxNode *import = syntax_node_make(SNT_IMPORT, name, token);
    char       *buffer = MUST(Char, read_file_by_name(sv_cstr(file_name)));
    SyntaxNode *module = parse_module(ctx, sv_from(buffer), name);
    import->next = ctx->program->program.imports;
    ctx->program->program.imports = import;
    module->next = import->import.modules;
    import->import.modules = module;
    return import;
}

SyntaxNode *parse_import(ParserContext *ctx)
{
    Token token = lexer_lex(ctx->lexer);
    if (!parser_context_expect(ctx, TK_QUOTED_STRING, TC_DOUBLE_QUOTED_STRING)) {
        return NULL;
    }
    token = lexer_lex(ctx->lexer);
    StringView path = sv_decode_quoted_str(token.text);
    if (!skip_semicolon(ctx)) {
        return NULL;
    }
    return import_package(ctx, token, path);
}

SyntaxNode *parse_module(ParserContext *ctx, StringView buffer, StringView name)
{
    Token       token = { name, TK_MODULE, TC_NONE };
    SyntaxNode *module = syntax_node_make(SNT_MODULE, name, token);
    Lexer       lexer = { 0 };

    printf("Compiling '" SV_SPEC "'\n", SV_ARG(name));
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
        } else if (token_matches(token, TK_KEYWORD, KW_IMPORT)) {
            statement = parse_import(ctx);
        } else if (token_matches(token, TK_KEYWORD, KW_CONST)) {
            statement = parse_variable_declaration(ctx, true);
        } else if (token_matches(token, TK_KEYWORD, KW_VAR)) {
            statement = parse_variable_declaration(ctx, false);
        } else if (token_matches_kind(token, TK_END_OF_FILE)) {
            return module;
        } else {
            parser_context_add_error(ctx, token, sv_printf("Only 'import', 'func', 'var', 'const', and 'struct' are allowed on the top level of files, '%.*s' is not", SV_ARG(token.text)));
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
    char *buffer = MUST(Char, read_file_at(dir_fd, file));
    return parse_module(ctx, sv_from(buffer), fn_barename(sv_copy_cstr(file)));
}

ParserContext parse(char const *dir_or_file)
{
    if (OPT_TRACE) {
        char cwd[256];
        getcwd(cwd, 256);
        trace(CAT_PARSE, "CWD: %s dir: %s", cwd, dir_or_file);
    }
    ParserContext ret = { 0 };
    ret.source_name = sv_copy_cstr(dir_or_file);
    Token token = { ret.source_name, TK_PROGRAM, TC_NONE };
    ret.program = syntax_node_make(SNT_PROGRAM, fn_barename(ret.source_name), token);
    import_package(&ret, token, sv_from("std"));

    DIR *dir = opendir(dir_or_file);
    if (dir == NULL) {
        if (errno == ENOTDIR) {
            dir = opendir(".");
            if (dir == NULL) {
                fatal("Could not open current directory");
            }
            SyntaxNode *module = parse_module_file(&ret, dirfd(dir), dir_or_file);
            module->next = ret.program->program.modules;
            ret.program->program.modules = module;
            closedir(dir);
            return ret;
        }
        fatal("Could not open directory '%s'", dir_or_file);
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
            if (module) {
                module->next = ret.program->program.modules;
                ret.program->program.modules = module;
            }
        }
    }
    closedir(dir);
    return ret;
}
