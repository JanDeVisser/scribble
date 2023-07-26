/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <dirent.h>
#include <string.h>

#include "error.h"
#include "io.h"
#include "lexer.h"
#include "log.h"
#include "mem.h"
#include "parser.h"

static SyntaxNode *syntax_node_make(SyntaxNodeType type, StringView name, Token token);
static SyntaxNode *parse_expression(Lexer *lexer);
static SyntaxNode *parse_expression_1(Lexer *lexer, SyntaxNode *lhs, int min_precedence);
static SyntaxNode *parse_primary_expression(Lexer *lexer);
static SyntaxNode *parse_type(Lexer *lexer);

char const *SyntaxNodeType_name(SyntaxNodeType type)
{
    switch (type) {
#undef SYNTAXNODETYPE_ENUM
#define SYNTAXNODETYPE_ENUM(type) \
    case type:                    \
        return #type;
        SYNTAXNODETYPES(SYNTAXNODETYPE_ENUM)
#undef SYNTAXNODETYPE_ENUM
    default:
        UNREACHABLE();
    }
}

size_t next_counter()
{
    static size_t counter = 0;
    return counter++;
}

SyntaxNode *syntax_node_make(SyntaxNodeType type, StringView name, Token token)
{
    SyntaxNode *node = (SyntaxNode *) mem_allocate(sizeof(SyntaxNode));
    node->type = type;
    node->name = name;
    node->next = NULL;
    node->index = next_counter();
    node->token = token;
    return node;
}

void parse_arguments(Lexer *lexer, SyntaxNode *call)
{
    Token token = lexer_lex(lexer);
    if (!token_matches(token, TK_SYMBOL, '(')) {
        fatal("Expected '(' in call to '" SV_SPEC "'", SV_ARG(call->name));
    }
    token = lexer_next(lexer);
    if (token_matches(token, TK_SYMBOL, ')')) {
        lexer_lex(lexer);
        return;
    }
    SyntaxNode *last_arg = NULL;
    while (true) {
        SyntaxNode *arg = parse_expression(lexer);
        if (!last_arg) {
            call->call.argument = arg;
        } else {
            last_arg->next = arg;
        }
        last_arg = arg;
        token = lexer_lex(lexer);
        if (token_matches(token, TK_SYMBOL, ')')) {
            return;
        }
        if (!token_matches(token, TK_SYMBOL, ',')) {
            fatal("Expected ',' between arguments of function '" SV_SPEC "'", SV_ARG(call->name));
        }
    }
}

char *Operator_name(Operator op)
{
    switch (op) {
#undef ENUM_BINARY_OPERATOR
#define ENUM_BINARY_OPERATOR(op, a, p, k, c) \
    case op:                                 \
        return #op;
        BINARY_OPERATORS(ENUM_BINARY_OPERATOR)
#undef ENUM_BINARY_OPERATOR
#undef ENUM_UNARY_OPERATOR
#define ENUM_UNARY_OPERATOR(op, k, c) \
    case op:                          \
        return #op;
        UNARY_OPERATORS(ENUM_UNARY_OPERATOR)
#undef ENUM_UNARY_OPERATOR
    default:
        fatal("Unknown Operator '{}'", (int) op);
    }
}

static OperatorMapping s_operator_mapping[] = {
#undef ENUM_BINARY_OPERATOR
#define ENUM_BINARY_OPERATOR(op, a, p, k, c) { op, true, k, (TokenCode) c, p },
    BINARY_OPERATORS(ENUM_BINARY_OPERATOR)
#undef ENUM_BINARY_OPERATOR
#undef ENUM_UNARY_OPERATOR
#define ENUM_UNARY_OPERATOR(op, k, c) { op, false, k, (TokenCode) c, -1 },
        UNARY_OPERATORS(ENUM_UNARY_OPERATOR)
#undef ENUM_BINARY_OPERATOR
            { OP_COUNT, false, TK_UNKNOWN, TC_NONE, -1 }
};

OperatorMapping operator_for_token(Token token)
{
    for (int ix = 0; s_operator_mapping[ix].operator!= OP_COUNT; ++ix) {
        if (token_matches(token, s_operator_mapping[ix].token_kind, s_operator_mapping[ix].token_code)) {
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
SyntaxNode *parse_expression(Lexer *lexer)
{
    SyntaxNode *primary = parse_primary_expression(lexer);
    if (!primary)
        return NULL;
    return parse_expression_1(lexer, primary, 0);
}

SyntaxNode *parse_expression_1(Lexer *lexer, SyntaxNode *lhs, int min_precedence)
{
    Token           lookahead = lexer_next(lexer);
    OperatorMapping op = operator_for_token(lookahead);
    Token           t;
    while (op.binary && op.precedence >= min_precedence) {
        lexer_lex(lexer);
        t = token_merge(lhs->token, lookahead);

        SyntaxNode *rhs = parse_primary_expression(lexer);
        int         prec = op.precedence;

        lookahead = lexer_next(lexer);
        OperatorMapping op_1 = operator_for_token(lookahead);
        while (op_1.binary && op_1.precedence > prec) {
            rhs = parse_expression_1(lexer, rhs, prec + 1);
            lookahead = lexer_next(lexer);
            op_1 = operator_for_token(lookahead);
        }
        t = token_merge(t, rhs->token);
        SyntaxNode *expr = syntax_node_make(SNT_BINARYEXPRESSION, sv_from(""), t);
        expr->binary_expr.lhs = lhs;
        expr->binary_expr.rhs = rhs;
        expr->binary_expr.operator= op.operator;
        lhs = expr;
        lookahead = lexer_next(lexer);
        op = operator_for_token(lookahead);
    }
    return lhs;
}

SyntaxNode *parse_primary_expression(Lexer *lexer)
{
    Token token = lexer_next(lexer);
    switch (token.kind) {
    case TK_IDENTIFIER: {
        lexer_lex(lexer);
        Token next = lexer_next(lexer);
        if (token_matches(next, TK_SYMBOL, '(')) {
            SyntaxNode *call = syntax_node_make(SNT_FUNCTION_CALL, token.text, token);
            parse_arguments(lexer, call);
            return call;
        } else {
            return syntax_node_make(SNT_VARIABLE, token.text, token);
        }
    }
    case TK_NUMBER:
        lexer_lex(lexer);
        return syntax_node_make(SNT_NUMBER, token.text, token);
    default:
        return NULL;
    }
}

SyntaxNode *parse_variable_declaration(Lexer *lexer, bool is_const)
{
    Token       var = lexer_lex(lexer);
    Token       ident = lexer_lex(lexer);
    Token       t = { 0 };
    Token       token;
    SyntaxNode *type = NULL;
    SyntaxNode *expr = NULL;

    if (ident.kind != TK_IDENTIFIER) {
        fatal("Expected variable name after 'var' keyword");
    }
    t = token_merge(var, ident);
    token = lexer_next(lexer);
    if (token_matches(token, TK_SYMBOL, ':')) {
        lexer_lex(lexer);
        type = parse_type(lexer);
        t = token_merge(t, type->token);
    }
    token = lexer_next(lexer);
    if (token_matches(token, TK_SYMBOL, '=')) {
        lexer_lex(lexer);
        expr = parse_expression(lexer);
        t = token_merge(t, expr->token);
    } else if (is_const) {
        fatal("'const' declaration without initializer expression");
    }
    SyntaxNode *ret = syntax_node_make(SNT_VARIABLE_DECL, ident.text, t);
    ret->variable_decl.var_type = type;
    ret->variable_decl.init_expr = expr;
    ret->variable_decl.is_const = is_const;
    return ret;
}

SyntaxNode *parse_statement(Lexer *lexer)
{
    Token token = lexer_next(lexer);
    if (token.kind == TK_KEYWORD) {
        switch (token.code) {
        case KW_VAR:
            return parse_variable_declaration(lexer, false);
        case KW_CONST:
            return parse_variable_declaration(lexer, true);
        default:
            NYI("Keywords");
        }
    }
    return parse_expression(lexer);
}

SyntaxNode *parse_type(Lexer *lexer)
{
    Token token = lexer_lex(lexer);
    if (token.kind != TK_IDENTIFIER) {
        fatal("Expected type name");
    }
    return syntax_node_make(SNT_TYPE, token.text, token);
}

SyntaxNode *parse_param(Lexer *lexer, StringView name)
{
    Token token = lexer_lex(lexer);
    if (token.kind != TK_SYMBOL || token.code != ':') {
        fatal("Expected ':' after parameter '" SV_SPEC "'", SV_ARG(name));
    }
    SyntaxNode *param = syntax_node_make(SNT_PARAMETER, name, token);
    param->parameter.parameter_type = parse_type(lexer);
    return param;
}

void parse_parameters(Lexer *lexer, SyntaxNode *func)
{
    Token token = lexer_lex(lexer);
    if (!token_matches(token, TK_SYMBOL, '(')) {
        fatal("Expected '(' in declaration of '" SV_SPEC "'", SV_ARG(func->name));
    }
    SyntaxNode *last_param = NULL;
    while (true) {
        token = lexer_lex(lexer);
        switch (token.kind) {
        case TK_IDENTIFIER: {
            SyntaxNode *param = parse_param(lexer, token.text);
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
                return;
            case ',':
                if (last_param == NULL) {
                    fatal("Expected parameter or ')' in parameter list of '" SV_SPEC "'", SV_ARG(func->name));
                }
                break;
            default:
                if (last_param == NULL) {
                    fatal("Expected parameter or ')' in parameter list of '" SV_SPEC "'", SV_ARG(func->name));
                } else {
                    fatal("Expected ',' or ')' after parameter '" SV_SPEC "' of '" SV_SPEC "'",
                        SV_ARG(last_param->name), SV_ARG(func->name));
                }
            }
        } break;
        default:
            fatal("Expected ',' or ')' in parameter list of '" SV_SPEC "'", SV_ARG(func->name));
        }
    }
}

void parse_return_types(Lexer *lexer, SyntaxNode *func)
{
    Token token = lexer_next(lexer);
    if (token_matches(token, TK_SYMBOL, '{')) {
        return;
    }
    lexer_lex(lexer);
    if (!token_matches(token, TK_KEYWORD, KW_RETURN_TYPES)) {
        fatal("Expect '->' or '{' after parameter list of '" SV_SPEC "'", SV_ARG(func->name));
    }
    func->function.return_type = parse_type(lexer);
    token = lexer_next(lexer);
    if (!token_matches(token, TK_SYMBOL, '/')) {
        return;
    }
    lexer_lex(lexer);
    func->function.error_type = parse_type(lexer);
}

SyntaxNode *parse_function_decl(Lexer *lexer)
{
    lexer_lex(lexer);
    Token token = lexer_lex(lexer);
    if (token.kind != TK_IDENTIFIER) {
        fatal("Expected function name");
    }
    SyntaxNode *func = syntax_node_make(SNT_FUNCTION, token.text, token);
    parse_parameters(lexer, func);
    parse_return_types(lexer, func);
    return func;
}

SyntaxNode *parse_function(Lexer *lexer)
{
    SyntaxNode *func = parse_function_decl(lexer);
    Token       token = lexer_lex(lexer);
    if (!token_matches(token, TK_SYMBOL, '{')) {
        fatal("Expected '{' to start definition of function '" SV_SPEC "'", SV_ARG(func->name));
    }
    SyntaxNode *last_stmt = NULL;
    while (true) {
        token = lexer_next(lexer);
        if (token_matches(token, TK_SYMBOL, '}')) {
            return func;
        }
        if (token_matches_kind(token, TK_END_OF_FILE)) {
            fatal("Expected '}' to end definition of function '" SV_SPEC "'", SV_ARG(func->name));
        }
        SyntaxNode *stmt = parse_statement(lexer);
        if (last_stmt == NULL) {
            func->function.statements = stmt;
        } else {
            last_stmt->next = stmt;
        }
        last_stmt = stmt;
    }
}

SyntaxNode *parse_module(int dir_fd, char const *file)
{
    char       *name_owned = (char *) mem_allocate(strlen(file) + 1);
    Token       token = { 0, sv_from(name_owned), TK_MODULE, TC_NONE };
    SyntaxNode *module = syntax_node_make(SNT_MODULE, sv_from(name_owned), token);
    Lexer       lexer = { 0 };

    MUST(Char, char *, buffer, read_file_at(dir_fd, file))
    strcpy(name_owned, file);
    lexer.buffer = buffer;
    lexer.skip_whitespace = true;
    lexer.tail = sv_from(buffer);
    SyntaxNode *last_statement = NULL;
    do {
        token = lexer_next(&lexer);

        SyntaxNode *statement = NULL;
        if (token_matches(token, TK_KEYWORD, KW_FUNC)) {
            statement = parse_function(&lexer);
        }
        if (statement) {
            if (!last_statement) {
                module->module.statements = statement;
            } else {
                last_statement->next = statement;
            }
            last_statement = statement;
        } else {
            lexer_lex(&lexer);
        }
    } while (token.kind != TK_END_OF_FILE);
    return module;
}

SyntaxNode *parse(char const *dir_name)
{
    DIR *dir = opendir(dir_name);
    if (dir == NULL)
        return NULL;

    Token          token = { 0, sv_from(dir_name), TK_PROGRAM, TC_NONE };
    SyntaxNode    *program = syntax_node_make(SNT_PROGRAM, sv_from(dir_name), token);
    struct dirent *dp;
    SyntaxNode    *last_module = NULL;
    while ((dp = readdir(dir)) != NULL) {
        if ((dp->d_namlen > 8) && strcmp(dp->d_name + (dp->d_namlen - 9), ".scribble") == 0) {
            SyntaxNode *module = parse_module(dirfd(dir), dp->d_name);
            if (!last_module) {
                program->program.modules = module;
            } else {
                last_module->next = module;
            }
            last_module = module;
        }
    }
    closedir(dir);
    return program;
}
