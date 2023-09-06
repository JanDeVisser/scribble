/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <dirent.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>

#include <error.h>
#include <io.h>
#include <lexer.h>
#include <log.h>
#include <options.h>
#include <parser.h>

#define STATIC_ALLOCATOR
#include <allocate.h>

static SyntaxNode *syntax_node_make(SyntaxNodeType type, StringView name, Token token);
static SyntaxNode *parse_expression(Lexer *lexer);
static SyntaxNode *parse_expression_1(Lexer *lexer, SyntaxNode *lhs, int min_precedence);
static SyntaxNode *parse_primary_expression(Lexer *lexer);
static void        parse_arguments(Lexer *lexer, SyntaxNode *node, char start, char end);
static SyntaxNode *parse_statement(Lexer *lexer);
static SyntaxNode *parse_type(Lexer *lexer);

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

void parse_arguments(Lexer *lexer, SyntaxNode *node, char start, char end)
{
    Token token = lexer_lex(lexer);
    if (!token_matches(token, TK_SYMBOL, start)) {
        fatal("Expected '(' in call to '" SV_SPEC "'", SV_ARG(node->name));
    }
    token = lexer_next(lexer);
    if (token_matches(token, TK_SYMBOL, end)) {
        lexer_lex(lexer);
        return;
    }
    SyntaxNode *last_arg = NULL;
    while (true) {
        SyntaxNode *arg = parse_expression(lexer);
        if (!last_arg) {
            node->arguments.argument = arg;
        } else {
            last_arg->next = arg;
        }
        last_arg = arg;
        token = lexer_lex(lexer);
        if (token_matches(token, TK_SYMBOL, end)) {
            return;
        }
        if (!token_matches(token, TK_SYMBOL, ',')) {
            fatal("Expected ',' between arguments of function '" SV_SPEC "'", SV_ARG(node->name));
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

OperatorMapping operator_for_token(Token token)
{
    for (int ix = 0; s_operator_mapping[ix].operator!= OP_COUNT; ++ix) {
        if (token_matches(token, s_operator_mapping[ix].token_kind, s_operator_mapping[ix].token_code)) {
            return s_operator_mapping[ix];
        }
    }
    return s_operator_mapping[0];
}

void skip_semicolon(Lexer *lexer, SyntaxNode *stmt)
{
    Token token = lexer_next(lexer);
    if (!token_matches(token, TK_SYMBOL, ';')) {
        fatal(TOKEN_SPEC "Expected ';' after statement, got " TOKEN_SPEC, TOKEN_ARG(stmt->token), TOKEN_ARG(token));
    }
    lexer_lex(lexer);
}

StringView cleanup_string(StringView str)
{
    assert(sv_length(str) >= 2 && str.ptr[0] == '\"' && str.ptr[sv_length(str) - 1] == '\"');
    int backslashes = 0;
    for (size_t ix = 1; ix < sv_length(str) - 1; ++ix) {
        if (str.ptr[ix] == '\\') {
            ++ix;
            ++backslashes;
        }
    }
    if (!backslashes) {
        StringView ret = { str.ptr + 1, str.length - 2 };
        return ret;
    }
    bool   prev_backslash = false;
    size_t len = sv_length(str) - 2 - backslashes;
    char  *buffer = allocate(len);
    char  *ptr = buffer;
    for (size_t ix = 1; ix < sv_length(str) - 1; ++ix) {
        if (prev_backslash || str.ptr[ix] != '\\') {
            char ch;
            switch (str.ptr[ix]) {
            case 'n':
                ch = '\n';
                break;
            case 't':
                ch = '\t';
                break;
            default:
                ch = str.ptr[ix];
            }
            *ptr++ = ch;
            prev_backslash = false;
        } else if (str.ptr[ix] == '\\') {
            prev_backslash = true;
        }
    }
    StringView ret = { buffer, len };
    return ret;
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
    while (op.binary && op.precedence >= min_precedence) {
        lexer_lex(lexer);

        SyntaxNode *rhs = parse_primary_expression(lexer);
        int         prec = op.precedence;

        lookahead = lexer_next(lexer);
        OperatorMapping op_1 = operator_for_token(lookahead);
        while (op_1.binary && op_1.precedence > prec) {
            rhs = parse_expression_1(lexer, rhs, prec + 1);
            lookahead = lexer_next(lexer);
            op_1 = operator_for_token(lookahead);
        }
        SyntaxNode *expr = syntax_node_make(SNT_BINARYEXPRESSION, sv_from(""), lhs->token);
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
            parse_arguments(lexer, call, '(', ')');
            return call;
        } else {
            return syntax_node_make(SNT_VARIABLE, token.text, token);
        }
    }
    case TK_NUMBER:
        lexer_lex(lexer);
        SyntaxNode *ret = syntax_node_make(SNT_NUMBER, token.text, token);
        ret->number.un_signed = false;
        ret->number.width = 32;
        Token type = lexer_next(lexer);
        if (token_matches_kind(type, TK_IDENTIFIER)) {
            if (sv_eq_cstr(type.text, "u8") || sv_eq_cstr(type.text, "i8") || sv_eq_cstr(type.text, "u16") || sv_eq_cstr(type.text, "i16") || sv_eq_cstr(type.text, "u32") || sv_eq_cstr(type.text, "i32") || sv_eq_cstr(type.text, "u64") || sv_eq_cstr(type.text, "i64")) {
                lexer_lex(lexer);
                ret->number.un_signed = type.text.ptr[0] == 'u';
                if (type.text.ptr[1] == '8') {
                    ret->number.width = 8;
                } else if (type.text.ptr[1] == '1') {
                    ret->number.width = 16;
                } else if (type.text.ptr[1] == '3') {
                    ret->number.width = 32;
                } else {
                    ret->number.width = 64;
                }
            }
        }
        return ret;
    case TK_QUOTED_STRING:
        switch (token.code) {
        case TC_DOUBLE_QUOTED_STRING:
            lexer_lex(lexer);
            return syntax_node_make(SNT_STRING, cleanup_string(token.text), token);
        default:
            return NULL;
        }
    case TK_KEYWORD:
        switch (token.code) {
        case KW_TRUE:
        case KW_FALSE:
            lexer_lex(lexer);
            return syntax_node_make(SNT_BOOL, token.text, token);
        default:
            return NULL;
        }
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
    token = lexer_next(lexer);
    if (token_matches(token, TK_SYMBOL, ':')) {
        lexer_lex(lexer);
        type = parse_type(lexer);
    }
    token = lexer_next(lexer);
    if (token_matches(token, TK_SYMBOL, '=')) {
        lexer_lex(lexer);
        token = lexer_next(lexer);
        if (token_matches(token, TK_SYMBOL, '{')) {
            expr = syntax_node_make(SNT_COMPOUND_INITIALIZER, ident.text, t);
            parse_arguments(lexer, expr, '{', '}');
        } else {
            expr = parse_expression(lexer);
        }
    } else if (is_const) {
        fatal("'const' declaration without initializer expression");
    }
    SyntaxNode *ret = syntax_node_make(SNT_VARIABLE_DECL, ident.text, var);
    ret->variable_decl.var_type = type;
    ret->variable_decl.init_expr = expr;
    ret->variable_decl.is_const = is_const;
    skip_semicolon(lexer, ret);
    return ret;
}

SyntaxNode *parse_if(Lexer *lexer)
{
    Token token;
    lexer_lex(lexer);
    token = lexer_next(lexer);
    if (!token_matches(token, TK_SYMBOL, '(')) {
        fatal("Expected '(' after 'if'");
    }
    lexer_lex(lexer);
    SyntaxNode *expr = parse_expression(lexer);
    if (!expr) {
        fatal("Expected condition in 'if' statement");
    }
    token = lexer_next(lexer);
    if (!token_matches(token, TK_SYMBOL, ')')) {
        fatal("Expected ')' after 'if' condition");
    }
    lexer_lex(lexer);
    SyntaxNode *if_true = parse_statement(lexer);
    SyntaxNode *if_false = NULL;
    token = lexer_next(lexer);
    if (token_matches(token, TK_KEYWORD, KW_ELSE)) {
        lexer_lex(lexer);
        if_false = parse_statement(lexer);
    }
    SyntaxNode *ret = syntax_node_make(SNT_IF, sv_from("if"), token);
    ret->if_statement.condition = expr;
    ret->if_statement.if_true = if_true;
    ret->if_statement.if_false = if_false;
    return ret;
}

SyntaxNode *parse_return(Lexer *lexer)
{
    Token token;
    lexer_lex(lexer);
    token = lexer_next(lexer);
    SyntaxNode *expr = parse_expression(lexer);
    SyntaxNode *ret = syntax_node_make(SNT_RETURN, sv_from("return"), token);
    ret->return_stmt.expression = expr;
    skip_semicolon(lexer, ret);
    return ret;
}

SyntaxNode *parse_while(Lexer *lexer)
{
    Token token;
    lexer_lex(lexer);
    token = lexer_next(lexer);
    if (!token_matches(token, TK_SYMBOL, '(')) {
        fatal("Expected '(' after 'while'");
    }
    lexer_lex(lexer);
    SyntaxNode *expr = parse_expression(lexer);
    if (!expr) {
        fatal("Expected condition in 'while' statement");
    }
    token = lexer_next(lexer);
    if (!token_matches(token, TK_SYMBOL, ')')) {
        fatal("Expected ')' after 'while' condition");
    }
    lexer_lex(lexer);
    SyntaxNode *stmt = parse_statement(lexer);
    SyntaxNode *ret = syntax_node_make(SNT_WHILE, sv_from("while"), token);
    ret->while_statement.condition = expr;
    ret->while_statement.statement = stmt;
    return ret;
}

SyntaxNode *parse_loop(Lexer *lexer)
{
    Token       token = lexer_lex(lexer);
    SyntaxNode *stmt = parse_statement(lexer);
    SyntaxNode *ret = syntax_node_make(SNT_LOOP, sv_from("loop"), token);
    ret->block.statements = stmt;
    return ret;
}

SyntaxNode *parse_block(Lexer *lexer)
{
    Token        token = lexer_lex(lexer);
    SyntaxNode  *ret = syntax_node_make(SNT_BLOCK, sv_from("block"), token);
    SyntaxNode **dst = &ret->block.statements;
    while (true) {
        token = lexer_next(lexer);
        if (token.kind == TK_SYMBOL && token.code == '}') {
            lexer_lex(lexer);
            return ret;
        }
        if (token.kind == TK_END_OF_FILE) {
            fatal("Expected '}' to close block");
        }
        SyntaxNode *stmt = parse_statement(lexer);
        if (stmt != NULL) {
            *dst = stmt;
            dst = &stmt->next;
        }
    }
}

SyntaxNode *parse_assignment_or_call(Lexer *lexer)
{
    Token       token = lexer_lex(lexer);
    StringView  name = token.text;
    Token       t = lexer_next(lexer);
    SyntaxNode *ret;
    switch (t.kind) {
    case TK_SYMBOL:
        switch (t.code) {
        case '=': {
            lexer_lex(lexer);
            SyntaxNode *expression;
            t = lexer_next(lexer);
            if (token_matches(t, TK_SYMBOL, '{')) {
                expression = syntax_node_make(SNT_COMPOUND_INITIALIZER, name, t);
                parse_arguments(lexer, expression, '{', '}');
            } else {
                expression = parse_expression(lexer);
            }
            ret = syntax_node_make(SNT_ASSIGNMENT, name, token);
            ret->assignment.expression = expression;
        } break;
        case '(': {
            ret = syntax_node_make(SNT_FUNCTION_CALL, name, token);
            parse_arguments(lexer, ret, '(', ')');
        } break;
        default:
            fatal("Expected '=' or '(' after identifier '" SV_SPEC "'", SV_ARG(name));
        }
        break;
    default:
        fatal("Expected '=' or '(' after identifier '" SV_SPEC "'", SV_ARG(name));
    }
    skip_semicolon(lexer, ret);
    return ret;
}

SyntaxNode *parse_statement(Lexer *lexer)
{
    Token       token = lexer_next(lexer);
    SyntaxNode *ret;
    switch (token.kind) {
    case TK_SYMBOL: {
        switch (token.code) {
        case '{':
            return parse_block(lexer);
        default:
            fatal("Unexpected symbol '" SV_SPEC "'", SV_ARG(token.text));
        }
    }
    case TK_KEYWORD: {
        switch (token.code) {
        case KW_BREAK:
            lexer_lex(lexer);
            ret = syntax_node_make(SNT_BREAK, token.text, token);
            skip_semicolon(lexer, ret);
            break;
        case KW_CONST:
            ret = parse_variable_declaration(lexer, true);
            break;
        case KW_CONTINUE:
            lexer_lex(lexer);
            ret = syntax_node_make(SNT_CONTINUE, token.text, token);
            skip_semicolon(lexer, ret);
            break;
        case KW_IF:
            ret = parse_if(lexer);
            break;
        case KW_LOOP:
            ret = parse_loop(lexer);
            break;
        case KW_RETURN:
            ret = parse_return(lexer);
            break;
        case KW_VAR:
            ret = parse_variable_declaration(lexer, false);
            break;
        case KW_WHILE:
            ret = parse_while(lexer);
            break;
        default:
            NYI("Keywords");
        }
        break;
    }
    case TK_IDENTIFIER:
        ret = parse_assignment_or_call(lexer);
        break;
    default:
        fatal("Unexpected token '" SV_SPEC "'", SV_ARG(token.text));
    }
    return ret;
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
    if (!token_matches(token, TK_SYMBOL, ':')) {
        fatal("Expect ':' or '{' after parameter list of '" SV_SPEC "'", SV_ARG(func->name));
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
    if (token_matches(token, TK_SYMBOL, '{')) {
        SyntaxNode *impl = syntax_node_make(SNT_FUNCTION_IMPL, func->name, token);
        func->function.function_impl = impl;
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
                impl->function_impl.statements = stmt;
            } else {
                last_stmt->next = stmt;
            }
            last_stmt = stmt;
        }
    } else if (token_matches(token, TK_KEYWORD, KW_FUNC_BINDING)) {
        token = lexer_next(lexer);
        if (!token_matches(token, TK_QUOTED_STRING, TC_DOUBLE_QUOTED_STRING)) {
            fatal("Expected native function reference after '->'");
        }
        lexer_lex(lexer);
        func->function.function_impl = syntax_node_make(
            SNT_NATIVE_FUNCTION,
            (StringView) { token.text.ptr + 1, token.text.length - 2},
            token);
        return func;
    } else {
        fatal("Expected '{' or '->' after function header");
    }
}

SyntaxNode *parse_struct_def(Lexer *lexer)
{
    lexer_lex(lexer);
    Token ident = lexer_next(lexer);
    if (!token_matches(ident, TK_IDENTIFIER, TC_IDENTIFIER)) {
        fatal("Expected 'struct' to be followed by type name");
    }
    lexer_lex(lexer);
    Token token = lexer_next(lexer);
    if (!token_matches(token, TK_SYMBOL, '{')) {
        fatal("Expected '{' to start definition of struct type '" SV_SPEC "'", SV_ARG(ident.text));
    }
    lexer_lex(lexer);
    SyntaxNode *strukt = syntax_node_make(SNT_STRUCT, ident.text, ident);
    SyntaxNode *last_component = NULL;
    while (true) {
        Token comp = lexer_lex(lexer);
        switch (comp.kind) {
        case TK_IDENTIFIER: {
            token = lexer_lex(lexer);
            if (token.kind != TK_SYMBOL || token.code != ':') {
                fatal("Expected ':' after parameter '" SV_SPEC "'", SV_ARG(comp.text));
            }
            SyntaxNode *component = syntax_node_make(SNT_TYPE_COMPONENT, comp.text, token);
            component->parameter.parameter_type = parse_type(lexer);
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
                    fatal("Expected parameter or ')' in component list of '" SV_SPEC "'", SV_ARG(strukt->name));
                }
                break;
            default:
                if (last_component == NULL) {
                    fatal("Expected component or ')' in component list of '" SV_SPEC "'", SV_ARG(strukt->name));
                } else {
                    fatal("Expected ';' or ')' after component '" SV_SPEC "' of '" SV_SPEC "'",
                        SV_ARG(last_component->name), SV_ARG(strukt->name));
                }
            }
        } break;
        default:
            fatal("Expected ',' or ')' in component list of '" SV_SPEC "'", SV_ARG(strukt->name));
        }
    }
}

SyntaxNode *parse_module(SyntaxNode *program, StringView buffer, StringView name)
{
    Token       token = { name, TK_MODULE, TC_NONE };
    SyntaxNode *module = syntax_node_make(SNT_MODULE, name, token);
    Lexer       lexer = { 0 };

    printf("Compiling '" SV_SPEC "'\n", SV_ARG(name));
    lexer.skip_whitespace = true;
    lexer_push_source(&lexer, buffer, name);
    SyntaxNode *last_statement = NULL;
    do {
        token = lexer_next(&lexer);

        SyntaxNode *statement = NULL;
        if (token_matches(token, TK_KEYWORD, KW_FUNC)) {
            statement = parse_function(&lexer);
        } else if (token_matches(token, TK_KEYWORD, KW_STRUCT)) {
            statement = parse_struct_def(&lexer);
        }
        if (statement) {
            if (!last_statement) {
                module->block.statements = statement;
            } else {
                last_statement->next = statement;
            }
            last_statement = statement;
        } else {
            lexer_lex(&lexer);
        }
    } while (token.kind != TK_END_OF_FILE);
    module->next = program->program.modules;
    program->program.modules = module;
    return module;
}

SyntaxNode *parse_module_file(SyntaxNode *program, int dir_fd, char const *file)
{
    MUST(Char, char *, buffer, read_file_at(dir_fd, file))
    return parse_module(program, sv_from(buffer), sv_copy_cstr_with_allocator(file, get_allocator()));
}

SyntaxNode *parse(char const *dir_or_file)
{
    if (OPT_TRACE) {
        char *cwd = getwd(NULL);
        trace("CWD: %s dir: %s", cwd, dir_or_file);
        free(cwd);
    }
    Token          token = { sv_from(dir_or_file), TK_PROGRAM, TC_NONE };
    SyntaxNode    *program = syntax_node_make(SNT_PROGRAM, sv_from(dir_or_file), token);

    DIR *dir = opendir(dir_or_file);
    if (dir == NULL) {
        if (errno == ENOTDIR) {
            dir = opendir(".");
            if (dir == NULL) {
               fatal("Could not open current directory");
            }
            SyntaxNode *module = parse_module_file(program, dirfd(dir), dir_or_file);
            closedir(dir);
            return program;
        }
    }

    struct dirent *dp;
    while ((dp = readdir(dir)) != NULL) {
        if ((dp->d_namlen > 8) && strcmp(dp->d_name + (dp->d_namlen - 9), ".scribble") == 0) {
            SyntaxNode *module = parse_module_file(program, dirfd(dir), dp->d_name);
        }
    }
    closedir(dir);
    return program;
}
