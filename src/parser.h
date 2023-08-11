/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include "lexer.h"
#include "sv.h"

#ifndef __PARSER_H__
#define __PARSER_H__

/*
 * Precendeces according to https://en.cppreference.com/w/c/language/operator_precedence
 */
#define BINARY_OPERATORS(S)                                           \
    S(INVALID, false, -1, TK_UNKNOWN, TC_NONE)                        \
    S(ADD, false, 11, TK_SYMBOL, '+')                                 \
    S(SUBTRACT, false, 11, TK_SYMBOL, '-')                            \
    S(MULTIPLY, false, 12, TK_SYMBOL, '*')                            \
    S(DIVIDE, false, 12, TK_SYMBOL, '/')                              \
    S(MODULO, false, 12, TK_SYMBOL, '%')                              \
    S(EQUALS, false, 8, TK_KEYWORD, KW_EQUALS)                        \
    S(NOT_EQUALS, false, 8, TK_KEYWORD, KW_NOT_EQUALS)                \
    S(GREATER_EQUALS, false, 9, TK_KEYWORD, KW_GREATER_EQUALS)        \
    S(LESS_EQUALS, false, 9, TK_KEYWORD, KW_LESS_EQUALS)              \
    S(GREATER, false, 9, TK_SYMBOL, '>')                              \
    S(LESS, false, 9, TK_SYMBOL, '<')                                 \
    S(LOGICAL_AND, false, 4, TK_KEYWORD, KW_LOGICAL_AND)              \
    S(LOGICAL_OR, false, 3, TK_KEYWORD, KW_LOGICAL_OR)                \
    S(BITWISE_AND, false, 7, TK_SYMBOL, '&')                          \
    S(BITWISE_OR, false, 5, TK_SYMBOL, '|')                           \
    S(BITWISE_XOR, false, 6, TK_SYMBOL, '^')                          \
    S(MEMBER_ACCESS, false, 14, TK_SYMBOL, '.')                       \
    S(BIT_SHIFT_LEFT, false, 10, TK_KEYWORD, KW_BIT_SHIFT_LEFT)       \
    S(BIT_SHIFT_RIGHT, false, 10, TK_KEYWORD, KW_BIT_SHIFT_RIGHT)     \
    S(RANGE, false, 8, TK_KEYWORD, KW_RANGE)                          \
    S(SUBSCRIPT, false, 14, TK_SYMBOL, '[')                           \
    S(CALL, false, 14, TK_SYMBOL, '*')

#define UNARY_OPERATORS(S)                             \
    S(INVALID_UNARY, TK_UNKNOWN, TC_NONE)              \
    S(IDENTITY, TK_SYMBOL, '+')                        \
    S(NEGATE, TK_SYMBOL, '-')                          \
    S(UNARY_INCREMENT, TK_KEYWORD, KW_UNARY_INCREMENT) \
    S(UNARY_DECREMENT, TK_KEYWORD, KW_UNARY_INCREMENT) \
    S(LOGICAL_INVERT, TK_SYMBOL, '!')                  \
    S(BITWISE_INVERT, TK_SYMBOL, '~')                  \
    S(DEREFERENCE, TK_SYMBOL, '*')                     \
    S(ADDRESS_OF, TK_SYMBOL, '@')                      \
    S(UNARY_MEMBER_ACCESS, TK_SYMBOL, '.')

typedef enum {
#undef ENUM_BINARY_OPERATOR
#define ENUM_BINARY_OPERATOR(op, a, p, k, c) OP_ ## op,
    BINARY_OPERATORS(ENUM_BINARY_OPERATOR)
#undef ENUM_BINARY_OPERATOR
#undef ENUM_UNARY_OPERATOR
#define ENUM_UNARY_OPERATOR(op, k, c) OP_ ## op,
        UNARY_OPERATORS(ENUM_UNARY_OPERATOR)
#undef ENUM_UNARY_OPERATOR
            OP_COUNT
} Operator;

typedef struct operator_mapping {
    Operator operator;
    bool      binary;
    TokenKind token_kind;
    TokenCode token_code;
    int       precedence;
} OperatorMapping;

#define SYNTAXNODETYPES(S)  \
    S(SNT_ASSIGNMENT)       \
    S(SNT_BINARYEXPRESSION) \
    S(SNT_BLOCK)            \
    S(SNT_BREAK)            \
    S(SNT_CONTINUE)         \
    S(SNT_FUNCTION)         \
    S(SNT_FUNCTION_CALL)    \
    S(SNT_IF)               \
    S(SNT_LOOP)             \
    S(SNT_MODULE)           \
    S(SNT_NUMBER)           \
    S(SNT_PARAMETER)        \
    S(SNT_PROGRAM)          \
    S(SNT_RETURN)           \
    S(SNT_STRING)           \
    S(SNT_TYPE)             \
    S(SNT_UNARYEXPRESSION)  \
    S(SNT_VARIABLE)         \
    S(SNT_VARIABLE_DECL)    \
    S(SNT_WHILE)

typedef enum {
    SNT_UNKNOWN = 0,
#undef SYNTAXNODETYPE_ENUM
#define SYNTAXNODETYPE_ENUM(type) type,
    SYNTAXNODETYPES(SYNTAXNODETYPE_ENUM)
#undef SYNTAXNODETYPE_ENUM
} SyntaxNodeType;

typedef struct syntax_node {
    SyntaxNodeType      type;
    StringView          name;
    struct syntax_node *next;
    size_t              index;
    Token               token;

    union {
        struct {
            struct syntax_node *modules;
        } program;
        struct {
            struct syntax_node *statements;
        } block;
        struct {
            struct syntax_node *return_type;
            struct syntax_node *error_type;
            struct syntax_node *parameter;
            struct syntax_node *statements;
        } function;
        struct {
            struct syntax_node *parameter_type;
        } parameter;
        struct {
            struct syntax_node *argument;
        } call;
        struct {
            struct syntax_node *lhs;
            struct syntax_node *rhs;
            Operator operator;
        } binary_expr;
        struct {
            struct syntax_node *var_type;
            struct syntax_node *init_expr;
            bool                is_const;
        } variable_decl;
        struct {
            struct syntax_node *condition;
            struct syntax_node *if_true;
            struct syntax_node *if_false;
        } if_statement;
        struct {
            struct syntax_node *expression;
        } return_stmt;
        struct {
            struct syntax_node *expression;
            Operator operator;
        } assignment;
        struct {
            struct syntax_node *condition;
            struct syntax_node *statement;
        } while_statement;
    };
} SyntaxNode;

extern size_t      next_index();
extern char       *Operator_name(Operator op);
extern char const *SyntaxNodeType_name(SyntaxNodeType type);
extern SyntaxNode *parse(char const *dir_name);

#endif /* __PARSER_H__ */
