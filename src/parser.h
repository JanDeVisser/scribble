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
#define BINARY_OPERATORS(S)                                              \
    S(OP_INVALID, false, -1, TK_UNKNOWN, TC_NONE)                        \
    S(OP_ADD, false, 11, TK_SYMBOL, '+')                                 \
    S(OP_SUBTRACT, false, 11, TK_SYMBOL, '-')                            \
    S(OP_MULTIPLY, false, 12, TK_SYMBOL, '*')                            \
    S(OP_DIVIDE, false, 12, TK_SYMBOL, '/')                              \
    S(OP_MODULO, false, 12, TK_SYMBOL, '%')                              \
    S(OP_ASSIGN, true, 1, TK_SYMBOL, '=')                                \
    S(OP_EQUALS, false, 8, TK_KEYWORD, KW_EQUALS)                        \
    S(OP_NOT_EQUALS, false, 8, TK_KEYWORD, KW_NOT_EQUALS)                \
    S(OP_GREATER_EQUALS, false, 9, TK_KEYWORD, KW_GREATER_EQUALS)        \
    S(OP_LESS_EQUALS, false, 9, TK_KEYWORD, KW_LESS_EQUALS)              \
    S(OP_GREATER, false, 9, TK_SYMBOL, '>')                              \
    S(OP_LESS, false, 9, TK_SYMBOL, '<')                                 \
    S(OP_LOGICAL_AND, false, 4, TK_KEYWORD, KW_LOGICAL_AND)              \
    S(OP_LOGICAL_OR, false, 3, TK_KEYWORD, KW_LOGICAL_OR)                \
    S(OP_BITWISE_AND, false, 7, TK_SYMBOL, '&')                          \
    S(OP_BITWISE_OR, false, 5, TK_SYMBOL, '|')                           \
    S(OP_BITWISE_XOR, false, 6, TK_SYMBOL, '^')                          \
    S(OP_BINARY_INCREMENT, true, 1, TK_KEYWORD, KW_BINARY_INCREMENT)     \
    S(OP_BINARY_DECREMENT, true, 1, TK_KEYWORD, KW_BINARY_DECREMENT)     \
    S(OP_MEMBER_ACCESS, false, 14, TK_SYMBOL, '.')                       \
    S(OP_BIT_SHIFT_LEFT, false, 10, TK_KEYWORD, KW_BIT_SHIFT_LEFT)       \
    S(OP_BIT_SHIFT_RIGHT, false, 10, TK_KEYWORD, KW_BIT_SHIFT_RIGHT)     \
    S(OP_ASSIGN_SHIFT_LEFT, true, 1, TK_KEYWORD, KW_ASSIGN_SHIFT_LEFT)   \
    S(OP_ASSIGN_SHIFT_RIGHT, true, 1, TK_KEYWORD, KW_ASSIGN_SHIFT_RIGHT) \
    S(OP_ASSIGN_BITWISE_AND, true, 1, TK_KEYWORD, KW_ASSIGN_BITWISE_AND) \
    S(OP_ASSIGN_BITWISE_OR, true, 1, TK_KEYWORD, KW_ASSIGN_BITWISE_OR)   \
    S(OP_ASSIGN_BITWISE_XOR, true, 1, TK_KEYWORD, KW_ASSIGN_BITWISE_XOR) \
    S(OP_RANGE, false, 8, TK_KEYWORD, KW_RANGE)                          \
    S(OP_SUBSCRIPT, false, 14, TK_SYMBOL, '[')                           \
    S(OP_CALL, false, 14, TK_SYMBOL, '*')

#define UNARY_OPERATORS(S)                                \
    S(OP_INVALID_UNARY, TK_UNKNOWN, TC_NONE)              \
    S(OP_IDENTITY, TK_SYMBOL, '+')                        \
    S(OP_NEGATE, TK_SYMBOL, '-')                          \
    S(OP_UNARY_INCREMENT, TK_KEYWORD, KW_UNARY_INCREMENT) \
    S(OP_UNARY_DECREMENT, TK_KEYWORD, KW_UNARY_INCREMENT) \
    S(OP_LOGICAL_INVERT, TK_SYMBOL, '!')                  \
    S(OP_BITWISE_INVERT, TK_SYMBOL, '~')                  \
    S(OP_DEREFERENCE, TK_SYMBOL, '*')                     \
    S(OP_ADDRESS_OF, TK_SYMBOL, '@')                      \
    S(OP_UNARY_MEMBER_ACCESS, TK_SYMBOL, '.')

typedef enum {
#undef ENUM_BINARY_OPERATOR
#define ENUM_BINARY_OPERATOR(op, a, p, k, c) op,
    BINARY_OPERATORS(ENUM_BINARY_OPERATOR)
#undef ENUM_BINARY_OPERATOR
#undef ENUM_UNARY_OPERATOR
#define ENUM_UNARY_OPERATOR(op, k, c) op,
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
    S(SNT_FUNCTION)         \
    S(SNT_FUNCTION_CALL)    \
    S(SNT_IF)               \
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
