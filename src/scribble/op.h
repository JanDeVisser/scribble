/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#ifndef __OP_H__
#define __OP_H__

/*
 * Precedences according to https://en.cppreference.com/w/c/language/operator_precedence
 */
#define BINARY_OPERATORS(S)                                               \
    S(INVALID, false, -1, TK_UNKNOWN, TC_NONE, 0)                         \
    S(COMMA, false, 0, TK_SYMBOL, ',', 0)                                 \
    S(ASSIGN, false, 1, TK_SYMBOL, '=', 0)                                \
    S(ASSIGN_BITWISE_AND, false, 1, TK_KEYWORD, KW_ASSIGN_BITWISE_AND, 0) \
    S(ASSIGN_BITWISE_OR, false, 1, TK_KEYWORD, KW_ASSIGN_BITWISE_OR, 0)   \
    S(ASSIGN_BITWISE_XOR, false, 1, TK_KEYWORD, KW_ASSIGN_BITWISE_XOR, 0) \
    S(ASSIGN_SHIFT_LEFT, false, 1, TK_KEYWORD, KW_ASSIGN_SHIFT_LEFT, 0)   \
    S(ASSIGN_SHIFT_RIGHT, false, 1, TK_KEYWORD, KW_ASSIGN_SHIFT_RIGHT, 0) \
    S(BINARY_DECREMENT, false, 1, TK_KEYWORD, KW_BINARY_DECREMENT, 0)     \
    S(BINARY_INCREMENT, false, 1, TK_KEYWORD, KW_BINARY_INCREMENT, 0)     \
    S(ASSIGN_MULTIPLY, false, 1, TK_KEYWORD, KW_ASSIGN_MULTIPLY, 0)       \
    S(ASSIGN_DIVIDE, false, 1, TK_KEYWORD, KW_ASSIGN_DIVIDE, 0)           \
    S(ASSIGN_MODULO, false, 1, TK_KEYWORD, KW_ASSIGN_MODULO, 0)           \
    S(SUBSCRIPT, false, 1, TK_SYMBOL, '[', ']')                           \
    S(TERNARY, false, 2, TK_SYMBOL, '?', 0)                               \
    S(TERNARY_ELSE, false, 2, TK_SYMBOL, ':', 0)                          \
    S(LOGICAL_OR, false, 3, TK_KEYWORD, KW_LOGICAL_OR, 0)                 \
    S(LOGICAL_AND, false, 4, TK_KEYWORD, KW_LOGICAL_AND, 0)               \
    S(BITWISE_OR, false, 5, TK_SYMBOL, '|', 0)                            \
    S(BITWISE_XOR, false, 6, TK_SYMBOL, '^', 0)                           \
    S(BITWISE_AND, false, 7, TK_SYMBOL, '&', 0)                           \
    S(EQUALS, false, 8, TK_KEYWORD, KW_EQUALS, 0)                         \
    S(NOT_EQUALS, false, 8, TK_KEYWORD, KW_NOT_EQUALS, 0)                 \
    S(RANGE, false, 8, TK_KEYWORD, KW_RANGE, 0)                           \
    S(GREATER_EQUALS, false, 9, TK_KEYWORD, KW_GREATER_EQUALS, 0)         \
    S(LESS_EQUALS, false, 9, TK_KEYWORD, KW_LESS_EQUALS, 0)               \
    S(GREATER, false, 9, TK_SYMBOL, '>', 0)                               \
    S(LESS, false, 9, TK_SYMBOL, '<', 0)                                  \
    S(BIT_SHIFT_LEFT, false, 10, TK_KEYWORD, KW_BIT_SHIFT_LEFT, 0)        \
    S(BIT_SHIFT_RIGHT, false, 10, TK_KEYWORD, KW_BIT_SHIFT_RIGHT, 0)      \
    S(ADD, false, 11, TK_SYMBOL, '+', 0)                                  \
    S(SUBTRACT, false, 11, TK_SYMBOL, '-', 0)                             \
    S(MULTIPLY, false, 12, TK_SYMBOL, '*', 0)                             \
    S(DIVIDE, false, 12, TK_SYMBOL, '/', 0)                               \
    S(MODULO, false, 12, TK_SYMBOL, '%', 0)                               \
    S(CAST, false, 13, TK_KEYWORD, KW_AS, 0)                              \
    S(CALL, false, 14, TK_SYMBOL, '(', ')')                               \
    S(COMPOUND, false, 14, TK_SYMBOL, '{', '}')                           \
    S(MEMBER_ACCESS, false, 14, TK_SYMBOL, '.', 0)

#define UNARY_OPERATORS(S)                             \
    S(INVALID_UNARY, TK_UNKNOWN, TC_NONE)              \
    S(IDENTITY, TK_SYMBOL, '+')                        \
    S(NEGATE, TK_SYMBOL, '-')                          \
    S(CARDINALITY, TK_SYMBOL, '#')                     \
    S(UNARY_INCREMENT, TK_KEYWORD, KW_UNARY_INCREMENT) \
    S(UNARY_DECREMENT, TK_KEYWORD, KW_UNARY_INCREMENT) \
    S(LOGICAL_INVERT, TK_SYMBOL, '!')                  \
    S(BITWISE_INVERT, TK_SYMBOL, '~')                  \
    S(DEREFERENCE, TK_SYMBOL, '*')                     \
    S(ADDRESS_OF, TK_SYMBOL, '@')                      \
    S(UNARY_MEMBER_ACCESS, TK_SYMBOL, '.')

// clang-format off
typedef enum {
#undef ENUM_BINARY_OPERATOR
#define ENUM_BINARY_OPERATOR(op, a, p, k, c, cl) OP_## op,
    BINARY_OPERATORS(ENUM_BINARY_OPERATOR)
#undef ENUM_BINARY_OPERATOR
#undef ENUM_UNARY_OPERATOR
#define ENUM_UNARY_OPERATOR(op, k, c) OP_## op,
    UNARY_OPERATORS(ENUM_UNARY_OPERATOR)
#undef ENUM_UNARY_OPERATOR
    OP_COUNT
} Operator;
// clang-format on

extern char *Operator_name(Operator op);

#endif /* __OP_H__ */
