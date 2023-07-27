/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include "parser.h"
#include "sv.h"

#ifndef __BINDER_H__
#define __BINDER_H__

typedef struct {
    size_t type_id;
    bool   optional;
} TypeSpec;

#define BOUNDNODETYPES(S)   \
    S(BNT_BINARYEXPRESSION) \
    S(BNT_BLOCK)            \
    S(BNT_FUNCTION)         \
    S(BNT_FUNCTION_CALL)    \
    S(BNT_IF)               \
    S(BNT_MODULE)           \
    S(BNT_NUMBER)           \
    S(BNT_PARAMETER)        \
    S(BNT_PROGRAM)          \
    S(BNT_RETURN)           \
    S(BNT_TYPE)             \
    S(BNT_UNARYEXPRESSION)  \
    S(BNT_UNBOUND_NODE)     \
    S(BNT_VARIABLE)         \
    S(BNT_VARIABLE_DECL)

typedef enum {
#undef BOUNDNODETYPE_ENUM
#define BOUNDNODETYPE_ENUM(type) type,
    BOUNDNODETYPES(BOUNDNODETYPE_ENUM)
#undef BOUNDNODETYPE_ENUM
} BoundNodeType;

extern char const *BoundNodeType_name(BoundNodeType type);

typedef struct bound_node {
    BoundNodeType      type;
    StringView         name;
    struct bound_node *parent;
    struct bound_node *next;
    TypeSpec           typespec;
    size_t             index;
    union {
        struct {
            struct bound_node *modules;
        } program;
        struct {
            struct bound_node *statements;
        } block;
        struct {
            struct bound_node *parameter;
            struct bound_node *statements;
        } function;
        struct {
            struct bound_node *lhs;
            struct bound_node *rhs;
            Operator operator;
        } binary_expr;
        struct {
            struct bound_node *init_expr;
        } variable_decl;
        struct {
            struct bound_node *decl;
        } variable;
        struct {
            struct bound_node *function;
            struct bound_node *argument;
        } call;
        struct {
            struct bound_node *condition;
            struct bound_node *if_true;
            struct bound_node *if_false;
        } if_statement;
        struct {
            struct bound_node *expression;
        } return_stmt;
        SyntaxNode *unbound_node;
    };
} BoundNode;

BoundNode *bind(SyntaxNode *program);

#endif /* __BINDER_H__ */
