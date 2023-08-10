/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <parser.h>
#include <sv.h>
#include <type.h>

#ifndef __BINDER_H__
#define __BINDER_H__

#define BOUNDNODETYPES(S)   \
    S(BNT_ASSIGNMENT)       \
    S(BNT_BINARYEXPRESSION) \
    S(BNT_BLOCK)            \
    S(BNT_FUNCTION)         \
    S(BNT_FUNCTION_CALL)    \
    S(BNT_IF)               \
    S(BNT_INTRINSIC)        \
    S(BNT_MODULE)           \
    S(BNT_NUMBER)           \
    S(BNT_PARAMETER)        \
    S(BNT_PROGRAM)          \
    S(BNT_RETURN)           \
    S(BNT_STRING)           \
    S(BNT_TYPE)             \
    S(BNT_UNARYEXPRESSION)  \
    S(BNT_UNBOUND_NODE)     \
    S(BNT_VARIABLE)         \
    S(BNT_VARIABLE_DECL)

typedef enum bound_node_type {
    BNT_OFFSET = 1000,
#undef BOUNDNODETYPE_ENUM
#define BOUNDNODETYPE_ENUM(type) type,
    BOUNDNODETYPES(BOUNDNODETYPE_ENUM)
#undef BOUNDNODETYPE_ENUM
        BNT_LAST
} BoundNodeType;

extern char const *BoundNodeType_name(BoundNodeType type);

#define INTRINSICS(S) \
    S(FPUTS)          \
    S(PUTLN)

typedef enum intrinsic {
#undef INTRINSIC_ENUM
#define INTRINSIC_ENUM(i) INT_##i,
    INTRINSICS(INTRINSIC_ENUM)
#undef INTRINSIC_ENUM
} Intrinsic;

typedef struct bound_node {
    BoundNodeType      type;
    StringView         name;
    struct bound_node *next;
    size_t             index;
    struct bound_node *parent;
    TypeSpec           typespec;
    union {
        struct {
            struct bound_node *intrinsics;
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
            struct bound_node *parameter;
            Intrinsic          intrinsic;
        } intrinsic;
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
        struct {
            struct bound_node *expression;
        } assignment;
        SyntaxNode *unbound_node;
    };
} BoundNode;

typedef void (*BindingObserver)(int, BoundNode *);

extern char const     *BoundNodeType_name(BoundNodeType type);
extern char const     *Intrinsic_name(Intrinsic intrinsic);
extern BoundNode      *bind(SyntaxNode *program);
extern BindingObserver register_binding_observer(BindingObserver observer);

#endif /* __BINDER_H__ */
