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
    S(ARRAY)                \
    S(ASSIGNMENT)           \
    S(BINARYEXPRESSION)     \
    S(BLOCK)                \
    S(BOOL)                 \
    S(BREAK)                \
    S(COMPOUND_INITIALIZER) \
    S(CONTINUE)             \
    S(DECIMAL)              \
    S(FOR)                  \
    S(FUNCTION)             \
    S(FUNCTION_CALL)        \
    S(FUNCTION_IMPL)        \
    S(NAME)                 \
    S(IF)                   \
    S(IMPORT)               \
    S(INTEGER)              \
    S(INTRINSIC)            \
    S(LOOP)                 \
    S(MODULE)               \
    S(NATIVE_FUNCTION)      \
    S(PARAMETER)            \
    S(PROGRAM)              \
    S(RETURN)               \
    S(STRING)               \
    S(STRUCT)               \
    S(TERNARYEXPRESSION)    \
    S(TYPE)                 \
    S(TYPE_COMPONENT)       \
    S(UNARYEXPRESSION)      \
    S(UNBOUND_NODE)         \
    S(UNBOUND_TYPE)         \
    S(VARIABLE)             \
    S(VARIABLE_DECL)        \
    S(VARIANT)              \
    S(WHILE)

typedef enum bound_node_type {
    BNT_OFFSET = 1000,
#undef BOUNDNODETYPE_ENUM
#define BOUNDNODETYPE_ENUM(type) BNT_##type,
    BOUNDNODETYPES(BOUNDNODETYPE_ENUM)
#undef BOUNDNODETYPE_ENUM
        BNT_LAST
} BoundNodeType;

#define INTRINSICS(S) \
    S(ALLOC)          \
    S(CLOSE)          \
    S(OPEN)           \
    S(READ)           \
    S(WRITE)

typedef enum intrinsic {
#undef INTRINSIC_ENUM
#define INTRINSIC_ENUM(i) INT_##i,
    INTRINSICS(INTRINSIC_ENUM)
#undef INTRINSIC_ENUM
} Intrinsic;

typedef struct bound_node {
    BoundNodeType        type;
    StringView           name;
    struct bound_node   *next;
    struct bound_node   *prev;
    size_t               index;
    struct bound_node   *parent;
    TypeSpec             typespec;
    struct intermediate *intermediate;
    union {
        struct {
            struct bound_node *modules;
        } import;
        struct {
            struct bound_node *types;
            struct bound_node *intrinsics;
            struct bound_node *imports;
            struct bound_node *modules;
        } program;
        struct {
            struct bound_node *statements;
        } block;
        struct bound_node *controlled_statement;
        struct {
            struct bound_node *parameter;
            struct bound_node *function_impl;
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
            type_id base_type;
            size_t  size;
        } array_def;
        struct {
            struct bound_node *expression;
        } assignment;
        struct {
            struct bound_node *function;
            struct bound_node *argument;
            bool               discard_result;
        } call;
        struct {
            struct bound_node *argument;
        } compound_initializer;
        struct {
            struct bound_node *condition;
            struct bound_node *if_true;
            struct bound_node *if_false;
        } if_statement;
        struct {
            struct bound_node *variable;
            struct bound_node *range;
            struct bound_node *statement;
        } for_statement;
        struct {
            struct bound_node *expression;
        } return_stmt;
        struct {
            struct bound_node *components;
        } compound_def;
        struct {
            struct bound_node *condition;
            struct bound_node *if_true;
            struct bound_node *if_false;
        } ternary_expr;
        struct {
            type_id alias_of;
        } type_alias;
        struct {
            struct bound_node *decl;
            struct bound_node *names;
        } variable;
        struct {
            struct bound_node *init_expr;
            bool               is_const;
        } variable_decl;
        struct {
            struct bound_node *condition;
            struct bound_node *statement;
        } while_statement;
        SyntaxNode *unbound_node;
    };
} BoundNode;

typedef void (*BindingObserver)(int, BoundNode *);

extern char const     *BoundNodeType_name(BoundNodeType type);
extern char const     *Intrinsic_name(Intrinsic intrinsic);
extern BoundNode      *bind(SyntaxNode *program);
extern BindingObserver register_binding_observer(BindingObserver observer);

#endif /* __BINDER_H__ */
