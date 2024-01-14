/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#ifndef __SYNTAXNODE_H__
#define __SYNTAXNODE_H__

#include <json.h>
#include <sv.h>

#include <model/op.h>
#include <model/token.h>

#define SYNTAXNODETYPES(S) \
    S(ASSIGNMENT)          \
    S(BINARYEXPRESSION)    \
    S(BLOCK)               \
    S(BOOL)                \
    S(BREAK)               \
    S(COMPOUND)            \
    S(CONTINUE)            \
    S(DECIMAL)             \
    S(ENUMERATION)         \
    S(ENUM_VALUE)          \
    S(FOR)                 \
    S(FUNCTION)            \
    S(FUNCTION_CALL)       \
    S(FUNCTION_IMPL)       \
    S(IF)                  \
    S(INTEGER)             \
    S(IMPORT)              \
    S(LABEL)               \
    S(LOOP)                \
    S(MACRO)               \
    S(MODULE)              \
    S(NAME)                \
    S(NATIVE_FUNCTION)     \
    S(PARAMETER)           \
    S(PROGRAM)             \
    S(RETURN)              \
    S(STRING)              \
    S(STRUCT)              \
    S(TYPE)                \
    S(TYPE_COMPONENT)      \
    S(UNARYEXPRESSION)     \
    S(VARIABLE)            \
    S(VARIABLE_DECL)       \
    S(VARIANT)             \
    S(VARIANT_OPTION)      \
    S(WHILE)

typedef enum {
    SNT_UNKNOWN = 0,
#undef SYNTAXNODETYPE_ENUM
#define SYNTAXNODETYPE_ENUM(type) SNT_##type,
    SYNTAXNODETYPES(SYNTAXNODETYPE_ENUM)
#undef SYNTAXNODETYPE_ENUM
} SyntaxNodeType;

// clang-format off
typedef struct type_descr {
    StringView name;
    DIA(struct type_descr *);
} TypeDescr;
// clang-format on

typedef struct syntax_node {
    SyntaxNodeType      type;
    StringView          name;
    struct syntax_node *next;
    size_t              index;
    Token               token;
    void               *data;

    union {
        struct {
            struct syntax_node *argument;
        } arguments;
        struct {
            struct syntax_node *variable;
            struct syntax_node *expression;
            Operator            operator;
        } assignment;
        struct {
            struct syntax_node *lhs;
            struct syntax_node *rhs;
            Operator            operator;
        } binary_expr;
        struct {
            struct syntax_node *statements;
        } block;
        struct {
            struct syntax_node *function;
            struct syntax_node *arguments;
            bool                discard_result;
        } call;
        struct {
            struct syntax_node *expressions;
        } compound_expr;
        struct {
            struct syntax_node *underlying_type;
            struct syntax_node *values;
        } enumeration;
        struct {
            struct syntax_node *underlying_value;
        } enum_value;
        struct {
            StringView          variable;
            struct syntax_node *range;
            struct syntax_node *statement;
        } for_statement;
        struct {
            struct syntax_node *return_type;
            struct syntax_node *error_type;
            struct syntax_node *parameter;
            struct syntax_node *function_impl;
        } function;
        struct {
            struct syntax_node *statements;
        } function_impl;
        struct {
            struct syntax_node *condition;
            struct syntax_node *if_true;
            struct syntax_node *if_false;
        } if_statement;
        struct {
            struct syntax_node *modules;
        } import;
        struct {
            IntegerType type;
        } integer;
        struct {
            struct syntax_node *parameter_type;
        } parameter;
        struct {
            struct syntax_node *modules;
            struct syntax_node *imports;
        } program;
        struct {
            struct syntax_node *expression;
        } return_stmt;
        struct {
            struct syntax_node *components;
        } struct_def;
        TypeDescr type_descr;
        struct {
            struct syntax_node *operand;
            Operator            operator;
        } unary_expr;
        struct {
            struct syntax_node *subscript;
        } variable;
        struct {
            struct syntax_node *variable;
            struct syntax_node *var_type;
            struct syntax_node *init_expr;
            bool                is_const;
        } variable_decl;
        struct {
            struct syntax_node *underlying_type;
            struct syntax_node *options;
        } variant_def;
        struct {
            struct syntax_node *underlying_value;
            struct syntax_node *payload_type;
        } variant_option;
        struct {
            struct syntax_node *condition;
            struct syntax_node *statement;
        } while_statement;
    };
} SyntaxNode;

extern char const    *SyntaxNodeType_name(SyntaxNodeType type);
extern SyntaxNodeType SyntaxNodeType_from_string(StringView type);
extern JSONValue      type_descr_to_json(TypeDescr *type);
extern TypeDescr     *type_descr_from_json(JSONValue json);
extern JSONValue      syntax_node_to_json(SyntaxNode *node);
extern SyntaxNode     syntax_node_from_json(JSONValue json);

#endif /* __SYNTAXNODE_H__ */
