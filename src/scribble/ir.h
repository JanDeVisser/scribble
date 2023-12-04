/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdlib.h>

#include <da.h>
#include <op.h>
#include <sv.h>
#include <type.h>

#ifndef __IR_H__
#define __IR_H__

#define IR_OPERATION_TYPES(S) \
    S(BINARY_OPERATOR)        \
    S(CALL)                   \
    S(CASE)                   \
    S(CAST)                   \
    S(DECL_VAR)               \
    S(DEFINE_AGGREGATE)       \
    S(DEFINE_ALIAS)           \
    S(DEFINE_VARIANT)         \
    S(DEREFERENCE)            \
    S(END_CASE)               \
    S(END_MATCH)              \
    S(JUMP)                   \
    S(JUMP_F)                 \
    S(JUMP_T)                 \
    S(LABEL)                  \
    S(MATCH)                  \
    S(NEW_DATUM)              \
    S(POP_VAR)                \
    S(PUSH_BOOL_CONSTANT)     \
    S(PUSH_FLOAT_CONSTANT)    \
    S(PUSH_INT_CONSTANT)      \
    S(PUSH_STRING_CONSTANT)   \
    S(PUSH_VAR)               \
    S(PUSH_VAR_ADDRESS)       \
    S(RETURN)                 \
    S(SCOPE_BEGIN)            \
    S(SCOPE_END)              \
    S(UNARY_OPERATOR)

typedef enum ir_operation_type {
#undef IR_OPERATION_TYPE
#define IR_OPERATION_TYPE(t) IR_##t,
    IR_OPERATION_TYPES(IR_OPERATION_TYPE)
#undef IR_OPERATION_TYPE
} IROperationType;

typedef struct ir_var_decl {
    StringView name;
    TypeSpec   type;
} IRVarDecl;

typedef struct ir_operation {
    IROperationType operation;
    size_t          index;
    StringView      sv;
    union {
        Integer   integer;
        size_t    label;
        double    double_value;
        bool      bool_value;
        type_id   type;
        IRVarDecl var_decl;
        struct {
            StringView name;
            bool       discard_result;
        } call;
        struct {
            Operator op;
            type_id  lhs;
            type_id  rhs;
        } binary_operator;
        struct {
            Operator op;
            type_id  operand;
        } unary_operator;
        struct {
            StringView name;
            DIA(size_t);
        } var_component;
    };
} IROperation;

DA(IROperation)

typedef enum ir_object_type {
    OT_PROGRAM = 0,
    OT_IMPORT = 1,
    OT_MODULE = 2,
    OT_FUNCTION = 3
} IRObjectType;

typedef struct ir_object {
    IRObjectType obj_type;
} IRObject;

typedef enum ir_function_kind {
    FK_SCRIBBLE = 0,
    FK_NATIVE,
} IRFunctionKind;

typedef struct ir_function {
    IRObjectType      obj_type;
    struct ir_module *module;
    IRFunctionKind    kind;
    StringView        name;
    size_t            num_parameters;
    IRVarDecl        *parameters;
    TypeSpec          type;
    union {
        DA_IROperation operations;
        StringView     native_name;
    };
} IRFunction;

DA(IRFunction)

typedef struct ir_module {
    IRObjectType       obj_type;
    struct ir_program *program;
    StringView         name;
    int                $static;
    DA_IRFunction      functions;
} IRModule;

DA(IRModule)

typedef struct ir_program {
    IRObjectType obj_type;
    StringView   name;
    DA_IRModule  modules;
} IRProgram;

extern char const *ir_operation_type_name(IROperationType optype);
extern void        ir_operation_set(IROperation *op, IROperationType operation);
extern void        ir_function_add_operation(IRFunction *fnc, IROperation op);
extern void        ir_function_add_push_u64(IRFunction *fnc, uint64_t value);
extern StringView  ir_operation_to_string(IROperation *op);
extern void        ir_operation_print_prefix(IROperation *op, char const *prefix);
extern void        ir_operation_print(IROperation *op);
extern StringView  ir_var_decl_to_string(IRVarDecl *var);
extern void        ir_var_decl_print(IRVarDecl *var);
extern void        ir_function_list(IRFunction *function, size_t mark);
extern StringView  ir_function_to_string(IRFunction *function);
extern void        ir_function_print(IRFunction *function);
extern ErrorOrSize ir_function_resolve_label(IRFunction *function, size_t label);
extern void        ir_module_list(IRModule *module, bool header);
extern IRFunction *ir_module_function_by_name(IRModule *module, StringView name);
extern void        ir_program_list(IRProgram program);
extern IRFunction *ir_program_function_by_name(IRProgram *program, StringView name);

#endif /* __IR_H__ */
