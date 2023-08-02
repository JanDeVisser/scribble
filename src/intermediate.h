/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdlib.h>

#include <binder.h>
#include <sv.h>
#include <type.h>

#ifndef __INTERMEDIATE_H__
#define __INTERMEDIATE_H__

#define IR_OPERATION_TYPES(S) \
    S(CALL)                   \
    S(DECL_VAR)               \
    S(JUMP)                   \
    S(JUMP_F)                 \
    S(JUMP_T)                 \
    S(LABEL)                  \
    S(OPERATOR)               \
    S(POP_VAR)                \
    S(PUSH_INT_CONSTANT)      \
    S(PUSH_VAR)               \
    S(RETURN)                 \
    S(SCOPE_BEGIN)            \
    S(SCOPE_END)

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
    union {
        int          int_value;
        unsigned int unsigned_value;
        double       double_value;
        bool         bool_value;
        StringView   sv;
        IRVarDecl    var_decl;
        Operator     op;
    };
} IROperation;

typedef struct ir_function {
    StringView   name;
    size_t       num_parameters;
    IRVarDecl   *parameters;
    TypeSpec     type;
    size_t       cap_operations;
    size_t       num_operations;
    IROperation *operations;
} IRFunction;

typedef struct ir_program {
    StringView  name;
    int         main;
    size_t      cap_functions;
    size_t      num_functions;
    IRFunction *functions;
} IRProgram;

char const *ir_operation_type_name(IROperationType optype);
void        ir_operation_print_prefix(IROperation *op, char const *prefix);
void        ir_operation_print(IROperation *op);
void        ir_function_list(IRFunction *function, size_t mark);
void        ir_function_print(IRFunction *function);
size_t      ir_function_resolve_label(IRFunction *function, size_t label);

IRProgram   generate(BoundNode *program);
void        ir_program_list(IRProgram program);
IRFunction *ir_program_function_by_name(IRProgram *program, StringView name);

#endif /* __INTERMEDIATE_H__ */