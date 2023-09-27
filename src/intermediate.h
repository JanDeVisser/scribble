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
    S(DEFINE_AGGREGATE)       \
    S(DEFINE_ALIAS)           \
    S(DEFINE_ARRAY)           \
    S(DEFINE_VARIANT)         \
    S(JUMP)                   \
    S(JUMP_F)                 \
    S(JUMP_T)                 \
    S(LABEL)                  \
    S(NEW_DATUM)              \
    S(OPERATOR)               \
    S(POP_VAR)                \
    S(POP_VAR_COMPONENT)      \
    S(PUSH_BOOL_CONSTANT)     \
    S(PUSH_FLOAT_CONSTANT)    \
    S(PUSH_INT_CONSTANT)      \
    S(PUSH_STRING_CONSTANT)   \
    S(PUSH_VAR)               \
    S(PUSH_VAR_COMPONENT)     \
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
        struct {
            union {
                int64_t  int_value;
                uint64_t unsigned_value;
            };
            size_t width;
            bool   un_signed;
        } integer;
        size_t     label;
        double     double_value;
        bool       bool_value;
        StringView sv;
        IRVarDecl  var_decl;
        Operator   op;
        struct {
            StringView name;
            size_t     component;
        } var_component;
    };
} IROperation;

typedef enum ir_function_kind {
    FK_SCRIBBLE = 0,
    FK_NATIVE,
    FK_INTRINSIC
} IRFunctionKind;

typedef struct ir_function {
    struct ir_program *program;
    IRFunctionKind     kind;
    StringView         name;
    size_t             num_parameters;
    IRVarDecl         *parameters;
    TypeSpec           type;
    union {
        struct {
            size_t       cap_operations;
            size_t       num_operations;
            IROperation *operations;
        } scribble;
        StringView native_name;
        Intrinsic  intrinsic;
    };
} IRFunction;

typedef struct ir_program {
    StringView  name;
    int         main;
    int         $static;
    size_t      cap_functions;
    size_t      num_functions;
    IRFunction *functions;
} IRProgram;

extern char const *ir_operation_type_name(IROperationType optype);
extern void        ir_operation_print_prefix(IROperation *op, char const *prefix);
extern void        ir_operation_print(IROperation *op);
extern StringView  ir_var_decl_to_string(IRVarDecl *var, Allocator *allocator);
extern void        ir_var_decl_print(IRVarDecl *var);
extern void        ir_function_list(IRFunction *function, size_t mark);
extern StringView  ir_function_to_string(IRFunction *function, Allocator *allocator);
extern void        ir_function_print(IRFunction *function);
extern size_t      ir_function_resolve_label(IRFunction *function, size_t label);
extern void        ir_program_list(IRProgram program);
extern IRFunction *ir_program_function_by_name(IRProgram *program, StringView name);
extern IRProgram   generate(BoundNode *program);

#endif /* __INTERMEDIATE_H__ */
