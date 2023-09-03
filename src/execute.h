/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdint.h>

#include <datum.h>
#include <intermediate.h>
#include <sv.h>
#include <type.h>

#ifndef __EXECUTE_H__
#define __EXECUTE_H__

typedef struct var_list {
    StringView       name;
    type_id          type;
    struct var_list *next;
    union {
        Datum value;
        struct {
            size_t num_components;
            Datum *components;
        } composite;
    };
} VarList;

typedef struct scope {
    struct scope *enclosing;
    VarList      *var_list;
    union {
        IRFunction *owner;
        IRProgram  *program;
    };
} Scope;

typedef struct datum_stack_entry {
    Datum                     datum;
    struct datum_stack_entry *prev;
} DatumStackEntry;

typedef struct datum_stack {
    DatumStackEntry *bottom;
    DatumStackEntry *top;
} DatumStack;

typedef struct call_stack_entry {
    IRFunction              *function;
    size_t                   index;
    struct call_stack_entry *down;
} CallStackEntry;

typedef struct call_stack {
    CallStackEntry *bottom;
    CallStackEntry *top;
} CallStack;

typedef enum execution_mode {
    EM_RUN = 0x00,
    EM_SINGLE_STEP = 0x01,
    EM_STEP_OVER = 0x02,
    EM_RUN_TO_RETURN = 0x04,
    EM_CONTINUE = 0x08,
} ExecutionMode;

#define MAX_BREAKPOINTS 64

typedef struct breakpoint {
    IRFunction *function;
    size_t      index;
} Breakpoint;

typedef struct execution_context {
    IRProgram    *program;
    Scope        *root_scope;
    Scope        *scope;
    DatumStack    stack;
    IRFunction   *function;
    size_t        index;
    CallStack     call_stack;
    ExecutionMode execution_mode;
    size_t        num_breakpoints;
    Breakpoint    breakpoints[MAX_BREAKPOINTS];
} ExecutionContext;

typedef enum function_return_type {
    FRT_NORMAL,
    FRT_EXCEPTION,
    FRT_EXIT,
} FunctionReturnType;

typedef struct function_return {
    FunctionReturnType type;
    union {
        Datum return_value;
        Datum exception;
        int   exit_code;
    };
} FunctionReturn;

int            execute(IRProgram program);
FunctionReturn execute_function(ExecutionContext *ctx, IRFunction *function);
FunctionReturn execute_intrinsic(ExecutionContext *ctx, IRIntrinsicFunction *intrinsic);

#endif /* __EXECUTE_H__ */
