/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#ifndef __EXECUTE_H__
#define __EXECUTE_H__

#include <datum.h>
#include <engine.h>
#include <intermediate.h>
#include <sv.h>

typedef struct var_list {
    StringView       name;
    type_id          type;
    struct var_list *next;
    Datum           *value;
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
    Datum                    *datum;
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

#define MAX_BREAKPOINTS 64

typedef struct execution_context {
    BackendConnection *conn;
    bool               debug;
    IRProgram         *program;
    Scope             *root_scope;
    Scope             *function_scope;
    Scope             *scope;
    DatumStack         stack;
    IRFunction        *function;
    size_t             index;
    CallStack          call_stack;
    void              *observer_data;
} ExecutionContext;

typedef enum function_return_type {
    FRT_NORMAL,
    FRT_EXCEPTION,
    FRT_EXIT,
} FunctionReturnType;

typedef struct function_return {
    FunctionReturnType type;
    union {
        Datum *return_value;
        Datum *exception;
        int    exit_code;
    };
} FunctionReturn;

typedef enum next_instruction_type {
    NIT_LABEL,
    NIT_RELATIVE,
    NIT_RETURN,
    NIT_RESET,
    NIT_EXIT,
    NIT_EXCEPTION,
} NextInstructionType;

typedef struct next_instruction_pointer {
    NextInstructionType type;
    union {
        size_t      pointer;
        char const *exception;
    };
} NextInstructionPointer;

extern void           datum_stack_dump(DatumStack *stack);
extern void           call_stack_dump(CallStack *stack);
extern void           scope_dump_variables(Scope *scope);
extern ErrorOrInt     execute(BackendConnection *conn, IRProgram program);
extern Datum         *evaluate_function(IRFunction function);
extern FunctionReturn execute_function(ExecutionContext *ctx, IRFunction *function);

#endif /* __EXECUTE_H__ */
