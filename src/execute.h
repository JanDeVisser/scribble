/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdint.h>

#include <datum.h>
#include <intermediate.h>
#include <sv.h>

#ifndef __EXECUTE_H__
#define __EXECUTE_H__

typedef struct var_list {
    StringView       name;
    Datum            value;
    struct var_list *next;
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

typedef enum execution_mode {
    EM_RUN,
    EM_SINGLE_STEP,
    EM_RUN_TO_RETURN,
    EM_CONTINUE,
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
    ExecutionMode execution_mode;
    size_t        num_breakpoints;
    Breakpoint    breakpoints[MAX_BREAKPOINTS];
} ExecutionContext;

int execute(IRProgram program, bool debug);

#endif /* __EXECUTE_H__ */
