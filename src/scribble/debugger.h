/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <da.h>
#include <ir.h>
#include <type.h>

#ifndef __DEBUGGER_H__
#define __DEBUGGER_H__

typedef enum execution_mode {
    EM_RUN = 0x00,
    EM_SINGLE_STEP = 0x01,
    EM_STEP_OVER = 0x02,
    EM_RUN_TO_RETURN = 0x04,
    EM_CONTINUE = 0x08,
} ExecutionMode;

typedef enum {
    EMT_STAGE_INIT,
    EMT_PROGRAM_START,
    EMT_FUNCTION_ENTRY,
    EMT_ON_INSTRUCTION,
    EMT_AFTER_INSTRUCTION,
    EMT_FUNCTION_RETURN,
    EMT_PROGRAM_EXIT,
} ExecutionMessageType;

typedef struct ExecutionMessage {
    ExecutionMessageType type;
    void                *payload;
} ExecutionMessage;

typedef struct breakpoint {
    IRFunction *function;
    size_t      index;
} Breakpoint;

DA(Breakpoint)
typedef DA_Breakpoint Breakpoints;

typedef struct observer_stack {
    IRFunction            *function;
    size_t                 index;
    bool                   step_over;
    struct observer_stack *prev;
} ObserverStack;

typedef struct command {
    int        command;
    StringView command_str;
    StringList arguments;
} Command;

typedef struct {
    IRProgram     *program;
    ObserverStack *stack;
    ExecutionMode  execution_mode;
    bool           trace;
    Breakpoints    breakpoints;
    Command        command;
    void          *execution_context;
    void          *execution_function;
} ObserverContext;

typedef bool (*ExecutionObserver)(void *ctx, ExecutionMessage msg);
typedef bool (*ObservationProcessor)(ObserverContext *ctx, ExecutionMessage msg);

typedef struct {
    ObservationProcessor processor;
    char const          *custom_commands;
    char const          *help_text;
    ObserverContext     *context;
} ObserverRegistry;

extern void    debug_describe_type(ExpressionType *type);
extern void    debug_list_types();
extern Command debug_get_command(char const *custom);
extern bool    debug_set_breakpoint(ObserverContext *ctx, StringView bp_function, StringView bp_index);
extern bool    debug_execution_observer(void *context, ExecutionMessage msg);
extern bool    debug_processor(ObserverContext *ctx, ExecutionMessage msg);
extern void    register_execution_observer(ObservationProcessor observer);

#endif /* __DEBUGGER_H__ */
