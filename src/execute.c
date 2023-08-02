/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <execute.h>
#include <mem.h>

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

Datum datum_stack_pop(DatumStack *stack);
void  datum_stack_push(DatumStack *stack, Datum datum);
void  datum_stack_dump(DatumStack *stack);

VarList    *scope_variable(Scope *scope, StringView name);
char const *scope_declare_variable(Scope *scope, StringView name);
char const *scope_assign_variable(Scope *scope, StringView name, Datum value);
Datum       scope_variable_value(Scope *scope, StringView name);
void        scope_dump_call_stack(Scope *scope);
void        scope_dump_variables(Scope *scope);

NextInstructionPointer execute_operation(ExecutionContext *ctx, IROperation *op);
FunctionReturn         execute_function(ExecutionContext *ctx, IRFunction *function);

static Arena *s_arena = NULL;

static void *allocate(size_t size)
{
    if (!s_arena) {
        s_arena = arena_new();
    }
    return arena_allocate(s_arena, size);
}

Datum datum_stack_pop(DatumStack *stack)
{
    if (!stack->top) {
        Datum error = { 0 };
        error.type = DT_ERROR;
        error.error = "Stack underflow";
        return error;
    }
    Datum ret = stack->top->datum;
    stack->top = stack->top->prev;
    if (!stack->top) {
        stack->bottom = NULL;
    }
    return ret;
}

void datum_stack_push(DatumStack *stack, Datum datum)
{
    DatumStackEntry *entry = allocate(sizeof(DatumStackEntry));
    entry->datum = datum;
    entry->prev = stack->top;
    if (!stack->top && !stack->bottom) {
        stack->top = stack->bottom = entry;
    } else {
        assert(stack->top && stack->bottom);
        stack->top = entry;
    }
}

void datum_stack_dump(DatumStack *stack)
{
    for (DatumStackEntry *entry = stack->top; entry; entry = entry->prev) {
        datum_print(entry->datum);
        printf("\n");
    }
}

VarList *scope_variable(Scope *scope, StringView name)
{
    for (VarList *var = scope->var_list; var; var = var->next) {
        if (sv_eq(var->name, name)) {
            return var;
        }
    }
    return NULL;
}

char const *scope_declare_variable(Scope *scope, StringView name)
{
    VarList *end;
    for (end = scope->var_list; end && end->next; end = end->next) {
        if (sv_eq(end->name, name)) {
            return "Variable was previously declared";
        }
    }
    VarList *new = allocate(sizeof(VarList));
    new->name = name;
    if (end) {
        end->next = new;
    } else {
        scope->var_list = new;
    }
    return NULL;
}

char const *scope_assign_variable(Scope *scope, StringView name, Datum value)
{
    Scope *s = scope;
    while (s) {
        VarList *v = scope_variable(s, name);
        if (v) {
            v->value = value;
            return NULL;
        }
        s = s->enclosing;
    }
    return "Cannot assign undeclared variable";
}

Datum scope_variable_value(Scope *scope, StringView name)
{
    Scope *s = scope;
    while (s) {
        VarList *v = scope_variable(s, name);
        if (v) {
            return v->value;
        }
        s = s->enclosing;
    }
    Datum error = { 0 };
    error.type = DT_ERROR;
    error.error = "Cannot get value of undeclared variable";
    return error;
}

void scope_dump_call_stack(Scope *scope)
{
    for (Scope *s = scope; s->enclosing; s = s->enclosing) {
        if (s->owner) {
            printf("%-10.10s" SV_SPEC "\n", "", SV_ARG(s->owner->name));
        }
    }
}

void scope_dump_variables(Scope *scope)
{
    for (Scope *s = scope; s->enclosing; s = s->enclosing) {
        if (s->var_list) {
            if (s->owner) {
                printf(SV_SPEC ":\n", SV_ARG(s->owner->name));
            } else {
                printf("<anonymous scope>\n");
            }
        }
        for (VarList *var = s->var_list; var; var = var->next) {
            printf("%5.5s" SV_SPEC "  %10.10s  ", "", SV_ARG(var->name), DatumType_name(var->value.type));
            datum_print(var->value);
            printf("\n");
        }
    }
}

NextInstructionPointer execute_operation(ExecutionContext *ctx, IROperation *op)
{
    NextInstructionPointer next = { 0 };
    next.type = NIT_RELATIVE;
    next.pointer = 1;
    switch (op->operation) {
    case IR_CALL: {
        IRFunction *function = NULL;
        function = ir_program_function_by_name(ctx->program, op->sv);
        assert(function);
        FunctionReturn func_ret = execute_function(ctx, function);
        switch (func_ret.type) {
        case FRT_EXIT:
        case FRT_EXCEPTION:
            next.type = NIT_EXIT;
            break;
        default:
            break;
        }
    } break;
    case IR_DECL_VAR: {
        char const *err = scope_declare_variable(ctx->scope, op->var_decl.name);
        if (err) {
            next.type = NIT_EXCEPTION;
            next.exception = err;
        }
    } break;
    case IR_JUMP: {
        next.type = NIT_LABEL;
        next.pointer = op->unsigned_value;
        return next;
    }
    case IR_JUMP_F: {
        Datum cond = datum_stack_pop(&ctx->stack);
        assert(cond.type == DT_BOOL);
        if (!cond.bool_value) {
            next.type = NIT_LABEL;
            next.pointer = op->unsigned_value;
            return next;
        }
    } break;
    case IR_JUMP_T: {
        Datum cond = datum_stack_pop(&ctx->stack);
        if (cond.type == DT_ERROR) {
            next.type = NIT_EXCEPTION;
            next.exception = cond.error;
            return next;
        }
        assert(cond.type == DT_BOOL);
        if (cond.bool_value) {
            next.type = NIT_LABEL;
            next.pointer = op->unsigned_value;
            return next;
        }
    } break;
    case IR_LABEL:
        break;
    case IR_OPERATOR: {
        Datum d1 = datum_stack_pop(&ctx->stack);
        Datum d2 = datum_stack_pop(&ctx->stack);
        datum_stack_push(&ctx->stack, datum_apply(d1, op->op, d2));
    } break;
    case IR_POP_VAR: {
        Datum d = datum_stack_pop(&ctx->stack);
        if (d.type == DT_ERROR) {
            next.type = NIT_EXCEPTION;
            next.exception = d.error;
            return next;
        }
        char const *err = scope_assign_variable(ctx->scope, op->sv, d);
        if (err) {
            next.type = NIT_EXCEPTION;
            next.exception = err;
            return next;
        }
    } break;
    case IR_PUSH_INT_CONSTANT: {
        Datum d = { 0 };
        d.type = DT_I32;
        d.i32 = op->int_value;
        datum_stack_push(&ctx->stack, d);
    } break;
    case IR_PUSH_VAR: {
        Datum d = scope_variable_value(ctx->scope, op->sv);
        if (d.type == DT_ERROR) {
            next.type = NIT_EXCEPTION;
            next.exception = d.error;
            return next;
        }
        datum_stack_push(&ctx->stack, d);
    } break;
    case IR_RETURN: {
        NextInstructionPointer nip = { 0 };
        nip.type = NIT_RETURN;
        return nip;
    }
    default:
        UNREACHABLE();
    }
    return next;
}

typedef struct command {
    int        command;
    StringView command_str;
    size_t     num_arguments;
    StringView arguments[16];
} Command;

Command get_command()
{
    static char arguments[256];
    int         cmd = 0;
    Command     ret = { 0 };
    char       *current_arg = arguments;
    char       *last_arg = NULL;
    size_t      arg_count = 0;

    memset(arguments, 0, 256);
    printf("*> ");
    bool loop = true;
    while (loop) {
        int ch = toupper(getchar());
        switch (ch) {
        case ' ':
        case '\t':
            if (cmd) {
                loop = false;
            }
            break;
        case '\n':
            if (cmd == 0) {
                printf("*> ");
            } else if (strchr("BCLNQSTVX", cmd) == NULL) {
                printf("Unrecognized command '%c'\n", cmd);
            } else {
                ret.command = cmd;
                ret.command_str = sv_from(arguments);
                ret.num_arguments = 0;
                return ret;
            }
            break;
        default:
            if (!cmd) {
                cmd = ch;
            }
            arguments[strlen(arguments)] = (char) ch;
            break;
        }
    }

    ret.command = cmd;
    ret.command_str = sv_from(arguments);
    last_arg = arguments + strlen(arguments);
    current_arg = NULL;
    while (true) {
        if (strlen(arguments) >= 255) {
            fatal("Exhausted argument buffer");
        }
        int ch = getchar();
        switch (ch) {
        case ' ':
        case '\t': {
            if (current_arg) {
                ret.arguments[arg_count] = sv_from(current_arg);
                ret.num_arguments = ++arg_count;
                last_arg = current_arg;
                current_arg = NULL;
            }
        } break;
        case '\n':
            if (current_arg) {
                ret.arguments[arg_count] = sv_from(current_arg);
                ret.num_arguments = ++arg_count;
            }
            return ret;
        default: {
            if (!current_arg) {
                if (arg_count == 16) {
                    fatal("Argument array exhausted");
                }
                current_arg = last_arg + strlen(last_arg);
            }
            current_arg[strlen(current_arg)] = (char) ch;
        }
        }
    }
}

bool debug_processor(ExecutionContext *ctx, IRFunction *function, size_t ix)
{
    ir_operation_print(function->operations + ix);
    bool loop = true;
    while (loop) {
        Command command = get_command();
        switch (command.command) {
        case 'B': {
            if (sv_eq_cstr(command.command_str, "BP")) {
                for (size_t bp_ix = 0; bp_ix < ctx->num_breakpoints; ++bp_ix) {
                    Breakpoint *bp = &ctx->breakpoints[bp_ix];
                    printf("%*s" SV_SPEC " %zu\n", (int) (40 - sv_length(bp->function->name)), "", SV_ARG(bp->function->name), bp->index);
                }
                break;
            }
            if (ctx->num_breakpoints == MAX_BREAKPOINTS) {
                fatal("Exhausted breakpoint storage");
            }
            Breakpoint *bp = &ctx->breakpoints[ctx->num_breakpoints];
            bp->index = ix;
            bp->function = function;
            if (command.num_arguments) {
                bp->function = ir_program_function_by_name(ctx->program, command.arguments[0]);
                if (bp->function) {
                    bp->index = 1;
                    if (command.num_arguments > 1) {
                        if (!sv_tolong(command.arguments[1], (long *) &bp->index, NULL) || bp->index == 0 || bp->index >= function->num_operations) {
                            printf("Invalid instruction '" SV_SPEC "'\n", SV_ARG(command.arguments[1]));
                            bp->function = NULL;
                        }
                    }
                } else {
                    printf("Unknown function '" SV_SPEC "'\n", SV_ARG(command.arguments[0]));
                }
            }
            if (bp->function) {
                ctx->num_breakpoints++;
            }
        } break;
        case 'C':
            loop = false;
            ctx->execution_mode = EM_CONTINUE;
            return true;
        case 'L': {
            IRFunction *fnc = function;
            if (command.num_arguments) {
                fnc = ir_program_function_by_name(ctx->program, command.arguments[0]);
                if (!fnc) {
                    printf("Unknown function '" SV_SPEC "'\n", SV_ARG(command.arguments[0]));
                }
            }
            if (fnc) {
                ir_function_list(fnc, ix);
            }
        } break;
        case 'N':
            return true;
        case 'Q':
            return false;
        case 'S':
            datum_stack_dump(&ctx->stack);
            break;
        case 'T':
            scope_dump_call_stack(ctx->scope);
            break;
        case 'V':
            scope_dump_variables(ctx->scope);
            break;
        case 'X':
            ctx->execution_mode = EM_RUN_TO_RETURN;
            return true;
        default:
            UNREACHABLE();
        }
    }
}

FunctionReturn execute_function(ExecutionContext *ctx, IRFunction *function)
{
    Scope  func_scope = { 0 };
    Scope *current = ctx->scope;
    size_t ix = 0;

    func_scope.enclosing = ctx->root_scope;
    func_scope.owner = function;
    ctx->scope = &func_scope;

    if (ctx->execution_mode == EM_SINGLE_STEP) {
        ir_function_print(function);
        printf("\n");
    }
    while (ix < function->num_operations) {
        if (ctx->execution_mode == EM_RUN_TO_RETURN || ctx->execution_mode == EM_CONTINUE) {
            for (size_t bp = 0; bp < ctx->num_breakpoints; ++bp) {
                if (function == ctx->breakpoints[bp].function && function->operations[ix].index == ctx->breakpoints[bp].index) {
                    ctx->execution_mode = EM_SINGLE_STEP;
                }
            }
        }
        if (ctx->execution_mode == EM_SINGLE_STEP) {
            if (!debug_processor(ctx, function, ix)) {
                FunctionReturn ret = { 0 };
                ret.type = FRT_EXIT;
                ret.exit_code = 0;
                ctx->scope = current;
                return ret;
            }
        }
        switch (function->operations[ix].operation) {
        case IR_SCOPE_BEGIN: {
            Scope *new_scope = allocate(sizeof(Scope));
            new_scope->enclosing = ctx->scope;
            ctx->scope = new_scope;
            ix += 1;
        } break;
        case IR_SCOPE_END: {
            ctx->scope = ctx->scope->enclosing;
            if (ctx->scope == func_scope.enclosing) {
                fatal("Scope stack underflow");
            }
            ix += 1;
        } break;
        default: {
            NextInstructionPointer pointer = execute_operation(ctx, function->operations + ix);
            switch (pointer.type) {
            case NIT_RELATIVE:
                ix += pointer.pointer;
                break;
            case NIT_LABEL: {
                ix = ir_function_resolve_label(func_scope.owner, pointer.pointer);
            } break;
            case NIT_RESET:
                ix = 0;
                break;
            case NIT_RETURN:
                ix = function->num_operations;
                if (ctx->execution_mode == EM_RUN_TO_RETURN) {
                    ctx->execution_mode = EM_SINGLE_STEP;
                }
                break;
            case NIT_EXIT: {
                FunctionReturn ret = { 0 };
                ret.type = FRT_EXIT;
                ret.exit_code = (int) pointer.pointer;
                ctx->scope = current;
                return ret;
            }
            case NIT_EXCEPTION:
                printf("Exception caught: %s\n", pointer.exception);
                printf("\nInstruction:\n");
                ir_operation_print(function->operations + ix);
                printf("\nCall stack:\n");
                scope_dump_call_stack(ctx->scope);
                if (ctx->stack.top) {
                    printf("\nCurrent data stack:\n");
                    printf("-------------------\n");
                    datum_stack_dump(&ctx->stack);
                    printf("-------------------\n");
                } else {
                    printf("\nCurrent data stack EMPTY\n");
                }
                FunctionReturn ret = { 0 };
                ret.type = FRT_EXCEPTION;
                Datum exception = { 0 };
                exception.type = DT_ERROR;
                exception.error = pointer.exception;
                ctx->scope = current;
                return ret;
            }
        } break;
        }
    }
    if (ctx->execution_mode == EM_RUN_TO_RETURN) {
        ctx->execution_mode = EM_SINGLE_STEP;
    }
    FunctionReturn ret = { 0 };
    ret.type = FRT_NORMAL;
    Datum ret_val = { 0 };
    ret.return_value = ret_val;
    ctx->scope = current;
    return ret;
}

int execute(IRProgram program, bool debug /*, int argc, char **argv*/)
{
    assert(program.main >= 0);
    Scope root_scope = { 0 };
    root_scope.program = &program;
    ExecutionContext ctx = { 0 };
    ctx.program = &program;
    ctx.root_scope = &root_scope;
    ctx.execution_mode = (debug) ? EM_SINGLE_STEP : EM_RUN;
    execute_function(&ctx, program.functions + program.main);
    Datum d = datum_stack_pop(&ctx.stack);
    if (DatumType_is_integer(d.type)) {
        return (int) datum_signed_integer_value(d);
    }
    return 0;
}