/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <fcntl.h>
#include <unistd.h>

#define STATIC_ALLOCATOR
#include <allocate.h>
#include <execute.h>
#include <native.h>
#include <options.h>

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

Datum                 *datum_stack_pop(DatumStack *stack);
void                   datum_stack_push(DatumStack *stack, Datum *datum);
void                   datum_stack_dump(DatumStack *stack);
CallStackEntry         call_stack_pop(CallStack *stack);
void                   call_stack_push(CallStack *stack, IRFunction *function, size_t index);
void                   call_stack_dump(CallStack *stack);
VarList               *scope_variable(Scope *scope, StringView name);
char const            *scope_declare_variable(Scope *scope, StringView name, type_id type);
char const            *scope_pop_variable(Scope *scope, StringView name, DatumStack *stack);
char const            *scope_push_variable(Scope *scope, StringView name, DatumStack *stack);
void                   scope_dump_call_stack(Scope *scope);
void                   scope_dump_variables(Scope *scope);
NextInstructionPointer execute_operation(ExecutionContext *ctx, IROperation *op);
FunctionReturn         execute_function(ExecutionContext *ctx, IRFunction *function);

#undef INTRINSIC_ENUM
#define INTRINSIC_ENUM(i) static Datum *execute_##i(ExecutionContext *ctx);
INTRINSICS(INTRINSIC_ENUM)
#undef INTRINSIC_ENUM

Datum *datum_stack_pop(DatumStack *stack)
{
    if (!stack->top) {
        Datum *error = datum_allocate(ERROR_ID);
        error->error = "Stack underflow";
        return error;
    }
    Datum *ret = stack->top->datum;
    stack->top = stack->top->prev;
    if (!stack->top) {
        stack->bottom = NULL;
    }
    return ret;
}

void datum_stack_push(DatumStack *stack, Datum *datum)
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

CallStackEntry call_stack_pop(CallStack *stack)
{
    assert(stack->top);
    CallStackEntry ret = *stack->top;
    stack->top = stack->top->down;
    if (!stack->top) {
        stack->bottom = NULL;
    }
    return ret;
}

void call_stack_push(CallStack *stack, IRFunction *function, size_t index)
{
    CallStackEntry *entry = allocate(sizeof(CallStackEntry));
    entry->function = function;
    entry->index = index;
    entry->down = stack->top;
    if (!stack->top && !stack->bottom) {
        stack->top = stack->bottom = entry;
    } else {
        assert(stack->top && stack->bottom);
        stack->top = entry;
    }
}

void call_stack_dump(CallStack *stack)
{
    for (CallStackEntry *entry = stack->top; entry; entry = entry->down) {
        printf("%*s" SV_SPEC " %zu\n", (int) (40 - sv_length(entry->function->name)), "", SV_ARG(entry->function->name), entry->index);
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

char const *scope_declare_variable(Scope *scope, StringView name, type_id type)
{
    VarList *end;
    for (end = scope->var_list; end && end->next; end = end->next) {
        if (sv_eq(end->name, name)) {
            return "Variable was previously declared";
        }
    }
    VarList *new = allocate(sizeof(VarList));
    new->name = name;
    new->type = type;
    if (end) {
        end->next = new;
    } else {
        scope->var_list = new;
    }
    return NULL;
}

char const *scope_pop_variable(Scope *scope, StringView name, DatumStack *stack)
{
    Scope *s = scope;
    while (s) {
        VarList *v = scope_variable(s, name);
        if (v) {
            Datum *d = datum_stack_pop(stack);
            if (d->type == ERROR_ID) {
                return d->error;
            }
            if (v->value) {
                datum_free(v->value);
            }
            v->value = d;
            return NULL;
        }
        s = s->enclosing;
    }
    return "Cannot assign undeclared variable";
}

char const *scope_pop_variable_component(Scope *scope, StringView name, size_t index, DatumStack *stack)
{
    Scope *s = scope;
    while (s) {
        VarList *v = scope_variable(s, name);
        if (v) {
            Datum *d = datum_stack_pop(stack);
            if (d->type == ERROR_ID) {
                return d->error;
            }
            Datum *component;
            switch (datum_kind(v->value)) {
            case TK_PRIMITIVE:
            case TK_VARIANT:
                return "Attempted pop of component of non-compound datum";
            case TK_COMPOSITE:
                if (index >= v->value->composite.num_components) {
                    return "Attempted pop of out-of-range component index";
                }
                component = v->value->composite.components + index;
                break;
            case TK_ARRAY:
                if (index >= v->value->array.size) {
                    return "Attempted pop of out-of-range component index";
                }
                component = v->value->array.components + index;
                break;
            default:
                UNREACHABLE();
            }
            assert(component);
            datum_copy(component, d);
            datum_free(d);
            return NULL;
        }
        s = s->enclosing;
    }
    return "Cannot assign undeclared variable";
}

char const *scope_push_variable(Scope *scope, StringView name, DatumStack *stack)
{
    Scope *s = scope;
    while (s) {
        VarList *v = scope_variable(s, name);
        if (v) {
            Datum *copy = datum_allocate(v->value->type);
            datum_copy(copy, v->value);
            datum_stack_push(stack, copy);
            return NULL;
        }
        s = s->enclosing;
    }
    return "Cannot get value of undeclared variable";
}

const char *scope_push_variable_component(Scope *scope, StringView name, size_t index, DatumStack *stack)
{
    Scope *s = scope;
    while (s) {
        VarList *v = scope_variable(s, name);
        if (v) {
            Datum *component;
            switch (datum_kind(v->value)) {
            case TK_PRIMITIVE:
            case TK_VARIANT:
                return "Attempted push of component of non-compound datum";
            case TK_COMPOSITE:
                if (index >= v->value->composite.num_components) {
                    return "Attempted push of out-of-range component index";
                }
                component = v->value->composite.components + index;
                break;
            case TK_ARRAY:
                if (index >= v->value->array.size) {
                    return "Attempted push of out-of-range component index";
                }
                component = v->value->array.components + index;
                break;
            default:
                UNREACHABLE();
            }
            assert(component);
            Datum *copy = datum_allocate(component->type);
            datum_copy(copy, component);
            datum_stack_push(stack, copy);
            return NULL;
        }
        s = s->enclosing;
    }
    return "Cannot get value of component of undeclared variable";
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
            ExpressionType *et = type_registry_get_type_by_id(var->type);
            assert(et);
            // FIXME Alignment of table
            printf("%5.5s" SV_SPEC "  %10.10s  " SV_SPEC, "", SV_ARG(var->name), "", SV_ARG(et->name));
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
        call_stack_push(&ctx->call_stack, ctx->function, ctx->index);
        IRAbstractFunction *function = NULL;
        function = ir_program_function_by_name(ctx->program, op->sv);
        assert(function);
        FunctionReturn func_ret;
        switch (function->kind) {
        case FK_SCRIBBLE:
            func_ret = execute_function(ctx, (IRFunction *) function);
            break;
        case FK_INTRINSIC:
            func_ret = execute_intrinsic(ctx, (IRIntrinsicFunction *) function);
            break;
        default:
            UNREACHABLE();
        }
        CallStackEntry entry = call_stack_pop(&ctx->call_stack);
        ctx->function = entry.function;
        ctx->index = entry.index;
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
        char const *err = scope_declare_variable(ctx->scope, op->var_decl.name, op->var_decl.type.type_id);
        if (err) {
            next.type = NIT_EXCEPTION;
            next.exception = err;
        }
    } break;
    case IR_JUMP: {
        next.type = NIT_LABEL;
        next.pointer = op->label;
        return next;
    }
    case IR_JUMP_F: {
        Datum *cond = datum_stack_pop(&ctx->stack);
        assert(cond->type == BOOL_ID);
        if (!cond->bool_value) {
            next.type = NIT_LABEL;
            next.pointer = op->label;
            datum_free(cond);
            return next;
        }
        datum_free(cond);
    } break;
    case IR_JUMP_T: {
        Datum *cond = datum_stack_pop(&ctx->stack);
        if (cond->type == ERROR_ID) {
            next.type = NIT_EXCEPTION;
            next.exception = cond->error;
            datum_free(cond);
            return next;
        }
        assert(cond->type == BOOL_ID);
        if (cond->bool_value) {
            next.type = NIT_LABEL;
            next.pointer = op->label;
            datum_free(cond);
            return next;
        }
        datum_free(cond);
    } break;
    case IR_LABEL:
        break;
    case IR_NATIVE_CALL: {
        Datum **args = allocate_array(Datum *, op->native.signature.argc);
        trace("Preparing native call of '%.*s'", SV_ARG(op->native.name));
        for (size_t ix = 0; ix < op->native.signature.argc; ++ix) {
            args[ix] = datum_stack_pop(&ctx->stack);
        }
        Datum *ret = datum_allocate(typeid_canonical_type_id(op->native.signature.ret_type->type_id));
        native_call(op->native.name, op->native.signature.argc, args, ret);
        datum_stack_push(&ctx->stack, ret);
    } break;
    case IR_OPERATOR: {
        Datum *d2 = datum_stack_pop(&ctx->stack);
        Datum *d1 = datum_stack_pop(&ctx->stack);
        datum_stack_push(&ctx->stack, datum_apply(d1, op->op, d2));
        datum_free(d1);
        datum_free(d2);
    } break;
    case IR_POP_VAR: {
        char const *err = scope_pop_variable(ctx->scope, op->sv, &ctx->stack);
        if (err) {
            next.type = NIT_EXCEPTION;
            next.exception = err;
            return next;
        }
    } break;
    case IR_POP_VAR_COMPONENT: {
        char const *err = scope_pop_variable_component(ctx->scope, op->var_component.name, op->var_component.component, &ctx->stack);
        if (err) {
            next.type = NIT_EXCEPTION;
            next.exception = err;
            return next;
        }
    } break;
    case IR_PUSH_BOOL_CONSTANT: {
        Datum *d = datum_allocate(BOOL_ID);
        d->bool_value = op->bool_value;
        datum_stack_push(&ctx->stack, d);
    } break;
    case IR_PUSH_INT_CONSTANT: {
        Datum *d = datum_make_integer(op->integer.width, op->integer.un_signed, op->integer.int_value, op->integer.int_value);
        datum_stack_push(&ctx->stack, d);
    } break;
    case IR_PUSH_STRING_CONSTANT: {
        Datum *d = datum_allocate(STRING_ID);
        d->string = op->sv;
        datum_stack_push(&ctx->stack, d);
    } break;
    case IR_PUSH_VAR: {
        char const *err = scope_push_variable(ctx->scope, op->sv, &ctx->stack);
        if (err) {
            next.type = NIT_EXCEPTION;
            next.exception = err;
            return next;
        }
    } break;
    case IR_PUSH_VAR_COMPONENT: {
        char const *err = scope_push_variable_component(ctx->scope, op->var_component.name, op->var_component.component, &ctx->stack);
        if (err) {
            next.type = NIT_EXCEPTION;
            next.exception = err;
            return next;
        }
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
            } else if (strchr("BCHLNOQRSTVX", cmd) == NULL) {
                printf("Unrecognized command '%c'\n", cmd);
                cmd = 0;
                printf("*> ");
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

bool set_breakpoint(ExecutionContext *ctx, StringView bp_function, StringView bp_index)
{
    if (ctx->num_breakpoints == MAX_BREAKPOINTS) {
        fatal("Exhausted breakpoint storage");
    }
    Breakpoint *bp = &ctx->breakpoints[ctx->num_breakpoints];
    bp->index = ctx->index;
    bp->function = ctx->function;
    if (sv_not_empty(bp_function)) {
        bp->function = (IRFunction *) ir_program_function_by_name(ctx->program, bp_function);
        if (bp->function) {
            if (bp->function->kind != FK_SCRIBBLE) {
                printf("Cannot set breakpoint in function '" SV_SPEC "'\n", SV_ARG(bp_function));
                bp->function = NULL;
            } else {
                bp->index = 1;
                if (sv_not_empty(bp_index)) {
                    if (!sv_tolong(bp_index, (long *) &bp->index, NULL) || bp->index == 0 || bp->index >= bp->function->num_operations) {
                        printf("Invalid instruction '" SV_SPEC "'\n", SV_ARG(bp_index));
                        bp->function = NULL;
                    }
                }
            }
        } else {
            printf("Unknown function '" SV_SPEC "'\n", SV_ARG(bp_function));
        }
    }
    if (bp->function) {
        ctx->num_breakpoints++;
    }
    return bp->function != NULL;
}

void debugger_help()
{
    printf("\nScribble debugger commands\n\n"
           "   b [function [index]] - Sets a breakpoint at the given index of the given\n"
           "                          function, or at the start of the function if no index\n"
           "                          was specified. If no function is specified either, a \n"
           "                          breakpoint is set at the current function and index,\n"
           "   bp                   - Lists all breakpoints.\n"
           "   c                    - (Re)start the program, allowing it to run to the next\n"
           "                          breakpoint or the end of the program.\n"
           "   h                    - Shows this text.\n"
           "   l [function]         - Lists the given function or the current function if no\n"
           "                          function is specified.\n"
           "   n                    - Step into.\n"
           "   o                    - Step over.\n"
           "   q                    - Quits the program and the debugger.\n"
           "   r                    - Toggles operation tracing.\n"
           "   s                    - Lists the current data stack.\n"
           "   t                    - Lists the current call stack.\n"
           "   v                    - Lists all variables, starting in the current scope\n"
           "                          walking up to the global scope.\n"
           "   x                    - Step out.\n\n");
}

bool debug_processor(ExecutionContext *ctx, IRFunction *function, size_t ix)
{
    ir_operation_print(function->operations + ix);
    while (true) {
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
            StringView bp_index = (command.num_arguments > 1) ? command.arguments[1] : sv_null();
            StringView bp_function = (command.num_arguments > 0) ? command.arguments[0] : sv_null();
            set_breakpoint(ctx, bp_function, bp_index);
        } break;
        case 'C':
            ctx->execution_mode = EM_CONTINUE;
            return true;
        case 'H':
            debugger_help();
            break;
        case 'L': {
            IRAbstractFunction *fnc = (IRAbstractFunction *) function;
            if (command.num_arguments) {
                fnc = ir_program_function_by_name(ctx->program, command.arguments[0]);
                if (!fnc) {
                    printf("Unknown function '" SV_SPEC "'\n", SV_ARG(command.arguments[0]));
                }
            }
            if (fnc) {
                switch (fnc->kind) {
                case FK_SCRIBBLE:
                    ir_function_list((IRFunction *) fnc, ix);
                    break;
                case FK_NATIVE: {
                    IRNativeFunction *native = (IRNativeFunction *) fnc;
                    printf(SV_SPEC " -> %s\n", SV_ARG(native->name), native->native_name);
                } break;
                case FK_INTRINSIC: {
                    IRIntrinsicFunction *intrinsic = (IRIntrinsicFunction *) fnc;
                    printf(SV_SPEC " -> intrinsic %s\n", SV_ARG(intrinsic->name), Intrinsic_name(intrinsic->intrinsic));
                } break;
                }
            }
        } break;
        case 'N':
            return true;
        case 'O':
            ctx->execution_mode = EM_STEP_OVER;
            return true;
        case 'Q':
            return false;
        case 'R':
            ctx->trace = !ctx->trace;
            printf("Tracing is %s\n", (ctx->trace) ? "ON" : "OFF");
            break;
        case 'S':
            datum_stack_dump(&ctx->stack);
            break;
        case 'T':
            call_stack_dump(&ctx->call_stack);
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
    ctx->function = function;

    bool step_over = ctx->execution_mode == EM_STEP_OVER;
    if (step_over) {
        ctx->execution_mode = EM_CONTINUE;
    }
    if (ctx->execution_mode == EM_SINGLE_STEP) {
        ir_function_print(function);
        printf("\n");
    }
    while (ix < function->num_operations) {
        ctx->index = function->operations[ix].index;
        if (ctx->execution_mode & (EM_RUN_TO_RETURN | EM_CONTINUE | EM_STEP_OVER)) {
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
        if (ctx->trace) {
            ir_operation_print(function->operations + ix);
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
            assert_msg(ctx->scope != func_scope.enclosing, "Scope stack underflow");
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
                call_stack_dump(&ctx->call_stack);
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
                exception.type = PT_ERROR;
                exception.error = pointer.exception;
                ctx->scope = current;
                return ret;
            }
        } break;
        }
    }
    if (ctx->execution_mode == EM_RUN_TO_RETURN || ctx->execution_mode == EM_STEP_OVER || step_over) {
        ctx->execution_mode = EM_SINGLE_STEP;
    }
    FunctionReturn ret = { 0 };
    ret.type = FRT_NORMAL;
    ret.return_value = NULL;
    ctx->scope = current;
    return ret;
}

Datum *execute_ALLOC(ExecutionContext *ctx)
{
    (void) ctx;
    Datum *ret = datum_allocate(POINTER_ID);
    Datum *size = datum_stack_pop(&ctx->stack);
    ret->pointer = allocate(datum_unsigned_integer_value(size));
    datum_free(size);
    return ret;
}

Datum *execute_ENDLN(ExecutionContext *ctx)
{
    (void) ctx;
    Datum *ret = datum_allocate(U64_ID);
    ret->u64 = endln();
    return ret;
}

Datum *execute_CLOSE(ExecutionContext *ctx)
{
    (void) ctx;
    Datum *fh = datum_stack_pop(&ctx->stack);
    Datum *ret = datum_allocate(I32_ID);
    ret->i32 = close((int) datum_signed_integer_value(fh));
    datum_free(fh);
    return ret;
}

Datum *execute_FPUTS(ExecutionContext *ctx)
{
    (void) ctx;
    Datum *fh = datum_stack_pop(&ctx->stack);
    assert(datum_is_integer(fh));
    Datum *s = datum_stack_pop(&ctx->stack);
    assert(s->type == STRING_ID);
    Datum *ret = datum_allocate(U32_ID);
    ret->u32 = write((int) datum_signed_integer_value(fh), s->string.ptr, s->string.length);
    datum_free(s);
    datum_free(fh);
    return ret;
}

Datum *execute_OPEN(ExecutionContext *ctx)
{
    (void) ctx;
    Datum *name = datum_stack_pop(&ctx->stack);
    assert(name->type == STRING_ID);
    Datum *mode = datum_stack_pop(&ctx->stack);
    assert(mode->type == I32_ID);
    Datum *ret = datum_allocate(I32_ID);
    char *file_name = alloca(name->string.length) + 1;
    file_name[name->string.length] = 0;
    memcpy(file_name, name->string.ptr, name->string.length);
    ret->i32 = open(file_name, mode->i32);
    datum_free(mode);
    datum_free(name);
    return ret;
}

Datum *execute_PUTI(ExecutionContext *ctx)
{
    (void) ctx;
    Datum *i = datum_stack_pop(&ctx->stack);
    assert(datum_is_integer(i));
    Datum *ret = datum_allocate(U64_ID);
    ret->u64 = putint(datum_signed_integer_value(i));
    datum_free(i);
    return ret;
}

Datum *execute_PUTLN(ExecutionContext *ctx)
{
    (void) ctx;
    Datum *s = datum_stack_pop(&ctx->stack);
    assert(s->type == STRING_ID);
    Datum *ret = datum_allocate(U32_ID);
    ret->u32 = write(1, s->string.ptr, s->string.length);
    if (ret->u32 == s->string.length) {
        ret->u32 += write(1, "\n", 1);
    }
    datum_free(s);
    return ret;
}

Datum *execute_READ(ExecutionContext *ctx)
{
    (void) ctx;
    Datum *fh = datum_stack_pop(&ctx->stack);
    assert(datum_is_integer(fh));
    Datum *buffer = datum_stack_pop(&ctx->stack);
    assert(buffer->type == POINTER_ID);
    Datum *bytes = datum_stack_pop(&ctx->stack);
    assert(datum_is_integer(bytes));
    Datum *ret = datum_allocate(I64_ID);
    ret->i64 = read((int) datum_signed_integer_value(fh), buffer->pointer, datum_unsigned_integer_value(fh));
    datum_free(bytes);
    datum_free(buffer);
    datum_free(fh);
    return ret;
}

Datum *execute_WRITE(ExecutionContext *ctx)
{
    (void) ctx;
    Datum *fh = datum_stack_pop(&ctx->stack);
    assert(datum_is_integer(fh));
    Datum *buffer = datum_stack_pop(&ctx->stack);
    assert(buffer->type == POINTER_ID);
    Datum *bytes = datum_stack_pop(&ctx->stack);
    assert(datum_is_integer(bytes));
    Datum *ret = datum_allocate(I64_ID);
    ret->i64 = write((int) datum_signed_integer_value(fh), buffer->pointer, datum_unsigned_integer_value(fh));
    datum_free(bytes);
    datum_free(buffer);
    datum_free(fh);
    return ret;
}

FunctionReturn execute_intrinsic(ExecutionContext *ctx, IRIntrinsicFunction *intrinsic)
{
    FunctionReturn ret = { 0 };
    ret.type = FRT_NORMAL;
    Datum *ret_val = NULL;

    switch (intrinsic->intrinsic) {
#undef INTRINSIC_ENUM
#define INTRINSIC_ENUM(i)           \
    case INT_##i:                   \
        ret_val = execute_##i(ctx); \
        break;
        INTRINSICS(INTRINSIC_ENUM)
#undef INTRINSIC_ENUM
    default:
        NYI("Intrinsic");
    }
    ret.return_value = ret_val;
    return ret;
}

int execute(IRProgram program /*, int argc, char **argv*/)
{
    assert(program.main >= 0);
    Scope root_scope = { 0 };
    root_scope.program = &program;
    ExecutionContext ctx = { 0 };
    ctx.program = &program;
    ctx.root_scope = &root_scope;
    ctx.execution_mode = EM_RUN;
    ctx.trace = false;
    if (OPT_DEBUG) {
        ctx.execution_mode = (OPT_RUN) ? EM_CONTINUE : EM_SINGLE_STEP;
        for (OptionList *breakpoint = get_option_values(sv_from("breakpoint")); breakpoint; breakpoint = breakpoint->next) {
            StringView bp[2];
            size_t     components = sv_split(breakpoint->value, sv_from(":"), 2, bp);
            StringView bp_index = (components > 1) ? bp[1] : sv_null();
            StringView bp_function = (components) ? bp[0] : sv_null();
            set_breakpoint(&ctx, bp_function, bp_index);
        }
    }
    execute_function(&ctx, (IRFunction *) (program.functions + program.main));
    Datum *d = datum_stack_pop(&ctx.stack);
    if (datum_is_integer(d)) {
        return (int) datum_signed_integer_value(d);
    }
    return 0;
}
