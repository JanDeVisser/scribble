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
uint64_t               datum_stack_pop_u64(DatumStack *stack);
StringView             datum_stack_pop_sv(DatumStack *stack);
void                   datum_stack_push(DatumStack *stack, Datum *datum);
void                   datum_stack_dump(DatumStack *stack);
CallStackEntry         call_stack_pop(CallStack *stack);
void                   call_stack_push(CallStack *stack, IRFunction *function, size_t index);
void                   call_stack_dump(CallStack *stack);
VarList               *scope_variable(Scope *scope, StringView name);
char const            *scope_declare_variable(Scope *scope, StringView name, type_id type);
char const            *scope_pop_variable(Scope *scope, StringView name, DatumStack *stack);
char const            *scope_push_variable(Scope *scope, StringView name, DatumStack *stack);
void                   scope_dump_variables(Scope *scope);
NextInstructionPointer execute_operation(ExecutionContext *ctx, IROperation *op);

#undef INTRINSIC_ENUM
#define INTRINSIC_ENUM(i) static Datum *execute_##i(ExecutionContext *ctx);
__attribute__((unused)) INTRINSICS(INTRINSIC_ENUM)
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

uint64_t datum_stack_pop_u64(DatumStack *stack)
{
    Datum *d = datum_stack_pop(stack);
    assert(d->type == U64_ID);
    uint64_t ret = d->u64;
    datum_free(d);
    return ret;
}

StringView datum_stack_pop_sv(DatumStack *stack)
{
    Datum *d = datum_stack_pop(stack);
    assert(d->type == STRING_ID);
    StringView ret = d->string;
    datum_free(d);
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
            case TK_AGGREGATE:
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

char const *scope_push_variable_component(Scope *scope, StringView name, size_t index, DatumStack *stack)
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
            case TK_AGGREGATE:
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
            printf(SV_SPEC_LALIGN " " SV_SPEC_LALIGN " ", SV_ARG_LALIGN(var->name, 20), SV_ARG_LALIGN(et->name, 20));
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
        IRFunction *function = NULL;
        function = ir_program_function_by_name(ctx->program, op->sv);
        assert(function);
        FunctionReturn func_ret;
        switch (function->kind) {
        case FK_SCRIBBLE:
            func_ret = execute_function(ctx, (IRFunction *) function);
            break;
        case FK_INTRINSIC:
            func_ret = execute_intrinsic(ctx, function);
            break;
        case FK_NATIVE: {
            Datum **args = allocate_array(Datum *, function->num_parameters);
            trace(CAT_EXECUTE, "Preparing native call of '%.*s'", SV_ARG(function->native_name));
            for (size_t ix = 0; ix < function->num_parameters; ++ix) {
                args[ix] = datum_stack_pop(&ctx->stack);
            }
            Datum *ret = datum_allocate(typeid_canonical_type_id(function->type.type_id));
            native_call(function->native_name, function->num_parameters, args, ret);
            datum_stack_push(&ctx->stack, ret);
        } break;
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
    case IR_DEFINE_ALIAS: {
        Datum *alias_of = datum_stack_pop(&ctx->stack);
        assert(alias_of->type == U64_ID);
        type_id         alias_for_id = datum_unsigned_integer_value(alias_of);
        ExpressionType *et = type_registry_get_type_by_name(op->sv);
        if (et) {
            if (type_kind(et) != TK_ALIAS) {
                fatal("Attempting to define '" SV_SPEC "' as an alias but it's already defined as a %s", SV_ARG(op->sv), TypeKind_name(type_kind(et)));
            }
            if (et->alias_for_id != alias_for_id) {
                fatal("Attempting to define '" SV_SPEC "' as an alias for '" SV_SPEC "' but it's already an alias for '" SV_SPEC "'",
                    SV_ARG(op->sv), SV_ARG(typeid_name(alias_for_id)), SV_ARG(et->name));
            }
        } else {
            MUST(TypeID, type_registry_alias(op->sv, alias_for_id));
        }
    } break;
    case IR_DEFINE_AGGREGATE: {
        size_t          num_components = datum_stack_pop_u64(&ctx->stack);
        ExpressionType *et = type_registry_get_type_by_name(op->sv);
        if (et) {
            if (type_kind(et) != TK_AGGREGATE) {
                fatal("Attempting to define '" SV_SPEC "' as an aggregate but it's already defined as a %s", SV_ARG(op->sv), TypeKind_name(type_kind(et)));
            }
            if (et->components.num_components != num_components) {
                fatal("Attempting to define '" SV_SPEC "' as an aggregate with %d components but it's already an aggregate with %d components",
                    SV_ARG(op->sv), num_components, et->components.num_components);
            }
        }
        TypeComponent *components = alloca(num_components * sizeof(TypeComponent));
        for (size_t ix = 0; ix < num_components; ++ix) {
            type_id    comp_type = datum_stack_pop_u64(&ctx->stack);
            StringView comp_name = datum_stack_pop_sv(&ctx->stack);
            if (et) {
                if (!sv_eq(et->components.components[ix].name, comp_name)) {
                    fatal("Attempting to define '" SV_SPEC "' as component %d of aggregate '" SV_SPEC "' but the component is already defined with name '%.*s'",
                        SV_ARG(comp_name), ix, SV_ARG(et->components.components[ix].name));
                }
                if (et->components.components[ix].type_id != comp_type) {
                    fatal("Attempting to define '" SV_SPEC "' as component with type '%.*s' of aggregate '" SV_SPEC "' but the component is already defined with type '%.*s'",
                        SV_ARG(comp_name), SV_ARG(typeid_name(comp_type)), SV_ARG(typeid_name(et->components.components[ix].type_id)));
                }
            }
            components[ix].kind = CK_TYPE;
            components[ix].name = comp_name;
            components[ix].type_id = comp_type;
        }
        if (!et) {
            type_id new_id = MUST(TypeID, type_registry_make_type(op->sv, TK_AGGREGATE));
            MUST(TypeID, type_set_struct_components(new_id, num_components, components));
        }
    } break;
    case IR_DEFINE_ARRAY: {
        size_t          size = datum_stack_pop_u64(&ctx->stack);
        type_id         type = datum_stack_pop_u64(&ctx->stack);
        ExpressionType *et = type_registry_get_type_by_name(op->sv);
        if (et) {
            if (type_kind(et) != TK_ARRAY) {
                fatal("Attempting to define '" SV_SPEC "' as an array but it's already defined as a %s", SV_ARG(op->sv), TypeKind_name(type_kind(et)));
            }
            if (et->array.base_type.type_id != type) {
                fatal("Attempting to define '" SV_SPEC "' as an array of '" SV_SPEC "' but it's already an array of '" SV_SPEC "'",
                    SV_ARG(op->sv), SV_ARG(typeid_name(type)), typeid_name(et->array.base_type.type_id));
            }
            if (et->array.size != size) {
                fatal("Attempting to define '" SV_SPEC "' as an array of '" SV_SPEC "' with %d elements but it's already an array of %d elements",
                    SV_ARG(op->sv), size, et->array.size);
            }
        } else {
            MUST(TypeID, type_registry_array(op->sv, type, size));
        }
    } break;
    case IR_DEFINE_VARIANT: {
        size_t          num_variants = datum_stack_pop_u64(&ctx->stack);
        ExpressionType *et = type_registry_get_type_by_name(op->sv);
        if (et) {
            if (type_kind(et) != TK_VARIANT) {
                fatal("Attempting to define '" SV_SPEC "' as a variant but it's already defined as a %s", SV_ARG(op->sv), TypeKind_name(type_kind(et)));
            }
            if (et->components.num_components != num_variants) {
                fatal("Attempting to define '" SV_SPEC "' as a variant with %d variants but it's already a variant with %d variants",
                    SV_ARG(op->sv), num_variants, et->components.num_components);
            }
        }
        type_id *options = alloca(num_variants * sizeof(type_id));
        for (size_t ix = 0; ix < num_variants; ++ix) {
            type_id opt_type = datum_stack_pop_u64(&ctx->stack);
            if (et) {
                if (et->components.components[ix].type_id != opt_type) {
                    fatal("Attempting to define '" SV_SPEC "' as option %d of variant '" SV_SPEC "' but the variant is already defined with type '%.*s'",
                        typeid_name(opt_type), ix, typeid_name(et->components.components[ix].type_id));
                }
            }
            options[ix] = opt_type;
        }
        if (!et) {
            MUST(TypeID, type_registry_get_variant(num_variants, options));
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
    case IR_NEW_DATUM: {
        type_id tid = typeid_canonical_type_id(op->integer.value.unsigned_value);
        assert(typeid_kind(tid) == TK_AGGREGATE || typeid_kind(tid) == TK_ARRAY);
        ExpressionType *et = type_registry_get_type_by_id(tid);
        Datum          *new_datum = datum_allocate(tid);
        switch (typeid_kind(tid)) {
        case TK_AGGREGATE: {
            for (int ix = (int) et->components.num_components - 1; ix >= 0; --ix) {
                Datum *d = datum_stack_pop(&ctx->stack);
                datum_copy(new_datum->composite.components + ix, d);
                datum_free(d);
            }
        } break;
        case TK_ARRAY: {
            for (int ix = (int) et->array.size - 1; ix >= 0; --ix) {
                Datum *d = datum_stack_pop(&ctx->stack);
                datum_copy(new_datum->array.components + ix, d);
                datum_free(d);
            }
        } break;
        default:
            UNREACHABLE();
        }
        datum_stack_push(&ctx->stack, new_datum);
    } break;
    case IR_OPERATOR: {
        Datum *d2 = datum_stack_pop(&ctx->stack);
        Datum *d1 = datum_stack_pop(&ctx->stack);
        datum_stack_push(&ctx->stack, datum_apply(d1, op->operator.op, d2));
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
    case IR_PUSH_FLOAT_CONSTANT: {
        Datum *d = datum_allocate(FLOAT_ID);
        d->float_value = op->double_value;
        datum_stack_push(&ctx->stack, d);
    } break;
    case IR_PUSH_INT_CONSTANT: {
        Datum *d = datum_make_integer(op->integer);
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
    char       *current_arg;
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
        bp->function = ir_program_function_by_name(ctx->program, bp_function);
        if (bp->function) {
            if (bp->function->kind != FK_SCRIBBLE) {
                printf("Cannot set breakpoint in function '" SV_SPEC "'\n", SV_ARG(bp_function));
                bp->function = NULL;
            } else {
                bp->index = 1;
                if (sv_not_empty(bp_index)) {
                    if (!sv_tolong(bp_index, (long *) &bp->index, NULL) || bp->index == 0 || bp->index >= bp->function->operations.size) {
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
    ir_operation_print(function->operations.elements + ix);
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
            IRFunction *fnc = function;
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
                    printf(SV_SPEC " -> %.*s\n", SV_ARG(fnc->name), SV_ARG(fnc->native_name));
                } break;
                case FK_INTRINSIC: {
                    printf(SV_SPEC " -> intrinsic %s\n", SV_ARG(fnc->name), Intrinsic_name(fnc->intrinsic));
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

    for (size_t param_ix = 0; param_ix < function->num_parameters; ++param_ix) {
        IRVarDecl  *var_decl = function->parameters + param_ix;
        char const *err = scope_declare_variable(ctx->scope, var_decl->name, var_decl->type.type_id);
        if (!err) {
            err = scope_pop_variable(ctx->scope, var_decl->name, &ctx->stack);
        }
        if (err) {
            printf("Exception caught: %s\n", err);
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
            exception.type = BIT_ERROR;
            exception.error = err;
            ctx->scope = current;
            return ret;
        }
    }

    bool step_over = ctx->execution_mode == EM_STEP_OVER;
    if (step_over) {
        ctx->execution_mode = EM_CONTINUE;
    }
    if (ctx->execution_mode == EM_SINGLE_STEP) {
        ir_function_print(function);
        printf("\n");
    }
    while (ix < function->operations.size) {
        ctx->index = function->operations.elements[ix].index;
        if (ctx->execution_mode & (EM_RUN_TO_RETURN | EM_CONTINUE | EM_STEP_OVER)) {
            for (size_t bp = 0; bp < ctx->num_breakpoints; ++bp) {
                if (function == ctx->breakpoints[bp].function && function->operations.elements[ix].index == ctx->breakpoints[bp].index) {
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
            ir_operation_print(function->operations.elements + ix);
        }
        switch (function->operations.elements[ix].operation) {
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
            NextInstructionPointer pointer = execute_operation(ctx, function->operations.elements + ix);
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
                ix = function->operations.size;
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
                ir_operation_print(function->operations.elements + ix);
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
                exception.type = BIT_ERROR;
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
    char  *file_name = alloca(name->string.length) + 1;
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

FunctionReturn execute_intrinsic(ExecutionContext *ctx, IRFunction *intrinsic)
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
    IRFunction *main = ir_program_function_by_name(&program, sv_from("main"));
    assert(main);
    Scope root_scope = { 0 };
    root_scope.program = &program;
    ExecutionContext ctx = { 0 };
    ctx.program = &program;
    ctx.root_scope = &root_scope;
    ctx.execution_mode = EM_RUN;
    ctx.trace = false;
    if (OPT_DEBUG) {
        StringList breakpoints = get_option_values(sv_from("breakpoint"));
        for (size_t ix = 0; ix < sl_size(&breakpoints); ++ix) {
            StringList components = sv_split(breakpoints.strings[ix], sv_from(":"));
            StringView bp_index = (sl_size(&components) > 1) ? components.strings[1] : sv_null();
            StringView bp_function = (!sl_empty(&components)) ? components.strings[0] : sv_null();
            set_breakpoint(&ctx, bp_function, bp_index);
        }
    }
    for (size_t ix = 0; ix < program.modules.size; ++ix) {
        IRModule *module = program.modules.elements + ix;
        if (module->$static >= 0) {
            if (OPT_STATIC && OPT_DEBUG) {
                ctx.execution_mode = EM_SINGLE_STEP;
            }
            execute_function(&ctx, module->functions.elements + module->$static);
            ctx.execution_mode = EM_RUN;
        }
    }
    if (OPT_DEBUG) {
        ctx.execution_mode = (OPT_RUN) ? EM_CONTINUE : EM_SINGLE_STEP;
    }
    execute_function(&ctx, main);
    Datum *d = datum_stack_pop(&ctx.stack);
    if (datum_is_integer(d)) {
        return (int) datum_signed_integer_value(d);
    }
    return 0;
}
