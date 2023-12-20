/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ctype.h>

#define STATIC_ALLOCATOR
#include <allocate.h>
#include <datum.h>
#include <debugger.h>
#include <execute.h>
#include <options.h>

static ObserverStack *stack_free_list = NULL;

static ObserverRegistry s_observers[16] = { 0 };

DA_IMPL(Breakpoint)

static ObserverStack *allocate_observer_stack()
{
    ObserverStack *ret = NULL;
    if (stack_free_list) {
        ret = stack_free_list;
        stack_free_list = ret->prev;
        memset(ret, 0, sizeof(ObserverStack));
    } else {
        ret = allocate_new(ObserverStack);
    }
    return ret;
}

static void free_debugger_stack(ObserverStack *stack)
{
    stack->prev = stack_free_list;
    stack_free_list = stack;
}

void debug_describe_type(ExpressionType *type)
{
    bool concrete = type_is_concrete(type);
    printf("%s %.*s", TypeKind_tag(type_kind(type)), SV_ARG(type->name));
    if (type->num_arguments > 0) {
        printf("<");
        for (size_t ix = 0; ix < type->num_arguments; ++ix) {
            if (ix > 0) {
                printf(", ");
            }
            printf("%.*s=", SV_ARG(type->template_arguments[ix].name));
            switch (type->template_arguments[ix].arg_type) {
            case TPT_TYPE:
                printf("%.*s", SV_ARG(typeid_name(type->template_arguments[ix].type)));
                break;
            case TPT_NUMBER:
                printf("%lld", type->template_arguments[ix].int_value);
                break;
            case TPT_STRING:
                printf("\"%.*s\"", SV_ARG(type->template_arguments[ix].string_value));
                break;
            default:
                UNREACHABLE();
            }
        }
        printf(">");
    }
    if (typeid_canonical_type_id(type->type_id) != type->type_id) {
        type_id canonical = typeid_canonical_type_id(type->type_id);
        printf(" -> %s %.*s", TypeKind_name(typeid_kind(canonical)), SV_ARG(typeid_name(canonical)));
    }
    switch (type_kind(type)) {
    case TK_PRIMITIVE: {
        BuiltinType builtin = type->builtin_type;
        printf(" : %s", BuiltinType_name(builtin));
    } break;
    case TK_AGGREGATE: {
        printf(" {\n");
        for (size_t ix = 0; ix < type->components.num_components; ++ix) {
            TypeComponent *component = type->components.components + ix;
            printf("    %.*s", SV_ARG(component->name));
            switch (component->kind) {
            case CK_TYPE: {
                printf(": %.*s", SV_ARG(typeid_name(component->type_id)));
                if (concrete) {
                    printf(" @%zu", typeid_offsetof(type->type_id, ix));
                }
            } break;
            case CK_TEMPLATE_PARAM: {
                printf(": %.*s", SV_ARG(component->param));
                break;
            }
            case CK_PARAMETERIZED_TYPE: {
                printf(": %.*s<%.*s=%.*s>",
                    SV_ARG(typeid_name(component->parameterized_type.template_type)),
                    SV_ARG(component->parameterized_type.parameter),
                    SV_ARG(component->parameterized_type.argument));
            } break;
            }
            printf("\n");
        }
        printf("}");
    } break;
    case TK_VARIANT: {
        printf(" : %.*s", SV_ARG(typeid_name(type->variant.enumeration)));
        printf(" {\n");
        ExpressionType *enum_type = type_registry_get_type_by_id(type->variant.enumeration);
        for (size_t ix = 0; enum_type->enumeration.size; ++ix) {
            printf("    %.*s: %.*s",
                SV_ARG(type->enumeration.elements[ix].name),
                SV_ARG(type->variant.elements[ix].name));
            printf("\n");
        }
        printf("}");
        if (concrete) {
            printf(" @%zu", typeid_offsetof_payload(type->type_id));
        }
    }
    case TK_ENUM: {
        printf(" {\n");
        for (size_t ix = 0; type->enumeration.size; ++ix) {
            printf("    %.*s: %.*s",
                SV_ARG(type->enumeration.elements[ix].name),
                SV_ARG(sv_render_integer(type->enumeration.elements[ix].value)));
            printf("\n");
        }
        printf("}");
    } break;
    case TK_ALIAS:
        break;
    default:
        UNREACHABLE();
    }
    if (concrete) {
        printf(" (%zu bytes)", typeid_sizeof(type->type_id));
    }
    printf("\n");
}

void debug_list_types()
{
    for (size_t ix = 4; ix < NEXT_CUSTOM_IX; ++ix) {
        ExpressionType *et = type_registry_get_type_by_index(ix);
        printf("%-20.20s%.*s\n", TypeKind_tag(type_kind(et)), SV_ARG(typeid_name(et->type_id)));
    }
}

Command debug_get_command(char const *custom)
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
            } else if (strchr("BCHLNOQRSTVXY", cmd) == NULL && strchr(custom, cmd) == NULL) {
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

bool debug_set_breakpoint(ObserverContext *ctx, StringView bp_function, StringView bp_index)
{
    Breakpoint bp = { 0 };
    bp.index = ctx->stack->index;
    bp.function = ctx->stack->function;
    if (sv_not_empty(bp_function)) {
        bp.function = ir_program_function_by_name(ctx->program, bp_function);
        if (bp.function) {
            if (bp.function->kind != FK_SCRIBBLE) {
                printf("Cannot set breakpoint in function '" SV_SPEC "'\n", SV_ARG(bp_function));
                bp.function = NULL;
            } else {
                bp.index = 1;
                if (sv_not_empty(bp_index)) {
                    IntegerParseResult parse_result = sv_parse_i64(bp_index);
                    if (!parse_result.success) {
                        printf("Invalid instruction '" SV_SPEC "'\n", SV_ARG(bp_index));
                        bp.function = NULL;
                    } else {
                        bp.index = parse_result.integer.i64;
                        if (bp.index == 0 || bp.index >= bp.function->operations.size) {
                            printf("Invalid instruction '" SV_SPEC "'\n", SV_ARG(bp_index));
                            bp.function = NULL;
                        }
                    }
                }
            }
        } else {
            printf("Unknown function '" SV_SPEC "'\n", SV_ARG(bp_function));
        }
    }
    if (bp.function) {
        da_append_Breakpoint(&ctx->breakpoints, bp);
    }
    return bp.function != NULL;
}

static char const *s_debugger_help = "\nScribble debugger commands\n\n"
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
                                     "   x                    - Step out.\n"
                                     "   y [type]             - Describe the given type or list all types if no\n"
                                     "                          type is specified.\n\n";

bool debug_processor(ObserverContext *ctx, ExecutionMessage msg)
{
    if (msg.type == EMT_OBSERVER_INIT) {
        ObserverRegistry *registry = (ObserverRegistry *) msg.payload;
        assert(registry->processor == debug_processor);
        registry->custom_commands = "SV";
        registry->help_text = s_debugger_help;
        return true;
    }
    if (msg.type != EMT_ON_INSTRUCTION) {
        return true;
    }
    ExecutionContext *execution_context = (ExecutionContext *) ctx->execution_context;
    switch (ctx->command.command) {
    case 'S':
        datum_stack_dump(&execution_context->stack);
        break;
    case 'V':
        scope_dump_variables(execution_context->scope);
        break;
    default:
        UNREACHABLE();
    }
    return true;
}

bool process(ObserverContext *ctx, ExecutionMessage msg, ObserverRegistry registry)
{
    IROperation *instruction = (IROperation *) msg.payload;

    while (true) {
        ctx->command = debug_get_command(registry.custom_commands);
        switch (ctx->command.command) {
        case 'B': {
            if (sv_eq_cstr(ctx->command.command_str, "BP")) {
                for (size_t bp_ix = 0; bp_ix < ctx->breakpoints.size; ++bp_ix) {
                    Breakpoint *bp = ctx->breakpoints.elements + bp_ix;
                    printf("%*s" SV_SPEC " %zu\n", (int) (40 - sv_length(bp->function->name)), "", SV_ARG(bp->function->name), bp->index);
                }
                break;
            }
            StringView bp_index = (ctx->command.num_arguments > 1) ? ctx->command.arguments[1] : sv_null();
            StringView bp_function = (ctx->command.num_arguments > 0) ? ctx->command.arguments[0] : sv_null();
            debug_set_breakpoint(ctx, bp_function, bp_index);
        } break;
        case 'C':
            ctx->execution_mode = EM_CONTINUE;
            return true;
        case 'H':
            printf("%s", s_debugger_help);
            break;
        case 'L': {
            IRFunction *fnc = ctx->stack->function;
            if (ctx->command.num_arguments) {
                fnc = ir_program_function_by_name(ctx->program, ctx->command.arguments[0]);
                if (!fnc) {
                    printf("Unknown function '" SV_SPEC "'\n", SV_ARG(ctx->command.arguments[0]));
                }
            }
            if (fnc) {
                switch (fnc->kind) {
                case FK_SCRIBBLE:
                    ir_function_list((IRFunction *) fnc, instruction->index);
                    break;
                case FK_NATIVE: {
                    printf(SV_SPEC " -> %.*s\n", SV_ARG(fnc->name), SV_ARG(fnc->native_name));
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
        case 'T': {
            for (ObserverStack *entry = ctx->stack; entry; entry = entry->prev) {
                printf("%*s" SV_SPEC " %zu\n", (int) (40 - sv_length(entry->function->name)), "", SV_ARG(entry->function->name), entry->index);
            }
        } break;
        case 'X':
            ctx->execution_mode = EM_RUN_TO_RETURN;
            return true;
        case 'Y': {
            if (ctx->command.num_arguments) {
                ExpressionType *type = type_registry_get_type_by_name(ctx->command.arguments[0]);
                if (type == NULL) {
                    printf("Unknown type '" SV_SPEC "'\n", SV_ARG(ctx->command.arguments[0]));
                } else {
                    debug_describe_type(type);
                }
            } else {
                debug_list_types();
            }
        } break;
        default:
            if (!registry.processor(ctx, msg)) {
                return false;
            }
        }
    }
}

bool debug_execution_observer(void *context, ExecutionMessage msg)
{
    ObserverContext *ctx = NULL;
    for (size_t ix = 0; ix < 16 && s_observers[ix].processor != NULL; ++ix) {
        ctx = s_observers[ix].context;
        if (ctx == NULL) {
            ctx = allocate_new(ObserverContext);
            ctx->execution_context = context;
            s_observers[ix].context = ctx;
        }
        switch (msg.type) {
        case EMT_OBSERVER_INIT:
            return s_observers[ix].processor(ctx, msg);
        case EMT_PROGRAM_START: {
            ctx->program = (IRProgram *) msg.payload;
            StringList breakpoints = get_option_values(sv_from("breakpoint"));
            for (size_t bp_ix = 0; bp_ix < sl_size(&breakpoints); ++bp_ix) {
                StringList components = sv_split(breakpoints.strings[bp_ix], sv_from(":"));
                StringView bp_index = (sl_size(&components) > 1) ? components.strings[1] : sv_null();
                StringView bp_function = (!sl_empty(&components)) ? components.strings[0] : sv_null();
                debug_set_breakpoint(ctx, bp_function, bp_index);
            }
            ctx->execution_mode = (OPT_RUN) ? EM_CONTINUE : EM_SINGLE_STEP;
            return s_observers[ix].processor(ctx, msg);
        }
        case EMT_FUNCTION_ENTRY: {
            ObserverStack *entry = allocate_observer_stack();
            entry->function = (IRFunction *) msg.payload;
            entry->step_over = ctx->execution_mode == EM_STEP_OVER;
            if (entry->step_over) {
                ctx->execution_mode = EM_CONTINUE;
            }
            entry->prev = ctx->stack;
            ctx->stack = entry;
            return s_observers[ix].processor(ctx, msg);
        } break;
        case EMT_ON_INSTRUCTION: {
            IROperation *instruction = (IROperation *) msg.payload;
            if (ctx->execution_mode & (EM_RUN_TO_RETURN | EM_CONTINUE | EM_STEP_OVER)) {
                for (size_t bp = 0; bp < ctx->breakpoints.size; ++bp) {
                    if (ctx->stack->function == ctx->breakpoints.elements[bp].function && instruction->index == ctx->breakpoints.elements[bp].index) {
                        ctx->execution_mode = EM_SINGLE_STEP;
                    }
                }
            }

            if (ctx->execution_mode != EM_SINGLE_STEP) {
                if (ctx->trace) {
                    ir_operation_print(instruction);
                }
                break;
            }
            ir_operation_print(instruction);
            return process(ctx, msg, s_observers[ix]);
        }
        case EMT_AFTER_INSTRUCTION:
            return s_observers[ix].processor(ctx, msg);
        case EMT_FUNCTION_RETURN: {
            bool           ret = s_observers[ix].processor(ctx, msg);
            ObserverStack *entry = ctx->stack;
            if (ctx->execution_mode == EM_RUN_TO_RETURN || ctx->execution_mode == EM_STEP_OVER || entry->step_over) {
                ctx->execution_mode = EM_SINGLE_STEP;
            }
            ctx->stack = entry->prev;
            free_debugger_stack(entry);
            return ret;
        }
        case EMT_PROGRAM_EXIT: {
            bool ret = s_observers[ix].processor(ctx, msg);
            ctx->program = NULL;
            s_observers[ix].context = NULL;
            return ret;
        }
        default:
            UNREACHABLE();
        }
    }
    return true;
}

void register_execution_observer(ObservationProcessor observer)
{
    for (size_t ix = 0; ix < 16; ++ix) {
        if (s_observers[ix].processor == NULL) {
            s_observers[ix].processor = observer;
            observer(NULL, (ExecutionMessage) { .type = EMT_OBSERVER_INIT, .payload = &s_observers[ix] });
            return;
        }
    }
}
