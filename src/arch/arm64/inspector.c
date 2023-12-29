/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <arm64.h>
#include <debugger.h>
#include <options.h>

static char const *s_debugger_help = "\nScribble ARM64 Code Generation Inspector\n\n"
                                     "   b [function [index]] - Sets a breakpoint at the given index of the given\n"
                                     "                          function, or at the start of the function if no index\n"
                                     "                          was specified. If no function is specified either, a \n"
                                     "                          breakpoint is set at the current function and index,\n"
                                     "   bp                   - Lists all breakpoints.\n"
                                     "   c                    - (Re)start the program, allowing it to run to the next\n"
                                     "                          breakpoint or the end of the program.\n"
                                     "   e                    - Echo generated assembly code.\n"
                                     "   f                    - Follow (trace) location stack.\n"
                                     "   h                    - Shows this text.\n"
                                     "   l [function]         - Lists the given function or the current function if no\n"
                                     "                          function is specified.\n"
                                     "   n                    - Step into.\n"
                                     "   o                    - Step over.\n"
                                     "   q                    - Quits the program and the debugger.\n"
                                     "   r                    - Toggles operation tracing.\n"
                                     "   s                    - Lists the current location stack.\n"
                                     "   t                    - Lists the current call stack.\n"
                                     "   v                    - Lists the definitions all variables in the current\n"
                                     "                          function.\n"
                                     "   x                    - Step out.\n"
                                     "   y [type]             - Describe the given type or list all types if no\n"
                                     "                          type is specified.\n\n";

void show_registers(ARM64Function *function)
{
    printf("Registers: ");
    size_t allocated = 0;
    for (size_t ix = 0; ix < REG_V31 + 1; ++ix) {
        if (function->scribble.registers[ix]) {
            printf("%s ", x_reg((Register) ix));
            ++allocated;
        }
    }
    if (allocated == 0) {
        printf("--");
    }
    printf("\n\n");
}

void show_stack(ARM64Function *function)
{
    printf("Location stack:\n");
    if (arm64function_location_stack_dump(function) == 0) {
        printf("  -- empty --\n");
    }
    printf("\n");
}

bool arm64_inspect(ObserverContext *ctx, ExecutionMessage msg)
{
    switch (msg.type) {
    case EMT_STAGE_INIT: {
        ObserverRegistry *registry = (ObserverRegistry *) msg.payload;
        assert(registry->processor == arm64_inspect);
        registry->custom_commands = "EFSV";
        registry->help_text = s_debugger_help;
    } break;
    case EMT_FUNCTION_ENTRY: {
        printf("*** Function %.*s ***\n", SV_ARG(ctx->stack->function->name));
        break;
    }
    case EMT_ON_INSTRUCTION: {
        switch (ctx->command.command) {
        case 'E': {
            ARM64Function *function = (ARM64Function *) ctx->stack->function->data;
            function->scribble.echo = !function->scribble.echo;
            printf("Assembly echo is %s\n", (function->scribble.echo) ? "ON" : "OFF");
        } break;
        case 'F': {
            ARM64Function *function = (ARM64Function *) ctx->stack->function->data;
            function->scribble.follow_stack = true;
            function->scribble.follow_registers = true;
            if (sv_eq_cstr(ctx->command.command_str, "FR")) {
                function->scribble.follow_stack = false;
            }
            if (sv_eq_cstr(ctx->command.command_str, "FS")) {
                function->scribble.follow_registers = false;
            }
            printf("Follow location stack is %s\n", (function->scribble.follow_stack) ? "ON" : "OFF");
            printf("Follow register allocation is %s\n", (function->scribble.follow_registers) ? "ON" : "OFF");
        } break;
        case 'S':
            show_stack((ARM64Function *) ctx->stack->function->data);
            break;
        case 'V':
            arm64function_variables_dump((ARM64Function *) ctx->stack->function->data);
            break;
        default:
            UNREACHABLE();
        }
        break;
    }
    case EMT_AFTER_INSTRUCTION: {
        ARM64Function *function = (ARM64Function *) ctx->stack->function->data;
        if (function->scribble.follow_stack) {
            show_stack(function);
        }
        if (function->scribble.follow_registers) {
            show_registers(function);
        }
    } break;
    case EMT_FUNCTION_RETURN: {
        ARM64Function *function = (ARM64Function *) ctx->stack->function->data;
        function->scribble.echo = false;
        printf("*** End function %.*s ***\n", SV_ARG(ctx->stack->function->name));
        break;
    }
    default:
        break;
    }
    return true;
}
