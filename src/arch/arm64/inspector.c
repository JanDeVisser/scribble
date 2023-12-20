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
                                     "   h                    - Shows this text.\n"
                                     "   l [function]         - Lists the given function or the current function if no\n"
                                     "                          function is specified.\n"
                                     "   n                    - Step into.\n"
                                     "   o                    - Step over.\n"
                                     "   q                    - Quits the program and the debugger.\n"
                                     "   r                    - Toggles operation tracing.\n"
                                     "   s                    - Lists the current location stack.\n"
                                     "   t                    - Lists the current call stack.\n"
                                     "   x                    - Step out.\n"
                                     "   y [type]             - Describe the given type or list all types if no\n"
                                     "                          type is specified.\n\n";

bool arm64_inspect(ObserverContext *ctx, ExecutionMessage msg)
{
    switch (msg.type) {
    case EMT_OBSERVER_INIT: {
        ObserverRegistry *registry = (ObserverRegistry *) msg.payload;
        assert(registry->processor == arm64_inspect);
        registry->custom_commands = "S";
        registry->help_text = s_debugger_help;
    } break;
    case EMT_FUNCTION_ENTRY: {
        ctx->execution_function = msg.payload;
        break;
    }
    case EMT_ON_INSTRUCTION: {
        switch (ctx->command.command) {
        case 'S':
            assert(ctx->execution_function);
            arm64function_location_stack_dump((ARM64Function *) ctx->execution_function);
            break;
        default:
            UNREACHABLE();
        }
    }
    default:
        break;
    }
    return true;
}
