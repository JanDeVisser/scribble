/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <binder.h>
#include <config.h>
#include <execute.h>
#include <graph.h>
#include <intermediate.h>
#include <options.h>
#include <parser.h>
#include <type.h>
#include <arch/arm64/arm64.h>

int main(int argc, char **argv)
{
    char  *program_dir_or_file = NULL;
    int    scribble_param_count = 0;
    char **scribble_params = NULL;

    for (int ix = 1; ix < argc; ++ix) {
        if (!program_dir_or_file) {
            if (!strncmp(argv[ix], "--", 2) && (strlen(argv[ix]) > 2)) {
                StringView option = sv_from(argv[ix] + 2);
                StringView value = sv_from("true");
                char const *equals = strchr(argv[ix]+2, '=');
                if (equals) {
                    option = (StringView) { argv[ix] + 2, equals - argv[ix] - 2 };
                    value = sv_from(equals + 1);
                }
                set_option(option, value);
            } else {
                program_dir_or_file = argv[ix];
            }
        } else {
            scribble_param_count = argc - ix;
            scribble_params = argv + ix;
        }
    }
    set_option(sv_from("scribble-dir"), sv_from(SCRIBBLE_DIR));
    log_init(OPT_TRACE);
    type_registry_init();

    SyntaxNode *program = parse((program_dir_or_file) ? program_dir_or_file : ".");
    if (OPT_GRAPH) {
        graph_program(program);
        register_binding_observer(graph_ast);
    }
    BoundNode *ast = bind(program);
    IRProgram ir = generate(ast);
    MUST_VOID(Int, output_arm64(&ir));
}
