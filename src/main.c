/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <binder.h>
#include <execute.h>
#include <graph.h>
#include <intermediate.h>
#include <parser.h>
#include <type.h>

int main(int argc, char **argv)
{
    bool   debug = false;
    bool   graph = false;
    char  *program_dir = NULL;
    int    scribble_param_count = 0;
    char **scribble_params = NULL;

    for (int ix = 1; ix < argc; ++ix) {
        if (!program_dir) {
            if (!strncmp(argv[ix], "--", 2)) {
                if (!strcmp(argv[ix], "--debug")) {
                    debug = true;
                } else if (!strcmp(argv[ix], "--graph")) {
                    graph = true;
                }
            } else {
                program_dir = argv[ix];
            }
        } else {
            scribble_param_count = argc - ix;
            scribble_params = argv + ix;
        }
    }
    type_registry_init();
    SyntaxNode *program = parse((program_dir) ? program_dir : ".");
    if (graph) {
        graph_program(program);
    }
    BoundNode *ast = bind(program);
    if (graph) {
        graph_ast(ast);
    }
    IRProgram ir = generate(ast);
    return execute(ir, debug);
}
