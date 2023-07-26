/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <binder.h>
#include <graph.h>
#include <parser.h>
#include <type.h>

int main(int argc, char **argv)
{
    type_registry_init();
    SyntaxNode *program = parse((argc < 2) ? "." : argv[1]);
    graph_program(program);
    BoundNode *ast = bind(program);
    graph_ast(ast);
    return 0;
}
