/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdio.h>

#include <graph.h>
#include <log.h>
#include <type.h>

void graph_syntax_node(size_t parent, char const *prefix, SyntaxNode *node, FILE *f)
{
    char namebuf[64];
    StringView name = node->name;
    if (sv_empty(name)) {
        switch (node->type) {
        case SNT_BINARYEXPRESSION:
            snprintf(namebuf, 64, "%s", Operator_name(node->binary_expr.operator));
            break;
        default:
            namebuf[0] = 0;
            break;
        }
        name = sv_from(namebuf);
    }

    if (!prefix) {
        fprintf(f, "    Node_%zu [label=\"%s " SV_SPEC "\"];\n", node->index, SyntaxNodeType_name(node->type), SV_ARG(name));
    } else {
        fprintf(f, "    Node_%zu [label=\"%s %s " SV_SPEC "\"];\n", node->index, prefix, SyntaxNodeType_name(node->type), SV_ARG(name));
    }
    fprintf(f, "    Node_%zu -> Node_%zu;\n", node->index, parent);
    switch (node->type) {
    case SNT_MODULE: {
        for (SyntaxNode *stmt = node->module.statements; stmt != NULL; stmt = stmt->next) {
            graph_syntax_node(node->index, NULL, stmt, f);
        }
    } break;
    case SNT_FUNCTION: {
        size_t p;
        if (node->function.parameter) {
            p = next_counter();
            fprintf(f, "    Node_%zu [label=\"Parameters\"];\n", p);
            fprintf(f, "    Node_%zu -> Node_%zu;\n", p, node->index);
            for (SyntaxNode* param = node->function.parameter; param != NULL; param = param->next) {
                graph_syntax_node(p, NULL, param, f);
                p = param->index;
            }
        }
        if (node->function.return_type) {
            p = next_counter();
            fprintf(f, "    Node_%zu [label=\"Return\"];\n", p);
            fprintf(f, "    Node_%zu -> Node_%zu;\n", p, node->index);
            graph_syntax_node(p, "return", node->function.return_type, f);
            if (node->function.error_type) {
                p = node->function.return_type->index;
                graph_syntax_node(p, "error", node->function.error_type, f);
            }
        }
        p = next_counter();
        fprintf(f, "    Node_%zu [label=\"Statements\"];\n", p);
        fprintf(f, "    Node_%zu -> Node_%zu;\n", p, node->index);
        for (SyntaxNode *stmt = node->function.statements; stmt != NULL; stmt = stmt->next) {
            graph_syntax_node(p, NULL, stmt, f);
            p = stmt->index;
        }
    } break;
    case SNT_PARAMETER:
        graph_syntax_node(node->index, NULL, node->parameter.parameter_type, f);
        break;
    case SNT_BINARYEXPRESSION:
        graph_syntax_node(node->index, "lhs", node->binary_expr.lhs, f);
        graph_syntax_node(node->index, "rhs", node->binary_expr.rhs, f);
        break;
    default:
        break;
    }
}

void graph_program(SyntaxNode *program)
{
    assert(program->type == SNT_PROGRAM);
    char dot_file[80];
    snprintf(dot_file, 80, SV_SPEC "-syntax.dot", SV_ARG(program->name));
    FILE *f = fopen(dot_file, "w");
    fprintf(f, "digraph " SV_SPEC " {\n", SV_ARG(program->name));
    for (SyntaxNode *mod = program->program.modules; mod != NULL; mod = mod->next) {
        fprintf(f, "    rankdir = BT;\n");
        fprintf(f, "    Node_%zu [label=\"" SV_SPEC "\"];\n", program->index, SV_ARG(program->name));
        graph_syntax_node(program->index, NULL, mod, f);
    }
    fprintf(f, "}\n");
    fclose(f);
    char cmd_line[256];
    snprintf(cmd_line, 256, "dot -Tsvg -O " SV_SPEC " " SV_SPEC "-syntax.dot", SV_ARG(program->name), SV_ARG(program->name));
    system(cmd_line);
}

void graph_bound_node(size_t parent, char const *prefix, BoundNode *node, FILE *f)
{
    char namebuf[64];
    ExpressionType *et = type_registry_get_type_by_id(node->typespec.type_id);
    assert(et);

    if (!prefix) {
        fprintf(f, "    Node_%zu [label=\"%s " SV_SPEC " " SV_SPEC "\"];\n",
            node->index, BoundNodeType_name(node->type), SV_ARG(node->name), SV_ARG(et->name));
    } else {
        fprintf(f, "    Node_%zu [label=\"%s %s " SV_SPEC " " SV_SPEC "\"];\n",
            node->index, prefix, BoundNodeType_name(node->type), SV_ARG(node->name), SV_ARG(et->name));
    }
    fprintf(f, "    Node_%zu -> Node_%zu;\n", node->index, parent);
    switch (node->type) {
    case BNT_MODULE: {
        for (BoundNode *stmt = node->module.statements; stmt != NULL; stmt = stmt->next) {
            graph_bound_node(node->index, NULL, stmt, f);
        }
    } break;
    case BNT_FUNCTION: {
        size_t p;
        if (node->function.parameter) {
            p = next_counter();
            fprintf(f, "    Node_%zu [label=\"Parameters\"];\n", p);
            fprintf(f, "    Node_%zu -> Node_%zu;\n", p, node->index);
            for (BoundNode* param = node->function.parameter; param != NULL; param = param->next) {
                graph_bound_node(p, NULL, param, f);
                p = param->index;
            }
        }
        p = next_counter();
        fprintf(f, "    Node_%zu [label=\"Statements\"];\n", p);
        fprintf(f, "    Node_%zu -> Node_%zu;\n", p, node->index);
        for (BoundNode *stmt = node->function.statements; stmt != NULL; stmt = stmt->next) {
            graph_bound_node(p, NULL, stmt, f);
            p = stmt->index;
        }
    } break;
    case BNT_VARIABLE_DECL: {
        if (node->variable_decl.init_expr) {
            graph_bound_node(node->index, "init", node->variable_decl.init_expr, f);
        }
        break;
    }
    case BNT_FUNCTION_CALL: {
        fprintf(f, "    Node_%zu -> Node_%zu;\n", node->index, node->call.function->index);
        if (node->call.argument) {
            size_t p = next_counter();
            fprintf(f, "    Node_%zu [label=\"Argumemts\"];\n", p);
            fprintf(f, "    Node_%zu -> Node_%zu;\n", p, node->index);
            for (BoundNode* param = node->call.argument; param != NULL; param = param->next) {
                graph_bound_node(p, NULL, param, f);
                p = param->index;
            }
        }
    }
    case BNT_BINARYEXPRESSION:
        graph_bound_node(node->index, "lhs", node->binary_expr.lhs, f);
        graph_bound_node(node->index, "rhs", node->binary_expr.rhs, f);
        break;
    default:
        break;
    }
}

void graph_ast(BoundNode *program)
{
    assert(program->type == SNT_PROGRAM);
    char dot_file[80];
    snprintf(dot_file, 80, SV_SPEC "-ast.dot", SV_ARG(program->name));
    FILE *f = fopen(dot_file, "w");
    fprintf(f, "digraph " SV_SPEC " {\n", SV_ARG(program->name));
    for (BoundNode *mod = program->program.modules; mod != NULL; mod = mod->next) {
        fprintf(f, "    rankdir = BT;\n");
        fprintf(f, "    Node_%zu [label=\"" SV_SPEC "\"];\n", program->index, SV_ARG(program->name));
        graph_bound_node(program->index, NULL, mod, f);
    }
    fprintf(f, "}\n");
    fclose(f);
    char cmd_line[256];
    snprintf(cmd_line, 256, "dot -Tsvg -O " SV_SPEC " " SV_SPEC "-ast.dot", SV_ARG(program->name), SV_ARG(program->name));
    system(cmd_line);
}
