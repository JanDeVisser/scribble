/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdio.h>

#include <graph.h>
#include <log.h>
#include <mem.h>
#include <type.h>

typedef enum node_kind {
    NK_SYNTAX,
    NK_BOUND,
    NK_VIRTUAL
} NodeKind;

typedef enum virtual_node_type {
    VNT_DUMMY = 2000
} VirtualNodeType;

typedef struct virtual_node {
    VirtualNodeType      type;
    StringView           name;
    struct virtual_node *next;
    size_t               index;
} VirtualNode;

typedef struct abstract_node {
    int                   type;
    StringView            name;
    struct abstract_node *next;
    size_t                index;
} AbstractNode;

#define abstract(n) ((AbstractNode *) (n))

typedef struct graph_node {
    NodeKind           kind;
    struct graph_node *parent;
    StringView         name;
    char              *prefix;
    StringView         id_str;
    StringView         label_str;
    union {
        AbstractNode *abstract_node;
        SyntaxNode   *syntax_node;
        BoundNode    *bound_node;
        VirtualNode  *virtual_node;
    };
} GraphNode;

void graph_node_emit(GraphNode *node, FILE *f);
void graph_node_connect(GraphNode *from, GraphNode *to, FILE *f);

char const *VirtualNodeType_name(VirtualNodeType type)
{
    return "";
}

VirtualNode *virtual_node_create(StringView name)
{
    VirtualNode *ret = mem_allocate(sizeof(VirtualNode));
    ret->type = VNT_DUMMY;
    ret->name = name;
    ret->index = next_index();
    return ret;
}

GraphNode *graph_node_create(AbstractNode *node, GraphNode *parent, char *prefix)
{
    GraphNode *ret = mem_allocate(sizeof(GraphNode));
    if (node->type < BNT_OFFSET) {
        ret->kind = NK_SYNTAX;
    } else if (node->type < VNT_DUMMY) {
        assert(node->type != BNT_OFFSET && node->type < BNT_LAST);
        ret->kind = NK_BOUND;
    } else {
        ret->kind = NK_VIRTUAL;
    }
    ret->name = node->name;
    ret->parent = parent;
    ret->prefix = (prefix) ? prefix : "";
    ret->abstract_node = node;
    return ret;
}

GraphNode *graph_node_create_virtual(GraphNode *parent, StringView name, FILE *f)
{
    GraphNode *ret = graph_node_create(abstract(virtual_node_create(name)), parent, NULL);
    graph_node_emit(ret, f);
    return ret;
}

void graph_node_forward(GraphNode *parent, AbstractNode *node, char *prefix, FILE *f)
{
    graph_node_emit(graph_node_create(node, parent, prefix), f);
}

StringView graph_node_id_str(GraphNode *node)
{
    if (sv_empty(node->id_str)) {
        char code;
        switch (node->kind) {
        case NK_SYNTAX:
            code = 's';
            break;
        case NK_BOUND:
            code = 'b';
            break;
        case NK_VIRTUAL:
            code = 'v';
            break;
        default:
            UNREACHABLE();
        }
        node->id_str = sv_printf("Node_%c_%zu", code, node->abstract_node->index);
    }
    return node->id_str;
}

StringView graph_node_name(GraphNode *node)
{
    if (sv_empty(node->name)) {
        switch (node->abstract_node->type) {
        case SNT_BINARYEXPRESSION:
            node->name = sv_from(Operator_name(node->syntax_node->binary_expr.operator));
            break;
        default:
            node->name = sv_from("<anon>");
            break;
        }
    }
    return node->name;
}

char const *graph_node_type_str(GraphNode *node)
{
    switch (node->kind) {
    case NK_SYNTAX:
        return SyntaxNodeType_name(node->syntax_node->type);
    case NK_BOUND:
        return BoundNodeType_name(node->bound_node->type);
    case NK_VIRTUAL:
        return VirtualNodeType_name(node->virtual_node->type);
    default:
        UNREACHABLE();
    }
}

StringView graph_node_label(GraphNode *node)
{
    if (sv_empty(node->label_str)) {
        switch (node->kind) {
        case NK_BOUND: {
            BoundNode *bn = node->bound_node;
            ExpressionType *et = type_registry_get_type_by_id(bn->typespec.type_id);
            assert(et);
            node->label_str = sv_printf("%s %s " SV_SPEC " " SV_SPEC,
                node->prefix, graph_node_type_str(node), SV_ARG(graph_node_name(node)), SV_ARG(et->name));
        } break;
        default:
            node->label_str = sv_printf("%s %s " SV_SPEC,
                node->prefix, graph_node_type_str(node), SV_ARG(graph_node_name(node)));
            break;
        }
    }
    return node->label_str;
}

void graph_node_connect(GraphNode *from, GraphNode *to, FILE *f)
{
    fprintf(f, "    " SV_SPEC " -> " SV_SPEC ";\n",
        SV_ARG(graph_node_id_str(from)), SV_ARG(graph_node_id_str(to)));
}

void graph_node_emit(GraphNode *node, FILE *f)
{
    fprintf(f, "    " SV_SPEC "[label=\"" SV_SPEC "\"];\n",
        SV_ARG(graph_node_id_str(node)), SV_ARG(graph_node_label(node)));
    if (node->parent) {
        graph_node_connect(node, node->parent, f);
    }
    SyntaxNode *sn = node->syntax_node;
    BoundNode  *bn = node->bound_node;
    switch (node->abstract_node->type) {
    case SNT_BINARYEXPRESSION:
        graph_node_forward(node, abstract(sn->binary_expr.lhs), "lhs", f);
        graph_node_forward(node, abstract(sn->binary_expr.rhs), "rhs", f);
        break;
    case SNT_BLOCK:
    case SNT_MODULE: {
        for (SyntaxNode *stmt = sn->block.statements; stmt != NULL; stmt = stmt->next) {
            graph_node_forward(node, abstract(stmt), NULL, f);
        }
    } break;
    case SNT_FUNCTION: {
        size_t p;
        if (sn->function.parameter) {
            GraphNode *parameters = graph_node_create_virtual(node, sv_from("parameters"), f);
            for (SyntaxNode *param = sn->function.parameter; param != NULL; param = param->next) {
                graph_node_forward(parameters, abstract(param), NULL, f);
            }
        }
        if (sn->function.return_type) {
            if (sn->function.error_type) {
                GraphNode *return_type = graph_node_create_virtual(node, sv_from("return type"), f);
                graph_node_forward(return_type, abstract(sn->function.return_type), "ok", f);
                graph_node_forward(return_type, abstract(sn->function.error_type), "error", f);
            } else {
                graph_node_forward(node, abstract(sn->function.return_type), "return type", f);
            }
        }
        GraphNode *statements = graph_node_create_virtual(node, sv_from("statements"), f);
        for (SyntaxNode *stmt = sn->function.statements; stmt != NULL; stmt = stmt->next) {
            graph_node_forward(statements, abstract(stmt), NULL, f);
        }
    } break;
    case SNT_FUNCTION_CALL: {
        if (sn->call.argument) {
            GraphNode *arguments = graph_node_create_virtual(node, sv_from("arguments"), f);
            for (SyntaxNode *arg = sn->call.argument; arg != NULL; arg = arg->next) {
                graph_node_forward(arguments, abstract(arg), NULL, f);
            }
        }
    } break;
    case SNT_IF:
        graph_node_forward(node, abstract(sn->if_statement.condition), "condition", f);
        graph_node_forward(node, abstract(sn->if_statement.if_true), "if true", f);
        if (sn->if_statement.if_false) {
            graph_node_forward(node, abstract(sn->if_statement.if_false), "if false", f);
        }
        break;
    case SNT_PARAMETER:
        graph_node_forward(node, abstract(sn->parameter.parameter_type), NULL, f);
        break;
    case SNT_PROGRAM: {
        for (SyntaxNode *mod = sn->program.modules; mod != NULL; mod = mod->next) {
            graph_node_forward(node, abstract(mod), NULL, f);
        }
    } break;
    case SNT_RETURN:
        if (sn->return_stmt.expression) {
            graph_node_forward(node, abstract(sn->return_stmt.expression), "condition", f);
        }
        break;
    case SNT_WHILE:
        graph_node_forward(node, abstract(sn->while_statement.condition), "condition", f);
        graph_node_forward(node, abstract(sn->while_statement.statement), "statement", f);
        break;

    case BNT_BINARYEXPRESSION:
        graph_node_forward(node, abstract(bn->binary_expr.lhs), "lhs", f);
        graph_node_forward(node, abstract(bn->binary_expr.rhs), "rhs", f);
        break;
    case BNT_FUNCTION: {
        if (bn->function.parameter) {
            GraphNode *parameters = graph_node_create_virtual(node, sv_from("parameters"), f);
            for (BoundNode *param = bn->function.parameter; param != NULL; param = param->next) {
                graph_node_forward(parameters, abstract(param), NULL, f);
            }
        }
        GraphNode *statements = graph_node_create_virtual(node, sv_from("statements"), f);
        for (BoundNode *stmt = bn->function.statements; stmt != NULL; stmt = stmt->next) {
            graph_node_forward(statements, abstract(stmt), NULL, f);
        }
    } break;
    case BNT_FUNCTION_CALL: {
        GraphNode *decl_node = graph_node_create(abstract(bn->call.function), node, NULL);
        graph_node_connect(node, decl_node, f);
        if (bn->call.argument) {
            GraphNode *arguments = graph_node_create_virtual(node, sv_from("arguments"), f);
            for (BoundNode *arg = bn->call.argument; arg != NULL; arg = arg->next) {
                graph_node_forward(arguments, abstract(arg), NULL, f);
            }
        }
    } break;
    case BNT_IF:
        graph_node_forward(node, abstract(bn->if_statement.condition), "condition", f);
        graph_node_forward(node, abstract(bn->if_statement.if_true), "if true", f);
        if (bn->if_statement.if_false) {
            graph_node_forward(node, abstract(bn->if_statement.if_false), "if false", f);
        }
        break;
    case BNT_MODULE: {
        for (BoundNode *stmt = bn->block.statements; stmt != NULL; stmt = stmt->next) {
            graph_node_forward(node, abstract(stmt), NULL, f);
        }
    } break;
    case BNT_PROGRAM: {
        for (BoundNode *mod = bn->program.modules; mod != NULL; mod = mod->next) {
            graph_node_forward(node, abstract(mod), NULL, f);
        }
    } break;
    case BNT_RETURN:
        graph_node_forward(node, abstract(bn->return_stmt.expression), "expression", f);
        break;
    case BNT_UNBOUND_NODE:
        graph_node_forward(node, abstract(bn->unbound_node), "Unbound", f);
        break;
    case BNT_VARIABLE_DECL: {
        if (bn->variable_decl.init_expr) {
            graph_node_forward(node, abstract(bn->variable_decl.init_expr), "init", f);
        }
    } break;
    case BNT_WHILE:
        graph_node_forward(node, abstract(bn->while_statement.condition), "condition", f);
        graph_node_forward(node, abstract(bn->while_statement.statement), "statement", f);
        break;

    default:
        break;
    }
}

void graph_program(SyntaxNode *program)
{
    assert(program->type == SNT_PROGRAM);
    AllocatorState alloc_state = mem_save();
    StringView     dot_file = sv_printf(SV_SPEC "-syntax.dot", SV_ARG(program->name));
    FILE          *f = fopen(dot_file.ptr, "w");
    fprintf(f, "digraph " SV_SPEC " {\n", SV_ARG(program->name));
    fprintf(f, "    rankdir = BT;\n");
    graph_node_emit(graph_node_create(abstract(program), NULL, NULL), f);
    fprintf(f, "}\n");
    fclose(f);
    StringView cmd_line = sv_printf("dot -Tsvg -O " SV_SPEC " %s", SV_ARG(program->name), dot_file);
    system(cmd_line.ptr);
    mem_release(alloc_state);
}

void graph_ast(int iteration, BoundNode *program)
{
    assert(program->type == BNT_PROGRAM);

    AllocatorState alloc_state = mem_save();
    StringView     dot_file = sv_printf(SV_SPEC "-ast-%d.dot", SV_ARG(program->name), iteration);
    FILE          *f = fopen(dot_file.ptr, "w");
    fprintf(f, "digraph " SV_SPEC " {\n", SV_ARG(program->name));
    fprintf(f, "    rankdir = BT;\n");
    graph_node_emit(graph_node_create(abstract(program), NULL, NULL), f);
    fprintf(f, "}\n");
    fclose(f);
    char cmd_line[256];
    snprintf(cmd_line, 256, "dot -Tsvg -O " SV_SPEC " " SV_SPEC, SV_ARG(program->name), SV_ARG(dot_file));
    system(cmd_line);
    mem_release(alloc_state);
}
