/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include "binder.h"
#include "log.h"
#include "mem.h"
#include "type.h"

typedef struct node_list {
    BoundNode        *node;
    struct node_list *next;
} BoundNodeList;

typedef struct bind_context {
    struct bind_context *parent;
    int                  unbound;
} BindContext;

static void      *allocate(size_t size);
static BoundNode *bound_node_make(BoundNodeType type, BoundNode *parent);
static void       bind_nodes(BoundNode *parent, SyntaxNode *first, BoundNode **first_dst, BindContext *ctx);
static BoundNode *bind_node(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx);
static BoundNode *rebind_node(BoundNode *node, BindContext *ctx);
static void       rebind_nodes(BoundNode *parent, BoundNode **first, BindContext *ctx);

char const *BoundNodeType_name(BoundNodeType type)
{
    switch (type) {
#undef BOUNDNODETYPE_ENUM
#define BOUNDNODETYPE_ENUM(type) \
    case type:                   \
        return #type;
        BOUNDNODETYPES(BOUNDNODETYPE_ENUM)
#undef BOUNDNODETYPE_ENUM
    default:
        UNREACHABLE();
    }
}

void *allocate(size_t size)
{
    static Arena *s_arena = NULL;
    if (!s_arena) {
        s_arena = arena_new();
    }
    return arena_allocate(s_arena, size);
}

BindContext *context_make_subcontext(BindContext *ctx)
{
    BindContext *ret = allocate(sizeof(BindContext));
    ret->parent = ctx;
    return ret;
}

void context_increment_unbound(BindContext *ctx)
{
    ++ctx->unbound;
    if (ctx->parent)
        context_increment_unbound(ctx->parent);
}

BoundNode *bound_node_make(BoundNodeType type, BoundNode *parent)
{
    BoundNode *node = (BoundNode *) allocate(sizeof(BoundNode));
    node->type = type;
    node->parent = parent;
    node->next = NULL;
    node->index = next_counter();
    return node;
}

BoundNode *bound_node_make_unbound(BoundNode *parent, SyntaxNode *node, BindContext *ctx)
{
    BoundNode *unbound = bound_node_make(BNT_UNBOUND_NODE, parent);
    unbound->unbound_node = node;
    if (ctx) {
        context_increment_unbound(ctx);
    }
    return unbound;
}

BoundNode * bound_node_find(BoundNode *node, BoundNodeType type, StringView name)
{
    switch (node->type) {
    case BNT_MODULE: {
        for (BoundNode *n = node->module.statements; n; n = n->next) {
            if (n->type == type && sv_eq(n->name, name)) {
                return n;
            }
        }
        break;
    }
    case BNT_FUNCTION: {
        if (type == BNT_VARIABLE_DECL) {
            for (BoundNode *n = node->function.parameter; n; n = n->next) {
                if (sv_eq(n->name, name)) {
                    return n;
                }
            }
        }
        for (BoundNode *n = node->function.statements; n; n = n->next) {
            if (n->type == type && sv_eq(n->name, name)) {
                return n;
            }
        }
        break;
    }
    default:
        break;
    }
    if (node->parent) {
        return bound_node_find(node->parent, type, name);
    }
    return NULL;
}

bool resolve_expression_type(Operator, TypeSpec lhs, TypeSpec rhs, TypeSpec *ret)
{
    // Temporary: just assume that operators act on 2 objects of the same
    // type and return an object of that type.
    bool retval = lhs.type_id == rhs.type_id;
    if (retval) {
        *ret = lhs;
    }
    return retval;
}

bool resolve_type_node(SyntaxNode *type_node, TypeSpec *typespec)
{
    assert(type_node->type == SNT_TYPE);
    ExpressionType *type = type_registry_get_type_by_name(type_node->name);
    if (!type) {
        return false;
    }
    (*typespec).type_id = type->type_id;
    (*typespec).optional = false;
    return true;
}

BoundNode *bind_node(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    static size_t VOID_ID = -1;

    if (VOID_ID < 0) {
        type_registry_id_of_primitive_type(PT_VOID);
    }
    switch (stmt->type) {
    case SNT_MODULE: {
        BoundNode   *ret = bound_node_make(BNT_MODULE, parent);
        char        *mod_name = (char *) allocate(sv_length(stmt->name) + 1);
        BindContext *mod_ctx = context_make_subcontext(ctx);

        strncpy(mod_name, stmt->name.ptr, sv_length(stmt->name));
        mod_name[sv_length(stmt->name)] = 0;
        if (sv_endswith(stmt->name, sv_from(".scribble"))) {
            mod_name[sv_length(stmt->name) - 9] = 0;
        }
        for (char *ptr = strchr(mod_name, '/'); ptr; ptr = strchr(mod_name, '/')) {
            *ptr = '.';
        }
        ret->name = sv_from(mod_name);
        bind_nodes(ret, stmt->module.statements, &ret->module.statements, mod_ctx);
        return ret;
    }
    case SNT_FUNCTION: {
        TypeSpec return_type = { VOID_ID, false };
        TypeSpec error_type = { VOID_ID, false };
        if (stmt->function.return_type != NULL && !resolve_type_node(stmt->function.return_type, &return_type)) {
            return bound_node_make_unbound(parent, stmt, ctx);
        }
        if (stmt->function.error_type != NULL && !resolve_type_node(stmt->function.error_type, &error_type)) {
            return bound_node_make_unbound(parent, stmt, ctx);
        }
        if (return_type.type_id != VOID_ID && error_type.type_id != VOID_ID) {
            return_type.type_id = type_registry_get_variant2(return_type.type_id, error_type.type_id);
        }

        int        unbound_cnt = ctx->unbound;
        BoundNode *ret = bound_node_make(BNT_FUNCTION, parent);

        ret->typespec = return_type;
        ret->name = stmt->name;
        bind_nodes(ret, stmt->function.parameter, &ret->function.parameter, ctx);
        if (ctx->unbound > unbound_cnt) {
            return bound_node_make_unbound(parent, stmt, NULL);
        }
        BindContext *func_ctx = context_make_subcontext(ctx);
        bind_nodes(ret, stmt->function.statements, &ret->function.statements, func_ctx);
        return ret;
    }
    case SNT_PARAMETER: {
        TypeSpec param_type = { VOID_ID, false };
        if (!resolve_type_node(stmt->parameter.parameter_type, &param_type)) {
            return bound_node_make_unbound(parent, stmt, ctx);
        }
        BoundNode *ret = bound_node_make(BNT_VARIABLE_DECL, parent);
        ret->name = stmt->name;
        ret->typespec = param_type;
        ret->variable_decl.init_expr = NULL;
        return ret;
    }
    case SNT_BINARYEXPRESSION: {
        BoundNode *ret = bound_node_make(BNT_BINARYEXPRESSION, parent);
        BoundNode *lhs = bind_node(ret, stmt->binary_expr.lhs, ctx);
        BoundNode *rhs = bind_node(ret, stmt->binary_expr.rhs, ctx);
        if (lhs->type == BNT_UNBOUND_NODE || rhs->type == BNT_UNBOUND_NODE) {
            return bound_node_make_unbound(parent, stmt, NULL);
        }
        TypeSpec type;
        if (!resolve_expression_type(stmt->binary_expr.operator, lhs->typespec, rhs->typespec, &type)) {
            ExpressionType *lhs_type = type_registry_get_type_by_id(lhs->typespec.type_id);
            ExpressionType *rhs_type = type_registry_get_type_by_id(rhs->typespec.type_id);
            fatal("Could not resolve return type of operator '%s' with lhs type '" SV_SPEC "' and rhs type '" SV_SPEC "'",
                Operator_name(stmt->binary_expr.operator), SV_ARG(lhs_type->name), SV_ARG(rhs_type->name));
        }
        ret->typespec = type;
        ret->name = sv_from(Operator_name(stmt->binary_expr.operator));
        ret->binary_expr.lhs = lhs;
        ret->binary_expr.rhs = rhs;
        ret->binary_expr.operator= stmt->binary_expr.operator;
        return ret;
    }
    case SNT_NUMBER: {
        BoundNode *ret = bound_node_make(BNT_NUMBER, parent);
        ret->name = stmt->token.text;
        switch (stmt->token.code) {
        case TC_INTEGER:
            ret->typespec.type_id = type_registry_id_of_primitive_type(PT_INT);
            break;
        case TC_DECIMAL:
            ret->typespec.type_id = type_registry_id_of_primitive_type(PT_FLOAT);
            break;
        default:
            fatal("Invalid token code '%s' for number", PrimitiveType_name(stmt->token.code));
        }
        ret->typespec.optional = false;
        return ret;
    }
    case SNT_VARIABLE: {
        BoundNode *decl = bound_node_find(parent, BNT_VARIABLE_DECL, stmt->name);
        if (!decl) {
            return bound_node_make_unbound(parent, stmt, ctx);
        }
        BoundNode *ret = bound_node_make(BNT_VARIABLE, parent);
        ret->typespec = decl->typespec;
        ret->variable.decl = decl;
        ret->name = stmt->name;
        return ret;
    }
    case SNT_VARIABLE_DECL: {
        BoundNode *expr = NULL;
        TypeSpec   var_type = { VOID_ID, false };
        if (stmt->variable_decl.var_type != NULL && !resolve_type_node(stmt->variable_decl.var_type, &var_type)) {
            return bound_node_make_unbound(parent, stmt, ctx);
        }
        BoundNode *ret = bound_node_make(BNT_VARIABLE_DECL, parent);
        if (stmt->variable_decl.init_expr) {
            expr = bind_node(parent, stmt->variable_decl.init_expr, ctx);
            if (expr->type == BNT_UNBOUND_NODE) {
                return bound_node_make_unbound(parent, stmt, NULL);
            }
        }
        if (var_type.type_id != VOID_ID && expr != NULL) {
            if (var_type.type_id != expr->typespec.type_id) {
                fatal("Declaration type and expression type are different");
            }
        }
        if (var_type.type_id == VOID_ID && expr != NULL) {
            var_type = expr->typespec;
        } else if (var_type.type_id == VOID_ID && expr == NULL) {
            return bound_node_make_unbound(parent, stmt, ctx);
        }
        ret->typespec = var_type;
        ret->name = stmt->name;
        ret->variable_decl.init_expr = expr;
        return ret;
    }
    case SNT_FUNCTION_CALL: {
        BoundNode *fnc = bound_node_find(parent, BNT_FUNCTION, stmt->name);
        if (!fnc) {
            return bound_node_make_unbound(parent, stmt, ctx);
        }
        BoundNode *ret = bound_node_make(BNT_FUNCTION_CALL, parent);
        ret->name = stmt->name;
        ret->call.function = fnc;
        ret->typespec = fnc->typespec;
        int unbound_cnt = ctx->unbound;
        bind_nodes(ret, stmt->call.argument, &ret->call.argument, ctx);
        if (unbound_cnt < ctx->unbound) {
            return bound_node_make_unbound(parent, stmt, NULL);
        }
        return ret;
    }
    default:
        fatal("Unexpected statement type '%s' in bind_node", SyntaxNodeType_name(stmt->type));
    }
}

void bind_nodes(BoundNode *parent, SyntaxNode *first, BoundNode **first_dst, BindContext *ctx)
{
    BoundNode *last_node = NULL;
    for (SyntaxNode *node = first; node != NULL; node = node->next) {
        BoundNode *bound_node = bind_node(parent, node, ctx);
        if (bound_node->type == BNT_UNBOUND_NODE) {
            bound_node = bound_node_make_unbound(parent, node, NULL);
        }
        if (last_node == NULL) {
            *first_dst = bound_node;
        } else {
            last_node->next = bound_node;
        }
        last_node = bound_node;
    }
}

void rebind_nodes(BoundNode *parent, BoundNode **first, BindContext *ctx)
{
    for (BoundNode **stmt = first; *stmt != NULL; stmt = &((*stmt)->next)) {
        BoundNode *bound_node = rebind_node(*stmt, ctx);
        if (bound_node->type == BNT_UNBOUND_NODE) {
            bound_node = bound_node_make_unbound(parent, bound_node->unbound_node, NULL);
        }
        *stmt = bound_node;
    }
}

BoundNode *rebind_node(BoundNode *node, BindContext *ctx)
{
    switch (node->type) {
    case BNT_PROGRAM:
        rebind_nodes(node->parent, &node->program.modules, ctx);
        return node;
    case BNT_MODULE:
        rebind_nodes(node->parent, &node->module.statements, ctx);
        return node;
    case BNT_FUNCTION:
        rebind_nodes(node->parent, &node->function.statements, ctx);
        return node;
    case BNT_UNBOUND_NODE:
        return bind_node(node->parent, node->unbound_node, ctx);
    default:
        return node;
    }
}

void find_main(BoundNode *program)
{
    for (BoundNode *module = program->program.modules; module != NULL; module = module->next) {
        BoundNode *main = bound_node_find(module, BNT_FUNCTION, sv_from("main"));
        if (main) {
            return;
        }
    }
    fatal("Not main function found");
}

BoundNode *bind(SyntaxNode *program)
{
    BoundNode *ret = bound_node_make(BNT_PROGRAM, NULL);
    ret->name = program->name;
    BindContext *ctx = allocate(sizeof(BindContext));

    assert(program->type == SNT_PROGRAM);
    bind_nodes(ret, program->program.modules, &ret->program.modules, ctx);

    find_main(ret);

    int current_unbound = INT32_MAX;
    int iter = 1;
    while (ctx->unbound > 0 && ctx->unbound < current_unbound) {
        fprintf(stderr, "Iteration %d: %d unbound nodes\n", iter++, ctx->unbound);
        current_unbound = ctx->unbound;
        ctx->unbound = 0;
        rebind_node(ret, ctx);
    }
    if (ctx->unbound > 0) {
        fatal("There are unbound references");
    }
    fprintf(stderr, "Iteration %d: All bound\n", iter);
    return ret;
}
