/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <binder.h>
#include <log.h>
#include <sv.h>
#include <type.h>

#define STATIC_ALLOCATOR
#include <allocate.h>

typedef struct bind_context {
    struct bind_context *parent;
    int                  unbound;
} BindContext;

static BoundNode *bound_node_make(BoundNodeType type, BoundNode *parent);
static BoundNode *bound_node_make_unbound(BoundNode *parent, SyntaxNode *node, BindContext *ctx);
static BoundNode *bound_node_find_here(BoundNode *node, BoundNodeType type, StringView name);
static BoundNode *bound_node_find(BoundNode *node, BoundNodeType type, StringView name);
static bool       resolve_expression_type(Operator, TypeSpec lhs, TypeSpec rhs, TypeSpec *ret);
static bool       resolve_type_node(SyntaxNode *type_node, TypeSpec *typespec);
static int        bind_nodes(BoundNode *parent, SyntaxNode *first, BoundNode **first_dst, BindContext *ctx);
static BoundNode *bind_node(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx);
static BoundNode *rebind_node(BoundNode *node, BindContext *ctx);
static void       rebind_nodes(BoundNode *parent, BoundNode **first, BindContext *ctx);

#undef SYNTAXNODETYPE_ENUM
#define SYNTAXNODETYPE_ENUM(type) static BoundNode *bind_##type(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx);
SYNTAXNODETYPES(SYNTAXNODETYPE_ENUM)
#undef SYNTAXNODETYPE_ENUM

char const *BoundNodeType_name(BoundNodeType type)
{
    switch (type) {
#undef BOUNDNODETYPE_ENUM
#define BOUNDNODETYPE_ENUM(type) \
    case BNT_##type:             \
        return "BNT_" #type;
        BOUNDNODETYPES(BOUNDNODETYPE_ENUM)
#undef BOUNDNODETYPE_ENUM
    default:
        UNREACHABLE();
    }
}

char const *Intrinsic_name(Intrinsic intrinsic)
{
    switch (intrinsic) {
#undef INTRINSIC_ENUM
#define INTRINSIC_ENUM(i) \
    case INT_##i:         \
        return #i;        \
        INTRINSICS(INTRINSIC_ENUM)
#undef INTRINSIC_ENUM
    default:
        UNREACHABLE();
    }
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
    node->prev = NULL;
    node->index = next_index();
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

BoundNode *bound_node_find_here(BoundNode *node, BoundNodeType type, StringView name)
{
    switch (node->type) {
    case BNT_BLOCK:
    case BNT_MODULE: {
        for (BoundNode *n = node->block.statements; n; n = n->next) {
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
    } break;
    case BNT_FUNCTION_IMPL: {
        for (BoundNode *n = node->block.statements; n; n = n->next) {
            if (n->type == type && sv_eq(n->name, name)) {
                return n;
            }
        }
    } break;
    case BNT_FOR: {
        if (type == BNT_VARIABLE_DECL && node->for_statement.variable && sv_eq(node->for_statement.variable->name, name)) {
            return node->for_statement.variable;
        }
    } break;
    case BNT_PROGRAM: {
        if (type == BNT_FUNCTION) {
            for (BoundNode *n = node->program.intrinsics; n; n = n->next) {
                assert(n->type == BNT_INTRINSIC);
                if (sv_eq(n->name, name)) {
                    return n;
                }
            }
        }
    } break;
    default:
        break;
    }
    return NULL;
}

BoundNode *bound_node_find(BoundNode *node, BoundNodeType type, StringView name)
{
    BoundNode *ret = bound_node_find_here(node, type, name);
    if (ret) {
        return ret;
    }
    if (node->parent) {
        return bound_node_find(node->parent, type, name);
    }
    return NULL;
}

bool resolve_expression_type(Operator op, TypeSpec lhs, TypeSpec rhs, TypeSpec *ret)
{
    if (op == OP_RANGE) {
        if (lhs.type_id != rhs.type_id) {
            return false;
        }
        MUST_TO_VAR(TypeID, ret->type_id, type_specialize_template(RANGE_ID, 1, (TemplateArgument[]) { { .name = sv_from("T"), .param_type = TPT_TYPE, .type = lhs.type_id } }));
        ret->optional = false;
        return true;
    }
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
        fprintf(stderr, "Unknown type '" SV_SPEC "'", SV_ARG(type_node->name));
        return false;
    }
    (*typespec).type_id = typeid_canonical_type_id(type->type_id);
    (*typespec).optional = false;
    return true;
}

BoundNode *bind_ASSIGNMENT(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *decl = bound_node_find(parent, BNT_VARIABLE_DECL, stmt->name);
    if (!decl) {
        fprintf(stderr, "Assignment to undeclared variable '" SV_SPEC "'\n", SV_ARG(stmt->name));
        return bound_node_make_unbound(parent, stmt, ctx);
    }
    BoundNode *ret = bound_node_make(BNT_ASSIGNMENT, parent);
    ret->name = stmt->name;
    ret->typespec = decl->typespec;
    ret->assignment.expression = bind_node(ret, stmt->assignment.expression, ctx);
    if (ret->assignment.expression->type == BNT_UNBOUND_NODE) {
        return ret;
    }
    if (!typespec_assignment_compatible(ret->typespec, ret->assignment.expression->typespec)) {
        fatal("Cannot assign value of expression of type '" SV_SPEC "' to variable of type '" SV_SPEC "'",
            SV_ARG(typespec_name(ret->assignment.expression->typespec)), SV_ARG(typespec_name(ret->typespec)));
    }
    return ret;
}

BoundNode *bind_BINARYEXPRESSION(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
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

BoundNode *bind_BLOCK(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode   *ret = bound_node_make(BNT_BLOCK, parent);
    BindContext *block_ctx = context_make_subcontext(ctx);

    bind_nodes(ret, stmt->block.statements, &ret->block.statements, block_ctx);
    return ret;
}

BoundNode *bind_BOOL(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_BOOL, parent);
    ret->name = stmt->token.text;
    ret->typespec.type_id = type_registry_id_of_builtin_type(BIT_BOOL);
    ret->typespec.optional = false;
    return ret;
}

BoundNode *bind_BREAK(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *controllable = parent;
    while (controllable) {
        if ((controllable->type == BNT_LOOP || controllable->type == BNT_WHILE) && sv_eq(controllable->name, stmt->name)) {
            break;
        }
        controllable = controllable->parent;
    }
    if (!controllable) {
        fatal(LOC_SPEC "No 'while' or 'loop' block found with label '" SV_SPEC "'", LOC_ARG(stmt->token.loc), SV_ARG(stmt->name));
    }
    BoundNode *ret = bound_node_make((stmt->type == SNT_BREAK) ? BNT_BREAK : BNT_CONTINUE, parent);
    ret->controlled_statement = controllable;
    return ret;
}

BoundNode *bind_COMPOUND_INITIALIZER(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_COMPOUND_INITIALIZER, parent);
    ret->name = stmt->name;
    bind_nodes(ret, stmt->arguments.argument, &ret->compound_initializer.argument, ctx);
    return ret;
}

BoundNode *bind_CONTINUE(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    return bind_BREAK(parent, stmt, ctx);
}

BoundNode *bind_DECIMAL(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_DECIMAL, parent);
    ret->name = stmt->name;
    ret->typespec = (TypeSpec) { FLOAT_ID, false };
    return ret;
}

BoundNode *bind_FOR(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_FOR, parent);
    ret->name = stmt->name;
    ret->for_statement.range = bind_node(ret, stmt->for_statement.range, ctx);
    type_id         range_type_id = ret->for_statement.range->typespec.type_id;
    ExpressionType *range_type = type_registry_get_type_by_id(range_type_id);
    if (range_type->specialization_of != RANGE_ID) {
        fatal(LOC_SPEC "Range expression must have `range' type", SN_LOC_ARG(stmt->for_statement.range));
    }
    ret->typespec.type_id = range_type->template_arguments[0].type;

    ret->for_statement.variable = bound_node_make(BNT_VARIABLE_DECL, ret);
    ret->for_statement.variable->name = stmt->for_statement.variable;
    ret->for_statement.variable->typespec.type_id = ret->typespec.type_id;
    ret->for_statement.variable->variable_decl.init_expr = NULL;

    ret->for_statement.statement = bind_node(ret, stmt->for_statement.statement, ctx);
    return ret;
}

BoundNode *bind_FUNCTION(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    TypeSpec return_type = { VOID_ID, false };
    TypeSpec error_type = { VOID_ID, false };
    if (stmt->function.return_type != NULL && !resolve_type_node(stmt->function.return_type, &return_type)) {
        return bound_node_make_unbound(parent, stmt, ctx);
    }
    if (stmt->function.error_type != NULL && !resolve_type_node(stmt->function.error_type, &error_type)) {
        return bound_node_make_unbound(parent, stmt, ctx);
    }
    if (return_type.type_id != VOID_ID && error_type.type_id != VOID_ID) {
        ErrorOrTypeID variant_id_maybe = type_registry_get_variant2(return_type.type_id, error_type.type_id);
        if (ErrorOrTypeID_is_error(variant_id_maybe)) {
            fatal(LOC_SPEC "Could not create variant return type", SN_LOC_ARG(stmt));
        }
        return_type.type_id = variant_id_maybe.value;
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
    ret->function.function_impl = bind_node(ret, stmt->function.function_impl, func_ctx);
    return ret;
}

BoundNode *bind_FUNCTION_CALL(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *fnc = bound_node_find(parent, BNT_FUNCTION, stmt->name);
    if (!fnc) {
        fprintf(stderr, "Cannot bind function '" SV_SPEC "'\n", SV_ARG(stmt->name));
        return bound_node_make_unbound(parent, stmt, ctx);
    }
    BoundNode *ret = bound_node_make(BNT_FUNCTION_CALL, parent);
    ret->name = stmt->name;
    ret->call.function = fnc;
    ret->typespec = fnc->typespec;
    bind_nodes(ret, stmt->arguments.argument, &ret->call.argument, ctx);
    return ret;
}

BoundNode *bind_FUNCTION_IMPL(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_FUNCTION_IMPL, parent);
    bind_nodes(ret, stmt->function_impl.statements, &ret->block.statements, ctx);
    return ret;
}

BoundNode *bind_IF(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_IF, parent);
    ret->if_statement.condition = bind_node(ret, stmt->if_statement.condition, ctx);
    ret->if_statement.if_true = bind_node(ret, stmt->if_statement.if_true, ctx);
    ret->if_statement.if_false = bind_node(ret, stmt->if_statement.if_false, ctx);
    return ret;
}

BoundNode *bind_INTEGER(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_INTEGER, parent);
    ret->name = stmt->token.text;
    ret->typespec = (TypeSpec) { type_registry_id_of_integer_type(stmt->integer.width, stmt->integer.un_signed), false };
    return ret;
}

BoundNode *bind_LABEL(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    SyntaxNode *breakable = stmt->next;
    if (breakable->type == SNT_LOOP || breakable->type == SNT_WHILE) {
        breakable->name = stmt->name;
    }
    return NULL;
}

BoundNode *bind_LOOP(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_LOOP, parent);
    ret->name = stmt->name;
    ret->block.statements = bind_node(ret, stmt->block.statements, ctx);
    return ret;
}

BoundNode *bind_MODULE(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
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
    bind_nodes(ret, stmt->block.statements, &ret->block.statements, mod_ctx);
    return ret;
}

BoundNode *bind_NAME(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    UNREACHABLE();
}

BoundNode *bind_NATIVE_FUNCTION(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_NATIVE_FUNCTION, parent);
    ret->name = stmt->name;
    return ret;
}

BoundNode *bind_PARAMETER(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
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

typedef struct intrinsic_param {
    char const *name;
    BuiltinType type;
} IntrinsicParam;

void program_define_intrinsic(BoundNode *program, char const *name, BuiltinType ret_type,
    Intrinsic intrinsic_code, IntrinsicParam params[])
{
    BoundNode *intrinsic;
    intrinsic = bound_node_make(BNT_INTRINSIC, program);
    intrinsic->next = program->program.intrinsics;
    program->program.intrinsics = intrinsic;
    intrinsic->name = sv_from(name);
    intrinsic->typespec.type_id = type_registry_id_of_builtin_type(ret_type);
    intrinsic->typespec.optional = false;
    intrinsic->intrinsic.intrinsic = intrinsic_code;
    for (size_t ix = 0; params[ix].name; ++ix) {
        BoundNode *param = bound_node_make(BNT_PARAMETER, intrinsic);
        param->next = intrinsic->intrinsic.parameter;
        if (intrinsic->intrinsic.parameter) {
            intrinsic->intrinsic.parameter->prev = param;
        }
        intrinsic->intrinsic.parameter = param;
        param->name = sv_from(params[ix].name);
        param->typespec.type_id = type_registry_id_of_builtin_type(params[ix].type);
        param->typespec.optional = false;
    }
}

void program_define_intrinsics(BoundNode *program)
{
    program_define_intrinsic(program, "close", BIT_I32, INT_CLOSE, (IntrinsicParam[]) { { "fh", BIT_I32 }, { NULL, BIT_VOID } });
    program_define_intrinsic(program, "endln", BIT_U64, INT_ENDLN, (IntrinsicParam[]) { { NULL, BIT_VOID } });
    program_define_intrinsic(program, "fputs", BIT_I32, INT_FPUTS, (IntrinsicParam[]) { { "fh", BIT_I32 }, { "s", BIT_STRING }, { NULL, BIT_VOID } });
    program_define_intrinsic(program, "open", BIT_I32, INT_OPEN, (IntrinsicParam[]) { { "name", BIT_STRING }, { "mode", BIT_U32 }, { NULL, BIT_VOID } });
    program_define_intrinsic(program, "puti", BIT_I32, INT_PUTI, (IntrinsicParam[]) { { "i", BIT_I32 }, { NULL, BIT_VOID } });
    program_define_intrinsic(program, "putln", BIT_I32, INT_PUTLN, (IntrinsicParam[]) { { "s", BIT_STRING }, { NULL, BIT_VOID } });
    program_define_intrinsic(program, "read", BIT_I64, INT_FPUTS, (IntrinsicParam[]) { { "fh", BIT_I32 }, { "buffer", BIT_POINTER }, { "bytes", BIT_U64 }, { NULL, BIT_VOID } });
    program_define_intrinsic(program, "write", BIT_I64, INT_FPUTS, (IntrinsicParam[]) { { "fh", BIT_I32 }, { "buffer", BIT_POINTER }, { "bytes", BIT_U64 }, { NULL, BIT_VOID } });
}

void program_collect_types(BoundNode *program)
{
    BoundNode **last_type = &program->program.types;
    for (size_t ix = FIRST_CUSTOM_IX; ix < NEXT_CUSTOM_IX; ++ix) {
        BoundNode      *type_node = NULL;
        ExpressionType *et = type_registry_get_type_by_index(ix);
        switch (type_kind(et)) {
        case TK_ALIAS: {
            type_node = bound_node_make(BNT_TYPE, program);
            type_node->name = et->name;
            type_node->typespec = (TypeSpec) { et->alias_for_id, false };
        } break;
        case TK_ARRAY: {
            type_node = bound_node_make(BNT_ARRAY, program);
            type_node->name = et->name;
            type_node->array_def.base_type = et->array.base_type.type_id;
            type_node->array_def.size = et->array.size;
        } break;
        case TK_AGGREGATE:
        case TK_VARIANT: {
            type_node = bound_node_make((type_kind(et) == TK_AGGREGATE) ? BNT_STRUCT : BNT_VARIANT, program);
            type_node->name = et->name;
            BoundNode **dst = &type_node->compound_def.components;
            BoundNode  *last = NULL;
            for (size_t ix = 0; ix < et->components.num_components; ++ix) {
                *dst = bound_node_make(BNT_TYPE_COMPONENT, type_node);
                (*dst)->name = et->components.components[ix].name;
                (*dst)->typespec = (TypeSpec) { et->components.components[ix].type_id, false };
                (*dst)->prev = last;
                last = *dst;
                dst = &last->next;
            }
        } break;
        default:
            fatal("Type %.*s has undefined kind %d", SV_ARG(et->name), type_kind(et));
        }
        type_node->prev = *last_type;
        if (*last_type) {
            (*last_type)->next = type_node;
        }
        *last_type = type_node;
    }
}

BoundNode *bind_PROGRAM(BoundNode *parent, SyntaxNode *program, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_PROGRAM, NULL);
    ret->name = program->name;

    program_define_intrinsics(ret);
    bind_nodes(ret, program->program.modules, &ret->program.modules, ctx);
    program_collect_types(ret);
    return ret;
}

BoundNode *bind_RETURN(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_RETURN, parent);
    if (stmt->return_stmt.expression) {
        ret->return_stmt.expression = bind_node(ret, stmt->return_stmt.expression, ctx);
    }
    return ret;
}

BoundNode *bind_STRING(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_STRING, parent);
    ret->name = stmt->name;
    ret->typespec.type_id = type_registry_id_of_builtin_type(BIT_STRING);
    ret->typespec.optional = false;
    return ret;
}

BoundNode *bind_STRUCT(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *type_node = bound_node_make(BNT_STRUCT, parent);
    type_node->name = stmt->name;
    if (bind_nodes(type_node, stmt->struct_def.components, &type_node->compound_def.components, ctx) != 0) {
        return bound_node_make_unbound(parent, stmt, ctx);
    }
    int num = 0;
    for (BoundNode *component = type_node->compound_def.components; component; component = component->next) {
        num++;
    }
    TypeComponent *type_components = alloca(num * sizeof(TypeComponent));
    num = 0;
    for (BoundNode *component = type_node->compound_def.components; component; component = component->next) {
        type_components[num].kind = CK_TYPE;
        type_components[num].name = component->name;
        type_components[num].type_id = component->typespec.type_id;
        num++;
    }
    ErrorOrTypeID struct_id_maybe = type_registry_make_type(stmt->name, TK_AGGREGATE);
    if (ErrorOrTypeID_is_error(struct_id_maybe)) {
        fatal(LOC_SPEC "Could not create composite type: %s", LOC_ARG(stmt->token.loc), Error_to_string(struct_id_maybe.error));
    }
    struct_id_maybe = type_set_struct_components(struct_id_maybe.value, num, type_components);
    if (ErrorOrTypeID_is_error(struct_id_maybe)) {
        fatal(LOC_SPEC "Could not create composite type: %s", LOC_ARG(stmt->token.loc), Error_to_string(struct_id_maybe.error));
    }
    return NULL;
}

BoundNode *bind_TYPE(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    return NULL;
}

BoundNode *bind_TYPE_COMPONENT(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    TypeSpec param_type = { VOID_ID, false };
    if (!resolve_type_node(stmt->parameter.parameter_type, &param_type)) {
        return bound_node_make_unbound(parent, stmt, ctx);
    }
    BoundNode *ret = bound_node_make(BNT_TYPE_COMPONENT, parent);
    ret->name = stmt->name;
    ret->typespec = param_type;
    return ret;
}

BoundNode *bind_UNARYEXPRESSION(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    return NULL;
}

BoundNode *bind_VARIABLE(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *decl = bound_node_find(parent, BNT_VARIABLE_DECL, stmt->variable.names->name);
    if (!decl) {
        fprintf(stderr, "Cannot bind variable '" SV_SPEC "'\n", SV_ARG(stmt->name));
        return bound_node_make_unbound(parent, stmt, ctx);
    }
    BoundNode *ret = bound_node_make(BNT_VARIABLE, parent);
    ret->variable.decl = decl;
    ret->name = stmt->name;

    BoundNode     **name_part = &ret->variable.names;
    ExpressionType *et = type_registry_get_type_by_id(decl->typespec.type_id);
    SyntaxNode     *name_stmt;
    for (name_stmt = stmt->variable.names; name_stmt->next != NULL; name_stmt = name_stmt->next) {
        if (type_kind(et) != TK_AGGREGATE) {
            fatal(LOC_SPEC "Type '" SV_SPEC "' is not an aggregate", LOC_ARG(name_stmt->token.loc), SV_ARG(et->name));
        }
        if (!type_is_concrete(et)) {
            fatal(LOC_SPEC "Type '" SV_SPEC "' must be specialized", LOC_ARG(name_stmt->token.loc), SV_ARG(et->name));
        }
        *name_part = bound_node_make(BNT_NAME, parent);
        (*name_part)->name = name_stmt->name;
        ret->typespec = (*name_part)->typespec = (TypeSpec) { et->type_id, false };
        TypeComponent *comp = type_get_component(et, name_stmt->next->name);
        if (comp == NULL) {
            fatal(LOC_SPEC "Type '" SV_SPEC "' has no component named '" SV_SPEC "'", LOC_ARG(name_stmt->token.loc), SV_ARG(et->name), SV_ARG(name_stmt->name));
        }
        et = type_registry_get_type_by_id(comp->type_id);
        name_part = &(*name_part)->next;
    }
    *name_part = bound_node_make(BNT_NAME, parent);
    (*name_part)->name = name_stmt->name;
    ret->typespec = (*name_part)->typespec = (TypeSpec) { et->type_id, false };
    return ret;
}

BoundNode *bind_VARIABLE_DECL(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
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
    if (expr) {
        if (expr->type != BNT_COMPOUND_INITIALIZER) {
            if (var_type.type_id != VOID_ID && expr->typespec.type_id != var_type.type_id) {
                fatal("Declaration type '%.*s' and expression type '%.*s' are different",
                    SV_ARG(typeid_name(var_type.type_id)),
                    SV_ARG(typeid_name(expr->typespec.type_id)));
            }
            if (var_type.type_id == VOID_ID) {
                var_type = expr->typespec;
            }
        } else {
            if (var_type.type_id == VOID_ID) {
                fatal("Variable type with compound initializer cannot be inferred");
            }
            ExpressionType *et = type_registry_get_type_by_id(var_type.type_id);
            assert(et);
            if (type_kind(et) != TK_AGGREGATE) {
                fatal(LOC_SPEC "Cannot initialize variables with non-compound types using a compound initializer", LOC_ARG(stmt->token.loc));
            }
            expr->typespec = var_type;
            BoundNode *arg = expr->compound_initializer.argument;
            for (size_t ix = 0; ix < et->components.num_components; ++ix) {
                TypeComponent *type_component = &et->components.components[ix];
                if (!arg) {
                    fatal("No initializer argument values for " SV_SPEC "." SV_SPEC, SV_ARG(et->name), SV_ARG(type_component->name));
                }
                if (!typespec_assignment_compatible((TypeSpec) { type_component->type_id, false }, arg->typespec)) {
                    fatal("Declaration type element and initializer argument value types are different");
                }
                arg = arg->next;
            }
            if (arg) {
                fatal("Too many initializer argument values for type '" SV_SPEC "'", et->name);
            }
        }
    } else if (var_type.type_id == VOID_ID) {
        fprintf(stderr, "Could not infer type of variable '" SV_SPEC "'\n", SV_ARG(stmt->name));
        return bound_node_make_unbound(parent, stmt, ctx);
    }
    ret->typespec = var_type;
    ret->name = stmt->name;
    ret->variable_decl.init_expr = expr;
    return ret;
}

BoundNode *bind_WHILE(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_WHILE, parent);
    ret->name = stmt->name;
    ret->while_statement.condition = bind_node(ret, stmt->while_statement.condition, ctx);
    ret->while_statement.statement = bind_node(ret, stmt->while_statement.statement, ctx);
    return ret;
}

BoundNode *bind_node(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    static size_t VOID_ID = -1;

    if (VOID_ID < 0) {
        type_registry_id_of_builtin_type(BIT_VOID);
    }
    if (!stmt) {
        return NULL;
    }

    switch (stmt->type) {
#define SYNTAXNODETYPE_ENUM(type)                                                    \
    case SNT_##type:                                                                 \
        trace(CAT_BIND, "Binding " #type " node '" SV_SPEC "'", SV_ARG(stmt->name)); \
        return bind_##type(parent, stmt, ctx);
        SYNTAXNODETYPES(SYNTAXNODETYPE_ENUM)
#undef SYNTAXNODETYPE_ENUM
    default:
        UNREACHABLE();
    }
}

int bind_nodes(BoundNode *parent, SyntaxNode *first, BoundNode **first_dst, BindContext *ctx)
{
    BoundNode *last_node = NULL;
    int        unbound = 0;
    for (SyntaxNode *node = first; node != NULL; node = node->next) {
        BoundNode *bound_node = bind_node(parent, node, ctx);
        if (!bound_node) {
            continue;
        }
        if (bound_node->type == BNT_UNBOUND_NODE) {
            bound_node = bound_node_make_unbound(parent, node, NULL);
            unbound++;
        }
        if (last_node == NULL) {
            *first_dst = bound_node;
        } else {
            last_node->next = bound_node;
            bound_node->prev = last_node;
        }
        last_node = bound_node;
    }
    return unbound;
}

void rebind_nodes(BoundNode *parent, BoundNode **first, BindContext *ctx)
{
    for (BoundNode **stmt = first; *stmt != NULL; stmt = &((*stmt)->next)) {
        BoundNode *bound_node = rebind_node(*stmt, ctx);
        if (!bound_node) {
            continue;
        }
        if (bound_node->type == BNT_UNBOUND_NODE) {
            bound_node = bound_node_make_unbound(parent, bound_node->unbound_node, NULL);
        }
        bound_node->prev = (*stmt)->prev;
        bound_node->next = (*stmt)->next;
        *stmt = bound_node;
    }
}

BoundNode *rebind_node(BoundNode *node, BindContext *ctx)
{
    if (!node) {
        return NULL;
    }
    switch (node->type) {
    case BNT_ASSIGNMENT: {
        node->assignment.expression = rebind_node(node->assignment.expression, ctx);
        return node;
    }
    case BNT_FUNCTION: {
        node->function.function_impl = rebind_node(node->function.function_impl, ctx);
        return node;
    }
    case BNT_FUNCTION_CALL: {
        rebind_nodes(node, &node->call.argument, ctx);
        return node;
    }
    case BNT_FUNCTION_IMPL: {
        rebind_nodes(node, &node->block.statements, ctx);
        return node;
    }
    case BNT_IF: {
        node->if_statement.condition = rebind_node(node->if_statement.condition, ctx);
        node->if_statement.if_true = rebind_node(node->if_statement.if_true, ctx);
        node->if_statement.if_false = rebind_node(node->if_statement.if_false, ctx);
        return node;
    }
    case BNT_LOOP: {
        node->block.statements = rebind_node(node->block.statements, ctx);
        return node;
    }
    case BNT_BLOCK:
    case BNT_MODULE: {
        rebind_nodes(node, &node->block.statements, ctx);
        return node;
    }
    case BNT_PROGRAM: {
        rebind_nodes(node, &node->program.modules, ctx);
        return node;
    }
    case BNT_RETURN: {
        node->return_stmt.expression = rebind_node(node->return_stmt.expression, ctx);
        return node;
    }
    case BNT_UNBOUND_NODE: {
        return bind_node(node->parent, node->unbound_node, ctx);
    }
    case BNT_WHILE: {
        node->while_statement.condition = rebind_node(node->while_statement.condition, ctx);
        node->while_statement.statement = rebind_node(node->while_statement.statement, ctx);
        return node;
    }
    default: {
        return node;
    }
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
    fatal("No main function found");
}

static BindingObserver s_observer = NULL;

BoundNode *bind(SyntaxNode *program)
{
    BindContext *ctx = allocate(sizeof(BindContext));
    BoundNode   *ret = bind_node(NULL, program, ctx);
    int          current_unbound = INT32_MAX;
    int          iter = 1;
    if (s_observer) {
        s_observer(iter, ret);
    }
    while (ctx->unbound > 0 && ctx->unbound < current_unbound) {
        fprintf(stderr, "Iteration %d: %d unbound nodes\n", iter++, ctx->unbound);
        current_unbound = ctx->unbound;
        ctx->unbound = 0;
        rebind_node(ret, ctx);
        if (s_observer) {
            s_observer(iter, ret);
        }
    }
    if (ctx->unbound > 0) {
        fatal("Iteration %d: There are %d unbound nodes. Exiting...", iter, ctx->unbound);
    }
    fprintf(stderr, "Iteration %d: All bound\n", iter);
    find_main(ret);
    return ret;
}

BindingObserver register_binding_observer(BindingObserver observer)
{
    BindingObserver ret = s_observer;
    s_observer = observer;
    return ret;
}
