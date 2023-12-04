/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <binder.h>
#include <execute.h>
#include <intermediate.h>
#include <log.h>
#include <native.h>
#include <options.h>
#include <sv.h>
#include <type.h>

#define STATIC_ALLOCATOR
#include <allocate.h>

typedef struct bind_context {
    struct bind_context *parent;
    int                  unbound;
    bool                 main;
} BindContext;

static BoundNode *bound_node_make(BoundNodeType type, BoundNode *parent);
static BoundNode *bound_node_make_unbound(BoundNode *parent, SyntaxNode *node, BindContext *ctx);
static BoundNode *bound_node_find_here(BoundNode *node, BoundNodeType type, SyntaxNode *variable);
static BoundNode *bound_node_find(BoundNode *node, BoundNodeType type, SyntaxNode *variable);
static bool       resolve_binary_expression_type(Operator op, TypeSpec lhs, TypeSpec rhs, TypeSpec *ret);
static bool       resolve_type_node(SyntaxNode *type_node, TypeSpec *typespec);
static int        bind_nodes(BoundNode *parent, SyntaxNode *first, BoundNode **first_dst, BindContext *ctx);
static BoundNode *bind_node(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx);
static BoundNode *rebind_node(BoundNode *node, BindContext *ctx);
static void       rebind_nodes(BoundNode *parent, BoundNode **first, BindContext *ctx);

#undef SYNTAXNODETYPE_ENUM
#define SYNTAXNODETYPE_ENUM(type) __attribute__((unused)) static BoundNode *bind_##type(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx);
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

BindContext *context_make_subcontext(BindContext *ctx)
{
    BindContext *ret = allocate(sizeof(BindContext));
    ret->parent = ctx;
    return ret;
}

void context_function_is_main(BindContext *ctx, BoundNode *function)
{
    if (ctx->parent) {
        context_function_is_main(ctx->parent, function);
        return;
    }
    if (function->type != BNT_FUNCTION) {
        return;
    }
    if (!sv_eq(function->name, sv_from("main"))) {
        return;
    }
    if (typeid_canonical_type_id(function->typespec.type_id) != I32_ID) {
        fatal("main function must return an integer");
    }
    if (function->function.parameter) {
        if (!function->function.parameter->next) {
            fatal("main function can have at most one parameter");
        }
        type_id         param_type = typeid_canonical_type_id(function->function.parameter->typespec.type_id);
        ExpressionType *et = type_registry_get_type_by_id(param_type);
        assert(et);
        if (et->type_id != ARRAY_ID || et->num_arguments != 1 || et->template_arguments[0].arg_type != TPT_TYPE || et->template_arguments[0].type != STRING_ID) {
            fatal("main function parameter must be an string array");
        }
    }
    if (ctx->main) {
        fatal("Multiple main functions defined");
    }
    ctx->main = true;
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

BoundNode *bound_node_find_here(BoundNode *node, BoundNodeType type, SyntaxNode *variable)
{
    StringView name = variable->name;
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
    case BNT_IMPORT: {
        for (BoundNode *module = node->import.modules; module; module = module->next) {
            BoundNode *n = bound_node_find_here(module, type, variable);
            if (n) {
                return n;
            }
        }
    } break;
    case BNT_PROGRAM: {
        if (type == BNT_IMPORT) {
            for (BoundNode *n = node->program.imports; n; n = n->next) {
                assert(n->type == BNT_IMPORT);
                if (sv_eq(n->name, name)) {
                    return n;
                }
            }
        }
        for (BoundNode *module = node->program.modules; module; module = module->next) {
            BoundNode *n = bound_node_find_here(module, type, variable);
            if (n) {
                return n;
            }
        }
        if (!has_option("no-std")) {
            for (BoundNode *import = node->program.imports; import; import = import->next) {
                if (sv_eq_cstr(import->name, "std")) {
                    BoundNode *n = bound_node_find_here(import, type, variable);
                    if (n) {
                        return n;
                    }
                }
            }
        }
    } break;
    default:
        break;
    }
    return NULL;
}

BoundNode *bound_node_find(BoundNode *node, BoundNodeType type, SyntaxNode *variable)
{
    BoundNode *ret = bound_node_find_here(node, type, variable);
    if (ret) {
        return ret;
    }
    BoundNode *p = node->parent;
    if (!p) {
        return NULL;
    }
    ret = bound_node_find(p, type, variable);
    return ret;
}

typedef struct binary_operator_signature {
    Operator    op;
    BuiltinType lhs;
    BuiltinType rhs;
    BuiltinType result;
} BinaryOperatorSignature;

typedef struct unary_operator_signature {
    Operator    op;
    BuiltinType operand;
    BuiltinType result;
} UnaryOperatorSignature;

#define ARITHMETIC_OPS(T)                                       \
    { .op = OP_ADD, .lhs = T, .rhs = T, .result = T },          \
        { .op = OP_SUBTRACT, .lhs = T, .rhs = T, .result = T }, \
        { .op = OP_MULTIPLY, .lhs = T, .rhs = T, .result = T }, \
        { .op = OP_DIVIDE, .lhs = T, .rhs = T, .result = T },

#define COMPARISON_OPS(T)                                                    \
    { .op = OP_EQUALS, .lhs = T, .rhs = T, .result = BIT_BOOL },             \
        { .op = OP_NOT_EQUALS, .lhs = T, .rhs = T, .result = BIT_BOOL },     \
        { .op = OP_GREATER_EQUALS, .lhs = T, .rhs = T, .result = BIT_BOOL }, \
        { .op = OP_LESS_EQUALS, .lhs = T, .rhs = T, .result = BIT_BOOL },    \
        { .op = OP_LESS, .lhs = T, .rhs = T, .result = BIT_BOOL },           \
        { .op = OP_GREATER, .lhs = T, .rhs = T, .result = BIT_BOOL },

#define BITWISE_OPS(T)                                            \
    { .op = OP_BITWISE_AND, .lhs = T, .rhs = T, .result = T },    \
        { .op = OP_BITWISE_OR, .lhs = T, .rhs = T, .result = T }, \
        { .op = OP_BITWISE_XOR, .lhs = T, .rhs = T, .result = T },

#define SHIFT_OPS(T)                                                   \
    { .op = OP_BIT_SHIFT_LEFT, .lhs = T, .rhs = BIT_U8, .result = T }, \
        { .op = OP_BIT_SHIFT_RIGHT, .lhs = T, .rhs = BIT_U8, .result = T },

// clang-format off
static BinaryOperatorSignature s_operator_signatures[] = {
#undef INTEGERTYPE
#define INTEGERTYPE(dt) ARITHMETIC_OPS(BIT_##dt)
INTEGERTYPES_WITH_BOOL(INTEGERTYPE)
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) \
    { .op = OP_MODULO, .lhs = BIT_##dt, .rhs = BIT_##dt, .result = BIT_##dt },
    INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
#define INTEGERTYPE(dt) COMPARISON_OPS(BIT_##dt)
    INTEGERTYPES_WITH_BOOL(INTEGERTYPE)
#undef INTEGERTYPE
#define INTEGERTYPE(dt) BITWISE_OPS(BIT_##dt)
    INTEGERTYPES_WITH_BOOL(INTEGERTYPE)
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) SHIFT_OPS(BIT_##dt)
    INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) \
    { .op = OP_RANGE, .lhs = BIT_##dt, .rhs = BIT_##dt, .result = BIT_RANGE },
    INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE

    ARITHMETIC_OPS(BIT_FLOAT)
    COMPARISON_OPS(BIT_FLOAT)

    { .op = OP_ADD, .lhs = BIT_STRING, .rhs = BIT_STRING, .result = BIT_STRING },
    { .op = OP_MULTIPLY, .lhs = BIT_STRING, .rhs = BIT_I32, .result = BIT_STRING },
    COMPARISON_OPS(BIT_STRING)

    { .op = OP_ADD, .lhs = BIT_POINTER, .rhs = BIT_U64, .result = BIT_POINTER },
    { .op = OP_SUBTRACT, .lhs = BIT_POINTER, .rhs = BIT_U64, .result = BIT_POINTER },
    COMPARISON_OPS(BIT_POINTER)

    { .op = OP_LOGICAL_AND, .lhs = BIT_BOOL, .rhs = BIT_BOOL, .result = BIT_BOOL },
    { .op = OP_LOGICAL_OR, .lhs = BIT_BOOL, .rhs = BIT_BOOL, .result = BIT_BOOL },

    { .op = OP_SUBSCRIPT, .lhs = BIT_ARRAY, .rhs = BIT_I32, .result = BIT_PARAMETER },
    { .op = OP_SUBSCRIPT, .lhs = BIT_ARRAY, .rhs = BIT_U32, .result = BIT_PARAMETER },
    { .op = OP_SUBSCRIPT, .lhs = BIT_ARRAY, .rhs = BIT_U64, .result = BIT_PARAMETER },

    { .op = OP_INVALID, .lhs = BIT_NOTYPE, .rhs = BIT_NOTYPE, .result = BIT_NOTYPE }
};

static UnaryOperatorSignature s_unary_operator_signatures[] = {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size)                  \
    { .op = OP_IDENTITY, .operand = BIT_##dt, .result = BIT_##dt },      \
    { .op = OP_NEGATE, .operand = BIT_##dt, .result = BIT_##dt },        \
    { .op = OP_BITWISE_INVERT, .operand = BIT_##dt, .result = BIT_##dt },
    INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    { .op = OP_LOGICAL_INVERT, .operand = BIT_BOOL, .result = BIT_BOOL },
    { .op = OP_CARDINALITY, .operand = BIT_ARRAY, .result = BIT_U64 },
    { .op = OP_CARDINALITY, .operand = BIT_STRING, .result = BIT_U64 },
    { .op = OP_INVALID, .operand = BIT_NOTYPE, .result = BIT_NOTYPE }
};
// clang-format on

bool resolve_binary_expression_type(Operator op, TypeSpec lhs, TypeSpec rhs, TypeSpec *ret)
{
    BuiltinType bit_lhs = typeid_builtin_type(lhs.type_id);
    BuiltinType bit_rhs = typeid_builtin_type(rhs.type_id);

    for (size_t ix = 0; s_operator_signatures[ix].op != OP_INVALID; ++ix) {
        if (s_operator_signatures[ix].op == op && s_operator_signatures[ix].lhs == bit_lhs && s_operator_signatures[ix].rhs == bit_rhs) {
            type_id result = type_registry_id_of_builtin_type(s_operator_signatures[ix].result);
            if (typeid_is_template(result)) {
                if (typeid_is_specialization(lhs.type_id) && typeid_specializes(lhs.type_id) == result) {
                    *ret = lhs;
                    return true;
                } else if (typeid_is_specialization(rhs.type_id) && typeid_specializes(rhs.type_id) == result) {
                    *ret = rhs;
                    return true;
                } else {
                    // FIXME A lot of assumptions here.
                    ret->type_id = MUST(TypeID,
                        type_specialize_template(
                            result,
                            1,
                            (TemplateArgument[]) {
                                {
                                    .name = sv_from("T"),
                                    .arg_type = TPT_TYPE,
                                    .type = lhs.type_id,
                                },
                            }));
                    return true;
                }
            } else if (result == BIT_PARAMETER) {
                assert(typeid_is_specialization(lhs.type_id));
                ExpressionType *type = type_registry_get_type_by_id(lhs.type_id);
                assert(type->num_arguments == 1);
                assert(type->template_arguments[0].arg_type == TPT_TYPE);
                *ret = (TypeSpec) { .type_id = type->template_arguments[0].type, .optional = false };
                return true;
            } else {
                *ret = (TypeSpec) { .type_id = result, .optional = false };
                return true;
            }
        }
    }
    return false;
}

bool resolve_unary_expression_type(Operator op, BoundNode *operand, TypeSpec *ret)
{
    if (op == OP_ADDRESS_OF) {
        if (operand->type != BNT_VARIABLE) {
            return false;
        }
        ret->type_id = typeid_pointer_to(operand->typespec.type_id);
        return true;
    }
    if (op == OP_DEREFERENCE) {
        if (typeid_builtin_type(operand->typespec.type_id) != BIT_POINTER) {
            return false;
        }
        ExpressionType   *et = type_registry_get_type_by_id(operand->typespec.type_id);
        TemplateArgument *template_arg = type_get_argument(et, sv_from("T"));
        ret->type_id = template_arg->type;
        return true;
    }

    type_id     type = operand->typespec.type_id;
    BuiltinType bit_operand = typeid_builtin_type(type);

    for (size_t ix = 0; s_unary_operator_signatures[ix].op != OP_INVALID; ++ix) {
        if (s_unary_operator_signatures[ix].op == op && s_unary_operator_signatures[ix].operand == bit_operand) {
            type_id result = type_registry_id_of_builtin_type(s_operator_signatures[ix].result);
            if (typeid_is_template(result)) {
                if (typeid_is_specialization(type) && typeid_specializes(type) == result) {
                    *ret = operand->typespec;
                    return true;
                } else {
                    return false;
                }
            } else if (result == BIT_PARAMETER) {
                assert(typeid_is_specialization(type));
                ExpressionType *et = type_registry_get_type_by_id(type);
                assert(et->num_arguments == 1);
                assert(et->template_arguments[0].arg_type == TPT_TYPE);
                *ret = (TypeSpec) { .type_id = et->template_arguments[0].type, .optional = false };
                return true;
            } else {
                *ret = (TypeSpec) { .type_id = result, .optional = false };
                return true;
            }
        }
    }
    return false;
}

ErrorOrTypeID resolve_type(TypeDescr *type_descr)
{
    ExpressionType *et = type_registry_get_type_by_name(type_descr->name);
    if (et == NULL) {
        ErrorOrTypeID_error(TypeError, 0, "Unknown type '%.*s'", type_descr->name);
    }
    if (type_descr->size == 0) {
        RETURN(TypeID, typeid_canonical_type_id(et->type_id));
    }
    if (type_descr->size != et->num_parameters) {
        // TODO: Partial specialization?
        ErrorOrTypeID_error(TypeError, 0,
            "Invalid number of template arguments for template '%.*s': expected %d, got %d",
            type_descr->name, et->num_parameters, type_descr->size);
    }
    TemplateArgument *arguments = allocate_array(TemplateArgument, type_descr->size);
    for (size_t ix = 0; ix < type_descr->size; ++ix) {
        assert(et->template_parameters[ix].type == TPT_TYPE);
        type_id arg_type = TRY(TypeID, resolve_type(type_descr->elements[ix]));
        arguments[ix].arg_type = TPT_TYPE;
        arguments[ix].name = et->template_parameters[ix].name;
        arguments[ix].type = arg_type;
    }
    return type_specialize_template(et->type_id, type_descr->size, arguments);
}

bool resolve_type_node(SyntaxNode *type_node, TypeSpec *typespec)
{
    assert(type_node->type == SNT_TYPE);
    ErrorOrTypeID type_id_maybe = resolve_type(&type_node->type_descr);
    if (ErrorOrTypeID_is_error(type_id_maybe)) {
        fprintf(stderr, "Could not resolve type: %s\n", Error_to_string(type_id_maybe.error));
        return false;
    }
    (*typespec).type_id = type_id_maybe.value;
    (*typespec).optional = false;
    return true;
}

BoundNode *coerce_node(BoundNode *node, type_id type)
{
    type_id node_type = node->typespec.type_id;
    if (node_type == type) {
        return node;
    }
    BuiltinType bit_node = typeid_builtin_type(node_type);
    BuiltinType bit_type = typeid_builtin_type(type);
    if (!BuiltinType_is_integer(bit_node) || !BuiltinType_is_integer(bit_type)) {
        return NULL;
    }
    if (node->type != BNT_INTEGER) {
        if (BuiltinType_integer_type(bit_node) > BuiltinType_integer_type(bit_type)) {
            return NULL;
        }
        if (BuiltinType_is_unsigned(bit_node) != BuiltinType_is_unsigned(bit_type)) {
            return NULL;
        }
        BoundNode *cast = bound_node_make(BNT_CAST, node->parent);
        node->parent = cast;
        cast->cast_expr.expr = node;
        cast->cast_expr.cast_to = type;
        return cast;
    } else {
        OptionalInteger coerced_maybe = integer_coerce_to(node->integer, BuiltinType_integer_type(bit_type));
        if (coerced_maybe.has_value) {
            node->integer = coerced_maybe.value;
            node->typespec.type_id = type;
            return node;
        }
        return NULL;
    }
}

__attribute__((unused)) BoundNode *bind_ASSIGNMENT(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *decl = bound_node_find(parent, BNT_VARIABLE_DECL, stmt->assignment.variable);
    if (!decl) {
        fprintf(stderr, "Assignment to undeclared variable '" SV_SPEC "'\n", SV_ARG(stmt->name));
        return bound_node_make_unbound(parent, stmt, ctx);
    }
    ExpressionType *et = type_registry_get_type_by_id(decl->typespec.type_id);
    assert(et != NULL);

    BoundNode *ret = bound_node_make(BNT_ASSIGNMENT, parent);
    ret->name = stmt->name;
    ret->typespec = decl->typespec;

    BoundNode *variable = bound_node_make(BNT_VARIABLE, ret);
    variable->name = stmt->assignment.variable->name;
    variable->typespec = ret->typespec;
    variable->variable.type = ret->typespec.type_id;
    ret->assignment.variable = variable;
    BoundNode **name_part = &ret->assignment.variable->variable.subscript;
    SyntaxNode *name_stmt;
    for (name_stmt = stmt->assignment.variable->variable.subscript; name_stmt != NULL; name_stmt = name_stmt->variable.subscript) {
        if (type_kind(et) != TK_AGGREGATE) {
            fatal(LOC_SPEC "Type '" SV_SPEC "' is not an aggregate", LOC_ARG(name_stmt->token.loc), SV_ARG(et->name));
        }
        TypeComponent *comp = type_get_component(et, name_stmt->name);
        if (comp == NULL) {
            fatal(LOC_SPEC "Type '" SV_SPEC "' has no component named '" SV_SPEC "'",
                SN_LOC_ARG(name_stmt), SV_ARG(et->name), SV_ARG(name_stmt->name));
        }
        et = type_registry_get_type_by_id(comp->type_id);
        if (!type_is_concrete(et)) {
            fatal(LOC_SPEC "Type '" SV_SPEC "' must be specialized", LOC_ARG(name_stmt->token.loc), SV_ARG(et->name));
        }
        *name_part = bound_node_make(BNT_VARIABLE, parent);
        (*name_part)->name = name_stmt->name;
        (*name_part)->variable.type = et->type_id;
        ret->typespec = variable->typespec = (*name_part)->typespec = (TypeSpec) { et->type_id, false };
        name_part = &(*name_part)->next;
    }
    for (BoundNode *component = ret->variable.subscript; component; component = component->variable.subscript) {
        component->typespec = ret->typespec;
    }

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

BoundNode *short_circuit_logical_operators(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_TERNARYEXPRESSION, parent);
    BoundNode *lhs = bind_node(ret, stmt->binary_expr.lhs, ctx);
    BoundNode *rhs = bind_node(ret, stmt->binary_expr.rhs, ctx);
    if (lhs->type == BNT_UNBOUND_NODE || rhs->type == BNT_UNBOUND_NODE) {
        return bound_node_make_unbound(parent, stmt, NULL);
    }
    if (typeid_canonical_type_id(lhs->typespec.type_id) != BOOL_ID || typeid_canonical_type_id(rhs->typespec.type_id) != BOOL_ID) {
        fatal(LOC_SPEC "Both operands of a logical operator must be of type 'bool'", SN_LOC_ARG(stmt));
    }
    ret->typespec = (TypeSpec) { BOOL_ID, false };
    ret->ternary_expr.condition = lhs;
    if (stmt->binary_expr.operator== OP_LOGICAL_OR) {
        ret->ternary_expr.if_true = bound_node_make(BNT_BOOL, ret);
        ret->ternary_expr.if_true->name = sv_from("true");
        ret->ternary_expr.if_true->typespec = (TypeSpec) { BOOL_ID, false };
        ret->ternary_expr.if_false = rhs;
    } else { // OP_LOGICAL_AND
        ret->ternary_expr.if_true = rhs;
        ret->ternary_expr.if_false = bound_node_make(BNT_BOOL, ret);
        ret->ternary_expr.if_false->name = sv_from("false");
        ret->ternary_expr.if_false->typespec = (TypeSpec) { BOOL_ID, false };
    }
    return ret;
}

BoundNode *datum_to_node(Datum *d, BoundNode *parent)
{
    BoundNode *ret;
    if (datum_kind(d) == TK_PRIMITIVE) {
        BuiltinType bit = typeid_builtin_type(d->type);
        bool        is_int = BuiltinType_is_integer(bit);
        if (is_int) {
            ret = bound_node_make(BNT_INTEGER, parent);
            ret->name = datum_sprint(d);
            ret->integer = d->integer;
            ret->typespec = (TypeSpec) { .type_id = type_registry_id_of_builtin_type(bit), .optional = false };
            return ret;
        }
        switch (bit) {
        case BIT_BOOL:
            ret = bound_node_make(BNT_BOOL, parent);
            ret->name = datum_sprint(d);
            ret->typespec = (TypeSpec) { .type_id = BOOL_ID, .optional = false };
            return ret;
        case BIT_FLOAT:
            ret = bound_node_make(BNT_DECIMAL, parent);
            ret->name = datum_sprint(d);
            ret->typespec = (TypeSpec) { .type_id = FLOAT_ID, .optional = false };
            return ret;
        default:
            break;
        }
    }
    if (d->type == STRING_ID) {
        ret = bound_node_make(BNT_STRING, parent);
        ret->name = datum_sprint(d);
        ret->typespec = (TypeSpec) { .type_id = STRING_ID, .optional = false };
        return ret;
    }
    return NULL;
}

Datum *node_to_datum(BoundNode *node)
{
    IRFunction expression = evaluate(node);
    return evaluate_function(expression);
}

BoundNode *evaluate_node(BoundNode *node)
{
    Datum *evaluated = node_to_datum(node);
    if (evaluated) {
        BoundNode *evaluated_node = datum_to_node(evaluated, node->parent);
        if (evaluated_node) {
            return evaluated_node;
        }
    }
    return node;
}

__attribute__((unused)) BoundNode *bind_BINARYEXPRESSION(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    if (stmt->binary_expr.operator== OP_LOGICAL_OR || stmt->binary_expr.operator== OP_LOGICAL_AND) {
        return short_circuit_logical_operators(parent, stmt, ctx);
    }
    BoundNode *ret = bound_node_make(BNT_BINARYEXPRESSION, parent);
    BoundNode *lhs = bind_node(ret, stmt->binary_expr.lhs, ctx);
    BoundNode *rhs = bind_node(ret, stmt->binary_expr.rhs, ctx);
    if (lhs->type == BNT_UNBOUND_NODE || rhs->type == BNT_UNBOUND_NODE) {
        return bound_node_make_unbound(parent, stmt, NULL);
    }
    TypeSpec type;
    if (!resolve_binary_expression_type(stmt->binary_expr.operator, lhs->typespec, rhs->typespec, &type)) {
        BoundNode *coerced = coerce_node(rhs, lhs->typespec.type_id);
        if (coerced && resolve_binary_expression_type(stmt->binary_expr.operator, lhs->typespec, coerced->typespec, &type)) {
            rhs = coerced;
        } else {
            coerced = coerce_node(lhs, rhs->typespec.type_id);
            if (!coerced || !resolve_binary_expression_type(stmt->binary_expr.operator, coerced->typespec, rhs->typespec, &type)) {
                ExpressionType *lhs_type = type_registry_get_type_by_id(lhs->typespec.type_id);
                ExpressionType *rhs_type = type_registry_get_type_by_id(rhs->typespec.type_id);
                fatal("Could not resolve return type of operator '%s' with lhs type '" SV_SPEC "' and rhs type '" SV_SPEC "'",
                    Operator_name(stmt->binary_expr.operator), SV_ARG(lhs_type->name), SV_ARG(rhs_type->name));
            }
            lhs = coerced;
        }
    }
    ret->typespec = type;
    ret->name = sv_from(Operator_name(stmt->binary_expr.operator));
    ret->binary_expr.lhs = lhs;
    ret->binary_expr.rhs = rhs;
    ret->binary_expr.operator= stmt->binary_expr.operator;
    return evaluate_node(ret);
}

__attribute__((unused)) BoundNode *bind_BLOCK(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode   *ret = bound_node_make(BNT_BLOCK, parent);
    BindContext *block_ctx = context_make_subcontext(ctx);

    bind_nodes(ret, stmt->block.statements, &ret->block.statements, block_ctx);
    return ret;
}

__attribute__((unused)) BoundNode *bind_BOOL(BoundNode *parent, SyntaxNode *stmt, BindContext *)
{
    BoundNode *ret = bound_node_make(BNT_BOOL, parent);
    ret->name = stmt->token.text;
    ret->typespec = (TypeSpec) { BOOL_ID, false };
    ret->integer = integer_create(U8, sv_eq_cstr(ret->name, "true") ? 1 : 0);
    return ret;
}

__attribute__((unused)) BoundNode *bind_BREAK(BoundNode *parent, SyntaxNode *stmt, BindContext *)
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

__attribute__((unused)) BoundNode *bind_CAST(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *cast = bound_node_make(BNT_CAST, parent);
    cast->name = stmt->name;
    cast->cast_expr.expr = bind_node(cast, stmt->cast_expr.expr, ctx);
    if (cast->cast_expr.expr->type == BNT_UNBOUND_NODE) {
        return bound_node_make_unbound(parent, stmt, ctx);
    }
    TypeSpec spec;
    if (!resolve_type_node(stmt->cast_expr.cast_to, &spec)) {
        return bound_node_make_unbound(parent, stmt, ctx);
    }
    cast->cast_expr.cast_to = spec.type_id;
    return cast;
}

__attribute__((unused)) BoundNode *bind_CONTINUE(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    return bind_BREAK(parent, stmt, ctx);
}

__attribute__((unused)) BoundNode *bind_DECIMAL(BoundNode *parent, SyntaxNode *stmt, BindContext *)
{
    BoundNode *ret = bound_node_make(BNT_DECIMAL, parent);
    ret->name = stmt->name;
    ret->typespec = (TypeSpec) { FLOAT_ID, false };
    ret->decimal_value = strtod(ret->name.ptr, NULL);
    return ret;
}

__attribute__((unused)) BoundNode *bind_ENUMERATION(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_ENUMERATION, parent);
    ret->name = stmt->name;
    ret->enumeration.underlying_type = (TypeSpec) { .type_id = I32_ID, .optional = false };
    if (stmt->enumeration.underlying_type != NULL) {
        if (!resolve_type_node(stmt->enumeration.underlying_type, &ret->enumeration.underlying_type)) {
            return bound_node_make_unbound(parent, stmt, ctx);
        }
    }
    if (bind_nodes(ret, stmt->enumeration.values, &ret->enumeration.values, ctx)) {
        return bound_node_make_unbound(ret, stmt, ctx);
    }
    BuiltinType bit = typeid_builtin_type(ret->enumeration.underlying_type.type_id);
    if (!BuiltinType_is_integer(bit)) {
        fatal(LOC_SPEC "Underlying type of enumeration must be an integer type", SN_LOC_ARG(stmt));
    }
    Integer cur_value = integer_create(BuiltinType_integer_type(bit), 0);

    EnumValues values = { 0 };
    for (BoundNode *value = ret->enumeration.values; value; value = value->next) {
        if (value->enum_value.underlying_value == NULL) {
            value->enum_value.underlying_value = bound_node_make(BNT_INTEGER, value);
            value->enum_value.underlying_value->integer = cur_value;
        } else {
            assert(value->enum_value.underlying_value->type == BNT_INTEGER);
            cur_value = value->enum_value.underlying_value->integer;
        }
        da_append_EnumValue(&values, (EnumValue) { .name = value->name, .value = cur_value });
        cur_value = integer_increment(cur_value);
    }
    type_id enum_id = MUST(TypeID,
        type_registry_make_enumeration(
            ret->name, ret->enumeration.underlying_type.type_id, &values));
    ret->typespec = (TypeSpec) { .type_id = enum_id, .optional = false };
    return ret;
}

__attribute__((unused)) BoundNode *bind_ENUM_VALUE(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_ENUM_VALUE, parent);
    ret->name = stmt->name;
    ret->enum_value.underlying_value = NULL;
    if (stmt->enum_value.underlying_value) {
        BoundNode *underlying = bind_node(ret, stmt->enum_value.underlying_value, ctx);
        Datum     *evaluated = node_to_datum(underlying);
        if (evaluated) {
            BoundNode *evaluated_node = datum_to_node(evaluated, ret);
            if (evaluated_node) {
                ret->enum_value.underlying_value = evaluated_node;
            }
        }
        if (ret->enum_value.underlying_value == NULL) {
            fatal("Non-const underlying value for enum value '%.*s.%.*s'", SV_ARG(parent->name), SV_ARG(ret->name));
        }
        if (!typespec_assignment_compatible(parent->enumeration.underlying_type, ret->enum_value.underlying_value->typespec)) {
            fatal("Incompatible underlying value type for enum value '%.*s.%.*s': %.*s <-> %.*s",
                SV_ARG(parent->name), SV_ARG(ret->name),
                SV_ARG(typespec_name(parent->enumeration.underlying_type)),
                SV_ARG(typespec_name(ret->enum_value.underlying_value->typespec)));
        }
    }
    return ret;
}

__attribute__((unused)) BoundNode *bind_FOR(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
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

__attribute__((unused)) BoundNode *bind_FUNCTION(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
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
        ErrorOrTypeID variant_id_maybe = type_registry_get_variant(RESULT_ID, error_type.type_id, return_type.type_id);
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
    context_function_is_main(ctx, ret);
    return ret;
}

__attribute__((unused)) BoundNode *bind_FUNCTION_CALL(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *fnc = bound_node_find(parent, BNT_FUNCTION, stmt->call.function);
    if (!fnc) {
        fprintf(stderr, "Cannot bind function '" SV_SPEC "'\n", SV_ARG(stmt->name));
        return bound_node_make_unbound(parent, stmt, ctx);
    }
    if (fnc->function.function_impl->type == BNT_MACRO) {
        Datum **args = allocate_array(Datum *, 3);
        args[0] = datum_allocate(POINTER_ID);
        args[0]->pointer = parent;
        args[1] = datum_allocate(POINTER_ID);
        args[1]->pointer = stmt;
        args[2] = datum_allocate(POINTER_ID);
        args[3]->pointer = ctx;

        Datum *processed = datum_allocate(POINTER_ID);
        native_call(fnc->function.function_impl->name, 3, args, processed);
        BoundNode *macro_ret = (BoundNode *) processed->pointer;
        if (macro_ret) {
            if (macro_ret->type == BNT_FUNCTION_CALL) {
                if (macro_ret->typespec.type_id == VOID_ID) {
                    fatal("Macro '%.*s' returned void function call to '%.*s'",
                        SV_ARG(fnc->function.function_impl->name),
                        SV_ARG(macro_ret->name));
                }
                macro_ret->call.discard_result = false;
            }
        }
        return macro_ret;
    }

    BoundNode *ret = bound_node_make(BNT_FUNCTION_CALL, parent);
    ret->name = stmt->name;
    ret->call.function = fnc;
    ret->call.discard_result = stmt->call.discard_result;
    ret->typespec = fnc->typespec;
    bind_nodes(ret, stmt->call.arguments, &ret->call.argument, ctx);
    // FIXME Typecheck parameters

    if (fnc->function.function_impl->type == BNT_MACRO) {
        Datum **args = allocate_array(Datum *, 2);
        args[0] = datum_allocate(POINTER_ID);
        args[0]->pointer = ret;
        args[1] = datum_allocate(POINTER_ID);
        args[1]->pointer = ctx;

        Datum *processed = datum_allocate(POINTER_ID);
        native_call(fnc->function.function_impl->name, 2, args, processed);
        BoundNode *macro_ret = (BoundNode *) processed->pointer;
        if (macro_ret) {
            assert(macro_ret->type == BNT_FUNCTION_CALL);
            macro_ret->call.discard_result = stmt->call.discard_result;
        }
        return macro_ret;
    }
    return ret;
}

__attribute__((unused)) BoundNode *bind_FUNCTION_IMPL(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_FUNCTION_IMPL, parent);
    bind_nodes(ret, stmt->function_impl.statements, &ret->block.statements, ctx);
    return ret;
}

__attribute__((unused)) BoundNode *bind_IF(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_IF, parent);
    ret->if_statement.condition = bind_node(ret, stmt->if_statement.condition, ctx);
    ret->if_statement.if_true = bind_node(ret, stmt->if_statement.if_true, ctx);
    ret->if_statement.if_false = bind_node(ret, stmt->if_statement.if_false, ctx);
    return ret;
}

__attribute__((unused)) BoundNode *bind_IMPORT(BoundNode *, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_IMPORT, NULL);
    ret->name = stmt->name;

    bind_nodes(ret, stmt->import.modules, &ret->import.modules, ctx);
    return ret;
}

__attribute__((unused)) BoundNode *bind_INTEGER(BoundNode *parent, SyntaxNode *stmt, BindContext *)
{
    BoundNode *ret = bound_node_make(BNT_INTEGER, parent);

    ret->name = stmt->token.text;
    ret->typespec = (TypeSpec) { type_registry_id_of_integer_type(stmt->integer.type), false };
    BuiltinType        bit = typeid_builtin_type(typeid_canonical_type_id(ret->typespec.type_id));
    IntegerParseResult parse_result = sv_parse_integer(ret->name, stmt->integer.type);
    if (!parse_result.success) {
        fatal("Cannot hold value '%.*s' in integer of type '%.*s'", SV_ARG(ret->name), SV_ARG(typeid_name(ret->typespec.type_id)));
    }
    ret->integer = parse_result.integer;
    return ret;
}

__attribute__((unused)) BoundNode *bind_LABEL(BoundNode *, SyntaxNode *stmt, BindContext *)
{
    SyntaxNode *breakable = stmt->next;
    if (breakable->type == SNT_LOOP || breakable->type == SNT_WHILE) {
        breakable->name = stmt->name;
    }
    return NULL;
}

__attribute__((unused)) BoundNode *bind_LOOP(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_LOOP, parent);
    ret->name = stmt->name;
    ret->block.statements = bind_node(ret, stmt->block.statements, ctx);
    return ret;
}

__attribute__((unused)) BoundNode *bind_MACRO(BoundNode *parent, SyntaxNode *stmt, BindContext *)
{
    BoundNode *ret = bound_node_make(BNT_MACRO, parent);
    ret->name = stmt->name;
    return ret;
}

__attribute__((unused)) BoundNode *bind_MODULE(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode   *ret = bound_node_make(BNT_MODULE, parent);
    BindContext *mod_ctx = context_make_subcontext(ctx);
    ret->name = stmt->name;
    if (sv_endswith(ret->name, sv_from(".scribble"))) {
        ret->name.length = ret->name.length - 9;
    }
    bind_nodes(ret, stmt->block.statements, &ret->block.statements, mod_ctx);
    return ret;
}

__attribute__((unused)) BoundNode *bind_NAME(BoundNode *, SyntaxNode *, BindContext *)
{
    UNREACHABLE();
}

__attribute__((unused)) BoundNode *bind_NATIVE_FUNCTION(BoundNode *parent, SyntaxNode *stmt, BindContext *)
{
    BoundNode *ret = bound_node_make(BNT_NATIVE_FUNCTION, parent);
    ret->name = stmt->name;
    return ret;
}

__attribute__((unused)) BoundNode *bind_PARAMETER(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
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

void program_collect_types(BoundNode *program)
{
    BoundNode **last_type = &program->program.types;
    for (size_t ix = FIRST_CUSTOM_IX; ix < NEXT_CUSTOM_IX; ++ix) {
        BoundNode      *type_node = NULL;
        ExpressionType *et = type_registry_get_type_by_index(ix);
        switch (type_kind(et)) {
        case TK_PRIMITIVE: {
            type_node = bound_node_make(BNT_TYPE, program);
            type_node->name = et->name;
            type_node->typespec = (TypeSpec) { et->type_id, false };
        } break;
        case TK_ALIAS: {
            type_node = bound_node_make(BNT_TYPE, program);
            type_node->name = et->name;
            type_node->typespec = (TypeSpec) { et->alias_for_id, false };
        } break;
        case TK_AGGREGATE: {
            type_node = bound_node_make(BNT_STRUCT, program);
            type_node->name = et->name;
            BoundNode **dst = &type_node->compound_def.components;
            BoundNode  *last = NULL;
            for (size_t comp_ix = 0; comp_ix < et->components.num_components; ++comp_ix) {
                *dst = bound_node_make(BNT_TYPE_COMPONENT, type_node);
                (*dst)->name = et->components.components[comp_ix].name;
                (*dst)->typespec = (TypeSpec) { et->components.components[comp_ix].type_id, false };
                (*dst)->prev = last;
                last = *dst;
                dst = &last->next;
            }
        } break;
        case TK_ENUM: {
            type_node = bound_node_make(BNT_ENUMERATION, program);
            type_node->name = et->name;
            type_node->typespec = (TypeSpec) { et->type_id, false };
            BoundNode **dst = &type_node->enumeration.values;
            BoundNode  *last = NULL;
            for (size_t comp_ix = 0; comp_ix < et->enumeration.size; ++comp_ix) {
                *dst = bound_node_make(BNT_ENUM_VALUE, type_node);
                (*dst)->name = et->enumeration.elements[comp_ix].name;
                (*dst)->enum_value.underlying_value = bound_node_make(BNT_INTEGER, type_node);
                (*dst)->enum_value.underlying_value->typespec = (TypeSpec) { et->enumeration.underlying_type, false };
                (*dst)->enum_value.underlying_value->integer = et->enumeration.elements[comp_ix].value;
                (*dst)->prev = last;
                last = *dst;
                dst = &last->next;
            }
        } break;
        case TK_VARIANT: {
            type_node = bound_node_make(BNT_VARIANT, program);
            type_node->name = et->name;
            type_node->variant_def.underlying_type = (TypeSpec) { et->variant.enumeration, false };
            BoundNode **dst = &type_node->variant_def.options;
            BoundNode  *last = NULL;
            for (size_t option_ix = 0; option_ix < et->variant.size; ++option_ix) {
                *dst = bound_node_make(BNT_VARIANT_OPTION, type_node);
                (*dst)->name = et->variant.elements[option_ix].name;
                (*dst)->variant_option.payload_type = (TypeSpec) { et->variant.elements[option_ix].type_id, false };
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

__attribute__((unused)) BoundNode *bind_PROGRAM(BoundNode *, SyntaxNode *program, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_PROGRAM, NULL);
    ret->name = program->name;

    bind_nodes(ret, program->program.modules, &ret->program.modules, ctx);
    bind_nodes(ret, program->program.imports, &ret->program.imports, ctx);
    program_collect_types(ret);
    return ret;
}

__attribute__((unused)) BoundNode *bind_RETURN(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_RETURN, parent);
    if (stmt->return_stmt.expression) {
        ret->return_stmt.expression = bind_node(ret, stmt->return_stmt.expression, ctx);
    }
    return ret;
}

__attribute__((unused)) BoundNode *bind_STRING(BoundNode *parent, SyntaxNode *stmt, BindContext *)
{
    BoundNode *ret = bound_node_make(BNT_STRING, parent);
    ret->name = stmt->name;
    ret->typespec = (TypeSpec) { .type_id = type_registry_id_of_builtin_type(BIT_STRING), .optional = false };
    return ret;
}

__attribute__((unused)) BoundNode *bind_STRUCT(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
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
    ErrorOrTypeID struct_id_maybe = type_registry_make_aggregate(stmt->name, num, type_components);
    if (ErrorOrTypeID_is_error(struct_id_maybe)) {
        fatal(LOC_SPEC "Could not create aggregate type: %s", LOC_ARG(stmt->token.loc), Error_to_string(struct_id_maybe.error));
    }
    return NULL;
}

__attribute__((unused)) BoundNode *bind_TERNARYEXPRESSION(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_TERNARYEXPRESSION, parent);
    BoundNode *condition = bind_node(ret, stmt->ternary_expr.condition, ctx);
    BoundNode *if_true = bind_node(ret, stmt->ternary_expr.if_true, ctx);
    BoundNode *if_false = bind_node(ret, stmt->ternary_expr.if_false, ctx);
    if (condition->type == BNT_UNBOUND_NODE || if_true->type == BNT_UNBOUND_NODE || if_false->type == BNT_UNBOUND_NODE) {
        return bound_node_make_unbound(parent, stmt, NULL);
    }
    if (typeid_canonical_type_id(condition->typespec.type_id) != BOOL_ID) {
        fatal(LOC_SPEC "Ternary expression condition must be of type 'bool'", SN_LOC_ARG(stmt));
    }
    if (typeid_canonical_type_id(if_true->typespec.type_id) != typeid_canonical_type_id(if_false->typespec.type_id)) {
        fatal(LOC_SPEC "Ternary expression `true` and `false` values must be of the same type", SN_LOC_ARG(stmt));
    }
    ret->typespec = (TypeSpec) { .type_id = typeid_canonical_type_id(if_true->typespec.type_id), .optional = false };
    ret->name = sv_from("?:");
    ret->ternary_expr.condition = condition;
    ret->ternary_expr.if_true = if_true;
    ret->ternary_expr.if_false = if_false;
    return evaluate_node(ret);
}

__attribute__((unused)) BoundNode *bind_TYPE(BoundNode *, SyntaxNode *, BindContext *)
{
    return NULL;
}

__attribute__((unused)) BoundNode *bind_TYPE_COMPONENT(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
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

__attribute__((unused)) BoundNode *bind_UNARYEXPRESSION(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_UNARYEXPRESSION, parent);

    BoundNode *operand = bind_node(ret, stmt->unary_expr.operand, ctx);
    if (operand->type == BNT_UNBOUND_NODE) {
        return bound_node_make_unbound(parent, stmt, NULL);
    }
    TypeSpec type;
    if (!resolve_unary_expression_type(stmt->unary_expr.operator, operand, &type)) {
        fatal("Could not resolve return type of operator '%s' with operand type '%.*s'",
            Operator_name(stmt->binary_expr.operator), SV_ARG(typeid_name(operand->typespec.type_id)));
    }
    ret->typespec = type;
    ret->name = sv_from(Operator_name(stmt->unary_expr.operator));
    ret->unary_expr.operand = operand;
    ret->unary_expr.operator= stmt->unary_expr.operator;
    return evaluate_node(ret);
}

__attribute__((unused)) BoundNode *bind_VARIABLE(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode      *decl = bound_node_find(parent, BNT_VARIABLE_DECL, stmt);
    BoundNode      *ret = NULL;
    ExpressionType *et = NULL;
    if (decl) {
        if (stmt->variable.subscript == NULL && decl->variable_decl.is_const) {
            switch (decl->variable_decl.init_expr->type) {
            case BNT_BOOL:
            case BNT_INTEGER:
                ret = bound_node_make(decl->variable_decl.init_expr->type, parent);
                ret->typespec = decl->variable_decl.init_expr->typespec;
                ret->integer = decl->variable_decl.init_expr->integer;
                ret->name = decl->variable_decl.init_expr->name;
                return ret;
            case BNT_DECIMAL:
                ret = bound_node_make(decl->variable_decl.init_expr->type, parent);
                ret->typespec = decl->variable_decl.init_expr->typespec;
                ret->decimal_value = decl->variable_decl.init_expr->decimal_value;
                ret->name = decl->variable_decl.init_expr->name;
                return ret;
            case BNT_STRING: {
                ret = bound_node_make(decl->variable_decl.init_expr->type, parent);
                ret->typespec = decl->variable_decl.init_expr->typespec;
                ret->name = decl->variable_decl.init_expr->name;
                return ret;
            }
            default:
                break;
            }
        }
        et = type_registry_get_type_by_id(decl->typespec.type_id);
        assert(et != NULL);
        if (!type_is_concrete(et)) {
            fatal(LOC_SPEC "Type '" SV_SPEC "' must be specialized", LOC_ARG(stmt->token.loc), SV_ARG(et->name));
        }
    }
    if (!et) {
        et = type_registry_get_type_by_name(stmt->name);
        if (et) {
            if (type_kind(et) == TK_ENUM && stmt->variable.subscript != NULL) {
                SyntaxNode *value_name = stmt->variable.subscript;
                if (value_name->next != NULL) {
                    fatal(LOC_SPEC "Enumeration value '" SV_SPEC "' has too many name parts", LOC_ARG(stmt->token.loc), SV_ARG(value_name->name));
                }
                for (size_t ix = 0; ix < et->enumeration.size; ++ix) {
                    EnumValue enum_value = et->enumeration.elements[ix];
                    if (sv_eq(enum_value.name, value_name->name)) {
                        ret = bound_node_make(BNT_INTEGER, parent);
                        ret->typespec = (TypeSpec) { .type_id = et->type_id, .optional = false };
                        ret->integer = enum_value.value;
                        ret->name = enum_value.name;
                        return ret;
                    }
                }
            }
        }
    }
    if (!decl) {
        fprintf(stderr, "Cannot bind variable '" SV_SPEC "'\n", SV_ARG(stmt->name));
        return bound_node_make_unbound(parent, stmt, ctx);
    }

    ret = bound_node_make(BNT_VARIABLE, parent);
    ret->variable.decl = decl;
    ret->name = stmt->name;
    ret->typespec = (TypeSpec) { et->type_id, false };
    ret->variable.type = et->type_id;

    BoundNode **name_part = &ret->variable.subscript;
    SyntaxNode *name_stmt;
    for (name_stmt = stmt->variable.subscript; name_stmt != NULL; name_stmt = name_stmt->variable.subscript) {
        if (type_kind(et) != TK_AGGREGATE) {
            fatal(LOC_SPEC "Type '" SV_SPEC "' is not an aggregate", LOC_ARG(name_stmt->token.loc), SV_ARG(et->name));
        }
        TypeComponent *comp = type_get_component(et, name_stmt->name);
        if (comp == NULL) {
            fatal(LOC_SPEC "Type '" SV_SPEC "' has no component named '" SV_SPEC "'", LOC_ARG(name_stmt->token.loc), SV_ARG(et->name), SV_ARG(name_stmt->name));
        }
        et = type_registry_get_type_by_id(comp->type_id);
        if (!type_is_concrete(et)) {
            fatal(LOC_SPEC "Type '" SV_SPEC "' must be specialized", LOC_ARG(name_stmt->token.loc), SV_ARG(et->name));
        }
        *name_part = bound_node_make(BNT_VARIABLE, parent);
        (*name_part)->name = name_stmt->name;
        (*name_part)->variable.type = et->type_id;
        ret->typespec = (*name_part)->typespec = (TypeSpec) { et->type_id, false };
        name_part = &(*name_part)->next;
    }
    for (BoundNode *component = ret->variable.subscript; component; component = component->variable.subscript) {
        component->typespec = ret->typespec;
    }
    return ret;
}

__attribute__((unused)) BoundNode *bind_VARIABLE_DECL(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *expr = NULL;
    TypeSpec   var_type = { VOID_ID, false };
    if (stmt->variable_decl.var_type != NULL && !resolve_type_node(stmt->variable_decl.var_type, &var_type)) {
        return bound_node_make_unbound(parent, stmt, ctx);
    }
    BoundNode *ret = bound_node_make(BNT_VARIABLE_DECL, parent);
    if (stmt->variable_decl.init_expr) {
        if (bind_nodes(parent, stmt->variable_decl.init_expr, &expr, ctx)) {
            return bound_node_make_unbound(parent, stmt, NULL);
        }
    }
    if (expr) {
        if (expr->next == NULL) { // Not a compound initializer
            if (var_type.type_id != VOID_ID && expr->typespec.type_id != var_type.type_id) {
                fatal(LOC_SPEC "Declaration type '%.*s' and expression type '%.*s' are different",
                    SN_LOC_ARG(stmt),
                    SV_ARG(typeid_name(var_type.type_id)),
                    SV_ARG(typeid_name(expr->typespec.type_id)));
            }
            if (var_type.type_id == VOID_ID) {
                var_type = expr->typespec;
            }
        } else {
            if (var_type.type_id == VOID_ID) {
                fatal(LOC_SPEC "Variable type with compound initializer cannot be inferred", SN_LOC_ARG(stmt));
            }
            ExpressionType *et = type_registry_get_type_by_id(var_type.type_id);
            assert(et);
            if (type_kind(et) != TK_AGGREGATE) {
                fatal(LOC_SPEC "Cannot initialize variables with non-compound types using a compound initializer", LOC_ARG(stmt->token.loc));
            }
            BoundNode *arg = expr;
            for (size_t ix = 0; ix < et->components.num_components; ++ix) {
                TypeComponent *type_component = &et->components.components[ix];
                if (!arg) {
                    fatal(LOC_SPEC "Not enough initializer argument value for " SV_SPEC "." SV_SPEC,
                        SN_LOC_ARG(stmt), SV_ARG(et->name), SV_ARG(type_component->name));
                }
                if (!typespec_assignment_compatible((TypeSpec) { type_component->type_id, false }, arg->typespec)) {
                    fatal(LOC_SPEC "Declaration element type '%.*s' and initializer argument value type '%.*s' are different",
                        SN_LOC_ARG(stmt), SV_ARG(typeid_name(type_component->type_id)), SV_ARG(typespec_name(arg->typespec)));
                }
                arg = arg->next;
            }
            if (arg) {
                fatal(LOC_SPEC "Too many initializer argument values for type '" SV_SPEC "'", SN_LOC_ARG(stmt), et->name);
            }
        }
    } else if (var_type.type_id == VOID_ID) {
        fprintf(stderr, "Could not infer type of variable '" SV_SPEC "'\n", SV_ARG(stmt->name));
        return bound_node_make_unbound(parent, stmt, ctx);
    }
    ret->typespec = var_type;
    ret->name = stmt->name;
    ret->variable_decl.init_expr = expr;
    ret->variable_decl.is_const = stmt->variable_decl.is_const;
    return ret;
}

__attribute__((unused)) BoundNode *bind_VARIANT(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_VARIANT, parent);
    ret->name = stmt->name;
    ret->variant_def.underlying_type = (TypeSpec) { .type_id = I32_ID, .optional = false };
    if (stmt->variant_def.underlying_type != NULL) {
        if (!resolve_type_node(stmt->variant_def.underlying_type, &ret->variant_def.underlying_type)) {
            return bound_node_make_unbound(parent, stmt, ctx);
        }
    }
    if (bind_nodes(ret, stmt->variant_def.options, &ret->variant_def.options, ctx)) {
        return bound_node_make_unbound(ret, stmt, ctx);
    }

    type_id enum_id;
    if (typeid_kind(ret->variant_def.underlying_type.type_id) == TK_PRIMITIVE) {
        BuiltinType bit = typeid_builtin_type(ret->enumeration.underlying_type.type_id);
        if (!BuiltinType_is_integer(bit)) {
            fatal(LOC_SPEC "Underlying type of enumeration must be an integer type", SN_LOC_ARG(stmt));
        }
        Integer cur_value = integer_create(BuiltinType_integer_type(bit), 0);

        EnumValues values = { 0 };
        for (BoundNode *option = ret->variant_def.options; option; option = option->next) {
            if (option->variant_option.underlying_value == NULL) {
                option->variant_option.underlying_value = bound_node_make(BNT_INTEGER, option);
                option->variant_option.underlying_value->integer = cur_value;
            } else {
                assert(option->variant_option.underlying_value->type == BNT_INTEGER);
                cur_value = option->enum_value.underlying_value->integer;
            }
            da_append_EnumValue(&values, (EnumValue) { .name = option->name, .value = cur_value });
            cur_value = integer_increment(cur_value);
        }
        StringBuilder sb = sb_copy_cstr("$");
        sb_append_sv(&sb, ret->name);
        enum_id = MUST(TypeID,
            type_registry_make_enumeration(
                sb.view, ret->variant_def.underlying_type.type_id, &values));
        ret->variant_def.underlying_type.type_id = enum_id;
    } else {
        enum_id = ret->variant_def.underlying_type.type_id;
        if (typeid_kind(enum_id) != TK_ENUM) {
            fatal(LOC_SPEC "Underlying type of variant must be an enumeration type", SN_LOC_ARG(stmt));
        }
    }

    ExpressionType *enum_type = type_registry_get_type_by_id(enum_id);
    assert(enum_type);
    TypeComponents components = { 0 };
    for (size_t ix = 0; ix < enum_type->enumeration.size; ++ix) {
        EnumValue enum_value = enum_type->enumeration.elements[ix];
        da_append_TypeComponent(&components, (TypeComponent) { .kind = CK_TYPE, .name = enum_value.name, .type_id = VOID_ID });
    }
    for (BoundNode *option = ret->variant_def.options; option; option = option->next) {
        for (size_t ix = 0; ix < components.size; ++ix) {
            if (sv_eq(components.elements[ix].name, option->name)) {
                if (components.elements[ix].type_id != VOID_ID) {
                    fatal(LOC_SPEC "Duplicate enumeration value in variant definition", SN_LOC_ARG(stmt));
                }
                components.elements[ix].type_id = option->variant_option.payload_type.type_id;
                break;
            }
        }
    }
    type_id variant_id = MUST(TypeID, type_registry_make_variant(ret->name, enum_id, &components));
    ret->typespec = (TypeSpec) { .type_id = variant_id, .optional = false };
    return ret;
}

__attribute__((unused)) BoundNode *bind_VARIANT_OPTION(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
{
    BoundNode *ret = bound_node_make(BNT_VARIANT_OPTION, parent);
    ret->name = stmt->name;
    ret->variant_option.underlying_value = NULL;
    if (stmt->variant_option.underlying_value) {
        if (typeid_kind(ret->variant_def.underlying_type.type_id) == TK_ENUM) {
            fatal(LOC_SPEC "Cannot specify underlying value for variant option of enumeration type", SN_LOC_ARG(stmt));
        }
        BoundNode *underlying = bind_node(ret, stmt->enum_value.underlying_value, ctx);
        Datum     *evaluated = node_to_datum(underlying);
        if (evaluated) {
            BoundNode *evaluated_node = datum_to_node(evaluated, ret);
            if (evaluated_node) {
                ret->variant_option.underlying_value = evaluated_node;
            }
        }
        if (ret->variant_option.underlying_value == NULL) {
            fatal("Non-const underlying value for enum value '%.*s.%.*s'", SV_ARG(parent->name), SV_ARG(ret->name));
        }
        if (!typespec_assignment_compatible(parent->variant_def.underlying_type, ret->variant_option.underlying_value->typespec)) {
            fatal("Incompatible underlying value type for enum value '%.*s.%.*s': %.*s <-> %.*s",
                SV_ARG(parent->name), SV_ARG(ret->name),
                SV_ARG(typespec_name(parent->variant_def.underlying_type)),
                SV_ARG(typespec_name(ret->variant_option.underlying_value->typespec)));
        }
    }
    ret->variant_option.payload_type = (TypeSpec) { VOID_ID, false };
    if (stmt->variant_option.payload_type != NULL) {
        if (!resolve_type_node(stmt->variant_option.payload_type, &ret->variant_option.payload_type)) {
            return bound_node_make_unbound(parent, stmt, ctx);
        }
    }
    return ret;
}

__attribute__((unused)) BoundNode *bind_WHILE(BoundNode *parent, SyntaxNode *stmt, BindContext *ctx)
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
    case BNT_FOR: {
        node->for_statement.statement = rebind_node(node->for_statement.statement, ctx);
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
        rebind_nodes(node, &node->program.imports, ctx);
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
    if (!ctx->main) {
        fatal("No main function found");
    }
    return ret;
}

BindingObserver register_binding_observer(BindingObserver observer)
{
    BindingObserver ret = s_observer;
    s_observer = observer;
    return ret;
}

#include <fmt.h>

__attribute__((unused)) BoundNode *static_message(BoundNode *node, void *v_ctx)
{
    StringView  fmt;
    int         argc = -1;
    BoundNode **last = NULL;
    FMTArgs     args = { 0 };
    for (BoundNode *arg = node->call.argument; arg; arg = arg->next) {
        Datum *evaluated = node_to_datum(arg);
        if (arg == NULL) {
            fatal("static_message called with run time evaluated arguments");
        }
        if (argc == -1) {
            assert(evaluated->type == STRING_ID);
            fmt = evaluated->string;
        } else {
            FMTArg fmt_arg;
            if (datum_is_integer(evaluated)) {
                fmt_arg.integer = evaluated->integer;
            } else if (evaluated->type == STRING_ID) {
                fmt_arg.sv = evaluated->string;
            } else if (typeid_builtin_type(evaluated->type) == BIT_POINTER) {
                fmt_arg.pointer = evaluated->pointer;
            } else {
                fatal("Unsupported type in static_message");
            }
            da_append_FMTArg(&args, fmt_arg);
        }
        ++argc;
    }
    if (argc < 0) {
        fatal("No format message specified in static_message");
    }
    StringView msg = fmt_format(fmt, args);
    printf("%.*s\n", SV_ARG(msg));
    return NULL;
}

#if 0

__attribute__((unused)) BoundNode *bind_format(BoundNode *parent, SyntaxNode *stmt, void *v_ctx)
{
    BindContext *ctx = (BindContext *) v_ctx;
    BoundNode *fmt_format = bound_node_find(parent, BNT_FUNCTION, sv_from("fmt_format"));
    if (!fmt_format) {
        return bound_node_make_unbound(parent, stmt, ctx);
    }
    BoundNode *ret = bound_node_make(BNT_FUNCTION_CALL, parent);
    ret->call.function = fmt_format;
    size_t argc = 0;
    BoundNode **last = NULL;
    for (SyntaxNode *arg = stmt->arguments.argument; arg; arg = arg->next) {
        if (argc == 0) {
            BoundNode *fmt_arg = bind_node(ret, arg, ctx);
            if (fmt_arg->type == BNT_UNBOUND_NODE) {
                return bound_node_make_unbound(parent, stmt, ctx);
            }
            ret->call.argument = fmt_arg;
            last = &fmt_arg;
        } else {

        }
        ++argc;
    }
    assert(argc > 0);


    if (!fnc) {
        fprintf(stderr, "Cannot bind function '" SV_SPEC "'\n", SV_ARG(stmt->name));
        return bound_node_make_unbound(parent, stmt, ctx);
    }

}
#endif
