/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#define STATIC_ALLOCATOR
#include <allocate.h>
#include <model/syntaxnode.h>

#undef SYNTAXNODETYPE
#define SYNTAXNODETYPE(type)                                                        \
    __attribute__((unused)) void type##_to_json(SyntaxNode *node, JSONValue *json); \
    __attribute__((unused)) void type##_from_json(SyntaxNode *node, JSONValue *json);
SYNTAXNODETYPES(SYNTAXNODETYPE)
#undef SYNTAXNODETYPE

char const *SyntaxNodeType_name(SyntaxNodeType type)
{
    switch (type) {
#undef SYNTAXNODETYPE_ENUM
#define SYNTAXNODETYPE_ENUM(type) \
    case SNT_##type:              \
        return "SNT_" #type;
        SYNTAXNODETYPES(SYNTAXNODETYPE_ENUM)
#undef SYNTAXNODETYPE_ENUM
    default:
        UNREACHABLE();
    }
}

SyntaxNodeType SyntaxNodeType_from_string(StringView type)
{
#undef SYNTAXNODETYPE_ENUM
#define SYNTAXNODETYPE_ENUM(T)            \
    if (sv_eq_ignore_case_cstr(type, #T)) \
        return SNT_##T;
    SYNTAXNODETYPES(SYNTAXNODETYPE_ENUM)
#undef SYNTAXNODETYPE_ENUM
    fatal("Invalid syntax node type '%.*s'", SV_ARG(type));
}

JSONValue type_descr_to_json(TypeDescr *type)
{
    JSONValue ret = json_object();
    json_set_string(&ret, "name", type->name);
    JSONValue components = json_array();
    for (size_t ix = 0; ix < type->size; ++ix) {
        json_append(&components, type_descr_to_json(type->elements[ix]));
    }
    json_set(&ret, "components", components);
    return ret;
}

TypeDescr *type_descr_from_json(JSONValue json)
{
    TypeDescr *ret = allocate_new(TypeDescr);
    ret->name = json_get_string(&json, "name", sv_null());
    JSONValue components = MUST_OPTIONAL(JSONValue, json_get(&json, "components"));
    assert(components.type == JSON_TYPE_ARRAY);
    for (size_t ix = 0; ix < json_len(&components); ++ix) {
        JSONValue  t = MUST_OPTIONAL(JSONValue, json_at(&components, ix));
        TypeDescr *descr = type_descr_from_json(t);
        DIA_APPEND(TypeDescr *, ret, descr);
    }
    return ret;
}

JSONValue node_facade(SyntaxNode *node)
{
    if (node != NULL) {
        JSONValue facade = json_object();
        json_set_string(&facade, "name", node->name);
        json_set_string(&facade, "nodetype", node->name);
        json_set(&facade, "token", token_to_json(node->token));
        json_set_int(&facade, "index", node->index);
        return facade;
    }
    return json_null();
}

void node_facade_sub(SyntaxNode *node, JSONValue *json, char const *name)
{
    if (node != NULL) {
        json_set(json, name, node_facade(node));
    }
}

SyntaxNode *node_from_facade(JSONValue *json)
{
    SyntaxNode *node = allocate_new(SyntaxNode);
    node->type = SyntaxNodeType_from_string(json_get_string(json, "type", sv_null()));
    node->name = json_get_string(json, "name", sv_null());
    node->token = token_from_json(MUST_OPTIONAL(JSONValue, json_get(json, "token")));
    node->index = json_get_int(json, "index", 0);
    return node;
}

SyntaxNode *node_from_facade_sub(JSONValue *json, char const *name)
{
    OptionalJSONValue sub_maybe = json_get(json, name);
    if (sub_maybe.has_value) {
        return node_from_facade(&sub_maybe.value);
    }
    return NULL;
}

void node_chain_to_array(SyntaxNode *chain, JSONValue *json, char const *attr)
{
    JSONValue ret = json_array();
    for (SyntaxNode *n = chain; n; n = n->next) {
        json_append(&ret, node_facade(n));
    }
    json_set(json, attr, ret);
}

void array_to_node_chain(JSONValue *json, char const *attr, SyntaxNode **chain)
{
    JSONValue    array = MUST_OPTIONAL(JSONValue, json_get(json, attr));
    SyntaxNode **dst = chain;
    for (size_t ix = 0; ix < json_len(&array); ++ix) {
        JSONValue n = MUST_OPTIONAL(JSONValue, json_at(&array, ix));
        *dst = node_from_facade(&n);
        dst = &(*dst)->next;
    }
}

void ASSIGNMENT_to_json(SyntaxNode *node, JSONValue *json)
{
    json_set_cstr(json, "operator", Operator_name(node->assignment.operator));
    node_facade_sub(node->assignment.expression, json, "expression");
}

void ASSIGNMENT_from_json(SyntaxNode *node, JSONValue *json)
{
    node->assignment.operator= Operator_from_string(json_get_string(json, "operator", sv_null()));
    node->assignment.expression = node_from_facade_sub(json, "expression");
}

void BINARYEXPRESSION_to_json(SyntaxNode *node, JSONValue *json)
{
    node_facade_sub(node->binary_expr.lhs, json, "lhs");
    json_set_cstr(json, "operator", Operator_name(node->binary_expr.operator));
    node_facade_sub(node->binary_expr.rhs, json, "rhs");
}

void BINARYEXPRESSION_from_json(SyntaxNode *node, JSONValue *json)
{
    node->binary_expr.lhs = node_from_facade_sub(json, "lhs");
    node->assignment.operator= Operator_from_string(json_get_string(json, "operator", sv_null()));
    node->binary_expr.rhs = node_from_facade_sub(json, "rhs");
}

void BLOCK_to_json(SyntaxNode *node, JSONValue *json)
{
    node_chain_to_array(node->block.statements, json, "statements");
}

void BLOCK_from_json(SyntaxNode *node, JSONValue *json)
{
    array_to_node_chain(json, "statements", &node->block.statements);
}

void BOOL_to_json(SyntaxNode *node, JSONValue *json)
{
    json_set(json, "value", json_bool(sv_eq_ignore_case_cstr(node->name, "true")));
}

void BOOL_from_json(SyntaxNode *, JSONValue *)
{
}

void BREAK_to_json(SyntaxNode *, JSONValue *)
{
}

void BREAK_from_json(SyntaxNode *, JSONValue *)
{
}

void COMPOUND_to_json(SyntaxNode *node, JSONValue *json)
{
    node_chain_to_array(node->compound_expr.expressions, json, "expressions");
}

void COMPOUND_from_json(SyntaxNode *node, JSONValue *json)
{
    array_to_node_chain(json, "expressions", &node->compound_expr.expressions);
}

void CONTINUE_to_json(SyntaxNode *, JSONValue *)
{
}

void CONTINUE_from_json(SyntaxNode *, JSONValue *)
{
}

void DECIMAL_to_json(SyntaxNode *node, JSONValue *json)
{
}

void DECIMAL_from_json(SyntaxNode *node, JSONValue *json)
{
}

void ENUMERATION_to_json(SyntaxNode *node, JSONValue *json)
{
    node_facade_sub(node->enumeration.underlying_type, json, "underlying_type");
    node_chain_to_array(node->enumeration.values, json, "values");
}

void ENUMERATION_from_json(SyntaxNode *node, JSONValue *json)
{
    node->enumeration.underlying_type = node_from_facade_sub(json, "underlying_type");
    array_to_node_chain(json, "values", &node->enumeration.values);
}

void ENUM_VALUE_to_json(SyntaxNode *node, JSONValue *json)
{
    if (node->enum_value.underlying_value) {
        node_facade_sub(node->enum_value.underlying_value, json, "underlying_value");
    }
}

void ENUM_VALUE_from_json(SyntaxNode *node, JSONValue *json)
{
    OptionalJSONValue underlying_maybe = json_get(json, "underlying_value");
    if (underlying_maybe.has_value) {
        node->enum_value.underlying_value = node_from_facade(&underlying_maybe.value);
    }
}

void FOR_to_json(SyntaxNode *node, JSONValue *json)
{
    json_set_string(json, "variable", node->for_statement.variable);
    node_facade_sub(node->for_statement.range, json, "range");
    node_facade_sub(node->for_statement.statement, json, "statement");
}

void FOR_from_json(SyntaxNode *node, JSONValue *json)
{
    node->for_statement.variable = json_get_string(json, "variable", sv_null());
    node->for_statement.range = node_from_facade_sub(json, "range");
    node->for_statement.statement = node_from_facade_sub(json, "statement");
}

void FUNCTION_to_json(SyntaxNode *node, JSONValue *json)
{
    node_chain_to_array(node->function.parameter, json, "parameters");
    node_facade_sub(node->function.return_type, json, "return_type");
    node_facade_sub(node->function.error_type, json, "error_type");
    node_facade_sub(node->function.function_impl, json, "implementation");
}

void FUNCTION_from_json(SyntaxNode *node, JSONValue *json)
{
    array_to_node_chain(json, "parameters", &node->function.parameter);
    node->function.return_type = node_from_facade_sub(json, "return_type");
    node->function.error_type = node_from_facade_sub(json, "error_type");
    node->function.function_impl = node_from_facade_sub(json, "implementation");
}

void FUNCTION_CALL_to_json(SyntaxNode *node, JSONValue *json)
{
    node_facade_sub(node->call.function, json, "function");
    json_set(json, "discard_result", json_bool(node->call.discard_result));
    node_chain_to_array(node->call.arguments, json, "arguments");
}

void FUNCTION_CALL_from_json(SyntaxNode *node, JSONValue *json)
{
    node->call.function = node_from_facade_sub(json, "function");
    node->call.discard_result = json_get_bool(json, "discard_result", false);
    array_to_node_chain(json, "arguments", &node->call.arguments);
}

void FUNCTION_IMPL_to_json(SyntaxNode *node, JSONValue *json)
{
    node_chain_to_array(node->function_impl.statements, json, "statements");
}

void FUNCTION_IMPL_from_json(SyntaxNode *node, JSONValue *json)
{
    array_to_node_chain(json, "statements", &node->function_impl.statements);
}

void IF_to_json(SyntaxNode *node, JSONValue *json)
{
    node_facade_sub(node->if_statement.condition, json, "condition");
    node_chain_to_array(node->if_statement.if_true, json, "if_true");
    node_chain_to_array(node->if_statement.if_false, json, "if_false");
}

void IF_from_json(SyntaxNode *node, JSONValue *json)
{
    node->if_statement.condition = node_from_facade_sub(json, "condition");
    array_to_node_chain(json, "if_true", &node->if_statement.if_true);
    array_to_node_chain(json, "if_false", &node->if_statement.if_false);
}

void INTEGER_to_json(SyntaxNode *, JSONValue *)
{
}

void INTEGER_from_json(SyntaxNode *, JSONValue *)
{
}

void IMPORT_to_json(SyntaxNode *node, JSONValue *json)
{
    node_chain_to_array(node->import.modules, json, "modules");
}

void IMPORT_from_json(SyntaxNode *node, JSONValue *json)
{
    array_to_node_chain(json, "modules", &node->import.modules);
}

void LABEL_to_json(SyntaxNode *, JSONValue *)
{
}

void LABEL_from_json(SyntaxNode *, JSONValue *)
{
}

void LOOP_to_json(SyntaxNode *node, JSONValue *json)
{
    BLOCK_to_json(node, json);
}

void LOOP_from_json(SyntaxNode *node, JSONValue *json)
{
    BLOCK_from_json(node, json);
}

void MACRO_to_json(SyntaxNode *, JSONValue *)
{
}

void MACRO_from_json(SyntaxNode *, JSONValue *)
{
}

void MODULE_to_json(SyntaxNode *node, JSONValue *json)
{
    BLOCK_to_json(node, json);
}

void MODULE_from_json(SyntaxNode *node, JSONValue *json)
{
    BLOCK_from_json(node, json);
}

void NAME_to_json(SyntaxNode *, JSONValue *)
{
}

void NAME_from_json(SyntaxNode *, JSONValue *)
{
}

void NATIVE_FUNCTION_to_json(SyntaxNode *, JSONValue *)
{
}

void NATIVE_FUNCTION_from_json(SyntaxNode *, JSONValue *)
{
}

void PARAMETER_to_json(SyntaxNode *node, JSONValue *json)
{
    node_facade_sub(node->parameter.parameter_type, json, "parameter_type");
}

void PARAMETER_from_json(SyntaxNode *node, JSONValue *json)
{
    node->parameter.parameter_type = node_from_facade_sub(json, "parameter_type");
}

void PROGRAM_to_json(SyntaxNode *node, JSONValue *json)
{
    node_chain_to_array(node->program.imports, json, "imports");
    node_chain_to_array(node->program.modules, json, "modules");
}

void PROGRAM_from_json(SyntaxNode *node, JSONValue *json)
{
    array_to_node_chain(json, "imports", &node->program.imports);
    array_to_node_chain(json, "modules", &node->program.modules);
}

void RETURN_to_json(SyntaxNode *node, JSONValue *json)
{
    node_facade_sub(node->return_stmt.expression, json, "expression");
}

void RETURN_from_json(SyntaxNode *node, JSONValue *json)
{
    node->return_stmt.expression = node_from_facade_sub(json, "expression");
}

void STRING_to_json(SyntaxNode *, JSONValue *)
{
}

void STRING_from_json(SyntaxNode *, JSONValue *)
{
}

void STRUCT_to_json(SyntaxNode *node, JSONValue *json)
{
    node_chain_to_array(node->struct_def.components, json, "components");
}

void STRUCT_from_json(SyntaxNode *node, JSONValue *json)
{
    array_to_node_chain(json, "components", &node->struct_def.components);
}

void TYPE_to_json(SyntaxNode *node, JSONValue *json)
{
    json_set(json, "type_descr", type_descr_to_json(&node->type_descr));
}

void TYPE_from_json(SyntaxNode *node, JSONValue *json)
{
    node->type_descr = *type_descr_from_json(MUST_OPTIONAL(JSONValue, json_get(json, "type_descr")));
}

void TYPE_COMPONENT_to_json(SyntaxNode *node, JSONValue *json)
{
    PARAMETER_to_json(node, json);
}

void TYPE_COMPONENT_from_json(SyntaxNode *node, JSONValue *json)
{
    PARAMETER_from_json(node, json);
}

void UNARYEXPRESSION_to_json(SyntaxNode *node, JSONValue *json)
{
    json_set_cstr(json, "operator", Operator_name(node->unary_expr.operator));
    node_facade_sub(node->unary_expr.operand, json, "operand");
}

void UNARYEXPRESSION_from_json(SyntaxNode *node, JSONValue *json)
{
    node->unary_expr.operator= Operator_from_string(json_get_string(json, "operator", sv_null()));
    node->unary_expr.operand = node_from_facade_sub(json, "operand");
}

void VARIABLE_to_json(SyntaxNode *node, JSONValue *json)
{
    if (node->variable.subscript) {
        JSONValue sub = json_array();
        for (SyntaxNode *n = node->variable.subscript; n; n = n->next) {
            json_append(&sub, node_facade(n));
        }
        json_set(json, "subscript", sub);
    }
}

void VARIABLE_from_json(SyntaxNode *node, JSONValue *json)
{
    OptionalJSONValue sub_maybe = json_get(json, "subscript");
    if (sub_maybe.has_value) {
        JSONValue    array = sub_maybe.value;
        SyntaxNode **dst = &node->variable.subscript;
        for (size_t ix = 0; ix < json_len(&array); ++ix) {
            JSONValue n = MUST_OPTIONAL(JSONValue, json_at(&array, ix));
            *dst = node_from_facade(&n);
            dst = &(*dst)->next;
        }
    }
}

void VARIABLE_DECL_to_json(SyntaxNode *node, JSONValue *json)
{
    json_set(json, "is_const", json_bool(node->variable_decl.is_const));
    node_facade_sub(node->variable_decl.variable, json, "variable");
    if (node->variable_decl.var_type) {
        node_facade_sub(node->variable_decl.var_type, json, "variable_type");
    }
    if (node->variable_decl.init_expr) {
        node_facade_sub(node->variable_decl.init_expr, json, "expression");
    }
}

void VARIABLE_DECL_from_json(SyntaxNode *node, JSONValue *json)
{
    node->variable_decl.is_const = json_get_bool(json, "is_const", false);
    node->variable_decl.variable = node_from_facade_sub(json, "variable");
    if (json_has(json, "variable_type")) {
        node->variable_decl.var_type = node_from_facade_sub(json, "variable_type");
    }
}

void VARIANT_to_json(SyntaxNode *node, JSONValue *json)
{
    node_facade_sub(node->variant_def.underlying_type, json, "underlying_type");
    node_chain_to_array(node->variant_def.options, json, "values");
}

void VARIANT_from_json(SyntaxNode *node, JSONValue *json)
{
    node->variant_def.underlying_type = node_from_facade_sub(json, "underlying_type");
    array_to_node_chain(json, "values", &node->variant_def.options);
}

void VARIANT_OPTION_to_json(SyntaxNode *node, JSONValue *json)
{
    node_facade_sub(node->variant_option.underlying_value, json, "underlying_value");
    node_facade_sub(node->variant_option.payload_type, json, "payload");
}

void VARIANT_OPTION_from_json(SyntaxNode *node, JSONValue *json)
{
    node->variant_option.underlying_value = node_from_facade_sub(json, "underlying_value");
    node->variant_option.payload_type = node_from_facade_sub(json, "payload");
}

void WHILE_to_json(SyntaxNode *node, JSONValue *json)
{
    node_facade_sub(node->while_statement.condition, json, "condition");
    node_chain_to_array(node->while_statement.statement, json, "statement");
}

void WHILE_from_json(SyntaxNode *node, JSONValue *json)
{
    node->while_statement.condition = node_from_facade_sub(json, "condition");
    array_to_node_chain(json, "statement", &node->while_statement.statement);
}

JSONValue syntax_node_to_json(SyntaxNode *node)
{
    JSONValue json = json_object();
    json_set_string(&json, "name", node->name);
    json_set_cstr(&json, "type", SyntaxNodeType_name(node->type));
    json_set_int(&json, "index", node->index);
    json_set(&json, "token", token_to_json(node->token));
    switch (node->type) {
#undef SYNTAXNODETYPE
#define SYNTAXNODETYPE(type)         \
    case SNT_##type:                 \
        type##_to_json(node, &json); \
        break;
        SYNTAXNODETYPES(SYNTAXNODETYPE)
#undef SYNTAXNODETYPE
    default:
        UNREACHABLE();
    }
    return json;
}

SyntaxNode syntax_node_from_json(JSONValue json)
{
    SyntaxNode node = { 0 };
    node.type = SyntaxNodeType_from_string(json_get_string(&json, "type", sv_null()));
    node.name = json_get_string(&json, "name", sv_null());
    node.index = json_get_int(&json, "index", 0);
    node.token = token_from_json(MUST_OPTIONAL(JSONValue, json_get(&json, "token")));
    switch (node.type) {
#undef SYNTAXNODETYPE
#define SYNTAXNODETYPE(type)            \
    case SNT_##type:                    \
        type##_from_json(&node, &json); \
        break;
        SYNTAXNODETYPES(SYNTAXNODETYPE)
#undef SYNTAXNODETYPE
    default:
        UNREACHABLE();
    }
    return node;
}
