/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <inttypes.h>

#define STATIC_ALLOCATOR
#include <allocate.h>
#include <ir.h>

char const *ir_operation_type_name(IROperationType optype)
{
    switch (optype) {
#undef IR_OPERATION_TYPE
#define IR_OPERATION_TYPE(t) \
    case IR_##t:             \
        return #t;
        IR_OPERATION_TYPES(IR_OPERATION_TYPE)
#undef IR_OPERATION_TYPE
    default:
        UNREACHABLE();
    }
}

void ir_operation_set(IROperation *op, IROperationType operation)
{
    memset(op, 0, sizeof(IROperation));
    op->operation = operation;
}

void ir_function_add_operation(IRFunction *fnc, IROperation op)
{
    assert(fnc->kind == FK_SCRIBBLE);
    op.index = fnc->operations.size + 1;
    da_append_IROperation(&fnc->operations, op);
}

StringView ir_var_decl_to_string(IRVarDecl *var)
{
    StringBuilder sb = sb_create();
    sb_printf(&sb, SV_SPEC ": ", SV_ARG(var->name));
    sb_append_sv(&sb, typespec_to_string(var->type));
    return sb.view;
}

void ir_var_decl_print(IRVarDecl *var)
{
    AllocatorState as = save_allocator();
    StringView     s = ir_var_decl_to_string(var);
    printf(SV_SPEC, SV_ARG(s));
    release_allocator(as);
}

JSONValue ir_var_decl_to_json(IRVarDecl var_decl)
{
    JSONValue ret = json_object();
    json_set(&ret, "name", json_string(var_decl.name));
    json_set(&ret, "type", json_string(typeid_name(var_decl.type.type_id)));
    return ret;
}

JSONValue ir_operation_to_json(IROperation op)
{
    JSONValue ret = json_object();
    json_set(&ret, "index", json_int(op.index));
    json_set(&ret, "type", json_string(sv_from(ir_operation_type_name(op.operation))));
    switch (op.operation) {
    case IR_CALL: {
        JSONValue call = json_object();
        json_set(&call, "name", json_string(op.call.name));
        json_set(&call, "discard", json_bool(op.call.discard_result));
        json_set(&ret, "call", call);
    } break;
    case IR_CASE:
    case IR_END_CASE:
    case IR_JUMP:
    case IR_JUMP_F:
    case IR_JUMP_T:
    case IR_LABEL:
        json_set(&ret, "label", json_int(op.label));
        break;
    case IR_CAST:
    case IR_MATCH:
        json_set(&ret, "type", json_string(typeid_name(op.type)));
        break;
    case IR_END_MATCH:
        break;
    case IR_ASSERT:
    case IR_PUSH_VAR_ADDRESS:
    case IR_PUSH_STRING_CONSTANT:
        json_set(&ret, "string", json_string(op.sv));
        break;
    case IR_DECL_VAR:
        json_set(&ret, "var_decl", ir_var_decl_to_json(op.var_decl));
        break;
    case IR_PUSH_BOOL_CONSTANT:
    case IR_RETURN:
        json_set(&ret, "bool", json_bool(op.bool_value));
        break;
    case IR_PUSH_FLOAT_CONSTANT:
        json_set(&ret, "double", json_number(op.double_value));
        break;
    case IR_PUSH_INT_CONSTANT: {
        JSONValue integer = json_object();
        json_set(&integer, "value", json_string(sv_render_integer(op.integer)));
        json_set(&integer, "type", json_string(sv_from(IntegerType_name(op.integer.type))));
        json_set(&integer, "integer", integer);
        json_set(&ret, "integer", integer);
    } break;
    case IR_SUBSCRIPT: {
        JSONValue subscript = json_object();
        json_set(&subscript, "name", json_string(op.sv));
        JSONValue components = json_array();
        for (size_t ix = 0; ix < op.var_component.size; ++ix) {
            json_append(&components, json_int(op.var_component.elements[ix]));
        }
        json_set(&subscript, "components", components);
        json_set(&ret, "subscript", subscript);
    } break;
    case IR_BINARY_OPERATOR: {
        JSONValue bin_op = json_object();
        json_set(&bin_op, "operator", json_string(sv_from(Operator_name(op.binary_operator.op))));
        json_set(&bin_op, "lhs", json_string(typeid_name(op.binary_operator.lhs)));
        json_set(&bin_op, "rhs", json_string(typeid_name(op.binary_operator.rhs)));
        json_set(&ret, "binary_op", bin_op);
    } break;
    case IR_UNARY_OPERATOR: {
        JSONValue unary_op = json_object();
        json_set(&unary_op, "operator", json_string(sv_from(Operator_name(op.unary_operator.op))));
        json_set(&unary_op, "operand", json_string(typeid_name(op.unary_operator.operand)));
        json_set(&ret, "unary_op", unary_op);
    } break;
    default:
        UNREACHABLE();
    }
    return ret;
}

static StringView _ir_operation_to_string(IROperation *op, char const *prefix)
{
    StringBuilder sb = sb_create();
    sb_printf(&sb, "%1.1s %4zu %-20.20s  ", prefix, op->index, ir_operation_type_name(op->operation));
    switch (op->operation) {
    case IR_CALL:
        sb_printf(&sb, SV_SPEC, SV_ARG(op->call.name));
        break;
    case IR_CASE:
    case IR_END_CASE:
    case IR_JUMP:
    case IR_JUMP_F:
    case IR_JUMP_T:
    case IR_LABEL:
        sb_printf(&sb, "lbl_%zu", op->label);
        break;
    case IR_CAST:
    case IR_MATCH:
        sb_printf(&sb, SV_SPEC, SV_ARG(typeid_name(op->type)));
        break;
    case IR_END_MATCH:
        break;
    case IR_ASSERT:
    case IR_PUSH_VAR_ADDRESS:
    case IR_PUSH_STRING_CONSTANT:
        sb_printf(&sb, SV_SPEC, SV_ARG(op->sv));
        break;
    case IR_DECL_VAR:
        sb_printf(&sb, "%.*s", SV_ARG(ir_var_decl_to_string(&op->var_decl)));
        break;
    case IR_PUSH_BOOL_CONSTANT:
        sb_printf(&sb, "%s", (op->bool_value) ? "true" : "false");
        break;
    case IR_PUSH_FLOAT_CONSTANT:
        sb_printf(&sb, "%f", op->double_value);
        break;
    case IR_PUSH_INT_CONSTANT:
        sb_append_sv(&sb, sv_render_integer(op->integer));
        if (integer_is_unsigned(op->integer)) {
            sb_printf(&sb, " [%.*s]", SV_ARG(sv_render_hex_integer(op->integer)));
        }
        sb_printf(&sb, " : %s", IntegerType_name(op->integer.type));
        break;
    case IR_SUBSCRIPT:
        sb_printf(&sb, SV_SPEC, SV_ARG(op->sv));
        for (size_t ix = 0; ix < op->var_component.size; ++ix) {
            sb_append_cstr(&sb, ".");
            size_t subscript = op->var_component.elements[ix];
            if (subscript == (size_t) -1) {
                sb_append_cstr(&sb, "[#]");
            } else {
                sb_printf(&sb, "[%zu]", subscript);
            }
        }
        break;
    case IR_NEW_DATUM:
        sb_printf(&sb, SV_SPEC " [0x%08" PRIx64 "]", SV_ARG(typeid_name(op->integer.u64)), op->integer.u64);
        break;
    case IR_BINARY_OPERATOR:
        sb_printf(&sb, "%s(%.*s, %.*s)", Operator_name(op->binary_operator.op), SV_ARG(typeid_name(op->binary_operator.lhs)), SV_ARG(typeid_name(op->binary_operator.rhs)));
        break;
    case IR_UNARY_OPERATOR:
        sb_printf(&sb, "%s(%.*s)", Operator_name(op->unary_operator.op), SV_ARG(typeid_name(op->unary_operator.operand)));
        break;
    case IR_RETURN:
        sb_printf(&sb, "%s", (op->bool_value) ? "true" : "false");
        break;
    default:
        break;
    }
    return sb.view;
}

StringView ir_operation_to_string(IROperation *op)
{
    return _ir_operation_to_string(op, "");
}

void ir_operation_print_prefix(IROperation *op, char const *prefix)
{
    StringView s = _ir_operation_to_string(op, prefix);
    printf(SV_SPEC "\n", SV_ARG(s));
}

void ir_operation_print(IROperation *op)
{
    StringView s = _ir_operation_to_string(op, " ");
    printf(SV_SPEC "\n", SV_ARG(s));
}

ErrorOrSize ir_function_resolve_label(IRFunction *function, size_t label)
{
    assert(function && function->kind == FK_SCRIBBLE);
    for (size_t ix = 0; ix < function->operations.size; ++ix) {
        if (function->operations.elements[ix].operation == IR_LABEL && function->operations.elements[ix].label == label) {
            RETURN(Size, ix);
        }
    }
    ERROR(Size, RuntimeError, 0, "Label '%d' not found in function '" SV_SPEC "'", label, SV_ARG(function->name));
}

StringView ir_function_to_string(IRFunction *function)
{
    StringBuilder sb = sb_create();
    sb_printf(&sb, SV_SPEC "(", SV_ARG(function->name));
    StringList params = sl_create();
    for (size_t ix = 0; ix < function->num_parameters; ++ix) {
        IRVarDecl *param = function->parameters + ix;
        sl_push(&params, ir_var_decl_to_string(param));
    }
    sb_append_sv(&sb, sl_join(&params, sv_from(", ")));
    sb_append_cstr(&sb, ") -> ");
    sb_append_sv(&sb, typespec_to_string(function->type));
    return sb.view;
}

void ir_function_print(IRFunction *function)
{
    AllocatorState as = save_allocator();
    StringView     s = ir_function_to_string(function);
    printf(SV_SPEC, SV_ARG(s));
    release_allocator(as);
}

JSONValue ir_function_to_json(IRFunction *fnc)
{
    JSONValue ret;
    if (fnc->module) {
        json_set_string(&ret, "module", fnc->module->name);
    }
    json_set_cstr(&ret, "kind", IRFunctionKind_name(fnc->kind));
    json_set_string(&ret, "name", fnc->name);
    json_set_string(&ret, "type", typeid_name(fnc->type.type_id));
    JSONValue params = json_array();
    for (size_t ix = 0; ix < fnc->num_parameters; ++ix) {
        json_append(&params, ir_var_decl_to_json(fnc->parameters[ix]));
    }
    json_set(&ret, "parameters", params);
    return ret;
}

void ir_function_list(IRFunction *function, size_t mark)
{
    printf(SV_SPEC "(", SV_ARG(function->name));
    for (size_t ix = 0; ix < function->num_parameters; ++ix) {
        if (ix > 0)
            printf(", ");
        ir_var_decl_print(function->parameters + ix);
    }
    printf(") -> ");
    typespec_print(stdout, function->type);
    printf("\n");
    printf("--------------------------------------------\n");
    switch (function->kind) {
    case FK_SCRIBBLE: {
        for (size_t ix = 0; ix < function->operations.size; ++ix) {
            if (function->operations.elements[ix].operation == IR_LABEL) {
                printf("lbl_%zu:\n", function->operations.elements[ix].label);
            }
            if (ix == mark) {
                ir_operation_print_prefix(function->operations.elements + ix, ">");
            } else {
                ir_operation_print(function->operations.elements + ix);
            }
        }
    } break;
    case FK_NATIVE: {
        printf("  Native Function => %.*s\n", SV_ARG(function->native_name));
    } break;
    default:
        UNREACHABLE();
    }
    printf("\n");
}

void ir_module_list(IRModule *module, bool header)
{
    if (header) {
        printf("Module " SV_SPEC "\n", SV_ARG(module->name));
        printf("============================================\n\n");
    }
    for (size_t fix = 0; fix < module->functions.size; ++fix) {
        if (module->functions.elements[fix].kind == FK_SCRIBBLE) {
            ir_function_list(module->functions.elements + fix, (size_t) -1);
        }
    }
}

JSONValue ir_module_to_json(IRModule module)
{
    JSONValue ret;
    json_set_string(&ret, "name", module.name);
    JSONValue functions = json_array();
    for (size_t ix = 0; ix < module.functions.size; ++ix) {
        json_append(&functions, ir_function_to_json(module.functions.elements + ix));
    }
    json_set(&ret, "functions", functions);
    return ret;
}

IRFunction *ir_module_function_by_name(IRModule *module, StringView name)
{
    for (size_t fix = 0; fix < module->functions.size; ++fix) {
        if (sv_eq(module->functions.elements[fix].name, name)) {
            return module->functions.elements + fix;
        }
    }
    return NULL;
}

void ir_program_list(IRProgram program)
{
    printf("Program " SV_SPEC "\n", SV_ARG(program.name));
    printf("============================================\n\n");
    for (size_t ix = 0; ix < program.modules.size; ++ix) {
        IRModule *module = program.modules.elements + ix;
        ir_module_list(module, program.modules.size > 1);
    }
}

JSONValue ir_program_to_json(IRProgram program)
{
    JSONValue ret;
    json_set_string(&ret, "name", program.name);
    JSONValue modules = json_array();
    for (size_t ix = 0; ix < program.modules.size; ++ix) {
        json_append(&modules, ir_module_to_json(program.modules.elements[ix]));
    }
    json_set(&ret, "modules", modules);
    return ret;
}

IRFunction *ir_program_function_by_name(IRProgram *program, StringView name)
{
    for (size_t ix = 0; ix < program->modules.size; ++ix) {
        IRModule   *module = program->modules.elements + ix;
        IRFunction *fnc = ir_module_function_by_name(module, name);
        if (fnc) {
            return fnc;
        }
    }
    return NULL;
}
