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

void ir_function_add_push_u64(IRFunction *fnc, uint64_t value)
{
    IROperation op;
    ir_operation_set(&op, IR_PUSH_INT_CONSTANT);
    op.integer = integer_create(U64, value);
    ir_function_add_operation(fnc, op);
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
            sb_printf(&sb, "[%zu]", op->var_component.elements[ix]);
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
