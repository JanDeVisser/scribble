/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <allocate.h>
#include <arm64.h>

DECLARE_SHARED_ALLOCATOR(arm64)

DA_IMPL(ARM64Variable)

StringView arm64variable_to_string(ARM64Variable *var)
{
    StringBuilder sb = sb_createf("%s %.*s : %.*s, %zu ",
        VariableKind_name(var->kind), SV_ARG(var->var_decl.name),
        SV_ARG(typeid_name(var->var_decl.type.type_id)), typeid_sizeof(var->var_decl.type.type_id));
    switch (var->kind) {
    case VK_PARAMETER:
        sb_printf(&sb, "@%ld, method %s: ", var->parameter.offset, ParameterPassingMethod_name(var->parameter.method));
        switch (var->parameter.method) {
        case PPM_REGISTER:
            sb_printf(&sb, "%s", reg(var->parameter.reg));
            break;
        case PPM_REGISTER_RANGE:
            sb_printf(&sb, "%s-%s", reg(var->parameter.range.start), reg(var->parameter.range.end - 1));
            break;
        case PPM_STACK:
            sb_printf(&sb, "[SP,%ld]", var->parameter.nsaa_offset);
            break;
        case PPM_POINTER:
            sb_printf(&sb, "[%s]", x_reg(var->parameter.reg));
            break;
        case PPM_POINTER_STACK:
            sb_printf(&sb, "[SP,%ld]", var->parameter.nsaa_offset);
            break;
        default:
            UNREACHABLE();
        }
        break;
    case VK_LOCAL:
        sb_printf(&sb, "@%ld", var->local_address.offset);
        break;
    case VK_GLOBAL:
    case VK_STATIC:
        sb_printf(&sb, "@%.*s", SV_ARG(var->static_address.label));
        break;
    default:
        UNREACHABLE();
    }
    return sb.view;
}

void arm64variable_store_variable(ARM64Variable *variable, ValueLocation from_location)
{
    type_id type = variable->var_decl.type.type_id;
    switch (variable->kind) {
    case VK_PARAMETER:
    case VK_LOCAL: {
        ValueLocation to_location = {
            .type = type,
            .kind = VLK_POINTER,
            .pointer = {
                .reg = REG_FP,
                .offset = variable->scope->function->scribble.stack_depth - variable->local_address.offset }
        };
        arm64function_copy(variable->scope->function, to_location, from_location);
    } break;
    case VK_STATIC:
    case VK_GLOBAL: {
        ValueLocation to_location = {
            .type = type,
            .kind = VLK_DATA,
            .symbol = variable->static_address.label
        };
        arm64function_copy(variable->scope->function, to_location, from_location);
    } break;
    case VK_AGGREGATE_COMPONENT: {
        NYI("VK_AGGREGATE_COMPONENT store");
    } break;
    case VK_ARRAY_ELEMENT: {
        NYI("VK_ARRAY_ELEMENT store");
    } break;
    default:
        UNREACHABLE();
    }
}

void arm64variable_load_variable(ARM64Variable *variable)
{
    type_id        type = variable->var_decl.type.type_id;
    ARM64Function *function = variable->scope->function;
    ValueLocation  from_location = { 0 };
    ValueLocation  to_location = arm64function_location_for_type(function, type);

    switch (variable->kind) {
    case VK_PARAMETER:
    case VK_LOCAL: {
        from_location = (ValueLocation) {
            .type = type,
            .kind = VLK_POINTER,
            .pointer = {
                .reg = REG_FP,
                .offset = variable->scope->function->scribble.stack_depth - variable->local_address.offset }
        };
    } break;
    case VK_STATIC:
    case VK_GLOBAL: {
        from_location = (ValueLocation) {
            .type = type,
            .kind = VLK_DATA,
            .symbol = variable->static_address.label
        };
    } break;
    case VK_AGGREGATE_COMPONENT: {
        NYI("VK_AGGREGATE_COMPONENT load");
    } break;
    case VK_ARRAY_ELEMENT: {
        NYI("VK_ARRAY_ELEMENT load");
    } break;
    default:
        UNREACHABLE();
    }
    arm64function_copy(variable->scope->function, to_location, from_location);
    arm64function_push_location(function, to_location);
}
