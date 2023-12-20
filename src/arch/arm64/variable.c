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

ValueLocation arm64variable_pointer(ARM64Variable *variable)
{
    type_id type = variable->var_decl.type.type_id;

    switch (variable->kind) {
    case VK_PARAMETER:
    case VK_LOCAL: {
        return (ValueLocation) {
            .type = type,
            .kind = VLK_POINTER,
            .pointer = {
                .reg = REG_FP,
                .offset = variable->scope->function->scribble.stack_depth - variable->local_address.offset,
            }
        };
    }
    case VK_STATIC:
    case VK_GLOBAL: {
        return (ValueLocation) {
            .type = type,
            .kind = VLK_DATA,
            .static_data = {
                .symbol = variable->static_address.label,
                .offset = variable->static_address.offset,
            }
        };
    }
    default:
        UNREACHABLE();
    }
}

ValueLocation arm64variable_reference(ARM64Variable *variable)
{
    ARM64Function *function = variable->scope->function;
    ValueLocation  to_location = { 0 };

    switch (variable->kind) {
    case VK_PARAMETER:
    case VK_LOCAL: {
        to_location = (ValueLocation) {
            .type = variable->var_decl.type.type_id,
            .kind = VLK_POINTER,
            .pointer = {
                .reg = REG_FP,
                .offset = variable->scope->function->scribble.stack_depth - variable->local_address.offset,
            }
        };
        break;
    }
    default:
        NYI("arm64variable_reference('%s')", VariableKind_name(variable->kind));
    }
    return to_location;
}

ARM64Variable arm64variable_component(ARM64Variable *variable, size_t index)
{
    ExpressionType *et = type_registry_get_type_by_id(variable->var_decl.type.type_id);
    int64_t         offset = 0;
    ARM64Variable   ret = { 0 };
    switch (type_kind(et)) {
    case TK_AGGREGATE: {
        assert(index < et->components.num_components);
        TypeComponent *tc = et->components.components + index;
        offset = (int64_t) typeid_offsetof(et->type_id, index);
        ret = (ARM64Variable) {
            .scope = variable->scope,
            .kind = variable->kind,
            .var_decl = {
                .name = tc->name,
                .type = {
                    .type_id = tc->type_id,
                    .optional = false,
                } },
        };
    } break;
    case TK_VARIANT: {
        assert(index < et->variant.size);
        TypeComponent *tc = et->variant.elements + index;
        offset = (int64_t) typeid_offsetof_payload(et->type_id);
        ret = (ARM64Variable) {
            .scope = variable->scope,
            .kind = variable->kind,
            .var_decl = {
                .name = tc->name,
                .type = {
                    .type_id = tc->type_id,
                    .optional = false,
                } },
        };
    } break;
    default:
        UNREACHABLE();
    }
    switch (variable->kind) {
    case VK_LOCAL:
        ret.local_address.offset = variable->local_address.offset - offset;
        break;
    case VK_PARAMETER:
        ret.parameter.offset = variable->local_address.offset - offset;
        break;
    case VK_STATIC:
    case VK_GLOBAL:
        ret.static_address.offset = variable->static_address.offset + offset;
        ret.static_address.label = variable->static_address.label;
        break;
    default:
        UNREACHABLE();
    }
    return ret;
}
