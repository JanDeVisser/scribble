/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <arm64.h>
#include <hash.h>

ARM64Function *arm64function_acreate(Allocator *allocator, IRFunction *function, int64_t nsaa, int64_t stack_depth)
{
    ARM64Function *ret = allocator_alloc_new(allocator, ARM64Function);
    ret->allocator = allocator;
    ret->function = function;
    if (stack_depth % 16)
        stack_depth += 16 - (stack_depth % 16);
    ret->scribble.stack_depth = stack_depth;
    if (nsaa % 16)
        nsaa += 16 - (nsaa % 16);
    ret->nsaa = nsaa;
    return ret;
}

StringView arm64variable_to_string(ARM64Variable *var, Allocator *allocator)
{
    StringBuilder sb = sb_acreate(allocator);
    sb_printf(&sb, "%s %.*s : %.*s, %zu ",
        VariableKind_name(var->kind), SV_ARG(var->var_decl.name),
        SV_ARG(typeid_name(var->var_decl.type.type_id)), typeid_sizeof(var->var_decl.type.type_id));
    switch (var->kind) {
    case VK_PARAMETER:
        sb_printf(&sb, "@%ld, method %s: ", var->parameter.offset, ParameterPassingMethod_name(var->parameter.method));
        switch (var->parameter.method) {
        case PPM_REGISTER:
            sb_printf(&sb, "%s", reg(var->parameter.reg));
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

StringView arm64function_label(ARM64Function *function)
{
    if (!function->num_parameters || sv_eq_cstr(function->function->name, "main")) {
        return function->function->name;
    }
    size_t hash = 0u;
    for (size_t ix = 0; ix < function->num_parameters; ++ix) {
        hash = (hash << 3) ^ hashlong(function->parameters[ix].var_decl.type.type_id);
    }
    return sv_aprintf(function->allocator, "%.*s_%zu", SV_ARG(function->function->name), hash % 4096);
}

StringView arm64function_to_string(ARM64Function *function)
{
    StringList params = sl_acreate(function->allocator);
    for (size_t ix = 0; ix < function->num_parameters; ++ix) {
        ARM64Variable *param = function->parameters + ix;
        sl_push(&params, ir_var_decl_to_string(&param->var_decl, function->allocator));
    }
    return sv_aprintf(function->allocator, "func %.*s(%.*s): %.*s [%ld/%ld]",
        SV_ARG(function->function->name), SV_ARG(sl_join(&params, sv_from(", "))),
        SV_ARG(typespec_to_string(function->function->type, function->allocator)),
        function->nsaa, function->scribble.stack_depth);
}

ARM64Variable *arm64function_variable_by_name(ARM64Function *function, StringView name)
{
    assert(function->function->kind == FK_SCRIBBLE);
    assert(function->scribble.current_scope);
    for (ARM64Scope *scope = function->scribble.current_scope; scope; scope = scope->up) {
        for (ARM64Variable *var = scope->variables; var; var = var->next) {
            if (sv_eq(var->var_decl.name, name)) {
                return var;
            }
        }
    }
    return NULL;
}
