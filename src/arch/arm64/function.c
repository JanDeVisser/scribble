/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <arch/arm64/arm64.h>
#include <hash.h>

ARM64Function *arm64function_acreate(Allocator *allocator, IRFunction *function, size_t nsaa, size_t stack_depth)
{
    ARM64Function *ret = allocator_alloc_new(allocator, ARM64Function);
    ret->allocator = allocator;
    ret->function = function;
    if (stack_depth % 16)
        stack_depth += 16 - (stack_depth % 16);
    ret->scribble.stack_depth = stack_depth;
    if (nsaa % 16)
        nsaa += 16 - (nsaa % 16);
    ret->scribble.nsaa = nsaa;
    return ret;
}

StringView arm64var_decl_to_string(ARM64VarDecl *var, Allocator *allocator)
{
    return sv_aprintf(allocator, "%.*s %d => %.*s",
        SV_ARG(var->var_decl->name), var->where,
        SV_ARG(arm64variable_address_to_string(&var->address, allocator)));
}

StringView arm64function_label(ARM64Function *function)
{
    if (!function->num_parameters || sv_eq_cstr(function->function->name, "main")) {
        return function->function->name;
    }
    size_t hash = 0u;
    for (size_t ix = 0; ix < function->num_parameters; ++ix) {
        hash ^= hashlong(function->function->type.type_id);
    }
    return sv_aprintf(function->allocator, "%.*s_%zu", SV_ARG(function->function->name), hash % 4096);
}

StringView arm64function_to_string(ARM64Function *function)
{
    StringList params = sl_acreate(function->allocator);
    for (size_t ix = 0; ix < function->num_parameters; ++ix) {
        ARM64VarDecl *param = function->parameters + ix;
        sl_push(&params, ir_var_decl_to_string(param->var_decl, function->allocator));
    }
    return sv_aprintf(function->allocator, "func %.*s(%.*s): %.*s [%d/%d]",
        SV_ARG(function->function->name), SV_ARG(sl_join(&params, sv_from(", "))),
        SV_ARG(typespec_to_string(function->function->type, function->allocator)),
        function->scribble.nsaa, function->scribble.stack_depth);
}
