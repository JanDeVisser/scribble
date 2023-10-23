/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <allocate.h>
#include <options.h>

#include <arm64.h>
#include <stddef.h>

DECLARE_SHARED_ALLOCATOR(arm64)

#if 0

void arm64context_define_static_storage(ARM64Context *ctx, StringView label, type_id type, bool global, long initial_value)
{
    switch (typeid_kind(type)) {
    case TK_PRIMITIVE: {
        StringView static_type = { 0 };
        switch (typeid_builtin_type(type)) {
        case BIT_POINTER:
        case BIT_I64:
        case BIT_U64:
            static_type = sv_from(".quad");
            break;
        case BIT_I32:
        case BIT_U32:
            static_type = sv_from(".int");
            break;
        case BIT_I16:
        case BIT_U16:
            static_type = sv_from(".short");
            break;
        case BIT_I8:
        case BIT_U8:
        case BIT_BOOL: {
            static_type = sv_from(".byte");
        } break;
        default:
            NYI("Defining static storage for builtin type '%s'", BuiltinType_name(typeid_builtin_type(type)));
        }
        assembly_add_data(
ctx->assembly, label, global, static_type, true, sv_printf("%ld", initial_value));
    } break;
    case TK_AGGREGATE:
    case TK_ARRAY: {
        size_t sz = typeid_sizeof(type);
assembly_add_data(ctx->assembly, label, global, sv_from(".space"), true, sv_printf("%zu", sz));
    } break;
    default:
        NYI("Defining static storage for type '%s'", typeid_name(type));
    }
}

void arm64context_load_immediate(ARM64Context *ctx, type_id type, uint64_t value, int target)
{
    if (!typeid_has_kind(type, TK_PRIMITIVE)) {
        fatal("Cannot load non-builtin types as immediates");
    }
    size_t      sz = typeid_sizeof(type);
    char const *width = (sz == 8) ? "x" : "w";
    size_t      words = (sz + 1) / 2;
    assembly_add_instruction(ctx->assembly, "mov", "%s%d,%szr", width, target, width);
    for (size_t ix = 0; value && ix < words; ix++) {
        uint16_t w = value & 0xFFFF;
        value >>= 16;
        assembly_add_instruction(ctx->assembly, "movk", "%s%d,#%d,lsl #%zu", width, target, w, ix * 16);
    }
}

#endif

ARM64Function *arm64context_function_by_name(ARM64Context *ctx, StringView name)
{
    for (size_t ix = 0; ix < ctx->assemblies.size; ++ix) {
        ARM64Function *function = assembly_function_by_name(ctx->assemblies.elements + ix, name);
        if (function) {
            return function;
        }
    }
    return NULL;
}
