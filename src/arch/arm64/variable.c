/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <allocate.h>
#include <arm64.h>

DECLARE_SHARED_ALLOCATOR(arm64)

void arm64variable_store_variable(ARM64Variable *variable, ARM64Context *ctx, ValueLocation from_location)
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
                .offset = ctx->function->scribble.stack_depth - variable->local_address.offset }
        };
        assembly_copy(ctx->assembly, to_location, from_location);
    } break;
    case VK_STATIC:
    case VK_GLOBAL: {
        ValueLocation to_location = {
            .type = type,
            .kind = VLK_SYMBOL,
            .symbol = variable->static_address.label
        };
        assembly_copy(ctx->assembly, to_location, from_location);
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

void arm64variable_load_variable(ARM64Variable *variable, ARM64Context *ctx, ValueLocation to_location)
{
    type_id type = variable->var_decl.type.type_id;
    switch (variable->kind) {
    case VK_PARAMETER:
    case VK_LOCAL: {
        ValueLocation from_location = {
            .type = type,
            .kind = VLK_POINTER,
            .pointer = {
                .reg = REG_FP,
                .offset = ctx->function->scribble.stack_depth - variable->local_address.offset }
        };
        assembly_copy(ctx->assembly, to_location, from_location);
    } break;
    case VK_STATIC:
    case VK_GLOBAL: {
        ValueLocation from_location = {
            .type = type,
            .kind = VLK_SYMBOL,
            .symbol = variable->static_address.label
        };
        assembly_copy(ctx->assembly, to_location, from_location);
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
}
