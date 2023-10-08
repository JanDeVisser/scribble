/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <allocate.h>
#include <arch/arm64/arm64.h>

DECLARE_SHARED_ALLOCATOR(arm64)

void arm64variable_store_variable(ARM64Variable *variable, ARM64Context *ctx, int from)
{
    type_id type = variable->var_decl.type.type_id;
    switch (variable->kind) {
    case VK_PARAMETER:
    case VK_LOCAL: {
        arm64context_store_variable(ctx, type, variable->local_address.offset, from);
    } break;
    case VK_STATIC:
    case VK_GLOBAL: {
        if (!typeid_has_kind(type, TK_AGGREGATE)) {
            MUST_OPTIONAL(OpcodeMap, opcode_map, get_opcode_map(type))
            assembly_add_instruction(ctx->assembly, "adrp", "x8,%.*s@PAGE", SV_ARG(variable->static_address.label));
            assembly_add_instruction(ctx->assembly, opcode_map.store_opcode, "%s%d,[x8,%.*s@PAGEOFF]", opcode_map.reg_width, from, SV_ARG(variable->static_address.label));
            return;
        }
        if (!typeid_is_concrete(type)) {
            fatal("Cannot access template composites");
        }
        assembly_add_comment(ctx->assembly, "Storing static struct variable");
        assembly_add_instruction(ctx->assembly, "adrp", "x8,%.*s@PAGE", SV_ARG(variable->static_address.label));
        ExpressionType *et = type_registry_get_type_by_id(type);
        for (size_t ix = 0; ix < et->components.num_components; ++ix) {
            ExpressionType *component_type = type_registry_get_type_by_id(et->components.components[ix].type_id);
            MUST(Size, size_t, sz, type_sizeof(component_type))
            MUST(Size, size_t, offset, type_offsetof_index(et, ix))
            char const *reg_width = "w";
            if (sz > 4) {
                reg_width = "x";
            }
            assembly_add_instruction(ctx->assembly, "str", "%s%d,[x8,%.*s@PAGEOFF+%zu]", reg_width, from++, SV_ARG(variable->static_address.label), offset);
        }
    } break;
    case VK_AGGREGATE_COMPONENT: {
        MUST_OPTIONAL(OpcodeMap, opcode_map, get_opcode_map(type))
        arm64variable_prepare_pointer(variable->aggregate_component.aggregate, ctx);
        if (variable->aggregate_component.offset > 0) {
            assembly_add_instruction(ctx->assembly, "add", "x8,x8,#{}", variable->aggregate_component.offset);
        }
        assembly_add_instruction(ctx->assembly, opcode_map.store_opcode, "%s%d,[x8]", opcode_map.reg_width, from);
    } break;
    case VK_ARRAY_ELEMENT: {
        MUST_OPTIONAL(OpcodeMap, opcode_map, get_opcode_map(type))
        arm64variable_prepare_pointer(variable->array_component.array, ctx);
        arm64variable_prepare_pointer(variable, ctx);
        assembly_pop(ctx->assembly, "x0");
        assembly_add_instruction(ctx->assembly, opcode_map.store_opcode, "%s%d,[x8]", opcode_map.reg_width, from);
    } break;
    default:
        UNREACHABLE();
    }
}

void arm64variable_load_variable(ARM64Variable *variable, ARM64Context *ctx, int target)
{
    type_id type = variable->var_decl.type.type_id;
    switch (variable->kind) {
    case VK_PARAMETER:
    case VK_LOCAL: {
        arm64context_load_variable(ctx, type, variable->local_address.offset, target);
    } break;
    case VK_STATIC:
    case VK_GLOBAL: {
        if (!typeid_has_kind(type, TK_AGGREGATE)) {
            MUST_OPTIONAL(OpcodeMap, opcode_map, get_opcode_map(type))
            assembly_add_instruction(ctx->assembly, "adrp", "x8,%.*s@PAGE", SV_ARG(variable->static_address.label));
            assembly_add_instruction(ctx->assembly, opcode_map.load_opcode, "%s%d,[x8,%.*s@PAGEOFF]", opcode_map.reg_width, target, SV_ARG(variable->static_address.label));
            return;
        }
        if (!typeid_is_concrete(type)) {
            fatal("Cannot access template aggregates");
        }
        assembly_add_comment(ctx->assembly, "Loading static struct variable");
        assembly_add_instruction(ctx->assembly, "adrp", "x8,%.*s@PAGE", SV_ARG(variable->static_address.label));
        ExpressionType *et = type_registry_get_type_by_id(type);
        for (size_t ix = 0; ix < et->components.num_components; ++ix) {
            ExpressionType *component_type = type_registry_get_type_by_id(et->components.components[ix].type_id);
            char const     *reg_width = "w";
            MUST(Size, size_t, sz, type_sizeof(component_type))
            MUST(Size, size_t, offset, type_offsetof_index(et, ix))
            if (sz > 4) {
                reg_width = "x";
            }
            assembly_add_instruction(ctx->assembly, "ldr", "%s%d,[x8,%.*s@PAGEOFF+%zu]", reg_width, target++, SV_ARG(variable->static_address.label), offset);
        }
    } break;
    case VK_AGGREGATE_COMPONENT: {
        MUST_OPTIONAL(OpcodeMap, opcode_map, get_opcode_map(type))
        arm64variable_prepare_pointer(variable->aggregate_component.aggregate, ctx);
        if (variable->aggregate_component.offset > 0) {
            assembly_add_instruction(ctx->assembly, "add", "x8,x8,#{}", variable->aggregate_component.offset);
        }
        assembly_add_instruction(ctx->assembly, opcode_map.load_opcode, "%s%d,[x8]", opcode_map.reg_width, target);
    } break;
    case VK_ARRAY_ELEMENT: {
        MUST_OPTIONAL(OpcodeMap, opcode_map, get_opcode_map(type))
        arm64variable_prepare_pointer(variable->array_component.array, ctx);
        arm64variable_prepare_pointer(variable, ctx);
        assembly_pop(ctx->assembly, "x0");
        assembly_add_instruction(ctx->assembly, opcode_map.load_opcode, "%s%d,[x8]", opcode_map.reg_width, target);
    } break;
    default:
        fatal("variable kind: %d", variable->kind);
    }
}

void arm64variable_prepare_pointer(ARM64Variable *variable, ARM64Context *ctx)
{
    switch (variable->kind) {
    case VK_LOCAL: {
        assembly_add_instruction(ctx->assembly, "add", "x8,fp,#{}", arm64context_get_stack_depth(ctx) - variable->local_address.offset);
    } break;
    case VK_STATIC:
    case VK_GLOBAL: {
        assembly_add_instruction(ctx->assembly, "adrp", "x8,%.*s@PAGE", SV_ARG(variable->static_address.label));
        assembly_add_instruction(ctx->assembly, "add", "x8,x8,%.*s@PAGEOFF", SV_ARG(variable->static_address.label));
    } break;
    case VK_AGGREGATE_COMPONENT: {
        assembly_add_instruction(ctx->assembly, "add", "x8,x8,#{}", arm64context_get_stack_depth(ctx) - variable->aggregate_component.offset);
    } break;
    case VK_ARRAY_ELEMENT: {
        // x0 will hold the array index. Here we add that index, multiplied by
        // the element size, to x8, which should hold the array base address

        switch (variable->array_component.element_size) {
        case 1:
            assembly_add_instruction(ctx->assembly, "add", "x8,x8,x0");
            break;
        case 2:
            assembly_add_instruction(ctx->assembly, "add", "x8,x8,x0,lsl #1");
            break;
        case 4:
            assembly_add_instruction(ctx->assembly, "add", "x8,x8,x0,lsl #2");
            break;
        case 8:
            assembly_add_instruction(ctx->assembly, "add", "x8,x8,x0,lsl #3");
            break;
        case 16:
            assembly_add_instruction(ctx->assembly, "add", "x8,x8,x0,lsl #4");
            break;
        default:
            fatal("Cannot access arrays with elements of size %d yet", variable->array_component.element_size);
        }
    } break;
    default:
        UNREACHABLE();
    }
}
