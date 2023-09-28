/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <allocate.h>
#include <arch/arm64/arm64.h>

DECLARE_SHARED_ALLOCATOR(arm64)

void arm64variable_address_store_variable(ARM64VariableAddress *address, type_id type, ARM64Context *ctx, int from)
{
    switch (address->type) {
    case VAT_STACK: {
        arm64context_store_variable(ctx, type, address->stack_address.offset, from);
    } break;
    case VAT_STATIC:
    case VAT_GLOBAL: {
        if (!typeid_has_kind(type, TK_AGGREGATE)) {
            MUST_OPTIONAL(OpcodeMap, opcode_map, get_opcode_map(type))
            assembly_add_instruction(ctx->assembly, "adrp", "x8,%.*s@PAGE", SV_ARG(address->static_address.label));
            assembly_add_instruction(ctx->assembly, opcode_map.store_opcode, "%s%d,[x8,%.*s@PAGEOFF]", opcode_map.reg_width, from, SV_ARG(address->static_address.label));
            return;
        }
        if (!typeid_is_concrete(type)) {
            fatal("Cannot access template composites");
        }
        assembly_add_comment(ctx->assembly, "Storing static struct variable");
        assembly_add_instruction(ctx->assembly, "adrp", "x8,%.*s@PAGE", SV_ARG(address->static_address.label));
        ExpressionType *et = type_registry_get_type_by_id(type);
        for (size_t ix = 0; ix < et->components.num_components; ++ix) {
            ExpressionType *component_type = type_registry_get_type_by_id(et->components.components[ix].type_id);
            MUST(Size, size_t, sz, type_sizeof(component_type))
            MUST(Size, size_t, offset, type_offsetof_index(et, ix))
            char const *reg_width = "w";
            if (sz > 4) {
                reg_width = "x";
            }
            assembly_add_instruction(ctx->assembly, "str", "%s%d,[x8,%.*s@PAGEOFF+%zu]", reg_width, from++, SV_ARG(address->static_address.label), offset);
        }
    } break;
    case VAT_AGGREGATE_COMPONENT: {
        MUST_OPTIONAL(OpcodeMap, opcode_map, get_opcode_map(type))
        arm64variable_address_prepare_pointer(address->aggregate_component.aggregate, ctx);
        if (address->aggregate_component.offset > 0) {
            assembly_add_instruction(ctx->assembly, "add", "x8,x8,#{}", address->aggregate_component.offset);
        }
        assembly_add_instruction(ctx->assembly, opcode_map.store_opcode, "%s%d,[x8]", opcode_map.reg_width, from);
    } break;
    case VAT_ARRAY_ELEMENT: {
        MUST_OPTIONAL(OpcodeMap, opcode_map, get_opcode_map(type))
        arm64variable_address_prepare_pointer(address->array_component.array, ctx);
        arm64variable_address_prepare_pointer(address, ctx);
        assembly_pop(ctx->assembly, "x0");
        assembly_add_instruction(ctx->assembly, opcode_map.store_opcode, "%s%d,[x8]", opcode_map.reg_width, from);
    } break;
    default:
        UNREACHABLE();
    }
}

void arm64variable_address_load_variable(ARM64VariableAddress *address, type_id type, ARM64Context *ctx, int target)
{
    switch (address->type) {
    case VAT_STACK: {
        arm64context_load_variable(ctx, type, address->stack_address.offset, target);
    } break;
    case VAT_STATIC:
    case VAT_GLOBAL: {
        if (!typeid_has_kind(type, TK_AGGREGATE)) {
            MUST_OPTIONAL(OpcodeMap, opcode_map, get_opcode_map(type))
            assembly_add_instruction(ctx->assembly, "adrp", "x8,%.*s@PAGE", SV_ARG(address->static_address.label));
            assembly_add_instruction(ctx->assembly, opcode_map.load_opcode, "%s%d,[x8,%.*s@PAGEOFF]", opcode_map.reg_width, target, SV_ARG(address->static_address.label));
            return;
        }
        if (!typeid_is_concrete(type)) {
            fatal("Cannot access template aggregates");
        }
        assembly_add_comment(ctx->assembly, "Loading static struct variable");
        assembly_add_instruction(ctx->assembly, "adrp", "x8,%.*s@PAGE", SV_ARG(address->static_address.label));
        ExpressionType *et = type_registry_get_type_by_id(type);
        for (size_t ix = 0; ix < et->components.num_components; ++ix) {
            ExpressionType *component_type = type_registry_get_type_by_id(et->components.components[ix].type_id);
            char const     *reg_width = "w";
            MUST(Size, size_t, sz, type_sizeof(component_type))
            MUST(Size, size_t, offset, type_offsetof_index(et, ix))
            if (sz > 4) {
                reg_width = "x";
            }
            assembly_add_instruction(ctx->assembly, "ldr", "%s%d,[x8,%.*s@PAGEOFF+%zu]", reg_width, target++, SV_ARG(address->static_address.label), offset);
        }
    } break;
    case VAT_AGGREGATE_COMPONENT: {
        MUST_OPTIONAL(OpcodeMap, opcode_map, get_opcode_map(type))
        arm64variable_address_prepare_pointer(address->aggregate_component.aggregate, ctx);
        if (address->aggregate_component.offset > 0) {
            assembly_add_instruction(ctx->assembly, "add", "x8,x8,#{}", address->aggregate_component.offset);
        }
        assembly_add_instruction(ctx->assembly, opcode_map.load_opcode, "%s%d,[x8]", opcode_map.reg_width, target);
    } break;
    case VAT_ARRAY_ELEMENT: {
        MUST_OPTIONAL(OpcodeMap, opcode_map, get_opcode_map(type))
        arm64variable_address_prepare_pointer(address->array_component.array, ctx);
        arm64variable_address_prepare_pointer(address, ctx);
        assembly_pop(ctx->assembly, "x0");
        assembly_add_instruction(ctx->assembly, opcode_map.load_opcode, "%s%d,[x8]", opcode_map.reg_width, target);
    } break;
    default:
        UNREACHABLE();
    }
}

void arm64variable_address_prepare_pointer(ARM64VariableAddress *address, ARM64Context *ctx)
{
    switch (address->type) {
    case VAT_STACK: {
        assembly_add_instruction(ctx->assembly, "add", "x8,fp,#{}", arm64context_get_stack_depth(ctx) - address->stack_address.offset);
    } break;
    case VAT_STATIC:
    case VAT_GLOBAL: {
        assembly_add_instruction(ctx->assembly, "adrp", "x8,%.*s@PAGE", SV_ARG(address->static_address.label));
        assembly_add_instruction(ctx->assembly, "add", "x8,x8,%.*s@PAGEOFF", SV_ARG(address->static_address.label));
    } break;
    case VAT_AGGREGATE_COMPONENT: {
        assembly_add_instruction(ctx->assembly, "add", "x8,x8,#{}", arm64context_get_stack_depth(ctx) - address->aggregate_component.offset);
    } break;
    case VAT_ARRAY_ELEMENT: {
        // x0 will hold the array index. Here we add that index, multiplied by
        // the element size, to x8, which should hold the array base address

        switch (address->array_component.element_size) {
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
            fatal("Cannot access arrays with elements of size %d yet", address->array_component.element_size);
        }
    } break;
    default:
        UNREACHABLE();
    }
}

StringView arm64variable_address_to_string(ARM64VariableAddress *address, Allocator *allocator)
{
    switch (address->type) {
    case VAT_STACK:
        return sv_aprintf((allocator) ? allocator : get_allocator(), "StackVariableAddress: [%zu]", address->stack_address.offset);
    case VAT_STATIC:
        return sv_aprintf((allocator) ? allocator : get_allocator(), "StaticVariableAddress: [.%.*s]", SV_ARG(address->static_address.label));
    case VAT_GLOBAL:
        return sv_aprintf((allocator) ? allocator : get_allocator(), "GlobalVariableAddress: [.%.*s]", SV_ARG(address->static_address.label));
    case VAT_AGGREGATE_COMPONENT:
        return sv_aprintf((allocator) ? allocator : get_allocator(), "AggregateComponentAddress: [%zu]", address->aggregate_component.offset);
    case VAT_ARRAY_ELEMENT:
        return sv_aprintf((allocator) ? allocator : get_allocator(), "ArrayElementAddress: [%zu]", address->array_component.element_size);
    default:
        UNREACHABLE();
    }
}
