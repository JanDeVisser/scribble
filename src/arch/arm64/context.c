/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <errno.h>

#include <allocate.h>
#include <error_or.h>
#include <options.h>

#include <arch/arm64/arm64.h>

DECLARE_SHARED_ALLOCATOR(arm64)

ARM64Context *arm64context_acreate(Allocator *allocator)
{
    ARM64Context *ctx = allocate_new(ARM64Context);
    ctx->allocator = allocator;
    arm64context_add_module(ctx, ROOT_MODULE_NAME);
    return ctx;
}

void arm64context_set_stack_depth(ARM64Context *ctx, size_t depth)
{
    IntList *sd = allocator_alloc_new(ctx->allocator, IntList);
    sd->value = depth;
    sd->next = ctx->stack_depths;
    ctx->stack_depths = sd;
    assembly_add_comment(ctx->assembly, "Set stack depth to %zu", depth);
}

void arm64context_pop_stack_depth(ARM64Context *ctx)
{
    assert(ctx->stack_depths);
    IntList *current = ctx->stack_depths;
    ctx->stack_depths = current->next;
    if (!ctx->stack_depths) {
        assembly_add_comment(ctx->assembly, "Stack depth popped. Was %zu, now empty", current->value);
    } else {
        assembly_add_comment(
            ctx->assembly, "Stack depth popped. Was %zu, now %zu", current->value, ctx->stack_depths->value);
    }
}

size_t arm64context_get_stack_depth(ARM64Context *ctx)
{
    if (ctx->stack_depths) {
        return ctx->stack_depths->value;
    }
    fatal("Stack depth empty!");
}

void arm64context_reserve_on_stack(ARM64Context *ctx, size_t bytes)
{
    if (bytes % 16) {
        bytes = bytes + (16 - (bytes % 16));
    }
    assembly_add_instruction(ctx->assembly, "sub", "sp,sp,#%zu", bytes);
    ctx->stack_allocated += bytes;
}

void arm64context_release_stack(ARM64Context *ctx)
{
    assembly_add_instruction(ctx->assembly, "add", "sp,sp,#%zu", ctx->stack_allocated);
    ctx->stack_allocated = 0;
}

void arm64context_add_module(ARM64Context *ctx, StringView module)
{
    for (Assembly *assembly = ctx->assemblies; assembly; assembly = assembly->next) {
        if (sv_eq(assembly->name, module)) {
            ctx->assembly = assembly;
            return;
        }
    }
    Assembly *new_assembly = assembly_acreate(ctx->allocator, module);
    new_assembly->next = ctx->assemblies;
    ctx->assemblies = new_assembly;
    ctx->assembly = ctx->assemblies;
}

size_t arm64context_counter()
{
    static size_t counter = 0;
    return counter++;
}

void arm64context_zero_initialize(ARM64Context *ctx, type_id type, int offset)
{
    switch (typeid_kind(type)) {
    case TK_PRIMITIVE:
        switch (typeid_builtin_type(type)) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) case BIT_##dt:
            INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
        case BIT_POINTER:
        case BIT_BOOL: {
            OptionalOpcodeMap opcode_map_maybe = get_opcode_map(type);
            assert(opcode_map_maybe.has_value);
            OpcodeMap *opcode_map = &opcode_map_maybe.value;
            assembly_add_instruction(ctx->assembly, "mov", "%s0,%szr", opcode_map->reg_width, opcode_map->reg_width);
            assembly_add_instruction(ctx->assembly, "str", "%s0,[fp,#%zu]", opcode_map->reg_width,
                arm64context_get_stack_depth(ctx) - offset);
        } break;
        default:
            NYI("zero_initialize of builtin type '%s'", BuiltinType_name(typeid_builtin_type(type)));
        }
        break;
    case TK_AGGREGATE: {
        ExpressionType *et = type_registry_get_type_by_id(type);
        for (size_t ix = 0; ix < et->components.num_components; ++ix) {
            TypeComponent *component = et->components.components + ix;
            MUST(Size, size_t, field_offset, type_offsetof_index(et, ix));
            arm64context_zero_initialize(ctx, component->type_id, offset - (int) field_offset);
        }
    } break;
    case TK_ARRAY: {
        // Arrays are not initialized now. Maybe that should be fixed
        break;
    }
    default:
        NYI("zero_initialize of type kind '%s'", TypeKind_name(typeid_kind(type)));
    }
}

int arm64context_load_variable(ARM64Context *ctx, type_id type, size_t offset, int target)
{
    switch (typeid_kind(type)) {
    case TK_PRIMITIVE: {
        OptionalOpcodeMap opcode_map_maybe = get_opcode_map(type);
        if (!opcode_map_maybe.has_value) {
            fatal("Cannot access values of variables of type '%.*s' yet", SV_ARG(typeid_name(type)));
        }
        OpcodeMap *opcode_map = &opcode_map_maybe.value;
        assembly_add_comment(
            ctx->assembly, "Loading variable: stack_depth %zu offset %zu", arm64context_get_stack_depth(ctx), offset);
        if (typeid_sizeof(type) < 8) {
            assembly_add_instruction(ctx->assembly, "mov", "x%.*s,xzr", target);
        }
        assembly_add_instruction(ctx->assembly, opcode_map->load_opcode, "%s%d,[fp,#%d]", opcode_map->reg_width, target,
            arm64context_get_stack_depth(ctx) - offset);
        return target + 1;
    }
    case TK_AGGREGATE: {
        assembly_add_comment(ctx->assembly, "Loading composite variable: stack_depth %zu offset %zu",
            arm64context_get_stack_depth(ctx), offset);
        ExpressionType *et = type_registry_get_type_by_id(type);
        for (size_t ix = 0; ix < et->components.num_components; ++ix) {
            TypeComponent *component_type = et->components.components + ix;
            MUST(Size, size_t, offset_of, type_offsetof_index(et, ix))
            target = arm64context_load_variable(ctx, component_type->type_id, offset + offset_of, target);
        }
        return target;
    }
    default:
        fatal("Cannot load variables of kind '%s' yet", TypeKind_name(typeid_kind(type)));
    }
}

int arm64context_store_variable(ARM64Context *ctx, type_id type, size_t offset, int from)
{
    switch (typeid_kind(type)) {
    case TK_PRIMITIVE: {
        OptionalOpcodeMap opcode_map_maybe = get_opcode_map(type);
        if (!opcode_map_maybe.has_value) {
            fatal("Cannot access values of variables of type '%.*s' yet", SV_ARG(typeid_name(type)));
        }
        OpcodeMap *opcode_map = &opcode_map_maybe.value;
        assembly_add_comment(ctx->assembly, "Storing to variable: stack_depth %zu offset %zu",
            arm64context_get_stack_depth(ctx), offset);
        assembly_add_instruction(ctx->assembly, opcode_map->store_opcode, "%s%d,[fp,#%ld]", opcode_map->reg_width, from,
            arm64context_get_stack_depth(ctx) - offset);
        return from + 1;
    }
    case TK_AGGREGATE: {
        assembly_add_comment(ctx->assembly, "Storing composite variable: stack_depth %zu offset %zu",
            arm64context_get_stack_depth(ctx), offset);
        ExpressionType *et = type_registry_get_type_by_id(type);
        for (size_t ix = 0; ix < et->components.num_components; ++ix) {
            TypeComponent *component_type = et->components.components + ix;
            MUST(Size, size_t, offset_of, type_offsetof_index(et, ix))
            from = arm64context_store_variable(ctx, component_type->type_id, offset + offset_of, from);
        }
        return from;
    }
    default:
        fatal("Cannot store variables of kind '%s' yet", TypeKind_name(typeid_kind(type)));
    }
}

int arm64context_push_value(ARM64Context *ctx, type_id type, int from)
{
    assembly_add_comment(ctx->assembly, "Pushing value of type '%.*s'", SV_ARG(typeid_name(type)));
    switch (typeid_kind(type)) {
    case TK_PRIMITIVE: {
        OptionalOpcodeMap opcode_map_maybe = get_opcode_map(type);
        if (!opcode_map_maybe.has_value) {
            fatal("Cannot access values of variables of type '%.*s' yet", SV_ARG(typeid_name(type)));
        }
        OpcodeMap *opcode_map = &opcode_map_maybe.value;
        assembly_add_instruction(ctx->assembly, opcode_map->store_opcode, "%s%d,[sp,#-16]!", opcode_map->reg_width, from);
        return from + 1;
    }
    case TK_AGGREGATE: {
        if (type != STRING_ID) {
            fatal("Cannot push values of kind '%s' yet", TypeKind_name(typeid_kind(type)));
        }
        //        ExpressionType *et = type_registry_get_type_by_id(type);
        //        for (size_t ix = 0; ix < et->components.num_components; ++ix) {
        //            TypeComponent *component_type = et->components.components + ix;
        //            MUST(Size, size_t, offset_of, type_offsetof_index(et, ix))
        //            from = arm64context_push_value(ctx, component_type->type_id, from);
        //        }
        //        return from;
        assembly_push(ctx->assembly, "x1");
        assembly_push(ctx->assembly, "x0");
        return from + 2;
    }
    default:
        fatal("Cannot push values of kind '%s' yet", TypeKind_name(typeid_kind(type)));
    }
}

int arm64context_pop_value(ARM64Context *ctx, type_id type, int target)
{
    assembly_add_comment(ctx->assembly, "Popping value of type '%.*s'", SV_ARG(typeid_name(type)));
    switch (typeid_kind(type)) {
    case TK_PRIMITIVE: {
        OptionalOpcodeMap opcode_map_maybe = get_opcode_map(type);
        if (!opcode_map_maybe.has_value) {
            fatal("Cannot access values of variables of type '%.*s' yet", SV_ARG(typeid_name(type)));
        }
        OpcodeMap *opcode_map = &opcode_map_maybe.value;
        assembly_add_instruction(ctx->assembly, opcode_map->load_opcode, "%s%d,[sp],#16", opcode_map->reg_width, target);
        return target + 1;
    }
    case TK_AGGREGATE: {
        if (type != STRING_ID) {
            fatal("Cannot pop values of type '%.*s' yet", SV_ARG(typeid_name(type)));
        }
        //        ExpressionType *et = type_registry_get_type_by_id(type);
        //        for (int ix = (int) et->components.num_components; ix >= 0; --ix) {
        //            TypeComponent *component_type = et->components.components + ix;
        //            MUST(Size, size_t, offset_of, type_offsetof_index(et, ix))
        //            target = arm64context_pop_value(ctx, component_type->type_id, target);
        //        }
        //        return target;
        assembly_pop(ctx->assembly, "x0");
        assembly_pop(ctx->assembly, "x1");
        return target + 2;
    }
    default:
        fatal("Cannot pop values of kind '%s' yet", TypeKind_name(typeid_kind(type)));
    }
}

void arm64context_define_static_storage(
    ARM64Context *ctx, StringView label, type_id type, bool global, long initial_value)
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
            ctx->assembly, label, global, static_type, true, sv_aprintf(ctx->allocator, "%ld", initial_value));
    } break;
    case TK_AGGREGATE:
    case TK_ARRAY: {
        size_t sz = typeid_sizeof(type);
        assembly_add_data(ctx->assembly, label, global, sv_from(".space"), true, sv_aprintf(ctx->allocator, "%zu", sz));
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

void arm64context_enter_function(ARM64Context *ctx, ARM64Function *func)
{
    ctx->function = func;
    size_t stack_depth = func->scribble.stack_depth;
    size_t nsaa = func->scribble.nsaa;
    arm64context_set_stack_depth(ctx, stack_depth);
    assembly_add_comment(ctx->assembly, "%.*s nsaa %zu stack depth %zu", arm64function_to_string(func),
        nsaa, stack_depth);
    assembly_add_directive(ctx->assembly, sv_from(".global"), arm64function_label(func));
    assembly_add_label(ctx->assembly, arm64function_label(func));

    // fp, lr, and sp have been set be the calling function

    // Copy parameters from registers to their spot in the stack.
    // @improve Do this lazily, i.e. when we need the registers
    assembly_add_instruction(ctx->assembly, "stp", "fp,lr,[sp,#-16]!");
    if (stack_depth) {
        assembly_add_instruction(ctx->assembly, "sub", "sp,sp,#%d", stack_depth);
    }
    assembly_add_instruction(ctx->assembly, "mov", "fp,sp");
    for (size_t ix = 0; ix < func->num_parameters; ++ix) {
        ARM64VarDecl *param = func->parameters + ix;
        switch (typeid_kind(param->var_decl->type.type_id)) {
        case TK_PRIMITIVE: {
            switch (typeid_builtin_type(param->var_decl->type.type_id)) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) case BIT_##dt:
                INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
            case BIT_POINTER:
            case BIT_BOOL: {
                switch (param->method) {
                case PPM_REGISTER: {
                    assembly_add_comment(ctx->assembly, "Register parameter %.*s: x%d -> %zu",
                        SV_ARG(param->var_decl->name), param->where, param->address.stack_address.offset);
                    assembly_add_instruction(ctx->assembly, "str", "x%d,[fp,#%zu]", param->where,
                        func->scribble.stack_depth - param->address.stack_address.offset);
                } break;
                case PPM_STACK: {
                    assembly_add_comment(ctx->assembly, "Stack parameter %.*s: nsaa %d -> %zu",
                        SV_ARG(param->var_decl->name), param->where, param->address.stack_address.offset);
                    assembly_add_instruction(ctx->assembly, "ldr", "x9,[fp,#%d]", 16 + nsaa - param->where);
                    assembly_add_instruction(ctx->assembly, "str", "x9,[fp,#%.*s]",
                        func->scribble.stack_depth - param->address.stack_address.offset);
                } break;
                }
            } break;
            default:
                NYI("Builtin type '%s'", BuiltinType_name(typeid_builtin_type(param->var_decl->type.type_id)));
            }
        } break;
        case TK_AGGREGATE:
            switch (param->method) {
            case PPM_REGISTER: {
                size_t          reg = param->where;
                ExpressionType *et = type_registry_get_type_by_id(param->var_decl->type.type_id);
                for (size_t cix = 0; cix < et->components.num_components; ++cix) {
                    char const *reg_width = "w";
                    if (typeid_sizeof(et->components.num_components) > 4) {
                        reg_width = "x";
                    }
                    MUST(Size, size_t, offset_of, type_offsetof_index(et, cix))

                    // FIXME BROKEN
                    assembly_add_instruction(ctx->assembly, "str", "%s%.*s,[fp,#-%.*s]", reg_width, reg++,
                        param->address.stack_address.offset + offset_of);
                }
            } break;
            case PPM_STACK:
                break;
            }
            // Fall through:
        default:
            NYI("Type '%.*s'", SV_ARG(typeid_name(param->var_decl->type.type_id)));
        }
    }
}

void arm64context_function_return(ARM64Context *ctx)
{
    assert(ctx->function);
    assembly_add_instruction(ctx->assembly, "b", "__%.*s__return", SV_ARG(arm64function_label(ctx->function)));
}

void arm64context_leave_function(ARM64Context *ctx)
{
    assert(ctx->function);
    assembly_add_label(ctx->assembly, sv_aprintf(ctx->allocator, "__%.*s__return", SV_ARG(arm64function_label(ctx->function))));
    assembly_add_instruction(ctx->assembly, "mov", "sp,fp");
    size_t depth = arm64context_get_stack_depth(ctx);
    if (depth) {
        assembly_add_instruction(ctx->assembly, "add", "sp,sp,#%zu", depth);
    }
    assembly_add_instruction(ctx->assembly, "ldp", "fp,lr,[sp],16");
    assembly_add_instruction(ctx->assembly, "ret", "");
    arm64context_pop_stack_depth(ctx);
    ctx->function = NULL;
}

void arm64context_prepare_function_arguments(ARM64Context *ctx)
{
}

ARM64Function *arm64context_function_by_name(ARM64Context *ctx, StringView name)
{
    for (size_t ix = 0; ix < ctx->num_functions; ++ix) {
        if (sv_eq(ctx->functions[ix].function->name, name)) {
            return ctx->functions + ix;
        }
    }
    return NULL;
}
