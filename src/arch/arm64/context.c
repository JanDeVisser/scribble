/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <errno.h>

#include <allocate.h>
#include <error_or.h>
#include <options.h>

#include <arm64.h>
#include <stddef.h>

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
    func->scribble.current_scope = func->scribble.scope;
    int64_t stack_depth = func->scribble.stack_depth;
    int64_t nsaa = func->nsaa;
    arm64context_set_stack_depth(ctx, stack_depth);
    assembly_add_comment(ctx->assembly, "%.*s nsaa %ld stack depth %ld", arm64function_to_string(func),
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
        ARM64Variable *param = func->parameters + ix;
        assert(param->kind == VK_PARAMETER);
        type_id       type = typeid_canonical_type_id(param->var_decl.type.type_id);
        ValueLocation param_location = {
            .type = type,
            .kind = VLK_POINTER,
            .pointer = {
                .reg = REG_FP,
                .offset = func->scribble.stack_depth - param->parameter.offset }
        };
        assembly_add_comment(ctx->assembly, "Unmarshalling parameter %.*s: %.*s -> offset %ld",
            SV_ARG(param->var_decl.name), SV_ARG(typeid_name(param->var_decl.type.type_id)), func->scribble.stack_depth - param->parameter.offset);
        switch (param->parameter.method) {
        case PPM_REGISTER: {
            ValueLocation arg_location = {
                .type = type,
                .kind = VLK_REGISTER,
                .reg = param->parameter.reg,
            };
            assembly_copy(ctx->assembly, param_location, arg_location);
        } break;
        case PPM_POINTER: {
            ValueLocation arg_location = {
                .type = type,
                .kind = VLK_POINTER,
                .pointer = {
                    .reg = param->parameter.reg,
                    .offset = 0,
                }
            };
            assembly_add_comment(ctx->assembly, "Pointer parameter %.*s: %s -> %ld",
                SV_ARG(param->var_decl.name), reg(param->parameter.reg), param->parameter.offset);
            assembly_copy(ctx->assembly, param_location, arg_location);
        } break;
        case PPM_STACK: {
            ValueLocation arg_location = {
                .type = type,
                .kind = VLK_STACK,
                .offset = param->parameter.nsaa_offset + stack_depth + 16,
            };
            assembly_add_comment(ctx->assembly, "Stack parameter %.*s: %ld -> %ld",
                SV_ARG(param->var_decl.name), param->parameter.nsaa_offset, param->parameter.offset);
            assembly_copy(ctx->assembly, param_location, arg_location);
        } break;
        case PPM_POINTER_STACK: {
            Register      r = assembly_allocate_register(ctx->assembly);
            ValueLocation arg_location = {
                .type = type,
                .kind = VLK_POINTER,
                .pointer = {
                    .reg = r,
                    .offset = 0,
                }
            };
            assembly_add_comment(ctx->assembly, "Stacked pointer parameter %.*s: %ld -> %ld",
                SV_ARG(param->var_decl.name), param->parameter.nsaa_offset, param->parameter.offset);
            assembly_add_instruction(ctx->assembly, "ldr", "%s,[fp,#%d]", x_reg(r), param->parameter.nsaa_offset + stack_depth + 16);
            assembly_copy(ctx->assembly, param_location, arg_location);
            assembly_release_register(ctx->assembly, r);
        } break;
        default:
            UNREACHABLE();
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
        assembly_add_instruction(ctx->assembly, "add", "sp,sp,#%ld", depth);
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

OptionalValueLocation arm64context_pop_location(ARM64Context *ctx)
{
    assert(ctx->function);
    assert(ctx->function->function->kind == FK_SCRIBBLE);
    ARM64Scope *scope = ctx->function->scribble.scope;
    assert(scope);
    if (!scope->expression_stack) {
        return OptionalValueLocation_empty();
    }
    switch (scope->expression_stack->kind) {
    case VLK_REGISTER: {
        assembly_release_register(ctx->assembly, scope->expression_stack->reg);
    } break;
    default:
        break;
    }
    OptionalValueLocation ret = OptionalValueLocation_create(*scope->expression_stack);
    scope->expression_stack = scope->expression_stack->next;
    return ret;
}

void arm64context_push_location(ARM64Context *ctx, ValueLocation entry)
{
    assert(ctx->function);
    assert(ctx->function->function->kind == FK_SCRIBBLE);
    ARM64Scope *scope = ctx->function->scribble.scope;
    assert(scope);
    ValueLocation *new_entry = allocate_new(ValueLocation);
    memcpy(new_entry, &entry, sizeof(ValueLocation));
    new_entry->next = scope->expression_stack;
    scope->expression_stack = new_entry;
}

void arm64context_push_register(ARM64Context *ctx, type_id type, Register reg)
{
    arm64context_push_location(ctx, (ValueLocation) { .type = type, .kind = VLK_REGISTER, .reg = reg });
}
