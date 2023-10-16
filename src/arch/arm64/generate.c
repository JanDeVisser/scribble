/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <arm64.h>
#include <sv.h>

#undef INTRINSIC_ENUM
#define INTRINSIC_ENUM(i) static void generate_##i(ARM64Context *ctx);
__attribute__((unused)) INTRINSICS(INTRINSIC_ENUM)
#undef INTRINSIC_ENUM

#undef IR_OPERATION_TYPE
#define IR_OPERATION_TYPE(t) static void generate_##t(ARM64Context *ctx, IROperation *op);
    IR_OPERATION_TYPES(IR_OPERATION_TYPE)
#undef IR_OPERATION_TYPE

        static void generate_code(ARM64Context *ctx, ARM64Function *arm_function);
static void generate_intrinsic_call(ARM64Context *ctx, ARM64Function *arm_function);
static void generate_native(ARM64Context *ctx, ARM64Function *arm_function);
static void generate_function_declaration(ARM64Context *ctx, size_t fnc_ix);

void generate_ALLOC(ARM64Context *ctx)
{
    // syscall SYS_mmap
    assembly_add_text(ctx->assembly,
        "mov    x1,x0\n"
        "mov    x0,xzr\n"
        "mov    w2,#3\n"
        "mov    w3,#0x1002\n"
        "mov    w4,#-1\n"
        "mov    x5,xzr\n");
    assembly_syscall(ctx->assembly, SYSCALL_MMAP);
}

void generate_ENDLN(ARM64Context *ctx)
{
    assembly_write_char(ctx->assembly, 1, '\n');
}

void generate_CLOSE(ARM64Context *ctx)
{
    assembly_syscall(ctx->assembly, SYSCALL_CLOSE);
}

void generate_FPUTS(ARM64Context *ctx)
{
    assembly_syscall(ctx->assembly, SYSCALL_WRITE);
}

void generate_OPEN(ARM64Context *ctx)
{
    assembly_add_instruction(ctx->assembly, "bl", "scribble$open");
}

void generate_PUTI(ARM64Context *ctx)
{
    assembly_add_instruction(ctx->assembly, "bl", "putint");
}

void generate_PUTLN(ARM64Context *ctx)
{
    assembly_add_instruction(ctx->assembly, "mov", "x2,x1");
    assembly_add_instruction(ctx->assembly, "mov", "x1,x0");
    assembly_add_instruction(ctx->assembly, "mov", "x0,#0x01");
    assembly_syscall(ctx->assembly, SYSCALL_WRITE);
    assembly_write_char(ctx->assembly, 1, '\n');
}

void generate_READ(ARM64Context *ctx)
{
    assembly_syscall(ctx->assembly, 0x04);
}

void generate_WRITE(ARM64Context *ctx)
{
    assembly_syscall(ctx->assembly, 0x04);
}

void generate_intrinsic_call(ARM64Context *ctx, ARM64Function *arm_function)
{
    IRFunction *function = arm_function->function;
    assert(function->kind == FK_INTRINSIC);
    switch (function->intrinsic) {
#undef INTRINSIC_ENUM
#define INTRINSIC_ENUM(i)  \
    case INT_##i:          \
        generate_##i(ctx); \
        break;
        INTRINSICS(INTRINSIC_ENUM)
#undef INTRINSIC_ENUM
    default:
        UNREACHABLE();
    }
}

void generate_native(ARM64Context *ctx, ARM64Function *arm_function)
{
    IRFunction *function = arm_function->function;
    assert(function->kind == FK_NATIVE);
    StringBuilder label_name = sb_acreate(ctx->allocator);
    sb_printf(&label_name, SV_SPEC "_$native_name", SV_ARG(function->name));
    assembly_add_text(ctx->assembly, ".global _resolve_function\n");
    size_t str_id = assembly_add_string(ctx->assembly, function->native_name);
    arm64context_enter_function(ctx, arm_function);
    assembly_push(ctx->assembly, "x0");
    assembly_push(ctx->assembly, "x1");
    assembly_add_text(ctx->assembly,
        "adrp   x0,str_%zu@PAGE\n"
        "add    x0,x0,str_%zu@PAGEOFF\n"
        "adrp   x16,_resolve_function@PAGE\n"
        "add    x16,x16,_resolve_function@PAGEOFF\n"
        "blr    x16\n"
        "cbz    x0,__%.*s_$error\n"
        "mov    x16,x0",
        str_id, str_id, SV_ARG(arm64function_label(arm_function)));
    assembly_pop(ctx->assembly, "x1");
    assembly_pop(ctx->assembly, "x0");
    assembly_add_instruction(ctx->assembly, "blr", "x16");
    arm64context_function_return(ctx);
    assembly_add_label(ctx->assembly, sv_aprintf(ctx->allocator, "__%.*s_$error", SV_ARG(arm64function_label(arm_function))));
    assembly_add_instruction(ctx->assembly, "mov", "x0,#-1");
    arm64context_leave_function(ctx);
}

void generate_CALL(ARM64Context *ctx, IROperation *op)
{
    ARM64Function *func = arm64context_function_by_name(ctx, op->sv);
    assert(func);
    IRFunction *function = func->function;

    if (func->nsaa > 0) {
        assembly_add_instruction(ctx->assembly, "sub", "sp,sp,#%d", func->nsaa);
    }
    for (size_t ix = 0; ix < func->num_parameters; ++ix) {
        ARM64Variable *param = func->parameters + ix;
        assert(param->kind == VK_PARAMETER);
        type_id type = typeid_canonical_type_id(param->var_decl.type.type_id);
        MUST_OPTIONAL(ValueLocation, arg_location, arm64context_pop_location(ctx))
        assembly_add_comment(ctx->assembly, "Marshalling argument %.*s: %.*s from %.*s",
            SV_ARG(param->var_decl.name), SV_ARG(typeid_name(param->var_decl.type.type_id)),
            SV_ARG(value_location_to_string(arg_location, ctx->allocator)));
        switch (param->parameter.method) {
        case PPM_REGISTER: {
            ValueLocation param_location = {
                .type = type,
                .kind = VLK_REGISTER,
                .reg = param->parameter.reg,
            };
            assembly_copy(ctx->assembly, param_location, arg_location);
        } break;
        case PPM_STACK: {
            ValueLocation param_location = {
                .type = type,
                .kind = VLK_STACK,
                .offset = param->parameter.nsaa_offset,
            };
            assembly_copy(ctx->assembly, param_location, arg_location);
        } break;
        case PPM_POINTER: {
            switch (arg_location.kind) {
            case VLK_POINTER:
                assembly_add_instruction(ctx->assembly, "add", "%s,%s,%#ld",
                    x_reg(param->parameter.reg), x_reg(arg_location.pointer.reg), arg_location.pointer.offset);
                break;
            case VLK_STACK:
                assembly_add_instruction(ctx->assembly, "add", "%s,%s,%#ld",
                    x_reg(param->parameter.reg), x_reg(REG_SP), arg_location.offset);
                break;
            case VLK_SYMBOL:
                assembly_add_instruction(ctx->assembly, "adrp", "%s,%.*s@PAGE", x_reg(param->parameter.reg), SV_ARG(arg_location.symbol));
                assembly_add_instruction(ctx->assembly, "add", "%s,%s,%.*s@PAGEOFF", x_reg(param->parameter.reg), x_reg(param->parameter.reg), SV_ARG(arg_location.symbol));
                break;
            default:
                UNREACHABLE();
            }
        } break;
        case PPM_POINTER_STACK: {
            Register r = assembly_allocate_register(ctx->assembly);
            switch (arg_location.kind) {
            case VLK_POINTER:
                assembly_add_instruction(ctx->assembly, "add", "%s,%s,%#ld",
                    x_reg(param->parameter.reg), x_reg(arg_location.pointer.reg), arg_location.pointer.offset);
                break;
            case VLK_STACK:
                assembly_add_instruction(ctx->assembly, "add", "%s,%s,%#ld",
                    x_reg(param->parameter.reg), reg(REG_SP), arg_location.offset);
                break;
            case VLK_SYMBOL:
                assembly_add_instruction(ctx->assembly, "adrp", "%s,%.*s@PAGE", x_reg(param->parameter.reg), SV_ARG(arg_location.symbol));
                assembly_add_instruction(ctx->assembly, "add", "%s,%s,%.*s@PAGEOFF", x_reg(param->parameter.reg), reg(param->parameter.reg), SV_ARG(arg_location.symbol));
                break;
            default:
                UNREACHABLE();
            }
            assembly_add_instruction(ctx->assembly, "str", "%s,[sp,#%d]", x_reg(param->parameter.reg), param->parameter.nsaa_offset);
            assembly_release_register(ctx->assembly, r);
        } break;
        }
    }
    switch (function->kind) {
    case FK_SCRIBBLE:
    case FK_NATIVE:
        assembly_add_instruction(ctx->assembly, "bl", "%.*s", SV_ARG(arm64function_label(func)));
        break;
    case FK_INTRINSIC:
        generate_intrinsic_call(ctx, func);
        break;
    default:
        UNREACHABLE();
    }
    ValueLocation x0 = {
        .type = function->type.type_id,
        .kind = VLK_REGISTER,
        .reg = REG_X0,
    };
    Register      r = assembly_allocate_register(ctx->assembly);
    ValueLocation target = {
        .type = function->type.type_id,
        .kind = VLK_REGISTER,
        .reg = r,
    };
    assembly_copy(ctx->assembly, target, x0);
    arm64context_push_location(ctx, target);
}

void generate_DECL_VAR(ARM64Context *ctx, IROperation *op)
{
}

void generate_DEFINE_AGGREGATE(ARM64Context *ctx, IROperation *op)
{
}

void generate_DEFINE_ALIAS(ARM64Context *ctx, IROperation *op)
{
}

void generate_DEFINE_ARRAY(ARM64Context *ctx, IROperation *op)
{
}

void generate_DEFINE_VARIANT(ARM64Context *ctx, IROperation *op)
{
}

void generate_JUMP(ARM64Context *ctx, IROperation *op)
{
    assembly_add_instruction(ctx->assembly, "b", "%.*s_%zu", SV_ARG(ctx->function->function->name), op->label);
}

void generate_JUMP_F(ARM64Context *ctx, IROperation *op)
{
    MUST_OPTIONAL(ValueLocation, location, arm64context_pop_location(ctx));
    assembly_add_instruction(ctx->assembly, "cbz", "%s,%.*s_%zu", reg(location.reg), SV_ARG(ctx->function->function->name), op->label);
}

void generate_JUMP_T(ARM64Context *ctx, IROperation *op)
{
    MUST_OPTIONAL(ValueLocation, location, arm64context_pop_location(ctx));
    assembly_add_instruction(ctx->assembly, "cbnz", "%s,%.*s_%zu", reg(location.reg), SV_ARG(ctx->function->function->name), op->label);
}

void generate_LABEL(ARM64Context *ctx, IROperation *op)
{
    assembly_add_label(ctx->assembly, sv_aprintf(ctx->allocator, "%.*s_%zu", SV_ARG(ctx->function->function->name), op->label));
}

void generate_NEW_DATUM(ARM64Context *ctx, IROperation *op)
{
}

void generate_OPERATOR(ARM64Context *ctx, IROperation *op)
{
    MUST_OPTIONAL(ValueLocation, lhs, arm64context_pop_location(ctx));
    MUST_OPTIONAL(ValueLocation, rhs, arm64context_pop_location(ctx));
    Register result = assembly_allocate_register(ctx->assembly);
    switch (op->operator.op) {
    case OP_ADD:
        assembly_add_instruction(ctx->assembly, "add", "%s,%s,%s", w_reg(result), w_reg(lhs.reg), w_reg(rhs.reg));
        break;
    case OP_MULTIPLY:
        assembly_add_instruction(ctx->assembly, "mul", "%s,%s,%s", w_reg(result), w_reg(lhs.reg), w_reg(rhs.reg));
        break;
    default:
        NYI("Operator %s", Operator_name(op->operator.op));
    }
    arm64context_push_register(ctx, I32_ID, result);
}

void generate_POP_VAR(ARM64Context *ctx, IROperation *op)
{
    MUST_OPTIONAL(ValueLocation, location, arm64context_pop_location(ctx));
    ARM64Variable *var = arm64function_variable_by_name(ctx->function, op->sv);
    arm64variable_store_variable(var, ctx, location);
}

void generate_POP_VAR_COMPONENT(ARM64Context *ctx, IROperation *op)
{
}

void generate_PUSH_BOOL_CONSTANT(ARM64Context *ctx, IROperation *op)
{
    Register r = assembly_allocate_register(ctx->assembly);
    assembly_add_instruction(ctx->assembly, "mov", "%s,#%d", w_reg(r), (op->bool_value) ? 1 : 0);
    arm64context_push_register(ctx, BOOL_ID, r);
}

void generate_PUSH_FLOAT_CONSTANT(ARM64Context *ctx, IROperation *op)
{
}

void generate_PUSH_INT_CONSTANT(ARM64Context *ctx, IROperation *op)
{
    Register      r = assembly_allocate_register(ctx->assembly);
    RegisterWidth w = (BuiltinType_width(op->integer.type) < 64) ? RW_32 : RW_64;
    if (BuiltinType_is_unsigned(op->integer.type)) {
        assembly_add_instruction(ctx->assembly, "mov", "%s,#%zu", reg_with_width(r, w), op->integer.value.unsigned_value);
    } else {
        assembly_add_instruction(ctx->assembly, "mov", "%s,#%ld", reg_with_width(r, w), op->integer.value.signed_value);
    }
    arm64context_push_register(ctx, type_registry_id_of_builtin_type(op->integer.type), r);
}

void generate_PUSH_STRING_CONSTANT(ARM64Context *ctx, IROperation *op)
{
    size_t str_id = assembly_add_string(ctx->assembly, op->sv);
    assembly_add_instruction(ctx->assembly, "adr", "x0,str_%zu", str_id);
    assembly_add_instruction(ctx->assembly, "mov", "x1,#%zu", op->sv.length);
    // arm64context_push_value(ctx, STRING_ID, 0);
}

void generate_PUSH_VAR(ARM64Context *ctx, IROperation *op)
{
    Register       r = assembly_allocate_register(ctx->assembly);
    ARM64Variable *var = arm64function_variable_by_name(ctx->function, op->sv);
    ValueLocation  location = {
         .type = var->var_decl.type.type_id,
         .kind = VLK_REGISTER,
         .reg = r,
    };
    arm64variable_load_variable(var, ctx, location);
    arm64context_push_location(ctx, location);
}

void generate_PUSH_VAR_COMPONENT(ARM64Context *ctx, IROperation *op)
{
}

void generate_RETURN(ARM64Context *ctx, IROperation *op)
{
    MUST_OPTIONAL(ValueLocation, expr, arm64context_pop_location(ctx))
    ValueLocation x0 = {
        .type = ctx->function->function->type.type_id,
        .kind = VLK_REGISTER,
        .reg = REG_X0,
    };
    assembly_copy(ctx->assembly, x0, expr);
    arm64context_function_return(ctx);
}

void generate_SCOPE_BEGIN(ARM64Context *ctx, IROperation *op)
{
}

void generate_SCOPE_END(ARM64Context *ctx, IROperation *op)
{
}

void generate_code(ARM64Context *ctx, ARM64Function *arm_function)
{
    IRFunction *function = arm_function->function;
    assert(function->kind == FK_SCRIBBLE);
    trace(CAT_COMPILE, "Generating code for %.*s", SV_ARG(function->name));
    arm64context_enter_function(ctx, arm_function);
    for (size_t ix = 0; ix < function->scribble.num_operations; ++ix) {
        IROperation *op = function->scribble.operations + ix;
        StringView   op_str = ir_operation_to_string(op, ctx->allocator);
        trace(CAT_COMPILE, "%.*s", SV_ARG(op_str));
        assembly_add_comment(ctx->assembly, "%.*s", SV_ARG(op_str));
        switch (op->operation) {
#undef IR_OPERATION_TYPE
#define IR_OPERATION_TYPE(t)   \
    case IR_##t: {             \
        generate_##t(ctx, op); \
    } break;
            IR_OPERATION_TYPES(IR_OPERATION_TYPE)
#undef IR_OPERATION_TYPE
        default:
            UNREACHABLE();
        }
    }
    arm64context_leave_function(ctx);
}

void generate_function_declaration(ARM64Context *ctx, size_t fnc_ix)
{
    IRFunction    *function = ctx->program->functions + fnc_ix;
    ARM64Function *arm_function = ctx->functions + fnc_ix;
    arm_function->allocator = ctx->allocator;
    arm_function->function = function;
    arm_function->num_parameters = function->num_parameters;
    size_t offset = 0;
    if (arm_function->num_parameters) {
        arm_function->parameters = allocator_alloc_array(ctx->allocator, ARM64Variable, function->num_parameters);
        Register       ngrn = REG_X0;
        Register       nsrn = REG_V0;
        ARM64Variable *prev = NULL;
        for (size_t ix = 0; ix < function->num_parameters; ++ix) {
            ARM64Variable *arm_param = arm_function->parameters + ix;
            IRVarDecl     *ir_param = function->parameters + ix;
            arm_param->kind = VK_PARAMETER;
            arm_param->var_decl = *ir_param;
            if (prev) {
                prev->next = arm_param;
            }
            prev = arm_param;
            size_t sz = align_at(typeid_sizeof(ir_param->type.type_id), 8);
            offset = align_at(offset + sz, 16);
            arm_param->parameter.offset = offset;
            type_id type = typeid_canonical_type_id(ir_param->type.type_id);
            switch (typeid_kind(type)) {
            case TK_PRIMITIVE: {
                if (type == FLOAT_ID && nsrn < REG_V8) {
                    arm_param->parameter.method = PPM_REGISTER;
                    arm_param->parameter.reg = nsrn++;
                    break;
                }
                if (type != FLOAT_ID && ngrn < REG_X8) {
                    arm_param->parameter.method = PPM_REGISTER;
                    arm_param->parameter.reg = ngrn++;
                    break;
                }
                arm_param->parameter.method = PPM_STACK;
                arm_param->parameter.nsaa_offset = arm_function->nsaa;
                arm_function->nsaa += sz;
            } break;
            case TK_AGGREGATE: {
                size_t size_in_double_words = align_at(typeid_sizeof(type), 8);
                if ((size_in_double_words <= 2) && (ngrn + size_in_double_words < REG_X8)) {
                    arm_param->parameter.method = PPM_REGISTER;
                    arm_param->parameter.reg = ngrn;
                    ngrn += size_in_double_words;
                    break;
                }
                if (ngrn < 8) {
                    arm_param->parameter.method = PPM_POINTER;
                    arm_param->parameter.reg = ngrn++;
                } else {
                    arm_param->parameter.method = PPM_POINTER_STACK;
                    arm_param->parameter.nsaa_offset = arm_function->nsaa;
                    arm_function->nsaa += align_at(typeid_sizeof(PCHAR_ID), 8);
                }
            } break;
            default:
                NYI("generate arm function parameter for type kind '%s'", TypeKind_name(typeid_kind(type)));
            }
        }
    }

    arm_function->scribble.stack_depth = offset;
    if (function->kind == FK_SCRIBBLE) {
        arm_function->scribble.scope = allocator_alloc_new(ctx->allocator, ARM64Scope);
        ARM64Scope *scope = arm_function->scribble.scope;

        // Parameters are accessible in two ways:
        // - As an _array_ of ARM64Variables through ARM64Function.parameters
        // - As elements of a _linked list_ of ARM64Variables in the scope
        // The linked list elements and array elements are the same memory
        // objects!
        scope->variables = arm_function->parameters;
        scope->depth = offset;
        for (size_t op_ix = 0; op_ix < function->scribble.num_operations; ++op_ix) {
            IROperation *op = function->scribble.operations + op_ix;
            switch (op->operation) {
            case IR_DECL_VAR: {
                ARM64Variable *variable = allocator_alloc_new(ctx->allocator, ARM64Variable);
                variable->var_decl = op->var_decl;
                variable->kind = VK_LOCAL;
                variable->next = scope->variables;
                scope->variables = variable;
                scope->depth = align_at(scope->depth + typeid_sizeof(variable->var_decl.type.type_id), 16);
                variable->local_address.offset = scope->depth;
            } break;
            case IR_SCOPE_BEGIN: {
                ARM64Scope *new_scope = allocator_alloc_new(ctx->allocator, ARM64Scope);
                new_scope->operation = op;
                new_scope->up = scope;
                new_scope->next = scope->scopes;
                scope->scopes = new_scope;
                scope = new_scope;
            } break;
            case IR_SCOPE_END: {
                assert(scope->up);
                scope = scope->up;
            } break;
            default:
                break;
            }
        }
        arm_function->scribble.stack_depth = arm_function->scribble.scope->depth;
    }
}

ARM64Context *generate_function_declarations(ARM64Context *ctx)
{
    ctx->functions = allocator_alloc_array(ctx->allocator, ARM64Function, ctx->program->num_functions);
    ctx->num_functions = ctx->program->num_functions;
    for (size_t ix = 0; ix < ctx->program->num_functions; ++ix) {
        generate_function_declaration(ctx, ix);
    }
    return ctx;
}

ARM64Context *generate_function_code(ARM64Context *ctx)
{
    for (size_t ix = 0; ix < ctx->program->num_functions; ++ix) {
        ARM64Function *function = ctx->functions + ix;
        switch (function->function->kind) {
        case FK_SCRIBBLE: {
            generate_code(ctx, function);
        } break;
        case FK_NATIVE: {
            generate_native(ctx, function);
        } break;
        case FK_INTRINSIC: {
        } break;
        default:
            UNREACHABLE();
        }
    }
    return ctx;
}

ARM64Context *generate_arm64(IRProgram *program, Allocator *allocator)
{
    ARM64Context *ctx = allocator_alloc_new(allocator, ARM64Context);
    ctx->allocator = allocator;
    ctx->program = program;
    generate_function_declarations(ctx);
    Assembly *assembly = assembly_acreate(allocator, program->name);
    ctx->assemblies = assembly;
    ctx->assembly = assembly;
    generate_function_code(ctx);
    return ctx;
}
