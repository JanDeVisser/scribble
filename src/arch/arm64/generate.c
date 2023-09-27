/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <arch/arm64/arm64.h>

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
        "mov    x5,xzr\n"
        "mov    x16,#0xC5\n"
        "svc    #0x00\n");
}

void generate_ENDLN(ARM64Context *ctx)
{
    assembly_write_char(ctx->assembly, 1, '\n');
}

void generate_CLOSE(ARM64Context *ctx)
{
    assembly_syscall(ctx->assembly, 0x06);
}

void generate_FPUTS(ARM64Context *ctx)
{
    assembly_syscall(ctx->assembly, 0x04);
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
    assembly_syscall(ctx->assembly, 0x04);
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
        "adr    x0,str_%zu\n"
        "mov    x1,#%d\n"
        "bl     _resolve_function\n"
        "cbz    x0,%.*s_$error\n"
        "mov    x16,x0",
        str_id, function->native_name.length, SV_ARG(function->name));
    assembly_pop(ctx->assembly, "x1");
    assembly_pop(ctx->assembly, "x0");
    assembly_add_instruction(ctx->assembly, "blr", "x16");
    arm64context_function_return(ctx);
    assembly_add_label(ctx->assembly, sv_aprintf(ctx->allocator, "%.*s_$error", SV_ARG(function->name)));
    assembly_add_instruction(ctx->assembly, "mov", "x0,#-1");
    arm64context_leave_function(ctx);
}

void generate_CALL(ARM64Context *ctx, IROperation *op)
{
    ARM64Function *arm_function = arm64context_function_by_name(ctx, op->sv);
    assert(arm_function);
    IRFunction *function = arm_function->function;

    for (size_t ix = 0; ix < arm_function->num_parameters; ++ix) {
        ARM64VarDecl *var_decl = arm_function->parameters + ix;
        arm64variable_address_store_variable(&var_decl->address, var_decl->var_decl->type.type_id, ctx, var_decl->where);
    }
    switch (function->kind) {
    case FK_SCRIBBLE:
    case FK_NATIVE:
        assembly_add_instruction(ctx->assembly, "bl", "%.*s", SV_ARG(arm_function->function->name));
        break;
    case FK_INTRINSIC:
        generate_intrinsic_call(ctx, arm_function);
        break;
    default:
        UNREACHABLE();
    }
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
    assembly_pop(ctx->assembly, "x0");
    assembly_add_instruction(ctx->assembly, "cbz", "x0,%.*s_%zu", SV_ARG(ctx->function->function->name), op->label);
}

void generate_JUMP_T(ARM64Context *ctx, IROperation *op)
{
    assembly_pop(ctx->assembly, "x0");
    assembly_add_instruction(ctx->assembly, "cbnz", "x0,%.*s_%zu", SV_ARG(ctx->function->function->name), op->label);
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
}

void generate_POP_VAR(ARM64Context *ctx, IROperation *op)
{
}

void generate_POP_VAR_COMPONENT(ARM64Context *ctx, IROperation *op)
{
}

void generate_PUSH_BOOL_CONSTANT(ARM64Context *ctx, IROperation *op)
{
    assembly_add_instruction(ctx->assembly, "mov", "w0,#%%d", (op->bool_value) ? 1 : 0);
    assembly_push(ctx->assembly, "x0");
}

void generate_PUSH_FLOAT_CONSTANT(ARM64Context *ctx, IROperation *op)
{
}

void generate_PUSH_INT_CONSTANT(ARM64Context *ctx, IROperation *op)
{
    if (op->integer.width < 64) {
        assembly_add_instruction(ctx->assembly, "mov", "w0,#%zu", op->integer.unsigned_value);
    } else {
        assembly_add_instruction(ctx->assembly, "mov", "x0,#%zu", op->integer.unsigned_value);
    }
    assembly_push(ctx->assembly, "x0");
}

void generate_PUSH_STRING_CONSTANT(ARM64Context *ctx, IROperation *op)
{
    size_t str_id = assembly_add_string(ctx->assembly, op->sv);
    assembly_add_instruction(ctx->assembly, "adr", "x0,str_%zu", str_id);
    assembly_add_instruction(ctx->assembly, "mov", "x1,#%zu", op->sv.length);
    assembly_push(ctx->assembly, "x1");
    assembly_push(ctx->assembly, "x0");
}

void generate_PUSH_VAR(ARM64Context *ctx, IROperation *op)
{
}

void generate_PUSH_VAR_COMPONENT(ARM64Context *ctx, IROperation *op)
{
}

void generate_RETURN(ARM64Context *ctx, IROperation *op)
{
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
    arm64context_enter_function(ctx, arm_function);
    for (size_t ix = 0; ix < function->scribble.num_operations; ++ix) {
        IROperation *op = function->scribble.operations + ix;
        switch (op->operation) {
#undef IR_OPERATION_TYPE
#define IR_OPERATION_TYPE(t)                                   \
    case IR_##t:                                               \
        generate_##t(ctx, function->scribble.operations + ix); \
        break;
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
    arm_function->parameters = allocator_alloc_array(ctx->allocator, ARM64VarDecl, function->num_parameters);
    size_t ngrn = 0;
    size_t offset = 0;
    for (size_t ix = 0; ix < function->num_parameters; ++ix) {
        ARM64VarDecl *arm_param = arm_function->parameters + ix;
        IRVarDecl    *ir_param = function->parameters + ix;
        arm_param->var_decl = ir_param;
        size_t sz = typeid_sizeof(ir_param->type.type_id);
        switch (typeid_kind(ir_param->type.type_id)) {
        case TK_PRIMITIVE: {
            if (ngrn < 8) {
                arm_param->method = PPM_REGISTER;
                arm_param->where = ngrn++;
                break;
            }
            arm_function->scribble.nsaa += 8;
            arm_param->method = PPM_STACK;
            arm_param->where = arm_function->scribble.nsaa;
        } break;
        case TK_AGGREGATE: {
            size_t size_in_double_words = typeid_sizeof(ir_param->type.type_id) / 8;
            if (sz % 8) {
                size_in_double_words++;
            }
            if (ngrn + size_in_double_words <= 8) {
                arm_param->method = PPM_REGISTER;
                arm_param->where = ngrn;
                ngrn += size_in_double_words;
                break;
            }
            arm_function->scribble.nsaa += sz;
            arm_param->method = PPM_STACK;
            arm_param->where = arm_function->scribble.nsaa;
        } break;
        default:
            NYI("generate arm function parameter for type kind '%s'", TypeKind_name(typeid_kind(ir_param->type.type_id)));
        }
        offset += sz;
        if (offset % 16)
            offset += 16 - (offset % 16);
        arm_param->address.type = VAT_STACK;
        arm_param->address.stack_address.offset = offset;
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
