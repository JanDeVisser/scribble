/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <allocate.h>
#include <arm64.h>
#include <sv.h>

#undef INTRINSIC_ENUM
#define INTRINSIC_ENUM(i) static void generate_##i(ARM64Function *caller, ARM64Function *intrinsic);
__attribute__((unused)) INTRINSICS(INTRINSIC_ENUM)
#undef INTRINSIC_ENUM

#undef IR_OPERATION_TYPE
#define IR_OPERATION_TYPE(t) static void generate_##t(ARM64Function *function, IROperation *op);
    IR_OPERATION_TYPES(IR_OPERATION_TYPE)
#undef IR_OPERATION_TYPE

        static void generate_code(ARM64Function *arm_function);
static void generate_intrinsic_call(ARM64Function *caller, ARM64Function *intrinsic);
static void generate_native(ARM64Function *arm_function);
static void generate_function_declaration(ARM64Function *arm_function, IRFunction *function);

DECLARE_SHARED_ALLOCATOR(arm64)

void generate_ALLOC(ARM64Function *caller, ARM64Function *intrinsic)
{
    // syscall SYS_mmap
    arm64function_add_text(intrinsic,
        "mov    x1,x0\n"
        "mov    x0,xzr\n"
        "mov    w2,#3\n"
        "mov    w3,#0x1002\n"
        "mov    w4,#-1\n"
        "mov    x5,xzr\n");
    arm64function_syscall(intrinsic, SYSCALL_MMAP);
}

void generate_ENDLN(ARM64Function *caller, ARM64Function *intrinsic)
{
    arm64function_write_char(caller, 1, '\n');
}

void generate_CLOSE(ARM64Function *caller, ARM64Function *intrinsic)
{
    arm64function_syscall(caller, SYSCALL_CLOSE);
}

void generate_FPUTS(ARM64Function *caller, ARM64Function *intrinsic)
{
    arm64function_syscall(caller, SYSCALL_WRITE);
}

void generate_OPEN(ARM64Function *caller, ARM64Function *intrinsic)
{
    arm64function_add_instruction(caller, "bl", "scribble$open");
}

void generate_PUTI(ARM64Function *caller, ARM64Function *intrinsic)
{
    arm64function_add_instruction(caller, "bl", "putint");
}

void generate_PUTLN(ARM64Function *caller, ARM64Function *intrinsic)
{
    arm64function_add_instruction(caller, "mov", "x2,x1");
    arm64function_add_instruction(caller, "mov", "x1,x0");
    arm64function_add_instruction(caller, "mov", "x0,#0x01");
    arm64function_syscall(caller, SYSCALL_WRITE);
    arm64function_write_char(caller, 1, '\n');
}

void generate_READ(ARM64Function *caller, ARM64Function *intrinsic)
{
    arm64function_syscall(caller, 0x04);
}

void generate_WRITE(ARM64Function *caller, ARM64Function *intrinsic)
{
    arm64function_syscall(caller, 0x04);
}

void generate_intrinsic_call(ARM64Function *caller, ARM64Function *intrinsic)
{
    IRFunction *function = intrinsic->function;
    assert(function->kind == FK_INTRINSIC);
    switch (function->intrinsic) {
#undef INTRINSIC_ENUM
#define INTRINSIC_ENUM(i)                \
    case INT_##i:                        \
        generate_##i(caller, intrinsic); \
        break;
        INTRINSICS(INTRINSIC_ENUM)
#undef INTRINSIC_ENUM
    default:
        UNREACHABLE();
    }
}

void generate_native(ARM64Function *function)
{
    IRFunction *ir_fnc = function->function;
    assert(ir_fnc->kind == FK_NATIVE);
    arm64function_enter(function);
    arm64function_push(function, REG_X0);
    arm64function_push(function, REG_X1);
    size_t str_id = assembly_add_string(function->assembly, ir_fnc->native_name);
    arm64function_add_text(function,
        "adrp   x0,str_%zu@PAGE\n"
        "add    x0,x0,str_%zu@PAGEOFF\n"
        "adrp   x16,_resolve_function@PAGE\n"
        "add    x16,x16,_resolve_function@PAGEOFF\n"
        "blr    x16\n"
        "cbz    x0,__%.*s_$error\n"
        "mov    x16,x0",
        str_id, str_id, SV_ARG(arm64function_label(function)));
    arm64function_pop(function, REG_X1);
    arm64function_pop(function, REG_X0);
    arm64function_add_instruction(function, "blr", "x16");
    arm64function_return(function);
    arm64function_add_label(function, sv_printf("__%.*s_$error", SV_ARG(arm64function_label(function))));
    arm64function_add_instruction(function, "mov", "x0,#-1");
    arm64function_leave(function);
}

void generate_CALL(ARM64Function *calling_function, IROperation *op)
{
    ARM64Function *called_function = arm64context_function_by_name(calling_function->assembly->ctx, op->sv);
    assert(called_function);
    arm64function_marshall_arguments(calling_function, called_function);
    switch (called_function->function->kind) {
    case FK_SCRIBBLE:
    case FK_NATIVE:
        arm64function_add_instruction(calling_function, "bl", "%.*s", SV_ARG(arm64function_label(called_function)));
        break;
    case FK_INTRINSIC:
        generate_intrinsic_call(calling_function, called_function);
        break;
    default:
        UNREACHABLE();
    }
    arm64function_marshall_return(calling_function, called_function, op->call.discard_result);
}

void generate_DECL_VAR(ARM64Function *function, IROperation *op)
{
}

void generate_DEFINE_AGGREGATE(ARM64Function *function, IROperation *op)
{
}

void generate_DEFINE_ALIAS(ARM64Function *function, IROperation *op)
{
}

void generate_DEFINE_ARRAY(ARM64Function *function, IROperation *op)
{
}

void generate_DEFINE_VARIANT(ARM64Function *function, IROperation *op)
{
}

void generate_JUMP(ARM64Function *function, IROperation *op)
{
    arm64function_add_instruction(function, "b", "%.*s_%zu",
        SV_ARG(arm64function_label(function)), op->label);
}

void generate_JUMP_F(ARM64Function *function, IROperation *op)
{
    MUST_OPTIONAL(ValueLocation, location, arm64function_pop_location(function));
    arm64function_add_instruction(function, "cbz", "%s,%.*s_%zu",
        reg(location.reg),
        SV_ARG(arm64function_label(function)), op->label);
}

void generate_JUMP_T(ARM64Function *function, IROperation *op)
{
    MUST_OPTIONAL(ValueLocation, location, arm64function_pop_location(function));
    arm64function_add_instruction(function, "cbnz", "%s,%.*s_%zu",
        reg(location.reg),
        SV_ARG(arm64function_label(function)), op->label);
}

void generate_LABEL(ARM64Function *function, IROperation *op)
{
    arm64function_add_label(function,
        sv_printf("%.*s_%zu",
            SV_ARG(arm64function_label(function)), op->label));
}

void generate_NEW_DATUM(ARM64Function *function, IROperation *op)
{
}

void generate_OPERATOR(ARM64Function *function, IROperation *op)
{
    MUST_OPTIONAL(ValueLocation, lhs, arm64function_pop_location(function));
    MUST_OPTIONAL(ValueLocation, rhs, arm64function_pop_location(function));
    Register result = arm64function_allocate_register(function);
    switch (op->operator.op) {
    case OP_ADD:
        arm64function_add_instruction(function, "add", "%s,%s,%s", w_reg(result), w_reg(lhs.reg), w_reg(rhs.reg));
        break;
    case OP_MULTIPLY:
        arm64function_add_instruction(function, "mul", "%s,%s,%s", w_reg(result), w_reg(lhs.reg), w_reg(rhs.reg));
        break;
    default:
        NYI("Operator %s", Operator_name(op->operator.op));
    }
    arm64function_push_register(function, I32_ID, result);
}

void generate_POP_VAR(ARM64Function *function, IROperation *op)
{
    MUST_OPTIONAL(ValueLocation, location, arm64function_pop_location(function));
    ARM64Variable *var = arm64function_variable_by_name(function, op->sv);
    arm64variable_store_variable(var, location);
}

void generate_POP_VAR_COMPONENT(ARM64Function *function, IROperation *op)
{
}

void generate_PUSH_BOOL_CONSTANT(ARM64Function *function, IROperation *op)
{
    Register r = arm64function_allocate_register(function);
    arm64function_add_instruction(function, "mov", "%s,#%d", w_reg(r), (op->bool_value) ? 1 : 0);
    arm64function_push_register(function, BOOL_ID, r);
}

void generate_PUSH_FLOAT_CONSTANT(ARM64Function *function, IROperation *op)
{
}

void generate_PUSH_INT_CONSTANT(ARM64Function *function, IROperation *op)
{
    Register      r = arm64function_allocate_register(function);
    RegisterWidth w = (BuiltinType_width(op->integer.type) < 64) ? RW_32 : RW_64;
    if (BuiltinType_is_unsigned(op->integer.type)) {
        arm64function_add_instruction(function, "mov", "%s,#%zu", reg_with_width(r, w), op->integer.value.unsigned_value);
    } else {
        arm64function_add_instruction(function, "mov", "%s,#%ld", reg_with_width(r, w), op->integer.value.signed_value);
    }
    arm64function_push_register(function, type_registry_id_of_builtin_type(op->integer.type), r);
}

void generate_PUSH_STRING_CONSTANT(ARM64Function *function, IROperation *op)
{
    RegisterRange r = arm64function_allocate_register_range(function, 2);
    size_t        str_id = assembly_add_string(function->assembly, op->sv);
    ValueLocation target = {
        .type = POINTER_ID,
        .kind = VLK_REGISTER,
        .reg = r.start,
    };
    ValueLocation source = {
        .type = POINTER_ID,
        .kind = VLK_LABEL,
        .symbol = sv_printf("str_%ld", str_id),
    };
    arm64function_copy(function, target, source);
    arm64function_add_instruction(function, "mov", "%s,#%ld", x_reg(r.end - 1), op->sv.length);
    arm64function_push_registers(function, STRING_ID, r);
}

void generate_PUSH_VAR(ARM64Function *function, IROperation *op)
{
    Register       r = arm64function_allocate_register(function);
    ARM64Variable *var = arm64function_variable_by_name(function, op->sv);
    ValueLocation  location = {
         .type = var->var_decl.type.type_id,
         .kind = VLK_REGISTER,
         .reg = r,
    };
    arm64variable_load_variable(var, location);
    arm64function_push_location(function, location);
}

void generate_PUSH_VAR_COMPONENT(ARM64Function *function, IROperation *op)
{
}

void generate_RETURN(ARM64Function *function, IROperation *op)
{
    MUST_OPTIONAL(ValueLocation, expr, arm64function_pop_location(function))
    ValueLocation x0 = {
        .type = function->function->type.type_id,
        .kind = VLK_REGISTER,
        .reg = REG_X0,
    };
    arm64function_copy(function, x0, expr);
    arm64function_return(function);
}

void generate_SCOPE_BEGIN(ARM64Function *function, IROperation *op)
{
    ARM64Scope *scope = function->scribble.current_scope;
    assert(scope);
    ARM64Scope *new_scope = (scope->current) ? scope->current->next : scope->scopes;
    assert(new_scope);
    assert(new_scope->operation == op);
    scope->current = new_scope;
    function->scribble.current_scope = new_scope;
}

void generate_SCOPE_END(ARM64Function *function, IROperation *op)
{
    ARM64Scope *scope = function->scribble.current_scope;
    assert(scope);
    scope->current = NULL;
    function->scribble.current_scope = scope->up;
}

void generate_code(ARM64Function *arm_function)
{
    IRFunction *function = arm_function->function;
    assert(function->kind == FK_SCRIBBLE);
    trace(CAT_COMPILE, "Generating code for %.*s", SV_ARG(function->name));
    arm_function->scribble.current_scope = &arm_function->scope;
    arm64function_enter(arm_function);
    for (size_t ix = 0; ix < function->operations.size; ++ix) {
        IROperation *op = function->operations.elements + ix;
        StringView   op_str = ir_operation_to_string(op);
        trace(CAT_COMPILE, "%.*s", SV_ARG(op_str));
        arm64function_add_comment(arm_function, "%.*s", SV_ARG(op_str));
        switch (op->operation) {
#undef IR_OPERATION_TYPE
#define IR_OPERATION_TYPE(t)            \
    case IR_##t: {                      \
        generate_##t(arm_function, op); \
    } break;
            IR_OPERATION_TYPES(IR_OPERATION_TYPE)
#undef IR_OPERATION_TYPE
        default:
            UNREACHABLE();
        }
    }
    arm64function_leave(arm_function);
}

void generate_function_declaration(ARM64Function *arm_function, IRFunction *function)
{
    int64_t offset = 0;
    if (function->num_parameters) {
        da_resize_ARM64Variable(&arm_function->scope.variables, function->num_parameters);
        Register ngrn = REG_X0;
        Register nsrn = REG_V0;
        for (size_t ix = 0; ix < function->num_parameters; ++ix) {
            IRVarDecl *ir_param = function->parameters + ix;
            type_id    type = typeid_canonical_type_id(ir_param->type.type_id);
            size_t     sz = align_at(typeid_sizeof(type), 8);
            offset += (int64_t) align_at(offset + sz, 16);
            size_t var_ix = da_append_ARM64Variable(
                &arm_function->scope.variables,
                (ARM64Variable) {
                    .scope = &arm_function->scope,
                    .kind = VK_PARAMETER,
                    .var_decl = *ir_param,
                    .parameter.offset = offset,
                });
            ARM64Variable *arm_param = arm_function->scope.variables.elements + var_ix;

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
                arm_function->nsaa += (int64_t) sz;
            } break;
            case TK_AGGREGATE: {
                size_t size_in_double_words = align_at(typeid_sizeof(type), 8) / 8;
                if ((size_in_double_words <= 2) && (ngrn + size_in_double_words < REG_X8)) {
                    arm_param->parameter.method = PPM_REGISTER_RANGE;
                    arm_param->parameter.range.start = ngrn;
                    ngrn += size_in_double_words;
                    arm_param->parameter.range.end = ngrn;
                    break;
                }
                if (ngrn < 8) {
                    arm_param->parameter.method = PPM_POINTER;
                    arm_param->parameter.reg = ngrn++;
                } else {
                    arm_param->parameter.method = PPM_POINTER_STACK;
                    arm_param->parameter.nsaa_offset = arm_function->nsaa;
                    arm_function->nsaa += (int64_t) align_at(typeid_sizeof(PCHAR_ID), 8);
                }
            } break;
            default:
                NYI("generate arm function parameter for type kind '%s'", TypeKind_name(typeid_kind(type)));
            }
        }
    }
    if (function->kind == FK_SCRIBBLE) {
        arm_function->scope.depth = offset;
        arm_function->scope.scope_offset = 0;
        ARM64Scope *scope = &arm_function->scope;
        for (size_t op_ix = 0; op_ix < function->operations.size; ++op_ix) {
            IROperation *op = function->operations.elements + op_ix;
            switch (op->operation) {
            case IR_DECL_VAR: {
                size_t var_ix = da_append_ARM64Variable(
                    &scope->variables,
                    (ARM64Variable) {
                        .scope = scope,
                        .kind = VK_LOCAL,
                        .var_decl = op->var_decl,
                    });
                ARM64Variable *variable = scope->variables.elements + var_ix;
                scope->depth += (int64_t) align_at(typeid_sizeof(variable->var_decl.type.type_id), 16);
                variable->local_address.offset = scope->scope_offset + scope->depth;
            } break;
            case IR_SCOPE_BEGIN: {
                ARM64Scope *new_scope = allocate_new(ARM64Scope);
                new_scope->kind = SK_BLOCK;
                new_scope->operation = op;
                new_scope->up = scope;
                new_scope->function = arm_function;
                new_scope->depth = 0;
                new_scope->scope_offset = scope->scope_offset + scope->depth;
                if (scope->current) {
                    scope->current->next = new_scope;
                } else {
                    scope->scopes = new_scope;
                }
                scope->current = new_scope;
                scope = new_scope;
            } break;
            case IR_SCOPE_END: {
                assert(scope->up);
                scope->current = NULL;
                if (scope->scope_offset + scope->depth > arm_function->scribble.stack_depth) {
                    arm_function->scribble.stack_depth = scope->scope_offset + scope->depth;
                }
                scope = scope->up;
            } break;
            default:
                break;
            }
        }
        scope->current = NULL;
    }
}

void initialize_assembly(Assembly *assembly)
{
    IRModule *module = assembly->module;

    da_resize_ARM64Function(&assembly->functions, module->functions.size);
    for (size_t ix = 0; ix < module->functions.size; ++ix) {
        IRFunction *function = module->functions.elements + ix;
        size_t      fnc_ix = da_append_ARM64Function(
            &assembly->functions,
            (ARM64Function) {
                     .assembly = assembly,
                     .function = function,
                     .scope.kind = SK_FUNCTION,
                     .scope.up = &assembly->scope,
            });
        ARM64Function *arm_function = assembly->functions.elements + fnc_ix;
        if (function->kind == FK_SCRIBBLE) {
            arm_function->scribble.code = code_create(arm_function);
        }
        generate_function_declaration(arm_function, function);
    }
}

void generate_assembly(Assembly *assembly)
{
    for (size_t ix = 0; ix < assembly->functions.size; ++ix) {
        ARM64Function *function = assembly->functions.elements + ix;
        switch (function->function->kind) {
        case FK_SCRIBBLE: {
            generate_code(function);
        } break;
        case FK_NATIVE: {
            generate_native(function);
        } break;
        case FK_INTRINSIC: {
        } break;
        default:
            UNREACHABLE();
        }
    }
}

ARM64Context *generate_arm64(IRProgram *program)
{
    ARM64Context *ctx = allocate_new(ARM64Context);
    ctx->program = program;
    ctx->scope.kind = SK_GLOBAL;
    ctx->scope.up = NULL;
    da_resize_Assembly(&ctx->assemblies, program->modules.size);
    for (size_t ix = 0; ix < program->modules.size; ++ix) {
        IRModule *module = program->modules.elements + ix;
        size_t    obj_ix = da_append_Assembly(
            &ctx->assemblies,
            (Assembly) {
                   .ctx = ctx,
                   .module = module,
                   .scope.kind = SK_STATIC,
                   .scope.up = &ctx->scope,
                   .code = code_create(NULL),
                   .data = code_create(NULL),
                   .has_exports = false,
                   .has_main = false,
            });
        initialize_assembly(ctx->assemblies.elements + obj_ix);
        generate_assembly(ctx->assemblies.elements + obj_ix);
    }
    return ctx;
}
