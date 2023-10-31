/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <allocate.h>
#include <arm64.h>
#include <operator.h>
#include <sv.h>

#undef IR_OPERATION_TYPE
#define IR_OPERATION_TYPE(t) static void generate_##t(ARM64Function *function, IROperation *op);
    IR_OPERATION_TYPES(IR_OPERATION_TYPE)
#undef IR_OPERATION_TYPE

        static void generate_code(ARM64Function *arm_function);
static void generate_native(ARM64Function *arm_function);
static void generate_function_declaration(ARM64Function *arm_function, IRFunction *function);

DECLARE_SHARED_ALLOCATOR(arm64)

void generate_native(ARM64Function *function)
{
    if (sv_first(function->function->native_name, ':') > 0) {
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
}

void generate_CALL(ARM64Function *calling_function, IROperation *op)
{
    ARM64Function *called_function = arm64context_function_by_name(calling_function->assembly->ctx, op->sv);
    assert(called_function);
    arm64function_marshall_arguments(calling_function, called_function);
    switch (called_function->function->kind) {
    case FK_SCRIBBLE: {
        arm64function_add_instruction(calling_function, "bl", "%.*s", SV_ARG(arm64function_label(called_function)));
    } break;
    case FK_NATIVE: {
        if (sv_first(called_function->function->native_name, ':') > 0) {
            arm64function_add_instruction(calling_function, "bl", "%.*s", SV_ARG(arm64function_label(called_function)));
        } else {
            arm64function_add_instruction(calling_function, "bl", "%.*s", SV_ARG(called_function->function->native_name));
        }
    } break;
    default:
        UNREACHABLE();
    }
    arm64function_marshall_return(calling_function, called_function, op->call.discard_result);
}

void generate_CASE(ARM64Function *function, IROperation *op)
{
    generate_JUMP_F(function, op);
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

void generate_END_CASE(ARM64Function *function, IROperation *op)
{
    ARM64Scope *scope = function->scribble.current_scope;
    assert(scope);
    ValueLocation *case_location = scope->match_value_stack;
    assert(case_location);
    MUST_OPTIONAL(ValueLocation, expression_value, arm64function_pop_location(function));
    arm64function_copy(function, *case_location, expression_value);
    if (op->label) {
        arm64function_add_instruction(function, "b", "%.*s_%zu",
            SV_ARG(arm64function_label(function)), op->label);
    }
}

void generate_END_MATCH(ARM64Function *function, IROperation *op)
{
    ARM64Scope *scope = function->scribble.current_scope;
    assert(scope);
    ValueLocation *match_location = scope->match_value_stack;
    assert(match_location);
    scope->match_value_stack = match_location->next;
    arm64function_push_location(function, *match_location);
}

void generate_JUMP(ARM64Function *function, IROperation *op)
{
    arm64function_add_instruction(function, "b", "%.*s_%zu",
        SV_ARG(arm64function_label(function)), op->label);
}

void generate_JUMP_F(ARM64Function *function, IROperation *op)
{
    MUST_OPTIONAL(ValueLocation, location, arm64function_pop_location(function));
    assert(location.kind == VLK_REGISTER || location.kind == VLK_IMMEDIATE);
    assert(location.type == BOOL_ID);
    ValueLocation l = location;
    if (location.kind != VLK_REGISTER) {
        l.kind = VLK_REGISTER;
        l.reg = arm64function_allocate_register(function);
        arm64function_copy(function, l, location);
    }
    arm64function_add_instruction(function, "cbz", "%s,%.*s_%zu",
        reg(l.reg),
        SV_ARG(arm64function_label(function)), op->label);
    if (location.kind != VLK_REGISTER) {
        arm64function_release_register(function, l.reg);
    }
}

void generate_JUMP_T(ARM64Function *function, IROperation *op)
{
    MUST_OPTIONAL(ValueLocation, location, arm64function_pop_location(function));
    assert(location.kind == VLK_REGISTER || location.kind == VLK_IMMEDIATE);
    assert(location.type == BOOL_ID);
    ValueLocation l = location;
    if (location.kind != VLK_REGISTER) {
        l.kind = VLK_REGISTER;
        l.reg = arm64function_allocate_register(function);
        arm64function_copy(function, l, location);
    }
    arm64function_add_instruction(function, "cbnz", "%s,%.*s_%zu",
        reg(l.reg),
        SV_ARG(arm64function_label(function)), op->label);
    if (location.kind != VLK_REGISTER) {
        arm64function_release_register(function, l.reg);
    }
}

void generate_LABEL(ARM64Function *function, IROperation *op)
{
    arm64function_add_label(function,
        sv_printf("%.*s_%zu",
            SV_ARG(arm64function_label(function)), op->label));
}

void generate_MATCH(ARM64Function *function, IROperation *op)
{
    ARM64Scope *scope = function->scribble.current_scope;
    assert(scope);

    ValueLocation *new_match_location = allocate_new(ValueLocation);
    ValueLocation  loc = arm64function_location_for_type(function, op->type);
    memcpy(new_match_location, &loc, sizeof(ValueLocation));
    new_match_location->next = scope->match_value_stack;
    scope->match_value_stack = new_match_location;
}

void generate_NEW_DATUM(ARM64Function *function, IROperation *op)
{
}

void generate_OPERATOR(ARM64Function *function, IROperation *op)
{
    ValueLocation result = arm64operator_apply(function, op->operator.lhs, op->operator.op, op->operator.rhs, NULL);
    arm64function_push_location(function, result);
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
    arm64function_push_location(
        function,
        (ValueLocation) {
            .type = BOOL_ID,
            .kind = VLK_IMMEDIATE,
            .unsigned_value = (op->bool_value) ? 1 : 0,
        });
}

void generate_PUSH_FLOAT_CONSTANT(ARM64Function *function, IROperation *op)
{
    arm64function_push_location(
        function,
        (ValueLocation) {
            .type = FLOAT_ID,
            .kind = VLK_FLOAT,
            .float_value = op->double_value,
        });
}

void generate_PUSH_INT_CONSTANT(ARM64Function *function, IROperation *op)
{
    Register reg = arm64function_allocate_register(function);

    ValueLocation immediate = {
        .type = type_registry_id_of_builtin_type(op->integer.type),
        .kind = VLK_IMMEDIATE,
    };
    if (BuiltinType_is_unsigned(typeid_builtin_type(immediate.type))) {
        immediate.unsigned_value = op->integer.value.unsigned_value;
    } else {
        immediate.signed_value = op->integer.value.signed_value;
    }
    ValueLocation result = {
        .type = immediate.type,
        .kind = VLK_REGISTER,
        .reg = reg,
    };
    arm64function_copy(function, result, immediate);
    arm64function_push_location(function, result);
}

void generate_PUSH_STRING_CONSTANT(ARM64Function *function, IROperation *op)
{
    RegisterRange regs = arm64function_allocate_register_range(function, 2);
    size_t        str_id = assembly_add_string(function->assembly, op->sv);
    arm64function_copy(function,
        (ValueLocation) {
            .type = POINTER_ID,
            .kind = VLK_REGISTER,
            .reg = regs.start,
        },
        (ValueLocation) {
            .type = POINTER_ID,
            .kind = VLK_LABEL,
            .symbol = sv_printf("str_%ld", str_id),
        });
    arm64function_copy(
        function,
        (ValueLocation) {
            .type = U64_ID,
            .kind = VLK_REGISTER,
            .reg = regs.start + 1,
        },
        (ValueLocation) {
            .type = U64_ID,
            .kind = VLK_IMMEDIATE,
            .unsigned_value = op->sv.length,
        });
    arm64function_push_registers(function, STRING_ID, regs);
}

void generate_PUSH_VAR(ARM64Function *function, IROperation *op)
{
    ARM64Variable *var = arm64function_variable_by_name(function, op->sv);
    arm64variable_load_variable(var);
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
        arm_function->scope.function = arm_function;
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
