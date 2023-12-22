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
#define IR_OPERATION_TYPE(t) static __attribute__((unused)) void generate_##t(ARM64Function *function, IROperation *op);
IR_OPERATION_TYPES(IR_OPERATION_TYPE)
#undef IR_OPERATION_TYPE

// clang-format off
static void generate_code(ARM64Function *arm_function);
// clang-format on
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

__attribute__((unused)) void generate_ASSERT(ARM64Function *function, IROperation *op)
{
    ValueLocation location = MUST_OPTIONAL(ValueLocation, arm64function_pop_location(function));
    assert(location.kind == VLK_REGISTER || location.kind == VLK_IMMEDIATE);
    assert(location.type == BOOL_ID);
    ValueLocation l = location;
    if (location.kind != VLK_REGISTER) {
        l.kind = VLK_REGISTER;
        l.reg = arm64function_allocate_register(function);
        arm64function_copy(function, l, location);
    }
    StringView lbl = sv_printf("__assert_%d_%.*s", op->index, SV_ARG(arm64function_label(function)));
    arm64function_add_instruction(function, "cbnz", "%s,%.*s", reg(l.reg), SV_ARG(lbl));
    arm64function_write_string(function, 2, sv_from("Assertion error: "));
    arm64function_write_string(function, 2, op->sv);
    arm64function_write_char(function, 2, '\n');
    arm64function_syscall1(function, SYSCALL_EXIT, 1);
    arm64function_add_label(function, lbl);
    if (location.kind != VLK_REGISTER) {
        arm64function_release_register(function, l.reg);
    } else {
        arm64function_release_register(function, location.reg);
    }
}

__attribute__((unused)) void generate_BINARY_OPERATOR(ARM64Function *function, IROperation *op)
{
    OptionalValueLocation result = arm64operator_apply(function, op->binary_operator.lhs, op->binary_operator.op, op->binary_operator.rhs, NULL);
    if (result.has_value) {
        arm64function_push_location(function, result.value);
    }
}

__attribute__((unused)) void generate_CALL(ARM64Function *calling_function, IROperation *op)
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

__attribute__((unused)) void generate_CASE(ARM64Function *function, IROperation *op)
{
    generate_LABEL(function, op);
    ValueLocation match_value = MUST_OPTIONAL(ValueLocation, arm64function_pop_location(function));
    arm64function_push_location(function, match_value);
    match_value.dont_release = true;
    arm64function_push_location(function, match_value);
}

__attribute__((unused)) void generate_CAST(ARM64Function *function, IROperation *op)
{
    ValueLocation expr = MUST_OPTIONAL(ValueLocation, arm64function_pop_location(function));
    assert(expr.kind == VLK_REGISTER);

    if (typeid_builtin_type(expr.type) == BIT_VAR_POINTER && typeid_builtin_type(op->type) == BIT_VAR_POINTER) {
        expr.type = op->type;
        arm64function_push_location(function, expr);
        return;
    }

    BuiltinType bit_cast = typeid_builtin_type(op->type);
    IntegerType sz_cast = BuiltinType_integer_type(bit_cast);
    Register    reg = arm64function_allocate_register(function);

    switch (sz_cast) {
    case I8:
        arm64function_add_instruction(function, "and", "%s,%s,#0xFF", w_reg(reg), w_reg(expr.reg));
        arm64function_add_instruction(function, "sxtb", "%s,%s", w_reg(reg), w_reg(reg));
        break;
    case U8:
        arm64function_add_instruction(function, "and", "%s,%s,#0xFF", w_reg(reg), w_reg(expr.reg));
        break;
    case I16:
        arm64function_add_instruction(function, "and", "%s,%s,#0xFFFF", w_reg(reg), w_reg(expr.reg));
        arm64function_add_instruction(function, "sxth", "%s,%s", w_reg(reg), w_reg(reg));
        break;
    case U16:
        arm64function_add_instruction(function, "and", "%s,%s,#0xFFFF", w_reg(reg), w_reg(expr.reg));
        break;
    case I32:
    case U32:
        arm64function_add_instruction(function, "mov", "%s,%s", w_reg(reg), w_reg(expr.reg));
        break;
    case I64:
        arm64function_add_instruction(function, "mov", "%s,%s", x_reg(reg), x_reg(expr.reg));
        arm64function_add_instruction(function, "sxtw", "%s,%s", x_reg(reg), x_reg(reg));
        break;
    case U64:
        arm64function_add_instruction(function, "mov", "%s,%s", x_reg(reg), x_reg(expr.reg));
        break;
    default:
        UNREACHABLE();
    }
    arm64function_push_location(
        function,
        (ValueLocation) {
            .type = op->type,
            .kind = VLK_REGISTER,
            .reg = reg,
        });
}

__attribute__((unused)) void generate_DECL_VAR(ARM64Function *, IROperation *)
{
}

__attribute__((unused)) void generate_DEFINE_AGGREGATE(ARM64Function *, IROperation *)
{
}

__attribute__((unused)) void generate_DEFINE_ALIAS(ARM64Function *, IROperation *)
{
}

__attribute__((unused)) void generate_DEFINE_ARRAY(ARM64Function *, IROperation *)
{
}

__attribute__((unused)) void generate_DEFINE_VARIANT(ARM64Function *, IROperation *)
{
}

__attribute__((unused)) void generate_END_CASE(ARM64Function *function, IROperation *op)
{
    ARM64Scope *scope = function->scribble.current_scope;
    assert(scope);
    ValueLocation *case_location = scope->match_value_stack;
    assert(case_location);
    ValueLocation expression_value = MUST_OPTIONAL(ValueLocation, arm64function_pop_location(function));
    arm64function_copy(function, *case_location, expression_value);
    if (op->label) {
        arm64function_add_instruction(function, "b", "%.*s_%zu",
            SV_ARG(arm64function_label(function)), op->label);
    }
}

__attribute__((unused)) void generate_END_MATCH(ARM64Function *function, IROperation *op)
{
    generate_LABEL(function, op);
    MUST_OPTIONAL(ValueLocation, arm64function_pop_location(function)); // pop match value
    ARM64Scope *scope = function->scribble.current_scope;
    assert(scope);
    ValueLocation *match_location = scope->match_value_stack;
    assert(match_location);
    scope->match_value_stack = match_location->next;
    arm64function_push_location(function, *match_location);
}

__attribute__((unused)) void generate_JUMP(ARM64Function *function, IROperation *op)
{
    arm64function_add_instruction(function, "b", "%.*s_%zu",
        SV_ARG(arm64function_label(function)), op->label);
}

__attribute__((unused)) void generate_JUMP_F(ARM64Function *function, IROperation *op)
{
    ValueLocation location = MUST_OPTIONAL(ValueLocation, arm64function_pop_location(function));
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
    } else {
        arm64function_release_register(function, location.reg);
    }
}

__attribute__((unused)) void generate_JUMP_T(ARM64Function *function, IROperation *op)
{
    ValueLocation location = MUST_OPTIONAL(ValueLocation, arm64function_pop_location(function));
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
    } else {
        arm64function_release_register(function, location.reg);
    }
}

__attribute__((unused)) void generate_LABEL(ARM64Function *function, IROperation *op)
{
    arm64function_add_label(function,
        sv_printf("%.*s_%zu",
            SV_ARG(arm64function_label(function)), op->label));
}

__attribute__((unused)) void generate_MATCH(ARM64Function *function, IROperation *op)
{
    ARM64Scope *scope = function->scribble.current_scope;
    assert(scope);

    ValueLocation *new_match_location = allocate_new(ValueLocation);
    ValueLocation  loc = arm64function_location_for_type(function, op->type);
    memcpy(new_match_location, &loc, sizeof(ValueLocation));
    new_match_location->next = scope->match_value_stack;
    scope->match_value_stack = new_match_location;
}

__attribute__((unused)) void generate_NEW_DATUM(ARM64Function *, IROperation *)
{
}

__attribute__((unused)) void generate_POP_VALUE(ARM64Function *function, IROperation *op)
{
    ValueLocation ptr_location = MUST_OPTIONAL(ValueLocation, arm64function_pop_location(function));
    arm64function_store_to_pointer(function, ptr_location);
}
__attribute__((unused)) void generate_PUSH_BOOL_CONSTANT(ARM64Function *function, IROperation *op)
{
    arm64function_push_location(
        function,
        (ValueLocation) {
            .type = BOOL_ID,
            .kind = VLK_IMMEDIATE,
            .integer = (Integer) {
                .type = U8,
                .u8 = (op->bool_value) ? 1 : 0,
            },
        });
}

__attribute__((unused)) void generate_PUSH_FLOAT_CONSTANT(ARM64Function *function, IROperation *op)
{
    arm64function_push_location(
        function,
        (ValueLocation) {
            .type = FLOAT_ID,
            .kind = VLK_FLOAT,
            .float_value = op->double_value,
        });
}

__attribute__((unused)) void generate_PUSH_INT_CONSTANT(ARM64Function *function, IROperation *op)
{
    Register reg = arm64function_allocate_register(function);

    ValueLocation immediate = {
        .type = type_registry_id_of_integer_type(op->integer.type),
        .kind = VLK_IMMEDIATE,
        .integer = op->integer,
    };
    ValueLocation result = {
        .type = immediate.type,
        .kind = VLK_REGISTER,
        .reg = reg,
    };
    arm64function_copy(function, result, immediate);
    arm64function_push_location(function, result);
}

__attribute__((unused)) void generate_PUSH_STRING_CONSTANT(ARM64Function *function, IROperation *op)
{
    RegisterRange regs = arm64function_allocate_register_range(function, 2);
    size_t        str_id = assembly_add_string(function->assembly, op->sv);
    arm64function_copy(function,
        (ValueLocation) {
            .type = RAW_POINTER_ID,
            .kind = VLK_REGISTER,
            .reg = regs.start,
        },
        (ValueLocation) {
            .type = RAW_POINTER_ID,
            .kind = VLK_LABEL,
            .static_data = {
                .symbol = sv_printf("str_%ld", str_id),
                .offset = 0,
            },
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
            .integer = (Integer) { .type = U64, .u64 = op->sv.length },
        });
    arm64function_push_registers(function, STRING_ID, regs);
}

__attribute__((unused)) void generate_PUSH_VALUE(ARM64Function *function, IROperation *)
{
    ValueLocation ptr_location = MUST_OPTIONAL(ValueLocation, arm64function_pop_location(function));
    arm64function_load_from_pointer(function, ptr_location);
}

__attribute__((unused)) void generate_PUSH_VAR_ADDRESS(ARM64Function *function, IROperation *op)
{
    ARM64Variable *var = arm64function_variable_by_name(function, op->sv);
    ValueLocation  var_ptr = arm64variable_reference(var);
    arm64function_push_location(function, var_ptr);
}

__attribute__((unused)) void generate_RETURN(ARM64Function *function, IROperation *)
{
    ValueLocation expr = MUST_OPTIONAL(ValueLocation, arm64function_pop_location(function));
    ValueLocation x0 = {
        .type = function->function->type.type_id,
        .kind = VLK_REGISTER,
        .reg = REG_X0,
    };
    arm64function_copy(function, x0, expr);
    arm64function_return(function);
}

__attribute__((unused)) void generate_SCOPE_BEGIN(ARM64Function *function, IROperation *op)
{
    ARM64Scope *scope = function->scribble.current_scope;
    assert(scope);
    ARM64Scope *new_scope = (scope->current) ? scope->current->next : scope->scopes;
    assert(new_scope);
    assert(new_scope->operation == op);
    scope->current = new_scope;
    function->scribble.current_scope = new_scope;
}

__attribute__((unused)) void generate_SCOPE_END(ARM64Function *function, IROperation *)
{
    ARM64Scope *scope = function->scribble.current_scope;
    assert(scope);
    scope->current = NULL;
    function->scribble.current_scope = scope->up;
}

__attribute__((unused)) void generate_SUBSCRIPT(ARM64Function *function, IROperation *op)
{
    ValueLocation reference = MUST_OPTIONAL(ValueLocation, arm64function_pop_location(function));
    for (size_t ix = 0; ix < op->var_component.size; ++ix) {
        reference = arm64function_component(function, reference, op->var_component.elements[ix]);
    }
    arm64function_push_location(function, reference);
}

__attribute__((unused)) void generate_UNARY_OPERATOR(ARM64Function *function, IROperation *op)
{
    OptionalValueLocation result = arm64operator_apply(function, op->unary_operator.operand, op->unary_operator.op, VOID_ID, NULL);
    if (result.has_value) {
        arm64function_push_location(function, result.value);
    }
}

__attribute__((unused)) void generate_WHEN(ARM64Function *function, IROperation *op)
{
    ValueLocation result = MUST_OPTIONAL(ValueLocation, arm64operator_apply(function, BOOL_ID, OP_EQUALS, BOOL_ID, NULL));
    arm64function_push_location(function, result);
    generate_JUMP_F(function, op);
}

void generate_code(ARM64Function *arm_function)
{
    IRFunction *function = arm_function->function;
    assert(function->kind == FK_SCRIBBLE);
    trace(CAT_COMPILE, "Generating code for %.*s", SV_ARG(function->name));
    if (!debug_execution_observer(NULL, (ExecutionMessage) { .type = EMT_FUNCTION_ENTRY, .payload = function })) {
        return;
    }
    arm_function->scribble.current_scope = &arm_function->scope;
    arm64function_enter(arm_function);
    for (size_t ix = 0; ix < function->operations.size; ++ix) {
        IROperation *op = function->operations.elements + ix;
        if (!debug_execution_observer(NULL, (ExecutionMessage) {
                                                .type = EMT_ON_INSTRUCTION,
                                                .payload = op,
                                            })) {
            return;
        }
        StringView op_str = ir_operation_to_string(op);
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
        arm64function_add_text(arm_function, "\n");
        if (!debug_execution_observer(NULL, (ExecutionMessage) {
                                                .type = EMT_AFTER_INSTRUCTION,
                                                .payload = op,
                                            })) {
            return;
        }
    }
    debug_execution_observer(NULL, (ExecutionMessage) {
                                       .type = EMT_FUNCTION_RETURN,
                                   });
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
        function->data = arm_function;
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

    if (!debug_execution_observer(ctx, (ExecutionMessage) { .type = EMT_PROGRAM_START, .payload = program })) {
        return NULL;
    }

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
    debug_execution_observer(&ctx, (ExecutionMessage) {
                                       .type = EMT_PROGRAM_EXIT,
                                   });
    return ctx;
}
