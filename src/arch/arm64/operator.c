/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <arm64.h>
#include <operator.h>

typedef struct operator_opcode_mapping {
    Operator    op;
    char const *opcode;
    char const *opcode_signed;
    char const *opcode_unsigned;
    char const *opcode_l_signed;
    char const *opcode_l_unsigned;
    char const *opcode_s_signed;
    char const *opcode_s_unsigned;
} OperatorOpcodeMapping;

static OperatorOpcodeMapping arm64_operator_opcode_mappings[] = {
    { OP_ADD, "add" },
    { OP_SUBTRACT, "sub" },
    { OP_MULTIPLY, "mul" },
    { OP_DIVIDE, NULL, "sdiv", "udiv" },
    { OP_MODULO, NULL },
    { OP_BITWISE_AND, "and" },
    { OP_BITWISE_OR, "orr" },
    { OP_BITWISE_XOR, "eor" },
    { OP_BIT_SHIFT_LEFT, "lsl" },
    { OP_BIT_SHIFT_RIGHT, NULL, NULL, NULL, "asr", "lsr", "asr", "asr" },
    { OP_EQUALS, "cmp" },
    { OP_NOT_EQUALS, "cmp" },
    { OP_LESS, "cmp" },
    { OP_LESS_EQUALS, "cmp" },
    { OP_GREATER, "cmp" },
    { OP_GREATER_EQUALS, "cmp" },
    { OP_COUNT, NULL },
};

typedef struct operator_cset_conditional_mapping {
    Operator    op;
    char const *cond_signed;
    char const *cond_unsigned;
} OperatorCSetConditionalMapping;

static OperatorCSetConditionalMapping arm64_operator_cset_conditional_mappings[] = {
    { OP_EQUALS, "eq", "eq" },
    { OP_NOT_EQUALS, "ne", "ne" },
    { OP_LESS, "lt", "cc" },
    { OP_LESS_EQUALS, "le", "ls" },
    { OP_GREATER, "gt", "hi" },
    { OP_GREATER_EQUALS, "ge", "cs" },
    { OP_COUNT, NULL },
};

ValueLocation arm64operator_apply_binary(ARM64Function *function, type_id lhs_type, Operator op, type_id rhs_type);
ValueLocation arm64operator_apply_unary(ARM64Function *function, Operator op, type_id operand_type);

char const *opcode_for_binary_op_by_type(Operator op, bool un_signed, bool is_short)
{
    char const *opcode;
    for (size_t ix = 0; arm64_operator_opcode_mappings[ix].op != OP_COUNT; ++ix) {
        OperatorOpcodeMapping *m = arm64_operator_opcode_mappings + ix;
        if (m->op == op) {
            opcode = (un_signed)
                ? ((is_short) ? m->opcode_s_unsigned : m->opcode_l_unsigned)
                : ((is_short) ? m->opcode_s_signed : m->opcode_l_signed);
            if (!opcode) {
                opcode = (un_signed)
                    ? m->opcode_unsigned
                    : m->opcode_signed;
            }
            if (!opcode) {
                opcode = m->opcode;
            }
            return opcode;
        }
    }
    return NULL;
}

ValueLocation call_string_function(ARM64Function *function, char const *string_function, ValueLocation lhs, ValueLocation rhs, type_id result_type)
{
    assert(lhs.type == STRING_ID);
    assert(rhs.type == STRING_ID);
    arm64function_copy(
        function,
        (ValueLocation) {
            .type = STRING_ID,
            .kind = VLK_REGISTER_RANGE,
            .range = {
                .start = REG_X0,
                .end = REG_X1 + 1,
            } },
        lhs);
    if (rhs.type == STRING_ID) {
        arm64function_copy(
            function,
            (ValueLocation) {
                .type = STRING_ID,
                .kind = VLK_REGISTER_RANGE,
                .range = {
                    .start = REG_X2,
                    .end = REG_X2 + 1,
                } },
            rhs);
    } else {
        assert(rhs.type == I32_ID);
        arm64function_copy(
            function,
            (ValueLocation) {
                .type = rhs.type,
                .kind = VLK_REGISTER,
                .reg = REG_X2,
            },
            rhs);
    }
    ValueLocation return_value = arm64function_call(function, sv_from(string_function), result_type);
    ValueLocation result_location = arm64function_location_for_type(function, result_type);
    arm64function_copy(function, result_location, return_value);
    return result_location;
}

ValueLocation arm64_apply_string_op(ARM64Function *function, type_id lhs_type, Operator op, type_id rhs_type, ValueLocation *result)
{
    assert(lhs_type == STRING_ID);
    assert(!result);
    ValueLocation rhs = { 0 };
    if (rhs_type != VOID_ID) {
        ValueLocation rhs_temp = MUST_OPTIONAL(ValueLocation, arm64function_pop_location(function));
        rhs = rhs_temp;
    }
    ValueLocation lhs = MUST_OPTIONAL(ValueLocation, arm64function_pop_location(function));

    switch (op) {
    case OP_ADD:
        return call_string_function(function, "string_concat", lhs, rhs, STRING_ID);
    case OP_MULTIPLY:
        return call_string_function(function, "sv_replicate", lhs, rhs, STRING_ID);
    case OP_EQUALS:
        return call_string_function(function, "string_eq", lhs, rhs, BOOL_ID);
    case OP_NOT_EQUALS: {
        ValueLocation eq = call_string_function(function, "string_eq", lhs, rhs, BOOL_ID);
        assert(eq.kind == VLK_REGISTER);
        arm64function_add_instruction(function, "mov", "x0,#1");
        arm64function_add_instruction(function, "sub", "%s,x0,%s", w_reg(eq.reg), w_reg(eq.reg));
        return eq;
    }
    case OP_GREATER: {
        ValueLocation cmp = call_string_function(function, "string_cmp", lhs, rhs, I32_ID);
        assert(cmp.kind == VLK_REGISTER);
        arm64function_add_instruction(function, "cmp", "%s,wzr", w_reg(cmp.reg));
        arm64function_add_instruction(function, "cset", "%s,gt", w_reg(cmp.reg));
        return cmp;
    }
    case OP_GREATER_EQUALS: {
        ValueLocation cmp = call_string_function(function, "string_cmp", lhs, rhs, I32_ID);
        assert(cmp.kind == VLK_REGISTER);
        arm64function_add_instruction(function, "cmp", "%s,wzr", w_reg(cmp.reg));
        arm64function_add_instruction(function, "cset", "%s,ge", w_reg(cmp.reg));
        return cmp;
    }
    case OP_LESS: {
        ValueLocation cmp = call_string_function(function, "string_cmp", lhs, rhs, I32_ID);
        assert(cmp.kind == VLK_REGISTER);
        arm64function_add_instruction(function, "cmp", "%s,wzr", w_reg(cmp.reg));
        arm64function_add_instruction(function, "cset", "%s,lt", w_reg(cmp.reg));
        return cmp;
    }
    case OP_LESS_EQUALS: {
        ValueLocation cmp = call_string_function(function, "string_cmp", lhs, rhs, I32_ID);
        assert(cmp.kind == VLK_REGISTER);
        arm64function_add_instruction(function, "cmp", "%s,wzr", w_reg(cmp.reg));
        arm64function_add_instruction(function, "cset", "%s,le", w_reg(cmp.reg));
        return cmp;
    }
    case OP_CARDINALITY: {
        Register      reg = arm64function_allocate_register(function);
        ValueLocation loc = {
            .type = U64_ID,
            .kind = VLK_REGISTER,
            .reg = reg,
        };
        arm64function_copy(
            function, loc,
            (ValueLocation) {
                .type = U64_ID,
                .kind = VLK_REGISTER,
                .reg = lhs.range.start + 1,
            });
        return loc;
    }
    default:
        NYI("codegen for operator `%s` on strings", Operator_name(op));
    }
}

ValueLocation arm64_apply_array_op(ARM64Function *function, type_id lhs_type, Operator op, type_id rhs_type, ValueLocation *result)
{
    assert(typeid_specializes(lhs_type) == ARRAY_ID);
    assert(!result);
    ValueLocation rhs = { 0 };
    if (rhs_type != VOID_ID) {
        ValueLocation rhs_temp = MUST_OPTIONAL(ValueLocation, arm64function_pop_location(function));
        rhs = rhs_temp;
    }
    ValueLocation lhs = MUST_OPTIONAL(ValueLocation, arm64function_pop_location(function));
    if (rhs_type != VOID_ID) {
        trace(CAT_COMPILE, "%.*s %s %.*s",
            SV_ARG(value_location_to_string(lhs)),
            Operator_name(op),
            SV_ARG(value_location_to_string(rhs)));
        arm64function_add_comment(function, "%.*s %s %.*s",
            SV_ARG(value_location_to_string(lhs)),
            Operator_name(op),
            SV_ARG(value_location_to_string(rhs)));
    } else {
        trace(CAT_COMPILE, "%s %.*s",
            Operator_name(op),
            SV_ARG(value_location_to_string(lhs)));
        arm64function_add_comment(function, "%s %.*s",
            Operator_name(op),
            SV_ARG(value_location_to_string(lhs)));
    }

    switch (op) {
    case OP_SUBSCRIPT: {
        assert(lhs.kind == VLK_REGISTER_RANGE);
        assert(rhs.kind == VLK_REGISTER);

        ExpressionType *array_type = type_registry_get_type_by_id(lhs_type);
        type_id         ptr_type_id = array_type->components.components[0].type_id;
        ExpressionType *ptr_type = type_registry_get_type_by_id(ptr_type_id);
        Register        ptr_reg = lhs.range.start;
        ValueLocation   ptr_location = {
              .type = ptr_type_id,
              .kind = VLK_REGISTER,
              .reg = ptr_reg,
        };
        arm64function_push_location(function, ptr_location);
        arm64function_push_location(function, rhs);
        ptr_location = arm64operator_apply(function, ptr_type_id, OP_ADD, rhs_type, &ptr_location);
        assert(ptr_location.kind == VLK_REGISTER);
        type_id       elem_type_id = ptr_type->template_arguments[0].type;
        ValueLocation subscript_ptr = {
            .kind = VLK_POINTER,
            .type = elem_type_id,
            .pointer = {
                .reg = ptr_location.reg,
                .offset = 0,
            },
        };
        ptr_location.type = elem_type_id;
        ValueLocation elem_location = arm64function_location_for_type(function, elem_type_id);
        arm64function_copy(
            function,
            elem_location,
            subscript_ptr);
        arm64function_release_register(function, ptr_reg);
        arm64function_release_register(function, lhs.range.start + 1);
        arm64function_release_register(function, rhs.reg);
        return elem_location;
    }
    case OP_CARDINALITY: {
        Register      reg = arm64function_allocate_register(function);
        ValueLocation loc = {
            .type = U64_ID,
            .kind = VLK_REGISTER,
            .reg = reg,
        };
        arm64function_copy(
            function, loc,
            (ValueLocation) {
                .type = U64_ID,
                .kind = VLK_REGISTER,
                .reg = lhs.range.start + 1,
            });
        return loc;
    }
    default:
        UNREACHABLE();
    }
}

ValueLocation copy_location_if_result_set(ARM64Function *function, ValueLocation loc, ValueLocation *result)
{
    if (result) {
        arm64function_copy(function, *result, loc);
        return *result;
    }
    return loc;
}

char const *conditional_for_op_by_type(Operator op, type_id type)
{
    char const *conditional = NULL;
    for (size_t ix = 0; arm64_operator_cset_conditional_mappings[ix].op != OP_COUNT; ++ix) {
        if (arm64_operator_cset_conditional_mappings[ix].op == op) {
            conditional = (BuiltinType_is_unsigned(typeid_builtin_type(type)))
                ? arm64_operator_cset_conditional_mappings[ix].cond_unsigned
                : arm64_operator_cset_conditional_mappings[ix].cond_signed;
            break;
        }
    }
    return conditional;
}

ValueLocation create_range(ARM64Function *function)
{
    ValueLocation rhs = MUST_OPTIONAL(ValueLocation, arm64function_pop_location(function));
    ValueLocation lhs = MUST_OPTIONAL(ValueLocation, arm64function_pop_location(function));
    assert(lhs.type == rhs.type);
    assert(BuiltinType_is_integer(typeid_builtin_type(lhs.type)));
    type_id range_id = MUST(
        TypeID,
        type_specialize_template(
            RANGE_ID,
            1,
            (TemplateArgument[]) {
                {
                    .name = sv_from("T"),
                    .arg_type = TPT_TYPE,
                    .type = lhs.type,
                },
            }));
    ValueLocation range = arm64function_allocate_space(function, range_id);
    ValueLocation component_loc = {
        .kind = VLK_POINTER,
        .type = lhs.type,
        .pointer = {
            .reg = REG_SP,
            .offset = (int64_t) typeid_offsetof(range_id, 0),
        }
    };
    arm64function_copy(function,
        component_loc,
        lhs);
    component_loc.pointer.offset = (int64_t) typeid_offsetof(range_id, 1);
    arm64function_copy(function,
        component_loc,
        rhs);
    return range;
}

ValueLocation arm64operator_apply(ARM64Function *function, type_id lhs_type, Operator op, type_id rhs_type, ValueLocation *result)
{
    if (rhs_type != VOID_ID) {
        trace(CAT_COMPILE, "Generating code for %.*s %s %.*s",
            SV_ARG(typeid_name(lhs_type)), Operator_name(op), SV_ARG(typeid_name(rhs_type)));
        arm64function_add_comment(function, "Generating code for %.*s %s %.*s",
            SV_ARG(typeid_name(lhs_type)), Operator_name(op), SV_ARG(typeid_name(rhs_type)));
    } else {
        trace(CAT_COMPILE, "Generating code for %s %.*s",
            Operator_name(op), SV_ARG(typeid_name(lhs_type)));
        arm64function_add_comment(function, "Generating code for %s %.*s",
            Operator_name(op), SV_ARG(typeid_name(lhs_type)));
    }
    if (lhs_type == STRING_ID) {
        return arm64_apply_string_op(function, lhs_type, op, rhs_type, result);
    }
    if (typeid_specializes(lhs_type) == ARRAY_ID) {
        return arm64_apply_array_op(function, lhs_type, op, rhs_type, result);
    }
    if (op == OP_RANGE) {
        return create_range(function);
    }
    return (rhs_type != VOID_ID)
        ? arm64operator_apply_binary(function, lhs_type, op, rhs_type)
        : arm64operator_apply_unary(function, op, lhs_type);
}

ValueLocation arm64operator_apply_binary(ARM64Function *function, type_id lhs_type, Operator op, type_id rhs_type)
{
    ValueLocation rhs = MUST_OPTIONAL(ValueLocation, arm64function_pop_location(function));
    ValueLocation lhs = MUST_OPTIONAL(ValueLocation, arm64function_pop_location(function));
    trace(CAT_COMPILE, "%.*s %s %.*s",
        SV_ARG(value_location_to_string(lhs)),
        Operator_name(op),
        SV_ARG(value_location_to_string(rhs)));
    arm64function_add_comment(function, "%.*s %s %.*s",
        SV_ARG(value_location_to_string(lhs)),
        Operator_name(op),
        SV_ARG(value_location_to_string(rhs)));
    assert(lhs.kind == VLK_REGISTER);
    assert(rhs.kind == VLK_REGISTER);
    assert(lhs.type == lhs_type);
    assert(rhs.type == rhs_type);
    Register      result_reg = arm64function_allocate_register(function);
    size_t        sz = typeid_sizeof(lhs.type);
    RegisterWidth width = (sz > 8) ? RW_64 : RW_32;
    bool          is_short = (sz < 4);
    bool          un_signed = BuiltinType_is_unsigned(typeid_builtin_type(lhs.type));
    char const   *l = reg_with_width(lhs.reg, width);
    char const   *r = reg_with_width(rhs.reg, width);
    char const   *res = reg_with_width(result_reg, width);
    char const   *conditional = conditional_for_op_by_type(op, lhs.type);

    if (lhs.type == BOOL_ID && op == OP_MULTIPLY) {
        arm64function_add_instruction(function, "and", "%s,%s,%s", w_reg(result_reg), w_reg(lhs.reg), w_reg(rhs.reg));
        arm64function_add_instruction(function, "cmp", "%s,wzr", w_reg(result_reg));
        arm64function_add_instruction(function, "cset", "%s,ne", w_reg(result_reg));
        arm64function_add_instruction(function, "and", "%s,%s,#0xFF", res, res);
        arm64function_release_register(function, lhs.reg);
        arm64function_release_register(function, rhs.reg);
        return (ValueLocation) {
            .type = lhs.type,
            .kind = VLK_REGISTER,
            .reg = result_reg,
        };
    }
    if (lhs.type == BOOL_ID && op == OP_LOGICAL_AND) {
        //        and     w0, w0, 1
        //        cmp     w0, 0
        //        beq     .L296
        //        ldrb    w0, [sp, 14]
        //        and     w0, w0, 1
        //        cmp     w0, 0
        //        beq     .L296
        //        mov     w0, 1
        //        b       .L297
        // .L296:
        //        mov     w0, 0
        // .L297:
        //        and     w0, w0, 255
        arm64function_add_instruction(function, "and", "%s,%s,#1", l, l);
        arm64function_add_instruction(function, "cmp", "%s,wzr", l);
        size_t ret_false = label_reserve_id();
        arm64function_add_instruction(function, "b.eq", "lbl_%zu", ret_false);
        arm64function_add_instruction(function, "and", "%s,%s,#1", r, r);
        arm64function_add_instruction(function, "cmp", "%s,wzr", r);
        arm64function_add_instruction(function, "b.eq", "lbl_%zu", ret_false);
        arm64function_add_instruction(function, "mov", "%s,1", res);
        size_t done = label_reserve_id();
        arm64function_add_instruction(function, "b", "lbl_%zu", done);
        arm64function_add_label(function, sv_printf("lbl_%zu", ret_false));
        arm64function_add_instruction(function, "mov", "%s,wzr", res);
        arm64function_add_label(function, sv_printf("lbl_%zu", done));
        arm64function_add_instruction(function, "and", "%s,%s,#0xFF", res, res);
        arm64function_release_register(function, lhs.reg);
        arm64function_release_register(function, rhs.reg);
        return (ValueLocation) {
            .type = lhs.type,
            .kind = VLK_REGISTER,
            .reg = result_reg,
        };
    }
    if (lhs.type == BOOL_ID && op == OP_LOGICAL_OR) {
        //        and     w0, w0, 1
        //        cmp     w0, 0
        //        bne     .L300
        //        ldrb    w0, [sp, 14]
        //        and     w0, w0, 1
        //        cmp     w0, 0
        //        beq     .L301
        //.L300:
        //        mov     w0, 1
        //        b       .L302
        //.L301:
        //        mov     w0, 0
        //.L302:
        //        and     w0, w0, 255
        arm64function_add_instruction(function, "and", "%s,%s,#1", l, l);
        arm64function_add_instruction(function, "cmp", "%s,wzr", l);
        size_t ret_true = label_reserve_id();
        arm64function_add_instruction(function, "b.ne", "lbl_%zu", ret_true);
        arm64function_add_instruction(function, "and", "%s,%s,#1", r, r);
        arm64function_add_instruction(function, "cmp", "%s,wzr", r);
        size_t ret_false = label_reserve_id();
        arm64function_add_instruction(function, "b.eq", "lbl_%zu", ret_false);
        arm64function_add_label(function, sv_printf("lbl_%zu", ret_true));
        arm64function_add_instruction(function, "mov", "%s,1", res);
        size_t done = label_reserve_id();
        arm64function_add_instruction(function, "b", "lbl_%zu", done);
        arm64function_add_label(function, sv_printf("lbl_%zu", ret_false));
        arm64function_add_instruction(function, "mov", "%s,wzr", res);
        arm64function_add_label(function, sv_printf("lbl_%zu", done));
        arm64function_add_instruction(function, "and", "%s,%s,#0xFF", res, res);
        arm64function_release_register(function, lhs.reg);
        arm64function_release_register(function, rhs.reg);
        return (ValueLocation) {
            .type = lhs.type,
            .kind = VLK_REGISTER,
            .reg = result_reg,
        };
    }
    if (typeid_kind(lhs.type) == TK_PRIMITIVE && typeid_builtin_type(lhs.type) == BIT_VAR_POINTER && op == OP_ADD) {
        size_t elem_sz = 1;
        if (lhs.type != VAR_POINTER_ID) {
            ExpressionType   *et = type_registry_get_type_by_id(lhs.type);
            TemplateArgument *arg = type_get_argument(et, sv_from("T"));
            assert(arg->arg_type == TPT_TYPE);
            elem_sz = typeid_sizeof(arg->type);
        }
        Register sz_reg = arm64function_allocate_register(function);
        arm64function_add_instruction(function, "mov", "%s,#0x%04x", x_reg(sz_reg), elem_sz);
        arm64function_add_instruction(function, "madd", "%s,%s,%s,%s",
            x_reg(result_reg), x_reg(rhs.reg), x_reg(sz_reg), x_reg(lhs.reg));
        arm64function_release_register(function, sz_reg);
        arm64function_release_register(function, lhs.reg);
        arm64function_release_register(function, rhs.reg);
        return (ValueLocation) {
            .type = lhs.type,
            .kind = VLK_REGISTER,
            .reg = result_reg,
        };
    }
    char const *opcode = opcode_for_binary_op_by_type(op, un_signed, is_short);
    if (opcode) {
        if (conditional) {
            arm64function_add_instruction(function, opcode, "%s,%s", l, r);
            arm64function_add_instruction(function, "cset", "%s,%s", res, conditional);
            arm64function_add_instruction(function, "and", "%s,%s,#0xFF", res, res);
        } else {
            arm64function_add_instruction(function, opcode, "%s,%s,%s", res, l, r);
        }
    } else {
        switch (op) {
        case OP_MODULO: {
            opcode = opcode_for_binary_op_by_type(OP_DIVIDE, un_signed, is_short);
            arm64function_add_instruction(function, opcode, "%s,%s,%s", res, l, r);
            arm64function_add_instruction(function, "mul", "%s,%s,%s", res, res, r);
            arm64function_add_instruction(function, "sub", "%s,%s,%s", res, l, res);
        } break;
        default:
            NYI("codegen for operator `%s`", Operator_name(op));
        }
    }

    if (!conditional) {
        if (lhs.type == BOOL_ID) {
            arm64function_add_instruction(function, "cmp", "%s,wzr", res);
            arm64function_add_instruction(function, "cset", "%s,ne", res);
            arm64function_add_instruction(function, "and", "%s,%s,#0xFF", res, res);
        } else if (lhs.type == I16_ID) {
            arm64function_add_instruction(function, "and", "%s,%s,#0xFFFF", res, res);
            arm64function_add_instruction(function, "sxth", "%s,%s", res, res);
        } else if (lhs.type == U16_ID) {
            arm64function_add_instruction(function, "and", "%s,%s,#0xFFFF", res, res);
        } else if (lhs.type == I8_ID) {
            arm64function_add_instruction(function, "and", "%s,%s,#0xFF", res, res);
            arm64function_add_instruction(function, "sxtb", "%s,%s", res, res);
        } else if (lhs.type == U8_ID) {
            arm64function_add_instruction(function, "and", "%s,%s,#0xFF", res, res);
        }
    }
    arm64function_release_register(function, lhs.reg);
    arm64function_release_register(function, rhs.reg);
    return (ValueLocation) {
        .type = (conditional) ? BOOL_ID : lhs.type,
        .kind = VLK_REGISTER,
        .reg = result_reg,
    };
}

ValueLocation arm64operator_apply_unary(ARM64Function *function, Operator op, type_id operand_type)
{
    ValueLocation operand = MUST_OPTIONAL(ValueLocation, arm64function_pop_location(function));
    trace(CAT_COMPILE, "%s %.*s",
        Operator_name(op),
        SV_ARG(value_location_to_string(operand)));
    arm64function_add_comment(function, "%s %.*s",
        Operator_name(op),
        SV_ARG(value_location_to_string(operand)));
    assert(operand.kind == VLK_REGISTER);
    assert(operand.type == operand_type);

    Register      result_reg = arm64function_allocate_register(function);
    size_t        sz = typeid_sizeof(operand.type);
    RegisterWidth width = (sz > 8) ? RW_64 : RW_32;
    bool          is_short = (sz < 4);
    bool          un_signed = BuiltinType_is_unsigned(typeid_builtin_type(operand_type));
    char const   *reg = reg_with_width(operand.reg, width);
    char const   *res = reg_with_width(result_reg, width);

    switch (op) {
    case OP_ADDRESS_OF:

    case OP_IDENTITY:
        return operand;
    case OP_NEGATE:
        arm64function_release_register(function, result_reg);
        arm64function_add_instruction(function, "neg", "%s,%s", res, reg);
        break;
    case OP_LOGICAL_INVERT:
    case OP_BITWISE_INVERT:
        arm64function_add_instruction(function, "mvn", "%s,%s", res, reg);
        break;
    default:
        UNREACHABLE();
    }
    arm64function_release_register(function, operand.reg);
    return (ValueLocation) {
        .type = operand_type,
        .kind = VLK_REGISTER,
        .reg = result_reg,
    };
}
