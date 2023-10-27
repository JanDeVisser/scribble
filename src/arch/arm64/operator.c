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

char const *opcode_for_op_by_type(Operator op, bool un_signed, bool is_short)
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

ValueLocation arm64_apply_string_op(ARM64Function *function, IROperation *op)
{
    assert(op->operator.lhs == STRING_ID);
    MUST_OPTIONAL(ValueLocation, rhs, arm64function_pop_location(function));
    MUST_OPTIONAL(ValueLocation, lhs, arm64function_pop_location(function));

    switch (op->operator.op) {
    case OP_ADD: {
        //        StringView concat(StringView s1, StringView s2)
        //        {
        //            StringBuilder sb = make_sb();
        //            sb_append(&sb, s1);
        //            sb_append(&sb, s2);
        //            return sb.view;
        //        }
        //
        //        stp     x0, x1, [sp, 32] ; lhs
        //        stp     x2, x3, [sp, 16] ; rhs
        //        bl      sb_create
        //        stp     x0, x1, [sp, 48] ; Store new stringbuilder on stack
        //        add     x0, sp, 48       ; Get pointer to new stringbuilder
        //        ldp     x1, x2, [sp, 32] ; Get lhs
        //        bl      sb_append        ; Copy lhs into stringbuilder
        //        add     x0, sp, 48       ; Get pointer to new stringbuilder
        //        ldp     x1, x2, [sp, 16] ; Get rhs
        //        bl      sb_append        ; Copy rhs into stringbuilder
        //        ldp     x0, x1, [sp, 48] ; Return stringbuilder
        arm64function_copy(
            function,
            (ValueLocation) {
                .type = STRING_ID,
                .kind = VLK_STACK,
            },
            lhs);
        arm64function_copy(
            function,
            (ValueLocation) {
                .type = STRING_ID,
                .kind = VLK_STACK,
            },
            rhs);
        arm64function_add_instruction(function, "bl", "_sb_create");
        arm64function_copy(
            function,
            (ValueLocation) {
                .type = STRING_ID,
                .kind = VLK_STACK,
            },
            (ValueLocation) {
                .type = STRING_ID,
                .kind = VLK_REGISTER_RANGE,
                .range = {
                    .start = REG_X0,
                    .end = REG_X1 + 1,
                },
            });

        arm64function_copy(
            function,
            (ValueLocation) {
                .type = POINTER_ID,
                .kind = VLK_REGISTER,
                .reg = REG_X0,
            },
            (ValueLocation) {
                .type = POINTER_ID,
                .kind = VLK_REGISTER,
                .reg = REG_SP,
            });
        arm64function_copy(
            function,
            (ValueLocation) {
                .type = STRING_ID,
                .kind = VLK_REGISTER_RANGE,
                .range = {
                    .start = REG_X1,
                    .end = REG_X2 + 1,
                } },
            (ValueLocation) {
                .type = STRING_ID,
                .kind = VLK_POINTER,
                .pointer = {
                    .reg = REG_SP,
                    .offset = 32,
                },
            });
        arm64function_add_instruction(function, "bl", "_sb_append_sv");

        arm64function_copy(
            function,
            (ValueLocation) {
                .type = POINTER_ID,
                .kind = VLK_REGISTER,
                .reg = REG_X0,
            },
            (ValueLocation) {
                .type = POINTER_ID,
                .kind = VLK_REGISTER,
                .reg = REG_SP,
            });
        arm64function_copy(
            function,
            (ValueLocation) {
                .type = STRING_ID,
                .kind = VLK_REGISTER_RANGE,
                .range = {
                    .start = REG_X1,
                    .end = REG_X2 + 1,
                } },
            (ValueLocation) {
                .type = STRING_ID,
                .kind = VLK_POINTER,
                .pointer = {
                    .reg = REG_SP,
                    .offset = 16,
                },
            });
        arm64function_add_instruction(function, "bl", "_sb_append_sv");
        arm64function_copy(
            function,
            (ValueLocation) {
                .type = STRING_ID,
                .kind = VLK_REGISTER_RANGE,
                .range = {
                    .start = REG_X0,
                    .end = REG_X1 + 1,
                } },
            (ValueLocation) {
                .type = STRING_ID,
                .kind = VLK_STACK,
            });
        arm64function_copy(
            function,
            (ValueLocation) {
                .type = STRING_ID,
                .kind = VLK_DISCARD,
            },
            (ValueLocation) {
                .type = STRING_ID,
                .kind = VLK_STACK,
            });
        arm64function_copy(
            function,
            (ValueLocation) {
                .type = STRING_ID,
                .kind = VLK_DISCARD,
            },
            (ValueLocation) {
                .type = STRING_ID,
                .kind = VLK_STACK,
            });
        return (ValueLocation) {
            .type = STRING_ID,
            .kind = VLK_REGISTER_RANGE,
            .range = {
                .start = REG_X0,
                .end = REG_X1 + 1,
            }
        };
    }
    default:
        NYI("codegen for operator `%s` on strings", Operator_name(op->operator.op));
    }
}

ValueLocation arm64_apply_op(ARM64Function *function, IROperation *op)
{
    if (op->operator.lhs == STRING_ID) {
        return arm64_apply_string_op(function, op);
    }

    MUST_OPTIONAL(ValueLocation, rhs, arm64function_pop_location(function));
    MUST_OPTIONAL(ValueLocation, lhs, arm64function_pop_location(function));
    assert(lhs.kind == VLK_REGISTER);
    assert(rhs.kind == VLK_REGISTER);
    Register      result = arm64function_allocate_register(function);
    size_t        sz = typeid_sizeof(lhs.type);
    RegisterWidth width = (sz > 8) ? RW_64 : RW_32;
    bool          is_short = (sz < 4);
    bool          un_signed = BuiltinType_is_unsigned(typeid_builtin_type(lhs.type));
    char const   *l = reg_with_width(lhs.reg, width);
    char const   *r = reg_with_width(rhs.reg, width);
    char const   *res = reg_with_width(result, width);
    char const   *conditional = conditional_for_op_by_type(op->operator.op, lhs.type);

    arm64function_release_register(function, lhs.reg);
    arm64function_release_register(function, rhs.reg);
    if (lhs.type == BOOL_ID && op->operator.op == OP_MULTIPLY) {
        arm64function_add_instruction(function, "and", "%s,%s,%s", w_reg(result), w_reg(lhs.reg), w_reg(rhs.reg));
        arm64function_add_instruction(function, "cmp", "%s,wzr", w_reg(result));
        arm64function_add_instruction(function, "cset", "%s,ne", w_reg(result));
        arm64function_add_instruction(function, "and", "%s,%s,#0xFF", res, res);
        return (ValueLocation) {
            .type = lhs.type,
            .kind = VLK_REGISTER,
            .reg = result,
        };
    }
    if (lhs.type == BOOL_ID && op->operator.op == OP_LOGICAL_AND) {
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
        return (ValueLocation) {
            .type = lhs.type,
            .kind = VLK_REGISTER,
            .reg = result,
        };
    }
    if (lhs.type == BOOL_ID && op->operator.op == OP_LOGICAL_OR) {
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
        return (ValueLocation) {
            .type = lhs.type,
            .kind = VLK_REGISTER,
            .reg = result,
        };
    }
    char const *opcode = opcode_for_op_by_type(op->operator.op, un_signed, is_short);
    if (opcode) {
        arm64function_add_instruction(function, opcode, "%s,%s,%s", res, l, r);
        if (conditional) {
            arm64function_add_instruction(function, "cset", "%s,%s", res, conditional);
            arm64function_add_instruction(function, "and", "%s,%s,#0xFF", res, res);
        }
    } else {
        switch (op->operator.op) {
        case OP_MODULO: {
            opcode = opcode_for_op_by_type(OP_DIVIDE, un_signed, is_short);
            arm64function_add_instruction(function, opcode, "%s,%s,%s", res, l, r);
            arm64function_add_instruction(function, "mul", "%s,%s,%s", res, res, r);
            arm64function_add_instruction(function, "sub", "%s,%s,%s", res, l, res);
        } break;
        default:
            NYI("codegen for operator `%s`", Operator_name(op->operator.op));
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
    return (ValueLocation) {
        .type = lhs.type,
        .kind = VLK_REGISTER,
        .reg = result,
    };
}
