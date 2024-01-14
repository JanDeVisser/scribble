/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <log.h>
#include <model/op.h>

char *Operator_name(Operator op)
{
    switch (op) {
#undef ENUM_BINARY_OPERATOR
#define ENUM_BINARY_OPERATOR(O, a, p, k, c, cl) \
    case OP_##O:                                \
        return #O;
        BINARY_OPERATORS(ENUM_BINARY_OPERATOR)
#undef ENUM_BINARY_OPERATOR
#undef ENUM_UNARY_OPERATOR
#define ENUM_UNARY_OPERATOR(O, k, c) \
    case OP_##O:                     \
        return #O;
        UNARY_OPERATORS(ENUM_UNARY_OPERATOR)
#undef ENUM_UNARY_OPERATOR
    default:
        UNREACHABLE();
    }
}

Operator Operator_from_string(StringView op)
{
#undef ENUM_BINARY_OPERATOR
#define ENUM_BINARY_OPERATOR(O, a, p, k, c, cl) \
    if (sv_eq_ignore_case_cstr(op, #O))         \
        return OP_##O;
    BINARY_OPERATORS(ENUM_BINARY_OPERATOR)
#undef ENUM_BINARY_OPERATOR
#undef ENUM_UNARY_OPERATOR
#define ENUM_UNARY_OPERATOR(O, K, C)    \
    if (sv_eq_ignore_case_cstr(op, #O)) \
        return OP_##O;
    UNARY_OPERATORS(ENUM_UNARY_OPERATOR)
#undef ENUM_UNARY_OPERATOR
    fatal("Invalid operator '%.*s'", SV_ARG(op));
}
