/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <datum.h>

char const *DatumType_name(DatumType dt)
{
    switch (dt) {
#undef DATUMTYPE
#define DATUMTYPE(dt, n, ct) \
    case DT_##dt:            \
        return #dt;
        DATUMTYPES(DATUMTYPE)
#undef DATUMTYPE
    default:
        UNREACHABLE();
    }
}

bool DatumType_is_integer(DatumType dt)
{
    switch (dt) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) case DT_##dt:
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
        return true;
    default:
        return false;
    }
}

bool DatumType_is_number(DatumType dt)
{
    switch (dt) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct) case DT_##dt:
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
        return true;
    default:
        return false;
    }
}

unsigned long datum_unsigned_integer_value(Datum d)
{
    assert(DatumType_is_integer(d.type));
    switch (d.type) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) \
    case DT_##dt:                                       \
        return (unsigned long) d.n;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    default:
        UNREACHABLE();
    }
}

long datum_signed_integer_value(Datum d)
{
    assert(DatumType_is_integer(d.type));
    switch (d.type) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) \
    case DT_##dt:                                       \
        return (long) d.n;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    default:
        UNREACHABLE();
    }
}

Datum datum_INVALID(Datum, Datum)
{
    UNREACHABLE();
}

Datum datum_MEMBER_ACCESS(Datum, Datum)
{
    NYI("datum_MEMBER_ACCESS");
}

Datum datum_RANGE(Datum, Datum)
{
    NYI("datum_RANGE");
}

Datum datum_SUBSCRIPT(Datum, Datum)
{
    NYI("datum_SUBSCRIPT");
}

Datum datum_CALL(Datum, Datum)
{
    NYI("datum_CALL");
}

Datum datum_ADD(Datum d1, Datum d2)
{
    assert(d1.type == d2.type);
    Datum ret = { 0 };
    ret.type = d1.type;
    switch (d1.type) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)      \
    case DT_##dt:                   \
        ret.n = (ct) (d1.n + d2.n); \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot add data of type '%s' yet", DatumType_name(d1.type));
    }
    return ret;
}

Datum datum_SUBTRACT(Datum d1, Datum d2)
{
    assert(d1.type == d2.type);
    Datum ret = { 0 };
    ret.type = d1.type;
    switch (d1.type) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)      \
    case DT_##dt:                   \
        ret.n = (ct) (d1.n - d2.n); \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot add data of type '%s' yet", DatumType_name(d1.type));
    }
    return ret;
}

Datum datum_MULTIPLY(Datum d1, Datum d2)
{
    assert(d1.type == d2.type);
    Datum ret = { 0 };
    ret.type = d1.type;
    switch (d1.type) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)      \
    case DT_##dt:                   \
        ret.n = (ct) (d1.n * d2.n); \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot multiply data of type '%s' yet", DatumType_name(d1.type));
    }
    return ret;
}

Datum datum_DIVIDE(Datum d1, Datum d2)
{
    assert(d1.type == d2.type);
    Datum ret = { 0 };
    ret.type = d1.type;
    switch (d1.type) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)      \
    case DT_##dt:                   \
        ret.n = (ct) (d1.n / d2.n); \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot multiply data of type '%s' yet", DatumType_name(d1.type));
    }
    return ret;
}

Datum datum_MODULO(Datum d1, Datum d2)
{
    assert(d1.type == d2.type);
    Datum ret = { 0 };
    ret.type = d1.type;
    switch (d1.type) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) \
    case DT_##dt:                                       \
        ret.n = (ct) (d1.n % d2.n);                     \
        break;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    default:
        fatal("Cannot multiply data of type '%s' yet", DatumType_name(d1.type));
    }
    return ret;
}

Datum datum_EQUALS(Datum d1, Datum d2)
{
    assert(d1.type == d2.type);
    Datum ret = { 0 };
    ret.type = DT_BOOL;
    switch (d1.type) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)         \
    case DT_##dt:                      \
        ret.bool_value = d1.n == d2.n; \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", DatumType_name(d1.type));
    }
    return ret;
}

Datum datum_NOT_EQUALS(Datum d1, Datum d2)
{
    assert(d1.type == d2.type);
    Datum ret = { 0 };
    ret.type = DT_BOOL;
    switch (d1.type) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)         \
    case DT_##dt:                      \
        ret.bool_value = d1.n != d2.n; \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", DatumType_name(d1.type));
    }
    return ret;
}

Datum datum_LESS(Datum d1, Datum d2)
{
    assert(d1.type == d2.type);
    Datum ret = { 0 };
    ret.type = DT_BOOL;
    switch (d1.type) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)        \
    case DT_##dt:                     \
        ret.bool_value = d1.n < d2.n; \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", DatumType_name(d1.type));
    }
    return ret;
}

Datum datum_LESS_EQUALS(Datum d1, Datum d2)
{
    assert(d1.type == d2.type);
    Datum ret = { 0 };
    ret.type = DT_BOOL;
    switch (d1.type) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)         \
    case DT_##dt:                      \
        ret.bool_value = d1.n <= d2.n; \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", DatumType_name(d1.type));
    }
    return ret;
}

Datum datum_GREATER(Datum d1, Datum d2)
{
    assert(d1.type == d2.type);
    Datum ret = { 0 };
    ret.type = DT_BOOL;
    switch (d1.type) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)        \
    case DT_##dt:                     \
        ret.bool_value = d1.n > d2.n; \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", DatumType_name(d1.type));
    }
    return ret;
}

Datum datum_GREATER_EQUALS(Datum d1, Datum d2)
{
    assert(d1.type == d2.type);
    Datum ret = { 0 };
    ret.type = DT_BOOL;
    switch (d1.type) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)         \
    case DT_##dt:                      \
        ret.bool_value = d1.n >= d2.n; \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", DatumType_name(d1.type));
    }
    return ret;
}

Datum datum_BITWISE_AND(Datum d1, Datum d2)
{
    assert(d1.type == d2.type);
    Datum ret = { 0 };
    ret.type = d1.type;
    switch (d1.type) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) \
    case DT_##dt:                                       \
        ret.bool_value = d1.n & d2.n;                   \
        break;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", DatumType_name(d1.type));
    }
    return ret;
}

Datum datum_BITWISE_OR(Datum d1, Datum d2)
{
    assert(d1.type == d2.type);
    Datum ret = { 0 };
    ret.type = d1.type;
    switch (d1.type) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) \
    case DT_##dt:                                       \
        ret.bool_value = d1.n | d2.n;                   \
        break;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", DatumType_name(d1.type));
    }
    return ret;
}

Datum datum_BITWISE_XOR(Datum d1, Datum d2)
{
    assert(d1.type == d2.type);
    Datum ret = { 0 };
    ret.type = d1.type;
    switch (d1.type) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) \
    case DT_##dt:                                       \
        ret.bool_value = d1.n ^ d2.n;                   \
        break;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", DatumType_name(d1.type));
    }
    return ret;
}

Datum datum_LOGICAL_AND(Datum d1, Datum d2)
{
    assert(d1.type == DT_BOOL && d2.type == DT_BOOL);
    Datum ret = { 0 };
    ret.type = DT_BOOL;
    ret.bool_value = d1.bool_value && d2.bool_value;
    return ret;
}

Datum datum_LOGICAL_OR(Datum d1, Datum d2)
{
    assert(d1.type == DT_BOOL && d2.type == DT_BOOL);
    Datum ret = { 0 };
    ret.type = DT_BOOL;
    ret.bool_value = d1.bool_value || d2.bool_value;
    return ret;
}

Datum datum_BIT_SHIFT_LEFT(Datum d1, Datum d2)
{
    assert(d2.type == DT_U8);
    Datum ret = { 0 };
    ret.type = d1.type;
    switch (d1.type) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size)   \
    case DT_##dt:                                         \
        ret.n = d1.n << datum_unsigned_integer_value(d2); \
        break;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    default:
        fatal("Cannot shift left datum of type '%s'", DatumType_name(d1.type));
    }
    return ret;
}

Datum datum_BIT_SHIFT_RIGHT(Datum d1, Datum d2)
{
    assert(d2.type == DT_U8);
    Datum ret = { 0 };
    ret.type = d1.type;
    switch (d1.type) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size)   \
    case DT_##dt:                                         \
        ret.n = d1.n >> datum_unsigned_integer_value(d2); \
        break;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    default:
        fatal("Cannot shift left datum of type '%s'", DatumType_name(d1.type));
    }
    return ret;
}

typedef Datum (*BinaryDatumFunction)(Datum, Datum);

typedef struct operator_functions {
    Operator            op;
    BinaryDatumFunction function;
} OperatorFunctions;

static OperatorFunctions s_functions[] = {
#undef ENUM_BINARY_OPERATOR
#define ENUM_BINARY_OPERATOR(op, a, p, k, c) { OP_ ## op, datum_ ## op },
    BINARY_OPERATORS(ENUM_BINARY_OPERATOR)
#undef ENUM_BINARY_OPERATOR
};

Datum datum_apply(Datum d1, Operator op, Datum d2)
{
    assert(s_functions[(size_t) op].op == op);
    BinaryDatumFunction fnc = s_functions[(size_t) op].function;
    return fnc(d1, d2);
}

void datum_print(Datum d)
{
    switch (d.type) {
    case DT_VOID:
        printf("** void **");
        break;
    case DT_ERROR:
        printf("%s", d.error);
        break;
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) \
    case DT_##dt:                                       \
        printf("%" format, d.n);                        \
        break;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    case DT_FLOAT:
        printf("%f", d.float_value);
        break;
    case DT_STRING:
        printf(SV_SPEC, SV_ARG(d.string));
        break;
    case DT_POINTER:
        printf("%p", d.pointer);
        break;
    case DT_BOOL:
        printf("%s", (d.bool_value) ? "true" : "false");
        break;
    }
}
