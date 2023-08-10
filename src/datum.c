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

Datum datum_add(Datum d1, Datum d2)
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

Datum datum_subtract(Datum d1, Datum d2)
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

Datum datum_multiply(Datum d1, Datum d2)
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

Datum datum_divide(Datum d1, Datum d2)
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

Datum datum_modulo(Datum d1, Datum d2)
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

Datum datum_equals(Datum d1, Datum d2)
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

Datum datum_less(Datum d1, Datum d2)
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

Datum datum_greater(Datum d1, Datum d2)
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

Datum datum_apply(Datum d1, Operator op, Datum d2)
{
    switch (op) {
    case OP_ADD:
        return datum_add(d1, d2);
    case OP_MULTIPLY:
        return datum_multiply(d1, d2);
    case OP_EQUALS:
        return datum_equals(d1, d2);
    case OP_LESS:
        return datum_less(d1, d2);
    case OP_GREATER:
        return datum_greater(d1, d2);
    default: {
        fatal("Cannot apply operator '%s' to data of type '%s' yet", Operator_name(op), DatumType_name(d1.type));
    }
    }
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
