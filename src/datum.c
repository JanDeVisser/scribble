/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#define STATIC_ALLOCATOR
#include <allocate.h>
#include <datum.h>

#undef ENUM_BINARY_OPERATOR
#define ENUM_BINARY_OPERATOR(op, a, p, k, c) static Datum datum_##op(Datum, Datum);
BINARY_OPERATORS(ENUM_BINARY_OPERATOR)
#undef ENUM_BINARY_OPERATOR

typedef Datum (*BinaryDatumFunction)(Datum, Datum);

typedef struct operator_functions {
    Operator            op;
    BinaryDatumFunction function;
} OperatorFunctions;

static OperatorFunctions s_functions[] = {
#undef ENUM_BINARY_OPERATOR
#define ENUM_BINARY_OPERATOR(op, a, p, k, c) { OP_##op, datum_##op },
    BINARY_OPERATORS(ENUM_BINARY_OPERATOR)
#undef ENUM_BINARY_OPERATOR
};

#define BLOCKSIZES(S) S(1) S(2) S(4) S(8) S(16) S(32) S(64) S(128) S(256) S(512) S(1024)

#undef BLOCKSIZE
#define BLOCKSIZE(size) static void *fl_##size = NULL;
BLOCKSIZES(BLOCKSIZE)
#undef BLOCKSIZE

Datum *allocate_datums(size_t num)
{
    Datum *ret = NULL;
    size_t cap = 1;
    while (cap < num)
        cap *= 2;
    assert(cap <= 1024);
    switch (cap) {
#define BLOCKSIZE(size)                         \
    case size:                                  \
        if (fl_##size) {                        \
            ret = fl_##size;                    \
            fl_##size = *((char **) fl_##size); \
        }                                       \
        break;
        BLOCKSIZES(BLOCKSIZE)
#undef BLOCKSIZE
    default:
        break;
    }
    if (!ret) {
        ret = allocate_array(Datum, cap);
    }
    return ret;
}

void free_datums(Datum *datums, size_t num)
{
    size_t cap = 1;
    while (cap < num)
        cap *= 2;
    assert(cap <= 1024);
    switch (cap) {
#undef BLOCKSIZE
#define BLOCKSIZE(size)                  \
    case size:                           \
        *((char **) datums) = fl_##size; \
        fl_##size = datums;              \
        break;
        BLOCKSIZES(BLOCKSIZE)
#undef BLOCKSIZE
    default:
        break;
    }
}

unsigned long datum_unsigned_integer_value(Datum d)
{
    assert(datum_is_integer(d));
    switch (d.type) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) \
    case PT_##dt:                                       \
        return (unsigned long) d.n;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    case PT_BOOL:
        return (unsigned long) d.bool_value;
    case PT_POINTER:
        return (unsigned long) d.pointer;
    default:
        UNREACHABLE();
    }
}

long datum_signed_integer_value(Datum d)
{
    assert(datum_is_integer(d));
    switch (d.type) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) \
    case PT_##dt:                                       \
        return (long) d.n;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    case PT_BOOL:
        return (long) d.bool_value;
    case PT_POINTER:
        return (long) d.pointer;
    default:
        UNREACHABLE();
    }
}

Datum datum_copy(Datum d)
{
    assert(datum_is_primitive(d));
    Datum ret = { 0 };
    ret.type = d.type;
    switch (d.type) {
#undef PRIMITIVETYPE
#define PRIMITIVETYPE(dt, n, ct) ret.n = d.n;
        DATUM_PRIMITIVETYPES(PRIMITIVETYPE)
#undef PRIMITIVETYPE
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

Datum datum_RANGE(Datum d1, Datum d2)
{
    assert(d1.type == d2.type);
    assert(datum_is_integer(d1));
    assert(datum_GREATER_EQUALS(d1, d2).bool_value);
    Datum ret = { 0 };
    MUST_TO_VAR(TypeID, ret.type, type_specialize_template(RANGE_ID, 1, (TemplateArgument[]) { { .name = sv_from("T"), .param_type = TPT_TYPE, .type = d1.type } }));
    assert(typeid_has_kind(ret.type, TK_COMPOSITE));
    Datum *fields = allocate_datums(2);
    fields[0] = datum_copy(d1);
    fields[1] = datum_copy(d2);
    ret.components = fields;
    return ret;
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
    assert(datum_is_primitive(d1));
    Datum ret = { 0 };
    ret.type = d1.type;
    switch (d1.type) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)      \
    case PT_##dt:                   \
        ret.n = (ct) (d1.n + d2.n); \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot add data of type '%s' yet", PrimitiveType_name(d1.type));
    }
    return ret;
}

Datum datum_SUBTRACT(Datum d1, Datum d2)
{
    assert(d1.type == d2.type);
    assert(datum_is_primitive(d1));
    Datum ret = { 0 };
    ret.type = d1.type;
    switch (d1.type) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)      \
    case PT_##dt:                   \
        ret.n = (ct) (d1.n - d2.n); \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot add data of type '%s' yet", PrimitiveType_name(d1.type));
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
    case PT_##dt:                   \
        ret.n = (ct) (d1.n * d2.n); \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot multiply data of type '%s' yet", PrimitiveType_name(d1.type));
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
    case PT_##dt:                   \
        ret.n = (ct) (d1.n / d2.n); \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot multiply data of type '%s' yet", PrimitiveType_name(d1.type));
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
    case PT_##dt:                                       \
        ret.n = (ct) (d1.n % d2.n);                     \
        break;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    default:
        fatal("Cannot multiply data of type '%s' yet", PrimitiveType_name(d1.type));
    }
    return ret;
}

Datum datum_EQUALS(Datum d1, Datum d2)
{
    assert(d1.type == d2.type);
    Datum ret = { 0 };
    ret.type = PT_BOOL;
    switch (d1.type) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)         \
    case PT_##dt:                      \
        ret.bool_value = d1.n == d2.n; \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", PrimitiveType_name(d1.type));
    }
    return ret;
}

Datum datum_NOT_EQUALS(Datum d1, Datum d2)
{
    assert(d1.type == d2.type);
    Datum ret = { 0 };
    ret.type = PT_BOOL;
    switch (d1.type) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)         \
    case PT_##dt:                      \
        ret.bool_value = d1.n != d2.n; \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", PrimitiveType_name(d1.type));
    }
    return ret;
}

Datum datum_LESS(Datum d1, Datum d2)
{
    assert(d1.type == d2.type);
    Datum ret = { 0 };
    ret.type = PT_BOOL;
    switch (d1.type) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)        \
    case PT_##dt:                     \
        ret.bool_value = d1.n < d2.n; \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", PrimitiveType_name(d1.type));
    }
    return ret;
}

Datum datum_LESS_EQUALS(Datum d1, Datum d2)
{
    assert(d1.type == d2.type);
    Datum ret = { 0 };
    ret.type = PT_BOOL;
    switch (d1.type) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)         \
    case PT_##dt:                      \
        ret.bool_value = d1.n <= d2.n; \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", PrimitiveType_name(d1.type));
    }
    return ret;
}

Datum datum_GREATER(Datum d1, Datum d2)
{
    assert(d1.type == d2.type);
    Datum ret = { 0 };
    ret.type = PT_BOOL;
    switch (d1.type) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)        \
    case PT_##dt:                     \
        ret.bool_value = d1.n > d2.n; \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", PrimitiveType_name(d1.type));
    }
    return ret;
}

Datum datum_GREATER_EQUALS(Datum d1, Datum d2)
{
    assert(d1.type == d2.type);
    Datum ret = { 0 };
    ret.type = PT_BOOL;
    switch (d1.type) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)         \
    case PT_##dt:                      \
        ret.bool_value = d1.n >= d2.n; \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", PrimitiveType_name(d1.type));
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
    case PT_##dt:                                       \
        ret.bool_value = d1.n & d2.n;                   \
        break;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", PrimitiveType_name(d1.type));
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
    case PT_##dt:                                       \
        ret.bool_value = d1.n | d2.n;                   \
        break;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", PrimitiveType_name(d1.type));
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
    case PT_##dt:                                       \
        ret.bool_value = d1.n ^ d2.n;                   \
        break;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", PrimitiveType_name(d1.type));
    }
    return ret;
}

Datum datum_LOGICAL_AND(Datum d1, Datum d2)
{
    assert(d1.type == PT_BOOL && d2.type == PT_BOOL);
    Datum ret = { 0 };
    ret.type = PT_BOOL;
    ret.bool_value = d1.bool_value && d2.bool_value;
    return ret;
}

Datum datum_LOGICAL_OR(Datum d1, Datum d2)
{
    assert(d1.type == PT_BOOL && d2.type == PT_BOOL);
    Datum ret = { 0 };
    ret.type = ret.type = PT_BOOL;
    ret.bool_value = d1.bool_value || d2.bool_value;
    return ret;
}

Datum datum_BIT_SHIFT_LEFT(Datum d1, Datum d2)
{
    assert(d2.type == PT_U8);
    Datum ret = { 0 };
    ret.type = d1.type;
    switch (d1.type) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size)   \
    case PT_##dt:                                         \
        ret.n = d1.n << datum_unsigned_integer_value(d2); \
        break;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    default:
        fatal("Cannot shift left datum of type '%s'", PrimitiveType_name(d1.type));
    }
    return ret;
}

Datum datum_BIT_SHIFT_RIGHT(Datum d1, Datum d2)
{
    assert(d2.type == PT_U8);
    Datum ret = { 0 };
    ret.type = d1.type;
    switch (d1.type) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size)   \
    case PT_##dt:                                         \
        ret.n = d1.n >> datum_unsigned_integer_value(d2); \
        break;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    default:
        fatal("Cannot shift left datum of type '%s'", PrimitiveType_name(d1.type));
    }
    return ret;
}

Datum datum_apply(Datum d1, Operator op, Datum d2)
{
    assert(s_functions[(size_t) op].op == op);
    BinaryDatumFunction fnc = s_functions[(size_t) op].function;
    return fnc(d1, d2);
}

void datum_print(Datum d)
{
    StringView sv = datum_sprint(d);
    printf("%.*s", SV_ARG(sv));
    sv_free(sv);
}

StringView datum_sprint(Datum d)
{
    switch (d.type) {
    case PT_VOID:
        return sv_printf("** void **");
    case PT_ERROR:
        return sv_printf("%s", d.error);
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) \
    case PT_##dt:                                       \
        return sv_printf("%" format, d.n);
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    case PT_FLOAT:
        return sv_printf("%f", d.float_value);
    case PT_STRING:
        return sv_printf(SV_SPEC, SV_ARG(d.string));
    case PT_POINTER:
        return sv_printf("%p", d.pointer);
    case PT_BOOL:
        return sv_printf("%s", (d.bool_value) ? "true" : "false");
    default:
        UNREACHABLE();
    }
}

Datum datum_make_integer(size_t width, bool un_signed, int64_t signed_value, uint64_t unsigned_value)
{
    Datum d = { 0 };
    d.type = PrimitiveType_get_integer_type(width, un_signed);
    if (un_signed) {
        switch (width) {
        case 8:
            if (unsigned_value > UINT8_MAX) {
                fatal("u8 value out of range: %zu", unsigned_value);
            }
            d.u8 = (uint8_t) unsigned_value;
            break;
        case 16:
            if (unsigned_value > UINT16_MAX) {
                fatal("u16 value out of range: %zu", unsigned_value);
            }
            d.u16 = (uint16_t) unsigned_value;
            break;
        case 32:
            if (unsigned_value > UINT32_MAX) {
                fatal("u32 value out of range: %zu", unsigned_value);
            }
            d.u32 = (uint32_t) unsigned_value;
            break;
        case 64:
            d.u64 = unsigned_value;
            break;
        default:
            UNREACHABLE();
        }
    } else {
        switch (width) {
        case 8:
            if (signed_value > INT8_MAX || signed_value < INT8_MIN) {
                fatal("i8 value out of range: %zu", signed_value);
            }
            d.i8 = (int8_t) signed_value;
            break;
        case 16:
            if (signed_value > INT16_MAX || signed_value < INT16_MIN) {
                fatal("i16 value out of range: %zu", signed_value);
            }
            d.i16 = (int16_t) signed_value;
            break;
        case 32:
            if (signed_value > INT32_MAX || signed_value < INT32_MIN) {
                fatal("i32 value out of range: %zu", signed_value);
            }
            d.i32 = (int32_t) signed_value;
            break;
        case 64:
            d.u64 = signed_value;
            break;
        default:
            UNREACHABLE();
        }
    }
    return d;
}

void datum_free(Datum d)
{
    ExpressionType *type = type_registry_get_type_by_id(d.type);
    switch (datum_kind(d)) {
    case TK_COMPOSITE: {
        for (size_t ix = 0; ix < type->components.num_components; ++ix) {
            datum_free(d.components[ix]);
        }
        free_datums(d.components, type->components.num_components);
    } break;
    case TK_ARRAY: {
        for (size_t ix = 0; ix < d.array.size; ++ix) {
            datum_free(d.array.components[ix]);
        }
        free_datums(d.array.components, d.array.size);
    } break;
    case TK_VARIANT: {
        datum_free(*d.variant.value);
    } break;
    default:
        break;
    }
}
