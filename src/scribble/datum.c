/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#define STATIC_ALLOCATOR
#include <allocate.h>
#include <datum.h>

#undef ENUM_BINARY_OPERATOR
#define ENUM_BINARY_OPERATOR(op, a, p, k, c) static Datum *datum_##op(Datum *, Datum *);
BINARY_OPERATORS(ENUM_BINARY_OPERATOR)
#undef ENUM_BINARY_OPERATOR
#undef ENUM_UNARY_OPERATOR
#define ENUM_UNARY_OPERATOR(op, k, c) static Datum *datum_##op(Datum *, Datum *);
UNARY_OPERATORS(ENUM_UNARY_OPERATOR)
#undef ENUM_UNARY_OPERATOR

typedef Datum *(*BinaryDatumFunction)(Datum *, Datum *);

typedef struct operator_functions {
    Operator            op;
    BinaryDatumFunction function;
} OperatorFunctions;

static OperatorFunctions s_functions[] = {
#undef ENUM_BINARY_OPERATOR
#define ENUM_BINARY_OPERATOR(op, a, p, k, c) { OP_##op, datum_##op },
    BINARY_OPERATORS(ENUM_BINARY_OPERATOR)
#undef ENUM_BINARY_OPERATOR
#undef ENUM_UNARY_OPERATOR
#define ENUM_UNARY_OPERATOR(op, k, c) { OP_##op, datum_##op },
        UNARY_OPERATORS(ENUM_UNARY_OPERATOR)
#undef ENUM_UNARY_OPERATOR
};

#define BLOCKSIZES(S) S(1) S(2) S(4) S(8) S(16) S(32) S(64) S(128) S(256) S(515) S(1024)

#undef BLOCKSIZE
#define BLOCKSIZE(size) static Datum *fl_##size = NULL;
BLOCKSIZES(BLOCKSIZE)
#undef BLOCKSIZE

Datum *allocate_datums(size_t num)
{
    Datum *ret = NULL;
    size_t cap = 1;
    while (cap < num)
        cap *= 2;
    switch (cap) {
#define BLOCKSIZE(size)                          \
    case size:                                   \
        if (fl_##size) {                         \
            ret = fl_##size;                     \
            fl_##size = *((Datum **) fl_##size); \
        }                                        \
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

void datum_initialize(Datum *d)
{
    ExpressionType *et = type_registry_get_type_by_id(d->type);
    ErrorOrSize     size_maybe = type_sizeof(et);
    if (!type_is_concrete(et)) {
        fatal("Cannot initialize datum of type '" SV_SPEC "': %s", SV_ARG(et->name), Error_to_string(size_maybe.error));
    }
    switch (typeid_kind(d->type)) {
    case TK_PRIMITIVE:
        break;
    case TK_AGGREGATE: {
        if (d->type == STRING_ID) {
            break;
        }
        d->aggregate.num_components = et->components.num_components;
        d->aggregate.components = allocate_datums(d->aggregate.num_components);
        for (size_t ix = 0; ix < d->aggregate.num_components; ++ix) {
            d->aggregate.components[ix].type = typeid_canonical_type_id(et->components.components[ix].type_id);
            datum_initialize(d->aggregate.components + ix);
        }
    } break;
    case TK_VARIANT:
        d->variant = datum_allocate(et->components.components[0].type_id);
        break;
    default:
        UNREACHABLE();
    }
}

Datum *datum_allocate(type_id type)
{
    Datum *ret = allocate_datums(1);
    ret->type = typeid_canonical_type_id(type);
    datum_initialize(ret);
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
#define BLOCKSIZE(size)                   \
    case size:                            \
        *((Datum **) datums) = fl_##size; \
        fl_##size = datums;               \
        break;
        BLOCKSIZES(BLOCKSIZE)
#undef BLOCKSIZE
    default:
        break;
    }
}

unsigned long datum_unsigned_integer_value(Datum *d)
{
    assert(datum_is_integer(d));
    switch (d->type) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) \
    case BIT_##dt:                                      \
        return (unsigned long) d->n;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    case BIT_BOOL:
        return (unsigned long) d->bool_value;
    case BIT_POINTER:
        return (unsigned long) d->pointer;
    default:
        UNREACHABLE();
    }
}

long datum_signed_integer_value(Datum *d)
{
    assert(datum_is_integer(d));
    switch (typeid_builtin_type(d->type)) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) \
    case BIT_##dt:                                      \
        return (long) d->n;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    case BIT_BOOL:
        return (long) d->bool_value;
    case BIT_POINTER:
        return (long) d->pointer;
    default:
        UNREACHABLE();
    }
}

Datum *datum_copy(Datum *dest, Datum *src)
{
    datum_free_contents(dest);
    dest->type = src->type;
    switch (datum_kind(src)) {
    case TK_PRIMITIVE: {
        switch (typeid_builtin_type(src->type)) {
#undef PRIMITIVETYPE
#define PRIMITIVETYPE(dt, n, ct) \
    case BIT_##dt:               \
        dest->n = src->n;        \
        break;
            DATUM_PRIMITIVETYPES(PRIMITIVETYPE)
#undef PRIMITIVETYPE
        default:
            UNREACHABLE();
        }
    } break;
    case TK_AGGREGATE: {
        switch (typeid_builtin_type(src->type)) {
        case BIT_STRING:
            dest->string = src->string;
            break;
        default: {
            dest->aggregate.num_components = src->aggregate.num_components;
            dest->aggregate.components = allocate_datums(src->aggregate.num_components);
            for (size_t ix = 0; ix < dest->aggregate.num_components; ++ix) {
                datum_copy(dest->aggregate.components + ix, src->aggregate.components + ix);
            }
        } break;
        }
    } break;
    case TK_VARIANT:
        dest->variant = datum_allocate(dest->variant->type);
        datum_copy(dest->variant, src->variant);
        break;
    default:
        UNREACHABLE();
    }
    return dest;
}

Datum *datum_INVALID(Datum *, Datum *)
{
    UNREACHABLE();
}

Datum *datum_MEMBER_ACCESS(Datum *, Datum *)
{
    NYI("datum_MEMBER_ACCESS");
}

Datum *datum_RANGE(Datum *d1, Datum *d2)
{
    assert(d1->type == d2->type);
    assert(datum_is_integer(d1));
    Datum *geq = datum_LESS_EQUALS(d1, d2);
    assert(geq->bool_value);
    datum_free(geq);
    type_id type = MUST(TypeID, type_specialize_template(RANGE_ID, 1, (TemplateArgument[]) { { .name = sv_from("T"), .arg_type = TPT_TYPE, .type = d1->type } }));
    assert(typeid_has_kind(type, TK_AGGREGATE));
    Datum *ret = datum_allocate(type);
    datum_copy(ret->aggregate.components, d1);
    datum_copy(ret->aggregate.components + 1, d2);
    return ret;
}

Datum *datum_SUBSCRIPT(Datum *d1, Datum *d2)
{
    size_t ix = datum_unsigned_integer_value(d2);
    switch (typeid_builtin_type(d1->type)) {
    case BIT_STRING: {
        if (ix >= d1->string.length) {
            fatal("String index out of bounds");
        }
        Datum *ret = datum_allocate(U8_ID);
        ret->u8 = *(d1->string.ptr + ix);
        return ret;
    } break;
    case BIT_ARRAY: {
        if (ix >= d1->aggregate.num_components) {
            fatal("Array index out of bounds");
        }
        ExpressionType *t = type_registry_get_type_by_id(d1->type);
        type_id         underlying = t->template_arguments[0].type;
        Datum          *ret = datum_allocate(underlying);
        return datum_copy(ret, d1->aggregate.components + ix);
    }
    default:
        fatal("Cannot get cardinality of data of type '%s' yet", BuiltinType_name(d1->type));
    }
}

Datum *datum_CALL(Datum *, Datum *)
{
    NYI("datum_CALL");
}

Datum *datum_ADD(Datum *d1, Datum *d2)
{
    assert(d1->type == d2->type);
    assert(datum_is_builtin(d1));
    Datum *ret = datum_allocate(d1->type);
    switch (typeid_builtin_type(ret->type)) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)         \
    case BIT_##dt:                     \
        ret->n = (ct) (d1->n + d2->n); \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot add data of type '%s' yet", BuiltinType_name(typeid_builtin_type(d1->type)));
    }
    return ret;
}

Datum *datum_SUBTRACT(Datum *d1, Datum *d2)
{
    trace(CAT_EXECUTE, "Subtract %.*s from %.*", SV_ARG(typeid_name(d1->type)), SV_ARG(typeid_name(d2->type)));
    assert(d1->type == d2->type);
    assert(datum_is_builtin(d1));
    Datum *ret = datum_allocate(d1->type);
    switch (typeid_builtin_type(ret->type)) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)         \
    case BIT_##dt:                     \
        ret->n = (ct) (d1->n - d2->n); \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot add data of type '%s' yet", BuiltinType_name(d1->type));
    }
    return ret;
}

Datum *datum_MULTIPLY(Datum *d1, Datum *d2)
{
    assert(d1->type == d2->type);
    Datum *ret = datum_allocate(d1->type);
    switch (typeid_builtin_type(ret->type)) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)         \
    case BIT_##dt:                     \
        ret->n = (ct) (d1->n * d2->n); \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot multiply data of type '%s' yet", BuiltinType_name(d1->type));
    }
    return ret;
}

Datum *datum_DIVIDE(Datum *d1, Datum *d2)
{
    assert(d1->type == d2->type);
    Datum *ret = datum_allocate(d1->type);
    switch (typeid_builtin_type(ret->type)) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)         \
    case BIT_##dt:                     \
        ret->n = (ct) (d1->n / d2->n); \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot multiply data of type '%s' yet", BuiltinType_name(d1->type));
    }
    return ret;
}

Datum *datum_MODULO(Datum *d1, Datum *d2)
{
    assert(d1->type == d2->type);
    Datum *ret = datum_allocate(d1->type);
    switch (typeid_builtin_type(ret->type)) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) \
    case BIT_##dt:                                      \
        ret->n = (ct) (d1->n % d2->n);                  \
        break;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    default:
        fatal("Cannot multiply data of type '%s' yet", BuiltinType_name(d1->type));
    }
    return ret;
}

Datum *datum_EQUALS(Datum *d1, Datum *d2)
{
    assert(d1->type == d2->type);
    Datum *ret = datum_allocate(BOOL_ID);
    switch (typeid_builtin_type(d1->type)) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)            \
    case BIT_##dt:                        \
        ret->bool_value = d1->n == d2->n; \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", BuiltinType_name(d1->type));
    }
    return ret;
}

Datum *datum_NOT_EQUALS(Datum *d1, Datum *d2)
{
    assert(d1->type == d2->type);
    Datum *ret = datum_allocate(BOOL_ID);
    switch (typeid_builtin_type(d1->type)) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)            \
    case BIT_##dt:                        \
        ret->bool_value = d1->n != d2->n; \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", BuiltinType_name(d1->type));
    }
    return ret;
}

Datum *datum_LESS(Datum *d1, Datum *d2)
{
    assert(d1->type == d2->type);
    Datum *ret = datum_allocate(BOOL_ID);
    switch (typeid_builtin_type(d1->type)) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)           \
    case BIT_##dt:                       \
        ret->bool_value = d1->n < d2->n; \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", BuiltinType_name(d1->type));
    }
    return ret;
}

Datum *datum_LESS_EQUALS(Datum *d1, Datum *d2)
{
    assert(d1->type == d2->type);
    Datum *ret = datum_allocate(BOOL_ID);
    switch (typeid_builtin_type(d1->type)) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)            \
    case BIT_##dt:                        \
        ret->bool_value = d1->n <= d2->n; \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", BuiltinType_name(d1->type));
    }
    return ret;
}

Datum *datum_GREATER(Datum *d1, Datum *d2)
{
    assert(d1->type == d2->type);
    Datum *ret = datum_allocate(BOOL_ID);
    switch (typeid_builtin_type(d1->type)) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)           \
    case BIT_##dt:                       \
        ret->bool_value = d1->n > d2->n; \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", BuiltinType_name(d1->type));
    }
    return ret;
}

Datum *datum_GREATER_EQUALS(Datum *d1, Datum *d2)
{
    assert(d1->type == d2->type);
    Datum *ret = datum_allocate(BOOL_ID);
    switch (typeid_builtin_type(d1->type)) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct)            \
    case BIT_##dt:                        \
        ret->bool_value = d1->n >= d2->n; \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", BuiltinType_name(d1->type));
    }
    return ret;
}

Datum *datum_BITWISE_AND(Datum *d1, Datum *d2)
{
    assert(d1->type == d2->type);
    Datum *ret = datum_allocate(d1->type);
    switch (typeid_builtin_type(ret->type)) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) \
    case BIT_##dt:                                      \
        ret->bool_value = d1->n & d2->n;                \
        break;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", BuiltinType_name(d1->type));
    }
    return ret;
}

Datum *datum_BITWISE_OR(Datum *d1, Datum *d2)
{
    assert(d1->type == d2->type);
    Datum *ret = datum_allocate(d1->type);
    switch (typeid_builtin_type(ret->type)) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) \
    case BIT_##dt:                                      \
        ret->bool_value = d1->n | d2->n;                \
        break;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", BuiltinType_name(d1->type));
    }
    return ret;
}

Datum *datum_BITWISE_XOR(Datum *d1, Datum *d2)
{
    assert(d1->type == d2->type);
    Datum *ret = datum_allocate(d1->type);
    switch (typeid_builtin_type(ret->type)) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) \
    case BIT_##dt:                                      \
        ret->bool_value = d1->n ^ d2->n;                \
        break;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    default:
        fatal("Cannot determine equality of data of type '%s' yet", BuiltinType_name(d1->type));
    }
    return ret;
}

Datum *datum_LOGICAL_AND(Datum *d1, Datum *d2)
{
    assert(d1->type == BOOL_ID && d2->type == BOOL_ID);
    Datum *ret = datum_allocate(BOOL_ID);
    ret->bool_value = d1->bool_value && d2->bool_value;
    return ret;
}

Datum *datum_LOGICAL_OR(Datum *d1, Datum *d2)
{
    assert(d1->type == BOOL_ID && d2->type == BOOL_ID);
    Datum *ret = datum_allocate(BOOL_ID);
    ret->bool_value = d1->bool_value || d2->bool_value;
    return ret;
}

Datum *datum_BIT_SHIFT_LEFT(Datum *d1, Datum *d2)
{
    assert(d2->type == BIT_U8);
    Datum *ret = datum_allocate(d1->type);
    switch (typeid_builtin_type(ret->type)) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size)     \
    case BIT_##dt:                                          \
        ret->n = d1->n << datum_unsigned_integer_value(d2); \
        break;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    default:
        fatal("Cannot shift left datum of type '%s'", BuiltinType_name(d1->type));
    }
    return ret;
}

Datum *datum_BIT_SHIFT_RIGHT(Datum *d1, Datum *d2)
{
    assert(d2->type == BIT_U8);
    Datum *ret = datum_allocate(d1->type);
    switch (typeid_builtin_type(ret->type)) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size)     \
    case BIT_##dt:                                          \
        ret->n = d1->n >> datum_unsigned_integer_value(d2); \
        break;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    default:
        fatal("Cannot shift left datum of type '%s'", BuiltinType_name(d1->type));
    }
    return ret;
}

Datum *datum_INVALID_UNARY(Datum *d1, Datum *d2)
{
    UNREACHABLE();
}

Datum *datum_IDENTITY(Datum *d, Datum *)
{
    Datum *ret = datum_allocate(d->type);
    return datum_copy(ret, d);
}

Datum *datum_NEGATE(Datum *d, Datum *)
{
    assert(datum_is_builtin(d));
    Datum *ret = datum_allocate(d->type);
    switch (typeid_builtin_type(ret->type)) {
#undef NUMERICTYPE
#define NUMERICTYPE(dt, n, ct) \
    case BIT_##dt:             \
        ret->n = (ct) (-d->n); \
        break;
        NUMERICTYPES(NUMERICTYPE)
#undef NUMERICTYPE
    default:
        fatal("Cannot negate data of type '%s' yet", BuiltinType_name(d->type));
    }
    return ret;
}

Datum *datum_CARDINALITY(Datum *d, Datum *)
{
    Datum *ret = datum_allocate(U64_ID);
    switch (typeid_builtin_type(d->type)) {
    case BIT_STRING:
        ret->u64 = d->string.length;
        break;
    case BIT_ARRAY:
        ret->u64 = d->aggregate.num_components;
        break;
    default:
        fatal("Cannot get cardinality of data of type '%s' yet", BuiltinType_name(d->type));
    }
    return ret;
}

Datum *datum_UNARY_INCREMENT(Datum *d1, Datum *d2)
{
    return NULL;
}

Datum *datum_UNARY_DECREMENT(Datum *d, Datum *)
{
    return NULL;
}

Datum *datum_LOGICAL_INVERT(Datum *d, Datum *)
{
    Datum *ret = datum_allocate(BOOL_ID);
    switch (typeid_builtin_type(d->type)) {
    case BIT_BOOL:
        ret->bool_value = !d->bool_value;
        break;
    default:
        fatal("Cannot invert data of type '%s'", BuiltinType_name(d->type));
    }
    return ret;
}

Datum *datum_BITWISE_INVERT(Datum *d, Datum *)
{
    assert(datum_is_builtin(d));
    Datum *ret = datum_allocate(d->type);
    switch (typeid_builtin_type(ret->type)) {
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) \
    case BIT_##dt:                                      \
        ret->n = (ct) (~d->n);                          \
        break;
        INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
    default:
        fatal("Cannot negate data of type '%s' yet", BuiltinType_name(d->type));
    }
    return ret;
}

Datum *datum_DEREFERENCE(Datum *d1, Datum *d2)
{
    return NULL;
}

Datum *datum_ADDRESS_OF(Datum *d1, Datum *d2)
{
    return NULL;
}

Datum *datum_UNARY_MEMBER_ACCESS(Datum *d1, Datum *d2)
{
    return NULL;
}

Datum *datum_apply(Datum *d1, Operator op, Datum *d2)
{
    assert(s_functions[(size_t) op].op == op);
    BinaryDatumFunction fnc = s_functions[(size_t) op].function;
    return fnc(d1, d2);
}

void datum_print(Datum *d)
{
    StringView sv = datum_sprint(d);
    printf(SV_SPEC_LALIGN ": %.*s", SV_ARG_LALIGN(sv, 12), SV_ARG(type_registry_get_type_by_id(d->type)->name));
    sv_free(sv);
}

StringView datum_sprint(Datum *d)
{
    StringBuilder sb = sb_create();
    switch (datum_kind(d)) {
    case TK_PRIMITIVE: {
        switch (typeid_builtin_type(d->type)) {
        case BIT_VOID:
            sb_append_cstr(&sb, "** void **");
            break;
        case BIT_ERROR:
            sb_append_cstr(&sb, d->error.exception);
            break;
#undef INTEGERTYPE
#define INTEGERTYPE(dt, n, ct, is_signed, format, size) \
    case BIT_##dt:                                      \
        sb_printf(&sb, "%" format, d->n);               \
        break;
            INTEGERTYPES(INTEGERTYPE)
#undef INTEGERTYPE
        case BIT_FLOAT:
            sb_printf(&sb, "%f", d->float_value);
            break;
        case BIT_POINTER:
            sb_printf(&sb, "%p", d->pointer);
            break;
        case BIT_BOOL:
            sb_printf(&sb, "%s", (d->bool_value) ? "true" : "false");
            break;
        default:
            UNREACHABLE();
        }
    } break;
    case TK_AGGREGATE: {
        if (d->type == STRING_ID) {
            sb_printf(&sb, "%.*s", SV_ARG(d->string));
            break;
        }
        sb_append_cstr(&sb, "{");
        char const     *comma = "";
        ExpressionType *et = type_registry_get_type_by_id(d->type);
        for (size_t ix = 0; ix < d->aggregate.num_components; ++ix) {
            sb_append_cstr(&sb, comma);
            comma = ",";
            sb_append_cstr(&sb, " ");
            sb_append_sv(&sb, et->components.components[ix].name);
            sb_append_cstr(&sb, ": ");
            sb_append_sv(&sb, datum_sprint(d->aggregate.components + ix));
        }
        sb_append_cstr(&sb, " }");
    } break;
    case TK_VARIANT: {
        ExpressionType *et = type_registry_get_type_by_id(d->variant->type);
        sb_append_sv(&sb, et->name);
        sb_append_cstr(&sb, ":");
        sb_append_sv(&sb, datum_sprint(d->variant));
    } break;
    default:
        UNREACHABLE();
    }
    return sb.view;
}

Datum *datum_make_integer(Integer value)
{
    Datum *d = datum_allocate(type_registry_id_of_builtin_type(value.type));
    Integer_boundscheck(value);
    switch (typeid_builtin_type(d->type)) {
    case BIT_I8:
        d->i8 = (int8_t) value.value.signed_value;
        break;
    case BIT_U8:
        d->u8 = (uint8_t) value.value.unsigned_value;
        break;
    case BIT_I16:
        d->i16 = (int16_t) value.value.signed_value;
        break;
    case BIT_U16:
        d->u16 = (uint16_t) value.value.unsigned_value;
        break;
    case BIT_I32:
        d->i32 = (int32_t) value.value.signed_value;
        break;
    case BIT_U32:
        d->u32 = (uint32_t) value.value.unsigned_value;
        break;
    case BIT_I64:
        d->u64 = value.value.signed_value;
        break;
    case BIT_U64:
        d->u64 = value.value.unsigned_value;
        break;
    default:
        UNREACHABLE();
    }
    return d;
}

void datum_free_contents(Datum *d)
{
    switch (datum_kind(d)) {
    case TK_AGGREGATE: {
        for (size_t ix = 0; ix < d->aggregate.num_components; ++ix) {
            datum_free(d->aggregate.components + ix);
        }
        free_datums(d->aggregate.components, d->aggregate.num_components);
    } break;
    case TK_VARIANT: {
        datum_free(d->variant);
    } break;
    default:
        break;
    }
}

void datum_free(Datum *d)
{
    datum_free_contents(d);
    free_datums(d, 1);
}
