/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <integer.h>
#include <log.h>

IntegerType IntegerType_from_name(StringView name)
{
    if (sv_eq_cstr(name, "i8")) {
        return I8;
    } else if (sv_eq_cstr(name, "u8")) {
        return U8;
    } else if (sv_eq_cstr(name, "i16")) {
        return I16;
    } else if (sv_eq_cstr(name, "u16")) {
        return U16;
    } else if (sv_eq_cstr(name, "i32")) {
        return I32;
    } else if (sv_eq_cstr(name, "u32")) {
        return U32;
    } else if (sv_eq_cstr(name, "i64")) {
        return I64;
    } else if (sv_eq_cstr(name, "u64")) {
        return U64;
    } else {
        return IU_NO_SUCH_TYPE;
    }
}

Integer integer_create(IntegerType type, uint64_t value)
{
    Integer ret = { .type = type };
    switch (type) {
#undef INTEGER_SIZE
#define INTEGER_SIZE(sz)                  \
    case U##sz:                           \
        ret.u##sz = (uint##sz##_t) value; \
        break;                            \
    case I##sz:                           \
        ret.u##sz = (int##sz##_t) value;  \
        break;
        INTEGER_SIZES(INTEGER_SIZE)
#undef INTEGER_SIZE
    default:
        UNREACHABLE();
    }
    return ret;
}

OptionalInt64 integer_signed_value(Integer i)
{
    switch (i.type) {
    case U8:
        return OptionalInt64_create(i.u8);
    case I8:
        return OptionalInt64_create(i.i8);
    case U16:
        return OptionalInt64_create(i.u16);
    case I16:
        return OptionalInt64_create(i.i16);
    case U32:
        return OptionalInt64_create(i.u32);
    case I32:
        return OptionalInt64_create(i.i32);
    case U64:
        return (i.u64 <= INT64_MAX) ? OptionalInt64_create((int64_t) i.u64) : OptionalInt64_empty();
    case I64:
        return OptionalInt64_create(i.i64);
    default:
        UNREACHABLE();
    }
}

OptionalUInt64 integer_unsigned_value(Integer i)
{
    switch (i.type) {
    case U8:
        return OptionalUInt64_create(i.u8);
    case I8:
        return OptionalUInt64_create(i.i8);
    case U16:
        return OptionalUInt64_create(i.u16);
    case I16:
        return OptionalUInt64_create(i.i16);
    case U32:
        return OptionalUInt64_create(i.u32);
    case I32:
        return OptionalUInt64_create(i.i32);
    case U64:
        return OptionalUInt64_create(i.u64);
    case I64:
        return (i.i64 > 0) ? OptionalUInt64_create((uint64_t) i.i64) : OptionalUInt64_empty();
    default:
        UNREACHABLE();
    }
}

OptionalInteger integer_coerce_to_unsigned(Integer i, IntegerType type)
{
    OptionalUInt64 value_maybe = integer_unsigned_value(i);
    if (!value_maybe.has_value) {
        return OptionalInteger_empty();
    }
    uint64_t value = value_maybe.value;
    Integer  coerced = { .type = ((int) type < 0) ? -type : type };
    switch (type) {
    case I8:
    case U8:
        if (value > UINT8_MAX) {
            return OptionalInteger_empty();
        }
        coerced.u8 = (uint8_t) value;
        break;
    case I16:
    case U16:
        if (value > UINT16_MAX) {
            return OptionalInteger_empty();
        }
        coerced.u16 = (uint16_t) value;
        break;
    case I32:
    case U32:
        if (value > UINT32_MAX) {
            return OptionalInteger_empty();
        }
        coerced.u32 = (uint32_t) value;
        break;
    case I64:
    case U64:
        coerced.u64 = value;
        break;
    default:
        UNREACHABLE();
    }
    return OptionalInteger_create(coerced);
}

OptionalInteger integer_coerce_to_signed(Integer i, IntegerType type)
{
    OptionalInt64 value_maybe = integer_signed_value(i);
    if (!value_maybe.has_value) {
        return OptionalInteger_empty();
    }
    int64_t value = value_maybe.value;
    Integer coerced = { .type = ((int) type < 0) ? type : -type };
    switch (type) {
    case I8:
    case U8:
        if (value < INT8_MIN || value > INT8_MAX) {
            return OptionalInteger_empty();
        }
        coerced.i8 = (int8_t) value;
        break;
    case I16:
    case U16:
        if (value < INT16_MIN || value > INT16_MAX) {
            return OptionalInteger_empty();
        }
        coerced.i16 = (int16_t) value;
        break;
    case I32:
    case U32:
        if (value < INT32_MIN || value > INT32_MAX) {
            return OptionalInteger_empty();
        }
        coerced.i32 = (int32_t) value;
        break;
    case I64:
    case U64:
        coerced.i64 = value;
        break;
    default:
        UNREACHABLE();
    }
    return OptionalInteger_create(coerced);
}

OptionalInteger integer_coerce_to(Integer i, IntegerType type)
{
    return ((int) type > 8) ? integer_coerce_to_unsigned(i, type) : integer_coerce_to_signed(i, type);
}

#define INTEGER_BINARY_OP(fnc, op)                                       \
    Integer integer_##fnc(Integer i1, Integer i2)                        \
    {                                                                    \
        assert((int) i1.type *(int) i2.type > 0);                        \
        int sz_1 = (int) i1.type;                                        \
        if (sz_1 < 0)                                                    \
            sz_1 = -sz_1;                                                \
        int sz_2 = (int) i2.type;                                        \
        if (sz_2 < 0)                                                    \
            sz_2 = -sz_2;                                                \
        if (sz_1 > sz_2) {                                               \
            i2 = MUST_OPTIONAL(Integer, integer_coerce_to(i2, i1.type)); \
        } else if (sz_2 > sz_1) {                                        \
            i1 = MUST_OPTIONAL(Integer, integer_coerce_to(i1, i2.type)); \
        }                                                                \
        Integer ret = { .type = i1.type };                               \
        switch (ret.type) {                                              \
        case U8:                                                         \
            ret.u8 = i1.u8 op i2.u8;                                     \
            break;                                                       \
        case I8:                                                         \
            ret.i8 = i1.i8 op i2.i8;                                     \
            break;                                                       \
        case U16:                                                        \
            ret.u16 = i1.u16 op i2.u16;                                  \
            break;                                                       \
        case I16:                                                        \
            ret.i16 = i1.i16 op i2.i16;                                  \
            break;                                                       \
        case U32:                                                        \
            ret.u32 = i1.u32 op i2.u32;                                  \
            break;                                                       \
        case I32:                                                        \
            ret.i32 = i1.i32 op i2.i32;                                  \
            break;                                                       \
        case U64:                                                        \
            ret.u64 = i1.u64 op i2.u64;                                  \
            break;                                                       \
        case I64:                                                        \
            ret.i64 = i1.i64 op i2.i64;                                  \
            break;                                                       \
        default:                                                         \
            UNREACHABLE();                                               \
        }                                                                \
        return ret;                                                      \
    }

#define INTEGER_BINARY_BOOL_OP(fnc, op)                                  \
    bool integer_##fnc(Integer i1, Integer i2)                           \
    {                                                                    \
        assert((int) i1.type *(int) i2.type > 0);                        \
        int sz_1 = (int) i1.type;                                        \
        if (sz_1 < 0)                                                    \
            sz_1 = -sz_1;                                                \
        int sz_2 = (int) i2.type;                                        \
        if (sz_2 < 0)                                                    \
            sz_2 = -sz_2;                                                \
        if (sz_1 > sz_2) {                                               \
            i2 = MUST_OPTIONAL(Integer, integer_coerce_to(i2, i1.type)); \
        } else if (sz_2 > sz_1) {                                        \
            i1 = MUST_OPTIONAL(Integer, integer_coerce_to(i1, i2.type)); \
        }                                                                \
        Integer ret = { .type = i1.type };                               \
        switch (i1.type) {                                               \
        case U8:                                                         \
            return i1.u8 op i2.u8;                                       \
        case I8:                                                         \
            return i1.i8 op i2.i8;                                       \
        case U16:                                                        \
            return i1.u16 op i2.u16;                                     \
        case I16:                                                        \
            return i1.i16 op i2.i16;                                     \
        case U32:                                                        \
            return i1.u32 op i2.u32;                                     \
        case I32:                                                        \
            return i1.i32 op i2.i32;                                     \
        case U64:                                                        \
            return i1.u64 op i2.u64;                                     \
        case I64:                                                        \
            return i1.i64 op i2.i64;                                     \
        default:                                                         \
            UNREACHABLE();                                               \
        }                                                                \
    }

INTEGER_BINARY_OP(add, +)
INTEGER_BINARY_OP(subtract, -)
INTEGER_BINARY_OP(multiply, *)
INTEGER_BINARY_OP(divide, /)
INTEGER_BINARY_OP(modulo, %)
INTEGER_BINARY_OP(bitwise_and, &)
INTEGER_BINARY_OP(bitwise_or, |)
INTEGER_BINARY_OP(bitwise_xor, ^)

INTEGER_BINARY_BOOL_OP(equals, ==)
INTEGER_BINARY_BOOL_OP(not_equals, !=)
INTEGER_BINARY_BOOL_OP(less, <)
INTEGER_BINARY_BOOL_OP(less_equals, <=)
INTEGER_BINARY_BOOL_OP(greater, >)
INTEGER_BINARY_BOOL_OP(greater_equals, >=)

Integer integer_shift_left(Integer i1, Integer i2)
{
    Integer ret = { .type = i1.type };
    Integer shift = MUST_OPTIONAL(Integer, integer_coerce_to_unsigned(i2, U8));
    switch (ret.type) {
#undef INTEGER_SIZE
#define INTEGER_SIZE(sz)                  \
    case U##sz:                           \
        ret.u##sz = i1.u##sz << shift.u8; \
        break;                            \
    case I##sz:                           \
        ret.i##sz = i1.i##sz << shift.u8; \
        break;
        INTEGER_SIZES(INTEGER_SIZE)
#undef INTEGER_SIZE
    default:
        UNREACHABLE();
    }
    return ret;
}

Integer integer_shift_right(Integer i1, Integer i2)
{
    Integer ret = { .type = i1.type };
    Integer shift = MUST_OPTIONAL(Integer, integer_coerce_to_unsigned(i2, U8));
    switch (ret.type) {
#undef INTEGER_SIZE
#define INTEGER_SIZE(sz)                  \
    case U##sz:                           \
        ret.u##sz = i1.u##sz >> shift.u8; \
        break;                            \
    case I##sz:                           \
        ret.i##sz = i1.i##sz >> shift.u8; \
        break;
        INTEGER_SIZES(INTEGER_SIZE)
#undef INTEGER_SIZE
    default:
        UNREACHABLE();
    }
    return ret;
}

Integer integer_invert(Integer i)
{
    Integer ret = { .type = i.type };
    switch (ret.type) {
#undef INTEGER_SIZE
#define INTEGER_SIZE(sz)      \
    case U##sz:               \
        ret.u##sz = ~i.u##sz; \
        break;                \
    case I##sz:               \
        ret.i##sz = ~i.i##sz; \
        break;
        INTEGER_SIZES(INTEGER_SIZE)
#undef INTEGER_SIZE
    default:
        UNREACHABLE();
    }
    return ret;
}

Integer integer_negate(Integer i)
{
    assert((int) i.type < 0);
    Integer one = integer_create(i.type, 1);
    return integer_add(integer_invert(i), one);
}

Integer integer_increment(Integer i)
{
    Integer one = integer_create(i.type, 1);
    return integer_add(i, one);
}

Integer integer_decrement(Integer i)
{
    Integer one = integer_create(i.type, 1);
    return integer_subtract(i, one);
}
