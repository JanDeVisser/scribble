/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <integer.h>
#include <log.h>

Integer integer_create(IntegerSize size, bool un_signed, uint64_t value)
{
    Integer ret = { .un_signed = un_signed, .size = size };
    switch (size) {
#undef INTEGER_SIZE
#define INTEGER_SIZE(sz)                      \
    case sz:                                  \
        if (ret.un_signed) {                  \
            ret.u##sz = (uint##sz##_t) value; \
        } else {                              \
            ret.i##sz = (int##sz##_t) value;  \
        }                                     \
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
    int64_t value;
    switch (i.size) {
    case BITS_8:
        return OptionalInt64_create((i.un_signed) ? i.u8 : i.i8);
    case BITS_16:
        return OptionalInt64_create((i.un_signed) ? i.u16 : i.i16);
    case BITS_32:
        return OptionalInt64_create((i.un_signed) ? i.u32 : i.i32);
    case BITS_64:
        if (i.un_signed) {
            if (i.u64 <= INT64_MAX) {
                return OptionalInt64_create((int64_t) i.u64);
            } else {
                return OptionalInt64_empty();
            }
        } else {
            return OptionalInt64_create(i.i64);
        }
    default:
        UNREACHABLE();
    }
}

OptionalUInt64 integer_unsigned_value(Integer i)
{
    int64_t value;
    switch (i.size) {
    case BITS_8:
        return OptionalUInt64_create((i.un_signed) ? i.u8 : i.i8);
    case BITS_16:
        return OptionalUInt64_create((i.un_signed) ? i.u16 : i.i16);
    case BITS_32:
        return OptionalUInt64_create((i.un_signed) ? i.u32 : i.i32);
    case BITS_64:
        if (i.un_signed) {
            return OptionalUInt64_create(i.u64);
        } else {
            if (i.i64 >= 0) {
                return OptionalUInt64_create(i.i64);
            } else {
                return OptionalUInt64_empty();
            }
        }
    default:
        UNREACHABLE();
    }
}

OptionalInteger integer_coerce_to_unsigned(Integer i, IntegerSize size)
{
    OptionalUInt64 value_maybe = integer_unsigned_value(i);
    if (!value_maybe.has_value) {
        return OptionalInteger_empty();
    }
    uint64_t value = value_maybe.value;
    Integer  coerced = { 0 };
    coerced.un_signed = true;
    coerced.size = size;
    switch (size) {
    case BITS_8:
        if (value > UINT8_MAX) {
            return OptionalInteger_empty();
        }
        coerced.u8 = (uint8_t) value;
        break;
    case BITS_16:
        if (value > UINT16_MAX) {
            return OptionalInteger_empty();
        }
        coerced.u16 = (uint16_t) value;
        break;
    case BITS_32:
        if (value > UINT32_MAX) {
            return OptionalInteger_empty();
        }
        coerced.u32 = (uint32_t) value;
        break;
    case BITS_64:
        coerced.u64 = value;
        break;
    default:
        UNREACHABLE();
    }
    return OptionalInteger_create(coerced);
}

OptionalInteger integer_coerce_to_signed(Integer i, IntegerSize size)
{
    OptionalInt64 value_maybe = integer_signed_value(i);
    if (!value_maybe.has_value) {
        return OptionalInteger_empty();
    }
    int64_t value = value_maybe.value;
    Integer coerced = { 0 };
    coerced.un_signed = false;
    coerced.size = size;
    switch (size) {
    case BITS_8:
        if (value < INT8_MIN || value > INT8_MAX) {
            return OptionalInteger_empty();
        }
        coerced.i8 = (int8_t) value;
        break;
    case BITS_16:
        if (value < INT16_MIN || value > INT16_MAX) {
            return OptionalInteger_empty();
        }
        coerced.i16 = (int16_t) value;
        break;
    case BITS_32:
        if (value < INT32_MIN || value > INT32_MAX) {
            return OptionalInteger_empty();
        }
        coerced.i32 = (int32_t) value;
        break;
    case BITS_64:
        coerced.i64 = value;
        break;
    default:
        UNREACHABLE();
    }
    return OptionalInteger_create(coerced);
}

OptionalInteger integer_coerce_to(Integer i, IntegerSize size, bool un_signed)
{
    return (un_signed) ? integer_coerce_to_unsigned(i, size) : integer_coerce_to_signed(i, size);
}

#define INTEGER_BINARY_OP(fnc, op)                                                     \
    Integer integer_##fnc(Integer i1, Integer i2)                                      \
    {                                                                                  \
        assert(i1.un_signed == i2.un_signed);                                          \
        if (i1.size > i2.size) {                                                       \
            i2 = MUST_OPTIONAL(Integer, integer_coerce_to(i2, i1.size, i1.un_signed)); \
        } else if (i2.size > i1.size) {                                                \
            i1 = MUST_OPTIONAL(Integer, integer_coerce_to(i1, i2.size, i2.un_signed)); \
        }                                                                              \
        Integer ret = { .un_signed = i1.un_signed, .size = i1.size };                  \
        switch (ret.size) {                                                            \
        case BITS_8:                                                                   \
            if (ret.un_signed) {                                                       \
                ret.u8 = i1.u8 op i2.u8;                                               \
            } else {                                                                   \
                ret.i8 = i1.i8 op i2.i8;                                               \
            }                                                                          \
            break;                                                                     \
        case BITS_16:                                                                  \
            if (ret.un_signed) {                                                       \
                ret.u16 = i1.u16 op i2.u16;                                            \
            } else {                                                                   \
                ret.i16 = i1.i16 op i2.i16;                                            \
            }                                                                          \
            break;                                                                     \
        case BITS_32:                                                                  \
            if (ret.un_signed) {                                                       \
                ret.u32 = i1.u32 op i2.u32;                                            \
            } else {                                                                   \
                ret.i32 = i1.i32 op i2.i32;                                            \
            }                                                                          \
            break;                                                                     \
        case BITS_64:                                                                  \
            if (ret.un_signed) {                                                       \
                ret.u64 = i1.u64 op i2.u64;                                            \
            } else {                                                                   \
                ret.i64 = i1.i64 op i2.i64;                                            \
            }                                                                          \
            break;                                                                     \
        default:                                                                       \
            UNREACHABLE();                                                             \
        }                                                                              \
        return ret;                                                                    \
    }

#define INTEGER_BINARY_BOOL_OP(fnc, op)                                                \
    bool integer_##fnc(Integer i1, Integer i2)                                         \
    {                                                                                  \
        assert(i1.un_signed == i2.un_signed);                                          \
        if (i1.size > i2.size) {                                                       \
            i2 = MUST_OPTIONAL(Integer, integer_coerce_to(i2, i1.size, i1.un_signed)); \
        } else if (i2.size > i1.size) {                                                \
            i1 = MUST_OPTIONAL(Integer, integer_coerce_to(i1, i2.size, i2.un_signed)); \
        }                                                                              \
        switch (i1.size) {                                                             \
        case BITS_8:                                                                   \
            return (i1.un_signed) ? (i1.u8 op i2.u8) : (i1.i8 op i2.i8);               \
        case BITS_16:                                                                  \
            return (i1.un_signed) ? (i1.u16 op i2.u16) : (i1.i16 op i2.i16);           \
        case BITS_32:                                                                  \
            return (i1.un_signed) ? (i1.u32 op i2.u32) : (i1.i32 op i2.i32);           \
        case BITS_64:                                                                  \
            return (i1.un_signed) ? (i1.u64 op i2.u64) : (i1.i64 op i2.i64);           \
        default:                                                                       \
            UNREACHABLE();                                                             \
        }                                                                              \
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
    Integer ret = { .un_signed = i1.un_signed, .size = i1.size };
    Integer shift = MUST_OPTIONAL(Integer, integer_coerce_to_unsigned(i2, 8));
    switch (ret.size) {
#undef INTEGER_SIZE
#define INTEGER_SIZE(sz)                      \
    case BITS_##sz:                           \
        if (ret.un_signed) {                  \
            ret.u##sz = i1.u##sz << shift.u8; \
        } else {                              \
            ret.i##sz = i1.i##sz << shift.u8; \
        }                                     \
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
    Integer ret = { .un_signed = i1.un_signed, .size = i1.size };
    Integer shift = MUST_OPTIONAL(Integer, integer_coerce_to_unsigned(i2, 8));
    switch (ret.size) {
#undef INTEGER_SIZE
#define INTEGER_SIZE(sz)                      \
    case BITS_##sz:                           \
        if (ret.un_signed) {                  \
            ret.u##sz = i1.u##sz >> shift.u8; \
        } else {                              \
            ret.i##sz = i1.i##sz >> shift.u8; \
        }                                     \
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
    Integer ret = { .un_signed = i.un_signed, .size = i.size };
    switch (ret.size) {
#undef INTEGER_SIZE
#define INTEGER_SIZE(sz)          \
    case sz:                      \
        if (ret.un_signed) {      \
            ret.u##sz = ~i.u##sz; \
        } else {                  \
            ret.i##sz = ~i.i##sz; \
        }                         \
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
    assert(!i.un_signed);
    Integer one = integer_create(i.size, i.un_signed, 1);
    return integer_add(integer_invert(i), one);
}

Integer integer_increment(Integer i)
{
    Integer one = integer_create(i.size, i.un_signed, 1);
    return integer_add(i, one);
}

Integer integer_decrement(Integer i)
{
    Integer one = integer_create(i.size, i.un_signed, 1);
    return integer_subtract(i, one);
}
