/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <optional.h>
#include <sv.h>

#ifndef __INTEGER_H__
#define __INTEGER_H__

#define INTEGER_SIZES(S) S(8) S(16) S(32) S(64)

typedef enum integer_type {
    IU_NO_SUCH_TYPE = 0,
#undef INTEGER_SIZE
#define INTEGER_SIZE(sz) \
    I##sz = -sz,         \
    U##sz = sz,
    INTEGER_SIZES(INTEGER_SIZE)
#undef INTEGER_SIZE
} IntegerType;

typedef struct integer {
    IntegerType type;
    union {
#undef INTEGER_SIZE
#define INTEGER_SIZE(sz) \
    int##sz##_t  i##sz;  \
    uint##sz##_t u##sz;
        INTEGER_SIZES(INTEGER_SIZE)
#undef INTEGER_SIZE
    };
} Integer;

OPTIONAL(Integer);

inline static char const *IntegerType_name(IntegerType type)
{
    switch (type) {
#undef INTEGER_SIZE
#define INTEGER_SIZE(sz) \
    case I##sz:          \
        return "i" #sz;  \
    case U##sz:          \
        return "u" #sz;
        INTEGER_SIZES(INTEGER_SIZE)
#undef INTEGER_SIZE
    default:
        fprintf(stderr, "Unknown integer type: %d\n", type);
        exit(1);
    }
}

struct string_view;
extern IntegerType IntegerType_from_name(struct string_view name);

static inline bool IntegerType_is_signed(IntegerType type)
{
    return (int) type < 0;
}

static inline bool IntegerType_is_unsigned(IntegerType type)
{
    return (int) type > 0;
}

extern Integer         integer_create(IntegerType type, uint64_t value);
extern OptionalInt64   integer_signed_value(Integer i);
extern OptionalUInt64  integer_unsigned_value(Integer i);
extern OptionalInteger integer_coerce_to_signed(Integer i, IntegerType type);
extern OptionalInteger integer_coerce_to_unsigned(Integer i, IntegerType type);
extern OptionalInteger integer_coerce_to(Integer i, IntegerType type);
extern Integer         integer_add(Integer i1, Integer i2);
extern Integer         integer_subtract(Integer i1, Integer i2);
extern Integer         integer_multiply(Integer i1, Integer i2);
extern Integer         integer_divide(Integer i1, Integer i2);
extern Integer         integer_modulo(Integer i1, Integer i2);
extern bool            integer_equals(Integer i1, Integer i2);
extern bool            integer_not_equals(Integer i1, Integer i2);
extern bool            integer_less(Integer i1, Integer i2);
extern bool            integer_less_equals(Integer i1, Integer i2);
extern bool            integer_greater(Integer i1, Integer i2);
extern bool            integer_greater_equals(Integer i1, Integer i2);
extern Integer         integer_bitwise_and(Integer i1, Integer i2);
extern Integer         integer_bitwise_or(Integer i1, Integer i2);
extern Integer         integer_bitwise_xor(Integer i1, Integer i2);
extern Integer         integer_shift_left(Integer i1, Integer i2);
extern Integer         integer_shift_right(Integer i1, Integer i2);
extern Integer         integer_negate(Integer i);
extern Integer         integer_invert(Integer i);
extern Integer         integer_increment(Integer i);
extern Integer         integer_decrement(Integer i);

#undef INTEGER_SIZE
#define INTEGER_SIZE(sz)                        \
    static inline Integer i##sz(int64_t value)  \
    {                                           \
        return integer_create(I##sz, value);    \
    }                                           \
    static inline Integer u##sz(uint64_t value) \
    {                                           \
        return integer_create(U##sz, value);    \
    }
INTEGER_SIZES(INTEGER_SIZE)
#undef INTEGER_SIZE

static inline bool integer_is_signed(Integer i)
{
    return IntegerType_is_signed(i.type);
}

static inline bool integer_is_unsigned(Integer i)
{
    return IntegerType_is_unsigned(i.type);
}

#endif /* __INTEGER_H__ */
