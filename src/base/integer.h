/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdint.h>

#include <optional.h>

#ifndef __INTEGER_H__
#define __INTEGER_H__

#define INTEGER_SIZES(S) S(8) S(16) S(32) S(64)

typedef enum integer_size {
#undef INTEGER_SIZE
#define INTEGER_SIZE(sz) \
    BITS_##sz = sz,
    INTEGER_SIZES(INTEGER_SIZE)
#undef INTEGER_SIZE
} IntegerSize;

typedef struct integer {
    IntegerSize size;
    bool        un_signed;
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

extern Integer         integer_create(IntegerSize size, bool un_signed, uint64_t value);
extern OptionalInt64   integer_signed_value(Integer i);
extern OptionalUInt64  integer_unsigned_value(Integer i);
extern OptionalInteger integer_coerce_to_signed(Integer i, IntegerSize size);
extern OptionalInteger integer_coerce_to_unsigned(Integer i, IntegerSize size);
extern OptionalInteger integer_coerce_to(Integer i, IntegerSize size, bool un_signed);
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

#endif /* __INTEGER_H__ */
