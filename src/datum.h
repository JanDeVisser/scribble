/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <parser.h>

#ifndef __DATUM_H__
#define __DATUM_H__

#define DATUMTYPES(S)             \
    S(VOID, void_value, int)      \
    S(ERROR, error, char const *) \
    S(U8, u8, uint8_t)            \
    S(I8, i8, int8_t)             \
    S(U16, u16, uint16_t)         \
    S(I16, i16, int16_t)          \
    S(U32, u32, uint32_t)         \
    S(I32, i32, int32_t)          \
    S(U64, u64, uint64_t)         \
    S(I64, i64, int64_t)          \
    S(FLOAT, float_value, double) \
    S(STRING, string, StringView) \
    S(POINTER, pointer, void *)   \
    S(BOOL, bool_value, bool)

#define NUMERICTYPES(S)   \
    S(U8, u8, uint8_t)    \
    S(I8, i8, int8_t)     \
    S(U16, u16, uint16_t) \
    S(I16, i16, int16_t)  \
    S(U32, u32, uint32_t) \
    S(I32, i32, int32_t)  \
    S(U64, u64, uint64_t) \
    S(I64, i64, int64_t)  \
    S(FLOAT, float_value, double)

#define INTEGERTYPES(S)                    \
    S(U8, u8, uint8_t, false, "hhu", 1)    \
    S(I8, i8, int8_t, true, "hhd", 1)      \
    S(U16, u16, uint16_t, false, "hu", 2)  \
    S(I16, i16, int16_t, true, "hd", 2)    \
    S(U32, u32, uint32_t, false, "u", 4)   \
    S(I32, i32, int32_t, true, "d", 4)     \
    S(U64, u64, uint64_t, false, "llu", 8) \
    S(I64, i64, int64_t, true, "lld", 8)

typedef enum datum_type {
#undef DATUMTYPE
#define DATUMTYPE(dt, n, ct) DT_##dt,
    DATUMTYPES(DATUMTYPE)
#undef DATUMTYPE
} DatumType;

typedef struct datum {
    DatumType type;
    union {
#undef DATUMTYPE
#define DATUMTYPE(dt, n, ct) ct n;
        DATUMTYPES(DATUMTYPE)
#undef DATUMTYPE
    };
} Datum;

char const   *DatumType_name(DatumType dt);
DatumType     DatumType_get_integer_type(size_t width, bool un_signed);
bool          DatumType_is_integer(DatumType dt);
bool          DatumType_is_number(DatumType dt);
Datum         datum_make_integer(size_t width, bool un_signed, int64_t signed_value, uint64_t unsigned_value);
unsigned long datum_unsigned_integer_value(Datum d);
long          datum_signed_integer_value(Datum d);
Datum         datum_apply(Datum d1, Operator op, Datum d2);
void          datum_print(Datum d);

#endif /* __DATUM_H__ */
