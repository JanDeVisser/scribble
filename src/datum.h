/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <parser.h>
#include <type.h>

#ifndef __DATUM_H__
#define __DATUM_H__

#define DATUM_PRIMITIVETYPES(S)         \
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
    S(POINTER, pointer, void *)   \
    S(STRING, string, StringView) \
    S(BOOL, bool_value, bool)     \
    S(FLOAT, float_value, double)

typedef struct datum {
    type_id type;
    union {
#undef PRIMITIVETYPE
#define PRIMITIVETYPE(dt, n, ct) ct n;
        DATUM_PRIMITIVETYPES(PRIMITIVETYPE)
#undef PRIMITIVETYPE
        struct datum *components;
        struct {
            type_id       component_type;
            size_t        size;
            struct datum *components;
        } array;
        struct {
            type_id       holds_alternative;
            struct datum *value;
        } variant;
    };
} Datum;

extern Datum         datum_make_integer(size_t width, bool un_signed, int64_t signed_value, uint64_t unsigned_value);
extern unsigned long datum_unsigned_integer_value(Datum d);
extern long          datum_signed_integer_value(Datum d);
extern Datum         datum_copy(Datum d);
extern Datum         datum_apply(Datum d1, Operator op, Datum d2);
extern void          datum_print(Datum d);
extern StringView    datum_sprint(Datum d);
extern void          datum_free(Datum d);

#define datum_kind(d) typeid_kind((d).type)
#define datum_is_primitive(d) typeid_has_kind((d).type, TK_PRIMITIVE)
#define datum_is_composite(d) (datum_kind((d)) == TK_COMPOSITE)
#define datum_is_array(d) (datum_kind((d)) == TK_ARRAY)
#define datum_is_variant(d) (datum_kind((d)) == TK_VARIANT)
#define datum_is_integer(d) (datum_is_primitive(d) && PrimitiveType_is_integer((d).type))

#endif /* __DATUM_H__ */
