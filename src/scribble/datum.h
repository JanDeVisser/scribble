/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <parser.h>
#include <type.h>

#ifndef __DATUM_H__
#define __DATUM_H__

typedef struct error_info {
    char const *exception;
    void       *operation;
} ErrorInfo;

#define DATUM_PRIMITIVETYPES(S) \
    S(VOID, void_value, int)    \
    S(ERROR, error, ErrorInfo)  \
    S(U8, u8, uint8_t)          \
    S(I8, i8, int8_t)           \
    S(U16, u16, uint16_t)       \
    S(I16, i16, int16_t)        \
    S(U32, u32, uint32_t)       \
    S(I32, i32, int32_t)        \
    S(U64, u64, uint64_t)       \
    S(I64, i64, int64_t)        \
    S(POINTER, pointer, void *) \
    S(BOOL, bool_value, bool)   \
    S(FLOAT, float_value, double)

typedef struct datum {
    type_id type;
    union {
#undef PRIMITIVETYPE
#define PRIMITIVETYPE(dt, n, ct) ct n;
        DATUM_PRIMITIVETYPES(PRIMITIVETYPE)
#undef PRIMITIVETYPE
        StringView string;
        struct {
            size_t        num_components;
            struct datum *components;
        } aggregate;
        struct datum *variant;
    };
} Datum;

extern Datum        *datum_allocate(type_id type);
extern Datum        *datum_make_integer(Integer value);
extern unsigned long datum_unsigned_integer_value(Datum *d);
extern long          datum_signed_integer_value(Datum *d);
extern Datum        *datum_copy(Datum *dest, Datum *src);
extern Datum        *datum_apply(Datum *d1, Operator op, Datum *d2);
extern void          datum_print(Datum *d);
extern StringView    datum_sprint(Datum *d);
extern void          datum_free_contents(Datum *d);
extern void          datum_free(Datum *d);

static inline TypeKind datum_kind(Datum *d)
{
    return typeid_kind(d->type);
}

static inline bool datum_is_builtin(Datum *d)
{
    return typeid_has_kind(d->type, TK_PRIMITIVE);
}

static inline bool datum_is_composite(Datum *d)
{
    return typeid_has_kind(d->type, TK_AGGREGATE);
}

static inline bool datum_is_variant(Datum *d)
{
    return typeid_has_kind(d->type, TK_VARIANT);
}

static inline bool datum_is_integer(Datum *d)
{
    return (datum_is_builtin(d)) && BuiltinType_is_integer(typeid_builtin_type(d->type));
}

#endif /* __DATUM_H__ */
