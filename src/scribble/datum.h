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

typedef struct datum_pointer {
    struct datum *pointer;
    DIA_ELEMENTS(size_t, components);
} DatumPointer;

#define DATUM_NONINTEGERPRIMITIVES(S)           \
    S(VOID, void_value, int)                    \
    S(ERROR, error, ErrorInfo)                  \
    S(VAR_POINTER, datum_pointer, DatumPointer) \
    S(RAW_POINTER, raw_pointer, void *)         \
    S(BOOL, bool_value, bool)                   \
    S(FLOAT, float_value, double)               \
    S(STRING, string, StringView)

typedef struct datum {
    type_id type;
    union {
#undef NONINTEGERPRIMITIVE
#define NONINTEGERPRIMITIVE(bit, field, ctype) ctype field;
        DATUM_NONINTEGERPRIMITIVES(NONINTEGERPRIMITIVE)
#undef NONINTEGERPRIMITIVE
        Integer integer;
        struct {
            size_t        num_components;
            struct datum *components;
        } aggregate;
        struct {
            struct datum *tag;
            struct datum *payload;
        } variant;
    };
} Datum;

extern Datum        *datum_allocate(type_id type);
Datum               *datum_initialize(Datum *d);
Datum               *datum_clone(Datum *d);
Datum               *datum_clone_into(Datum *into, Datum *from);
extern Datum        *datum_make_integer(Integer value);
extern unsigned long datum_unsigned_integer_value(Datum *d);
extern long          datum_signed_integer_value(Datum *d);
extern Datum        *datum_copy(Datum *dest, Datum *src);
extern Datum        *datum_apply(Datum *d1, Operator op, Datum *d2);
extern void          datum_print(Datum *d);
extern StringView    datum_sprint(Datum *d);
extern void          datum_free_contents(Datum *d);
extern void          datum_free(Datum *d);
extern void          datums_free(Datum *d, ...);
extern void          datums_vfree(Datum *d, const va_list args);

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
