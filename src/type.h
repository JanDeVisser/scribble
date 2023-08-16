/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdio.h>

#include <sv.h>

#ifndef __TYPE_H__
#define __TYPE_H__

#define TYPEKINDS(S) \
    S(TK_PRIMITIVE)  \
    S(TK_COMPOSITE)  \
    S(TK_VARIANT)    \
    S(TK_ARRAY)      \
    S(TK_ALIAS)

typedef enum {
#undef TYPEKINDS_ENUM
#define TYPEKINDS_ENUM(type) type,
    TYPEKINDS(TYPEKINDS_ENUM)
#undef TYPEKINDS_ENUM
} TypeKind;

#define PRIMITIVETYPES(S)       \
    S(VOID, void, 0, false)     \
    S(I8, i8, 8, false)         \
    S(U8, u8, 8, true)          \
    S(I16, i16, 16, false)      \
    S(U16, u16, 16, true)       \
    S(I32, i32, 32, false)      \
    S(U32, u32, 32, true)       \
    S(I64, i64, 64, false)      \
    S(U64, u64, 64, true)       \
    S(STRING, string, 0, false) \
    S(BOOL, bool, 8, false)     \
    S(POINTER, ptr, 64, false)  \
    S(FLOAT, float, 64, false)

typedef enum {
#undef PRIMITIVETYPE_ENUM
#define PRIMITIVETYPE_ENUM(type, name, width, un_signed) PT_ ## type,
    PRIMITIVETYPES(PRIMITIVETYPE_ENUM)
#undef PRIMITIVETYPE_ENUM
        PT_COUNT
} PrimitiveType;

typedef size_t                 type_id;
typedef struct expression_type ExpressionType;

// Used for both TK_COMPOSITE and TK_VARIANT.
typedef struct type_component {
    StringView             name;
    type_id                type_id;
    struct type_component *next;
} TypeComponent;

typedef struct expression_type {
    type_id    type_id;
    StringView name;
    TypeKind   kind;
    union {
        struct {
            PrimitiveType type;
            size_t        width;
            bool          un_signed;
        } primitive;
        TypeComponent component;
        struct {
            size_t base_type;
            size_t size;
        } array;
        size_t alias_for_id;
    };
} ExpressionType;

typedef struct {
    type_id type_id;
    bool    optional;
} TypeSpec;

#define PRIMITIVETYPE_ENUM(type, name, width, un_signed) extern type_id type ## _ID;
    PRIMITIVETYPES(PRIMITIVETYPE_ENUM)
#undef PRIMITIVETYPE_ENUM

extern char const     *PrimitiveType_name(PrimitiveType type);
extern size_t          PrimitiveType_width(PrimitiveType type);
extern bool            PrimitiveType_is_unsigned(PrimitiveType type);
extern ExpressionType *type_registry_get_type_by_name(StringView name);
extern ExpressionType *type_registry_get_type_by_id(type_id id);
extern type_id         type_registry_id_of_primitive_type(PrimitiveType type);
extern type_id         type_registry_id_of_integer_type(size_t width, bool un_signed);
extern type_id         type_registry_get_variant(type_id num, type_id *types);
extern type_id         type_registry_get_variant2(type_id t1, type_id t2);
extern type_id         type_registry_make_struct(StringView name, size_t num, StringView *names, type_id *types);
extern type_id         type_registry_canonical_type(type_id type);
extern void            type_registry_init();
extern bool            typespec_assignment_compatible(TypeSpec ts1, TypeSpec ts2);
extern StringView      typespec_name(TypeSpec typespec);
extern void            typespec_print(FILE *f, TypeSpec typespec);

#endif /* __TYPE_H__ */
