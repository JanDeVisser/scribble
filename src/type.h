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

#define PRIMITIVETYPES(S) \
    S(PT_VOID, void)      \
    S(PT_INT, int)        \
    S(PT_STRING, string)  \
    S(PT_BOOL, bool)      \
    S(PT_POINTER, void *) \
    S(PT_FLOAT, float)

typedef enum {
#undef PRIMITIVETYPE_ENUM
#define PRIMITIVETYPE_ENUM(type, name) type,
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
        PrimitiveType primitive;
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

extern char const     *PrimitiveType_name(PrimitiveType type);
extern ExpressionType *type_registry_get_type_by_name(StringView name);
extern ExpressionType *type_registry_get_type_by_id(type_id id);
extern type_id         type_registry_id_of_primitive_type(PrimitiveType type);
extern type_id         type_registry_get_variant(type_id num, type_id *types);
extern type_id         type_registry_get_variant2(type_id t1, type_id t2);
extern type_id         type_registry_make_struct(StringView name, size_t num, StringView *names, type_id *types);
extern void            type_registry_init();
extern bool            typespec_assignment_compatible(TypeSpec ts1, TypeSpec ts2);
extern StringView      typespec_name(TypeSpec typespec);
extern void            typespec_print(FILE *f, TypeSpec typespec);

#endif /* __TYPE_H__ */
