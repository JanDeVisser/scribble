/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdio.h>

#include <error.h>
#include <sv.h>

#ifndef __TYPE_H__
#define __TYPE_H__

typedef void (*void_t)();
typedef int (*qsort_fnc_t)(void const *, void const *);

#define TYPEKINDS(S)         \
    S(PRIMITIVE, 0x00) \
    S(COMPOSITE, 0x01) \
    S(VARIANT, 0x02)   \
    S(ARRAY, 0x04)     \
    S(ALIAS, 0x08)

typedef enum : uint8_t {
#undef TYPEKINDS_ENUM
#define TYPEKINDS_ENUM(type, value) TK_##type = value,
    TYPEKINDS(TYPEKINDS_ENUM)
#undef TYPEKINDS_ENUM
} TypeKind;

#define COMPONENTKINDS(S) \
    S(TYPE)               \
    S(TEMPLATE_PARAM)

typedef enum {
#undef COMPONENTKINDS_ENUM
#define COMPONENTKINDS_ENUM(component_kind) CK_##component_kind,
    COMPONENTKINDS(COMPONENTKINDS_ENUM)
#undef COMPONENTKINDS_ENUM
} ComponentKind;

#define TEMPLATEPARAMTYPES(S) \
    S(TYPE)                   \
    S(NUMBER)                 \
    S(STRING)

typedef enum {
#undef TEMPLATEPARAMTYPES_ENUM
#define TEMPLATEPARAMTYPES_ENUM(param_type) TPT_##param_type,
    TEMPLATEPARAMTYPES(TEMPLATEPARAMTYPES_ENUM)
#undef TEMPLATEPARAMTYPES_ENUM
        TPT_MAX,
} TemplateParamType;

// type_id is 32 bit unsigned int.
// top 16 bits:
//   0x0xxx - Primitive types
//   0x0100 - unsigned
//   0x0200 - integer
//   0x0400 - can be treated as integer
//   0x0000 - Other types
//   0x00FF - Width in bits
//   0x1000 - Composite types
//   0x2000 - Array types
//   0x4000 - Variant types
//   0x8000 - Array types
//
// Bottom 16 bits position in type registry.

#define TYPE_MASK 0x0FFFFFFF

#define UNSIGNED_MASK 0x0100
#define INTEGER_MASK 0x0200
#define LIKE_INTEGER_MASK 0x0400
#define ALL_INTEGERS_MASK 0x0600
#define WIDTH_MASK 0x00FF

#define PRIMITIVETYPES(S)     \
    S(VOID, void, 0x0000)     \
    S(ERROR, error, 0x0001)   \
    S(STRING, string, 0x0002) \
    S(FLOAT, float, 0x0004)   \
    S(I8, i8, 0x0208)         \
    S(U8, u8, 0x0308)         \
    S(I16, i16, 0x0210)       \
    S(U16, u16, 0x0310)       \
    S(I32, i32, 0x0220)       \
    S(U32, u32, 0x0320)       \
    S(I64, i64, 0x0240)       \
    S(U64, u64, 0x0340)       \
    S(BOOL, bool, 0x0408)     \
    S(POINTER, ptr, 0x0440)

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

typedef enum {
#undef PRIMITIVETYPE_ENUM
#define PRIMITIVETYPE_ENUM(type, name, code) PT_##type = code,
    PRIMITIVETYPES(PRIMITIVETYPE_ENUM)
#undef PRIMITIVETYPE_ENUM
        PT_COUNT
} PrimitiveType;


typedef uint32_t               type_id;
typedef struct expression_type ExpressionType;

// Used for both TK_COMPOSITE and TK_VARIANT.
typedef struct type_component {
    ComponentKind kind;
    StringView    name;
    union {
        type_id    type_id;
        StringView param;
    };
} TypeComponent;

typedef struct template_parameter {
    StringView        name;
    TemplateParamType type;
} TemplateParameter;

typedef struct template_argument {
    StringView        name;
    TemplateParamType param_type;
    union {
        StringView string_value;
        int64_t    int_value;
        type_id    type;
    };
} TemplateArgument;

typedef struct expression_type {
    type_id            type_id;
    StringView         name;
    size_t             num_parameters;
    TemplateParameter *template_parameters;
    type_id            specialization_of;
    size_t             num_arguments;
    TemplateArgument  *template_arguments;
    union {
        PrimitiveType primitive_type;
        struct {
            size_t         num_components;
            TypeComponent *components;
        } components;
        struct {
            TypeComponent base_type;
            size_t        size;
        } array;
        type_id alias_for_id;
    };
} ExpressionType;

typedef struct {
    type_id type_id;
    bool    optional;
} TypeSpec;

typedef struct signature {
    size_t           argc;
    ExpressionType **types;
    ExpressionType  *ret_type;
} Signature;

#define PRIMITIVETYPE_ENUM(type, name, code) extern type_id type##_ID;
PRIMITIVETYPES(PRIMITIVETYPE_ENUM)
#undef PRIMITIVETYPE_ENUM
extern type_id RANGE_ID;

ErrorOr(TypeID, type_id);
ErrorOr(Size, size_t);

extern char const        *TypeKind_name(TypeKind kind);
extern char const        *PrimitiveType_name(PrimitiveType type);
extern size_t             PrimitiveType_width(PrimitiveType type);
extern PrimitiveType      PrimitiveType_get_integer_type(size_t width, bool un_signed);
extern bool               PrimitiveType_is_integer(PrimitiveType type);
extern bool               PrimitiveType_is_number(PrimitiveType type);
extern bool               PrimitiveType_is_unsigned(PrimitiveType type);
extern ExpressionType    *type_registry_get_type_by_name(StringView name);
extern ExpressionType    *type_registry_get_type_by_id(type_id id);
extern type_id            type_registry_id_of_primitive_type(PrimitiveType type);
extern type_id            type_registry_id_of_integer_type(size_t width, bool un_signed);
extern ErrorOrTypeID      type_registry_get_variant(size_t num, type_id *types);
extern ErrorOrTypeID      type_registry_get_variant2(type_id t1, type_id t2);
extern ErrorOrTypeID      type_registry_alias(StringView name, type_id aliased);
extern ErrorOrTypeID      type_registry_make_type(StringView name, TypeKind kind);
extern type_id            typeid_canonical_type_id(type_id type);
extern ExpressionType    *typeid_canonical_type(type_id type);
extern void               type_registry_init();
extern bool               typespec_assignment_compatible(TypeSpec ts1, TypeSpec ts2);
extern StringView         typespec_name(TypeSpec typespec);
extern void               typespec_print(FILE *f, TypeSpec typespec);
extern ErrorOrSize        type_sizeof(ExpressionType *type);
extern ErrorOrSize        type_alignat(ExpressionType *type);
extern TemplateParameter *type_get_parameter(ExpressionType *type, StringView param);
extern TemplateArgument  *type_get_argument(ExpressionType *type, StringView arg);
extern ErrorOrTypeID      type_set_struct_components(type_id struct_id, size_t num, TypeComponent *components);
extern ErrorOrTypeID      type_set_template_parameters(type_id template_id, size_t num, TemplateParameter *parameters);
extern ErrorOrTypeID      type_specialize_template(type_id template_id, size_t num, TemplateArgument *arguments);

static inline TypeKind typeid_kind(type_id type)
{
    return (TypeKind) (type >> 28);
}

static inline bool typeid_has_kind(type_id type, TypeKind kind)
{
    return typeid_kind(type) == kind;
}

static PrimitiveType typeid_primitive_type(type_id type)
{
    assert(typeid_kind(type) == TK_PRIMITIVE);
    return (PrimitiveType) ((type >> 16) & 0x0FFF);
}

static size_t typeid_ix(type_id type)
{
    return type & 0x0000FFFF;
}

static inline TypeKind type_kind(ExpressionType *type)
{
    return typeid_kind(type->type_id);
}

static inline bool type_has_kind(ExpressionType *type, TypeKind kind)
{
    return typeid_has_kind(type->type_id, kind);
}

#endif /* __TYPE_H__ */
