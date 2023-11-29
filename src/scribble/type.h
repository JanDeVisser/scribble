/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdio.h>

#include <error_or.h>
#include <integer.h>
#include <optional.h>
#include <sv.h>

#ifndef __TYPE_H__
#define __TYPE_H__

typedef int (*qsort_fnc_t)(void const *, void const *);

#define TYPEKINDS(S)   \
    S(PRIMITIVE, 0x00) \
    S(AGGREGATE, 0x01) \
    S(VARIANT, 0x02)   \
    S(ALIAS, 0x04)     \
    S(ENUM, 0x08)

typedef enum /* : uint8_t */ {
#undef TYPEKINDS_ENUM
#define TYPEKINDS_ENUM(type, value) TK_##type = value,
    TYPEKINDS(TYPEKINDS_ENUM)
#undef TYPEKINDS_ENUM
} TypeKind;

#define COMPONENTKINDS(S) \
    S(TYPE)               \
    S(TEMPLATE_PARAM)     \
    S(PARAMETERIZED_TYPE)

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
//   0x0xxx - Builtin types
//   0x0100 - unsigned
//   0x0200 - integer
//   0x0400 - can be treated as integer
//   0x0xFF - Width in bits
//   0x00xx - Other types

//   0x1000 - Aggregate types
//   0x1F00 - Aggregate id
//   0x1x0F - Number of components

//   0x2000 - Variant types
//   0x4000 - Aliases
//   0x8000 - Enumerations
//
// Bottom 16 bits position in type registry.

#define TYPE_MASK 0x0FFFFFFF

#define UNSIGNED_MASK 0x0100
#define INTEGER_MASK 0x0200
#define LIKE_INTEGER_MASK 0x0400
#define ALL_INTEGERS_MASK 0x0600
#define WIDTH_MASK 0x00FF

#define BUILTINTYPES(S)             \
    S(VOID, void, 0x0001)           \
    S(SELF, self, 0x0002)           \
    S(PARAMETER, parameter, 0x0003) \
    S(ERROR, error, 0x0004)         \
    S(FLOAT, float, 0x0005)         \
    S(I8, i8, 0x0208)               \
    S(U8, u8, 0x0308)               \
    S(I16, i16, 0x0210)             \
    S(U16, u16, 0x0310)             \
    S(I32, i32, 0x0220)             \
    S(U32, u32, 0x0320)             \
    S(I64, i64, 0x0240)             \
    S(U64, u64, 0x0340)             \
    S(BOOL, bool, 0x0408)           \
    S(POINTER, ptr, 0x0440)         \
    S(STRING, string, 0x1102)       \
    S(RANGE, range, 0x1202)         \
    S(ARRAY, array, 0x1302)

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

#define INTEGERTYPES_WITH_BOOL(S) \
    S(U8)                         \
    S(I8)                         \
    S(U16)                        \
    S(I16)                        \
    S(U32)                        \
    S(I32)                        \
    S(U64)                        \
    S(I64)                        \
    S(BOOL)

typedef enum /* : uint16_t */ {
    BIT_NOTYPE = 0x0000,
#undef BUILTINTYPE_ENUM
#define BUILTINTYPE_ENUM(type, name, code) BIT_##type = code,
    BUILTINTYPES(BUILTINTYPE_ENUM)
#undef BUILTINTYPE_ENUM
} BuiltinType;

typedef uint32_t               type_id;
typedef struct expression_type ExpressionType;

typedef struct template_parameter {
    StringView        name;
    TemplateParamType type;
} TemplateParameter;

typedef struct template_argument {
    StringView        name;
    TemplateParamType arg_type;
    union {
        StringView string_value;
        int64_t    int_value;
        type_id    type;
    };
} TemplateArgument;

// Used for both TK_COMPOSITE, TK_VARIANT, and TK_ENUM
typedef struct type_component {
    ComponentKind kind;
    StringView    name;
    union {
        type_id    type_id;
        StringView param;
        Integer    enum_value;

        // Can only have a typecomponent which is parameterized using one
        // parameter. Update once we need more than one.
        struct {
            type_id    template_type;
            StringView parameter;
            StringView argument;
        } parameterized_type;
    };
} TypeComponent;

typedef struct {
    StringView name;
    Integer    value;
} EnumValue;

DA(EnumValue)
typedef DA_EnumValue EnumValues;

typedef struct expression_type {
    type_id            type_id;
    StringView         name;
    BuiltinType        builtin_type;
    size_t             num_parameters;
    TemplateParameter *template_parameters;
    type_id            specialization_of;
    size_t             num_arguments;
    TemplateArgument  *template_arguments;
    union {
        struct {
            size_t         num_components;
            TypeComponent *components;
        } components;
        struct {
            type_id underlying_type;
            DIA(EnumValue);
        } enumeration;
        type_id alias_for_id;
    };
} ExpressionType;

typedef struct {
    type_id type_id;
    bool    optional;
} TypeSpec;

typedef struct signature {
    size_t   argc;
    type_id *arg_types;
    type_id  ret_type;
} Signature;

#define BUILTINTYPE_ENUM(type, name, code) extern type_id type##_ID;
BUILTINTYPES(BUILTINTYPE_ENUM)
#undef BUILTINTYPE_ENUM
extern type_id PCHAR_ID;
extern type_id FIRST_CUSTOM_IX;
extern type_id NEXT_CUSTOM_IX;

ErrorOr(TypeID, type_id);
ErrorOr(Size, size_t);

extern char const        *TypeKind_name(TypeKind kind);
extern char const        *BuiltinType_name(BuiltinType type);
extern size_t             BuiltinType_width(BuiltinType type);
extern BuiltinType        BuiltinType_get_integer_type(size_t width, bool un_signed);
extern bool               BuiltinType_is_integer(BuiltinType type);
extern bool               BuiltinType_is_number(BuiltinType type);
extern bool               BuiltinType_is_unsigned(BuiltinType type);
extern ExpressionType    *type_registry_get_type_by_name(StringView name);
extern ExpressionType    *type_registry_get_type_by_id(type_id id);
extern ExpressionType    *type_registry_get_type_by_index(size_t ix);
extern type_id            type_registry_id_of_builtin_type(BuiltinType type);
extern type_id            type_registry_id_of_integer_type(IntegerSize bits, bool un_signed);
extern ErrorOrTypeID      type_registry_get_variant(size_t num, type_id *types);
extern ErrorOrTypeID      type_registry_get_variant2(type_id t1, type_id t2);
extern ErrorOrTypeID      type_registry_alias(StringView name, type_id aliased);
extern ErrorOrTypeID      type_registry_make_aggregate(StringView name, size_t num, TypeComponent *components);
extern ErrorOrTypeID      type_registry_make_enumeration(StringView name, type_id underlying_type, EnumValues *values);
extern type_id            typeid_canonical_type_id(type_id type);
extern type_id            typeid_underlying_type_id(type_id type);
extern ExpressionType    *typeid_canonical_type(type_id type);
extern void               type_registry_init();
extern bool               typespec_assignment_compatible(TypeSpec ts1, TypeSpec ts2);
extern StringView         typespec_name(TypeSpec typespec);
extern StringView         typespec_to_string(TypeSpec typespec);
extern void               typespec_print(FILE *f, TypeSpec typespec);
extern bool               type_is_concrete(ExpressionType *type);
extern ErrorOrSize        type_sizeof(ExpressionType *type);
extern ErrorOrSize        type_alignat(ExpressionType *type);
extern ErrorOrSize        type_offsetof_name(ExpressionType *type, StringView name);
extern ErrorOrSize        type_offsetof_index(ExpressionType *type, size_t index);
extern TemplateParameter *type_get_parameter(ExpressionType *type, StringView param);
extern TemplateArgument  *type_get_argument(ExpressionType *type, StringView arg);
extern TypeComponent     *type_get_component(ExpressionType *type, StringView component);
extern ErrorOrTypeID      type_set_template_parameters(type_id template_id, size_t num, TemplateParameter *parameters);
extern ErrorOrTypeID      type_specialize_template(type_id template_id, size_t num, TemplateArgument *arguments);
extern type_id            typeid_pointer_to(type_id type);
extern type_id            typeid_pointer_references(type_id type);

static inline TypeKind typeid_kind(type_id type)
{
    return (TypeKind) (type >> 28);
}

static inline StringView typeid_name(type_id type)
{
    return type_registry_get_type_by_id(type)->name;
}

static inline bool typeid_has_kind(type_id type, TypeKind kind)
{
    return typeid_kind(type) == kind;
}

static BuiltinType typeid_builtin_type(type_id type)
{
    return (BuiltinType) ((type >> 16) & 0xFFFF);
}

static size_t typeid_ix(type_id type)
{
    return type & 0x0000FFFF;
}

static size_t typeid_sizeof(type_id type)
{
    ExpressionType *et = type_registry_get_type_by_id(type);
    return MUST(Size, type_sizeof(et));
}

static size_t typeid_offsetof(type_id type, size_t index)
{
    ExpressionType *et = type_registry_get_type_by_id(type);
    return MUST(Size, type_offsetof_index(et, index));
}

static inline TypeKind type_kind(ExpressionType *type)
{
    return typeid_kind(type->type_id);
}

static inline bool type_has_kind(ExpressionType *type, TypeKind kind)
{
    return typeid_has_kind(type->type_id, kind);
}

static inline bool typeid_is_concrete(type_id type)
{
    ExpressionType *et = type_registry_get_type_by_id(type);
    return type_is_concrete(et);
}

static inline bool typeid_is_specialization(type_id type)
{
    ExpressionType *et = type_registry_get_type_by_id(type);
    return et->num_arguments > 0;
}

static inline type_id typeid_specializes(type_id type)
{
    ExpressionType *et = type_registry_get_type_by_id(type);
    return et->specialization_of;
}

static inline bool typeid_is_template(type_id type)
{
    ExpressionType *et = type_registry_get_type_by_id(type);
    return et->num_parameters > 0;
}

#endif /* __TYPE_H__ */
