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

// 0x0100 - unsigned
// 0x0200 - integer
// 0x0400 - can be treated as integer
// 0x0000 - Other types
// 0x00FF - Width in bits

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
        PrimitiveType primitive_type;
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

typedef struct signature {
    size_t           argc;
    ExpressionType **types;
    ExpressionType  *ret_type;
} Signature;

#define PRIMITIVETYPE_ENUM(type, name, code) extern type_id type##_ID;
PRIMITIVETYPES(PRIMITIVETYPE_ENUM)
#undef PRIMITIVETYPE_ENUM

extern char const     *PrimitiveType_name(PrimitiveType type);
extern size_t          PrimitiveType_width(PrimitiveType type);
extern PrimitiveType   PrimitiveType_get_integer_type(size_t width, bool un_signed);
extern bool            PrimitiveType_is_integer(PrimitiveType type);
extern bool            PrimitiveType_is_number(PrimitiveType type);
extern bool            PrimitiveType_is_unsigned(PrimitiveType type);
extern ExpressionType *type_registry_get_type_by_name(StringView name);
extern ExpressionType *type_registry_get_type_by_id(type_id id);
extern type_id         type_registry_id_of_primitive_type(PrimitiveType type);
extern type_id         type_registry_id_of_integer_type(size_t width, bool un_signed);
extern type_id         type_registry_get_variant(type_id num, type_id *types);
extern type_id         type_registry_get_variant2(type_id t1, type_id t2);
extern type_id         type_registry_make_struct(StringView name, size_t num, StringView *names, type_id *types);
extern type_id         type_registry_canonical_type_id(type_id type);
extern ExpressionType *type_registry_canonical_type(type_id type);
extern void            type_registry_init();
extern bool            typespec_assignment_compatible(TypeSpec ts1, TypeSpec ts2);
extern StringView      typespec_name(TypeSpec typespec);
extern void            typespec_print(FILE *f, TypeSpec typespec);
extern size_t          type_sizeof(ExpressionType *type);
extern size_t          type_alignat(ExpressionType *type);

#endif /* __TYPE_H__ */
