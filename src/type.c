/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <log.h>
#include <sv.h>
#include <type.h>

#define STATIC_ALLOCATOR
#include <allocate.h>

static ExpressionType *type_registry_next_type(TypeKind kind, StringView name);
static type_id         type_registry_add_primitive(StringView name, PrimitiveType primitive_type, size_t width, bool un_signed);

typedef struct {
    size_t          size;
    size_t          capacity;
    ExpressionType *types;
    size_t          components_size;
    size_t          components_capacity;
    TypeComponent  *components;
} TypeRegistry;

static TypeRegistry type_registry = { 0 };

#define PRIMITIVETYPE_ENUM(type, name, width, un_signed) type_id type ## _ID = 0;
    PRIMITIVETYPES(PRIMITIVETYPE_ENUM)
#undef PRIMITIVETYPE_ENUM

char const *PrimitiveType_name(PrimitiveType type)
{
    switch (type) {
#undef PRIMITIVETYPE_ENUM
#define PRIMITIVETYPE_ENUM(type, name, w, u) \
    case PT_##type:                          \
        return #name;
        PRIMITIVETYPES(PRIMITIVETYPE_ENUM)
#undef PRIMITIVETYPE_ENUM
    default:
        UNREACHABLE();
    }
}

size_t PrimitiveType_width(PrimitiveType type)
{
    switch (type) {
#undef PRIMITIVETYPE_ENUM
#define PRIMITIVETYPE_ENUM(type, name, w, u) \
    case PT_##type:                          \
        return w;
        PRIMITIVETYPES(PRIMITIVETYPE_ENUM)
#undef PRIMITIVETYPE_ENUM
    default:
        UNREACHABLE();
    }
}

bool PrimitiveType_is_unsigned(PrimitiveType type)
{
    switch (type) {
#undef PRIMITIVETYPE_ENUM
#define PRIMITIVETYPE_ENUM(type, name, w, u) \
    case PT_##type:                          \
        return u;
        PRIMITIVETYPES(PRIMITIVETYPE_ENUM)
#undef PRIMITIVETYPE_ENUM
    default:
        UNREACHABLE();
    }
}

ExpressionType *type_registry_next_type(TypeKind kind, StringView name)
{
    if (type_registry.capacity <= type_registry.size + 1) {
        OUT_OF_MEMORY("Could not expand Type Registry");
    }
    ExpressionType *type = &type_registry.types[type_registry.size];
    type->kind = kind;
    type->name = name;
    type->type_id = type_registry.size;
    ++type_registry.size;
    return type;
}

type_id type_registry_add_primitive(StringView name, PrimitiveType primitive_type, size_t width, bool un_signed)
{
    ExpressionType *type = type_registry_next_type(TK_PRIMITIVE, name);
    type->primitive.type = primitive_type;
    type->primitive.width = width;
    type->primitive.un_signed = un_signed;
    return type->type_id;
}

ExpressionType *type_registry_get_type_by_name(StringView name)
{
    for (int ix = 0; ix < type_registry.size; ++ix) {
        if (sv_eq(type_registry.types[ix].name, name)) {
            return &type_registry.types[ix];
        }
    }
    return NULL;
}

ExpressionType *type_registry_get_type_by_id(type_id id)
{
    if (id <= type_registry.size) {
        return &type_registry.types[id];
    }
    return NULL;
}

type_id type_registry_id_of_primitive_type(PrimitiveType type)
{
    for (int ix = 0; ix < type_registry.size; ++ix) {
        if (type_registry.types[ix].kind == TK_PRIMITIVE && type_registry.types[ix].primitive.type == type) {
            return type_registry.types[ix].type_id;
        }
    }
    fatal("Primitive type '%s' not in registry", PrimitiveType_name(type));
}

type_id type_registry_id_of_integer_type(size_t width, bool un_signed)
{
    switch (width) {
    case 8:
        return type_registry_id_of_primitive_type((un_signed) ? PT_U8 : PT_I8);
    case 16:
        return type_registry_id_of_primitive_type((un_signed) ? PT_U16 : PT_I16);
    case 32:
        return type_registry_id_of_primitive_type((un_signed) ? PT_U32 : PT_I32);
    case 64:
        return type_registry_id_of_primitive_type((un_signed) ? PT_U64 : PT_I64);
    default:
        fatal("Invalid integer width %d", width);
    }
}

type_id type_registry_get_variant(size_t num, type_id *types)
{
    assert(num > 0);
    for (int ix = 0; ix < type_registry.size; ++ix) {
        if (type_registry.types[ix].kind == TK_VARIANT && type_registry.types[ix].component.type_id == types[0]) {
            bool           found = true;
            TypeComponent *ptr = type_registry.types[ix].component.next;
            for (int cix = 1; ix < num; ++ix) {
                if (!ptr || ptr->type_id != types[cix]) {
                    found = false;
                    break;
                }
                ptr = ptr->next;
            }
            if (found) {
                return type_registry.types[ix].type_id;
            }
        }
    }

    if (type_registry.components_capacity <= type_registry.components_size + num) {
        OUT_OF_MEMORY("Could not expand Type Component Registry");
    }

    size_t name_len = 2; // "<>"
    for (int ix = 0; ix < num; ++ix) {
        ExpressionType *expr_type = type_registry_get_type_by_id(types[ix]);
        assert(expr_type);
        name_len += sv_length(expr_type->name);
        if (ix < num - 1) {
            ++name_len; // for the comma
        }
    }

    char *name_buf = allocate(name_len);
    name_buf[0] = '<';
    size_t len = 1;
    for (int ix = 0; ix < num; ++ix) {
        ExpressionType *expr_type = type_registry_get_type_by_id(types[ix]);
        memcpy(name_buf + len, expr_type->name.ptr, expr_type->name.length);
        len += expr_type->name.length;
        if (ix < num - 1) {
            name_buf[len++] = ',';
        }
    }
    name_buf[name_len - 1] = '>';

    ExpressionType *type = type_registry_next_type(TK_VARIANT, sv_from(name_buf));
    TypeComponent  *component = &type->component;
    for (int ix = 0; ix < num; ++ix) {
        ExpressionType *expr_type = type_registry_get_type_by_id(types[ix]);
        assert(expr_type);
        component->name = expr_type->name;
        component->type_id = types[ix];
        if (ix < num - 1) {
            component->next = &type_registry.components[type_registry.components_size++];
            component = component->next;
        }
    }
    return type->type_id;
}

type_id type_registry_make_struct(StringView name, size_t num, StringView *names, type_id *types)
{
    if (type_registry_get_type_by_name(name)) {
        fatal("Type '" SV_SPEC "' already exists", SV_ARG(name));
    }
    if (type_registry.components_capacity <= type_registry.components_size + num) {
        OUT_OF_MEMORY("Could not expand Type Component Registry");
    }

    ExpressionType *type = type_registry_next_type(TK_COMPOSITE, name);
    TypeComponent  *component = &type->component;
    for (int ix = 0; ix < num; ++ix) {
        ExpressionType *expr_type = type_registry_get_type_by_id(types[ix]);
        assert(expr_type);
        component->name = names[ix];
        component->type_id = types[ix];
        if (ix < num - 1) {
            component->next = &type_registry.components[type_registry.components_size++];
            component = component->next;
        }
    }
    return type->type_id;
}

type_id type_registry_get_variant2(type_id t1, type_id t2)
{
    type_id types[2];
    types[0] = t1;
    types[1] = t2;
    return type_registry_get_variant(2, types);
}

type_id type_registry_alias(StringView name, type_id aliased)
{
    ExpressionType *type = type_registry_next_type(TK_ALIAS, name);
    type->alias_for_id = aliased;
    return type->type_id;
}

void type_registry_init()
{
    type_registry.types = allocate_array(ExpressionType, 8 * 1024);
    type_registry.capacity = 8 * 1024;
    type_registry.size = 0;
    type_registry.components = allocate_array(TypeComponent, 64 * 1024);
    type_registry.components_capacity = 64 * 1024;
    type_registry.components_size = 0;
#undef PRIMITIVETYPE_ENUM
#define PRIMITIVETYPE_ENUM(code, name, width, un_signed) \
    code ## _ID = type_registry_add_primitive(sv_from(#name), PT_ ## code, width, un_signed);
    PRIMITIVETYPES(PRIMITIVETYPE_ENUM)
#undef PRIMITIVETYPE_ENUM
    type_registry_alias(sv_from("int"), type_registry_id_of_primitive_type(PT_I32));
    type_registry_alias(sv_from("unsigned"), type_registry_id_of_primitive_type(PT_U32));
    type_registry_alias(sv_from("byte"), type_registry_id_of_primitive_type(PT_I8));
    type_registry_alias(sv_from("char"), type_registry_id_of_primitive_type(PT_U8));
    type_registry_alias(sv_from("short"), type_registry_id_of_primitive_type(PT_I16));
    type_registry_alias(sv_from("ushort"), type_registry_id_of_primitive_type(PT_U16));
    type_registry_alias(sv_from("long"), type_registry_id_of_primitive_type(PT_I64));
    type_registry_alias(sv_from("ulong"), type_registry_id_of_primitive_type(PT_U64));
}

type_id type_registry_canonical_type(type_id type)
{
    ExpressionType *et = type_registry_get_type_by_id(type);
    return (et->kind == TK_ALIAS) ? type_registry_canonical_type(et->alias_for_id) : type;
}

bool typespec_assignment_compatible(TypeSpec ts1, TypeSpec ts2)
{
    ExpressionType *et1 = type_registry_get_type_by_id(ts1.type_id);
    assert(et1);
    ExpressionType *et2 = type_registry_get_type_by_id(ts2.type_id);
    assert(et2);
    return type_registry_canonical_type(ts1.type_id) == type_registry_canonical_type(ts2.type_id);
}

StringView typespec_name(TypeSpec typespec)
{
    ExpressionType *et = type_registry_get_type_by_id(typespec.type_id);
    assert(et);
    return et->name;
}

void typespec_print(FILE *f, TypeSpec typespec)
{
    ExpressionType *et = type_registry_get_type_by_id(typespec.type_id);
    assert(et);
    fprintf(f, SV_SPEC "%s", SV_ARG(et->name), (typespec.optional) ? "?" : "");
}
