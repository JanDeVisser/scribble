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
static type_id         type_registry_add_primitive(StringView name, PrimitiveType primitive_type);

typedef struct {
    size_t          size;
    size_t          capacity;
    ExpressionType *types;
    size_t          components_size;
    size_t          components_capacity;
    TypeComponent  *components;
} TypeRegistry;

static TypeRegistry type_registry = { 0 };

#define PRIMITIVETYPE_ENUM(type, name, code) type_id type##_ID = 0;
PRIMITIVETYPES(PRIMITIVETYPE_ENUM)
#undef PRIMITIVETYPE_ENUM

char const *PrimitiveType_name(PrimitiveType type)
{
    switch (type) {
#undef PRIMITIVETYPE_ENUM
#define PRIMITIVETYPE_ENUM(type, name, code) \
    case PT_##type:                                \
        return #name;
        PRIMITIVETYPES(PRIMITIVETYPE_ENUM)
#undef PRIMITIVETYPE_ENUM
    default:
        UNREACHABLE();
    }
}

size_t PrimitiveType_width(PrimitiveType type)
{
    return type & WIDTH_MASK;
}

PrimitiveType PrimitiveType_get_integer_type(size_t width, bool un_signed)
{
    return INTEGER_MASK | width | ((size_t) un_signed << 16);
}

bool PrimitiveType_is_integer(PrimitiveType type)
{
    return type & INTEGER_MASK;
}

bool PrimitiveType_is_number(PrimitiveType type)
{
    return PrimitiveType_is_integer(type) || (type == PT_FLOAT);
}

bool PrimitiveType_is_unsigned(PrimitiveType type)
{
    return type & UNSIGNED_MASK;
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

type_id type_registry_add_primitive(StringView name, PrimitiveType primitive_type)
{
    ExpressionType *type = type_registry_next_type(TK_PRIMITIVE, name);
    type->primitive_type = primitive_type;
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
        if (type_registry.types[ix].kind == TK_PRIMITIVE && type_registry.types[ix].primitive_type == type) {
            return type_registry.types[ix].type_id;
        }
    }
    fatal("Primitive type '%s' not in registry", PrimitiveType_name(type));
}

type_id type_registry_id_of_integer_type(size_t width, bool un_signed)
{
    switch (width) {
    case 8:
        return (un_signed) ? U8_ID : I8_ID;
    case 16:
        return (un_signed) ? U16_ID : I16_ID;
    case 32:
        return (un_signed) ? U32_ID : I32_ID;
    case 64:
        return (un_signed) ? U64_ID : I64_ID;
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
#define PRIMITIVETYPE_ENUM(type, name, code) \
    type##_ID = type_registry_add_primitive(sv_from(#name), PT_##type);
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

type_id type_registry_canonical_type_id(type_id type)
{
    ExpressionType *et = type_registry_get_type_by_id(type);
    return (et->kind == TK_ALIAS) ? type_registry_canonical_type_id(et->alias_for_id) : type;
}

ExpressionType *type_registry_canonical_type(type_id type)
{
    return type_registry_get_type_by_id(type_registry_canonical_type_id(type));
}

bool typespec_assignment_compatible(TypeSpec ts1, TypeSpec ts2)
{
    ExpressionType *et1 = type_registry_get_type_by_id(ts1.type_id);
    assert(et1);
    ExpressionType *et2 = type_registry_get_type_by_id(ts2.type_id);
    assert(et2);
    return type_registry_canonical_type_id(ts1.type_id) == type_registry_canonical_type_id(ts2.type_id);
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

size_t type_sizeof(ExpressionType *type)
{
    switch (type->kind) {
    case TK_PRIMITIVE:
        return PrimitiveType_width(type->primitive_type) / 8;
    case TK_ALIAS:
        return type_sizeof(type_registry_canonical_type(type->type_id));
    case TK_ARRAY:
        return type_sizeof(type_registry_get_type_by_id(type->array.base_type)) * type->array.size;
    case TK_COMPOSITE: {
        size_t size = 0;
        size_t align = type_alignat(type);
        for (TypeComponent *component = &type->component; component; component = component->next) {
            if (size % align) {
                size += size + align - (size % align);
            }
            ExpressionType *component_type = type_registry_get_type_by_id(component->type_id);
            size += type_sizeof(component_type);
        }
        return size;
    }
    case TK_VARIANT: {
        size_t size = 0;
        for (TypeComponent *component = &type->component; component; component = component->next) {
            ExpressionType *component_type = type_registry_get_type_by_id(component->type_id);
            size_t          component_size = type_sizeof(component_type);
            if (component_size > size) {
                size = component_size;
            }
        }
        return size;
    }
    default:
        UNREACHABLE();
    }
}

size_t type_alignat(ExpressionType *type)
{
    switch (type->kind) {
    case TK_PRIMITIVE:
        return PrimitiveType_width(type->primitive_type) / 8;
    case TK_ALIAS:
        return type_alignat(type_registry_canonical_type(type->type_id));
    case TK_ARRAY:
        return type_alignat(type_registry_get_type_by_id(type->array.base_type));
    case TK_COMPOSITE:
    case TK_VARIANT: {
        size_t align = 0;
        for (TypeComponent *component = &type->component; component; component = component->next) {
            ExpressionType *component_type = type_registry_get_type_by_id(component->type_id);
            size_t          component_align = type_alignat(component_type);
            if (component_align > align) {
                align = component_align;
            }
        }
        return align;
    }
    default:
        UNREACHABLE();
    }
}
