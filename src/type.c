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

char const *PrimitiveType_name(PrimitiveType type)
{
    switch (type) {
#undef PRIMITIVETYPE_ENUM
#define PRIMITIVETYPE_ENUM(type, name) \
    case type:                         \
        return #name;
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

type_id type_registry_add_primitive(StringView name, PrimitiveType primitive_type)
{
    ExpressionType *type = type_registry_next_type(TK_PRIMITIVE, name);
    type->primitive = primitive_type;
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
        if (type_registry.types[ix].kind == TK_PRIMITIVE && type_registry.types[ix].primitive == type) {
            return type_registry.types[ix].type_id;
        }
    }
    fatal("Primitive type '%s' not in registry", PrimitiveType_name(type));
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

void type_registry_init()
{
    type_registry.types = allocate_array(ExpressionType, 8 * 1024);
    type_registry.capacity = 8 * 1024;
    type_registry.size = 0;
    type_registry.components = allocate_array(TypeComponent, 64 * 1024);
    type_registry.components_capacity = 64 * 1024;
    type_registry.components_size = 0;
#undef PRIMITIVETYPE_ENUM
#define PRIMITIVETYPE_ENUM(code, name) \
    type_registry_add_primitive(sv_from(#name), code);
    PRIMITIVETYPES(PRIMITIVETYPE_ENUM)
#undef PRIMITIVETYPE_ENUM
}

bool typespec_assignment_compatible(TypeSpec ts1, TypeSpec ts2)
{
    ExpressionType *et1 = type_registry_get_type_by_id(ts1.type_id);
    assert(et1);
    ExpressionType *et2 = type_registry_get_type_by_id(ts2.type_id);
    assert(et2);
    return ts1.type_id == ts2.type_id;
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
