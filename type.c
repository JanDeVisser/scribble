/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <log.h>
#include <mem.h>
#include <sv.h>
#include <type.h>

static size_t type_registry_add_primitive(StringView name, PrimitiveType primitive_type);

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

void *allocate(size_t size)
{
    static Arena *s_arena = NULL;
    if (!s_arena) {
        s_arena = arena_new();
    }
    return arena_allocate(s_arena, size);
}

size_t type_registry_add_primitive(StringView name, PrimitiveType primitive_type)
{
    if (type_registry.capacity <= type_registry.size + 1) {
        OUT_OF_MEMORY("Could not expand Type Registry");
    }
    ExpressionType *type = &type_registry.types[type_registry.size];
    type->type_id = type_registry.size;
    type->name = name;
    type->kind = TK_PRIMITIVE;
    type->primitive = primitive_type;
    ++type_registry.size;
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

ExpressionType *type_registry_get_type_by_id(size_t id)
{
    if (id <= type_registry.size) {
        return &type_registry.types[id];
    }
    return NULL;
}

size_t type_registry_id_of_primitive_type(PrimitiveType type)
{
    for (int ix = 0; ix < type_registry.size; ++ix) {
        if (type_registry.types[ix].kind == TK_PRIMITIVE && type_registry.types[ix].primitive == type) {
            return type_registry.types[ix].type_id;
        }
    }
    fatal("Primitive type '%s' not in registry", PrimitiveType_name(type));
}

size_t type_registry_get_variant(size_t num, size_t *types)
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

    if (type_registry.capacity <= type_registry.size + 1) {
        OUT_OF_MEMORY("Could not expand Type Registry");
    }
    if (type_registry.components_capacity <= type_registry.components_size + num) {
        OUT_OF_MEMORY("Could not expand Type Component Registry");
    }

    ExpressionType *type = &type_registry.types[type_registry.size];
    type->type_id = type_registry.size;
    type->kind = TK_VARIANT;

    TypeComponent *component = &type->component;
    size_t         name_len = 3; // "<>\0"
    for (int ix = 0; ix < num; ++ix) {
        ExpressionType *expr_type = type_registry_get_type_by_id(types[ix]);
        assert(expr_type);
        name_len += sv_length(expr_type->name);
        component->name = expr_type->name;
        component->type_id = types[ix];
        if (ix < num - 1) {
            component->next = &type_registry.components[type_registry.components_size++];
            component = component->next;
            ++name_len; // for the comma
        }
    }

    char *name_buf = allocate(name_len);
    strcpy(name_buf, "<");
    for (int ix = 0; ix < num; ++ix) {
        ExpressionType *expr_type = type_registry_get_type_by_id(types[ix]);
        sprintf(name_buf + strlen(name_buf), SV_SPEC, SV_ARG(expr_type->name));
        if (ix < num - 1) {
            name_buf[strlen(name_buf)] = ',';
        }
    }
    name_buf[name_len - 2] = '>';
    type->name = sv_from(name_buf);
    ++type_registry.components_size;
    return type->type_id;
}

size_t type_registry_get_variant2(size_t t1, size_t t2)
{
    size_t types[2];
    types[0] = t1;
    types[1] = t2;
    return type_registry_get_variant(2, types);
}

void type_registry_init()
{
    type_registry.types = allocate(8 * 1024 * sizeof(ExpressionType));
    type_registry.capacity = 8 * 1024;
    type_registry.size = 0;
    type_registry.components = allocate(64 * 1024 * sizeof(TypeComponent));
    type_registry.components_capacity = 64 * 1024;
    type_registry.components_size = 0;
#undef PRIMITIVETYPE_ENUM
#define PRIMITIVETYPE_ENUM(code, name) \
    type_registry_add_primitive(sv_from(#name), code);
    PRIMITIVETYPES(PRIMITIVETYPE_ENUM)
#undef PRIMITIVETYPE_ENUM
}
