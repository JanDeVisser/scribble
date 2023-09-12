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

static ErrorOrTypeID type_registry_add_primitive(StringView name, PrimitiveType primitive_type);

typedef struct {
    size_t          size;
    size_t          capacity;
    ExpressionType *types;
} TypeRegistry;

static TypeRegistry type_registry = { 0 };

#define PRIMITIVETYPE_ENUM(type, name, code) type_id type##_ID = 0;
PRIMITIVETYPES(PRIMITIVETYPE_ENUM)
#undef PRIMITIVETYPE_ENUM
type_id RANGE_ID = 0;

char const *PrimitiveType_name(PrimitiveType type)
{
    switch (type) {
#undef PRIMITIVETYPE_ENUM
#define PRIMITIVETYPE_ENUM(type, name, code) \
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

ErrorOrTypeID type_registry_add_primitive(StringView name, PrimitiveType primitive_type)
{
    TRY(TypeID, type_id, id, type_registry_make_type(name, TK_PRIMITIVE))
    ExpressionType *type = type_registry_get_type_by_id(id);
    assert(type);
    type->type_id |= primitive_type << 16;
    type->primitive_type = primitive_type;
    RETURN(TypeID, id);
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
    size_t ix = typeid_ix(id);
    if (ix <= type_registry.size) {
        return &type_registry.types[ix];
    }
    fatal("Invalid type id 0x%04x:0x%04x referenced", id >> 16, id & 0xFFFF);
}

type_id type_registry_id_of_primitive_type(PrimitiveType type)
{
    for (size_t ix = 0; ix < 20 && ix < type_registry.size; ++ix) {
        if (typeid_has_kind(type_registry.types[ix].type_id, TK_PRIMITIVE) &&
            typeid_primitive_type(type_registry.types[ix].type_id) == type) {
            return type;
        }
    }
    fatal("Primitive type '%s' (0x%04x) not found", PrimitiveType_name(type), type);
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

int sort_type_ids(type_id const *t1, type_id const *t2)
{
    return (*t1 > *t2) ? 1 : ((*t2 > *t1) ? -1 : 0);
}

ErrorOrTypeID type_registry_make_type(StringView name, TypeKind kind)
{
    if (type_registry_get_type_by_name(name)) {
        ERROR(TypeID, TypeError, 0, "Type already exists");
    }
    if (type_registry.capacity <= type_registry.size + 1) {
        OUT_OF_MEMORY("Could not expand Type Registry");
    }
    ExpressionType *type = &type_registry.types[type_registry.size];
    type->name = sv_copy_with_allocator(name, get_allocator());
    type->type_id = type_registry.size | (kind << 31);
    ++type_registry.size;
    RETURN(TypeID, type->type_id);
}

void type_registry_init()
{
    type_registry.types = allocate_array(ExpressionType, 8 * 1024);
    type_registry.capacity = 8 * 1024;
    type_registry.size = 0;
#undef PRIMITIVETYPE_ENUM
#define PRIMITIVETYPE_ENUM(type, name, code)                                                \
    MUST_TO_VAR(TypeID, type##_ID, type_registry_add_primitive(sv_from(#name), PT_##type));
    PRIMITIVETYPES(PRIMITIVETYPE_ENUM)
#undef PRIMITIVETYPE_ENUM
    MUST_VOID(TypeID, type_registry_alias(sv_from("int"), I32_ID))
    MUST_VOID(TypeID, type_registry_alias(sv_from("unsigned"), U32_ID))
    MUST_VOID(TypeID, type_registry_alias(sv_from("byte"), I8_ID))
    MUST_VOID(TypeID, type_registry_alias(sv_from("char"), U8_ID))
    MUST_VOID(TypeID, type_registry_alias(sv_from("short"), I16_ID))
    MUST_VOID(TypeID, type_registry_alias(sv_from("ushort"), U16_ID))
    MUST_VOID(TypeID, type_registry_alias(sv_from("long"), I64_ID))
    MUST_VOID(TypeID, type_registry_alias(sv_from("ulong"), U64_ID))

    MUST_TO_VAR(TypeID, RANGE_ID, type_registry_make_type(sv_from("range"), TK_COMPOSITE))
    MUST_VOID(TypeID,
        type_set_template_parameters(RANGE_ID, 1, (TemplateParameter[]) { { sv_from("T"), TPT_TYPE } }))
    MUST_VOID(TypeID,
        type_set_struct_components(RANGE_ID, 2,
            (TypeComponent[]) {
                { .kind = CK_TEMPLATE_PARAM, .name = sv_from("min"), .param = sv_from("T") },
                { .kind = CK_TEMPLATE_PARAM, .name = sv_from("max"), .param = sv_from("T") } }))
}

type_id typeid_canonical_type_id(type_id type)
{
    ExpressionType *et = type_registry_get_type_by_id(type);
    return (type_kind(et) == TK_ALIAS) ? typeid_canonical_type_id(et->alias_for_id) : type;
}

ExpressionType *typeid_canonical_type(type_id type)
{
    return type_registry_get_type_by_id(typeid_canonical_type_id(type));
}

bool typespec_assignment_compatible(TypeSpec ts1, TypeSpec ts2)
{
    ExpressionType *et1 = type_registry_get_type_by_id(ts1.type_id);
    assert(et1);
    ExpressionType *et2 = type_registry_get_type_by_id(ts2.type_id);
    assert(et2);
    return typeid_canonical_type_id(ts1.type_id) == typeid_canonical_type_id(ts2.type_id);
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

ErrorOrSize type_sizeof(ExpressionType *type)
{
    switch (type_kind(type)) {
    case TK_PRIMITIVE:
        RETURN(Size, PrimitiveType_width(type->primitive_type) / 8);
    case TK_ALIAS:
        return type_sizeof(typeid_canonical_type(type->type_id));
    case TK_ARRAY: {
        if (type->array.base_type.kind != CK_TYPE) {
            ERROR(Size, TypeError, 0, "Cannot get size of template type");
        }
        TRY(Size, size_t, component_size, type_sizeof(type_registry_get_type_by_id(type->array.base_type.type_id)))
        RETURN(Size, component_size * type->array.size);
    }
    case TK_COMPOSITE: {
        size_t size = 0;
        TRY(Size, size_t, align, type_alignat(type))
        for (size_t ix = 0; ix < type->components.num_components; ++ix) {
            if (type->components.components[ix].kind != CK_TYPE) {
                ERROR(Size, TypeError, 0, "Cannot get size of template type");
            }
            if (size % align) {
                size += size + align - (size % align);
            }
            ExpressionType *component_type = type_registry_get_type_by_id(type->components.components[ix].type_id);
            TRY(Size, size_t, component_size, type_sizeof(component_type))
            size += component_size;
        }
        RETURN(Size, size);
    }
    case TK_VARIANT: {
        size_t size = 0;
        for (size_t ix = 0; ix < type->components.num_components; ++ix) {
            if (type->components.components[ix].kind != CK_TYPE) {
                ERROR(Size, TypeError, 0, "Cannot get size of template type");
            }
            ExpressionType *component_type = type_registry_get_type_by_id(type->components.components[ix].type_id);
            TRY(Size, size_t, component_size, type_sizeof(component_type))
            if (component_size > size) {
                size = component_size;
            }
        }
        RETURN(Size, size);
    }
    default:
        UNREACHABLE();
    }
}

ErrorOrSize type_alignat(ExpressionType *type)
{
    switch (type_kind(type)) {
    case TK_PRIMITIVE:
        RETURN(Size, PrimitiveType_width(type->primitive_type) / 8);
    case TK_ALIAS:
        return type_alignat(typeid_canonical_type(type->type_id));
    case TK_ARRAY:
        if (type->array.base_type.kind != CK_TYPE) {
            ERROR(Size, TypeError, 0, "Cannot get size of template type");
        }
        return type_alignat(type_registry_get_type_by_id(type->array.base_type.type_id));
    case TK_COMPOSITE:
    case TK_VARIANT: {
        size_t align = 0;
        for (size_t ix = 0; ix < type->components.num_components; ++ix) {
            if (type->components.components[ix].kind != CK_TYPE) {
                ERROR(Size, TypeError, 0, "Cannot get alignment of template type");
            }
            ExpressionType *component_type = type_registry_get_type_by_id(type->components.components[ix].type_id);
            TRY(Size, size_t, component_align, type_alignat(component_type))
            if (component_align > align) {
                align = component_align;
            }
        }
        RETURN(Size, align);
    }
    default:
        UNREACHABLE();
    }
}

ErrorOrTypeID type_set_struct_components(type_id struct_id, size_t num, TypeComponent *components)
{
    ExpressionType *struct_type = type_registry_get_type_by_id(struct_id);
    if (!struct_type) {
        ERROR(TypeID, TypeError, 0, "Invalid type ID");
    }
    if (typeid_kind(struct_id) != TK_COMPOSITE) {
        ERROR(TypeID, TypeError, 0, "Type is not a composite");
    }
    for (size_t ix = 0; ix < num; ++ix) {
        TypeComponent *component = &components[ix];
        switch (component->kind) {
        case CK_TYPE: {
            ExpressionType *expr_type = type_registry_get_type_by_id(component->type_id);
            if (!expr_type) {
                ERROR(TypeID, TypeError, 0, "Unknown type for struct component");
            }
        } break;
        case CK_TEMPLATE_PARAM: {
            TemplateParameter *param = type_get_parameter(struct_type, component->param);
            if (!param) {
                ERROR(TypeID, TypeError, 0, "Unknown template parameter");
            }
            if (param->type != TPT_TYPE) {
                ERROR(TypeID, TypeError, 0, "Template parameter is not a type");
            }
        } break;
        default:
            UNREACHABLE();
        }
    }

    struct_type->components.num_components = num;
    struct_type->components.components = allocate_array(TypeComponent, num);
    for (size_t ix = 0; ix < num; ++ix) {
        struct_type->components.components[ix].kind = components[ix].kind;
        struct_type->components.components[ix].name = sv_copy_with_allocator(components[ix].name, get_allocator());
        switch (components[ix].kind) {
        case CK_TYPE: {
            struct_type->components.components[ix].type_id = components[ix].type_id;
        } break;
        case CK_TEMPLATE_PARAM: {
            struct_type->components.components[ix].param = sv_copy_with_allocator(components[ix].param, get_allocator());
        } break;
        default:
            UNREACHABLE();
        }
    }
    RETURN(TypeID, struct_type->type_id);
}

ErrorOrTypeID type_set_template_parameters(type_id template_id, size_t num, TemplateParameter *parameters)
{
    ExpressionType *type = type_registry_get_type_by_id(template_id);
    if (!type) {
        ERROR(TypeID, TypeError, 0, "Invalid type ID");
    }
    for (size_t ix = 0; ix < num; ++ix) {
        if (parameters[ix].type < 0 || parameters[ix].type >= TPT_MAX) {
            ERROR(TypeID, TypeError, 0, "Invalid template parameter type");
        }
        for (size_t param_ix = 0; param_ix < ix; ++param_ix) {
            if (sv_eq(type->template_parameters[param_ix].name, parameters[ix].name)) {
                ERROR(TypeID, TypeError, 0, "Duplicate template parameter");
            }
        }
    }
    type->num_parameters = num;
    type->template_parameters = allocate_array(TemplateParameter, num);
    for (size_t ix = 0; ix < num; ++ix) {
        type->template_parameters[ix].name = sv_copy_with_allocator(parameters[ix].name, get_allocator());
        type->template_parameters[ix].type = parameters[ix].type;
    }
    RETURN(TypeID, type->type_id);
}

TemplateArgument *type_get_argument(ExpressionType *type, StringView arg)
{
    assert(type);
    for (size_t ix = 0; ix < type->num_arguments; ++ix) {
        if (sv_eq(type->template_arguments[ix].name, arg)) {
            return &type->template_arguments[ix];
        }
    }
    return NULL;
}

TemplateParameter *type_get_parameter(ExpressionType *type, StringView param)
{
    assert(type);
    for (size_t ix = 0; ix < type->num_parameters; ++ix) {
        if (sv_eq(type->template_parameters[ix].name, param)) {
            return &type->template_parameters[ix];
        }
    }
    return NULL;
}

ErrorOrTypeID type_specialize_template(type_id template_id, size_t num, TemplateArgument *arguments)
{
    ExpressionType *template_type = type_registry_get_type_by_id(template_id);
    if (!template_type) {
        ERROR(TypeID, TypeError, 0, "Invalid type ID");
    }
    if (!template_type->template_parameters) {
        ERROR(TypeID, TypeError, 0, "Type is not parameterized");
    }
    if (template_type->num_parameters != num) {
        ERROR(TypeID, TypeError, 0, "Incorrect number of template arguments provided");
    }

    AllocatorState    alloc_state = save_allocator();
    TemplateArgument *type_arguments = allocate_array(TemplateArgument, num);
    for (size_t ix = 0; ix < num; ++ix) {
        for (size_t arg_ix = 0; arg_ix < ix; ++arg_ix) {
            if (sv_eq(arguments[arg_ix].name, arguments[ix].name)) {
                release_allocator(alloc_state);
                ERROR(TypeID, TypeError, 0, "Duplicate template parameter specified");
            }
        }
        bool found = false;
        for (size_t param_ix = 0; param_ix < num; ++param_ix) {
            TemplateParameter *param = template_type->template_parameters + param_ix;
            if (sv_eq(arguments[ix].name, param->name)) {
                if (param->type != arguments[ix].param_type) {
                    release_allocator(alloc_state);
                    ERROR(TypeID, TypeError, 0, "Template parameter type mismatch");
                }
                type_arguments[param_ix] = arguments[ix];
                found = true;
            }
        }
        if (!found) {
            release_allocator(alloc_state);
            ERROR(TypeID, TypeError, 0, "Invalid template parameter specified");
        }
    }

    for (size_t ix = 0; ix < type_registry.size; ++ix) {
        if (type_registry.types[ix].specialization_of == template_id) {
            bool matches = true;
            for (size_t arg_ix = 0; arg_ix < num; ++arg_ix) {
                assert(arg_ix < type_registry.types[ix].num_arguments);
                TemplateArgument *arg = type_arguments + arg_ix;
                TemplateArgument *other_arg = type_registry.types[ix].template_arguments + arg_ix;
                assert(arg->param_type == other_arg->param_type);
                assert(sv_eq(arg->name, other_arg->name));
                switch (arg->param_type) {
                case TPT_TYPE:
                    matches = arg->type == other_arg->type;
                    break;
                case TPT_NUMBER:
                    matches = arg->int_value == other_arg->int_value;
                    break;
                case TPT_STRING:
                    matches = sv_eq(arg->string_value, other_arg->string_value);
                    break;
                default:
                    UNREACHABLE();
                }
                if (!matches) {
                    break;
                }
            }
            if (matches) {
                release_allocator(alloc_state);
                RETURN(TypeID, ix);
            }
        }
    }

    StringBuilder name = sb_create_with_allocator(get_allocator());
    sb_printf(&name, SV_SPEC "<", SV_ARG(template_type->name));
    char *comma = "";
    for (size_t ix = 0; ix < num; ++ix) {
        sb_append_cstr(&name, comma);
        comma = ",";
        sb_append_sv(&name, type_arguments[ix].name);
    }
    sb_append_cstr(&name, ">");

    TRY(TypeID, type_id, new_id, type_registry_make_type(name.view, type_kind(template_type)))
    ExpressionType *type = type_registry_get_type_by_id(new_id);
    assert(type);
    type->specialization_of = template_type->type_id;
    type->num_arguments = num;
    type->template_arguments = type_arguments;
    switch (type_kind(type)) {
    case TK_ALIAS:
        type->alias_for_id = template_type->alias_for_id;
        break;
    case TK_ARRAY: {
        type->array.size = template_type->array.size;
        switch (template_type->array.base_type.kind) {
        case CK_TYPE:
            type->array.base_type = template_type->array.base_type;
            break;
        case CK_TEMPLATE_PARAM: {
            TemplateArgument *base_type = type_get_argument(type, template_type->array.base_type.name);
            if (base_type) {
                type->array.base_type.kind = CK_TYPE;
                type->array.base_type.type_id = template_type->array.base_type.type_id;
                break;
            } else {
                type->array.base_type = template_type->array.base_type;
            }
        } break;
        }
    }
    case TK_COMPOSITE:
    case TK_VARIANT: {
        type->components.num_components = template_type->components.num_components;
        type->components.components = allocate_array(TypeComponent, type->components.num_components);
        for (size_t ix = 0; ix < template_type->components.num_components; ++ix) {
            TypeComponent *template_comp = &template_type->components.components[ix];
            TypeComponent *type_comp = &type->components.components[ix];
            switch (template_comp->kind) {
            case CK_TYPE:
                *type_comp = *template_comp;
                break;
            case CK_TEMPLATE_PARAM: {
                TemplateArgument *arg = type_get_argument(type, template_comp->name);
                if (arg) {
                    type_comp->kind = CK_TYPE;
                    type_comp->type_id = arg->type;
                } else {
                    *type_comp = *template_comp;
                }
            } break;
            }
        }
    } break;
    case TK_PRIMITIVE:
        type->primitive_type = template_type->primitive_type;
        break;
    }
    RETURN(TypeID, type->type_id);
}

ErrorOrTypeID type_registry_get_variant(size_t num, type_id *types)
{
    assert(num > 0);
    AllocatorState alloc_state = save_allocator();
    type_id       *sorted_types = allocate_array(type_id, num);
    memcpy(types, sorted_types, sizeof(type_id) * num);
    qsort(sorted_types, num, sizeof(type_id), (qsort_fnc_t) sort_type_ids);
    for (int ix = 0; ix < type_registry.size; ++ix) {
        if (typeid_kind(type_registry.types[ix].type_id) == TK_VARIANT) {
            ExpressionType *type = &type_registry.types[ix];
            bool            found = true;
            for (size_t comp_ix = 0; comp_ix < type->components.num_components; ++comp_ix) {
                if (type->components.components[comp_ix].type_id != sorted_types[comp_ix]) {
                    found = false;
                    break;
                }
            }
            if (found) {
                release_allocator(alloc_state);
                RETURN(TypeID, type_registry.types[ix].type_id);
            }
        }
    }

    StringBuilder name = sb_create_with_allocator(get_allocator());
    char         *comma = "";
    sb_append_cstr(&name, "<");
    for (size_t ix = 0; ix < num; ++ix) {
        sb_append_cstr(&name, comma);
        comma = ",";
        ExpressionType *type = type_registry_get_type_by_id(sorted_types[ix]);
        if (!type) {
            release_allocator(alloc_state);
            ERROR(TypeID, TypeError, 0, "Invalid type ID constructing variant");
        }
        sb_append_sv(&name, type->name);
    }
    sb_append_cstr(&name, ">");

    TRY(TypeID, type_id, new_variant_id, type_registry_make_type(name.view, TK_VARIANT))
    ExpressionType *new_variant = type_registry_get_type_by_id(new_variant_id);
    assert(new_variant);
    new_variant->components.num_components = num;
    new_variant->components.components = allocate_array(TypeComponent, num);
    for (int ix = 0; ix < num; ++ix) {
        ExpressionType *comp_type = type_registry_get_type_by_id(sorted_types[ix]);
        assert(comp_type);
        new_variant->components.components[ix].kind = CK_TYPE;
        new_variant->components.components[ix].name = comp_type->name;
        new_variant->components.components[ix].type_id = comp_type->type_id;
    }
    RETURN(TypeID, new_variant_id);
}

ErrorOrTypeID type_registry_get_variant2(type_id t1, type_id t2)
{
    type_id types[2];
    types[0] = t1;
    types[1] = t2;
    return type_registry_get_variant(2, types);
}

ErrorOrTypeID type_registry_alias(StringView name, type_id aliased)
{
    TRY(TypeID, type_id, new_id, type_registry_make_type(name, TK_ALIAS))
    ExpressionType *type = type_registry_get_type_by_id(new_id);
    assert(type);
    type->alias_for_id = aliased;
    RETURN(TypeID, new_id);
}
