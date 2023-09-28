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

static ErrorOrTypeID type_registry_add_builtin(StringView name, BuiltinType builtin_type);

typedef struct {
    size_t           size;
    size_t           capacity;
    ExpressionType **types;
} TypeRegistry;

static TypeRegistry type_registry = { 0 };

#define BUILTINTYPE_ENUM(type, name, code) type_id type##_ID = 0;
BUILTINTYPES(BUILTINTYPE_ENUM)
#undef BUILTINTYPE_ENUM
type_id FIRST_CUSTOM_IX = 0;
type_id NEXT_CUSTOM_IX = 0;

char const *TypeKind_name(TypeKind kind)
{
    switch (kind) {
#undef TYPEKINDS_ENUM
#define TYPEKINDS_ENUM(type, value) \
    case TK_##type:                 \
        return #type;
        TYPEKINDS(TYPEKINDS_ENUM)
#undef TYPEKINDS_ENUM
    default:
        UNREACHABLE();
    }
}

char const *BuiltinType_name(BuiltinType type)
{
    switch (type) {
#undef BUILTINTYPE_ENUM
#define BUILTINTYPE_ENUM(type, name, code) \
    case BIT_##type:                       \
        return #name;
        BUILTINTYPES(BUILTINTYPE_ENUM)
#undef BUILTINTYPE_ENUM
    default:
        UNREACHABLE();
    }
}

size_t BuiltinType_width(BuiltinType type)
{
    return type & WIDTH_MASK;
}

BuiltinType BuiltinType_get_integer_type(size_t width, bool un_signed)
{
    return INTEGER_MASK | width | ((size_t) un_signed << 16);
}

bool BuiltinType_is_integer(BuiltinType type)
{
    return type & INTEGER_MASK;
}

bool BuiltinType_is_number(BuiltinType type)
{
    return BuiltinType_is_integer(type) || (type == BIT_FLOAT);
}

bool BuiltinType_is_unsigned(BuiltinType type)
{
    return type & UNSIGNED_MASK;
}

ErrorOrTypeID type_registry_add_builtin(StringView name, BuiltinType builtin_type)
{
    uint8_t kind = builtin_type >> 12;
    TRY(TypeID, type_id, id, type_registry_make_type(name, kind))
    ExpressionType *type = type_registry_get_type_by_id(id);
    assert(type);
    type->type_id |= ((uint32_t) builtin_type) << 16;
    type->builtin_type = builtin_type;
    RETURN(TypeID, type->type_id);
}

ExpressionType *type_registry_get_type_by_name(StringView name)
{
    for (int ix = 0; ix < type_registry.size; ++ix) {
        if (sv_eq(type_registry.types[ix]->name, name)) {
            return type_registry.types[ix];
        }
    }
    return NULL;
}

ExpressionType *type_registry_get_type_by_id(type_id id)
{
    size_t ix = typeid_ix(id);
    if (ix <= type_registry.size) {
        return type_registry.types[ix];
    }
    fatal("Invalid type id 0x%04x:0x%04x referenced", id >> 16, id & 0xFFFF);
}

ExpressionType *type_registry_get_type_by_index(size_t ix)
{
    if (ix <= type_registry.size) {
        return type_registry.types[ix];
    }
    fatal("Invalid type index %d referenced", ix);
}

type_id type_registry_id_of_builtin_type(BuiltinType type)
{
    for (size_t ix = 0; ix < 20 && ix < type_registry.size; ++ix) {
        if (typeid_has_kind(type_registry.types[ix]->type_id, type >> 12) && typeid_builtin_type(type_registry.types[ix]->type_id) == type) {
            return type_registry.types[ix]->type_id;
        }
    }
    fatal("Builtin type '%s' (0x%04x) not found", BuiltinType_name(type), type);
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
    uint16_t tix1 = typeid_ix(*t1);
    uint16_t tix2 = typeid_ix(*t2);
    return (tix1 > tix2) ? 1 : ((tix2 > tix1) ? -1 : 0);
}

ErrorOrTypeID type_registry_make_type(StringView name, TypeKind kind)
{
    if (type_registry_get_type_by_name(name)) {
        ERROR(TypeID, TypeError, 0, "Type already exists");
    }
    if (type_registry.capacity <= type_registry.size + 1) {
        size_t           new_cap = (type_registry.capacity) ? type_registry.capacity * 2 : 32;
        ExpressionType  *new_block = allocate_array(ExpressionType, new_cap - type_registry.capacity);
        ExpressionType **new_index = allocate_array(ExpressionType *, new_cap);
        memcpy(new_index, type_registry.types, type_registry.capacity * sizeof(ExpressionType *));
        for (size_t ix = 0; ix < new_cap - type_registry.capacity; ++ix) {
            new_index[ix + type_registry.capacity] = new_block + ix;
        }
        type_registry.types = new_index;
        type_registry.capacity = new_cap;
    }
    ExpressionType *type = type_registry.types[type_registry.size];
    type->name = sv_copy_with_allocator(name, get_allocator());
    type->type_id = type_registry.size | (kind << 28);
    NEXT_CUSTOM_IX = ++type_registry.size;
    RETURN(TypeID, type->type_id);
}

void type_registry_init()
{
    type_registry.types = NULL;
    type_registry.capacity = 0;
    type_registry.size = 0;
#undef BUILTINTYPE_ENUM
#define BUILTINTYPE_ENUM(type, name, code) \
    MUST_TO_VAR(TypeID, type##_ID, type_registry_add_builtin(sv_from(#name), BIT_##type));
    BUILTINTYPES(BUILTINTYPE_ENUM)
#undef BUILTINTYPE_ENUM
    MUST_VOID(TypeID, type_registry_alias(sv_from("int"), I32_ID))
    MUST_VOID(TypeID, type_registry_alias(sv_from("unsigned"), U32_ID))
    MUST_VOID(TypeID, type_registry_alias(sv_from("byte"), I8_ID))
    MUST_VOID(TypeID, type_registry_alias(sv_from("char"), U8_ID))
    MUST_VOID(TypeID, type_registry_alias(sv_from("short"), I16_ID))
    MUST_VOID(TypeID, type_registry_alias(sv_from("ushort"), U16_ID))
    MUST_VOID(TypeID, type_registry_alias(sv_from("long"), I64_ID))
    MUST_VOID(TypeID, type_registry_alias(sv_from("ulong"), U64_ID))
    assert(typeid_has_kind(STRING_ID, TK_AGGREGATE));
    assert(typeid_has_kind(RANGE_ID, TK_AGGREGATE));

    MUST_VOID(TypeID,
        type_set_template_parameters(RANGE_ID, 1, (TemplateParameter[]) { { sv_from("T"), TPT_TYPE } }))
    MUST_VOID(TypeID,
        type_set_struct_components(RANGE_ID, 2,
            (TypeComponent[]) {
                { .kind = CK_TEMPLATE_PARAM, .name = sv_from("min"), .param = sv_from("T") },
                { .kind = CK_TEMPLATE_PARAM, .name = sv_from("max"), .param = sv_from("T") } }))
    MUST_VOID(TypeID,
        type_set_struct_components(STRING_ID, 2,
            (TypeComponent[]) {
                { .kind = CK_TYPE, .name = sv_from("ptr"), .type_id = POINTER_ID },
                { .kind = CK_TYPE, .name = sv_from("length"), .type_id = U64_ID } }))
    FIRST_CUSTOM_IX = type_registry.size;
    NEXT_CUSTOM_IX = FIRST_CUSTOM_IX;
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

StringView typespec_to_string(TypeSpec typespec, Allocator *allocator)
{
    ExpressionType *et = type_registry_get_type_by_id(typespec.type_id);
    assert(et);
    return sv_aprintf(allocator, SV_SPEC "%s [0x%08x]", SV_ARG(et->name), (typespec.optional) ? "?" : "", typespec.type_id);
}

void typespec_print(FILE *f, TypeSpec typespec)
{
    AllocatorState as = save_allocator();
    StringView     s = typespec_to_string(typespec, get_allocator());
    fprintf(f, SV_SPEC, SV_ARG(s));
    release_allocator(as);
}

bool type_is_concrete(ExpressionType *type)
{
    return typeid_is_concrete(type->type_id);
}

bool typeid_is_concrete(type_id type)
{
    ExpressionType *et = type_registry_get_type_by_id(type);
    if (typeid_has_kind(type, TK_PRIMITIVE) || et->specialization_of == 0)
        return true;
    switch (type_kind(et)) {
    case TK_ALIAS:
        return typeid_is_concrete(typeid_canonical_type_id(type));
    case TK_ARRAY:
        return et->array.base_type.kind == CK_TYPE;
    case TK_AGGREGATE:
    case TK_VARIANT: {
        for (size_t ix = 0; ix < et->components.num_components; ++ix) {
            if (et->components.components[ix].kind != CK_TYPE) {
                return false;
            }
        }
        return false;
    }
    default:
        UNREACHABLE();
    }
}

ErrorOrSize type_sizeof(ExpressionType *type)
{
    switch (type_kind(type)) {
    case TK_PRIMITIVE:
        RETURN(Size, BuiltinType_width(type->builtin_type) / 8);
    case TK_ALIAS:
        return type_sizeof(typeid_canonical_type(type->type_id));
    case TK_ARRAY: {
        if (type->array.base_type.kind != CK_TYPE) {
            ERROR(Size, TypeError, 0, "Cannot get size of template type");
        }
        TRY(Size, size_t, component_size, type_sizeof(type_registry_get_type_by_id(type->array.base_type.type_id)))
        RETURN(Size, component_size * type->array.size);
    }
    case TK_AGGREGATE: {
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
        RETURN(Size, BuiltinType_width(type->builtin_type) / 8);
    case TK_ALIAS:
        return type_alignat(typeid_canonical_type(type->type_id));
    case TK_ARRAY:
        if (type->array.base_type.kind != CK_TYPE) {
            ERROR(Size, TypeError, 0, "Cannot get size of template type");
        }
        return type_alignat(type_registry_get_type_by_id(type->array.base_type.type_id));
    case TK_AGGREGATE:
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

ErrorOrSize type_offsetof_index(ExpressionType *type, size_t index)
{
    if (!type_has_kind(type, TK_AGGREGATE)) {
        ERROR(Size, TypeError, 0, "Type '%.*s' is not an aggregate", SV_ARG(type->name));
    }
    if (index >= type->components.num_components) {
        ERROR(Size, TypeError, 0, "Type '%.*s' has no component with index %d", SV_ARG(type->name), index);
    }
    if (!type_is_concrete(type)) {
        ERROR(Size, TypeError, 0, "Type '%.*s' is not concrete. Cannot get component offset");
    }
    size_t offset = 0;
    TRY(Size, size_t, align, type_alignat(type))
    for (size_t ix = 0; ix < index; ++ix) {
        if (offset % align) {
            offset += offset + align - (offset % align);
        }
        ExpressionType *component_type = type_registry_get_type_by_id(type->components.components[ix].type_id);
        TRY(Size, size_t, component_size, type_sizeof(component_type))
        offset += component_size;
    }
    RETURN(Size, offset);
}

ErrorOrSize type_offsetof_name(ExpressionType *type, StringView name)
{
    if (!type_has_kind(type, TK_AGGREGATE)) {
        ERROR(Size, TypeError, 0, "Type '%.*s' is not an aggregate", SV_ARG(type->name));
    }
    if (!type_is_concrete(type)) {
        ERROR(Size, TypeError, 0, "Type '%.*s' is not concrete. Cannot get component offset");
    }
    size_t offset = 0;
    TRY(Size, size_t, align, type_alignat(type))
    for (size_t ix = 0; ix < type->components.num_components; ++ix) {
        if (sv_eq(type->components.components[ix].name, name)) {
            RETURN(Size, offset);
        }
        if (offset % align) {
            offset += offset + align - (offset % align);
        }
        ExpressionType *component_type = type_registry_get_type_by_id(type->components.components[ix].type_id);
        TRY(Size, size_t, component_size, type_sizeof(component_type))
        offset += component_size;
    }
    ERROR(Size, TypeError, 0, "Type '%.*s' has no component with name '%.*s'", SV_ARG(type->name), SV_ARG(name));
}

ErrorOrTypeID type_set_struct_components(type_id struct_id, size_t num, TypeComponent *components)
{
    ExpressionType *struct_type = type_registry_get_type_by_id(struct_id);
    if (!struct_type) {
        ERROR(TypeID, TypeError, 0, "Invalid type ID");
    }
    if (typeid_kind(struct_id) != TK_AGGREGATE) {
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

TypeComponent *type_get_component(ExpressionType *type, StringView component)
{
    assert(type);
    for (size_t ix = 0; ix < type->components.num_components; ++ix) {
        if (sv_eq(type->components.components[ix].name, component)) {
            return &type->components.components[ix];
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
        if (type_registry.types[ix]->specialization_of == template_id) {
            bool matches = true;
            for (size_t arg_ix = 0; arg_ix < num; ++arg_ix) {
                assert(arg_ix < type_registry.types[ix]->num_arguments);
                TemplateArgument *arg = type_arguments + arg_ix;
                TemplateArgument *other_arg = type_registry.types[ix]->template_arguments + arg_ix;
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
                RETURN(TypeID, type_registry.types[ix]->type_id);
            }
        }
    }

    StringBuilder name = sb_acreate(get_allocator());
    sb_printf(&name, SV_SPEC "<", SV_ARG(template_type->name));
    char *comma = "";
    for (size_t ix = 0; ix < num; ++ix) {
        TemplateArgument *arg = type_arguments + ix;
        sb_append_cstr(&name, comma);
        comma = ",";
        sb_append_sv(&name, arg->name);
        sb_append_cstr(&name, "=");
        switch (arg->param_type) {
        case TPT_TYPE: {
            ExpressionType *t = type_registry_get_type_by_id(arg->type);
            sb_append_sv(&name, t->name);
        } break;
        case TPT_NUMBER: {
            sb_printf(&name, "%ld", arg->int_value);
        } break;
        case TPT_STRING: {
            sb_append_sv(&name, arg->string_value);
        } break;
        default:
            UNREACHABLE();
        }
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
    case TK_AGGREGATE:
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
                TemplateArgument *arg = type_get_argument(type, template_comp->param);
                if (arg) {
                    type_comp->name = template_comp->name;
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
        type->builtin_type = template_type->builtin_type;
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
        if (typeid_kind(type_registry.types[ix]->type_id) == TK_VARIANT) {
            ExpressionType *type = type_registry.types[ix];
            bool            found = true;
            for (size_t comp_ix = 0; comp_ix < type->components.num_components; ++comp_ix) {
                if (type->components.components[comp_ix].type_id != sorted_types[comp_ix]) {
                    found = false;
                    break;
                }
            }
            if (found) {
                release_allocator(alloc_state);
                RETURN(TypeID, type_registry.types[ix]->type_id);
            }
        }
    }

    StringBuilder name = sb_acreate(get_allocator());
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

ErrorOrTypeID type_registry_array(StringView name, type_id base_type, size_t size)
{
    TRY(TypeID, type_id, new_id, type_registry_make_type(name, TK_ARRAY))
    ExpressionType *type = type_registry_get_type_by_id(new_id);
    assert(type);
    type->array.base_type.type_id = base_type;
    type->array.size = size;
    RETURN(TypeID, new_id);
}
