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

DA_IMPL(TypeComponent)
DA_IMPL(EnumValue)

static ErrorOrTypeID type_registry_add_builtin(StringView name, BuiltinType builtin_type);
static ErrorOrTypeID type_registry_make_type(StringView name, TypeKind kind, BuiltinType builtin_type);
static ErrorOrTypeID type_set_components(type_id aggregate_id, size_t num, TypeComponent *components);

typedef struct {
    DIA(ExpressionType *);
} TypeRegistry;

static TypeRegistry type_registry = { 0 };

#define BUILTINTYPE_ENUM(type, name, code) type_id type##_ID = 0;
BUILTINTYPES(BUILTINTYPE_ENUM)
#undef BUILTINTYPE_ENUM
type_id PCHAR_ID = 0;
type_id RESULT_ID = 0;
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

IntegerType BuiltinType_integer_type(BuiltinType type)
{
    uint8_t width = (uint8_t) (type & WIDTH_MASK);
    if (type & UNSIGNED_MASK) {
        return (IntegerType) width;
    }
    return (IntegerType) (-((int) width));
    //
    //    IntegerType unsigned_type = (IntegerType) (~(type & WIDTH_MASK) + 1);
    //    if (unsigned_type & UNSIGNED_MASK) {
    //        unsigned_type = (IntegerType) (~((int) unsigned_type) + 1);
    //    }
    //    return unsigned_type;
}

size_t BuiltinType_sizeof(BuiltinType type)
{
    if (type & ALL_INTEGERS_MASK) {
        return (size_t) (type & WIDTH_MASK) / 8;
    }
    switch (type) {
    case BIT_VOID:
    case BIT_SELF:
    case BIT_PARAMETER:
    case BIT_ERROR:
        return 0;
    case BIT_FLOAT:
        return sizeof(double);
    default:
        UNREACHABLE();
    }
}

BuiltinType BuiltinType_by_integer_spec(size_t width, bool un_signed)
{
    return INTEGER_MASK | width | ((size_t) un_signed << 8);
}

BuiltinType BuiltinType_by_integer_type(IntegerType type)
{
    bool   un_signed = true;
    size_t width = (size_t) type;
    if ((int) type < 0) {
        width = (size_t) (-((int) type));
        un_signed = false;
    }
    return BuiltinType_by_integer_spec(width, un_signed);
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
    uint8_t         kind = builtin_type >> 12;
    type_id         id = TRY(TypeID, type_registry_make_type(name, kind, builtin_type));
    ExpressionType *type = type_registry_get_type_by_id(id);
    assert(type);
    RETURN(TypeID, type->type_id);
}

ExpressionType *type_registry_get_type_by_name(StringView name)
{
    for (int ix = 0; ix < type_registry.size; ++ix) {
        if (sv_eq(type_registry.elements[ix]->name, name)) {
            return type_registry.elements[ix];
        }
    }
    return NULL;
}

ExpressionType *type_registry_get_type_by_id(type_id id)
{
    size_t ix = typeid_ix(id);
    if (ix < type_registry.size) {
        return type_registry.elements[ix];
    }
    fatal("Invalid type id 0x%04x:0x%04x referenced", id >> 16, id & 0xFFFF);
}

ExpressionType *type_registry_get_type_by_index(size_t ix)
{
    if (ix <= type_registry.size) {
        return type_registry.elements[ix];
    }
    fatal("Invalid type index %d referenced", ix);
}

type_id type_registry_id_of_builtin_type(BuiltinType type)
{
    for (size_t ix = 0; ix < 20 && ix < type_registry.size; ++ix) {
        if (typeid_has_kind(type_registry.elements[ix]->type_id, type >> 12) && typeid_builtin_type(type_registry.elements[ix]->type_id) == type) {
            return type_registry.elements[ix]->type_id;
        }
    }
    fatal("Builtin type '%s' (0x%04x) not found", BuiltinType_name(type), type);
}

type_id type_registry_id_of_integer_type(IntegerType type)
{
    switch (type) {
#undef INTEGER_SIZE
#define INTEGER_SIZE(sz)   \
    case I##sz:            \
        return I##sz##_ID; \
    case U##sz:            \
        return U##sz##_ID;
        INTEGER_SIZES(INTEGER_SIZE)
#undef INTEGER_SIZE
    default:
        UNREACHABLE();
    }
}

ErrorOrTypeID type_registry_make_type(StringView name, TypeKind kind, BuiltinType builtin_type)
{
    if (type_registry_get_type_by_name(name)) {
        ERROR(TypeID, TypeError, 0, "A type named '%.*s' already exists", SV_ARG(name));
    }
    ExpressionType *type = allocate_new(ExpressionType);
    DIA_APPEND(ExpressionType *, (&type_registry), type);
    type->name = sv_copy(name);
    type->type_id = (type_registry.size - 1) | (kind << 28) | (builtin_type << 16);
    type->builtin_type = builtin_type;
    NEXT_CUSTOM_IX = type_registry.size;
    RETURN(TypeID, type->type_id);
}

void type_registry_init()
{
    type_registry = (TypeRegistry) { 0 };
#undef BUILTINTYPE_ENUM
#define BUILTINTYPE_ENUM(type, name, code) \
    type##_ID = MUST(TypeID, type_registry_add_builtin(sv_from(#name), BIT_##type));
    BUILTINTYPES(BUILTINTYPE_ENUM)
#undef BUILTINTYPE_ENUM
    MUST(TypeID, type_registry_alias(sv_from("int"), I32_ID));
    MUST(TypeID, type_registry_alias(sv_from("unsigned"), U32_ID));
    MUST(TypeID, type_registry_alias(sv_from("byte"), I8_ID));
    MUST(TypeID, type_registry_alias(sv_from("char"), U8_ID));
    MUST(TypeID, type_registry_alias(sv_from("short"), I16_ID));
    MUST(TypeID, type_registry_alias(sv_from("ushort"), U16_ID));
    MUST(TypeID, type_registry_alias(sv_from("long"), I64_ID));
    MUST(TypeID, type_registry_alias(sv_from("ulong"), U64_ID));
    assert(typeid_has_kind(STRING_ID, TK_AGGREGATE));
    assert(typeid_has_kind(RANGE_ID, TK_AGGREGATE));
    assert(typeid_has_kind(ARRAY_ID, TK_AGGREGATE));

    MUST(TypeID,
        type_set_template_parameters(VAR_POINTER_ID, 1, (TemplateParameter[]) { { sv_from("T"), TPT_TYPE } }));
    PCHAR_ID = MUST(TypeID, type_specialize_template(VAR_POINTER_ID, 1, (TemplateArgument[]) { { .name = sv_from("T"), .arg_type = TPT_TYPE, .type = U8_ID } }));
    MUST(TypeID, type_registry_alias(sv_from("pchar"), PCHAR_ID));
    MUST(TypeID,
        type_set_template_parameters(RANGE_ID, 1, (TemplateParameter[]) { { sv_from("T"), TPT_TYPE } }));
    MUST(TypeID,
        type_set_components(RANGE_ID, 2,
            (TypeComponent[]) {
                { .kind = CK_TEMPLATE_PARAM, .name = sv_from("min"), .param = sv_from("T") },
                { .kind = CK_TEMPLATE_PARAM, .name = sv_from("max"), .param = sv_from("T") } }));
    MUST(TypeID,
        type_set_components(STRING_ID, 2,
            (TypeComponent[]) {
                { .kind = CK_TYPE, .name = sv_from("ptr"), .type_id = PCHAR_ID },
                { .kind = CK_TYPE, .name = sv_from("length"), .type_id = U64_ID } }));
    MUST(TypeID,
        type_set_template_parameters(ARRAY_ID, 1, (TemplateParameter[]) { { sv_from("T"), TPT_TYPE } }));
    MUST(TypeID,
        type_set_components(ARRAY_ID, 2,
            (TypeComponent[]) {
                {
                    .kind = CK_PARAMETERIZED_TYPE,
                    .name = sv_from("ptr"),
                    .parameterized_type = {
                        .template_type = VAR_POINTER_ID,
                        .parameter = sv_from("T"),
                        .argument = sv_from("T"),
                    },
                },
                { .kind = CK_TYPE, .name = sv_from("size"), .type_id = U64_ID } }));

    EnumValues result_values = { 0 };
    da_append_EnumValue(&result_values, (EnumValue) { sv_from("Error"), u8(0) });
    da_append_EnumValue(&result_values, (EnumValue) { sv_from("Ok"), u8(1) });
    RESULT_ID = MUST(TypeID, type_registry_make_enumeration(sv_from("result"), U8_ID, &result_values));

    FIRST_CUSTOM_IX = type_registry.size;
    NEXT_CUSTOM_IX = FIRST_CUSTOM_IX;
}

type_id typeid_canonical_type_id(type_id type)
{
    ExpressionType *et = type_registry_get_type_by_id(type);
    switch (type_kind(et)) {
    case TK_PRIMITIVE:
    case TK_AGGREGATE:
    case TK_VARIANT:
    case TK_ENUM:
        return type;
    case TK_ALIAS:
        return typeid_canonical_type_id(et->alias_for_id);
    }
}

ExpressionType *typeid_canonical_type(type_id type)
{
    return type_registry_get_type_by_id(typeid_canonical_type_id(type));
}

type_id typeid_underlying_type_id(type_id type)
{
    ExpressionType *et = type_registry_get_type_by_id(type);
    switch (type_kind(et)) {
    case TK_PRIMITIVE:
    case TK_AGGREGATE:
    case TK_VARIANT:
        return type;
    case TK_ENUM:
        return typeid_underlying_type_id(et->enumeration.underlying_type);
    case TK_ALIAS:
        return typeid_underlying_type_id(et->alias_for_id);
    }
}

bool typespec_assignment_compatible(TypeSpec ts1, TypeSpec ts2)
{
    ExpressionType *et1 = type_registry_get_type_by_id(ts1.type_id);
    assert(et1);
    ExpressionType *et2 = type_registry_get_type_by_id(ts2.type_id);
    assert(et2);
    return typeid_underlying_type_id(ts1.type_id) == typeid_underlying_type_id(ts2.type_id);
}

StringView typespec_name(TypeSpec typespec)
{
    ExpressionType *et = type_registry_get_type_by_id(typespec.type_id);
    assert(et);
    return et->name;
}

StringView typespec_to_string(TypeSpec typespec)
{
    ExpressionType *et = type_registry_get_type_by_id(typespec.type_id);
    assert(et);
    if (typeid_specializes(typespec.type_id) == ARRAY_ID) {
        return sv_printf("%.*s%s", SV_ARG(et->name), typespec.optional ? "?" : "");
    }
    return sv_printf("%.*s%s", SV_ARG(et->name), typespec.optional ? "?" : "");
}

void typespec_print(FILE *f, TypeSpec typespec)
{
    AllocatorState as = save_allocator();
    StringView     s = typespec_to_string(typespec);
    fprintf(f, SV_SPEC, SV_ARG(s));
    release_allocator(as);
}

bool type_is_concrete(ExpressionType *type)
{
    if (type_has_kind(type, TK_PRIMITIVE) || type->specialization_of == 0)
        return true;
    switch (type_kind(type)) {
    case TK_ALIAS:
        return type_is_concrete(typeid_canonical_type(type->type_id));
    case TK_AGGREGATE:
    case TK_VARIANT: {
        for (size_t ix = 0; ix < type->components.num_components; ++ix) {
            if (type->components.components[ix].kind != CK_TYPE) {
                return false;
            }
        }
        return true;
    }
    default:
        UNREACHABLE();
    }
}

ErrorOrSize type_sizeof(ExpressionType *type)
{
    switch (type_kind(type)) {
    case TK_PRIMITIVE:
        RETURN(Size, BuiltinType_sizeof(type->builtin_type));
    case TK_ENUM:
        RETURN(Size, typeid_sizeof(type->enumeration.underlying_type));
    case TK_ALIAS:
        return type_sizeof(typeid_canonical_type(type->type_id));
    case TK_AGGREGATE: {
        size_t size = 0;
        for (size_t ix = 0; ix < type->components.num_components; ++ix) {
            if (type->components.components[ix].kind != CK_TYPE) {
                ERROR(Size, TypeError, 0, "Cannot get size of template type");
            }
            ExpressionType *component_type = type_registry_get_type_by_id(type->components.components[ix].type_id);
            size_t          component_size = TRY(Size, type_sizeof(component_type));
            size_t          align = TRY(Size, type_alignat(component_type));
            size = align_at(size, align);
            size += component_size;
        }
        RETURN(Size, size);
    }
    case TK_VARIANT: {
        size_t switch_size = typeid_sizeof(type->variant.enumeration);
        size_t payload_size = TRY(Size, type_sizeof_payload(type));
        for (size_t ix = 0; ix < type->variant.size; ++ix) {
            if (type->variant.elements[ix].kind != CK_TYPE) {
                ERROR(Size, TypeError, 0, "Cannot get size of template type");
            }
            ExpressionType *component_type = type_registry_get_type_by_id(type->variant.elements[ix].type_id);
            size_t          component_size = TRY(Size, type_sizeof(component_type));
            size_t          align = TRY(Size, type_alignat(component_type));
            if (switch_size % align) {
                switch_size = switch_size + align - (switch_size % align);
            }
            if (component_size > payload_size) {
                payload_size = component_size;
            }
        }
        RETURN(Size, switch_size + payload_size);
    }
    default:
        UNREACHABLE();
    }
}

ErrorOrSize type_alignat(ExpressionType *type)
{
    switch (type_kind(type)) {
    case TK_PRIMITIVE:
        RETURN(Size, BuiltinType_integer_type(type->builtin_type) / 8);
    case TK_ALIAS:
        return type_alignat(typeid_canonical_type(type->type_id));
    case TK_AGGREGATE: {
        size_t align = 0;
        for (size_t ix = 0; ix < type->components.num_components; ++ix) {
            if (type->components.components[ix].kind != CK_TYPE) {
                ERROR(Size, TypeError, 0, "Cannot get alignment of template type");
            }
            ExpressionType *component_type = type_registry_get_type_by_id(type->components.components[ix].type_id);
            size_t          component_align = TRY(Size, type_alignat(component_type));
            if (component_align > align) {
                align = component_align;
            }
        }
        RETURN(Size, align);
    }
    case TK_VARIANT: {
        size_t align = typeid_alignat(type->variant.enumeration);
        for (size_t ix = 0; ix < type->variant.size; ++ix) {
            if (type->variant.elements[ix].kind != CK_TYPE) {
                ERROR(Size, TypeError, 0, "Cannot get size of template type");
            }
            ExpressionType *option_type = type_registry_get_type_by_id(type->variant.elements[ix].type_id);
            size_t          option_align = TRY(Size, type_alignat(option_type));
            if (option_align > align) {
                align = option_align;
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
        ERROR(Size, TypeError, 0, "Type '%.*s' is not concrete. Cannot get component offset", SV_ARG(type->name));
    }
    size_t offset = 0;
    for (size_t ix = 0; ix < index; ++ix) {
        ExpressionType *component_type = type_registry_get_type_by_id(type->components.components[ix].type_id);
        size_t          component_size = TRY(Size, type_sizeof(component_type));
        size_t          align = TRY(Size, type_sizeof(component_type));
        offset += align_at(component_size, align);
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
    for (size_t ix = 0; ix < type->components.num_components; ++ix) {
        if (sv_eq(type->components.components[ix].name, name)) {
            RETURN(Size, offset);
        }
        ExpressionType *component_type = type_registry_get_type_by_id(type->components.components[ix].type_id);
        size_t          component_size = TRY(Size, type_sizeof(component_type));
        size_t          align = TRY(Size, type_sizeof(component_type));
        offset += align_at(component_size, align);
    }
    ERROR(Size, TypeError, 0, "Type '%.*s' has no component with name '%.*s'", SV_ARG(type->name), SV_ARG(name));
}

ErrorOrSize type_sizeof_payload(ExpressionType *type)
{
    if (!type_has_kind(type, TK_VARIANT)) {
        ERROR(Size, TypeError, 0, "Type '%.*s' is not an aggregate", SV_ARG(type->name));
    }
    if (!type_is_concrete(type)) {
        ERROR(Size, TypeError, 0, "Type '%.*s' is not concrete. Cannot get payload size", SV_ARG(type->name));
    }
    size_t size = 0;
    for (size_t ix = 0; ix < type->variant.size; ++ix) {
        if (type->variant.elements[ix].kind != CK_TYPE) {
            ERROR(Size, TypeError, 0, "Cannot get size of template type");
        }
        ExpressionType *option_type = type_registry_get_type_by_id(type->variant.elements[ix].type_id);
        size_t          option_size = TRY(Size, type_sizeof(option_type));
        if (option_size > size) {
            size = option_size;
        }
    }
    RETURN(Size, size);
}

ErrorOrSize type_offsetof_payload(ExpressionType *type)
{
    if (!type_has_kind(type, TK_VARIANT)) {
        ERROR(Size, TypeError, 0, "Type '%.*s' is not an aggregate", SV_ARG(type->name));
    }
    if (!type_is_concrete(type)) {
        ERROR(Size, TypeError, 0, "Type '%.*s' is not concrete. Cannot get payload offset", SV_ARG(type->name));
    }
    size_t offset = typeid_sizeof(type->variant.enumeration);
    for (size_t ix = 0; ix < type->variant.size; ++ix) {
        if (type->variant.elements[ix].kind != CK_TYPE) {
            ERROR(Size, TypeError, 0, "Cannot get size of template type");
        }
        ExpressionType *option_type = type_registry_get_type_by_id(type->variant.elements[ix].type_id);
        size_t          option_align = TRY(Size, type_sizeof(option_type));
        offset = align_at(offset, option_align);
    }
    RETURN(Size, offset);
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
        type->template_parameters[ix].name = sv_copy(parameters[ix].name);
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
                if (param->type != arguments[ix].arg_type) {
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
        if (type_registry.elements[ix]->specialization_of == template_id) {
            bool matches = true;
            for (size_t arg_ix = 0; arg_ix < num; ++arg_ix) {
                assert(arg_ix < type_registry.elements[ix]->num_arguments);
                TemplateArgument *arg = type_arguments + arg_ix;
                TemplateArgument *other_arg = type_registry.elements[ix]->template_arguments + arg_ix;
                assert(arg->arg_type == other_arg->arg_type);
                assert(sv_eq(arg->name, other_arg->name));
                switch (arg->arg_type) {
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
                RETURN(TypeID, type_registry.elements[ix]->type_id);
            }
        }
    }

    StringBuilder name = sb_create();
    sb_printf(&name, SV_SPEC "<", SV_ARG(template_type->name));
    char *comma = "";
    for (size_t ix = 0; ix < num; ++ix) {
        TemplateArgument *arg = type_arguments + ix;
        sb_append_cstr(&name, comma);
        comma = ",";
        sb_append_sv(&name, arg->name);
        sb_append_cstr(&name, "=");
        switch (arg->arg_type) {
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

    type_id         new_id = TRY(TypeID, type_registry_make_type(name.view, typeid_kind(template_id), typeid_builtin_type(template_id)));
    ExpressionType *type = type_registry_get_type_by_id(new_id);
    assert(type);
    type->specialization_of = template_type->type_id;
    type->num_arguments = num;
    type->template_arguments = type_arguments;
    switch (type_kind(type)) {
    case TK_ALIAS:
        type->alias_for_id = template_type->alias_for_id;
        break;
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
            case CK_PARAMETERIZED_TYPE: {
                TemplateArgument *arg = type_get_argument(type, template_comp->parameterized_type.argument);
                if (arg) {
                    type_comp->name = template_comp->name;
                    type_comp->kind = CK_TYPE;
                    TemplateArgument template_arg = *arg;
                    template_arg.name = template_comp->parameterized_type.parameter;
                    type_comp->type_id = MUST(TypeID, type_specialize_template(template_comp->parameterized_type.template_type, 1, &template_arg));
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
    case TK_ENUM:
        NYI("Template specialization of enum");
        break;
    }
    RETURN(TypeID, type->type_id);
}

ErrorOrTypeID type_registry_get_variant(type_id enumeration, ...)
{
    if (typeid_kind(enumeration) != TK_ENUM) {
        ERROR(TypeID, TypeError, 0, "Non-enumeration type '%.*s' passed as variant enumeration", typeid_name(enumeration));
    }
    ExpressionType *enum_type = type_registry_get_type_by_id(enumeration);

    va_list type_args;
    va_start(type_args, enumeration);
    size_t   num = enum_type->enumeration.size;
    type_id *types = alloca(num * sizeof(type_id));
    for (size_t ix = 0; ix < num; ++ix) {
        types[ix] = va_arg(type_args, type_id);
    }
    va_end(type_args);
    return type_registry_get_variant_by_types(enumeration, types);
}

ErrorOrTypeID type_registry_get_variant_by_types(type_id enumeration, type_id *types)
{
    if (typeid_kind(enumeration) != TK_ENUM) {
        ERROR(TypeID, TypeError, 0, "Non-enumeration type '%.*s' passed as variant enumeration", typeid_name(enumeration));
    }
    ExpressionType *enum_type = type_registry_get_type_by_id(enumeration);
    size_t          num = enum_type->enumeration.size;

    for (int ix = 0; ix < type_registry.size; ++ix) {
        if (typeid_kind(type_registry.elements[ix]->type_id) != TK_VARIANT) {
            continue;
        }
        ExpressionType *type = type_registry.elements[ix];
        if (type->enumeration.underlying_type != enumeration) {
            continue;
        }
        bool found = true;
        for (size_t option_ix = 0; option_ix < num; ++option_ix) {
            if (type->variant.elements[option_ix].type_id != types[option_ix]) {
                found = false;
                break;
            }
        }
        if (found) {
            RETURN(TypeID, type_registry.elements[ix]->type_id);
        }
    }

    TypeComponents components;
    StringBuilder  name = sb_copy_sv(enum_type->name);
    sb_append_cstr(&name, "(");
    for (size_t ix = 0; ix < num; ++ix) {
        if (ix > 0) {
            sb_append_cstr(&name, ",");
        }
        ExpressionType *type = type_registry_get_type_by_id(types[ix]);
        if (!type) {
            ERROR(TypeID, TypeError, 0, "Invalid type ID constructing variant");
        }
        sb_append_sv(&name, type->name);
        da_append_TypeComponent(&components, (TypeComponent) { .kind = CK_TYPE, .name = type->name, .type_id = types[ix] });
    }
    sb_append_cstr(&name, ")");
    return type_registry_make_variant(name.view, enumeration, &components);
}

ErrorOrTypeID type_registry_alias(StringView name, type_id aliased)
{
    type_id         new_id = TRY(TypeID, type_registry_make_type(name, TK_ALIAS, BIT_ALIAS));
    ExpressionType *type = type_registry_get_type_by_id(new_id);
    assert(type);
    type->alias_for_id = aliased;
    RETURN(TypeID, new_id);
}

ErrorOrTypeID type_registry_make_aggregate(StringView name, size_t num, TypeComponent *components)
{
    type_id         new_id = TRY(TypeID, type_registry_make_type(name, TK_AGGREGATE, BIT_AGGREGATE));
    ExpressionType *type = type_registry_get_type_by_id(new_id);
    assert(type);
    return type_set_components(new_id, num, components);
}

ErrorOrTypeID type_registry_make_enumeration(StringView name, type_id underlying_type, EnumValues *values)
{
    BuiltinType     bit = typeid_builtin_type(underlying_type);
    type_id         new_id = TRY(TypeID, type_registry_make_type(name, TK_ENUM, bit));
    ExpressionType *type = type_registry_get_type_by_id(new_id);
    assert(type);
    type->enumeration.underlying_type = underlying_type;
    for (size_t ix = 0; ix < values->size; ++ix) {
        for (size_t val_ix = 0; val_ix < ix; ++val_ix) {
            EnumValue elem_ix = values->elements[ix];
            EnumValue elem_val_ix = values->elements[val_ix];
            if (sv_eq(elem_ix.name, elem_val_ix.name)) {
                ERROR(TypeID, TypeError, 0, "Duplicate enumeration value");
            }
            if (integer_equals(elem_ix.value, elem_val_ix.value)) {
                ERROR(TypeID, TypeError, 0, "Duplicate enumeration value");
            }
        }
        DIA_APPEND(EnumValue, (&type->enumeration), values->elements[ix]);
    }
    RETURN(TypeID, new_id);
}

ErrorOrTypeID type_registry_make_variant(StringView name, type_id enumeration, TypeComponents *options)
{
    if (typeid_kind(enumeration) != TK_ENUM) {
        ERROR(TypeID, TypeError, 0, "Non-enumeration type '%.*s' passed as variant enumeration", SV_ARG(typeid_name(enumeration)));
    }
    ExpressionType *enum_type = type_registry_get_type_by_id(enumeration);
    if (enum_type->enumeration.size != options->size) {
        ERROR(TypeID, TypeError, 0, "Enumeration size does not match number of variant options");
    }

    type_id         new_id = TRY(TypeID, type_registry_make_type(name, TK_VARIANT, BIT_VARIANT));
    ExpressionType *type = type_registry_get_type_by_id(new_id);
    type->variant.enumeration = enumeration;
    for (int ix = 0; ix < options->size; ++ix) {
        DIA_APPEND(TypeComponent, (&type->variant), options->elements[ix]);
    }
    RETURN(TypeID, new_id);
}

ErrorOrTypeID type_set_components(type_id aggregate_id, size_t num, TypeComponent *components)
{
    ExpressionType *type = type_registry_get_type_by_id(aggregate_id);
    for (size_t ix = 0; ix < num; ++ix) {
        TypeComponent *component = components + ix;
        switch (component->kind) {
        case CK_TYPE: {
            ExpressionType *expr_type = type_registry_get_type_by_id(component->type_id);
            if (!expr_type) {
                ERROR(TypeID, TypeError, 0, "Unknown type for struct component");
            }
        } break;
        case CK_TEMPLATE_PARAM: {
            TemplateParameter *param = type_get_parameter(type, component->param);
            if (!param) {
                ERROR(TypeID, TypeError, 0, "Unknown template parameter");
            }
            if (param->type != TPT_TYPE) {
                ERROR(TypeID, TypeError, 0, "Template parameter is not a type");
            }
        } break;
        case CK_PARAMETERIZED_TYPE: {
            ExpressionType    *template_type = type_registry_get_type_by_id(component->parameterized_type.template_type);
            TemplateParameter *param = type_get_parameter(template_type, component->parameterized_type.parameter);
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

    type->components.num_components = num;
    type->components.components = allocate_array(TypeComponent, num);
    for (size_t ix = 0; ix < num; ++ix) {
        type->components.components[ix].kind = components[ix].kind;
        type->components.components[ix].name = sv_copy(components[ix].name);
        switch (components[ix].kind) {
        case CK_TYPE: {
            type->components.components[ix].type_id = components[ix].type_id;
        } break;
        case CK_TEMPLATE_PARAM: {
            type->components.components[ix].param = sv_copy(components[ix].param);
        } break;
        case CK_PARAMETERIZED_TYPE: {
            type->components.components[ix].parameterized_type = components[ix].parameterized_type;
        } break;
        default:
            UNREACHABLE();
        }
    }
    RETURN(TypeID, type->type_id);
}

type_id typeid_pointer_to(type_id type)
{
    return MUST(TypeID,
        type_specialize_template(VAR_POINTER_ID, 1,
            (TemplateArgument[]) {
                {
                    .name = sv_from("T"),
                    .arg_type = TPT_TYPE,
                    .type = type,
                },
            }));
}

type_id typeid_pointer_references(type_id type)
{
    assert(typeid_builtin_type(type) == BIT_VAR_POINTER);
    ExpressionType   *et = type_registry_get_type_by_id(type);
    TemplateArgument *template_arg = type_get_argument(et, sv_from("T"));
    return template_arg->type;
}

#ifdef TYPE_TEST

int main()
{
    type_registry_init();
    printf("typeid_sizeof(STRING_ID): %zu\n", typeid_sizeof(STRING_ID));
    return 0;
}

#endif /* TYPE_TEST */
