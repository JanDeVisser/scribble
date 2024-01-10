/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <fcntl.h>
#include <unistd.h>

#define STATIC_ALLOCATOR
#include <allocate.h>
#include <execute.h>
#include <http.h>
#include <native.h>
#include <options.h>

Datum                 *datum_stack_pop(DatumStack *stack);
uint64_t               datum_stack_pop_u64(DatumStack *stack);
StringView             datum_stack_pop_sv(DatumStack *stack);
void                   datum_stack_push(DatumStack *stack, Datum *datum);
CallStackEntry         call_stack_pop(CallStack *stack);
void                   call_stack_push(CallStack *stack, IRFunction *function, size_t index);
VarList               *scope_variable(Scope *scope, StringView name);
VarList               *scope_declare_variable(Scope *scope, StringView name, type_id type);
Datum                 *pointer_assign(ExecutionContext *ctx, DatumPointer pointer);
char const            *pointer_dereference(ExecutionContext *ctx, DatumPointer pointer);
NextInstructionPointer execute_operation(ExecutionContext *ctx, IROperation *op);

#undef IR_OPERATION_TYPE
#define IR_OPERATION_TYPE(t) static __attribute__((unused)) NextInstructionPointer execute_##t(ExecutionContext *ctx, IROperation *op);
IR_OPERATION_TYPES(IR_OPERATION_TYPE)
#undef IR_OPERATION_TYPE

Datum *datum_stack_pop(DatumStack *stack)
{
    if (!stack->top) {
        Datum *error = datum_allocate(ERROR_ID);
        error->error.exception = "Stack underflow";
        return error;
    }
    Datum *ret = stack->top->datum;
    stack->top = stack->top->prev;
    if (!stack->top) {
        stack->bottom = NULL;
    }
    return ret;
}

uint64_t datum_stack_pop_u64(DatumStack *stack)
{
    Datum *d = datum_stack_pop(stack);
    assert(d->type == U64_ID);
    uint64_t ret = d->integer.u64;
    datum_free(d);
    return ret;
}

StringView datum_stack_pop_sv(DatumStack *stack)
{
    Datum *d = datum_stack_pop(stack);
    assert(d->type == STRING_ID);
    StringView ret = d->string;
    datum_free(d);
    return ret;
}

void datum_stack_push(DatumStack *stack, Datum *datum)
{
    DatumStackEntry *entry = allocate(sizeof(DatumStackEntry));
    assert(datum->type != 0);
    entry->datum = datum;
    entry->prev = stack->top;
    if (!stack->top && !stack->bottom) {
        stack->top = stack->bottom = entry;
    } else {
        assert(stack->top && stack->bottom);
        stack->top = entry;
    }
}

void datum_stack_dump(DatumStack *stack)
{
    for (DatumStackEntry *entry = stack->top; entry; entry = entry->prev) {
        datum_print(entry->datum);
        printf("\n");
    }
}

JSONValue datum_stack_to_json(DatumStack *stack)
{
    JSONValue ret = json_array();
    for (DatumStackEntry *entry = stack->top; entry; entry = entry->prev) {
        json_append(&ret, datum_to_json(entry->datum));
    }
    return ret;
}

CallStackEntry call_stack_pop(CallStack *stack)
{
    assert(stack->top);
    CallStackEntry ret = *stack->top;
    stack->top = stack->top->down;
    if (!stack->top) {
        stack->bottom = NULL;
    }
    return ret;
}

void call_stack_push(CallStack *stack, IRFunction *function, size_t index)
{
    CallStackEntry *entry = allocate(sizeof(CallStackEntry));
    entry->function = function;
    entry->index = index;
    entry->down = stack->top;
    if (!stack->top && !stack->bottom) {
        stack->top = stack->bottom = entry;
    } else {
        assert(stack->top && stack->bottom);
        stack->top = entry;
    }
}

void call_stack_dump(CallStack *stack)
{
    for (CallStackEntry *entry = stack->top; entry; entry = entry->down) {
        printf("%*s%.*s %zu\n", (int) (40 - sv_length(entry->function->name)), "", SV_ARG(entry->function->name), entry->index);
    }
}

JSONValue call_stack_to_json(CallStack *stack)
{
    JSONValue ret = json_array();
    for (CallStackEntry *entry = stack->top; entry; entry = entry->down) {
        JSONValue e = json_object();
        json_set(&e, "function", json_string(entry->function->name));
        json_set(&e, "index", json_int(entry->index));
        json_append(&ret, e);
    }
    return ret;
}

VarList *scope_variable(Scope *scope, StringView name)
{
    for (VarList *var = scope->var_list; var; var = var->next) {
        if (sv_eq(var->name, name)) {
            return var;
        }
    }
    return NULL;
}

VarList *scope_declare_variable(Scope *scope, StringView name, type_id type)
{
    VarList *end;
    for (end = scope->var_list; end && end->next; end = end->next) {
        if (sv_eq(end->name, name)) {
            return NULL;
        }
    }
    VarList *new = allocate(sizeof(VarList));
    new->name = name;
    new->type = type;
    new->value = datum_allocate(type);
    if (end) {
        end->next = new;
    } else {
        scope->var_list = new;
    }
    return new;
}

Datum *datum_error(char const *msg)
{
    Datum *error = datum_allocate(ERROR_ID);
    error->error.exception = msg;
    return error;
}

Datum *_pointer_assign(ExecutionContext *ctx, DatumPointer pointer, Datum *d, size_t ptr_ix)
{
    if (ptr_ix >= pointer.size) {
        switch (datum_kind(d)) {
        case TK_PRIMITIVE:
        case TK_ENUM: {
            return datum_clone_into(d, datum_stack_pop(&ctx->stack));
        }
        case TK_AGGREGATE: {
            datum_free_contents(d);
            datum_initialize(d);
            for (int ix = (int) d->aggregate.num_components - 1; ix >= 0; --ix) {
                Datum *component = datum_stack_pop(&ctx->stack);
                datum_copy(d->aggregate.components + ix, component);
                datum_free(component);
            }
            return d;
        }
        case TK_VARIANT:
            datum_free_contents(d);
            d->variant.payload = datum_stack_pop(&ctx->stack);
            d->variant.tag = datum_stack_pop(&ctx->stack);
            // TODO: Check that the payload is valid for the tag
            return d;
        default:
            UNREACHABLE();
        }
    }

    size_t index = pointer.components[ptr_ix];
    switch (datum_kind(d)) {
    case TK_PRIMITIVE:
    case TK_ENUM: {
        return datum_error("Attempted piecewise assignment of non-compound datum");
    }
    case TK_AGGREGATE: {
        if (index >= d->aggregate.num_components) {
            return datum_error("Attempted dereference of out-of-range component index");
        }
        return _pointer_assign(ctx, pointer, d->aggregate.components + index, ptr_ix + 1);
    }
    case TK_VARIANT: {
        assert(ptr_ix == pointer.size - 1);
        datum_free_contents(d);
        ExpressionType *variant_type = type_registry_get_type_by_id(d->type);
        ExpressionType *enum_type = type_registry_get_type_by_id(variant_type->variant.enumeration);
        if (index >= enum_type->enumeration.size) {
            return datum_error("Attempted dereference of out-of-range enumeration index");
        }
        Datum *tag = datum_allocate(enum_type->type_id);
        tag->integer = integer_create(BuiltinType_integer_type(typeid_builtin_type(enum_type->type_id)), index);
        Datum *payload = datum_stack_pop(&ctx->stack);
        assert(typeid_canonical_type_id(variant_type->variant.elements[index].type_id) == typeid_canonical_type_id(payload->type));
        d->variant.payload = payload;
        return d;
    }
    default:
        UNREACHABLE();
    }
}

Datum *pointer_assign(ExecutionContext *ctx, DatumPointer pointer)
{
    return _pointer_assign(ctx, pointer, pointer.pointer, 0);
}

char const *_pointer_dereference(ExecutionContext *ctx, DatumPointer pointer, Datum *d, size_t ptr_ix)
{
    if (ptr_ix >= pointer.size) {
        switch (typeid_kind(d->type)) {
        case TK_PRIMITIVE:
        case TK_ENUM: {
            Datum *copy = datum_allocate(d->type);
            datum_copy(copy, d);
            datum_stack_push(&ctx->stack, copy);
            return NULL;
        }
        case TK_AGGREGATE: {
            for (size_t ix = 0; ix < d->aggregate.num_components; ++ix) {
                Datum *copy = datum_allocate(d->aggregate.components[ix].type);
                datum_copy(copy, d->aggregate.components + ix);
                datum_stack_push(&ctx->stack, copy);
            }
            return NULL;
        }
        case TK_VARIANT: {
            Datum *copy = datum_allocate(d->variant.tag->type);
            datum_copy(copy, d->variant.tag);
            datum_stack_push(&ctx->stack, copy);
            copy = datum_allocate(d->variant.payload->type);
            datum_copy(copy, d->variant.payload);
            datum_stack_push(&ctx->stack, copy);
            return NULL;
        }
        default:
            UNREACHABLE();
        }
    }

    size_t index = pointer.components[ptr_ix];
    switch (datum_kind(d)) {
    case TK_PRIMITIVE:
    case TK_ENUM: {
        return "Attempted dereference of non-compound datum";
    }
    case TK_AGGREGATE: {
        if (index >= d->aggregate.num_components) {
            return "Attempted dereference of out-of-range component index";
        }
        d = d->aggregate.components + index;
        return _pointer_dereference(ctx, pointer, d, ptr_ix + 1);
    }
    case TK_VARIANT: {
        if (index == (size_t) -1) {
            assert(ptr_ix == pointer.size - 1);
            Datum *copy = datum_allocate(d->variant.tag->type);
            datum_copy(copy, d->variant.tag);
            datum_stack_push(&ctx->stack, copy);
            return NULL;
        }
        ExpressionType *enum_type = type_registry_get_type_by_id(d->variant.tag->type);
        if (index >= enum_type->enumeration.size) {
            return "Attempted dereference of out-of-range enumeration index";
        }
        ExpressionType *variant_type = type_registry_get_type_by_id(d->type);
        assert(variant_type->variant.elements[index].type_id == d->variant.payload->type);
        return _pointer_dereference(ctx, pointer, d->variant.payload, ptr_ix + 1);
    }
    default:
        UNREACHABLE();
    }
}

char const *pointer_dereference(ExecutionContext *ctx, DatumPointer pointer)
{
    return _pointer_dereference(ctx, pointer, pointer.pointer, 0);
}

void scope_dump_variables(Scope *scope)
{
    for (Scope *s = scope; s->enclosing; s = s->enclosing) {
        if (s->var_list) {
            if (s->owner) {
                printf(SV_SPEC ":\n", SV_ARG(s->owner->name));
            } else {
                printf("<anonymous scope>\n");
            }
        }
        for (VarList *var = s->var_list; var; var = var->next) {
            ExpressionType *et = type_registry_get_type_by_id(var->type);
            assert(et);
            printf(SV_SPEC_LALIGN " " SV_SPEC_LALIGN " ", SV_ARG_LALIGN(var->name, 20), SV_ARG_LALIGN(et->name, 20));
            datum_print(var->value);
            printf("\n");
        }
    }
}

#define NIP_NEXT \
    return (NextInstructionPointer) { .type = NIT_RELATIVE, .pointer = 1 }
#define NIP_EXCEPTION(msg) \
    return (NextInstructionPointer) { .type = NIT_EXCEPTION, .exception = msg }
#define NIP_CHECK_DATUM(expr, ...)                                                                       \
    ({                                                                                                   \
        Datum *__d__ = (expr);                                                                           \
        if (__d__->type == ERROR_ID) {                                                                   \
            NextInstructionPointer nip = { .type = NIT_EXCEPTION, .exception = __d__->error.exception }; \
            datums_free(__d__ __VA_OPT__(, ) __VA_ARGS__);                                               \
            return nip;                                                                                  \
        }                                                                                                \
        __d__;                                                                                           \
    })
#define NIP_POP_AND_CHECK(ctx, ...) \
    NIP_CHECK_DATUM(datum_stack_pop(&ctx->stack) __VA_OPT__(, ) __VA_ARGS__)
#define NIP_LABEL(label) \
    return (NextInstructionPointer) { .type = NIT_LABEL, .pointer = label }
#define NIP_FORWARD_OP(ctx, op)                                  \
    do {                                                         \
        NextInstructionPointer nip = execute_operation(ctx, op); \
        if (nip.type != NIT_RELATIVE) {                          \
            return nip;                                          \
        }                                                        \
    } while (0)
#define NIP_FORWARD_TO(fnc, ctx, op)                         \
    do {                                                     \
        NextInstructionPointer nip = execute_##fnc(ctx, op); \
        if (nip.type != NIT_RELATIVE) {                      \
            return nip;                                      \
        }                                                    \
    } while (0)

__attribute__((unused)) NextInstructionPointer execute_ASSERT(ExecutionContext *ctx, IROperation *op)
{
    Datum *assertion = NIP_POP_AND_CHECK(ctx);
    assert(assertion->type == BOOL_ID);
    bool asserted = assertion->bool_value;
    datum_free(assertion);
    if (!asserted) {
        // FIXME include assertion message
        NIP_EXCEPTION("Assertion failed");
    }
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_BINARY_OPERATOR(ExecutionContext *ctx, IROperation *op)
{
    Datum *d2 = NIP_POP_AND_CHECK(ctx);
    Datum *d1 = NIP_POP_AND_CHECK(ctx, d2);
    Datum *result = NIP_CHECK_DATUM(datum_apply(d1, op->binary_operator.op, d2), d1, d2);
    datum_free(d1);
    datum_free(d2);
    datum_stack_push(&ctx->stack, result);
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_CALL(ExecutionContext *ctx, IROperation *op)
{
    if (!ctx->program) {
        NIP_EXCEPTION("Cannot call functions when evaluating expression");
    }
    call_stack_push(&ctx->call_stack, ctx->function, ctx->index);
    IRFunction *function = NULL;
    function = ir_program_function_by_name(ctx->program, op->sv);
    assert(function);
    FunctionReturn func_ret = { 0 };
    switch (function->kind) {
    case FK_SCRIBBLE:
        func_ret = execute_function(ctx, (IRFunction *) function);
        break;
    case FK_NATIVE: {
        Datum **args = allocate_array(Datum *, function->num_parameters);
        trace(CAT_EXECUTE, "Preparing native call of '%.*s'", SV_ARG(function->native_name));
        for (size_t ix = 0; ix < function->num_parameters; ++ix) {
            args[ix] = datum_stack_pop(&ctx->stack);
        }
        Datum *ret = datum_allocate(typeid_canonical_type_id(function->type.type_id));
        native_call(function->native_name, function->num_parameters, args, ret);
        datum_stack_push(&ctx->stack, ret);
    } break;
    default:
        UNREACHABLE();
    }
    CallStackEntry entry = call_stack_pop(&ctx->call_stack);
    ctx->function = entry.function;
    ctx->index = entry.index;
    switch (func_ret.type) {
    case FRT_EXIT:
    case FRT_EXCEPTION:
        return (NextInstructionPointer) { .type = NIT_EXIT };
    default:
        break;
    }
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_CASE(ExecutionContext *ctx, IROperation *op)
{
    NIP_FORWARD_TO(LABEL, ctx, op);
    Datum *match_value = NIP_POP_AND_CHECK(ctx);
    Datum *clone = datum_clone(match_value);
    datum_stack_push(&ctx->stack, match_value);
    datum_stack_push(&ctx->stack, clone);
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_CAST(ExecutionContext *ctx, IROperation *op)
{
    // @implement
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_DECL_VAR(ExecutionContext *ctx, IROperation *op)
{
    VarList *end;
    for (end = ctx->scope->var_list; end && end->next; end = end->next) {
        if (sv_eq(end->name, op->sv)) {
            NIP_EXCEPTION("Variable was previously declared");
        }
    }
    scope_declare_variable(ctx->scope, op->var_decl.name, op->var_decl.type.type_id);
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_DEFINE_AGGREGATE(ExecutionContext *ctx, IROperation *op)
{
    size_t          num_components = datum_stack_pop_u64(&ctx->stack);
    ExpressionType *et = type_registry_get_type_by_name(op->sv);
    if (et) {
        if (type_kind(et) != TK_AGGREGATE) {
            fatal("Attempting to define '%.*s' as an aggregate but it's already defined as a %s", SV_ARG(op->sv), TypeKind_name(type_kind(et)));
        }
        if (et->components.num_components != num_components) {
            fatal("Attempting to define '%.*s' as an aggregate with %zu components but it's already an aggregate with %zu components",
                SV_ARG(op->sv), num_components, et->components.num_components);
        }
    }
    TypeComponent *components = alloca(num_components * sizeof(TypeComponent));
    for (size_t ix = 0; ix < num_components; ++ix) {
        type_id    comp_type = datum_stack_pop_u64(&ctx->stack);
        StringView comp_name = datum_stack_pop_sv(&ctx->stack);
        if (et) {
            if (!sv_eq(et->components.components[ix].name, comp_name)) {
                fatal("Attempting to define '%.*s' as component %zu of aggregate '%.*s' but the component is already defined with type '%.*s'",
                    SV_ARG(comp_name), ix, SV_ARG(et->name), SV_ARG(typeid_name(et->components.components[ix].type_id)));
            }
            if (et->components.components[ix].type_id != comp_type) {
                fatal("Attempting to define '%.*s' with type '%.*s' as component %zu of aggregate '%.*s' but the component is already defined with type '%.*s'",
                    SV_ARG(comp_name), SV_ARG(typeid_name(comp_type)), ix, SV_ARG(et->name), SV_ARG(typeid_name(et->components.components[ix].type_id)));
            }
        }
        components[ix].kind = CK_TYPE;
        components[ix].name = comp_name;
        components[ix].type_id = comp_type;
    }
    if (!et) {
        type_id new_id = MUST(TypeID, type_registry_make_aggregate(op->sv, num_components, components));
    }
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_DEFINE_ALIAS(ExecutionContext *ctx, IROperation *op)
{
    Datum *alias_of = NIP_POP_AND_CHECK(ctx);
    assert(alias_of->type == U64_ID);
    type_id         alias_for_id = datum_unsigned_integer_value(alias_of);
    ExpressionType *et = type_registry_get_type_by_name(op->sv);
    if (et) {
        if (type_kind(et) != TK_ALIAS) {
            fatal("Attempting to define '%.*s' as an alias but it's already defined as a %s", SV_ARG(op->sv), TypeKind_name(type_kind(et)));
        }
        if (et->alias_for_id != alias_for_id) {
            fatal("Attempting to define '%.*s' as an alias for '%.*s' but it's already an alias for '%.*s'",
                SV_ARG(op->sv), SV_ARG(typeid_name(alias_for_id)), SV_ARG(et->name));
        }
    } else {
        MUST(TypeID, type_registry_alias(op->sv, alias_for_id));
    }
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_DEFINE_ARRAY(ExecutionContext *, IROperation *)
{
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_DEFINE_VARIANT(ExecutionContext *ctx, IROperation *op)
{
    size_t          num_variants = datum_stack_pop_u64(&ctx->stack);
    ExpressionType *et = type_registry_get_type_by_name(op->sv);
    if (et) {
        if (type_kind(et) != TK_VARIANT) {
            fatal("Attempting to define '%.*s' as a variant but it's already defined as a %s", SV_ARG(op->sv), TypeKind_name(type_kind(et)));
        }
        if (et->components.num_components != num_variants) {
            fatal("Attempting to define '%.*s' as a variant with %zu variants but it's already a variant with %zu variants",
                SV_ARG(op->sv), num_variants, et->components.num_components);
        }
    }
    type_id *options = alloca(num_variants * sizeof(type_id));
    for (size_t ix = 0; ix < num_variants; ++ix) {
        type_id opt_type = datum_stack_pop_u64(&ctx->stack);
        if (et) {
            if (et->components.components[ix].type_id != opt_type) {
                fatal("Attempting to define '%.*s' as option %zu of variant '%.*s' but the variant is already defined with type '%.*s'",
                    SV_ARG(typeid_name(opt_type)), ix, SV_ARG(et->name), SV_ARG(typeid_name(et->components.components[ix].type_id)));
            }
        }
        options[ix] = opt_type;
    }
    if (!et) {
        MUST(TypeID, type_registry_get_variant(num_variants, options));
    }
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_END_CASE(ExecutionContext *ctx, IROperation *op)
{
    Datum *expression_value = NIP_POP_AND_CHECK(ctx);
    NIP_POP_AND_CHECK(ctx, expression_value); // Ignore!
    datum_stack_push(&ctx->stack, expression_value);
    if (op->label) {
        NIP_FORWARD_TO(LABEL, ctx, op);
    }
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_END_MATCH(ExecutionContext *ctx, IROperation *op)
{
    NIP_FORWARD_TO(LABEL, ctx, op);
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_JUMP(ExecutionContext *, IROperation *op)
{
    NIP_LABEL(op->label);
}

__attribute__((unused)) NextInstructionPointer execute_JUMP_F(ExecutionContext *ctx, IROperation *op)
{
    Datum *cond = NIP_POP_AND_CHECK(ctx);
    assert(cond->type == BOOL_ID);
    bool jump = !cond->bool_value;
    datum_free(cond);
    if (jump) {
        NIP_LABEL(op->label);
    }
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_JUMP_T(ExecutionContext *ctx, IROperation *op)
{
    Datum *cond = NIP_POP_AND_CHECK(ctx);
    assert(cond->type == BOOL_ID);
    bool jump = cond->bool_value;
    datum_free(cond);
    if (jump) {
        NIP_LABEL(op->label);
    }
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_LABEL(ExecutionContext *, IROperation *)
{
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_MATCH(ExecutionContext *, IROperation *)
{
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_NEW_DATUM(ExecutionContext *ctx, IROperation *op)
{
    type_id tid = typeid_canonical_type_id(op->integer.u64);
    assert(typeid_kind(tid) == TK_AGGREGATE);
    ExpressionType *et = type_registry_get_type_by_id(tid);
    Datum          *new_datum = datum_allocate(tid);
    switch (typeid_kind(tid)) {
    case TK_AGGREGATE: {
        for (int ix = (int) et->components.num_components - 1; ix >= 0; --ix) {
            Datum *d = datum_stack_pop(&ctx->stack);
            datum_copy(new_datum->aggregate.components + ix, d);
            datum_free(d);
        }
    } break;
    default:
        UNREACHABLE();
    }
    datum_stack_push(&ctx->stack, new_datum);
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_POP_VALUE(ExecutionContext *ctx, IROperation *op)
{
    Datum *d = NIP_POP_AND_CHECK(ctx);
    assert(d->type == VAR_POINTER_ID);
    Datum *assigned = NIP_CHECK_DATUM(pointer_assign(ctx, d->datum_pointer), d);
    datum_free(d);
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_PUSH_BOOL_CONSTANT(ExecutionContext *ctx, IROperation *op)
{
    Datum *d = datum_allocate(BOOL_ID);
    d->bool_value = op->bool_value;
    datum_stack_push(&ctx->stack, d);
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_PUSH_FLOAT_CONSTANT(ExecutionContext *ctx, IROperation *op)
{
    Datum *d = datum_allocate(FLOAT_ID);
    d->float_value = op->double_value;
    datum_stack_push(&ctx->stack, d);
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_PUSH_INT_CONSTANT(ExecutionContext *ctx, IROperation *op)
{
    Datum *d = datum_make_integer(op->integer);
    datum_stack_push(&ctx->stack, d);
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_PUSH_STRING_CONSTANT(ExecutionContext *ctx, IROperation *op)
{
    Datum *d = datum_allocate(STRING_ID);
    d->string = op->sv;
    datum_stack_push(&ctx->stack, d);
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_PUSH_VALUE(ExecutionContext *ctx, IROperation *)
{
    Datum *d = datum_stack_pop(&ctx->stack);
    assert(d->type == VAR_POINTER_ID);
    char const *msg = pointer_dereference(ctx, d->datum_pointer);
    if (msg) {
        NIP_EXCEPTION(msg);
    }
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_PUSH_VAR_ADDRESS(ExecutionContext *ctx, IROperation *op)
{
    VarList *var = scope_variable(ctx->scope, op->sv);
    if (var == NULL) {
        NIP_EXCEPTION("Unknown variable");
    }
    Datum *d = datum_allocate(VAR_POINTER_ID);
    d->datum_pointer.pointer = var->value;
    datum_stack_push(&ctx->stack, d);
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_RETURN(ExecutionContext *ctx, IROperation *)
{
    NextInstructionPointer nip = { 0 };
    nip.type = NIT_RETURN;
    return nip;
}

__attribute__((unused)) NextInstructionPointer execute_SCOPE_BEGIN(ExecutionContext *ctx, IROperation *op)
{
    Scope *new_scope = allocate(sizeof(Scope));
    new_scope->enclosing = ctx->scope;
    ctx->scope = new_scope;
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_SCOPE_END(ExecutionContext *ctx, IROperation *)
{
    assert_msg(ctx->scope != ctx->function_scope, "Scope stack underflow");
    ctx->scope = ctx->scope->enclosing;
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_SUBSCRIPT(ExecutionContext *ctx, IROperation *op)
{
    Datum *d = NIP_POP_AND_CHECK(ctx);
    assert(d->type == VAR_POINTER_ID);
    for (size_t ix = 0; ix < op->var_component.size; ++ix) {
        DIA_APPEND_ELEMENT(size_t, components, (&d->datum_pointer), op->var_component.elements[ix]);
    }
    datum_stack_push(&ctx->stack, d);
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_UNARY_OPERATOR(ExecutionContext *ctx, IROperation *op)
{
    Datum *d = NIP_POP_AND_CHECK(ctx);
    datum_stack_push(&ctx->stack, datum_apply(d, op->binary_operator.op, NULL));
    datum_free(d);
    NIP_NEXT;
}

__attribute__((unused)) NextInstructionPointer execute_WHEN(ExecutionContext *ctx, IROperation *op)
{
    Datum *match_with_value = NIP_POP_AND_CHECK(ctx);
    Datum *match_value = NIP_POP_AND_CHECK(ctx, match_with_value);
    datum_stack_push(&ctx->stack, match_value);
    datum_stack_push(&ctx->stack, match_with_value);
    IROperation binary_op = {
        .operation = IR_BINARY_OPERATOR,
        .binary_operator = {
            .lhs = match_value->type,
            .op = OP_EQUALS,
            .rhs = match_with_value->type,
        },
    };
    NIP_FORWARD_OP(ctx, &binary_op);
    return execute_JUMP_F(ctx, op);
}

NextInstructionPointer execute_operation(ExecutionContext *ctx, IROperation *op)
{
    switch (op->operation) {
#undef IR_OPERATION_TYPE
#define IR_OPERATION_TYPE(t)         \
    case IR_##t: {                   \
        return execute_##t(ctx, op); \
    } break;
        IR_OPERATION_TYPES(IR_OPERATION_TYPE)
#undef IR_OPERATION_TYPE
    default:
        UNREACHABLE();
    }
}

HttpResponse on_function_entry(ExecutionContext *ctx, HttpResponse response)
{
    return response;
}

HttpResponse on_function_exit(ExecutionContext *ctx, HttpResponse response)
{
    return response;
}

HttpResponse on_operation(ExecutionContext *ctx, HttpResponse response)
{
    return response;
}

HttpResponse after_operation(ExecutionContext *ctx, HttpResponse response)
{
    return response;
}

FunctionReturn execute_function(ExecutionContext *ctx, IRFunction *function)
{
    Scope  func_scope = { 0 };
    Scope *current = ctx->scope;
    Scope *current_function_scope = ctx->function_scope;
    size_t ix = 0;

    func_scope.enclosing = ctx->root_scope;
    func_scope.owner = function;
    ctx->function_scope = &func_scope;
    ctx->scope = &func_scope;
    ctx->function = function;

    for (size_t param_ix = 0; param_ix < function->num_parameters; ++param_ix) {
        IRVarDecl *var_decl = function->parameters + param_ix;
        VarList   *end = NULL;
        for (end = ctx->scope->var_list; end && end->next; end = end->next) {
            if (sv_eq(end->name, var_decl->name)) {
                FunctionReturn ret = { 0 };
                ret.type = FRT_EXCEPTION;
                ret.exception = datum_allocate(ERROR_ID);
                ret.exception->error.exception = "Duplicate parameter name?";
                ret.exception->error.operation = NULL;
                ctx->scope = current;
                ctx->function_scope = current_function_scope;
                return ret;
            }
        }
        scope_declare_variable(ctx->scope, var_decl->name, var_decl->type.type_id);
    }
    if (ctx->debug) {
        JSONValue resp = HTTP_POST_CALLBACK_MUST(
            ctx->conn->fd,
            "/emulation/function/entry",
            ir_function_to_json(function),
            on_function_entry,
            ctx);
        assert(resp.type == JSON_TYPE_BOOLEAN);
        if (!resp.boolean) {
            FunctionReturn ret = { .type = FRT_EXIT, .exit_code = 0 };
            ctx->scope = current;
            ctx->function_scope = current_function_scope;
            return ret;
        }
    }
    while (ix < function->operations.size) {
        ctx->index = function->operations.elements[ix].index;
        if (ctx->debug) {
            JSONValue resp = HTTP_POST_CALLBACK_MUST(ctx->conn->fd,
                "/emulation/function/on",
                ir_operation_to_json(function->operations.elements[ix]),
                on_operation, ctx);
            assert(resp.type == JSON_TYPE_BOOLEAN);
            if (!resp.boolean) {
                FunctionReturn ret = { .type = FRT_EXIT, .exit_code = 0 };
                ctx->scope = current;
                ctx->function_scope = current_function_scope;
                return ret;
            }
        }

        NextInstructionPointer pointer = execute_operation(ctx, function->operations.elements + ix);

        if (ctx->debug) {
            JSONValue resp = HTTP_POST_CALLBACK_MUST(
                ctx->conn->fd,
                "/emulation/function/after",
                ir_operation_to_json(function->operations.elements[ix]),
                after_operation,
                ctx);
            assert(resp.type == JSON_TYPE_BOOLEAN);
            if (!resp.boolean) {
                FunctionReturn ret = { .type = FRT_EXIT, .exit_code = 0 };
                ctx->scope = current;
                ctx->function_scope = current_function_scope;
                return ret;
            }
        }

        switch (pointer.type) {
        case NIT_RELATIVE:
            ix += pointer.pointer;
            break;
        case NIT_LABEL: {
            ix = MUST(Size, ir_function_resolve_label(func_scope.owner, pointer.pointer));
        } break;
        case NIT_RESET:
            ix = 0;
            break;
        case NIT_RETURN:
            ix = function->operations.size;
            break;
        case NIT_EXIT: {
            FunctionReturn ret = { 0 };
            ret.type = FRT_EXIT;
            ret.exit_code = (int) pointer.pointer;
            ctx->scope = current;
            return ret;
        }
        case NIT_EXCEPTION: {
            FunctionReturn ret = { 0 };
            ret.type = FRT_EXCEPTION;
            ctx->scope = current;
            ctx->function_scope = current_function_scope;
            ret.exception = datum_allocate(ERROR_ID);
            ret.exception->error.exception = pointer.exception;
            ret.exception->error.operation = function->operations.elements + ix;
            return ret;
        }
        }
    }

    if (ctx->debug) {
        JSONValue resp = HTTP_POST_CALLBACK_MUST(
            ctx->conn->fd,
            "/emulation/function/exit",
            ir_function_to_json(function),
            on_function_exit,
            ctx);
        assert(resp.type == JSON_TYPE_BOOLEAN);
        if (!resp.boolean) {
            FunctionReturn ret = { .type = FRT_EXIT, .exit_code = 0 };
            ctx->scope = current;
            ctx->function_scope = current_function_scope;
            return ret;
        }
    }

    ctx->scope = current;
    ctx->function_scope = current_function_scope;
    return (FunctionReturn) { .type = FRT_NORMAL, .return_value = NULL };
}

ErrorOrInt execute(BackendConnection *conn, IRProgram program /*, int argc, char **argv*/)
{
    IRFunction *main = ir_program_function_by_name(&program, sv_from("main"));
    assert(main);
    Scope root_scope = { 0 };
    root_scope.program = &program;
    ExecutionContext ctx = { 0 };
    ctx.conn = conn;
    ctx.program = &program;
    ctx.root_scope = &root_scope;

    ctx.debug = (conn != NULL) && json_get_bool(&conn->config, "debug_emulation", false);

    if (ctx.debug) {
        HTTP_GET_MUST(conn->fd, "/emulation/start", (StringList) { 0 });
    }

    for (size_t ix = 0; ix < program.modules.size; ++ix) {
        IRModule *module = program.modules.elements + ix;
        if (module->$static >= 0) {
            execute_function(&ctx, module->functions.elements + module->$static);
        }
    }

    FunctionReturn ret = execute_function(&ctx, main);

    if (ret.type == FRT_EXCEPTION && ctx.debug) {
        JSONValue exception = json_object();
        if (ret.exception->error.operation) {
            IROperation op = *((IROperation *) ret.exception->error.operation);
            json_set(&exception, "instruction", ir_operation_to_json(op));
        }
        json_set(&exception, "callstack", call_stack_to_json(&ctx.call_stack));
        json_set(&exception, "datastack", datum_stack_to_json(&ctx.stack));
        HTTP_POST_MUST(conn->fd, "/emulation/exception", exception);
        ERROR(Int, RuntimeError, 0, "Exception: %s", ret.exception->error.exception);
    }
    int    exit_code = 0;
    Datum *d = datum_stack_pop(&ctx.stack);
    if (datum_is_integer(d)) {
        exit_code = (int) datum_signed_integer_value(d);
    }
    if (ctx.debug) {
        StringList params = sl_create();
        sl_push(&params, sv_printf("exit_code=%d", exit_code));
        HTTP_GET_MUST(conn->fd, "/emulation/done", params);
    }
    RETURN(Int, exit_code);
}

Datum *evaluate_function(IRFunction function)
{
    Scope            root_scope = { 0 };
    ExecutionContext ctx = { 0 };
    ctx.root_scope = &root_scope;
    FunctionReturn ret = execute_function(&ctx, &function);
    if (ret.type == FRT_EXCEPTION) {
        return NULL;
    }
    return datum_stack_pop(&ctx.stack);
}
