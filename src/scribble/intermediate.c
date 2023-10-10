/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <inttypes.h>

#define STATIC_ALLOCATOR
#include <allocate.h>
#include <intermediate.h>
#include <options.h>

typedef struct intermediate {
    union {
        struct {
            size_t loop;
            size_t done;
        } loop;
    };
} Intermediate;

#undef BOUNDNODETYPE_ENUM
#define BOUNDNODETYPE_ENUM(type) static void generate_##type(BoundNode *parent, void *target);
BOUNDNODETYPES(BOUNDNODETYPE_ENUM)
#undef BOUNDNODETYPE_ENUM

void generate_node(BoundNode *node, void *target);

static unsigned int s_label = 0;

IRVarDecl *allocate_parameters(size_t num)
{
    return (IRVarDecl *) array_allocate(sizeof(IRVarDecl), num);
}

IROperation *allocate_operations(size_t num)
{
    return (IROperation *) array_allocate(sizeof(IROperation), num);
}

IRFunction *allocate_functions(size_t num)
{
    return allocate_array(IRFunction, num);
}

unsigned int next_label()
{
    return s_label++;
}

char const *ir_operation_type_name(IROperationType optype)
{
    switch (optype) {
#undef IR_OPERATION_TYPE
#define IR_OPERATION_TYPE(t) \
    case IR_##t:             \
        return #t;
        IR_OPERATION_TYPES(IR_OPERATION_TYPE)
#undef IR_OPERATION_TYPE
    default:
        UNREACHABLE();
    }
}

void ir_function_add_operation(IRFunction *fnc, IROperation op)
{
    assert(fnc->kind == FK_SCRIBBLE);
    if (fnc->scribble.num_operations == fnc->scribble.cap_operations - 1) {
        IROperation *new_operations = allocate_operations(2 * fnc->scribble.cap_operations);
        memcpy(new_operations, fnc->scribble.operations, fnc->scribble.cap_operations * sizeof(IROperation));
        fnc->scribble.operations = new_operations;
        fnc->scribble.cap_operations *= 2;
    }
    op.index = fnc->scribble.num_operations + 1;
    fnc->scribble.operations[fnc->scribble.num_operations++] = op;
}

void ir_function_add_push_u64(IRFunction *fnc, uint64_t value)
{
    IROperation op;
    op.operation = IR_PUSH_INT_CONSTANT;
    op.integer.type = BIT_U64;
    op.integer.value.unsigned_value = value;
    ir_function_add_operation(fnc, op);
}

void generate_ARRAY(BoundNode *node, void *target)
{
    IRFunction *fnc = (IRFunction *) target;
    IROperation op;
    ir_function_add_push_u64(fnc, node->array_def.base_type);
    ir_function_add_push_u64(fnc, node->array_def.size);
    op.operation = IR_DEFINE_ARRAY;
    op.sv = node->name;
    ir_function_add_operation((IRFunction *) target, op);
}

void generate_ASSIGNMENT(BoundNode *node, void *target)
{
    generate_node(node->assignment.expression, target);
    IROperation op;
    op.operation = IR_POP_VAR;
    op.sv = node->name;
    ir_function_add_operation((IRFunction *) target, op);
}

void generate_BINARYEXPRESSION(BoundNode *node, void *target)
{
    IROperation op;
    generate_node(node->binary_expr.lhs, target);
    generate_node(node->binary_expr.rhs, target);
    op.operation = IR_OPERATOR;
    op.operator.op = node->binary_expr.operator;
    op.operator.lhs = node->binary_expr.lhs->typespec.type_id;
    op.operator.rhs = node->binary_expr.rhs->typespec.type_id;
    ir_function_add_operation((IRFunction *) target, op);
}

void generate_BLOCK(BoundNode *node, void *target)
{
    IRFunction *fnc = (IRFunction *) target;
    IROperation op;
    op.operation = IR_SCOPE_BEGIN;
    ir_function_add_operation(fnc, op);
    for (BoundNode *stmt = node->block.statements; stmt; stmt = stmt->next) {
        generate_node(stmt, target);
    }
    op.operation = IR_SCOPE_END;
    ir_function_add_operation(fnc, op);
}

void generate_BOOL(BoundNode *node, void *target)
{
    IROperation op;
    op.operation = IR_PUSH_BOOL_CONSTANT;
    op.bool_value = sv_eq_cstr(node->name, "true");
    ir_function_add_operation((IRFunction *) target, op);
}

void generate_BREAK(BoundNode *node, void *target)
{
    assert(node->block.statements->intermediate);
    IROperation op;
    op.operation = IR_JUMP;
    op.label = node->controlled_statement->intermediate->loop.done;
    ir_function_add_operation((IRFunction *) target, op);
}

void generate_COMPOUND_INITIALIZER(BoundNode *node, void *target)
{
    for (BoundNode *arg = node->compound_initializer.argument; arg; arg = arg->next) {
        generate_node(arg, target);
    }
    IROperation op;
    op.operation = IR_NEW_DATUM;
    op.integer.value.unsigned_value = node->typespec.type_id;
    ir_function_add_operation((IRFunction *) target, op);
}

void generate_CONTINUE(BoundNode *node, void *target)
{
    assert(node->block.statements->intermediate);
    IROperation op;
    op.operation = IR_JUMP;
    op.label = node->controlled_statement->intermediate->loop.loop;
    ir_function_add_operation((IRFunction *) target, op);
}

void generate_DECIMAL(BoundNode *node, void *target)
{
    IROperation op;
    op.operation = IR_PUSH_FLOAT_CONSTANT;
    op.double_value = strtod(node->name.ptr, NULL);
    ir_function_add_operation((IRFunction *) target, op);
}

void generate_FOR(BoundNode *node, void *target)
{
    IRFunction *fnc = (IRFunction *) target;
    IROperation op;
    op.operation = IR_SCOPE_BEGIN;
    ir_function_add_operation(fnc, op);
    ExpressionType *range = type_registry_get_type_by_id(node->for_statement.range->typespec.type_id);
    ExpressionType *range_of = typeid_canonical_type(type_get_argument(range, sv_from("T"))->type);

    op.operation = IR_DECL_VAR;
    op.var_decl.name = sv_from("$range");
    op.var_decl.type.type_id = range->type_id;
    ir_function_add_operation(fnc, op);
    generate_node(node->for_statement.range, target);
    op.operation = IR_POP_VAR;
    op.sv = sv_from("$range");
    ir_function_add_operation(fnc, op);

    op.operation = IR_DECL_VAR;
    op.var_decl.name = node->for_statement.variable->name;
    op.var_decl.type.type_id = range_of->type_id;
    ir_function_add_operation(fnc, op);
    op.operation = IR_PUSH_VAR_COMPONENT;
    op.var_component.name = sv_from("$range");
    op.var_component.component = 0;
    ir_function_add_operation(fnc, op);

    node->intermediate = allocate_new(Intermediate);
    node->intermediate->loop.loop = next_label();
    node->intermediate->loop.done = next_label();
    op.operation = IR_LABEL;
    op.label = node->intermediate->loop.loop;
    ir_function_add_operation(fnc, op);
    op.operation = IR_POP_VAR;
    op.sv = node->for_statement.variable->name;
    ir_function_add_operation(fnc, op);
    op.operation = IR_PUSH_VAR;
    op.sv = node->for_statement.variable->name;
    ir_function_add_operation(fnc, op);
    op.operation = IR_PUSH_VAR_COMPONENT;
    op.var_component.name = sv_from("$range");
    op.var_component.component = 1;
    ir_function_add_operation(fnc, op);
    op.operation = IR_OPERATOR;
    op.operator.op = OP_LESS;
    op.operator.lhs = op.operator.rhs = range_of->type_id;

    ir_function_add_operation(fnc, op);
    op.operation = IR_JUMP_F;
    op.label = node->intermediate->loop.done;
    ir_function_add_operation(fnc, op);

    generate_node(node->for_statement.statement, target);

    op.operation = IR_PUSH_VAR;
    op.sv = node->for_statement.variable->name;
    ir_function_add_operation(fnc, op);
    op.operation = IR_PUSH_INT_CONSTANT;
    assert(typeid_builtin_type(range_of->type_id));
    op.integer.type = range_of->builtin_type;
    if (BuiltinType_is_unsigned(op.integer.type)) {
        op.integer.value.unsigned_value = 1;
    } else {
        op.integer.value.signed_value = 1;
    }
    ir_function_add_operation(fnc, op);
    op.operation = IR_OPERATOR;
    op.operator.op = OP_ADD;
    op.operator.lhs = op.operator.rhs = range_of->type_id;
    ir_function_add_operation(fnc, op);

    op.operation = IR_JUMP;
    op.label = node->intermediate->loop.loop;
    ir_function_add_operation(fnc, op);
    op.operation = IR_LABEL;
    op.label = node->intermediate->loop.done;
    ir_function_add_operation(fnc, op);
}

void generate_FUNCTION(BoundNode *node, void *target)
{
    IRProgram *program = (IRProgram *) target;
    if (program->num_functions == program->cap_functions - 1) {
        IRFunction *new_functions = allocate_functions(2 * program->cap_functions);
        memcpy(new_functions, program->functions, program->cap_functions * sizeof(IRFunction));
        program->functions = new_functions;
        program->cap_functions *= 2;
    }
    if (sv_eq_cstr(node->name, "main")) {
        program->main = (int) program->num_functions;
    }
    IRFunction *fnc = (IRFunction *) &program->functions[program->num_functions++];
    fnc->program = program;
    fnc->kind = (node->function.function_impl->type == BNT_NATIVE_FUNCTION) ? FK_NATIVE : FK_SCRIBBLE;
    fnc->type = node->typespec;
    fnc->name = node->name;
    for (BoundNode *param = node->function.parameter; param; param = param->next) {
        ++fnc->num_parameters;
    }
    if (fnc->num_parameters) {
        fnc->parameters = allocate_parameters(fnc->num_parameters);
        int ix = 0;
        for (BoundNode *param = node->function.parameter; param; param = param->next) {
            fnc->parameters[ix].name = param->name;
            fnc->parameters[ix].type = param->typespec;
            ++ix;
        }
    }
    generate_node(node->function.function_impl, fnc);
}

void generate_FUNCTION_CALL(BoundNode *node, void *target)
{
    size_t     argc = 0;
    BoundNode *last = NULL;
    for (BoundNode *arg = node->call.argument; arg; arg = arg->next) {
        last = arg;
        argc++;
    }
    for (BoundNode *arg = last; arg; arg = arg->prev) {
        generate_node(arg, target);
    }
    IROperation op;
    op.operation = IR_CALL;
    op.sv = node->name;
    ir_function_add_operation((IRFunction *) target, op);
}

void generate_FUNCTION_IMPL(BoundNode *node, void *target)
{
    IRFunction *fnc = (IRFunction *) target;
    fnc->scribble.cap_operations = 256;
    fnc->scribble.operations = allocate_operations(256);
    for (BoundNode *stmt = node->block.statements; stmt; stmt = stmt->next) {
        generate_node(stmt, target);
    }
}

void generate_IF(BoundNode *node, void *target)
{
    IRFunction  *fnc = (IRFunction *) target;
    IROperation  op;
    unsigned int else_label = next_label();

    generate_node(node->if_statement.condition, target);
    op.operation = IR_JUMP_F;
    op.label = else_label;
    ir_function_add_operation(fnc, op);
    generate_node(node->if_statement.if_true, target);
    if (node->if_statement.if_false) {
        unsigned int end_label = next_label();
        op.operation = IR_JUMP;
        op.label = end_label;
        ir_function_add_operation(fnc, op);
        op.operation = IR_LABEL;
        op.label = else_label;
        ir_function_add_operation(fnc, op);
        generate_node(node->if_statement.if_false, target);
        op.operation = IR_LABEL;
        op.label = end_label;
        ir_function_add_operation(fnc, op);
    } else {
        op.operation = IR_LABEL;
        op.label = else_label;
        ir_function_add_operation(fnc, op);
    }
}

void generate_INTEGER(BoundNode *node, void *target)
{
    IROperation op;
    op.operation = IR_PUSH_INT_CONSTANT;
    ExpressionType *et = type_registry_get_type_by_id(node->typespec.type_id);
    op.integer.type = typeid_builtin_type(typeid_canonical_type_id(node->typespec.type_id));
    if (BuiltinType_is_unsigned(op.integer.type)) {
        op.integer.value.unsigned_value = strtoul(node->name.ptr, NULL, 10);
    } else {
        op.integer.value.signed_value = strtol(node->name.ptr, NULL, 10);
    }
    Integer_boundscheck(op.integer);
    ir_function_add_operation((IRFunction *) target, op);
}

void generate_INTRINSIC(BoundNode *node, void *target)
{
    IRProgram *program = (IRProgram *) target;
    if (program->num_functions == program->cap_functions - 1) {
        IRFunction *new_functions = allocate_functions(2 * program->cap_functions);
        memcpy(new_functions, program->functions, program->cap_functions * sizeof(IRFunction));
        program->functions = new_functions;
        program->cap_functions *= 2;
    }
    IRFunction *intrinsic = program->functions + program->num_functions++;
    intrinsic->program = program;
    intrinsic->kind = FK_INTRINSIC;
    intrinsic->type = node->typespec;
    intrinsic->name = node->name;
    intrinsic->intrinsic = node->intrinsic.intrinsic;
    for (BoundNode *param = node->intrinsic.parameter; param; param = param->next) {
        ++intrinsic->num_parameters;
    }
    if (intrinsic->num_parameters) {
        intrinsic->parameters = allocate_parameters(intrinsic->num_parameters);
        int ix = 0;
        for (BoundNode *param = node->intrinsic.parameter; param; param = param->next) {
            intrinsic->parameters[ix].name = param->name;
            intrinsic->parameters[ix].type = param->typespec;
            ++ix;
        }
    }
}

void generate_LOOP(BoundNode *node, void *target)
{
    IRFunction *fnc = (IRFunction *) target;
    node->intermediate = allocate(sizeof(Intermediate));
    node->intermediate->loop.loop = next_label();
    node->intermediate->loop.done = next_label();
    IROperation op;
    op.operation = IR_LABEL;
    op.label = node->intermediate->loop.loop;
    ir_function_add_operation(fnc, op);
    generate_node(node->block.statements, target);
    op.operation = IR_JUMP;
    op.label = node->intermediate->loop.loop;
    ir_function_add_operation(fnc, op);
    op.operation = IR_LABEL;
    op.label = node->intermediate->loop.done;
    ir_function_add_operation(fnc, op);
}

void generate_MODULE(BoundNode *node, void *target)
{
    for (BoundNode *stmt = node->block.statements; stmt; stmt = stmt->next) {
        generate_node(stmt, target);
    }
}

void generate_NAME(BoundNode *node, void *target)
{
    UNREACHABLE();
}

void generate_NATIVE_FUNCTION(BoundNode *node, void *target)
{
    IRFunction *function = (IRFunction *) target;
    assert(function->kind == FK_NATIVE);
    function->native_name = node->name;
}

void generate_PARAMETER(BoundNode *node, void *target)
{
}

void generate_PROGRAM(BoundNode *node, void *target)
{
    IRProgram *program = (IRProgram *) target;
    program->$static = (int) program->num_functions++;
    IRFunction *statik = (IRFunction *) &program->functions[program->$static];
    statik->kind = FK_SCRIBBLE;
    statik->type = (TypeSpec) { .type_id = VOID_ID, .optional = false };
    statik->name = sv_from("$static");
    statik->scribble.cap_operations = 256;
    statik->scribble.operations = allocate_operations(256);
    for (BoundNode *type = node->program.types; type; type = type->next) {
        generate_node(type, statik);
    }
    for (BoundNode *intrinsic = node->program.intrinsics; intrinsic; intrinsic = intrinsic->next) {
        generate_node(intrinsic, target);
    }
    for (BoundNode *module = node->program.modules; module; module = module->next) {
        generate_node(module, target);
    }
}

void generate_RETURN(BoundNode *node, void *target)
{
    if (node->return_stmt.expression) {
        generate_node(node->return_stmt.expression, target);
    }
    IROperation op;
    op.operation = IR_RETURN;
    op.bool_value = node->return_stmt.expression != NULL;
    ir_function_add_operation((IRFunction *) target, op);
}

void generate_STRING(BoundNode *node, void *target)
{
    IROperation op;
    op.operation = IR_PUSH_STRING_CONSTANT;
    op.sv = node->name;
    ir_function_add_operation((IRFunction *) target, op);
}

void generate_STRUCT(BoundNode *node, void *target)
{
    BoundNode *last;
    size_t     components = 0;
    for (BoundNode *component = node->compound_def.components; component; component = component->next) {
        last = component;
        ++components;
    }
    if (last) {
        for (BoundNode *component = last; component; component = component->prev) {
            generate_node(component, target);
        }
    }
    ir_function_add_push_u64((IRFunction *) target, components);
    IROperation op;
    op.operation = (node->type == BNT_STRUCT) ? IR_DEFINE_AGGREGATE : IR_DEFINE_VARIANT;
    op.sv = node->name;
    ir_function_add_operation((IRFunction *) target, op);
}

void generate_TYPE(BoundNode *node, void *target)
{
    ir_function_add_push_u64((IRFunction *) target, node->typespec.type_id);
    IROperation op;
    op.operation = IR_DEFINE_ALIAS;
    op.sv = node->name;
    ir_function_add_operation((IRFunction *) target, op);
}

void generate_TYPE_COMPONENT(BoundNode *node, void *target)
{
    IRFunction *fnc = (IRFunction *) target;
    IROperation op;
    op.operation = IR_PUSH_STRING_CONSTANT;
    op.sv = node->name;
    ir_function_add_operation(fnc, op);
    ir_function_add_push_u64(fnc, node->typespec.type_id);
}

void generate_UNARYEXPRESSION(BoundNode *node, void *target)
{
    IRFunction *fnc = (IRFunction *) target;
}

void generate_UNBOUND_NODE(BoundNode *node, void *target)
{
    IRFunction *fnc = (IRFunction *) target;
}

void generate_UNBOUND_TYPE(BoundNode *node, void *target)
{
    IRFunction *fnc = (IRFunction *) target;
}

void generate_VARIABLE(BoundNode *node, void *target)
{
    IROperation op;
    if (node->variable.names->next) {
        op.operation = IR_PUSH_VAR_COMPONENT;
        op.var_component.name = node->variable.names->name;
        ExpressionType *et = type_registry_get_type_by_id(node->variable.names->typespec.type_id);
        for (size_t ix = 0; ix < et->components.num_components; ++ix) {
            if (sv_eq(node->variable.names->next->name, et->components.components->name)) {
                op.var_component.component = ix;
                ir_function_add_operation((IRFunction *) target, op);
                return;
            }
        }
        fatal("Could not find index of component '" SV_SPEC "' of type '" SV_SPEC "'. This shouldn't happen",
            SV_ARG(node->variable.names->next->name), SV_ARG(et->name));
    } else {
        op.operation = IR_PUSH_VAR;
        op.sv = node->name;
        ir_function_add_operation((IRFunction *) target, op);
    }
}

void generate_VARIABLE_DECL(BoundNode *node, void *target)
{
    IRFunction *fnc = (IRFunction *) target;
    IROperation op;
    op.operation = IR_DECL_VAR;
    op.var_decl.name = node->name;
    op.var_decl.type = node->typespec;
    ir_function_add_operation(fnc, op);
    if (node->variable_decl.init_expr) {
        generate_node(node->variable_decl.init_expr, fnc);
        op.operation = IR_POP_VAR;
        op.sv = node->name;
        ir_function_add_operation(fnc, op);
    }
}

void generate_VARIANT(BoundNode *node, void *target)
{
    generate_STRUCT(node, target);
}

void generate_WHILE(BoundNode *node, void *target)
{
    IRFunction *fnc = (IRFunction *) target;
    IROperation op;
    node->intermediate = allocate(sizeof(Intermediate));
    node->intermediate->loop.loop = next_label();
    node->intermediate->loop.done = next_label();
    op.operation = IR_LABEL;
    op.label = node->intermediate->loop.loop;
    ir_function_add_operation(fnc, op);
    generate_node(node->while_statement.condition, target);
    op.operation = IR_JUMP_F;
    op.label = node->intermediate->loop.done;
    ir_function_add_operation(fnc, op);
    generate_node(node->while_statement.statement, target);
    op.operation = IR_JUMP;
    op.label = node->intermediate->loop.loop;
    ir_function_add_operation(fnc, op);
    op.operation = IR_LABEL;
    op.label = node->intermediate->loop.done;
    ir_function_add_operation(fnc, op);
}

void generate_node(BoundNode *node, void *target)
{
    trace(CAT_IR, "Generating IR for %s node '" SV_SPEC "'", BoundNodeType_name(node->type), SV_ARG(node->name));
    switch (node->type) {
#define BOUNDNODETYPE_ENUM(type) \
    case BNT_##type:             \
        return generate_##type(node, target);
        BOUNDNODETYPES(BOUNDNODETYPE_ENUM)
#undef BOUNDNODETYPE_ENUM
    default:
        fatal("Unexpected bound node type '%s' generating IR", BoundNodeType_name(node->type));
    }
}

IRProgram generate(BoundNode *program)
{
    IRProgram ret = { 0 };
    ret.name = program->name;
    ret.main = -1;
    ret.$static = -1;
    ret.cap_functions = 256;
    ret.functions = allocate_functions(256);
    generate_node(program, &ret);
    if (has_option("list-ir")) {
        ir_program_list(ret);
    }
    return ret;
}

StringView ir_var_decl_to_string(IRVarDecl *var, Allocator *allocator)
{
    StringBuilder sb = sb_acreate((allocator) ? allocator : get_allocator());
    sb_printf(&sb, SV_SPEC ": ", SV_ARG(var->name));
    sb_append_sv(&sb, typespec_to_string(var->type, allocator));
    return sb.view;
}

void ir_var_decl_print(IRVarDecl *var)
{
    AllocatorState as = save_allocator();
    StringView     s = ir_var_decl_to_string(var, get_allocator());
    printf(SV_SPEC, SV_ARG(s));
    release_allocator(as);
}

void ir_operation_print_prefix(IROperation *op, char const *prefix)
{
    printf("%1.1s %4zu %-20.20s  ", prefix, op->index, ir_operation_type_name(op->operation));
    switch (op->operation) {
    case IR_CALL:
    case IR_POP_VAR:
    case IR_PUSH_VAR:
    case IR_PUSH_STRING_CONSTANT:
        printf(SV_SPEC, SV_ARG(op->sv));
        break;
    case IR_POP_VAR_COMPONENT:
    case IR_PUSH_VAR_COMPONENT:
        printf(SV_SPEC ".%zu", SV_ARG(op->var_component.name), op->var_component.component);
        break;
    case IR_DECL_VAR:
        ir_var_decl_print(&op->var_decl);
        break;
    case IR_PUSH_BOOL_CONSTANT:
        printf("%s", (op->bool_value) ? "true" : "false");
        break;
    case IR_PUSH_FLOAT_CONSTANT:
        printf("%f", op->double_value);
        break;
    case IR_PUSH_INT_CONSTANT:
        if (BuiltinType_is_unsigned(op->integer.type)) {
            printf("%" PRIu64 " [0x%08" PRIx64 "]", op->integer.value.unsigned_value, op->integer.value.unsigned_value);
        } else {
            printf("%" PRIi64, op->integer.value.signed_value);
        }
        break;
    case IR_JUMP:
    case IR_JUMP_F:
    case IR_JUMP_T:
    case IR_LABEL:
        printf("lbl_%zu", op->label);
        break;
    case IR_NEW_DATUM:
        printf(SV_SPEC " [0x%08" PRIx64 "]", SV_ARG(typeid_name(op->integer.value.unsigned_value)), op->integer.value.unsigned_value);
        break;
    case IR_OPERATOR:
        printf("%s(%.*s, %.*s)", Operator_name(op->operator.op), SV_ARG(typeid_name(op->operator.lhs)), SV_ARG(typeid_name(op->operator.rhs)));
        break;
    case IR_RETURN:
        printf("%s", (op->bool_value) ? "true" : "false");
        break;
    default:
        break;
    }
    printf("\n");
}

void ir_operation_print(IROperation *op)
{
    ir_operation_print_prefix(op, " ");
}

size_t ir_function_resolve_label(IRFunction *function, size_t label)
{
    assert(function && function->kind == FK_SCRIBBLE);
    for (size_t ix = 0; ix < function->scribble.num_operations; ++ix) {
        if (function->scribble.operations[ix].operation == IR_LABEL && function->scribble.operations[ix].label == label) {
            return ix;
        }
    }
    fatal("Label '%d' not found in function '" SV_SPEC "'", label, SV_ARG(function->name));
}

StringView ir_function_to_string(IRFunction *function, Allocator *allocator)
{
    StringBuilder sb = sb_acreate((allocator) ? allocator : get_allocator());
    sb_printf(&sb, SV_SPEC "(", SV_ARG(function->name));
    StringList params = sl_acreate(sb.allocator);
    for (size_t ix = 0; ix < function->num_parameters; ++ix) {
        IRVarDecl *param = function->parameters + ix;
        sl_push(&params, ir_var_decl_to_string(param, sb.allocator));
    }
    sb_append_sv(&sb, sl_join(&params, sv_from(", ")));
    sb_append_cstr(&sb, ") -> ");
    sb_append_sv(&sb, typespec_to_string(function->type, sb.allocator));
    return sb.view;
}

void ir_function_print(IRFunction *function)
{
    AllocatorState as = save_allocator();
    StringView     s = ir_function_to_string(function, get_allocator());
    printf(SV_SPEC, SV_ARG(s));
    release_allocator(as);
}

void ir_function_list(IRFunction *function, size_t mark)
{
    printf(SV_SPEC "(", SV_ARG(function->name));
    for (size_t ix = 0; ix < function->num_parameters; ++ix) {
        if (ix > 0)
            printf(", ");
        ir_var_decl_print(function->parameters + ix);
    }
    printf(") -> ");
    typespec_print(stdout, function->type);
    printf("\n");
    printf("--------------------------------------------\n");
    switch (function->kind) {
    case FK_SCRIBBLE: {
        for (size_t ix = 0; ix < function->scribble.num_operations; ++ix) {
            if (function->scribble.operations[ix].operation == IR_LABEL) {
                printf("lbl_%zu:\n", function->scribble.operations[ix].label);
            }
            if (ix == mark) {
                ir_operation_print_prefix(function->scribble.operations + ix, ">");
            } else {
                ir_operation_print(function->scribble.operations + ix);
            }
        }
    } break;
    case FK_NATIVE: {
        printf("  Native Function => %.*s\n", SV_ARG(function->native_name));
    } break;
    case FK_INTRINSIC: {
        printf("  Intrinsic %s\n", Intrinsic_name(function->intrinsic));
    } break;
    default:
        UNREACHABLE();
    }
    printf("\n");
}

void ir_program_list(IRProgram program)
{
    printf("Program " SV_SPEC "\n", SV_ARG(program.name));
    printf("============================================\n\n");
    for (size_t ix = 0; ix < program.num_functions; ++ix) {
        if (program.functions[ix].kind == FK_SCRIBBLE) {
            ir_function_list((IRFunction *) program.functions + ix, (size_t) -1);
        }
    }
}

IRFunction *ir_program_function_by_name(IRProgram *program, StringView name)
{
    for (size_t ix = 0; ix < program->num_functions; ++ix) {
        if (sv_eq(program->functions[ix].name, name)) {
            return program->functions + ix;
        }
    }
    return NULL;
}
