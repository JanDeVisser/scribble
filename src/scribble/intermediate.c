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

DA_IMPL(IROperation)
DA_IMPL(IRFunction)
DA_IMPL(IRModule)

typedef struct intermediate {
    union {
        struct {
            size_t loop;
            size_t done;
        } loop;
    };
} Intermediate;

#undef BOUNDNODETYPE_ENUM
#define BOUNDNODETYPE_ENUM(type) static __attribute__((unused)) void generate_##type(BoundNode *node, IRObject *target);
__attribute__((unused)) BOUNDNODETYPES(BOUNDNODETYPE_ENUM)
#undef BOUNDNODETYPE_ENUM

    __attribute__((unused)) void generate_node(BoundNode *node, IRObject *target);

static unsigned int s_label = 1; // Start with 1 so that 0 can be used as a sentinel value.

IRVarDecl *allocate_parameters(size_t num)
{
    return (IRVarDecl *) array_allocate(sizeof(IRVarDecl), num);
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
    op.index = fnc->operations.size + 1;
    da_append_IROperation(&fnc->operations, op);
}

void ir_function_add_push_u64(IRFunction *fnc, uint64_t value)
{
    IROperation op;
    op.operation = IR_PUSH_INT_CONSTANT;
    op.integer = integer_create(BITS_64, true, value);
    ir_function_add_operation(fnc, op);
}

__attribute__((unused)) void generate_ASSIGNMENT(BoundNode *node, IRObject *target)
{
    generate_node(node->assignment.expression, target);
    IROperation op;
    op.operation = IR_POP_VAR;
    op.sv = node->name;
    ir_function_add_operation((IRFunction *) target, op);
}

__attribute__((unused)) void generate_BINARYEXPRESSION(BoundNode *node, IRObject *target)
{
    IROperation op;
    generate_node(node->binary_expr.lhs, target);
    generate_node(node->binary_expr.rhs, target);
    op.operation = IR_BINARY_OPERATOR;
    op.binary_operator.op = node->binary_expr.operator;
    op.binary_operator.lhs = node->binary_expr.lhs->typespec.type_id;
    op.binary_operator.rhs = node->binary_expr.rhs->typespec.type_id;
    ir_function_add_operation((IRFunction *) target, op);
}

__attribute__((unused)) void generate_BLOCK(BoundNode *node, IRObject *target)
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

__attribute__((unused)) void generate_BOOL(BoundNode *node, IRObject *target)
{
    IROperation op;
    op.operation = IR_PUSH_BOOL_CONSTANT;
    op.bool_value = sv_eq_cstr(node->name, "true");
    ir_function_add_operation((IRFunction *) target, op);
}

__attribute__((unused)) void generate_BREAK(BoundNode *node, IRObject *target)
{
    assert(node->block.statements->intermediate);
    IROperation op;
    op.operation = IR_JUMP;
    op.label = node->controlled_statement->intermediate->loop.done;
    ir_function_add_operation((IRFunction *) target, op);
}

__attribute__((unused)) void generate_CAST(BoundNode *node, IRObject *target)
{
    generate_node(node->cast_expr.expr, target);
    IROperation op;
    op.operation = IR_CAST;
    op.integer = integer_create(BITS_64, true, node->cast_expr.cast_to);
    ir_function_add_operation((IRFunction *) target, op);
}

__attribute__((unused)) void generate_COMPOUND_INITIALIZER(BoundNode *node, IRObject *target)
{
    for (BoundNode *arg = node->compound_initializer.argument; arg; arg = arg->next) {
        generate_node(arg, target);
    }
    IROperation op;
    op.operation = IR_NEW_DATUM;
    op.integer = integer_create(BITS_64, true, node->typespec.type_id);
    ir_function_add_operation((IRFunction *) target, op);
}

__attribute__((unused)) void generate_CONTINUE(BoundNode *node, IRObject *target)
{
    assert(node->block.statements->intermediate);
    IROperation op;
    op.operation = IR_JUMP;
    op.label = node->controlled_statement->intermediate->loop.loop;
    ir_function_add_operation((IRFunction *) target, op);
}

__attribute__((unused)) void generate_DECIMAL(BoundNode *node, IRObject *target)
{
    IROperation op;
    op.operation = IR_PUSH_FLOAT_CONSTANT;
    op.double_value = strtod(node->name.ptr, NULL);
    ir_function_add_operation((IRFunction *) target, op);
}

__attribute__((unused)) void generate_FOR(BoundNode *node, IRObject *target)
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
    op.operation = IR_POP_VAR;
    op.sv = node->for_statement.variable->name;
    ir_function_add_operation(fnc, op);
    op.operation = IR_LABEL;
    op.label = node->intermediate->loop.loop;
    ir_function_add_operation(fnc, op);
    op.operation = IR_PUSH_VAR;
    op.sv = node->for_statement.variable->name;
    ir_function_add_operation(fnc, op);
    op.operation = IR_PUSH_VAR_COMPONENT;
    op.var_component.name = sv_from("$range");
    op.var_component.component = 1;
    ir_function_add_operation(fnc, op);
    op.operation = IR_BINARY_OPERATOR;
    op.binary_operator.op = OP_LESS;
    op.binary_operator.lhs = op.binary_operator.rhs = range_of->type_id;

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
    op.integer = integer_create(
        BuiltinType_width(range_of->builtin_type),
        BuiltinType_is_unsigned(range_of->builtin_type),
        1);
    ir_function_add_operation(fnc, op);
    op.operation = IR_BINARY_OPERATOR;
    op.binary_operator.op = OP_ADD;
    op.binary_operator.lhs = op.binary_operator.rhs = range_of->type_id;
    ir_function_add_operation(fnc, op);
    op.operation = IR_POP_VAR;
    op.sv = node->for_statement.variable->name;
    ir_function_add_operation(fnc, op);

    op.operation = IR_JUMP;
    op.label = node->intermediate->loop.loop;
    ir_function_add_operation(fnc, op);
    op.operation = IR_LABEL;
    op.label = node->intermediate->loop.done;
    ir_function_add_operation(fnc, op);
    op.operation = IR_SCOPE_END;
    ir_function_add_operation(fnc, op);
}

__attribute__((unused)) void generate_FUNCTION(BoundNode *node, IRObject *target)
{
    IRModule *module = (IRModule *) target;
    size_t    fnc_ix = da_append_IRFunction(
        &module->functions,
        (IRFunction) {
               .obj_type = OT_FUNCTION,
               .module = module,
               .kind = (node->function.function_impl->type == BNT_NATIVE_FUNCTION) ? FK_NATIVE : FK_SCRIBBLE,
               .name = node->name,
               .type = node->typespec,
        });

    IRFunction *fnc = module->functions.elements + fnc_ix;
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
    generate_node(node->function.function_impl, (IRObject *) fnc);
}

__attribute__((unused)) void generate_FUNCTION_CALL(BoundNode *node, IRObject *target)
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
    op.call.name = node->name;
    op.call.discard_result = node->call.discard_result;
    ir_function_add_operation((IRFunction *) target, op);
}

__attribute__((unused)) void generate_FUNCTION_IMPL(BoundNode *node, IRObject *target)
{
    IRFunction *fnc = (IRFunction *) target;
    IROperation op;
    da_resize_IROperation(&fnc->operations, 256);
    op.operation = IR_SCOPE_BEGIN;
    ir_function_add_operation(fnc, op);
    for (BoundNode *stmt = node->block.statements; stmt; stmt = stmt->next) {
        generate_node(stmt, target);
    }
    op.operation = IR_SCOPE_END;
    ir_function_add_operation(fnc, op);
}

__attribute__((unused)) void generate_IF(BoundNode *node, IRObject *target)
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

__attribute__((unused)) void generate_IMPORT(BoundNode *node, IRObject *target)
{
    for (BoundNode *module = node->import.modules; module; module = module->next) {
        generate_node(module, target);
    }
}

__attribute__((unused)) void generate_INTEGER(BoundNode *node, IRObject *target)
{
    IROperation op;
    op.operation = IR_PUSH_INT_CONSTANT;
    op.integer = node->integer;
    ir_function_add_operation((IRFunction *) target, op);
}

__attribute__((unused)) void generate_LOOP(BoundNode *node, IRObject *target)
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

__attribute__((unused)) void generate_MACRO(BoundNode *node, IRObject *target)
{
    (void) node;
    (void) target;
}

__attribute__((unused)) void generate_MODULE(BoundNode *node, IRObject *target)
{
    IRProgram *program = (IRProgram *) target;
    size_t     mod_ix = da_append_IRModule(
        &program->modules,
        (IRModule) {
                .obj_type = OT_MODULE,
                .program = program,
                .name = node->name,
                .$static = -1,
        });
    for (BoundNode *stmt = node->block.statements; stmt; stmt = stmt->next) {
        generate_node(stmt, (IRObject *) (program->modules.elements + mod_ix));
    }
}

__attribute__((unused)) void generate_NAME(BoundNode *node, IRObject *target)
{
    UNREACHABLE();
}

__attribute__((unused)) void generate_NATIVE_FUNCTION(BoundNode *node, IRObject *target)
{
    IRFunction *function = (IRFunction *) target;
    assert(function->kind == FK_NATIVE);
    function->native_name = node->name;
}

__attribute__((unused)) void generate_PARAMETER(BoundNode *node, IRObject *target)
{
}

__attribute__((unused)) void generate_PROGRAM(BoundNode *node, IRObject *target)
{
    IRProgram *program = (IRProgram *) target;
    assert(program->modules.size == 0 && program->modules.cap > 0);
    size_t builtin_ix = da_append_IRModule(
        &program->modules,
        (IRModule) {
            .obj_type = OT_MODULE,
            .program = program,
            .name = sv_printf("$%.*s_builtins", SV_ARG(program->name)),
        });
    IRModule *builtin = program->modules.elements + builtin_ix;
    da_resize_IRFunction(&builtin->functions, 256);

    builtin->$static = (int) da_append_IRFunction(&builtin->functions,
        (IRFunction) {
            .kind = FK_SCRIBBLE,
            .type = (TypeSpec) { .type_id = VOID_ID, .optional = false },
            .name = sv_from("$static") });
    IRFunction *statik = builtin->functions.elements + builtin->$static;
    for (BoundNode *type = node->program.types; type; type = type->next) {
        generate_node(type, (IRObject *) statik);
    }
    for (BoundNode *import = node->program.imports; import; import = import->next) {
        generate_node(import, target);
    }
    for (BoundNode *module = node->program.modules; module; module = module->next) {
        generate_node(module, target);
    }
}

__attribute__((unused)) void generate_RETURN(BoundNode *node, IRObject *target)
{
    if (node->return_stmt.expression) {
        generate_node(node->return_stmt.expression, target);
    }
    IROperation op;
    op.operation = IR_RETURN;
    op.bool_value = node->return_stmt.expression != NULL;
    ir_function_add_operation((IRFunction *) target, op);
}

__attribute__((unused)) void generate_STRING(BoundNode *node, IRObject *target)
{
    IROperation op;
    op.operation = IR_PUSH_STRING_CONSTANT;
    op.sv = node->name;
    ir_function_add_operation((IRFunction *) target, op);
}

__attribute__((unused)) void generate_STRUCT(BoundNode *node, IRObject *target)
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

__attribute__((unused)) void generate_TERNARYEXPRESSION(BoundNode *node, IRObject *target)
{
    IRFunction  *fnc = (IRFunction *) target;
    unsigned int else_label = next_label();
    unsigned int end_label = next_label();

    ir_function_add_operation(fnc, (IROperation) { .operation = IR_MATCH, .type = node->ternary_expr.if_true->typespec.type_id });
    generate_node(node->ternary_expr.condition, target);
    ir_function_add_operation(fnc, (IROperation) { .operation = IR_CASE, .label = else_label });
    generate_node(node->ternary_expr.if_true, target);
    ir_function_add_operation(fnc, (IROperation) { .operation = IR_END_CASE, .label = end_label });
    ir_function_add_operation(fnc, (IROperation) { .operation = IR_LABEL, .label = else_label });
    ir_function_add_operation(fnc, (IROperation) { .operation = IR_PUSH_BOOL_CONSTANT, .bool_value = true });
    ir_function_add_operation(fnc, (IROperation) { .operation = IR_CASE, .label = end_label });
    generate_node(node->ternary_expr.if_false, target);
    ir_function_add_operation(fnc, (IROperation) { .operation = IR_END_CASE, .label = 0 });
    ir_function_add_operation(fnc, (IROperation) { .operation = IR_LABEL, .label = end_label });
    ir_function_add_operation(fnc, (IROperation) { .operation = IR_END_MATCH });
}

__attribute__((unused)) void generate_TYPE(BoundNode *node, IRObject *target)
{
}

__attribute__((unused)) void generate_TYPE_COMPONENT(BoundNode *node, IRObject *target)
{
}

__attribute__((unused)) void generate_UNARYEXPRESSION(BoundNode *node, IRObject *target)
{
    IROperation op;
    switch (node->unary_expr.operator) {
    case OP_ADDRESS_OF:
        op.operation = IR_PUSH_VAR_ADDRESS;
        op.sv = node->unary_expr.operand->name;
        ir_function_add_operation((IRFunction *) target, op);
        break;
    case OP_DEREFERENCE:
        generate_node(node->unary_expr.operand, target);
        op.operation = IR_DEREFERENCE;
        op.type = node->typespec.type_id;
        ir_function_add_operation((IRFunction *) target, op);
        break;
    default:
        generate_node(node->unary_expr.operand, target);
        op.operation = IR_UNARY_OPERATOR;
        op.unary_operator.op = node->unary_expr.operator;
        op.unary_operator.operand = node->unary_expr.operand->typespec.type_id;
        ir_function_add_operation((IRFunction *) target, op);
        break;
    }
}

__attribute__((unused)) void generate_UNBOUND_NODE(BoundNode *node, IRObject *target)
{
    IRFunction *fnc = (IRFunction *) target;
}

__attribute__((unused)) void generate_UNBOUND_TYPE(BoundNode *node, IRObject *target)
{
    IRFunction *fnc = (IRFunction *) target;
}

__attribute__((unused)) void generate_VARIABLE(BoundNode *node, IRObject *target)
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

__attribute__((unused)) void generate_VARIABLE_DECL(BoundNode *node, IRObject *target)
{
    if (target->obj_type != OT_FUNCTION) {
        return;
    }
    IRFunction *fnc = (IRFunction *) target;
    IROperation op;
    op.operation = IR_DECL_VAR;
    op.var_decl.name = node->name;
    op.var_decl.type = node->typespec;
    ir_function_add_operation(fnc, op);
    if (node->variable_decl.init_expr) {
        generate_node(node->variable_decl.init_expr, (IRObject *) fnc);
        op.operation = IR_POP_VAR;
        op.sv = node->name;
        ir_function_add_operation(fnc, op);
    }
}

__attribute__((unused)) void generate_VARIANT(BoundNode *node, IRObject *target)
{
    generate_STRUCT(node, target);
}

__attribute__((unused)) void generate_WHILE(BoundNode *node, IRObject *target)
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

__attribute__((unused)) void generate_node(BoundNode *node, IRObject *target)
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
    ret.obj_type = OT_PROGRAM;
    ret.name = program->name;
    da_resize_IRModule(&ret.modules, 8);
    generate_node(program, (IRObject *) &ret);
    if (has_option("list-ir")) {
        ir_program_list(ret);
    }
    return ret;
}

IRFunction evaluate(BoundNode *expr)
{
    IRFunction ret = {
        .obj_type = OT_FUNCTION,
        .module = NULL,
        .kind = FK_SCRIBBLE,
        .name = sv_from("** eval **"),
        .type = expr->typespec,
        .num_parameters = 0,
        .parameters = NULL,
    };
    generate_node(expr, (IRObject *) &ret);
    return ret;
}

StringView ir_var_decl_to_string(IRVarDecl *var)
{
    StringBuilder sb = sb_create();
    sb_printf(&sb, SV_SPEC ": ", SV_ARG(var->name));
    sb_append_sv(&sb, typespec_to_string(var->type));
    return sb.view;
}

void ir_var_decl_print(IRVarDecl *var)
{
    AllocatorState as = save_allocator();
    StringView     s = ir_var_decl_to_string(var);
    printf(SV_SPEC, SV_ARG(s));
    release_allocator(as);
}

static StringView _ir_operation_to_string(IROperation *op, char const *prefix)
{
    StringBuilder sb = sb_create();
    sb_printf(&sb, "%1.1s %4zu %-20.20s  ", prefix, op->index, ir_operation_type_name(op->operation));
    switch (op->operation) {
    case IR_CALL:
        sb_printf(&sb, SV_SPEC, SV_ARG(op->call.name));
        break;
    case IR_CASE:
    case IR_END_CASE:
    case IR_JUMP:
    case IR_JUMP_F:
    case IR_JUMP_T:
    case IR_LABEL:
        sb_printf(&sb, "lbl_%zu", op->label);
        break;
    case IR_DEREFERENCE:
    case IR_MATCH:
        sb_printf(&sb, SV_SPEC, SV_ARG(typeid_name(op->type)));
        break;
    case IR_END_MATCH:
        break;
    case IR_POP_VAR:
    case IR_PUSH_VAR:
    case IR_PUSH_VAR_ADDRESS:
    case IR_PUSH_STRING_CONSTANT:
        sb_printf(&sb, SV_SPEC, SV_ARG(op->sv));
        break;
    case IR_POP_VAR_COMPONENT:
    case IR_PUSH_VAR_COMPONENT:
        sb_printf(&sb, SV_SPEC ".%zu", SV_ARG(op->var_component.name), op->var_component.component);
        break;
    case IR_DECL_VAR:
        sb_printf(&sb, "%.*s", SV_ARG(ir_var_decl_to_string(&op->var_decl)));
        break;
    case IR_PUSH_BOOL_CONSTANT:
        sb_printf(&sb, "%s", (op->bool_value) ? "true" : "false");
        break;
    case IR_PUSH_FLOAT_CONSTANT:
        sb_printf(&sb, "%f", op->double_value);
        break;
    case IR_PUSH_INT_CONSTANT:
        if (op->integer.un_signed) {
            sb_printf(&sb, "%" PRIu64 " [0x%08" PRIx64 "]", op->integer.u64, op->integer.u64);
        } else {
            sb_printf(&sb, "%" PRIi64, op->integer.i64);
        }
        sb_printf(&sb, " : %c%d", (op->integer.un_signed) ? 'u' : 'i', (int) op->integer.size);
        break;
    case IR_NEW_DATUM:
        sb_printf(&sb, SV_SPEC " [0x%08" PRIx64 "]", SV_ARG(typeid_name(op->integer.u64)), op->integer.u64);
        break;
    case IR_BINARY_OPERATOR:
        sb_printf(&sb, "%s(%.*s, %.*s)", Operator_name(op->binary_operator.op), SV_ARG(typeid_name(op->binary_operator.lhs)), SV_ARG(typeid_name(op->binary_operator.rhs)));
        break;
    case IR_UNARY_OPERATOR:
        sb_printf(&sb, "%s(%.*s)", Operator_name(op->unary_operator.op), SV_ARG(typeid_name(op->unary_operator.operand)));
        break;
    case IR_RETURN:
        sb_printf(&sb, "%s", (op->bool_value) ? "true" : "false");
        break;
    default:
        break;
    }
    return sb.view;
}

StringView ir_operation_to_string(IROperation *op)
{
    return _ir_operation_to_string(op, "");
}

void ir_operation_print_prefix(IROperation *op, char const *prefix)
{
    StringView s = _ir_operation_to_string(op, prefix);
    printf(SV_SPEC "\n", SV_ARG(s));
}

void ir_operation_print(IROperation *op)
{
    StringView s = _ir_operation_to_string(op, " ");
    printf(SV_SPEC "\n", SV_ARG(s));
}

size_t ir_function_resolve_label(IRFunction *function, size_t label)
{
    assert(function && function->kind == FK_SCRIBBLE);
    for (size_t ix = 0; ix < function->operations.size; ++ix) {
        if (function->operations.elements[ix].operation == IR_LABEL && function->operations.elements[ix].label == label) {
            return ix;
        }
    }
    fatal("Label '%d' not found in function '" SV_SPEC "'", label, SV_ARG(function->name));
}

StringView ir_function_to_string(IRFunction *function)
{
    StringBuilder sb = sb_create();
    sb_printf(&sb, SV_SPEC "(", SV_ARG(function->name));
    StringList params = sl_create();
    for (size_t ix = 0; ix < function->num_parameters; ++ix) {
        IRVarDecl *param = function->parameters + ix;
        sl_push(&params, ir_var_decl_to_string(param));
    }
    sb_append_sv(&sb, sl_join(&params, sv_from(", ")));
    sb_append_cstr(&sb, ") -> ");
    sb_append_sv(&sb, typespec_to_string(function->type));
    return sb.view;
}

void ir_function_print(IRFunction *function)
{
    AllocatorState as = save_allocator();
    StringView     s = ir_function_to_string(function);
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
        for (size_t ix = 0; ix < function->operations.size; ++ix) {
            if (function->operations.elements[ix].operation == IR_LABEL) {
                printf("lbl_%zu:\n", function->operations.elements[ix].label);
            }
            if (ix == mark) {
                ir_operation_print_prefix(function->operations.elements + ix, ">");
            } else {
                ir_operation_print(function->operations.elements + ix);
            }
        }
    } break;
    case FK_NATIVE: {
        printf("  Native Function => %.*s\n", SV_ARG(function->native_name));
    } break;
    default:
        UNREACHABLE();
    }
    printf("\n");
}

void ir_module_list(IRModule *module, bool header)
{
    if (header) {
        printf("Module " SV_SPEC "\n", SV_ARG(module->name));
        printf("============================================\n\n");
    }
    for (size_t fix = 0; fix < module->functions.size; ++fix) {
        if (module->functions.elements[fix].kind == FK_SCRIBBLE) {
            ir_function_list(module->functions.elements + fix, (size_t) -1);
        }
    }
}

IRFunction *ir_module_function_by_name(IRModule *module, StringView name)
{
    for (size_t fix = 0; fix < module->functions.size; ++fix) {
        if (sv_eq(module->functions.elements[fix].name, name)) {
            return module->functions.elements + fix;
        }
    }
    return NULL;
}

void ir_program_list(IRProgram program)
{
    printf("Program " SV_SPEC "\n", SV_ARG(program.name));
    printf("============================================\n\n");
    for (size_t ix = 0; ix < program.modules.size; ++ix) {
        IRModule *module = program.modules.elements + ix;
        ir_module_list(module, program.modules.size > 1);
    }
}

IRFunction *ir_program_function_by_name(IRProgram *program, StringView name)
{
    for (size_t ix = 0; ix < program->modules.size; ++ix) {
        IRModule   *module = program->modules.elements + ix;
        IRFunction *fnc = ir_module_function_by_name(module, name);
        if (fnc) {
            return fnc;
        }
    }
    return NULL;
}
