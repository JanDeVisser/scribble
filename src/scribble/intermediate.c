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

__attribute__((unused)) void generate_ASSIGNMENT(BoundNode *node, IRObject *target)
{
    generate_node(node->assignment.expression, target);
    IROperation op;
    ir_operation_set(&op, IR_PUSH_VAR_ADDRESS);
    op.sv = node->name;
    ir_function_add_operation((IRFunction *) target, op);

    if (node->assignment.variable->variable.subscript != NULL) {
        ir_operation_set(&op, IR_SUBSCRIPT);
        BoundNode *variable = node->assignment.variable;
        op.var_component.type = variable->variable.type;
        ExpressionType *et = type_registry_get_type_by_id(variable->variable.type);
        for (BoundNode *sub = variable->variable.subscript; sub; sub = sub->variable.subscript) {
            switch (type_kind(et)) {
            case TK_AGGREGATE: {
                bool found = false;
                for (size_t ix = 0; ix < et->components.num_components; ++ix) {
                    if (sv_eq(sub->name, et->components.components[ix].name)) {
                        DIA_APPEND(size_t, (&op.var_component), ix);
                        op.var_component.type = et->components.components[ix].type_id;
                        found = true;
                        break;
                    }
                }
                assert(found);
                et = type_registry_get_type_by_id(sub->variable.type);
            } break;
            case TK_VARIANT: {
                ExpressionType *enumeration = type_registry_get_type_by_id(et->variant.enumeration);
                assert(enumeration != NULL);
                ExpressionType *payload = NULL;
                for (size_t ix = 0; ix < enumeration->enumeration.size; ++ix) {
                    EnumValue *v = enumeration->enumeration.elements + ix;
                    if (sv_eq(v->name, sub->name)) {
                        payload = type_registry_get_type_by_id(et->variant.elements[ix].type_id);
                        DIA_APPEND(size_t, (&op.var_component), ix);
                        op.var_component.type = et->variant.elements[ix].type_id;
                        break;
                    }
                }
                assert(payload != NULL);
                et = payload;
            } break;
            default:
                UNREACHABLE();
            }
        }
        ir_function_add_operation((IRFunction *) target, op);
    }
    ir_operation_set(&op, IR_POP_VALUE);
    ir_function_add_operation((IRFunction *) target, op);
}

__attribute__((unused)) void generate_BINARYEXPRESSION(BoundNode *node, IRObject *target)
{
    IROperation op;
    generate_node(node->binary_expr.lhs, target);
    generate_node(node->binary_expr.rhs, target);
    ir_operation_set(&op, IR_BINARY_OPERATOR);
    op.binary_operator.op = node->binary_expr.operator;
    op.binary_operator.lhs = node->binary_expr.lhs->typespec.type_id;
    op.binary_operator.rhs = node->binary_expr.rhs->typespec.type_id;
    ir_function_add_operation((IRFunction *) target, op);
}

__attribute__((unused)) void generate_BLOCK(BoundNode *node, IRObject *target)
{
    IRFunction *fnc = (IRFunction *) target;
    IROperation op;
    ir_operation_set(&op, IR_SCOPE_BEGIN);
    ir_function_add_operation(fnc, op);
    for (BoundNode *stmt = node->block.statements; stmt; stmt = stmt->next) {
        generate_node(stmt, target);
    }
    ir_operation_set(&op, IR_SCOPE_END);
    ir_function_add_operation(fnc, op);
}

__attribute__((unused)) void generate_BOOL(BoundNode *node, IRObject *target)
{
    IROperation op;
    ir_operation_set(&op, IR_PUSH_BOOL_CONSTANT);
    op.bool_value = (bool) node->integer.u8;
    ir_function_add_operation((IRFunction *) target, op);
}

__attribute__((unused)) void generate_BREAK(BoundNode *node, IRObject *target)
{
    assert(node->block.statements->intermediate);
    IROperation op;
    ir_operation_set(&op, IR_JUMP);
    op.label = node->controlled_statement->intermediate->loop.done;
    ir_function_add_operation((IRFunction *) target, op);
}

__attribute__((unused)) void generate_CAST(BoundNode *node, IRObject *target)
{
    generate_node(node->cast_expr.expr, target);
    IROperation op;
    ir_operation_set(&op, IR_CAST);
    op.type = node->cast_expr.cast_to;
    ir_function_add_operation((IRFunction *) target, op);
}

__attribute__((unused)) void generate_COMPOUND(BoundNode *node, IRObject *target)
{
    for (BoundNode *expr = node->compound_expr.expressions; expr; expr = expr->next) {
        generate_node(expr, target);
    }
}

__attribute__((unused)) void generate_CONST(BoundNode *, IRObject *)
{
}

__attribute__((unused)) void generate_CONTINUE(BoundNode *node, IRObject *target)
{
    assert(node->block.statements->intermediate);
    IROperation op;
    ir_operation_set(&op, IR_JUMP);
    op.label = node->controlled_statement->intermediate->loop.loop;
    ir_function_add_operation((IRFunction *) target, op);
}

__attribute__((unused)) void generate_DECIMAL(BoundNode *node, IRObject *target)
{
    IROperation op;
    ir_operation_set(&op, IR_PUSH_FLOAT_CONSTANT);
    op.double_value = node->decimal_value;
    ir_function_add_operation((IRFunction *) target, op);
}

__attribute__((unused)) void generate_ENUMERATION(BoundNode *, IRObject *)
{
}

__attribute__((unused)) void generate_ENUM_VALUE(BoundNode *, IRObject *)
{
}

__attribute__((unused)) void generate_FOR(BoundNode *node, IRObject *target)
{
    IRFunction *fnc = (IRFunction *) target;
    IROperation op;
    ir_operation_set(&op, IR_SCOPE_BEGIN);
    ir_function_add_operation(fnc, op);
    ExpressionType *range = type_registry_get_type_by_id(node->for_statement.range->typespec.type_id);
    ExpressionType *range_of = typeid_canonical_type(type_get_argument(range, sv_from("T"))->type);

    ir_operation_set(&op, IR_DECL_VAR);
    op.var_decl.name = sv_from("$range");
    op.var_decl.type.type_id = range->type_id;
    ir_function_add_operation(fnc, op);

    generate_node(node->for_statement.range, target);

    ir_operation_set(&op, IR_PUSH_VAR_ADDRESS);
    op.sv = sv_from("$range");
    ir_function_add_operation(fnc, op);

    ir_operation_set(&op, IR_POP_VALUE);
    op.sv = sv_from("$range");
    ir_function_add_operation(fnc, op);

    ir_operation_set(&op, IR_DECL_VAR);
    op.var_decl.name = node->for_statement.variable->name;
    op.var_decl.type.type_id = range_of->type_id;
    ir_function_add_operation(fnc, op);

    ir_operation_set(&op, IR_PUSH_VAR_ADDRESS);
    op.sv = sv_from("$range");
    ir_function_add_operation(fnc, op);

    ir_operation_set(&op, IR_SUBSCRIPT);
    op.var_component.type = range_of->type_id;
    DIA_APPEND(size_t, (&op.var_component), 0);
    ir_function_add_operation(fnc, op);

    ir_operation_set(&op, IR_PUSH_VALUE);
    op.sv = node->for_statement.variable->name;
    ir_function_add_operation(fnc, op);

    ir_operation_set(&op, IR_PUSH_VAR_ADDRESS);
    op.sv = node->for_statement.variable->name;
    ir_function_add_operation(fnc, op);

    ir_operation_set(&op, IR_POP_VALUE);
    op.sv = node->for_statement.variable->name;
    ir_function_add_operation(fnc, op);

    node->intermediate = allocate_new(Intermediate);
    node->intermediate->loop.loop = next_label();
    node->intermediate->loop.done = next_label();

    ir_operation_set(&op, IR_LABEL);
    op.label = node->intermediate->loop.loop;
    ir_function_add_operation(fnc, op);

    ir_operation_set(&op, IR_PUSH_VAR_ADDRESS);
    op.sv = node->for_statement.variable->name;
    ir_function_add_operation(fnc, op);

    ir_operation_set(&op, IR_PUSH_VALUE);
    ir_function_add_operation(fnc, op);

    ir_operation_set(&op, IR_PUSH_VAR_ADDRESS);
    op.sv = sv_from("$range");
    ir_function_add_operation(fnc, op);

    ir_operation_set(&op, IR_SUBSCRIPT);
    op.var_component.type = range_of->type_id;
    DIA_APPEND(size_t, (&op.var_component), 1);
    ir_function_add_operation(fnc, op);

    ir_operation_set(&op, IR_PUSH_VALUE);
    ir_function_add_operation(fnc, op);

    ir_operation_set(&op, IR_BINARY_OPERATOR);
    op.binary_operator.op = OP_LESS;
    op.binary_operator.lhs = op.binary_operator.rhs = range_of->type_id;

    ir_function_add_operation(fnc, op);
    ir_operation_set(&op, IR_JUMP_F);
    op.label = node->intermediate->loop.done;
    ir_function_add_operation(fnc, op);

    generate_node(node->for_statement.statement, target);

    ir_operation_set(&op, IR_PUSH_VAR_ADDRESS);
    op.sv = node->for_statement.variable->name;
    ir_function_add_operation(fnc, op);

    ir_operation_set(&op, IR_PUSH_VALUE);
    ir_function_add_operation(fnc, op);

    ir_operation_set(&op, IR_PUSH_INT_CONSTANT);
    assert(typeid_builtin_type(range_of->type_id));
    op.integer = integer_create(BuiltinType_integer_type(range_of->builtin_type), 1);
    ir_function_add_operation(fnc, op);
    ir_operation_set(&op, IR_BINARY_OPERATOR);

    op.binary_operator.op = OP_ADD;
    op.binary_operator.lhs = op.binary_operator.rhs = range_of->type_id;
    ir_function_add_operation(fnc, op);

    ir_operation_set(&op, IR_PUSH_VAR_ADDRESS);
    op.sv = node->for_statement.variable->name;
    ir_function_add_operation(fnc, op);

    ir_operation_set(&op, IR_POP_VALUE);
    op.sv = node->for_statement.variable->name;
    ir_function_add_operation(fnc, op);

    ir_operation_set(&op, IR_JUMP);
    op.label = node->intermediate->loop.loop;
    ir_function_add_operation(fnc, op);

    ir_operation_set(&op, IR_LABEL);
    op.label = node->intermediate->loop.done;
    ir_function_add_operation(fnc, op);

    ir_operation_set(&op, IR_SCOPE_END);
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
    ir_operation_set(&op, IR_CALL);
    op.sv = node->name;
    op.call.name = node->name;
    op.call.discard_result = node->call.discard_result;
    ir_function_add_operation((IRFunction *) target, op);
}

__attribute__((unused)) void generate_FUNCTION_IMPL(BoundNode *node, IRObject *target)
{
    IRFunction *fnc = (IRFunction *) target;
    IROperation op;
    da_resize_IROperation(&fnc->operations, 256);
    ir_operation_set(&op, IR_SCOPE_BEGIN);
    ir_function_add_operation(fnc, op);
    for (BoundNode *stmt = node->block.statements; stmt; stmt = stmt->next) {
        generate_node(stmt, target);
    }
    ir_operation_set(&op, IR_SCOPE_END);
    ir_function_add_operation(fnc, op);
}

__attribute__((unused)) void generate_IF(BoundNode *node, IRObject *target)
{
    IRFunction  *fnc = (IRFunction *) target;
    IROperation  op;
    unsigned int else_label = next_label();

    generate_node(node->if_statement.condition, target);
    ir_operation_set(&op, IR_JUMP_F);
    op.label = else_label;
    ir_function_add_operation(fnc, op);
    generate_node(node->if_statement.if_true, target);
    if (node->if_statement.if_false) {
        unsigned int end_label = next_label();
        ir_operation_set(&op, IR_JUMP);
        op.label = end_label;
        ir_function_add_operation(fnc, op);
        ir_operation_set(&op, IR_LABEL);
        op.label = else_label;
        ir_function_add_operation(fnc, op);
        generate_node(node->if_statement.if_false, target);
        ir_operation_set(&op, IR_LABEL);
        op.label = end_label;
        ir_function_add_operation(fnc, op);
    } else {
        ir_operation_set(&op, IR_LABEL);
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
    ir_operation_set(&op, IR_PUSH_INT_CONSTANT);
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
    ir_operation_set(&op, IR_LABEL);
    op.label = node->intermediate->loop.loop;
    ir_function_add_operation(fnc, op);
    generate_node(node->block.statements, target);
    ir_operation_set(&op, IR_JUMP);
    op.label = node->intermediate->loop.loop;
    ir_function_add_operation(fnc, op);
    ir_operation_set(&op, IR_LABEL);
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
    ir_operation_set(&op, IR_RETURN);
    op.bool_value = node->return_stmt.expression != NULL;
    ir_function_add_operation((IRFunction *) target, op);
}

__attribute__((unused)) void generate_STRING(BoundNode *node, IRObject *target)
{
    IROperation op;
    ir_operation_set(&op, IR_PUSH_STRING_CONSTANT);
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
    ir_operation_set(&op, (node->type == BNT_STRUCT) ? IR_DEFINE_AGGREGATE : IR_DEFINE_VARIANT);
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
    if (typeid_kind(node->unary_expr.operand->typespec.type_id) == TK_VARIANT && node->unary_expr.operator== OP_CARDINALITY) {
        generate_node(node->unary_expr.operand, target);

        ir_operation_set(&op, IR_SUBSCRIPT);
        ExpressionType *et = type_registry_get_type_by_id(node->unary_expr.operand->typespec.type_id);
        op.var_component.type = typeid_canonical_type_id(et->enumeration.underlying_type);
        DIA_APPEND(size_t, (&op.var_component), (size_t) -1);
        ir_function_add_operation((IRFunction *) target, op);
        ir_operation_set(&op, IR_PUSH_VALUE);
        ir_function_add_operation((IRFunction *) target, op);
        return;
    }

    switch (node->unary_expr.operator) {
    case OP_ADDRESS_OF:
        assert(node->unary_expr.operand->type == BNT_VARIABLE);
        ir_operation_set(&op, IR_PUSH_VAR_ADDRESS);
        op.sv = node->unary_expr.operand->name;
        ir_function_add_operation((IRFunction *) target, op);
        break;
    case OP_DEREFERENCE:
        generate_node(node->unary_expr.operand, target);
        ir_operation_set(&op, IR_PUSH_VALUE);
        op.type = node->typespec.type_id;
        ir_function_add_operation((IRFunction *) target, op);
        break;
    default:
        generate_node(node->unary_expr.operand, target);
        ir_operation_set(&op, IR_UNARY_OPERATOR);
        op.unary_operator.op = node->unary_expr.operator;
        op.unary_operator.operand = node->unary_expr.operand->typespec.type_id;
        ir_function_add_operation((IRFunction *) target, op);
        break;
    }
}

__attribute__((unused)) void generate_UNBOUND_NODE(BoundNode *, IRObject *)
{
    UNREACHABLE();
}

__attribute__((unused)) void generate_UNBOUND_TYPE(BoundNode *, IRObject *)
{
    UNREACHABLE();
}

__attribute__((unused)) void generate_VARIABLE(BoundNode *node, IRObject *target)
{
    IROperation op;
    ir_operation_set(&op, IR_PUSH_VAR_ADDRESS);
    op.sv = node->name;
    ir_function_add_operation((IRFunction *) target, op);

    if (node->variable.subscript) {
        ExpressionType *et = type_registry_get_type_by_id(node->variable.type);
        ir_operation_set(&op, IR_SUBSCRIPT);
        for (BoundNode *sub = node->variable.subscript; sub; sub = sub->variable.subscript) {
            switch (type_kind(et)) {
            case TK_AGGREGATE: {
                bool found = false;
                for (size_t ix = 0; ix < et->components.num_components; ++ix) {
                    if (sv_eq(sub->name, et->components.components[ix].name)) {
                        op.var_component.type = et->components.components[ix].type_id;
                        DIA_APPEND(size_t, (&op.var_component), ix);
                        found = true;
                        break;
                    }
                }
                assert(found);
                et = type_registry_get_type_by_id(sub->variable.type);
            } break;
            case TK_VARIANT: {
                ExpressionType *enumeration = type_registry_get_type_by_id(et->variant.enumeration);
                assert(enumeration != NULL);
                ExpressionType *payload = NULL;
                for (size_t ix = 0; ix < enumeration->enumeration.size; ++ix) {
                    EnumValue *v = enumeration->enumeration.elements + ix;
                    if (sv_eq(v->name, sub->name)) {
                        payload = type_registry_get_type_by_id(et->variant.elements[ix].type_id);
                        op.var_component.type = payload->type_id;
                        DIA_APPEND(size_t, (&op.var_component), ix);
                        break;
                    }
                }
                assert(payload != NULL);
                et = payload;
            } break;
            default:
                UNREACHABLE();
            }
        }
        ir_function_add_operation((IRFunction *) target, op);
    }

    ir_operation_set(&op, IR_PUSH_VALUE);
    ir_function_add_operation((IRFunction *) target, op);
}

__attribute__((unused)) void generate_VARIABLE_DECL(BoundNode *node, IRObject *target)
{
    if (target->obj_type != OT_FUNCTION) {
        return;
    }
    IRFunction *fnc = (IRFunction *) target;
    IROperation op;
    ir_operation_set(&op, IR_DECL_VAR);
    op.var_decl.name = node->name;
    op.var_decl.type = node->typespec;
    ir_function_add_operation(fnc, op);
}

__attribute__((unused)) void generate_VARIANT(BoundNode *node, IRObject *target)
{
}

__attribute__((unused)) void generate_VARIANT_OPTION(BoundNode *node, IRObject *target)
{
}

__attribute__((unused)) void generate_WHILE(BoundNode *node, IRObject *target)
{
    IRFunction *fnc = (IRFunction *) target;
    IROperation op;
    node->intermediate = allocate(sizeof(Intermediate));
    node->intermediate->loop.loop = next_label();
    node->intermediate->loop.done = next_label();
    ir_operation_set(&op, IR_LABEL);
    op.label = node->intermediate->loop.loop;
    ir_function_add_operation(fnc, op);
    generate_node(node->while_statement.condition, target);
    ir_operation_set(&op, IR_JUMP_F);
    op.label = node->intermediate->loop.done;
    ir_function_add_operation(fnc, op);
    generate_node(node->while_statement.statement, target);
    ir_operation_set(&op, IR_JUMP);
    op.label = node->intermediate->loop.loop;
    ir_function_add_operation(fnc, op);
    ir_operation_set(&op, IR_LABEL);
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
