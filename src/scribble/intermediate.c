/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#define STATIC_ALLOCATOR
#include <allocate.h>
#include <http.h>
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

typedef struct {
    IRObject          *target;
    BackendConnection *conn;
} IRContext;

#undef BOUNDNODETYPE_ENUM
#define BOUNDNODETYPE_ENUM(type) static __attribute__((unused)) void generate_##type(BoundNode *node, IRContext *ctx);
__attribute__((unused)) BOUNDNODETYPES(BOUNDNODETYPE_ENUM)
#undef BOUNDNODETYPE_ENUM

    // clang-format off
__attribute__((unused)) void generate_node(BoundNode *node, IRContext *ctx);
// clang-format on

static unsigned int s_label = 1; // Start with 1 so that 0 can be used as a sentinel value.

IRVarDecl *allocate_parameters(size_t num)
{
    return (IRVarDecl *) array_allocate(sizeof(IRVarDecl), num);
}

unsigned int next_label()
{
    return s_label++;
}

void add_operation(IRContext *ctx, IROperation op)
{
    assert(ctx->target->obj_type == OT_FUNCTION);
    if (ctx->conn && json_get_bool(&ctx->conn->config, "debug_intermediate", false)) {
        JSONValue op_json = json_object();
        json_set(&op_json, "operation", json_string(ir_operation_to_string(&op)));
        http_post_message(ctx->conn->fd, sv_from("/intermediate/operation"), op_json);
    }
    ir_function_add_operation((IRFunction *) ctx->target, op);
}

void add_push_u64(IRContext *ctx, uint64_t value)
{
    IROperation op;
    ir_operation_set(&op, IR_PUSH_INT_CONSTANT);
    op.integer = integer_create(U64, value);
    add_operation(ctx, op);
}

__attribute__((unused)) void generate_ASSIGNMENT(BoundNode *node, IRContext *ctx)
{
    generate_node(node->assignment.expression, ctx);
    IROperation op;
    ir_operation_set(&op, IR_PUSH_VAR_ADDRESS);
    op.sv = node->name;
    add_operation(ctx, op);

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
        add_operation(ctx, op);
    }
    ir_operation_set(&op, IR_POP_VALUE);
    add_operation(ctx, op);
}

__attribute__((unused)) void generate_BINARYEXPRESSION(BoundNode *node, IRContext *ctx)
{
    IROperation op;
    generate_node(node->binary_expr.lhs, ctx);
    generate_node(node->binary_expr.rhs, ctx);
    ir_operation_set(&op, IR_BINARY_OPERATOR);
    op.binary_operator.op = node->binary_expr.operator;
    op.binary_operator.lhs = node->binary_expr.lhs->typespec.type_id;
    op.binary_operator.rhs = node->binary_expr.rhs->typespec.type_id;
    add_operation(ctx, op);
}

__attribute__((unused)) void generate_BLOCK(BoundNode *node, IRContext *ctx)
{
    IRFunction *fnc = (IRFunction *) ctx->target;
    IROperation op;
    ir_operation_set(&op, IR_SCOPE_BEGIN);
    add_operation(ctx, op);
    for (BoundNode *stmt = node->block.statements; stmt; stmt = stmt->next) {
        generate_node(stmt, ctx);
    }
    ir_operation_set(&op, IR_SCOPE_END);
    add_operation(ctx, op);
}

__attribute__((unused)) void generate_BOOL(BoundNode *node, IRContext *ctx)
{
    IROperation op;
    ir_operation_set(&op, IR_PUSH_BOOL_CONSTANT);
    op.bool_value = (bool) node->integer.u8;
    add_operation(ctx, op);
}

__attribute__((unused)) void generate_BREAK(BoundNode *node, IRContext *ctx)
{
    assert(node->block.statements->intermediate);
    IROperation op;
    ir_operation_set(&op, IR_JUMP);
    op.label = node->controlled_statement->intermediate->loop.done;
    add_operation(ctx, op);
}

__attribute__((unused)) void generate_CAST(BoundNode *node, IRContext *ctx)
{
    generate_node(node->cast_expr.expr, ctx);
    IROperation op;
    ir_operation_set(&op, IR_CAST);
    op.type = node->cast_expr.cast_to;
    add_operation(ctx, op);
}

__attribute__((unused)) void generate_COMPOUND(BoundNode *node, IRContext *ctx)
{
    for (BoundNode *expr = node->compound_expr.expressions; expr; expr = expr->next) {
        generate_node(expr, ctx);
    }
}

__attribute__((unused)) void generate_CONST(BoundNode *, IRContext *)
{
}

__attribute__((unused)) void generate_CONTINUE(BoundNode *node, IRContext *ctx)
{
    assert(node->block.statements->intermediate);
    IROperation op;
    ir_operation_set(&op, IR_JUMP);
    op.label = node->controlled_statement->intermediate->loop.loop;
    add_operation(ctx, op);
}

__attribute__((unused)) void generate_DECIMAL(BoundNode *node, IRContext *ctx)
{
    IROperation op;
    ir_operation_set(&op, IR_PUSH_FLOAT_CONSTANT);
    op.double_value = node->decimal_value;
    add_operation(ctx, op);
}

__attribute__((unused)) void generate_ENUMERATION(BoundNode *, IRContext *)
{
}

__attribute__((unused)) void generate_ENUM_VALUE(BoundNode *, IRContext *)
{
}

__attribute__((unused)) void generate_FOR(BoundNode *node, IRContext *ctx)
{
    IRFunction *fnc = (IRFunction *) ctx->target;
    IROperation op;
    ir_operation_set(&op, IR_SCOPE_BEGIN);
    add_operation(ctx, op);
    ExpressionType *range = type_registry_get_type_by_id(node->for_statement.range->typespec.type_id);
    ExpressionType *range_of = typeid_canonical_type(type_get_argument(range, sv_from("T"))->type);

    ir_operation_set(&op, IR_DECL_VAR);
    op.var_decl.name = sv_from("$range");
    op.var_decl.type.type_id = range->type_id;
    add_operation(ctx, op);

    generate_node(node->for_statement.range, ctx);

    ir_operation_set(&op, IR_PUSH_VAR_ADDRESS);
    op.sv = sv_from("$range");
    add_operation(ctx, op);

    ir_operation_set(&op, IR_POP_VALUE);
    op.sv = sv_from("$range");
    add_operation(ctx, op);

    ir_operation_set(&op, IR_DECL_VAR);
    op.var_decl.name = node->for_statement.variable->name;
    op.var_decl.type.type_id = range_of->type_id;
    add_operation(ctx, op);

    ir_operation_set(&op, IR_PUSH_VAR_ADDRESS);
    op.sv = sv_from("$range");
    add_operation(ctx, op);

    ir_operation_set(&op, IR_SUBSCRIPT);
    op.var_component.type = range_of->type_id;
    DIA_APPEND(size_t, (&op.var_component), 0);
    add_operation(ctx, op);

    ir_operation_set(&op, IR_PUSH_VALUE);
    op.sv = node->for_statement.variable->name;
    add_operation(ctx, op);

    ir_operation_set(&op, IR_PUSH_VAR_ADDRESS);
    op.sv = node->for_statement.variable->name;
    add_operation(ctx, op);

    ir_operation_set(&op, IR_POP_VALUE);
    op.sv = node->for_statement.variable->name;
    add_operation(ctx, op);

    node->intermediate = allocate_new(Intermediate);
    node->intermediate->loop.loop = next_label();
    node->intermediate->loop.done = next_label();

    ir_operation_set(&op, IR_LABEL);
    op.label = node->intermediate->loop.loop;
    add_operation(ctx, op);

    ir_operation_set(&op, IR_PUSH_VAR_ADDRESS);
    op.sv = node->for_statement.variable->name;
    add_operation(ctx, op);

    ir_operation_set(&op, IR_PUSH_VALUE);
    add_operation(ctx, op);

    ir_operation_set(&op, IR_PUSH_VAR_ADDRESS);
    op.sv = sv_from("$range");
    add_operation(ctx, op);

    ir_operation_set(&op, IR_SUBSCRIPT);
    op.var_component.type = range_of->type_id;
    DIA_APPEND(size_t, (&op.var_component), 1);
    add_operation(ctx, op);

    ir_operation_set(&op, IR_PUSH_VALUE);
    add_operation(ctx, op);

    ir_operation_set(&op, IR_BINARY_OPERATOR);
    op.binary_operator.op = OP_LESS;
    op.binary_operator.lhs = op.binary_operator.rhs = range_of->type_id;

    add_operation(ctx, op);
    ir_operation_set(&op, IR_JUMP_F);
    op.label = node->intermediate->loop.done;
    add_operation(ctx, op);

    generate_node(node->for_statement.statement, ctx);

    ir_operation_set(&op, IR_PUSH_VAR_ADDRESS);
    op.sv = node->for_statement.variable->name;
    add_operation(ctx, op);

    ir_operation_set(&op, IR_PUSH_VALUE);
    add_operation(ctx, op);

    ir_operation_set(&op, IR_PUSH_INT_CONSTANT);
    assert(typeid_builtin_type(range_of->type_id));
    op.integer = integer_create(BuiltinType_integer_type(range_of->builtin_type), 1);
    add_operation(ctx, op);
    ir_operation_set(&op, IR_BINARY_OPERATOR);

    op.binary_operator.op = OP_ADD;
    op.binary_operator.lhs = op.binary_operator.rhs = range_of->type_id;
    add_operation(ctx, op);

    ir_operation_set(&op, IR_PUSH_VAR_ADDRESS);
    op.sv = node->for_statement.variable->name;
    add_operation(ctx, op);

    ir_operation_set(&op, IR_POP_VALUE);
    op.sv = node->for_statement.variable->name;
    add_operation(ctx, op);

    ir_operation_set(&op, IR_JUMP);
    op.label = node->intermediate->loop.loop;
    add_operation(ctx, op);

    ir_operation_set(&op, IR_LABEL);
    op.label = node->intermediate->loop.done;
    add_operation(ctx, op);

    ir_operation_set(&op, IR_SCOPE_END);
    add_operation(ctx, op);
}

__attribute__((unused)) void generate_FUNCTION(BoundNode *node, IRContext *ctx)
{
    IRModule *module = (IRModule *) ctx->target;
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
    ctx->target = (IRObject *) fnc;
    generate_node(node->function.function_impl, ctx);
    ctx->target = (IRObject *) module;
}

__attribute__((unused)) void generate_FUNCTION_CALL(BoundNode *node, IRContext *ctx)
{
    size_t     argc = 0;
    BoundNode *last = NULL;
    for (BoundNode *arg = node->call.argument; arg; arg = arg->next) {
        last = arg;
        argc++;
    }
    for (BoundNode *arg = last; arg; arg = arg->prev) {
        generate_node(arg, ctx);
    }
    IROperation op;
    ir_operation_set(&op, IR_CALL);
    op.sv = node->name;
    op.call.name = node->name;
    op.call.discard_result = node->call.discard_result;
    add_operation(ctx, op);
}

__attribute__((unused)) void generate_FUNCTION_IMPL(BoundNode *node, IRContext *ctx)
{
    IRFunction *fnc = (IRFunction *) ctx->target;
    IROperation op;
    da_resize_IROperation(&fnc->operations, 256);
    ir_operation_set(&op, IR_SCOPE_BEGIN);
    add_operation(ctx, op);
    for (BoundNode *stmt = node->block.statements; stmt; stmt = stmt->next) {
        generate_node(stmt, ctx);
    }
    ir_operation_set(&op, IR_SCOPE_END);
    add_operation(ctx, op);
}

__attribute__((unused)) void generate_IF(BoundNode *node, IRContext *ctx)
{
    IRFunction  *fnc = (IRFunction *) ctx->target;
    IROperation  op;
    unsigned int else_label = next_label();

    generate_node(node->if_statement.condition, ctx);
    ir_operation_set(&op, IR_JUMP_F);
    op.label = else_label;
    add_operation(ctx, op);
    generate_node(node->if_statement.if_true, ctx);
    if (node->if_statement.if_false) {
        unsigned int end_label = next_label();
        ir_operation_set(&op, IR_JUMP);
        op.label = end_label;
        add_operation(ctx, op);
        ir_operation_set(&op, IR_LABEL);
        op.label = else_label;
        add_operation(ctx, op);
        generate_node(node->if_statement.if_false, ctx);
        ir_operation_set(&op, IR_LABEL);
        op.label = end_label;
        add_operation(ctx, op);
    } else {
        ir_operation_set(&op, IR_LABEL);
        op.label = else_label;
        add_operation(ctx, op);
    }
}

__attribute__((unused)) void generate_IMPORT(BoundNode *node, IRContext *ctx)
{
    for (BoundNode *module = node->import.modules; module; module = module->next) {
        generate_node(module, ctx);
    }
}

__attribute__((unused)) void generate_INTEGER(BoundNode *node, IRContext *ctx)
{
    IROperation op;
    ir_operation_set(&op, IR_PUSH_INT_CONSTANT);
    op.integer = node->integer;
    add_operation(ctx, op);
}

__attribute__((unused)) void generate_LOOP(BoundNode *node, IRContext *ctx)
{
    IRFunction *fnc = (IRFunction *) ctx->target;
    node->intermediate = allocate(sizeof(Intermediate));
    node->intermediate->loop.loop = next_label();
    node->intermediate->loop.done = next_label();
    IROperation op;
    ir_operation_set(&op, IR_LABEL);
    op.label = node->intermediate->loop.loop;
    add_operation(ctx, op);
    generate_node(node->block.statements, ctx);
    ir_operation_set(&op, IR_JUMP);
    op.label = node->intermediate->loop.loop;
    add_operation(ctx, op);
    ir_operation_set(&op, IR_LABEL);
    op.label = node->intermediate->loop.done;
    add_operation(ctx, op);
}

__attribute__((unused)) void generate_MACRO(BoundNode *node, IRContext *ctx)
{
    (void) node;
    (void) ctx->target;
}

__attribute__((unused)) void generate_MODULE(BoundNode *node, IRContext *ctx)
{
    IRProgram *program = (IRProgram *) ctx->target;
    size_t     mod_ix = da_append_IRModule(
        &program->modules,
        (IRModule) {
                .obj_type = OT_MODULE,
                .program = program,
                .name = node->name,
                .$static = -1,
        });
    for (BoundNode *stmt = node->block.statements; stmt; stmt = stmt->next) {
        ctx->target = (IRObject *) (program->modules.elements + mod_ix);
        generate_node(stmt, ctx);
    }
    ctx->target = (IRObject *) program;
}

__attribute__((unused)) void generate_NAME(BoundNode *node, IRContext *ctx)
{
    UNREACHABLE();
}

__attribute__((unused)) void generate_NATIVE_FUNCTION(BoundNode *node, IRContext *ctx)
{
    IRFunction *function = (IRFunction *) ctx->target;
    assert(function->kind == FK_NATIVE);
    function->native_name = node->name;
}

__attribute__((unused)) void generate_PARAMETER(BoundNode *node, IRContext *ctx)
{
}

__attribute__((unused)) void generate_PROGRAM(BoundNode *node, IRContext *ctx)
{
    IRProgram *program = (IRProgram *) ctx->target;
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
    ctx->target = (IRObject *) statik;
    for (BoundNode *type = node->program.types; type; type = type->next) {
        generate_node(type, ctx);
    }
    ctx->target = (IRObject *) program;
    for (BoundNode *import = node->program.imports; import; import = import->next) {
        generate_node(import, ctx);
    }
    for (BoundNode *module = node->program.modules; module; module = module->next) {
        generate_node(module, ctx);
    }
}

__attribute__((unused)) void generate_RETURN(BoundNode *node, IRContext *ctx)
{
    if (node->return_stmt.expression) {
        generate_node(node->return_stmt.expression, ctx);
    }
    IROperation op;
    ir_operation_set(&op, IR_RETURN);
    op.bool_value = node->return_stmt.expression != NULL;
    add_operation(ctx, op);
}

__attribute__((unused)) void generate_STRING(BoundNode *node, IRContext *ctx)
{
    IROperation op;
    ir_operation_set(&op, IR_PUSH_STRING_CONSTANT);
    op.sv = node->name;
    add_operation(ctx, op);
}

__attribute__((unused)) void generate_STRUCT(BoundNode *node, IRContext *ctx)
{
    BoundNode *last = NULL;
    size_t     components = 0;
    for (BoundNode *component = node->compound_def.components; component; component = component->next) {
        last = component;
        ++components;
    }
    if (last) {
        for (BoundNode *component = last; component; component = component->prev) {
            generate_node(component, ctx);
        }
    }
    add_push_u64(ctx, components);
    IROperation op;
    ir_operation_set(&op, (node->type == BNT_STRUCT) ? IR_DEFINE_AGGREGATE : IR_DEFINE_VARIANT);
    op.sv = node->name;
    add_operation(ctx, op);
}

__attribute__((unused)) void generate_TERNARYEXPRESSION(BoundNode *node, IRContext *ctx)
{
    unsigned int if_label = next_label();
    unsigned int else_label = next_label();
    unsigned int end_label = next_label();

    add_operation(ctx, (IROperation) { .operation = IR_MATCH, .type = node->ternary_expr.if_true->typespec.type_id });
    generate_node(node->ternary_expr.condition, ctx);
    add_operation(ctx, (IROperation) { .operation = IR_CASE, .label = if_label });
    add_operation(ctx, (IROperation) { .operation = IR_PUSH_BOOL_CONSTANT, .bool_value = true });
    add_operation(ctx, (IROperation) { .operation = IR_WHEN, .label = else_label });
    generate_node(node->ternary_expr.if_true, ctx);
    add_operation(ctx, (IROperation) { .operation = IR_END_CASE, .label = end_label });
    add_operation(ctx, (IROperation) { .operation = IR_CASE, .label = else_label });
    add_operation(ctx, (IROperation) { .operation = IR_PUSH_BOOL_CONSTANT, .bool_value = false });
    add_operation(ctx, (IROperation) { .operation = IR_WHEN, .label = end_label });
    generate_node(node->ternary_expr.if_false, ctx);
    add_operation(ctx, (IROperation) { .operation = IR_END_CASE, .label = 0 });
    add_operation(ctx, (IROperation) { .operation = IR_END_MATCH, .label = end_label });
}

__attribute__((unused)) void generate_TYPE(BoundNode *node, IRContext *ctx)
{
}

__attribute__((unused)) void generate_TYPE_COMPONENT(BoundNode *node, IRContext *ctx)
{
}

__attribute__((unused)) void generate_UNARYEXPRESSION(BoundNode *node, IRContext *ctx)
{
    IROperation op;
    if (typeid_kind(node->unary_expr.operand->typespec.type_id) == TK_VARIANT && node->unary_expr.operator== OP_CARDINALITY) {
        generate_node(node->unary_expr.operand, ctx);

        ir_operation_set(&op, IR_SUBSCRIPT);
        ExpressionType *et = type_registry_get_type_by_id(node->unary_expr.operand->typespec.type_id);
        op.var_component.type = typeid_canonical_type_id(et->enumeration.underlying_type);
        DIA_APPEND(size_t, (&op.var_component), (size_t) -1);
        add_operation(ctx, op);
        ir_operation_set(&op, IR_PUSH_VALUE);
        add_operation(ctx, op);
        return;
    }

    switch (node->unary_expr.operator) {
    case OP_ADDRESS_OF:
        assert(node->unary_expr.operand->type == BNT_VARIABLE);
        ir_operation_set(&op, IR_PUSH_VAR_ADDRESS);
        op.sv = node->unary_expr.operand->name;
        add_operation(ctx, op);
        break;
    case OP_DEREFERENCE:
        generate_node(node->unary_expr.operand, ctx);
        ir_operation_set(&op, IR_PUSH_VALUE);
        op.type = node->typespec.type_id;
        add_operation(ctx, op);
        break;
    default:
        generate_node(node->unary_expr.operand, ctx);
        ir_operation_set(&op, IR_UNARY_OPERATOR);
        op.unary_operator.op = node->unary_expr.operator;
        op.unary_operator.operand = node->unary_expr.operand->typespec.type_id;
        add_operation(ctx, op);
        break;
    }
}

__attribute__((unused)) void generate_UNBOUND_NODE(BoundNode *, IRContext *)
{
    UNREACHABLE();
}

__attribute__((unused)) void generate_UNBOUND_TYPE(BoundNode *, IRContext *)
{
    UNREACHABLE();
}

__attribute__((unused)) void generate_VARIABLE(BoundNode *node, IRContext *ctx)
{
    IROperation op;
    ir_operation_set(&op, IR_PUSH_VAR_ADDRESS);
    op.sv = node->name;
    add_operation(ctx, op);

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

                        // TODO: Wrap this in a !unsafe block once we have that.
                        {
                            IROperation op2 = { 0 };
                            ir_operation_set(&op2, IR_PUSH_VAR_ADDRESS);
                            op2.sv = node->name;
                            add_operation(ctx, op2);
                            ir_operation_set(&op2, IR_SUBSCRIPT);
                            for (size_t sub_ix = 0; sub_ix < op.var_component.size; ++sub_ix) {
                                DIA_APPEND(size_t, (&op2.var_component), op.var_component.elements[sub_ix]);
                            }
                            DIA_APPEND(size_t, (&op2.var_component), -1);
                            add_operation(ctx, op2);
                            ir_operation_set(&op2, IR_PUSH_VALUE);
                            add_operation(ctx, op2);
                            ir_operation_set(&op2, IR_PUSH_INT_CONSTANT);
                            op2.integer = v->value;
                            add_operation(ctx, op2);
                            ir_operation_set(&op2, IR_BINARY_OPERATOR);
                            op2.binary_operator.lhs = op2.binary_operator.rhs = enumeration->enumeration.underlying_type;
                            op2.binary_operator.op = OP_EQUALS;
                            add_operation(ctx, op2);
                            ir_operation_set(&op2, IR_ASSERT);
                            op2.sv = sv_from("Variant tag/payload reference mismatch");
                            add_operation(ctx, op2);
                        }
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
        add_operation(ctx, op);
    }

    ir_operation_set(&op, IR_PUSH_VALUE);
    add_operation(ctx, op);
}

__attribute__((unused)) void generate_VARIABLE_DECL(BoundNode *node, IRContext *ctx)
{
    if (ctx->target->obj_type != OT_FUNCTION) {
        return;
    }
    IRFunction *fnc = (IRFunction *) ctx->target;
    IROperation op;
    ir_operation_set(&op, IR_DECL_VAR);
    op.var_decl.name = node->name;
    op.var_decl.type = node->typespec;
    add_operation(ctx, op);
}

__attribute__((unused)) void generate_VARIANT(BoundNode *node, IRContext *ctx)
{
}

__attribute__((unused)) void generate_VARIANT_OPTION(BoundNode *node, IRContext *ctx)
{
}

__attribute__((unused)) void generate_WHILE(BoundNode *node, IRContext *ctx)
{
    IRFunction *fnc = (IRFunction *) ctx->target;
    IROperation op;
    node->intermediate = allocate(sizeof(Intermediate));
    node->intermediate->loop.loop = next_label();
    node->intermediate->loop.done = next_label();
    ir_operation_set(&op, IR_LABEL);
    op.label = node->intermediate->loop.loop;
    add_operation(ctx, op);
    generate_node(node->while_statement.condition, ctx);
    ir_operation_set(&op, IR_JUMP_F);
    op.label = node->intermediate->loop.done;
    add_operation(ctx, op);
    generate_node(node->while_statement.statement, ctx);
    ir_operation_set(&op, IR_JUMP);
    op.label = node->intermediate->loop.loop;
    add_operation(ctx, op);
    ir_operation_set(&op, IR_LABEL);
    op.label = node->intermediate->loop.done;
    add_operation(ctx, op);
}

__attribute__((unused)) void generate_node(BoundNode *node, IRContext *ctx)
{
    trace(CAT_IR, "Generating IR for %s node '%.*s'", BoundNodeType_name(node->type), SV_ARG(node->name));
    if (ctx->conn && json_get_bool(&ctx->conn->config, "debug_intermediate", false)) {
        http_post_message(ctx->conn->fd, sv_from("/intermediate/node"), bound_node_to_json(node));
    }
    switch (node->type) {
#define BOUNDNODETYPE_ENUM(type) \
    case BNT_##type:             \
        return generate_##type(node, ctx);
        BOUNDNODETYPES(BOUNDNODETYPE_ENUM)
#undef BOUNDNODETYPE_ENUM
    default:
        fatal("Unexpected bound node type '%s' generating IR", BoundNodeType_name(node->type));
    }
}

IRProgram generate(BackendConnection *conn, BoundNode *program)
{
    IRProgram ret = { 0 };
    ret.obj_type = OT_PROGRAM;
    ret.name = program->name;
    da_resize_IRModule(&ret.modules, 8);
    IRContext ctx = { 0 };
    ctx.conn = conn;
    ctx.target = (IRObject *) &ret;
    generate_node(program, &ctx);
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
    IRContext ctx = { 0 };
    ctx.conn = NULL;
    ctx.target = (IRObject *) &ret;
    generate_node(expr, &ctx);
    return ret;
}
