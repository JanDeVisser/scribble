/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <intermediate.h>

#define STATIC_ALLOCATOR
#include <allocate.h>

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

IRAbstractFunction *allocate_functions(size_t num)
{
    return (IRAbstractFunction *) array_allocate(sizeof(IRAbstractFunction), num);
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
    if (fnc->num_operations == fnc->cap_operations - 1) {
        IROperation *new_operations = allocate_operations(2 * fnc->cap_operations);
        memcpy(new_operations, fnc->operations, fnc->cap_operations * sizeof(IROperation));
        fnc->operations = new_operations;
        fnc->cap_operations *= 2;
    }
    op.index = fnc->num_operations + 1;
    fnc->operations[fnc->num_operations++] = op;
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
    op.op = node->binary_expr.operator;
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
}

void generate_CONTINUE(BoundNode *node, void *target)
{
    assert(node->block.statements->intermediate);
    IROperation op;
    op.operation = IR_JUMP;
    op.label = node->controlled_statement->intermediate->loop.loop;
    ir_function_add_operation((IRFunction *) target, op);
}

void generate_FOR(BoundNode *node, void *target)
{
    NYI("generate_FOR");
}

void generate_FUNCTION(BoundNode *node, void *target)
{
    IRProgram *program = (IRProgram *) target;
    if (program->num_functions == program->cap_functions - 1) {
        IRAbstractFunction *new_functions = allocate_functions(2 * program->cap_functions);
        memcpy(new_functions, program->functions, program->cap_functions * sizeof(IRFunction));
        program->functions = new_functions;
        program->cap_functions *= 2;
    }
    if (sv_eq_cstr(node->name, "main")) {
        program->main = (int) program->num_functions;
    }
    IRFunction *fnc = (IRFunction *) &program->functions[program->num_functions++];
    fnc->kind = FK_SCRIBBLE;
    fnc->type = node->typespec;
    fnc->name = node->name;
    fnc->cap_operations = 256;
    fnc->operations = allocate_operations(256);
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
            IROperation op;
            op.operation = IR_DECL_VAR;
            op.var_decl.name = param->name;
            op.var_decl.type = param->typespec;
            ir_function_add_operation(fnc, op);
            op.operation = IR_POP_VAR;
            op.sv = param->name;
            ir_function_add_operation(fnc, op);
        }
    }
    generate_node(node->function.function_impl, fnc);
}

void generate_FUNCTION_CALL(BoundNode *node, void *target)
{
    struct arg_list {
        BoundNode       *arg;
        struct arg_list *prev;
        struct arg_list *next;
    };

    struct arg_list *last_arg = NULL;
    size_t           argc = 0;
    for (BoundNode *arg = node->call.argument; arg; arg = arg->next) {
        struct arg_list *current = allocate(sizeof(struct arg_list));
        current->arg = arg;
        current->prev = last_arg;
        if (last_arg)
            last_arg->next = current;
        last_arg = current;
        ++argc;
    }
    for (struct arg_list *arg = last_arg; arg; arg = arg->prev) {
        generate_node(arg->arg, target);
    }
    IROperation op;
    BoundNode *func = node->call.function;
    switch (func->type) {
    case BNT_FUNCTION:
        if (func->function.function_impl->type == BNT_FUNCTION_IMPL) {
            op.operation = IR_CALL;
            op.sv = node->name;
        } else {
            op.operation = IR_NATIVE_CALL;
            op.native.name = func->function.function_impl->name;
            op.native.signature.argc = argc;
            op.native.signature.types = allocate_array(ExpressionType*, argc);
            int ix = 0;
            for (BoundNode *param = func->function.parameter; param; param = param->next) {
                op.native.signature.types[ix++] = type_registry_get_type_by_id(param->typespec.type_id);
            }
            op.native.signature.ret_type = type_registry_get_type_by_id(func->typespec.type_id);
        }
        break;
    case BNT_INTRINSIC:
        op.operation = IR_CALL;
        op.sv = node->name;
        break;
    default:
        UNREACHABLE();
    }
    ir_function_add_operation((IRFunction *) target, op);
}

void generate_FUNCTION_IMPL(BoundNode *node, void *target)
{
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

void generate_INTRINSIC(BoundNode *node, void *target)
{
    IRProgram *program = (IRProgram *) target;
    if (program->num_functions == program->cap_functions - 1) {
        IRAbstractFunction *new_functions = allocate_functions(2 * program->cap_functions);
        memcpy(new_functions, program->functions, program->cap_functions * sizeof(IRFunction));
        program->functions = new_functions;
        program->cap_functions *= 2;
    }
    IRIntrinsicFunction *intrinsic = (IRIntrinsicFunction *) &program->functions[program->num_functions++];
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

void generate_NUMBER(BoundNode *node, void *target)
{
    IROperation op;
    op.operation = IR_PUSH_INT_CONSTANT;
    ExpressionType *et = type_registry_get_type_by_id(node->typespec.type_id);
    op.integer.width = PrimitiveType_width(et->primitive_type);
    op.integer.un_signed = PrimitiveType_is_unsigned(et->primitive_type);
    if (op.integer.un_signed) {
        op.integer.unsigned_value = strtoul(node->name.ptr, NULL, 10);
    } else {
        op.integer.int_value = strtol(node->name.ptr, NULL, 10);
    }
    ir_function_add_operation((IRFunction *) target, op);
}

void generate_NATIVE_FUNCTION(BoundNode *node, void *target)
{
}

void generate_PARAMETER(BoundNode *node, void *target)
{
}

void generate_PROGRAM(BoundNode *node, void *target)
{
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

void generate_TYPE(BoundNode *node, void *target)
{
    IRFunction *fnc = (IRFunction *) target;
}

void generate_TYPE_COMPONENT(BoundNode *node, void *target)
{
    IRFunction *fnc = (IRFunction *) target;
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
    op.operation = IR_PUSH_VAR;
    op.sv = node->name;
    ir_function_add_operation((IRFunction *) target, op);
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
    trace("Generating IR for %s node '" SV_SPEC "'", BoundNodeType_name(node->type), SV_ARG(node->name));
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
    ret.cap_functions = 256;
    ret.functions = allocate_functions(256);
    generate_node(program, &ret);
    return ret;
}

void ir_var_decl_print(IRVarDecl *var)
{
    printf(SV_SPEC ": ", SV_ARG(var->name));
    typespec_print(stdout, var->type);
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
    case IR_DECL_VAR:
        ir_var_decl_print(&op->var_decl);
        break;
    case IR_PUSH_BOOL_CONSTANT:
        printf("%s", (op->bool_value) ? "true" : "false");
        break;
    case IR_PUSH_INT_CONSTANT:
        if (op->integer.un_signed) {
            printf("%llu", op->integer.unsigned_value);
        } else {
            printf("%lld", op->integer.int_value);
        }
        break;
    case IR_JUMP:
    case IR_JUMP_F:
    case IR_JUMP_T:
    case IR_LABEL:
        printf("lbl_%zu", op->label);
        break;
    case IR_NATIVE_CALL:
        printf(SV_SPEC, SV_ARG(op->native.name));
        break;
    case IR_OPERATOR:
        printf("%s", Operator_name(op->op));
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
    assert(function);
    for (size_t ix = 0; ix < function->num_operations; ++ix) {
        if (function->operations[ix].operation == IR_LABEL && function->operations[ix].label == label) {
            return ix;
        }
    }
    fatal("Label '%d' not found in function '" SV_SPEC "'", label, SV_ARG(function->name));
}

void ir_function_print(IRFunction *function)
{
    printf(SV_SPEC "(", SV_ARG(function->name));
    for (size_t ix = 0; ix < function->num_parameters; ++ix) {
        if (ix > 0)
            printf(", ");
        ir_var_decl_print(function->parameters + ix);
    }
    printf(") -> ");
    typespec_print(stdout, function->type);
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
    for (size_t ix = 0; ix < function->num_operations; ++ix) {
        if (function->operations[ix].operation == IR_LABEL) {
            printf("lbl_%zu:\n", function->operations[ix].label);
        }
        if (ix == mark) {
            ir_operation_print_prefix(function->operations + ix, ">");
        } else {
            ir_operation_print(function->operations + ix);
        }
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

IRAbstractFunction *ir_program_function_by_name(IRProgram *program, StringView name)
{
    for (size_t ix = 0; ix < program->num_functions; ++ix) {
        if (sv_eq(program->functions[ix].name, name)) {
            return (IRAbstractFunction *) program->functions + ix;
        }
    }
    return NULL;
}
