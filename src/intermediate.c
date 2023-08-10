/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <intermediate.h>

#define STATIC_ALLOCATOR
#include <allocate.h>

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

void generate_node(BoundNode *node, void *target)
{
    IRFunction *fnc = (IRFunction *) target;
    switch (node->type) {
    case BNT_ASSIGNMENT: {
        generate_node(node->assignment.expression, fnc);
        IROperation op;
        op.operation = IR_POP_VAR;
        op.sv = node->name;
        ir_function_add_operation(fnc, op);
    } break;
    case BNT_FUNCTION: {
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
        fnc = (IRFunction *) &program->functions[program->num_functions++];
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
        for (BoundNode *stmt = node->function.statements; stmt; stmt = stmt->next) {
            generate_node(stmt, fnc);
        }
    } break;
    case BNT_INTRINSIC: {
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
    } break;
    case BNT_PROGRAM: {
        for (BoundNode *intrinsic = node->program.intrinsics; intrinsic; intrinsic = intrinsic->next) {
            generate_node(intrinsic, target);
        }
        for (BoundNode *module = node->program.modules; module; module = module->next) {
            for (BoundNode *stmt = module->block.statements; stmt; stmt = stmt->next) {
                generate_node(stmt, target);
            }
        }
    } break;
    case BNT_VARIABLE_DECL: {
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
    } break;
    case BNT_VARIABLE: {
        IROperation op;
        op.operation = IR_PUSH_VAR;
        op.sv = node->name;
        ir_function_add_operation(fnc, op);
    } break;
    case BNT_NUMBER: {
        IROperation op;
        op.operation = IR_PUSH_INT_CONSTANT;
        op.int_value = (int) strtol(node->name.ptr, NULL, 10);
        ir_function_add_operation(fnc, op);
    } break;
    case BNT_STRING: {
        IROperation op;
        op.operation = IR_PUSH_STRING_CONSTANT;
        op.sv = node->name;
        ir_function_add_operation(fnc, op);
    } break;
    case BNT_BINARYEXPRESSION: {
        IROperation op;
        generate_node(node->binary_expr.lhs, fnc);
        generate_node(node->binary_expr.rhs, fnc);
        op.operation = IR_OPERATOR;
        op.op = node->binary_expr.operator;
        ir_function_add_operation(fnc, op);
    } break;
    case BNT_FUNCTION_CALL: {
        size_t arg_count = 0;

        struct arg_list {
            BoundNode       *arg;
            struct arg_list *prev;
            struct arg_list *next;
        };

        struct arg_list *last_arg = NULL;
        for (BoundNode *arg = node->call.argument; arg; arg = arg->next) {
            struct arg_list *current = allocate(sizeof(struct arg_list));
            current->arg = arg;
            current->prev = last_arg;
            if (last_arg)
                last_arg->next = current;
            last_arg = current;
        }
        for (struct arg_list *arg = last_arg; arg; arg = arg->prev) {
            generate_node(arg->arg, fnc);
        }
        IROperation op;
        op.operation = IR_CALL;
        op.sv = node->name;
        ir_function_add_operation(fnc, op);
    } break;
    case BNT_RETURN: {
        if (node->return_stmt.expression) {
            generate_node(node->return_stmt.expression, fnc);
        }
        IROperation op;
        op.operation = IR_RETURN;
        op.bool_value = node->return_stmt.expression != NULL;
        ir_function_add_operation(fnc, op);
    } break;
    case BNT_IF: {
        generate_node(node->if_statement.condition, fnc);
        IROperation  op;
        unsigned int else_label = next_label();
        op.operation = IR_JUMP_F;
        op.unsigned_value = else_label;
        ir_function_add_operation(fnc, op);
        generate_node(node->if_statement.if_true, fnc);
        if (node->if_statement.if_false) {
            unsigned int end_label = next_label();
            op.operation = IR_JUMP;
            op.unsigned_value = end_label;
            ir_function_add_operation(fnc, op);
            op.operation = IR_LABEL;
            op.unsigned_value = else_label;
            ir_function_add_operation(fnc, op);
            generate_node(node->if_statement.if_false, fnc);
            op.operation = IR_LABEL;
            op.unsigned_value = end_label;
            ir_function_add_operation(fnc, op);
        } else {
            op.operation = IR_LABEL;
            op.unsigned_value = else_label;
            ir_function_add_operation(fnc, op);
        }
    } break;
    case BNT_BLOCK: {
        IROperation op;
        op.operation = IR_SCOPE_BEGIN;
        ir_function_add_operation(fnc, op);
        for (BoundNode *stmt = node->block.statements; stmt; stmt = stmt->next) {
            generate_node(stmt, fnc);
        }
        op.operation = IR_SCOPE_END;
        ir_function_add_operation(fnc, op);
    } break;
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
    case IR_PUSH_INT_CONSTANT:
        printf("%d", op->int_value);
        break;
    case IR_JUMP:
    case IR_JUMP_F:
    case IR_JUMP_T:
    case IR_LABEL:
        printf("lbl_%u", op->unsigned_value);
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
        if (function->operations[ix].operation == IR_LABEL && function->operations[ix].unsigned_value == label) {
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
            printf("lbl_%u:\n", function->operations[ix].unsigned_value);
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
