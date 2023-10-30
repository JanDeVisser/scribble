/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <allocate.h>
#include <arm64.h>
#include <hash.h>

DECLARE_SHARED_ALLOCATOR(arm64)

DA_IMPL(ARM64Function)

StringView arm64function_label(ARM64Function *function)
{
    if (sv_empty(function->label)) {
        if (!function->scope.variables.size || sv_eq_cstr(function->function->name, "main")) {
            function->label = function->function->name;
        } else {
            size_t hash = 0u;
            for (size_t ix = 0; ix < function->scope.variables.size; ++ix) {
                hash = (hash << 3) ^ hashlong(function->scope.variables.elements[ix].var_decl.type.type_id);
            }
            function->label = sv_printf("%.*s_%zu", SV_ARG(function->function->name), hash % 4096);
        }
    }
    return function->label;
}

StringView arm64function_to_string(ARM64Function *function)
{
    StringList params = sl_create();
    for (size_t ix = 0; ix < function->scope.variables.size; ++ix) {
        ARM64Variable *param = function->scope.variables.elements + ix;
        sl_push(&params, ir_var_decl_to_string(&param->var_decl));
    }
    return sv_printf("func %.*s(%.*s): %.*s [%ld/%ld]",
        SV_ARG(function->function->name), SV_ARG(sl_join(&params, sv_from(", "))),
        SV_ARG(typespec_to_string(function->function->type)),
        function->nsaa, function->scope.depth);
}

ARM64Variable *arm64function_variable_by_name(ARM64Function *function, StringView name)
{
    assert(function->function->kind == FK_SCRIBBLE);
    assert(function->scribble.current_scope);
    for (ARM64Scope *scope = function->scribble.current_scope; scope; scope = scope->up) {
        for (size_t ix = 0; ix < scope->variables.size; ++ix) {
            ARM64Variable *var = scope->variables.elements + ix;
            if (sv_eq(var->var_decl.name, name)) {
                return var;
            }
        }
    }
    return NULL;
}

void arm64function_add_instruction(ARM64Function *function, char const *opcode, char const *arg_fmt, ...)
{
    va_list args;
    va_start(args, arg_fmt);
    code_vadd_instruction(function->scribble.code, opcode, arg_fmt, args);
    va_end(args);
}

void arm64function_add_text_sv(ARM64Function *function, StringView text)
{
    arm64function_add_text(function, sv_cstr(text));
}

void arm64function_add_text(ARM64Function *function, char const *text, ...)
{
    va_list args;
    va_start(args, text);
    arm64function_vadd_text(function, text, args);
    va_end(args);
}

void arm64function_vadd_text(ARM64Function *function, char const *text, va_list args)
{
    code_vadd_text(function->scribble.code, text, args);
}

void arm64function_add_label(ARM64Function *function, StringView label)
{
    code_add_label(function->scribble.code, label);
}

void arm64function_add_comment(ARM64Function *function, char const *comment, ...)
{
    va_list args;
    va_start(args, comment);
    code_vadd_comment(function->scribble.code, comment, args);
    va_end(args);
}

void arm64function_append_code(ARM64Function *function, Code *code)
{
    code_append_code(function->scribble.code, code);
}

void arm64function_push(ARM64Function *function, Register r)
{
    code_push(function->scribble.code, r);
}

void arm64function_pop(ARM64Function *function, Register r)
{
    code_pop(function->scribble.code, r);
}

void arm64function_write_char(ARM64Function *function, int fd, char ch)
{
    arm64function_add_instruction(function, "mov", "w0,#0x%02x", (uint8_t) ch);
    arm64function_add_instruction(function, "strb", "w0,[sp,-16]!");
    arm64function_add_instruction(function, "mov", "x0,#%d", fd); // x0: fd
    arm64function_add_instruction(function, "mov", "x1,sp");      // x1: SP
    arm64function_add_instruction(function, "mov", "x2,#1");      // x2: Number of characters
    arm64function_syscall(function, SYSCALL_WRITE);
    arm64function_add_instruction(function, "add", "sp,sp,16");
}

void arm64function_syscall(ARM64Function *function, int id)
{
    arm64function_add_instruction(function, "mov", SYSCALL_REG ",#0x%02x", id);
    arm64function_add_instruction(function, "svc", "#0x00");
}

void arm64function_syscall1(ARM64Function *function, int id, uint64_t arg1)
{
    arm64function_add_instruction(function, "mov", "x0,#0x%x", arg1);
    arm64function_syscall(function, id);
}

void arm64function_syscall2(ARM64Function *function, int id, uint64_t arg1, uint64_t arg2)
{
    arm64function_add_instruction(function, "mov", "x0,#0x%x", arg1);
    arm64function_add_instruction(function, "mov", "x1,#0x%x", arg2);
    arm64function_syscall(function, id);
}

void arm64function_syscall3(ARM64Function *function, int id, uint64_t arg1, uint64_t arg2, uint64_t arg3)
{
    arm64function_add_instruction(function, "mov", "x0,#0x%x", arg1);
    arm64function_add_instruction(function, "mov", "x1,#0x%x", arg2);
    arm64function_add_instruction(function, "mov", "x2,#0x%x", arg3);
    arm64function_syscall(function, id);
}

void arm64function_syscall4(ARM64Function *function, int id, uint64_t arg1, uint64_t arg2, uint64_t arg3, uint64_t arg4)
{
    arm64function_add_instruction(function, "mov", "x0,#0x%x", arg1);
    arm64function_add_instruction(function, "mov", "x1,#0x%x", arg2);
    arm64function_add_instruction(function, "mov", "x2,#0x%x", arg3);
    arm64function_add_instruction(function, "mov", "x3,#0x%x", arg4);
    arm64function_syscall(function, id);
}

void arm64function_copy_pointers(ARM64Function *function, Register to_pointer, size_t to_offset, Register from_pointer, size_t from_offset, size_t size)
{
    code_copy_memory(function->scribble.code, to_pointer, to_offset, from_pointer, from_offset, size);
}

void arm64function_copy_to_registers(ARM64Function *function, Register r, Register from_pointer, size_t from_offset, size_t size)
{
    code_copy_to_registers(function->scribble.code, r, from_pointer, from_offset, size);
}

void arm64function_copy_from_registers(ARM64Function *function, Register to_pointer, size_t to_offset, Register r, size_t size)
{
    code_copy_from_registers(function->scribble.code, to_pointer, to_offset, r, size);
}

void arm64function_copy_to_stack(ARM64Function *function, Register r, size_t size)
{
    code_copy_to_stack(function->scribble.code, r, size);
}

void arm64function_copy(ARM64Function *function, ValueLocation to_location, ValueLocation from_location)
{
    code_copy(function->scribble.code, to_location, from_location);
}

extern Register arm64function_allocate_register(ARM64Function *function)
{
    for (Register ix = REG_X9; ix < REG_X16; ++ix) {
        if (!function->scribble.registers[ix]) {
            function->scribble.registers[ix] = true;
            return ix;
        }
    }
    for (Register ix = REG_X19; ix < REG_FP; ++ix) {
        if (!function->scribble.registers[ix]) {
            function->scribble.registers[ix] = true;
            function->scribble.callee_saved[ix - REG_X19] = true;
            return ix;
        }
    }
    fatal("Out of registers");
}

extern RegisterRange arm64function_allocate_register_range(ARM64Function *function, size_t num)
{
    for (Register ix = REG_X9; ix <= REG_X16 - num; ++ix) {
        for (size_t jx = 0; jx < num; ++jx) {
            if (function->scribble.registers[ix + jx]) {
                ix = ix + jx;
                goto next_1;
            }
        }
        for (size_t jx = 0; jx < num; ++jx) {
            function->scribble.registers[ix + jx] = true;
        }
        return (RegisterRange) { .start = ix, .end = ix + num };
    next_1:;
    }
    for (Register ix = REG_X19; ix < REG_FP; ++ix) {
        for (size_t jx = 0; jx < num; ++jx) {
            if (function->scribble.registers[ix + jx]) {
                ix = ix + jx;
                goto next_2;
            }
        }
        for (size_t jx = 0; jx < num; ++jx) {
            function->scribble.registers[ix + jx] = true;
            function->scribble.callee_saved[ix + jx - REG_X19] = true;
        }
        return (RegisterRange) { .start = ix, .end = ix + num };
    next_2:;
    }
    fatal("Out of registers");
}

void arm64function_release_register(ARM64Function *function, Register reg)
{
    if ((reg >= REG_X9 && reg < REG_X16) || (reg >= REG_X19 && reg < REG_FP)) {
        function->scribble.registers[reg] = false;
    }
}

void arm64function_release_all_registers(ARM64Function *function)
{
    for (Register ix = REG_X9; ix < REG_FP; ++ix) {
        function->scribble.registers[ix] = false;
    }
}

OptionalValueLocation arm64function_pop_location(ARM64Function *function)
{
    OptionalValueLocation ret = arm64function_peek_location(function);
    if (ret.has_value) {
        ARM64Scope *scope = &function->scope;
        assert(scope);
        scope->expression_stack = scope->expression_stack->next;
    }
    return ret;
}

OptionalValueLocation arm64function_peek_location(ARM64Function *function)
{
    assert(function);
    assert(function->function->kind == FK_SCRIBBLE);
    ARM64Scope *scope = &function->scope;
    assert(scope);
    if (!scope->expression_stack) {
        return OptionalValueLocation_empty();
    }
    OptionalValueLocation ret = OptionalValueLocation_create(*scope->expression_stack);
    return ret;
}

void arm64function_push_location(ARM64Function *function, ValueLocation entry)
{
    assert(function);
    assert(function->function->kind == FK_SCRIBBLE);
    ARM64Scope *scope = &function->scope;
    assert(scope);
    ValueLocation *new_entry = allocate_new(ValueLocation);
    memcpy(new_entry, &entry, sizeof(ValueLocation));
    new_entry->next = scope->expression_stack;
    scope->expression_stack = new_entry;
}

void arm64function_push_register(ARM64Function *function, type_id type, Register reg)
{
    arm64function_push_location(function, (ValueLocation) { .type = type, .kind = VLK_REGISTER, .reg = reg });
}

void arm64function_push_registers(ARM64Function *function, type_id type, RegisterRange regs)
{
    arm64function_push_location(
        function,
        (ValueLocation) {
            .type = type,
            .kind = VLK_REGISTER_RANGE,
            .range = regs,
        });
}

void arm64function_enter(ARM64Function *function)
{
    function->scribble.current_scope = &function->scope;
    int64_t nsaa = function->nsaa;
    //    arm64context_set_stack_depth(function->, stack_depth);

    // fp, lr, and sp have been set be the calling function

    // Copy parameters from registers to their spot in the stack.
    // @improve Do this lazily, i.e. when we need the registers
    for (size_t ix = 0; ix < function->scope.variables.size; ++ix) {
        ARM64Variable *param = function->scope.variables.elements + ix;
        assert(param->kind == VK_PARAMETER);
        type_id       type = typeid_canonical_type_id(param->var_decl.type.type_id);
        ValueLocation param_location = {
            .type = type,
            .kind = VLK_POINTER,
            .pointer = {
                .reg = REG_FP,
                .offset = function->scope.depth - param->parameter.offset }
        };
        arm64function_add_comment(function, "Unmarshalling parameter %.*s: %.*s -> offset %ld",
            SV_ARG(param->var_decl.name), SV_ARG(typeid_name(param->var_decl.type.type_id)),
            function->scope.depth - param->parameter.offset);
        switch (param->parameter.method) {
        case PPM_REGISTER: {
            ValueLocation arg_location = {
                .type = type,
                .kind = VLK_REGISTER,
                .reg = param->parameter.reg,
            };
            arm64function_copy(function, param_location, arg_location);
        } break;
        case PPM_REGISTER_RANGE: {
            ValueLocation arg_location = {
                .type = type,
                .kind = VLK_REGISTER_RANGE,
                .range.start = param->parameter.range.start,
                .range.end = param->parameter.range.end,
            };
            arm64function_copy(function, param_location, arg_location);
        } break;
        case PPM_POINTER: {
            ValueLocation arg_location = {
                .type = type,
                .kind = VLK_POINTER,
                .pointer = {
                    .reg = param->parameter.reg,
                    .offset = 0,
                }
            };
            arm64function_add_comment(function, "Pointer parameter %.*s: %s -> %ld",
                SV_ARG(param->var_decl.name), reg(param->parameter.reg), param->parameter.offset);
            arm64function_copy(function, param_location, arg_location);
        } break;
        case PPM_STACK: {
            ValueLocation arg_location = {
                .type = type,
                .kind = VLK_STACK,
                .offset = param->parameter.nsaa_offset + function->scribble.stack_depth + 16,
            };
            arm64function_add_comment(function, "Stack parameter %.*s: %ld -> %ld",
                SV_ARG(param->var_decl.name), param->parameter.nsaa_offset, param->parameter.offset);
            arm64function_copy(function, param_location, arg_location);
        } break;
        case PPM_POINTER_STACK: {
            Register      r = arm64function_allocate_register(function);
            ValueLocation arg_location = {
                .type = type,
                .kind = VLK_POINTER,
                .pointer = {
                    .reg = r,
                    .offset = 0,
                }
            };
            arm64function_add_comment(function, "Stacked pointer parameter %.*s: %ld -> %ld",
                SV_ARG(param->var_decl.name), param->parameter.nsaa_offset, param->parameter.offset);
            arm64function_add_instruction(function, "ldr", "%s,[fp,#%d]", x_reg(r), param->parameter.nsaa_offset + function->scribble.stack_depth + 16);
            arm64function_copy(function, param_location, arg_location);
            arm64function_release_register(function, r);
        } break;
        default:
            UNREACHABLE();
        }
    }
}

void arm64function_return(ARM64Function *function)
{
    assert(function);
    code_add_instruction(function->scribble.code, "b", "__%.*s__return", SV_ARG(arm64function_label(function)));
}

void arm64function_leave(ARM64Function *function)
{
    assert(function);

    StringView name = arm64function_label(function);

    if (sv_eq_cstr(function->function->name, "main")) {
        function->assembly->has_main = true;
    }
    code_select_prolog(function->scribble.code);
    code_add_export(function->scribble.code, name);
    code_add_label(function->scribble.code, name);
    code_add_instruction(function->scribble.code, "stp", "fp,lr,[sp,#-16]!");
    if (function->scribble.stack_depth > 0) {
        code_add_instruction(function->scribble.code, "sub", "sp,sp,#%zu", function->scribble.stack_depth);
    }
    code_add_instruction(function->scribble.code, "mov", "fp,sp");
    Register regs[2];
    int      r = 0;
    for (int ix = 0; ix < (int) REG_FP - (int) REG_X19; ++ix) {
        if (function->scribble.code->function->scribble.callee_saved[ix]) {
            regs[r] = REG_X19 + ix;
            r = 1 - r;
            if (r == 0) {
                code_add_instruction(function->scribble.code, "stp", "%s,%s,[sp,#-16]!", x_reg(regs[0]), x_reg(regs[1]));
            }
        }
    }
    if (r == 1) {
        code_add_instruction(function->scribble.code, "str", "%s,[sp,#-16]!", x_reg(regs[0]));
    }

    code_select_epilog(function->scribble.code);
    code_add_label(function->scribble.code, sv_printf("__%.*s__return", SV_ARG(name)));
    r = 0;
    for (int ix = (int) REG_FP - (int) REG_X19 - 1; ix >= 0; --ix) {
        if (function->scribble.code->function->scribble.callee_saved[ix]) {
            regs[r] = REG_X19 + ix;
            r = 1 - r;
            if (r == 0) {
                code_add_instruction(function->scribble.code, "ldp", "%s,%s,[sp,#-16]!", x_reg(regs[1]), x_reg(regs[0]));
            }
        }
    }
    if (r == 1) {
        code_add_instruction(function->scribble.code, "ldr", "%s,[sp,#-16]!", x_reg(regs[0]));
    }
    code_add_instruction(function->scribble.code, "mov", "sp,fp");
    if (function->scribble.stack_depth > 0) {
        code_add_instruction(function->scribble.code, "add", "sp,sp,#%zu", function->scribble.stack_depth);
    }
    code_add_instruction(function->scribble.code, "ldp", "fp,lr,[sp],16");
    code_add_instruction(function->scribble.code, "ret", "");
    code_select_code(function->scribble.code);
    // arm64context_pop_stack_depth(ctx);
}

void arm64function_marshall_arguments(ARM64Function *calling_function, ARM64Function *called_function)
{
    if (called_function->nsaa > 0) {
        arm64function_add_instruction(calling_function, "sub", "sp,sp,#%d", called_function->nsaa);
    }
    Code *marshalling = code_create(calling_function);
    for (size_t ix = 0; ix < called_function->scope.variables.size; ++ix) {
        ARM64Variable *param = called_function->scope.variables.elements + ix;
        assert(param->kind == VK_PARAMETER);
        type_id type = typeid_canonical_type_id(param->var_decl.type.type_id);
        MUST_OPTIONAL(ValueLocation, arg_location, arm64function_pop_location(calling_function))
        code_add_comment(marshalling, "Marshalling argument %.*s: %.*s from %.*s",
            SV_ARG(param->var_decl.name), SV_ARG(typeid_name(param->var_decl.type.type_id)),
            SV_ARG(value_location_to_string(arg_location)));
        switch (param->parameter.method) {
        case PPM_REGISTER: {
            ValueLocation param_location = {
                .type = type,
                .kind = VLK_REGISTER,
                .reg = param->parameter.reg,
            };
            code_copy(marshalling, param_location, arg_location);
        } break;
        case PPM_REGISTER_RANGE: {
            ValueLocation param_location = {
                .type = type,
                .kind = VLK_REGISTER_RANGE,
                .range.start = param->parameter.range.start,
                .range.end = param->parameter.range.end,
            };
            code_copy(marshalling, param_location, arg_location);
        } break;
        case PPM_STACK: {
            ValueLocation param_location = {
                .type = type,
                .kind = VLK_STACK,
                .offset = param->parameter.nsaa_offset,
            };
            code_copy(marshalling, param_location, arg_location);
        } break;
        case PPM_POINTER: {
            switch (arg_location.kind) {
            case VLK_POINTER:
                code_add_instruction(marshalling, "add", "%s,%s,%#ld",
                    x_reg(param->parameter.reg), x_reg(arg_location.pointer.reg), arg_location.pointer.offset);
                break;
            case VLK_STACK:
                code_add_instruction(marshalling, "add", "%s,sp,%#ld",
                    x_reg(param->parameter.reg), arg_location.offset);
                break;
            case VLK_LABEL:
                code_add_instruction(marshalling, "adrp", "%s,%.*s@PAGE", x_reg(param->parameter.reg), SV_ARG(arg_location.symbol));
                code_add_instruction(marshalling, "add", "%s,%s,%.*s@PAGEOFF", x_reg(param->parameter.reg), x_reg(param->parameter.reg), SV_ARG(arg_location.symbol));
                break;
            default:
                UNREACHABLE();
            }
        } break;
        case PPM_POINTER_STACK: {
            Register r = arm64function_allocate_register(calling_function);
            switch (arg_location.kind) {
            case VLK_POINTER:
                code_add_instruction(marshalling, "add", "%s,%s,%#ld",
                    x_reg(r), x_reg(arg_location.pointer.reg), arg_location.pointer.offset);
                break;
            case VLK_STACK:
                code_add_instruction(marshalling, "add", "%s,%s,%#ld",
                    x_reg(param->parameter.reg), reg(REG_SP), arg_location.offset);
                break;
            case VLK_LABEL:
                code_add_instruction(marshalling, "adrp", "%s,%.*s@PAGE", x_reg(r), SV_ARG(arg_location.symbol));
                code_add_instruction(marshalling, "add", "%s,%s,%.*s@PAGEOFF", x_reg(r), reg(param->parameter.reg), SV_ARG(arg_location.symbol));
                break;
            default:
                UNREACHABLE();
            }
            code_add_instruction(marshalling, "str", "%s,[sp,#%d]", x_reg(r), param->parameter.nsaa_offset);
            arm64function_release_register(calling_function, r);
        } break;
        }
        switch (arg_location.kind) {
        case VLK_REGISTER:
            arm64function_release_register(calling_function, arg_location.reg);
            break;
        case VLK_REGISTER_RANGE: {
            for (Register r = arg_location.range.start; r < arg_location.range.end; ++r) {
                arm64function_release_register(calling_function, r);
            }
        } break;
        default:
            break;
        }
    }
    code_select_prolog(marshalling);
    for (Register r = REG_X9; r <= REG_X16; ++r) {
        if (calling_function->scribble.registers[(int) r]) {
            code_push(marshalling, r);
        }
    }
    arm64function_append_code(calling_function, marshalling);
}

void arm64function_marshall_return(ARM64Function *calling_function, ARM64Function *called_function, bool discard_result)
{
    for (Register r = REG_X15; r > REG_X8; --r) {
        if (calling_function->scribble.registers[(int) r]) {
            arm64function_pop(calling_function, r);
        }
    }
    if (discard_result) {
        return;
    }
    ValueLocation x0 = {
        .type = called_function->function->type.type_id,
        .kind = VLK_REGISTER,
        .reg = REG_X0,
    };
    Register      r = arm64function_allocate_register(calling_function);
    ValueLocation target = {
        .type = called_function->function->type.type_id,
        .kind = VLK_REGISTER,
        .reg = r,
    };
    arm64function_copy(calling_function, target, x0);
    arm64function_push_location(calling_function, target);
}

ValueLocation arm64function_location_for_type(ARM64Function *function, type_id type)
{
    ValueLocation ret = { 0 };
    switch (typeid_kind(type)) {
    case TK_PRIMITIVE: {
        return (ValueLocation) {
            .type = type,
            .kind = VLK_REGISTER,
            .reg = arm64function_allocate_register(function),
        };
    } break;
    case TK_AGGREGATE: {
        size_t size_in_double_words = align_at(typeid_sizeof(type), 8) / 8;
        if (size_in_double_words <= 2) {
            return (ValueLocation) {
                .type = type,
                .kind = VLK_REGISTER_RANGE,
                .range = arm64function_allocate_register_range(function, size_in_double_words),
            };
            break;
        }
        return (ValueLocation) {
            .type = type,
            .kind = VLK_STACK,
        };
    } break;
    default:
        NYI("load_variable for non-primitive, non-aggregate type");
        break;
    }
}
