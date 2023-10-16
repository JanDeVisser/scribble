/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <allocate.h>
#include <options.h>

#include <arm64.h>

DECLARE_SHARED_ALLOCATOR(arm64)

Code *code_acreate(Assembly *assembly)
{
    Code *ret = allocator_alloc_new(assembly->allocator, Code);
    ret->assembly = assembly;
    ret->prolog = sb_acreate(assembly->allocator);
    ret->code = sb_acreate(assembly->allocator);
    ret->epilog = sb_acreate(assembly->allocator);
    code_select_code(ret);
    return ret;
}

void code_add_instruction(Code *code, char const *opcode, char const *arg_fmt, ...)
{
    va_list args;
    va_start(args, arg_fmt);
    code_vadd_instruction(code, opcode, arg_fmt, args);
    va_end(args);
}

void code_vadd_instruction(Code *code, char const *opcode, char const *arg_fmt, va_list args)
{
    StringView opcode_sv = sv_aprintf(code->assembly->allocator, "\t%s\t%s", opcode, arg_fmt);
    assert(sv_is_cstr(opcode_sv));
    code_vadd_text(code, opcode_sv.ptr, args);
}

void code_add_text(Code *code, char const *text, ...)
{
    va_list args;
    va_start(args, text);
    code_vadd_text(code, text, args);
    va_end(args);
}

void code_vadd_text(Code *code, char const *text, va_list args)
{
    StringView txt = sv_from(text);
    if (strchr(text, '%')) {
        txt = sv_avprintf(code->assembly->allocator, text, args);
    }
    txt = sv_strip(txt);
    if (sv_empty(txt)) {
        return;
    }
    StringList lines = sv_asplit(code->assembly->allocator, txt, sv_from("\n"));
    for (size_t ix = 0; ix < lines.size; ++ix) {
        StringView line = lines.strings[ix];
        if (sv_empty(line)) {
            sb_append_cstr(code->active, "\n");
            continue;
        }
        if (line.ptr[0] == ';') {
            sb_printf(code->active, "\t%.*s\n", SV_ARG(line));
            continue;
        }
        if ((line.ptr[0] == '.') || (line.ptr[line.length - 1] == ':')) {
            sb_printf(code->active, "%.*s\n", SV_ARG(line));
            continue;
        }
        StringList parts = sv_asplit_by_whitespace(code->assembly->allocator, line);
        if (parts.size > 0) {
            StringView l = sl_join(&parts, sv_from("\t"));
            sb_printf(code->active, "\t%.*s\n", SV_ARG(l));
        }
    }
}

void code_add_label(Code *code, StringView label)
{
    sb_printf(code->active, "%.*s:\n", SV_ARG(label));
}

void code_add_directive(Code *code, StringView directive, StringView args)
{
    if (sv_eq_cstr(directive, ".global")) {
        code->assembly->has_exports = true;
    }
    sb_printf(code->active, "%.*s\t%.*s\n", SV_ARG(directive), SV_ARG(args));
}

void code_add_comment(Code *code, char const *text, ...)
{
    va_list args;
    va_start(args, text);
    code_vadd_comment(code, text, args);
    va_end(args);
}

void code_vadd_comment(Code *code, char const *fmt, va_list args)
{
    StringView comment = sv_avprintf(code->assembly->allocator, fmt, args);
    StringList sl = sv_asplit(code->assembly->allocator, comment, sv_from("\n"));
    for (size_t ix = 0; ix < sl.size; ++ix) {
        sb_printf(code->active, "\t// %.*s\n", SV_ARG(sl.strings[ix]));
    }
}

void code_append_code(Code *code, Code *append)
{
    sb_append_sv(code->active, code_to_string(append));
}

StringView code_to_string(Code *code)
{
    StringBuilder ret = sb_acreate(code->assembly->allocator);
    if (!sv_empty(code->prolog.view)) {
        sb_append_sv(&ret, code->prolog.view);
        sb_append_cstr(&ret, "\n");
    }
    sb_append_sv(&ret, code->code.view);
    if (!sv_empty(code->epilog.view)) {
        sb_append_sv(&ret, code->epilog.view);
        sb_append_cstr(&ret, "\n");
    }
    return ret.view;
}

bool code_empty(Code *code)
{
    return sv_empty(code->code.view);
}

bool code_has_text(Code *code)
{
    return !code_empty(code);
}

void code_close_function(Code *code, StringView name, size_t stack_depth)
{
    code_select_prolog(code);
    code_add_directive(code, sv_from(".global"), name);
    code_add_label(code, name);
    code_add_instruction(code, "stp", "fp,lr,[sp,#-16]!");
    if (stack_depth > 0) {
        code_add_instruction(code, "sub", "sp,sp,#%zu", stack_depth);
    }
    code_add_instruction(code, "mov", "fp,sp");
    Register regs[2];
    int      r = 0;
    for (int ix = 0; ix < (int) REG_FP - (int) REG_X19; ++ix) {
        if (code->assembly->callee_saved[ix]) {
            regs[r] = REG_X19 + ix;
            r = 1 - r;
            if (r == 0) {
                code_add_instruction(code, "stp", "%s,%s,[sp,#-16]!", x_reg(regs[0]), x_reg(regs[1]));
            }
        }
    }
    if (r == 1) {
        code_add_instruction(code, "str", "%s,[sp,#-16]!", x_reg(regs[0]));
    }

    code_select_epilog(code);
    code_add_label(code, sv_aprintf(code->assembly->allocator, "__%.*s__return", SV_ARG(name)));
    r = 0;
    for (int ix = (int) REG_FP - (int) REG_X19 - 1; ix >= 0; --ix) {
        if (code->assembly->callee_saved[ix]) {
            regs[r] = REG_X19 + ix;
            r = 1 - r;
            if (r == 0) {
                code_add_instruction(code, "ldp", "%s,%s,[sp,#-16]!", x_reg(regs[1]), x_reg(regs[0]));
            }
        }
    }
    if (r == 1) {
        code_add_instruction(code, "ldr", "%s,[sp,#-16]!", x_reg(regs[0]));
    }
    code_add_instruction(code, "mov", "sp,fp");
    if (stack_depth > 0) {
        code_add_instruction(code, "add", "sp,sp,#%zu", stack_depth);
    }
    code_add_instruction(code, "ldp", "fp,lr,[sp],16");
    code_add_instruction(code, "ret", "");
    code_select_code(code);
}

void code_select_prolog(Code *code)
{
    code->active = &code->prolog;
}

void code_select_epilog(Code *code)
{
    code->active = &code->epilog;
}

void code_select_code(Code *code)
{
    code->active = &code->code;
}

void code_copy_pointers(Code *code, Register to_pointer, size_t to_offset, Register from_pointer, size_t from_offset, size_t size)
{
    Register t1 = assembly_allocate_register(code->assembly);
    Register t2 = assembly_allocate_register(code->assembly);
    size_t   remaining = size;
    while (remaining >= 16) {
        code_add_instruction(code, "ldp", "%s,%s,[%s,#%zu]",
            x_reg(t1), x_reg(t2), x_reg(from_pointer), from_offset + size - remaining);
        code_add_instruction(code, "stp", "%s,%s,[%s,#%zu]",
            x_reg(t1), x_reg(t2), x_reg(to_pointer), to_offset + size - remaining);
        remaining -= 16;
    }
    if (remaining >= 8) {
        code_add_instruction(code, "ldr", "%s,[%s,#%zu]",
            x_reg(t1), x_reg(from_pointer), from_offset + size - remaining);
        code_add_instruction(code, "str", "%s,[%s,#%zu]",
            x_reg(t1), x_reg(to_pointer), to_offset + size - remaining);
        remaining -= 8;
    }
    if (remaining >= 4) {
        code_add_instruction(code, "ldr", "%s,[%s,#%zu]",
            w_reg(t1 + 32), x_reg(from_pointer), from_offset + size - remaining);
        code_add_instruction(code, "str", "%s,[%s,#%zu]",
            w_reg(t1 + 32), x_reg(to_pointer), to_offset + size - remaining);
        remaining -= 4;
    }
    if (remaining >= 2) {
        code_add_instruction(code, "ldrh", "%s,[%s,#%zu]",
            w_reg(t1), x_reg(from_pointer), from_offset + size - remaining);
        code_add_instruction(code, "strh", "%s,[%s,#%zu]",
            w_reg(t1), x_reg(to_pointer), to_offset + size - remaining);
        remaining -= 2;
    }
    if (remaining == 1) {
        code_add_instruction(code, "ldrb", "%s,[%s,#%zu]",
            w_reg(t1), x_reg(from_pointer), from_offset + size - remaining);
        code_add_instruction(code, "strb", "%s,[%s,#%zu]",
            w_reg(t1), x_reg(to_pointer), to_offset + size - remaining);
        remaining -= 1;
    }
    assert(!remaining);
    assembly_release_register(code->assembly, t1);
    assembly_release_register(code->assembly, t2);
}

void code_copy_to_registers(Code *code, Register r, Register from_pointer, size_t from_offset, size_t size)
{
    assert(size % 8 == 0);
    size_t quad_words = size / 16;
    for (size_t ix = 0; ix < quad_words; ++ix) {
        Register next_reg = r + 1;
        code_add_instruction(code, "ldp", "%s,%s,[%s,#%zu]",
            x_reg(r), x_reg(next_reg), x_reg(from_pointer), from_offset + 16 * ix);
        r = next_reg + 1;
    }
    if (quad_words % 2) {
        code_add_instruction(code, "ldr", "%s,[%s,#%zu]",
            x_reg(r), x_reg(from_pointer), from_offset + 16 * quad_words);
    }
}

void code_copy_from_registers(Code *code, Register to_pointer, size_t to_offset, Register r, size_t size)
{
    assert(size % 8 == 0);
    size_t quad_words = size / 16;
    for (size_t ix = 0; ix < quad_words; ++ix) {
        Register next_reg = r + 1;
        code_add_instruction(code, "stp", "%s,%s,[%s,#%zu]",
            x_reg(r), x_reg(next_reg), x_reg(to_pointer), to_offset + 16 * ix);
        r = next_reg + 1;
    }
    if (quad_words % 2) {
        code_add_instruction(code, "ldr", "%s,[%s,#%zu]",
            x_reg(r), x_reg(to_pointer), to_offset + 16 * quad_words);
    }
}

void code_copy_to_stack(Code *code, Register r, size_t size)
{
    assert(size % 8 == 0);
    size_t quad_words = size / 16;
    for (size_t ix = 0; ix < quad_words; ++ix) {
        Register next_reg = r + 1;
        code_add_instruction(code, "stp", "%s,%s,[sp,#%zu]",
            x_reg(r), x_reg(next_reg), 16 * ix);
        r = next_reg + 1;
    }
    if (quad_words % 2) {
        code_add_instruction(code, "str", "%s,[sp,#%zu]",
            x_reg(r), 16 * quad_words);
    }
}

void code_copy(Code *code, ValueLocation to_location, ValueLocation from_location)
{
    assert(to_location.type == from_location.type);
    size_t    sz = align_at(typeid_sizeof(from_location.type), 8);
    OpcodeMap opcode_map = get_opcode_map(from_location.type);
    code_add_comment(code, "copy %.*s to %.*s",
        SV_ARG(value_location_to_string(from_location, code->assembly->allocator)),
        SV_ARG(value_location_to_string(to_location, code->assembly->allocator)));
    switch (to_location.kind) {
    case VLK_POINTER: {
        switch (from_location.kind) {
        case VLK_POINTER: {
            code_copy_pointers(code, to_location.pointer.reg, to_location.pointer.offset, from_location.pointer.reg, from_location.pointer.offset, sz);
        } break;
        case VLK_REGISTER: {
            assert(sz == 8);
            code_add_instruction(code, opcode_map.store_opcode, "%s,[%s,#%zu]",
                reg_with_width(from_location.reg, opcode_map.reg_width),
                x_reg(to_location.pointer.reg), to_location.pointer.offset);
        } break;
        case VLK_REGISTER_RANGE: {
            assert(8 * ((int) from_location.range.end - (int) from_location.range.start) == sz);
            code_copy_from_registers(code, to_location.pointer.reg, to_location.pointer.offset, from_location.range.start, sz);
        } break;
        case VLK_STACK: {
            code_copy_pointers(code, to_location.pointer.reg, to_location.pointer.offset, REG_SP, from_location.offset, sz);
        } break;
        case VLK_SYMBOL: {
            Register pointer = assembly_allocate_register(code->assembly);
            code_add_instruction(code, "adrp", "%s,%.*s@PAGE", x_reg(pointer), SV_ARG(from_location.symbol));
            code_add_instruction(code, "add", "%s,%s,%.*s@PAGEOFF",
                x_reg(pointer), x_reg(pointer), SV_ARG(from_location.symbol));
            code_copy_pointers(code, to_location.pointer.reg, to_location.pointer.offset, pointer, 0, sz);
            assembly_release_register(code->assembly, pointer);
        } break;
        default:
            UNREACHABLE();
        }
    } break;
    case VLK_REGISTER: {
        assert(sz == 8);
        switch (from_location.kind) {
        case VLK_POINTER: {
            code_add_instruction(code, opcode_map.load_opcode, "%s,[%s,#%ld]",
                reg_with_width(to_location.reg, opcode_map.reg_width),
                reg(from_location.pointer.reg), from_location.pointer.offset);
        } break;
        case VLK_REGISTER: {
            code_add_instruction(code, "mov", "%s,%s",
                reg_with_width(to_location.reg, opcode_map.reg_width),
                reg_with_width(from_location.reg, opcode_map.reg_width));
        } break;
        case VLK_REGISTER_RANGE: {
            assert((int) from_location.range.end - (int) from_location.range.start == 1);
            code_add_instruction(code, "mov", "%s,%s",
                reg_with_width(to_location.reg, opcode_map.reg_width),
                reg_with_width(from_location.range.start, opcode_map.reg_width));
        } break;
        case VLK_STACK: {
            code_add_instruction(code, opcode_map.load_opcode, "%s,[sp,#%ld]",
                reg_with_width(to_location.reg, opcode_map.reg_width),
                reg(from_location.pointer.reg), from_location.offset);
        } break;
        case VLK_SYMBOL: {
            Register pointer = assembly_allocate_register(code->assembly);
            code_add_instruction(code, "adrp", "%s,%.*s@PAGE", x_reg(pointer), SV_ARG(from_location.symbol));
            code_add_instruction(code, opcode_map.load_opcode, "%s,[%s,%.*s@PAGEOFF]",
                reg_with_width(to_location.reg, opcode_map.reg_width),
                x_reg(pointer), SV_ARG(from_location.symbol));
            assembly_release_register(code->assembly, pointer);
        } break;
        default:
            UNREACHABLE();
        }
    } break;
    case VLK_REGISTER_RANGE: {
        assert(sz == 8);
        switch (from_location.kind) {
        case VLK_POINTER: {
            code_copy_to_registers(code, to_location.range.start, from_location.pointer.reg, from_location.pointer.offset, sz);
        } break;
        case VLK_REGISTER: {
            assert((int) to_location.range.end - (int) to_location.range.start == 1);
            code_add_instruction(code, "mov", "%s,%s",
                reg_with_width(to_location.range.start, opcode_map.reg_width),
                reg_with_width(from_location.reg, opcode_map.reg_width));
        } break;
        case VLK_REGISTER_RANGE: {
            assert((int) from_location.range.end - (int) from_location.range.start == (int) to_location.range.end - (int) to_location.range.start);
            for (size_t ix = 0; ix < (int) from_location.range.end - (int) from_location.range.start; ++ix) {
                code_add_instruction(code, "mov", "%s,%s",
                    reg_with_width(to_location.range.start + ix, opcode_map.reg_width),
                    reg_with_width(from_location.range.start + ix, opcode_map.reg_width));
            }
        } break;
        case VLK_STACK: {
            code_copy_to_registers(code, to_location.range.start, REG_SP, from_location.offset, sz);
        } break;
        case VLK_SYMBOL: {
            Register pointer = assembly_allocate_register(code->assembly);
            code_add_instruction(code, "adrp", "%s,%.*s@PAGE", x_reg(pointer), SV_ARG(from_location.symbol));
            code_add_instruction(code, "add", "%s,%s,%.*s@PAGEOFF",
                x_reg(pointer), x_reg(pointer), SV_ARG(from_location.symbol));
            code_copy_to_registers(code, to_location.range.start, pointer, 0, sz);
            assembly_release_register(code->assembly, pointer);
        } break;
        default:
            UNREACHABLE();
        }
    } break;
    case VLK_STACK: {
        switch (from_location.kind) {
        case VLK_POINTER: {
            code_copy_pointers(code, REG_SP, to_location.offset, from_location.pointer.reg, from_location.pointer.offset, sz);
        } break;
        case VLK_REGISTER: {
            assert(sz == 8);
            code_add_instruction(code, opcode_map.store_opcode, "%s,[%s,#%ld]",
                reg_with_width(from_location.reg, opcode_map.reg_width),
                reg(REG_SP), to_location.offset);
        } break;
        case VLK_REGISTER_RANGE: {
            assert(8 * ((int) from_location.range.end - (int) from_location.range.start) == sz);
            code_copy_from_registers(code, REG_SP, to_location.offset, from_location.range.start, sz);
        } break;
        case VLK_STACK: {
            code_copy_pointers(code, REG_SP, to_location.offset, REG_SP, from_location.offset, sz);
        } break;
        case VLK_SYMBOL: {
            Register pointer = assembly_allocate_register(code->assembly);
            code_add_instruction(code, "adrp", "%s,%.*s@PAGE", x_reg(pointer), SV_ARG(from_location.symbol));
            code_add_instruction(code, "add", "%s,%s,%.*s@PAGEOFF",
                x_reg(pointer), x_reg(pointer), SV_ARG(from_location.symbol));
            code_copy_pointers(code, REG_SP, to_location.offset, pointer, 0, sz);
            assembly_release_register(code->assembly, pointer);
        } break;
        default:
            UNREACHABLE();
        }
    } break;
    case VLK_SYMBOL: {
        Register to_pointer = assembly_allocate_register(code->assembly);
        code_add_instruction(code, "adrp", "%s,%.*s@PAGE", x_reg(to_pointer), SV_ARG(to_location.symbol));
        code_add_instruction(code, "add", "%s,%s,%.*s@PAGEOFF",
            x_reg(to_pointer), x_reg(to_pointer), SV_ARG(to_location.symbol));
        switch (from_location.kind) {
        case VLK_POINTER: {
            code_copy_pointers(code, to_pointer, 0, from_location.pointer.reg, from_location.pointer.offset, sz);
        } break;
        case VLK_REGISTER: {
            assert(sz == 8);
            code_add_instruction(code, opcode_map.store_opcode, "%s,[%s]",
                reg_with_width(from_location.reg, opcode_map.reg_width), to_pointer);
        } break;
        case VLK_REGISTER_RANGE: {
            assert(8 * ((int) from_location.range.end - (int) from_location.range.start) == sz);
            code_copy_from_registers(code, to_pointer, 0, from_location.range.start, sz);
        } break;
        case VLK_STACK: {
            code_copy_pointers(code, to_pointer, 0, REG_SP, from_location.offset, sz);
        } break;
        case VLK_SYMBOL: {
            Register from_pointer = assembly_allocate_register(code->assembly);
            code_add_instruction(code, "adrp", "%s,%.*s@PAGE", x_reg(from_pointer), SV_ARG(from_location.symbol));
            code_add_instruction(code, "add", "%s,%s,%.*s@PAGEOFF",
                x_reg(from_pointer), x_reg(from_pointer), SV_ARG(from_location.symbol));
            code_copy_pointers(code, to_pointer, 0, from_pointer, 0, sz);
            assembly_release_register(code->assembly, from_pointer);
        } break;
        default:
            UNREACHABLE();
        }
        assembly_release_register(code->assembly, to_pointer);
    } break;
    default:
        UNREACHABLE();
    }
}

void code_push(Code *code, Register r)
{
    code_add_instruction(code, "str", "%s,[sp,-16]!", reg(r));
}

void code_pop(Code *code, Register r)
{
    code_add_instruction(code, "ldr", "%s,[sp],16", reg(r));
}
