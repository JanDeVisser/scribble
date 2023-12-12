/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <allocate.h>
#include <options.h>

#include <arm64.h>

DECLARE_SHARED_ALLOCATOR(arm64)

Code *code_create(ARM64Function *function)
{
    Code *ret = allocate_new(Code);
    ret->function = function;
    ret->prolog = sb_create();
    ret->code = sb_create();
    ret->epilog = sb_create();
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
    StringView opcode_sv = sv_printf("\t%s\t%s", opcode, arg_fmt);
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
        txt = sv_vprintf(text, args);
    }
    txt = sv_strip(txt);
    if (sv_empty(txt)) {
        return;
    }
    StringList lines = sv_split(txt, sv_from("\n"));
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
        StringList parts = sv_split_by_whitespace(line);
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

void code_add_directive(Code *code, char const *directive, char const *args)
{
    assert(directive && *directive);
    if (*directive == '.') {
        ++directive;
        assert(*directive);
    }
    sb_printf(code->active, ".%s\t%s\n", directive, args);
}

void code_add_export(Code *code, StringView function_name)
{
    if (code->function) {
        code->function->assembly->has_exports = true;
    }
    code_add_directive(code, ".global", sv_cstr(function_name));
}

void code_add_import(Code *code, StringView function_name)
{
    code_add_directive(code, ".global", sv_cstr(function_name));
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
    StringView comment = sv_vprintf(fmt, args);
    StringList sl = sv_split(comment, sv_from("\n"));
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
    StringBuilder ret = sb_create();
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

void code_load_label(Code *code, Register reg, StaticData label)
{
    code_add_instruction(code, "adrp", "%s,%.*s@PAGE", x_reg(reg), SV_ARG(label.symbol));
    code_add_instruction(code, "add", "%s,%s,%.*s@PAGEOFF",
        x_reg(reg), x_reg(reg), SV_ARG(label.symbol));
    if (label.offset != 0) {
        code_add_instruction(code, "add", "%s,%s,#0x%0x",
            x_reg(reg), x_reg(reg), label.offset);
    }
}

void code_copy_pointer(Code *code, Register reg, Register from, int64_t offset)
{
    code_add_instruction(code, "add", "%s,%s,#%ld", x_reg(reg), x_reg(from), offset);
}

void code_copy_memory(Code *code, Register to_pointer, size_t to_offset, Register from_pointer, size_t from_offset, size_t size)
{
    Register t1 = arm64function_allocate_register(code->function);
    Register t2 = arm64function_allocate_register(code->function);
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
    arm64function_release_register(code->function, t1);
    arm64function_release_register(code->function, t2);
}

void code_copy_to_registers(Code *code, Register r, Register from_pointer, size_t from_offset, size_t size)
{
    size_t   half_words = size / 2;
    size_t   words = size / 4;
    size_t   double_words = size / 8;
    size_t   quad_words = double_words / 2;
    Register next_reg = r + 1;
    for (size_t ix = 0; ix < quad_words; ++ix) {
        code_add_instruction(code, "ldp", "%s,%s,[%s,#%zu]",
            x_reg(r), x_reg(next_reg), x_reg(from_pointer), from_offset + 16 * ix);
        r = next_reg + 1;
    }
    if (double_words % 2) {
        code_add_instruction(code, "ldr", "%s,[%s,#%zu]",
            x_reg(r), x_reg(from_pointer), from_offset + 16 * quad_words);
        r = next_reg + 1;
    }
    if (words % 2) {
        code_add_instruction(code, "ldr", "%s,[%s,#%zu]",
            w_reg(r), x_reg(from_pointer), from_offset + 8 * double_words);
        r = next_reg + 1;
    }
    if (half_words % 2) {
        code_add_instruction(code, "strh", "%s,[%s,#%zu]",
            w_reg(r), x_reg(from_pointer), from_offset + 4 * words);
        r = next_reg + 1;
    }
    if (size % 1) {
        code_add_instruction(code, "strh", "%s,[%s,#%zu]",
            w_reg(r), x_reg(from_pointer), from_offset + 2 * half_words);
    }
}

void code_copy_from_registers(Code *code, Register to_pointer, size_t to_offset, Register r, size_t size)
{
    size_t   half_words = size / 2;
    size_t   words = size / 4;
    size_t   double_words = size / 8;
    size_t   quad_words = double_words / 2;
    Register next_reg = r + 1;
    for (size_t ix = 0; ix < quad_words; ++ix) {
        code_add_instruction(code, "stp", "%s,%s,[%s,#%zu]",
            x_reg(r), x_reg(next_reg), x_reg(to_pointer), to_offset + 16 * ix);
        r = next_reg + 1;
    }
    if (double_words % 2) {
        code_add_instruction(code, "str", "%s,[%s,#%zu]",
            x_reg(r), x_reg(to_pointer), to_offset + 16 * quad_words);
        r = next_reg + 1;
    }
    if (words % 2) {
        code_add_instruction(code, "str", "%s,[%s,#%zu]",
            w_reg(r), x_reg(to_pointer), to_offset + 8 * double_words);
        r = next_reg + 1;
    }
    if (half_words % 2) {
        code_add_instruction(code, "strh", "%s,[%s,#%zu]",
            w_reg(r), x_reg(to_pointer), to_offset + 4 * words);
        r = next_reg + 1;
    }
    if (size % 1) {
        code_add_instruction(code, "strh", "%s,[%s,#%zu]",
            w_reg(r), x_reg(to_pointer), to_offset + 2 * half_words);
    }
}

void code_copy_to_stack(Code *code, Register r, size_t size)
{
    assert(size % 8 == 0);
    size_t double_words = size / 8;
    size_t quad_words = double_words / 2;
    for (size_t ix = 0; ix < quad_words; ++ix) {
        Register next_reg = r + 1;
        code_add_instruction(code, "stp", "%s,%s,[sp,#%zu]",
            x_reg(r), x_reg(next_reg), 16 * ix);
        r = next_reg + 1;
    }
    if (double_words % 2) {
        code_add_instruction(code, "str", "%s,[sp,#%zu]",
            x_reg(r), 16 * quad_words);
    }
}

void code_load_immediate(Code *code, Register reg, ValueLocation loc)
{
    assert(loc.kind == VLK_IMMEDIATE);
    OpcodeMap opcode_map = get_opcode_map(loc.type);
    if ((int) loc.integer.type > 0) {
        uint64_t v = MUST_OPTIONAL(UInt64, integer_unsigned_value(loc.integer));
        code_add_instruction(code, "mov", "%s,#%zu", reg_with_width(reg, opcode_map.reg_width), v);
    } else {
        int64_t v = MUST_OPTIONAL(Int64, integer_signed_value(loc.integer));
        code_add_instruction(code, "mov", "%s,#%ld", reg_with_width(reg, opcode_map.reg_width), v);
    }
}

void code_copy(Code *code, ValueLocation to_location, ValueLocation from_location)
{
    trace(CAT_COMPILE, "copy %.*s to %.*s",
        SV_ARG(value_location_to_string(from_location)),
        SV_ARG(value_location_to_string(to_location)));
    assert(to_location.type == 0 || typeid_underlying_type_id(to_location.type) == typeid_underlying_type_id(from_location.type));
    size_t    sz = typeid_sizeof(from_location.type);
    size_t    aligned_sz = align_at(sz, 16);
    OpcodeMap opcode_map = get_opcode_map(from_location.type);
    code_add_comment(code, "       Copy %.*s", SV_ARG(value_location_to_string(from_location)));
    switch (to_location.kind) {
    case VLK_DISCARD:
        // Nothing.
        break;
    case VLK_POINTER: {
        switch (from_location.kind) {
        case VLK_POINTER: {
            code_copy_memory(code, to_location.pointer.reg, to_location.pointer.offset, from_location.pointer.reg, from_location.pointer.offset, sz);
        } break;
        case VLK_REGISTER: {
            assert(sz <= 8);
            code_copy_from_registers(code, to_location.pointer.reg, to_location.pointer.offset, from_location.reg, sz);
        } break;
        case VLK_REGISTER_RANGE: {
            code_copy_from_registers(code, to_location.pointer.reg, to_location.pointer.offset, from_location.range.start, sz);
        } break;
        case VLK_STACK: {
            code_copy_memory(code, to_location.pointer.reg, to_location.pointer.offset, REG_SP, 0, sz);
        } break;
        case VLK_LABEL: {
            Register label = arm64function_allocate_register(code->function);
            code_load_label(code, label, from_location.static_data);
            code_copy_from_registers(code, to_location.pointer.reg, to_location.pointer.offset, label, sz);
            arm64function_release_register(code->function, label);
        } break;
        case VLK_DATA: {
            Register pointer = arm64function_allocate_register(code->function);
            code_add_instruction(code, "adrp", "%s,%.*s@PAGE", x_reg(pointer), SV_ARG(from_location.static_data.symbol));
            code_add_instruction(code, "add", "%s,%s,%.*s@PAGEOFF",
                x_reg(pointer), x_reg(pointer), SV_ARG(from_location.static_data.symbol));
            if (from_location.static_data.offset != 0) {
                code_add_instruction(code, "add", "%s,%s,#0x%0x",
                    x_reg(pointer), x_reg(pointer), from_location.static_data.offset);
            }
            code_copy_memory(code, to_location.pointer.reg, to_location.pointer.offset, pointer, 0, sz);
            arm64function_release_register(code->function, pointer);
        } break;
        case VLK_IMMEDIATE: {
            Register value = arm64function_allocate_register(code->function);
            code_load_immediate(code, value, from_location);
            code_copy_from_registers(code, to_location.pointer.reg, to_location.pointer.offset, value, sz);
            arm64function_release_register(code->function, value);
        } break;
        case VLK_FLOAT: {
            NYI("code_copy w/ VLK_FLOAT");
        } break;
        default:
            UNREACHABLE();
        }
    } break;
    case VLK_REGISTER: {
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
            code_add_instruction(code, opcode_map.load_opcode, "%s,[%s]",
                reg_with_width(to_location.reg, opcode_map.reg_width), reg(REG_SP));
        } break;
        case VLK_LABEL: {
            code_load_label(code, to_location.reg, from_location.static_data);
        } break;
        case VLK_DATA: {
            Register pointer = arm64function_allocate_register(code->function);
            code_load_label(code, pointer, from_location.static_data);
            arm64function_release_register(code->function, pointer);
        } break;
        case VLK_IMMEDIATE: {
            code_load_immediate(code, to_location.reg, from_location);
        } break;
        case VLK_FLOAT: {
            NYI("code_copy w/ VLK_FLOAT");
        } break;
        default:
            UNREACHABLE();
        }
    } break;
    case VLK_REGISTER_RANGE: {
        assert(to_location.range.end - to_location.range.start == sz / 8);
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
            code_copy_to_registers(code, to_location.range.start, REG_SP, 0, sz);
        } break;
        case VLK_LABEL: {
            code_load_label(code, to_location.range.start, from_location.static_data);
        } break;
        case VLK_DATA: {
            Register pointer = arm64function_allocate_register(code->function);
            code_load_label(code, pointer, from_location.static_data);
            code_copy_to_registers(code, to_location.range.start, pointer, 0, sz);
            arm64function_release_register(code->function, pointer);
        } break;
        case VLK_IMMEDIATE: {
            code_load_immediate(code, to_location.range.start, from_location);
        } break;
        case VLK_FLOAT: {
            NYI("code_copy w/ VLK_FLOAT");
        } break;
        default:
            UNREACHABLE();
        }
    } break;
    case VLK_STACK: {
        assert(typeid_sizeof(to_location.type) == sz);
        switch (from_location.kind) {
        case VLK_POINTER: {
            code_add_instruction(code, "sub", "sp,sp,#%zu", aligned_sz);
            code_copy_memory(code, REG_SP, 0, from_location.pointer.reg, from_location.pointer.offset, sz);
        } break;
        case VLK_REGISTER: {
            code_add_instruction(code, opcode_map.store_opcode, "%s,[%s,-#%ld]!",
                reg_with_width(from_location.reg, opcode_map.reg_width),
                reg(REG_SP), aligned_sz);
        } break;
        case VLK_REGISTER_RANGE: {
            assert(8 * ((int) from_location.range.end - (int) from_location.range.start) == sz);
            code_add_instruction(code, "sub", "sp,sp,#%zu", aligned_sz);
            code_copy_from_registers(code, REG_SP, 0, from_location.range.start, sz);
        } break;
        case VLK_STACK: {
            // NO-OP
        } break;
        case VLK_LABEL: {
            Register label = arm64function_allocate_register(code->function);
            code_load_label(code, label, from_location.static_data);
            code_add_instruction(code, "str", "%s,[%s,-#%ld]!", x_reg(label), reg(REG_SP), aligned_sz);
            arm64function_release_register(code->function, label);
        } break;
        case VLK_DATA: {
            code_add_instruction(code, "sub", "sp,sp,#%zu", aligned_sz);
            Register pointer = arm64function_allocate_register(code->function);
            code_load_label(code, pointer, from_location.static_data);
            code_copy_memory(code, REG_SP, 0, pointer, 0, sz);
            arm64function_release_register(code->function, pointer);
        } break;
        case VLK_IMMEDIATE: {
            Register value = arm64function_allocate_register(code->function);
            code_load_immediate(code, value, from_location);
            code_add_instruction(code, opcode_map.store_opcode, "%s,[%s,-#%ld]!",
                reg_with_width(value, opcode_map.reg_width),
                reg(REG_SP), aligned_sz);
            arm64function_release_register(code->function, value);
        } break;
        case VLK_FLOAT: {
            NYI("code_copy w/ VLK_FLOAT");
        } break;
        default:
            UNREACHABLE();
        }
    } break;
    case VLK_DATA: {
        Register to_pointer = arm64function_allocate_register(code->function);
        code_load_label(code, to_pointer, to_location.static_data);
        switch (from_location.kind) {
        case VLK_POINTER: {
            code_copy_memory(code, to_pointer, 0, from_location.pointer.reg, from_location.pointer.offset, sz);
        } break;
        case VLK_REGISTER: {
            code_add_instruction(code, opcode_map.store_opcode, "%s,[%s]",
                reg_with_width(from_location.reg, opcode_map.reg_width), to_pointer);
        } break;
        case VLK_REGISTER_RANGE: {
            assert(8 * ((int) from_location.range.end - (int) from_location.range.start) == sz);
            code_copy_from_registers(code, to_pointer, 0, from_location.range.start, sz);
        } break;
        case VLK_STACK: {
            code_copy_memory(code, to_pointer, 0, REG_SP, from_location.offset, sz);
        } break;
        case VLK_LABEL: {
            Register label = arm64function_allocate_register(code->function);
            code_load_label(code, label, from_location.static_data);
            code_copy_from_registers(code, to_pointer, 0, label, sz);
            arm64function_release_register(code->function, label);
        } break;
        case VLK_DATA: {
            Register from_pointer = arm64function_allocate_register(code->function);
            code_load_label(code, from_pointer, from_location.static_data);
            code_copy_memory(code, to_pointer, 0, from_pointer, 0, sz);
            arm64function_release_register(code->function, from_pointer);
        } break;
        case VLK_IMMEDIATE: {
            Register value = arm64function_allocate_register(code->function);
            code_load_immediate(code, value, from_location);
            code_add_instruction(code, opcode_map.store_opcode, "%s,[%s]",
                reg_with_width(value, opcode_map.reg_width), to_pointer);
            arm64function_release_register(code->function, value);
        } break;
        case VLK_FLOAT: {
            NYI("code_copy w/ VLK_FLOAT");
        } break;
        default:
            UNREACHABLE();
        }
        arm64function_release_register(code->function, to_pointer);
    } break;
    default:
        UNREACHABLE();
    }
    switch (from_location.kind) {
    case VLK_REGISTER: {
        arm64function_release_register(code->function, from_location.reg);
    } break;
    case VLK_REGISTER_RANGE: {
        for (size_t ix = from_location.range.start; ix < from_location.range.end; ++ix) {
            arm64function_release_register(code->function, ix);
        }
    } break;
    case VLK_POINTER: {
        arm64function_release_register(code->function, from_location.pointer.reg);
    } break;
    case VLK_STACK: {
        if (to_location.kind != VLK_STACK) {
            code_add_instruction(code, "add", "sp,sp,#%zu", aligned_sz);
        }
    } break;
    default:
        break;
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
