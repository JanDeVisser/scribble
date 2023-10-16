/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <errno.h>

#include <allocate.h>
#include <error_or.h>
#include <options.h>
#include <process.h>

#include <arm64.h>

DECLARE_SHARED_ALLOCATOR(arm64)

Assembly *assembly_acreate(Allocator *allocator, StringView name)
{
    Assembly *ret = allocator_alloc_new(allocator, Assembly);
    ret->allocator = allocator;
    ret->name = name;
#ifdef IS_APPLE
    ret->code = code_acreate(allocator,
        sv_from(".section\t__TEXT,__text,regular,pure_instructions\n\n.align 2\n\n"),
        sv_null());
#elif defined(IS_LINUX)
    ret->code = code_acreate(allocator,
        sv_from(".text\n\n.align 2\n\n"),
        sv_null());
#endif
    ret->statik = code_acreate(allocator, sv_null(), sv_null());
    code_select_prolog(ret->statik);
    code_enter_function(ret->statik, sv_aprintf(allocator, "static_%.*s", SV_ARG(name)), 0);
    code_select_epilog(ret->statik);
    code_leave_function(ret->statik, 0);
    code_select_code(ret->statik);
    ret->active = ret->code;
    return ret;
}

void assembly_add_instruction(Assembly *assembly, char const *opcode, char const *arg_fmt, ...)
{
    va_list args;
    va_start(args, arg_fmt);
    if (assembly->active == assembly->statik) {
        assembly->has_static = true;
    }
    code_vadd_instruction(assembly->active, opcode, arg_fmt, args);
    va_end(args);
}

void assembly_add_text_sv(Assembly *assembly, StringView text)
{
    assembly_add_text(assembly, sv_cstr(text));
}

void assembly_add_text(Assembly *assembly, char const *text, ...)
{
    va_list args;
    va_start(args, text);
    assembly_vadd_text(assembly, text, args);
    va_end(args);
}

void assembly_vadd_text(Assembly *assembly, char const *text, va_list args)
{
    if (assembly->active == assembly->statik) {
        assembly->has_static = true;
    }
    code_vadd_text(assembly->active, text, args);
}

void assembly_add_label(Assembly *assembly, StringView label)
{
    if (assembly->active == assembly->statik) {
        assembly->has_static = true;
    }
    code_add_label(assembly->active, label);
}

void assembly_add_comment(Assembly *assembly, char const *comment, ...)
{
    if (assembly->active == assembly->statik) {
        assembly->has_static = true;
    }
    va_list args;
    va_start(args, comment);
    StringView formatted = sv_avprintf(assembly->allocator, comment, args);
    va_end(args);
    code_add_comment(assembly->active, formatted);
}

void assembly_enter_function(Assembly *assembly, StringView fnc, size_t stack_depth)
{
    if (assembly->active == assembly->statik) {
        assembly->has_static = true;
    }
    code_enter_function(assembly->active, fnc, stack_depth);
}

void assembly_leave_function(Assembly *assembly, size_t stack_depth)
{
    if (assembly->active == assembly->statik) {
        assembly->has_static = true;
    }
    code_leave_function(assembly->active, stack_depth);
}

void assembly_add_directive(Assembly *assembly, StringView directive, StringView args)
{
    if (assembly->active == assembly->statik) {
        assembly->has_static = true;
    }
    if (sv_eq_cstr(directive, ".global")) {
        assembly->has_exports = true;
        if (sv_eq_cstr(args, "main")) {
            assembly->has_main = true;
        }
    }
    code_add_directive(assembly->active, directive, args);
}

size_t label_reserve_id()
{
    static size_t id = 0;
    return id++;
}

size_t assembly_add_string(Assembly *assembly, StringView str)
{
    for (StringID *sid = assembly->strings; sid; sid = sid->next) {
        if (sv_eq(sid->string, str)) {
            return sid->id;
        }
    }
    size_t id = label_reserve_id();
    sb_printf(&assembly->text, ".align 2\nstr_%zu:\n\t.string\t\"%.*s\"\n", id, SV_ARG(str));
    StringID *sid = allocator_alloc_new(assembly->allocator, StringID);
    sid->string = str;
    sid->id = id;
    sid->next = assembly->strings;
    assembly->strings = sid;
    return id;
}

void assembly_add_data(Assembly *assembly, StringView label, bool global, StringView type, bool is_static, StringView value)
{
    if (sv_empty(assembly->data.view)) {
        sb_append_cstr(&assembly->data, "\n.section __DATA,__data\n");
    }
    if (global) {
        sb_printf(&assembly->data, "\n.global %.*s\n", SV_ARG(label));
    }
    sb_printf(&assembly->data, "\n.align 8\n%.*s:\n\t%.*s\t%.*s\n", SV_ARG(label), SV_ARG(type), SV_ARG(value));
    if (is_static) {
        sb_append_cstr(&assembly->data, "\t.short 0\n");
    }
}

StringView assembly_to_string(Assembly *assembly)
{
    StringBuilder ret = sb_acreate(assembly->allocator);
    sb_append_sv(&ret, code_to_string(assembly->code));
    sb_append_cstr(&ret, "\n");
    if (code_has_text(assembly->statik)) {
        sb_append_sv(&ret, code_to_string(assembly->statik));
        sb_append_cstr(&ret, "\n");
    }
    sb_append_sv(&ret, assembly->text.view);
    sb_append_cstr(&ret, "\n");
    sb_append_sv(&ret, assembly->data.view);
    sb_append_cstr(&ret, "\n");
    return ret.view;
}

void assembly_save_and_assemble(Assembly *assembly, StringView bare_file_name)
{
    StringView asm_file = sv_aprintf(assembly->allocator, "%.*s.s", SV_ARG(bare_file_name));
    StringView obj_file = sv_aprintf(assembly->allocator, "%.*s.o", SV_ARG(bare_file_name));
    FILE      *s = fopen(sv_cstr(asm_file), "w+");
    if (!s) {
        fatal("Could not open assembly file %.*s: %s", SV_ARG(asm_file), strerror(errno));
    }
    StringView asm_text = assembly_to_string(assembly);
    if (fwrite(sv_cstr(asm_text), 1, asm_text.length, s) != asm_text.length) {
        fatal("Could not write assembly text to %.*s: %s", SV_ARG(asm_file), strerror(ferror(s)));
    }
    fclose(s);
    MUST_VOID(Int, execute(sv_from("as"), sv_cstr(asm_file), "-o", sv_cstr(obj_file)))
}

bool assembly_has_exports(Assembly *assembly)
{
    return assembly->has_exports;
}

bool assembly_has_main(Assembly *assembly)
{
    return assembly->has_main;
}

bool assembly_has_static(Assembly *assembly)
{
    return assembly->has_static;
}

Code *assembly_static_initializer(Assembly *assembly)
{
    return assembly->statik;
}

void assembly_select_code(Assembly *assembly)
{
    assembly->active = assembly->code;
}

void assembly_select_static(Assembly *assembly)
{
    assembly->active = assembly->statik;
}

void assembly_push(Assembly *assembly, char const *reg)
{
    assembly_add_instruction(assembly, "str", "%s,[sp,-16]!", reg);
}

void assembly_pop(Assembly *assembly, char const *reg)
{
    assembly_add_instruction(assembly, "ldr", "%s,[sp],16", reg);
}

void assembly_write_char(Assembly *assembly, int fd, char ch)
{
    assembly_add_instruction(assembly, "mov", "w0,#0x%02x", (uint8_t) ch);
    assembly_add_instruction(assembly, "strb", "w0,[sp,-16]!");
    assembly_add_instruction(assembly, "mov", "x0,#%d", fd); // x0: fd
    assembly_add_instruction(assembly, "mov", "x1,sp");      // x1: SP
    assembly_add_instruction(assembly, "mov", "x2,#1");      // x2: Size-Remainingber of characters
    assembly_syscall(assembly, SYSCALL_WRITE);
    assembly_add_instruction(assembly, "add", "sp,sp,16");
}

void assembly_syscall(Assembly *assembly, int id)
{
    assembly_add_instruction(assembly, "mov", SYSCALL_REG ",#0x%02x", id);
    assembly_add_instruction(assembly, "svc", "#0x00");
}

void assembly_syscall1(Assembly *assembly, int id, uint64_t arg1)
{
    assembly_add_instruction(assembly, "mov", "x0,#0x%x", arg1);
    assembly_syscall(assembly, id);
}

void assembly_syscall2(Assembly *assembly, int id, uint64_t arg1, uint64_t arg2)
{
    assembly_add_instruction(assembly, "mov", "x0,#0x%x", arg1);
    assembly_add_instruction(assembly, "mov", "x1,#0x%x", arg2);
    assembly_syscall(assembly, id);
}

void assembly_syscall3(Assembly *assembly, int id, uint64_t arg1, uint64_t arg2, uint64_t arg3)
{
    assembly_add_instruction(assembly, "mov", "x0,#0x%x", arg1);
    assembly_add_instruction(assembly, "mov", "x1,#0x%x", arg2);
    assembly_add_instruction(assembly, "mov", "x2,#0x%x", arg3);
    assembly_syscall(assembly, id);
}

void assembly_syscall4(Assembly *assembly, int id, uint64_t arg1, uint64_t arg2, uint64_t arg3, uint64_t arg4)
{
    assembly_add_instruction(assembly, "mov", "x0,#0x%x", arg1);
    assembly_add_instruction(assembly, "mov", "x1,#0x%x", arg2);
    assembly_add_instruction(assembly, "mov", "x2,#0x%x", arg3);
    assembly_add_instruction(assembly, "mov", "x3,#0x%x", arg4);
    assembly_syscall(assembly, id);
}

void assembly_copy_pointers(Assembly *assembly, Register to_pointer, size_t to_offset, Register from_pointer, size_t from_offset, size_t size)
{
    Register temp1 = assembly_allocate_register(assembly);
    Register temp2 = assembly_allocate_register(assembly);
    size_t   remaining = size;
    while (remaining >= 16) {
        assembly_add_instruction(assembly, "ldp", "%s,%s,[%s,#%zu]",
            x_reg(temp1), x_reg(temp2), x_reg(from_pointer), from_offset + size - remaining);
        assembly_add_instruction(assembly, "stp", "%s,%s,[%s,#%zu]",
            x_reg(temp1), x_reg(temp2), x_reg(to_pointer), to_offset + size - remaining);
        remaining -= 16;
    }
    if (remaining >= 8) {
        assembly_add_instruction(assembly, "ldr", "%s,[%s,#%zu]",
            x_reg(temp1), x_reg(from_pointer), from_offset + size - remaining);
        assembly_add_instruction(assembly, "str", "%s,[%s,#%zu]",
            x_reg(temp1), x_reg(to_pointer), to_offset + size - remaining);
        remaining -= 8;
    }
    if (remaining >= 4) {
        assembly_add_instruction(assembly, "ldr", "%s,[%s,#%zu]",
            w_reg(temp1 + 32), x_reg(from_pointer), from_offset + size - remaining);
        assembly_add_instruction(assembly, "str", "%s,[%s,#%zu]",
            w_reg(temp1 + 32), x_reg(to_pointer), to_offset + size - remaining);
        remaining -= 4;
    }
    if (remaining >= 2) {
        assembly_add_instruction(assembly, "ldrh", "%s,[%s,#%zu]",
            w_reg(temp1), x_reg(from_pointer), from_offset + size - remaining);
        assembly_add_instruction(assembly, "strh", "%s,[%s,#%zu]",
            w_reg(temp1), x_reg(to_pointer), to_offset + size - remaining);
        remaining -= 2;
    }
    if (remaining == 1) {
        assembly_add_instruction(assembly, "ldrb", "%s,[%s,#%zu]",
            w_reg(temp1), x_reg(from_pointer), from_offset + size - remaining);
        assembly_add_instruction(assembly, "strb", "%s,[%s,#%zu]",
            w_reg(temp1), x_reg(to_pointer), to_offset + size - remaining);
        remaining -= 1;
    }
    assert(!remaining);
    assembly_release_register(assembly, temp1);
    assembly_release_register(assembly, temp2);
}

void assembly_copy_to_registers(Assembly *assembly, Register r, Register from_pointer, size_t from_offset, size_t size)
{
    assert(size % 8 == 0);
    size_t quad_words = size / 16;
    for (size_t ix = 0; ix < quad_words; ++ix) {
        Register next_reg = r + 1;
        assembly_add_instruction(assembly, "ldp", "%s,%s,[%s,#%zu]",
            x_reg(r), x_reg(next_reg), x_reg(from_pointer), from_offset + 16 * ix);
        r = next_reg + 1;
    }
    if (quad_words % 2) {
        assembly_add_instruction(assembly, "ldr", "%s,[%s,#%zu]",
            x_reg(r), x_reg(from_pointer), from_offset + 16 * quad_words);
    }
}

void assembly_copy_from_registers(Assembly *assembly, Register to_pointer, size_t to_offset, Register r, size_t size)
{
    assert(size % 8 == 0);
    size_t quad_words = size / 16;
    for (size_t ix = 0; ix < quad_words; ++ix) {
        Register next_reg = r + 1;
        assembly_add_instruction(assembly, "stp", "%s,%s,[%s,#%zu]",
            x_reg(r), x_reg(next_reg), x_reg(to_pointer), to_offset + 16 * ix);
        r = next_reg + 1;
    }
    if (quad_words % 2) {
        assembly_add_instruction(assembly, "ldr", "%s,[%s,#%zu]",
            x_reg(r), x_reg(to_pointer), to_offset + 16 * quad_words);
    }
}

void assembly_copy_to_stack(Assembly *assembly, Register r, size_t size)
{
    assert(size % 8 == 0);
    size_t quad_words = size / 16;
    for (size_t ix = 0; ix < quad_words; ++ix) {
        Register next_reg = r + 1;
        assembly_add_instruction(assembly, "stp", "%s,%s,[sp,#%zu]",
            x_reg(r), x_reg(next_reg), 16 * ix);
        r = next_reg + 1;
    }
    if (quad_words % 2) {
        assembly_add_instruction(assembly, "str", "%s,[sp,#%zu]",
            x_reg(r), 16 * quad_words);
    }
}

void assembly_copy(Assembly *assembly, ValueLocation to_location, ValueLocation from_location)
{
    assert(to_location.type == from_location.type);
    size_t    sz = align_at(typeid_sizeof(from_location.type), 8);
    OpcodeMap opcode_map = get_opcode_map(from_location.type);
    assembly_add_comment(assembly, "copy %.*s to %.*s",
        SV_ARG(value_location_to_string(from_location, assembly->allocator)),
        SV_ARG(value_location_to_string(to_location, assembly->allocator)));
    switch (to_location.kind) {
    case VLK_POINTER: {
        switch (from_location.kind) {
        case VLK_POINTER: {
            assembly_copy_pointers(assembly, to_location.pointer.reg, to_location.pointer.offset, from_location.pointer.reg, from_location.pointer.offset, sz);
        } break;
        case VLK_REGISTER: {
            assert(sz == 8);
            assembly_add_instruction(assembly, opcode_map.store_opcode, "%s,[%s,#%zu]",
                reg_with_width(from_location.reg, opcode_map.reg_width),
                x_reg(to_location.pointer.reg), to_location.pointer.offset);
        } break;
        case VLK_REGISTER_RANGE: {
            assert(8 * ((int) from_location.range.end - (int) from_location.range.start) == sz);
            assembly_copy_from_registers(assembly, to_location.pointer.reg, to_location.pointer.offset, from_location.range.start, sz);
        } break;
        case VLK_STACK: {
            assembly_copy_pointers(assembly, to_location.pointer.reg, to_location.pointer.offset, REG_SP, from_location.offset, sz);
        } break;
        case VLK_SYMBOL: {
            Register pointer = assembly_allocate_register(assembly);
            assembly_add_instruction(assembly, "adrp", "%s,%.*s@PAGE", x_reg(pointer), SV_ARG(from_location.symbol));
            assembly_add_instruction(assembly, "add", "%s,%s,%.*s@PAGEOFF",
                x_reg(pointer), x_reg(pointer), SV_ARG(from_location.symbol));
            assembly_copy_pointers(assembly, to_location.pointer.reg, to_location.pointer.offset, pointer, 0, sz);
            assembly_release_register(assembly, pointer);
        } break;
        default:
            UNREACHABLE();
        }
    } break;
    case VLK_REGISTER: {
        assert(sz == 8);
        switch (from_location.kind) {
        case VLK_POINTER: {
            assembly_add_instruction(assembly, opcode_map.load_opcode, "%s,[%s,#%ld]",
                reg_with_width(to_location.reg, opcode_map.reg_width),
                reg(from_location.pointer.reg), from_location.pointer.offset);
        } break;
        case VLK_REGISTER: {
            assembly_add_instruction(assembly, "mov", "%s,%s",
                reg_with_width(to_location.reg, opcode_map.reg_width),
                reg_with_width(from_location.reg, opcode_map.reg_width));
        } break;
        case VLK_REGISTER_RANGE: {
            assert((int) from_location.range.end - (int) from_location.range.start == 1);
            assembly_add_instruction(assembly, "mov", "%s,%s",
                reg_with_width(to_location.reg, opcode_map.reg_width),
                reg_with_width(from_location.range.start, opcode_map.reg_width));
        } break;
        case VLK_STACK: {
            assembly_add_instruction(assembly, opcode_map.load_opcode, "%s,[sp,#%ld]",
                reg_with_width(to_location.reg, opcode_map.reg_width),
                reg(from_location.pointer.reg), from_location.offset);
        } break;
        case VLK_SYMBOL: {
            Register pointer = assembly_allocate_register(assembly);
            assembly_add_instruction(assembly, "adrp", "%s,%.*s@PAGE", x_reg(pointer), SV_ARG(from_location.symbol));
            assembly_add_instruction(assembly, opcode_map.load_opcode, "%s,[%s,%.*s@PAGEOFF]",
                reg_with_width(to_location.reg, opcode_map.reg_width),
                x_reg(pointer), SV_ARG(from_location.symbol));
            assembly_release_register(assembly, pointer);
        } break;
        default:
            UNREACHABLE();
        }
    } break;
    case VLK_REGISTER_RANGE: {
        assert(sz == 8);
        switch (from_location.kind) {
        case VLK_POINTER: {
            assembly_copy_to_registers(assembly, to_location.range.start, from_location.pointer.reg, from_location.pointer.offset, sz);
        } break;
        case VLK_REGISTER: {
            assert((int) to_location.range.end - (int) to_location.range.start == 1);
            assembly_add_instruction(assembly, "mov", "%s,%s",
                reg_with_width(to_location.range.start, opcode_map.reg_width),
                reg_with_width(from_location.reg, opcode_map.reg_width));
        } break;
        case VLK_REGISTER_RANGE: {
            assert((int) from_location.range.end - (int) from_location.range.start == (int) to_location.range.end - (int) to_location.range.start);
            for (size_t ix = 0; ix < (int) from_location.range.end - (int) from_location.range.start; ++ix) {
                assembly_add_instruction(assembly, "mov", "%s,%s",
                    reg_with_width(to_location.range.start + ix, opcode_map.reg_width),
                    reg_with_width(from_location.range.start + ix, opcode_map.reg_width));
            }
        } break;
        case VLK_STACK: {
            assembly_copy_to_registers(assembly, to_location.range.start, REG_SP, from_location.offset, sz);
        } break;
        case VLK_SYMBOL: {
            Register pointer = assembly_allocate_register(assembly);
            assembly_add_instruction(assembly, "adrp", "%s,%.*s@PAGE", x_reg(pointer), SV_ARG(from_location.symbol));
            assembly_add_instruction(assembly, "add", "%s,%s,%.*s@PAGEOFF",
                x_reg(pointer), x_reg(pointer), SV_ARG(from_location.symbol));
            assembly_copy_to_registers(assembly, to_location.range.start, pointer, 0, sz);
            assembly_release_register(assembly, pointer);
        } break;
        default:
            UNREACHABLE();
        }
    } break;
    case VLK_STACK: {
        switch (from_location.kind) {
        case VLK_POINTER: {
            assembly_copy_pointers(assembly, REG_SP, to_location.offset, from_location.pointer.reg, from_location.pointer.offset, sz);
        } break;
        case VLK_REGISTER: {
            assert(sz == 8);
            assembly_add_instruction(assembly, opcode_map.store_opcode, "%s,[%s,#%ld]",
                reg_with_width(from_location.reg, opcode_map.reg_width),
                reg(REG_SP), to_location.offset);
        } break;
        case VLK_REGISTER_RANGE: {
            assert(8 * ((int) from_location.range.end - (int) from_location.range.start) == sz);
            assembly_copy_from_registers(assembly, REG_SP, to_location.offset, from_location.range.start, sz);
        } break;
        case VLK_STACK: {
            assembly_copy_pointers(assembly, REG_SP, to_location.offset, REG_SP, from_location.offset, sz);
        } break;
        case VLK_SYMBOL: {
            Register pointer = assembly_allocate_register(assembly);
            assembly_add_instruction(assembly, "adrp", "%s,%.*s@PAGE", x_reg(pointer), SV_ARG(from_location.symbol));
            assembly_add_instruction(assembly, "add", "%s,%s,%.*s@PAGEOFF",
                x_reg(pointer), x_reg(pointer), SV_ARG(from_location.symbol));
            assembly_copy_pointers(assembly, REG_SP, to_location.offset, pointer, 0, sz);
            assembly_release_register(assembly, pointer);
        } break;
        default:
            UNREACHABLE();
        }
    } break;
    case VLK_SYMBOL: {
        Register to_pointer = assembly_allocate_register(assembly);
        assembly_add_instruction(assembly, "adrp", "%s,%.*s@PAGE", x_reg(to_pointer), SV_ARG(to_location.symbol));
        assembly_add_instruction(assembly, "add", "%s,%s,%.*s@PAGEOFF",
            x_reg(to_pointer), x_reg(to_pointer), SV_ARG(to_location.symbol));
        switch (from_location.kind) {
        case VLK_POINTER: {
            assembly_copy_pointers(assembly, to_pointer, 0, from_location.pointer.reg, from_location.pointer.offset, sz);
        } break;
        case VLK_REGISTER: {
            assert(sz == 8);
            assembly_add_instruction(assembly, opcode_map.store_opcode, "%s,[%s]",
                reg_with_width(from_location.reg, opcode_map.reg_width), to_pointer);
        } break;
        case VLK_REGISTER_RANGE: {
            assert(8 * ((int) from_location.range.end - (int) from_location.range.start) == sz);
            assembly_copy_from_registers(assembly, to_pointer, 0, from_location.range.start, sz);
        } break;
        case VLK_STACK: {
            assembly_copy_pointers(assembly, to_pointer, 0, REG_SP, from_location.offset, sz);
        } break;
        case VLK_SYMBOL: {
            Register from_pointer = assembly_allocate_register(assembly);
            assembly_add_instruction(assembly, "adrp", "%s,%.*s@PAGE", x_reg(from_pointer), SV_ARG(from_location.symbol));
            assembly_add_instruction(assembly, "add", "%s,%s,%.*s@PAGEOFF",
                x_reg(from_pointer), x_reg(from_pointer), SV_ARG(from_location.symbol));
            assembly_copy_pointers(assembly, to_pointer, 0, from_pointer, 0, sz);
            assembly_release_register(assembly, from_pointer);
        } break;
        default:
            UNREACHABLE();
        }
        assembly_release_register(assembly, to_pointer);
    } break;
    default:
        UNREACHABLE();
    }
}

extern Register assembly_allocate_register(Assembly *assembly)
{
    for (Register ix = REG_X9; ix < REG_X16; ++ix) {
        if (!assembly->registers[ix]) {
            assembly->registers[ix] = true;
            return ix;
        }
    }
    for (Register ix = REG_X19; ix < REG_FP; ++ix) {
        if (!assembly->registers[ix]) {
            assembly->registers[ix] = true;
            assembly->callee_saved[ix - REG_X19] = true;
            return ix;
        }
    }
    fatal("Out of registers");
}

void assembly_release_register(Assembly *assembly, Register reg)
{
    assembly->registers[reg] = false;
}

void assembly_release_all_registers(Assembly *assembly)
{
    for (Register ix = REG_X9; ix < REG_FP; ++ix) {
        assembly->registers[ix] = false;
    }
    for (Register ix = REG_X19; ix < REG_FP; ++ix) {
        assembly->callee_saved[ix - REG_X19] = false;
    }
}
