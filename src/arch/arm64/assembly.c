/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <errno.h>

#include <allocate.h>
#include <error.h>
#include <options.h>
#include <process.h>

#include <arch/arm64/arm64.h>

DECLARE_SHARED_ALLOCATOR(arm64)

Assembly *assembly_acreate(Allocator *allocator, StringView name)
{
    Assembly *ret = allocator_alloc_new(allocator, Assembly);
    ret->allocator = allocator;
    ret->name = name;
    ret->code = code_acreate(allocator,
        sv_from(".section\t__TEXT,__text,regular,pure_instructions\n\n.align 2\n\n"),
        sv_null());
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
    assembly_add_instruction(assembly, "mov", "x2,#1");      // x2: Number of characters
    assembly_syscall(assembly, 0x04);
    assembly_add_instruction(assembly, "add", "sp,sp,16");
}

void assembly_syscall(Assembly *assembly, int id)
{
    assembly_add_instruction(assembly, "mov", "x16,#0x%02x", id);
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
