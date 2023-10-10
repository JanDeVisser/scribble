/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <allocate.h>
#include <options.h>

#include <arm64.h>

DECLARE_SHARED_ALLOCATOR(arm64)

Code *code_acreate(Allocator *allocator, StringView prolog, StringView epilog)
{
    Code *ret = allocate_new(Code);
    ret->allocator = allocator;
    ret->prolog = sb_acreate(allocator);
    if (!sv_empty(prolog)) {
        sb_append_sv(&ret->prolog, prolog);
    }
    ret->code = sb_acreate(allocator);
    ret->epilog = sb_acreate(allocator);
    if (!sv_empty(epilog)) {
        sb_append_sv(&ret->epilog, epilog);
    }
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
    StringView opcode_sv = sv_aprintf(code->allocator, "\t%s\t%s", opcode, arg_fmt);
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
        txt = sv_avprintf(code->allocator, text, args);
    }
    txt = sv_strip(txt);
    if (sv_empty(txt)) {
        return;
    }
    StringList lines = sv_asplit(code->allocator, txt, sv_from("\n"));
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
        StringList parts = sv_asplit_by_whitespace(code->allocator, line);
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
    sb_printf(code->active, "%.*s\t%.*s\n", SV_ARG(directive), SV_ARG(args));
}

void code_add_comment(Code *code, StringView comment)
{
    StringList sl = sv_asplit(code->allocator, comment, sv_from("\n"));
    for (size_t ix = 0; ix < sl.size; ++ix) {
        sb_printf(code->active, "\t// %.*s\n", SV_ARG(sl.strings[ix]));
    }
}

StringView code_to_string(Code *code)
{
    StringBuilder ret = sb_acreate(code->allocator);
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

void code_enter_function(Code *code, StringView name, size_t stack_depth)
{
    code_add_directive(code, sv_from(".global"), name);
    code_add_label(code, name);
    code_add_instruction(code, "stp", "fp,lr,[sp,#-16]!");
    if (stack_depth > 0) {
        code_add_instruction(code, "sub", "sp,sp,#%zu", stack_depth);
    }
    code_add_instruction(code, "mov", "fp,sp");
}

void code_leave_function(Code *code, size_t stack_depth)
{
    code_add_instruction(code, "mov", "sp,fp");
    if (stack_depth > 0) {
        code_add_instruction(code, "add", "sp,sp,#%zu", stack_depth);
    }
    code_add_instruction(code, "ldp", "fp,lr,[sp],16");
    code_add_instruction(code, "ret", "");
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
