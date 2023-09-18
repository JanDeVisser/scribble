/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#ifndef __SV_H__
#define __SV_H__

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include <mem.h>

typedef struct string_view {
    char const *ptr;
    size_t      length;
    size_t      capacity;
} StringView;

typedef struct string_builder {
    Allocator *allocator;
    StringView view;
} StringBuilder;

extern StringView sv_null();
extern void       sv_free(StringView sv);
extern StringView sv_from(char const *s);
extern StringView sv_copy(StringView sv);
extern StringView sv_copy_chars(char const *ptr, size_t len);
extern StringView sv_copy_cstr(char const *s);
extern StringView sv_copy_with_allocator(StringView sv, Allocator *allocator);
extern StringView sv_copy_chars_with_allocator(char const *ptr, size_t len, Allocator *allocator);
extern StringView sv_copy_cstr_with_allocator(char const *s, Allocator *allocator);
extern bool       sv_empty(StringView sv);
extern bool       sv_not_empty(StringView sv);
extern size_t     sv_length(StringView sv);
extern int        sv_cmp(StringView s1, StringView s2);
extern bool       sv_eq(StringView s1, StringView s2);
extern bool       sv_eq_cstr(StringView s1, char const *s2);
extern bool       sv_eq_cstr_n(StringView s1, char const *s2, size_t n);
extern bool       sv_startswith(StringView s1, StringView s2);
extern bool       sv_endswith(StringView s1, StringView s2);
extern bool       sv_tolong(StringView sv, long *result, StringView *tail);
extern StringView sv_chop(StringView sv, size_t num);
extern int        sv_find_char(StringView sv, char ch);
extern int        sv_find_str(StringView sv, StringView sub);
extern StringView sv_substring(StringView sv, size_t at, size_t len);
extern size_t     sv_split(StringView sv, StringView sep, size_t max_components, StringView components[]);

extern StringView sv_printf(char const *fmt, ...);
extern StringView sv_vprintf(char const *fmt, va_list args);

#define SV_SPEC "%.*s"
#define SV_SPEC_RALIGN "%*.s%.*s"
#define SV_SPEC_LALIGN "%.*s%*.s"
#define SV_ARG(sv) (int) sv.length, sv.ptr
#define SV_ARG_RALIGN(sv, width) (int) (width - sv.length), "", (int) sv.length, sv.ptr
#define SV_ARG_LALIGN(sv, width) (int) sv.length, sv.ptr, (int) (width - sv.length), ""

extern StringBuilder sb_create();
extern StringBuilder sb_create_with_allocator(Allocator *allocator);
extern StringBuilder sb_copy_chars(char const *ptr, size_t len);
extern StringBuilder sb_copy_cstr(char const *s);
extern StringBuilder sb_copy_sv(StringView sv);
extern void          sb_append_chars(StringBuilder *sb, char const *ptr, size_t len);
extern void          sb_append_sv(StringBuilder *sb, StringView sv);
extern void          sb_append_cstr(StringBuilder *sb, char const *s);
extern void          sb_vprintf(StringBuilder *sb, char const *fmt, va_list args);
extern void          sb_printf(StringBuilder *sb, char const *fmt, ...);
extern StringView    sb_view(StringBuilder *sb);

#define SB_SPEC SV_SPEC
#define SB_ARG(sb) (int) sb->view.length, sb->view.ptr

#endif /* __SV_H__ */
