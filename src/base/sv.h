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

#include <da.h>
#include <mem.h>

typedef struct string_view {
    char const *ptr;
    size_t      length;
} StringView;

DA_ELEMENTS(StringView, strings)
typedef DA_StringView StringList;

typedef struct string_builder {
    StringView view;
} StringBuilder;

DA(StringBuilder)

extern StringView  sv_null();
extern void        sv_free(StringView sv);
extern StringView  sv_copy(StringView sv);
extern StringView  sv_copy_chars(char const *ptr, size_t len);
extern StringView  sv_copy_cstr(char const *s);
extern StringView  sv_printf(char const *fmt, ...);
extern StringView  sv_vprintf(char const *fmt, va_list args);
extern StringView  sv_from(char const *s);
extern StringView  sv_decode_quoted_str(StringView str);
extern StringView  sv_replace(StringView str, StringView from, StringView to);
extern bool        sv_empty(StringView sv);
extern bool        sv_not_empty(StringView sv);
extern size_t      sv_length(StringView sv);
extern bool        sv_is_cstr(StringView sv);
extern char const *sv_cstr(StringView sv);
extern int         sv_cmp(StringView s1, StringView s2);
extern bool        sv_eq(StringView s1, StringView s2);
extern bool        sv_eq_cstr(StringView s1, char const *s2);
extern bool        sv_eq_chars(StringView s1, char const *s2, size_t n);
extern bool        sv_startswith(StringView s1, StringView s2);
extern bool        sv_endswith(StringView s1, StringView s2);
extern bool        sv_tolong(StringView sv, long *result, StringView *tail);
extern StringView  sv_lchop(StringView sv, size_t num);
extern StringView  sv_rchop(StringView sv, size_t num);
extern int         sv_first(StringView sv, char ch);
extern int         sv_last(StringView sv, char ch);
extern int         sv_find(StringView sv, StringView sub);
extern StringView  sv_substring(StringView sv, size_t at, size_t len);
extern StringList  sv_split(StringView sv, StringView sep);
extern StringList  sv_split_by_whitespace(StringView sv);
extern StringView  sv_strip(StringView sv);

#define SV_SPEC "%.*s"
#define SV_SPEC_RALIGN "%*.s%.*s"
#define SV_SPEC_LALIGN "%.*s%*.s"
#define SV_ARG(sv) (int) sv.length, sv.ptr
#define SV_ARG_RALIGN(sv, width) (int) (width - sv.length), "", (int) sv.length, sv.ptr
#define SV_ARG_LALIGN(sv, width) (int) sv.length, sv.ptr, (int) (width - sv.length), ""

extern StringBuilder sb_create();
extern StringBuilder sb_createf(char const *fmt, ...);
extern StringBuilder sb_vcreatef(char const *fmt, va_list args);
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
#define SB_ARG(sb) (int) (sb).view.length, (sb).view.ptr

extern StringList  sl_create();
extern StringList  sl_copy(StringList *sl);
extern StringList *sl_push(StringList *sl, StringView sv);
extern StringList *sl_extend(StringList *sl, StringList *with);
extern StringView  sl_pop(StringList *sl);
extern StringView  sl_join(StringList *sl, StringView sep);
extern StringView  sl_front(StringList *sl);
extern StringView  sl_back(StringList *sl);
extern bool        sl_empty(StringList *sl);
extern size_t      sl_size(StringList *sl);

#endif /* __SV_H__ */
