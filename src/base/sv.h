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
#include <error_or.h>
#include <integer.h>
#include <mem.h>
#include <optional.h>

typedef struct string_view {
    char const *ptr;
    size_t      length;
} StringView;

OPTIONAL(StringView)
ERROR_OR(StringView);

DA_ELEMENTS(StringView, strings)
typedef DA_StringView StringList;

typedef struct string_builder {
    StringView view;
} StringBuilder;

DA(StringBuilder)

#define INTEGER_SIZES(S) S(8) S(16) S(32) S(64)

typedef struct integer_parse_result {
    bool       success;
    StringView tail;
    Integer    integer;
} IntegerParseResult;

typedef struct StringScanner {
    StringView string;
    size_t     mark;
    size_t     point;
} StringScanner;

extern void               free_buffer(char  *buffer);
extern StringView         sv_null();
extern void               sv_free(StringView sv);
extern StringView         sv_copy(StringView sv);
extern StringView         sv_copy_chars(char const *ptr, size_t len);
extern StringView         sv_copy_cstr(char const *s);
extern StringView         sv_render_integer(Integer integer);
extern StringView         sv_render_hex_integer(Integer integer);
extern StringView         sv_printf(char const *fmt, ...);
extern StringView         sv_vprintf(char const *fmt, va_list args);
extern StringView         sv_replicate(StringView s, int repeats);
extern StringView         sv_from(char const *s);
extern StringView         sv_decode_quoted_str(StringView str);
extern StringView         sv_replace(StringView str, StringView from, StringView to);
extern bool               sv_empty(StringView sv);
extern bool               sv_not_empty(StringView sv);
extern size_t             sv_length(StringView sv);
extern bool               sv_is_cstr(StringView sv);
extern char const        *sv_cstr(StringView sv);
extern int                sv_cmp(StringView s1, StringView s2);
extern bool               sv_eq(StringView s1, StringView s2);
extern bool               sv_eq_cstr(StringView s1, char const *s2);
extern bool               sv_eq_chars(StringView s1, char const *s2, size_t n);
extern bool               sv_eq_ignore_case(StringView s1, StringView s2);
extern bool               sv_eq_ignore_case_cstr(StringView s1, char const *s2);
extern bool               sv_eq_ignore_case_chars(StringView s1, char const *s2, size_t n);
extern bool               sv_startswith(StringView s1, StringView s2);
extern bool               sv_endswith(StringView s1, StringView s2);
extern IntegerParseResult sv_parse_integer(StringView sv, IntegerType type);
extern StringView         sv_lchop(StringView sv, size_t num);
extern StringView         sv_rchop(StringView sv, size_t num);
extern StringView         sv_chop_to_delim(StringView *sv, StringView delim);
extern int                sv_first(StringView sv, char ch);
extern int                sv_last(StringView sv, char ch);
extern int                sv_find(StringView sv, StringView sub);
extern StringView         sv_substring(StringView sv, size_t at, size_t len);
extern StringList         sv_split(StringView sv, StringView sep);
extern StringList         sv_split_by_whitespace(StringView sv);
extern StringView         sv_strip(StringView sv);

#undef INTEGER_SIZE
#define INTEGER_SIZE(sz)                                     \
    extern IntegerParseResult sv_parse_u##sz(StringView sv); \
    extern IntegerParseResult sv_parse_i##sz(StringView sv);
INTEGER_SIZES(INTEGER_SIZE)
#undef INTEGER_SIZE

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
extern void          sb_clear(StringBuilder *sb);
extern void          sb_append_chars(StringBuilder *sb, char const *ptr, size_t len);
extern void          sb_append_sv(StringBuilder *sb, StringView sv);
extern void          sb_append_cstr(StringBuilder *sb, char const *s);
extern void          sb_append_char(StringBuilder *sb, char ch);
extern void          sb_vprintf(StringBuilder *sb, char const *fmt, va_list args);
extern void          sb_printf(StringBuilder *sb, char const *fmt, ...);
extern void          sb_insert_sv(StringBuilder *sb, StringView sv, size_t at);
extern void          sb_insert_chars(StringBuilder *sb, char const *ptr, size_t len, size_t at);
extern void          sb_insert_cstr(StringBuilder *sb, char const *str, size_t at);
extern StringView    sb_view(StringBuilder *sb);

#define SB_SPEC SV_SPEC
#define SB_ARG(sb) (int) (sb).view.length, (sb).view.ptr

extern StringList  sl_create();
extern void        sl_free(StringList *sl);
extern StringList  sl_copy(StringList *sl);
extern StringList *sl_push(StringList *sl, StringView sv);
extern StringList *sl_extend(StringList *sl, StringList *with);
extern StringView  sl_pop(StringList *sl);
extern StringView  sl_join(StringList *sl, StringView sep);
extern StringView  sl_front(StringList *sl);
extern StringView  sl_back(StringList *sl);
extern bool        sl_empty(StringList *sl);
extern size_t      sl_size(StringList *sl);

extern StringScanner ss_create(StringView sv);
extern void          ss_reset(StringScanner *ss);
extern bool          ss_expect(StringScanner *ss, char ch);
extern bool          ss_expect_sv(StringScanner *ss, StringView sv);
extern bool          ss_expect_with_offset(StringScanner *ss, char ch, size_t offset);
extern bool          ss_expect_one_of_with_offset(StringScanner *ss, char const *expect, size_t offset);
extern bool          ss_expect_one_of(StringScanner *ss, char const *expect);
extern bool          ss_is_one_of(StringScanner *ss, char const *expect);
extern bool          ss_is_one_of_with_offset(StringScanner *ss, char const *expect, size_t offset);
extern int           ss_one_of(StringScanner *ss, char const *expect);
extern StringView    ss_read(StringScanner *ss, size_t num);
extern StringView    ss_read_from_mark(StringScanner *ss);
extern int           ss_readchar(StringScanner *ss);
extern int           ss_peek(StringScanner *ss);
extern int           ss_peek_with_offset(StringScanner *ss, size_t offset);
extern void          ss_skip(StringScanner *ss, size_t num);
extern void          ss_skip_one(StringScanner *ss);
extern void          ss_skip_whitespace(StringScanner *ss);
extern size_t        ss_read_number(StringScanner *ss);

#endif /* __SV_H__ */
