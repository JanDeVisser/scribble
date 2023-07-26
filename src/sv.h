/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#ifndef __SV_H__
#define __SV_H__

#include <stdlib.h>
#include <string.h>

typedef struct {
    char const* ptr;
    size_t length;
} StringView;

extern StringView sv_null();
extern StringView sv_from(char const* s);
extern bool sv_empty(StringView sv);
extern bool sv_not_empty(StringView sv);
extern size_t sv_length(StringView sv);
extern int sv_cmp(StringView s1, StringView s2);
extern bool sv_eq(StringView s1, StringView s2);
extern bool sv_eq_cstr(StringView s1, char const *s2);
extern bool sv_eq_cstr_n(StringView s1, char const *s2, size_t n);
extern bool sv_startswith(StringView s1, StringView s2);
extern bool sv_endswith(StringView s1, StringView s2);

extern StringView sv_chop(StringView sv, size_t num);

#define SV_SPEC "%.*s"
#define SV_ARG(sv) (int)sv.length, sv.ptr

#endif /* __SV_H__ */
