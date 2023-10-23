/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ctype.h>

#include <allocate.h>
#include <sv.h>

DECLARE_SHARED_ALLOCATOR(sv)

char  *allocate_for_length(size_t length, size_t *capacity);
size_t buffer_capacity(char const *buffer);
void   free_buffer(char *buffer);

StringView sv_from(char const *s)
{
    size_t     len = (s) ? strlen(s) : 0;
    StringView ret = { 0 };
    if (len) {
        ret.ptr = s;
        ret.length = len;
    }
    return ret;
}

StringView sv_copy(StringView sv)
{
    if (sv_empty(sv)) {
        return sv;
    }
    return sv_copy_chars(sv.ptr, sv.length);
}

StringView sv_copy_chars(char const *ptr, size_t len)
{
    if (!ptr || !len) {
        return sv_null();
    }
    StringView ret = { 0 };
    ret.ptr = allocate_for_length(len + 1, NULL);
    ret.length = len;
    memcpy((char *) ret.ptr, ptr, len);
    ((char *) ret.ptr)[ret.length] = 0;
    return ret;
}

StringView sv_copy_cstr(char const *s)
{
    size_t len = strlen(s);
    if (!s || !len) {
        return sv_null();
    }
    return sv_copy_chars(s, len);
}

void sv_free(StringView sv)
{
    free_buffer((char *) sv.ptr);
}

StringView sv_null()
{
    StringView ret = { 0 };
    return ret;
}

StringView sv_printf(char const *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    StringView ret = sv_vprintf(fmt, args);
    va_end(args);
    return ret;
}

StringView sv_vprintf(char const *fmt, va_list args)
{
    StringBuilder builder = sb_vcreatef(fmt, args);
    return builder.view;
}

bool sv_empty(StringView sv)
{
    return sv.length == 0;
}

bool sv_not_empty(StringView sv)
{
    return sv.length > 0;
}

size_t sv_length(StringView sv)
{
    return sv.length;
}

bool sv_is_cstr(StringView sv)
{
    bool ret = sv.ptr && (sv.ptr[sv.length] == 0);
    if (!ret && (buffer_capacity(sv.ptr) > sv.length)) {
        char *ptr = (char *) sv.ptr;
        ptr[sv.length] = '\0';
        ret = true;
    }
    return ret;
}

char const *sv_cstr(StringView sv)
{
    if (sv_is_cstr(sv)) {
        return sv.ptr;
    } else {
        char *cstr = allocate_for_length(sv.length + 1, NULL);
        memcpy(cstr, sv.ptr, sv.length);
        cstr[sv.length] = '\0';
        return cstr;
    }
}

int sv_cmp(StringView s1, StringView s2)
{
    if (s1.length != s2.length) {
        return ((int) s1.length) - ((int) s2.length);
    }
    if (s1.length == 0 && s2.length == 0) {
        return 0;
    }
    return memcmp(s1.ptr, s2.ptr, s1.length);
}

bool sv_eq(StringView s1, StringView s2)
{
    if (s1.length != s2.length) {
        return false;
    }
    if (s1.length == 0 && s2.length == 0) {
        return true;
    }
    return memcmp(s1.ptr, s2.ptr, s1.length) == 0;
}

bool sv_eq_cstr(StringView s1, char const *s2)
{
    size_t len = (s2) ? strlen(s2) : 0;
    if (s1.length != len) {
        return false;
    }
    if (s1.length == 0 && len == 0) {
        return true;
    }
    return memcmp(s1.ptr, s2, s1.length) == 0;
}

bool sv_eq_chars(StringView s1, char const *s2, size_t n)
{
    if (s1.length != n) {
        return false;
    }
    if (s1.length == 0 && (!s2 || !*s2)) {
        return true;
    }
    return memcmp(s1.ptr, s2, n) == 0;
}

bool sv_startswith(StringView s1, StringView s2)
{
    if (!s2.length || s1.length < s2.length) {
        return false;
    }
    return memcmp(s1.ptr, s2.ptr, s2.length) == 0;
}

bool sv_endswith(StringView s1, StringView s2)
{
    if (!s2.length || s1.length < s2.length) {
        return false;
    }
    return memcmp(s1.ptr + (s1.length - s2.length), s2.ptr, s2.length) == 0;
}

StringView sv_lchop(StringView sv, size_t num)
{
    if (num >= sv.length) {
        return sv_null();
    }
    StringView ret = { sv.ptr + num, sv.length - num };
    return ret;
}

StringView sv_rchop(StringView sv, size_t num)
{
    if (num >= sv.length) {
        return sv_null();
    }
    StringView ret = { sv.ptr, sv.length - num };
    return ret;
}

int sv_first(StringView sv, char ch)
{
    for (int ix = 0; ix < sv.length; ++ix) {
        if (*(sv.ptr + ix) == ch)
            return ix;
    }
    return -1;
}

int sv_last(StringView sv, char ch)
{
    for (int ix = 0; ix < sv.length; ++ix) {
        if (*(sv.ptr + (sv.length - ix - 1)) == ch)
            return (int) sv.length - ix - 1;
    }
    return -1;
}

int sv_find(StringView sv, StringView sub)
{
    for (int ix = 0; ix <= sv.length - sub.length; ++ix) {
        if (!memcmp(sv.ptr + ix, sub.ptr, sub.length))
            return ix;
    }
    return -1;
}

StringView sv_substring(StringView sv, size_t at, size_t len)
{
    if (at >= sv.length) {
        return sv_null();
    }
    if (at + len > sv.length) {
        len = sv.length - at;
    }
    return (StringView) { sv.ptr + at, len };
}

StringList sv_split(StringView sv, StringView sep)
{
    StringList  ret = sl_create();
    char const *ptr = sv.ptr;
    char const *component_start = sv.ptr;
    while (true) {
        if (ptr - sv.ptr > sv.length - sep.length) {
            sl_push(&ret, (StringView) { component_start, sv.ptr + sv.length - component_start });
            return ret;
        }
        if (memcmp(ptr, sep.ptr, sep.length) == 0) {
            sl_push(&ret, (StringView) { component_start, ptr - component_start });
            ptr += sep.length;
            component_start = ptr;
            continue;
        }
        ++ptr;
    }
    UNREACHABLE();
}

StringList sv_split_by_whitespace(StringView sv)
{
    StringList  ret = sl_create();
    char const *current = NULL;
    for (size_t ix = 0; ix < sv.length; ++ix) {
        if (isspace(sv.ptr[ix]) && current) {
            sl_push(&ret, (StringView) { current, (sv.ptr + ix) - current });
            current = NULL;
        }
        if (!isspace(sv.ptr[ix]) && !current) {
            current = sv.ptr + ix;
        }
    }
    if (current) {
        sl_push(&ret, (StringView) { current, (sv.ptr + sv.length) - current });
    }
    return ret;
}

StringView sv_strip(StringView sv)
{
    StringView ret = { 0 };
    if (!sv.length) {
        return sv_null();
    }
    for (ret.ptr = sv.ptr; (ret.ptr - sv.ptr < sv.length) && isspace(*ret.ptr); ++ret.ptr)
        ;
    if (ret.ptr - sv.ptr >= sv.length) {
        return sv_null();
    }
    for (ret.length = sv.length - (ret.ptr - sv.ptr);
         ret.length > 0 && isspace(ret.ptr[ret.length - 1]);
         --ret.length)
        ;
    if (!ret.length) {
        ret.ptr = NULL;
    }
    return ret;
}

bool sv_tolong(StringView sv, long *result, StringView *tail)
{
    assert(result);
    if (sv.length == 0) {
        if (tail) {
            *tail = sv;
        }
        return false;
    }

    AllocatorState state = mem_save();
    char          *sv_str = mem_allocate(sv.length + 1);
    char          *tail_str;
    memcpy(sv_str, sv.ptr, sv.length);
    sv_str[sv.length] = 0;
    long res = strtol(sv_str, &tail_str, 10);
    bool ret = tail_str != sv_str;
    if (tail) {
        size_t processed = tail_str - sv_str;
        tail->ptr = tail->ptr + processed;
        tail->length = tail->length - processed;
    }
    if (ret) {
        *result = res;
    }
    mem_release(state);
    return ret;
}

#ifdef SV_TEST

void test_split_join(StringView sv)
{
    StringList split = sv_split_by_whitespace(sv);
    printf("split size: %zu\n", split.size);
    for (size_t ix = 0; ix < split.size; ++ix) {
        printf("split[%zu] = %.*s\n", ix, SV_ARG(split.strings[ix]));
    }
    StringView joined = sl_join(&split, sv_from("*"));
    printf("--%.*s--\n", SV_ARG(joined));
}

int main()
{
    printf("--%.*s--\n", SV_ARG(sv_strip(sv_from("  abcd \t"))));
    printf("--%.*s--\n", SV_ARG(sv_strip(sv_from("abcd \t"))));
    printf("--%.*s--\n", SV_ARG(sv_strip(sv_from("  abcd"))));

    test_split_join(sv_from("  ab cd ef  "));
    test_split_join(sv_from("ab cd ef  "));
    test_split_join(sv_from("  ab \t cd \n\n ef"));
    return 0;
}

#endif
