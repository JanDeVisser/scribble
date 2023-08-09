/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <sv.h>

#define STATIC_ALLOCATOR
#include <allocate.h>

StringView sv_from(char const *s)
{
    StringView ret = { s, strlen(s) };
    return ret;
}

StringView sv_null()
{
    StringView ret = { NULL, 0 };
    return ret;
}

StringView sv_printf(char const* fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    StringView ret = sv_vprintf(fmt, args);
    va_end(args);
    return ret;
}

StringView sv_vprintf(char const* fmt, va_list args)
{
    va_list args2;
    va_copy(args2, args);
    int len = vsnprintf(NULL, 0, fmt, args);
    char *str = mem_allocate(len+1);
    vsnprintf(str, len+1, fmt, args2);
    va_end(args2);
    return sv_from(str);
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

int sv_cmp(StringView s1, StringView s2)
{
    if (s1.length != s2.length) {
        return ((int) s1.length) - ((int) s2.length);
    }
    return memcmp(s1.ptr, s2.ptr, s1.length);
}

bool sv_eq(StringView s1, StringView s2)
{
    if (s1.length != s2.length) {
        return false;
    }
    return memcmp(s1.ptr, s2.ptr, s1.length) == 0;
}

bool sv_eq_cstr(StringView s1, char const *s2)
{
    if (s1.length != strlen(s2)) {
        return false;
    }
    return memcmp(s1.ptr, s2, s1.length) == 0;
}

bool sv_eq_cstr_n(StringView s1, char const *s2, size_t n)
{
    if (s1.length != n) {
        return false;
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
    if (s1.length < s2.length) {
        return false;
    }
    return memcmp(s1.ptr + (s1.length - s2.length), s2.ptr, s2.length) == 0;
}

StringView sv_chop(StringView sv, size_t num)
{
    if (num > sv.length)
        num = sv.length;
    StringView ret = { sv.ptr + num, sv.length - num };
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
    char       *sv_str = mem_allocate(sv.length + 1);
    char       *tail_str;
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
