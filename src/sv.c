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
    va_list args2;
    va_copy(args2, args);
    int   len = vsnprintf(NULL, 0, fmt, args);
    char *str = mem_allocate(len + 1);
    vsnprintf(str, len + 1, fmt, args2);
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

size_t sv_split(StringView sv, StringView sep, size_t num, StringView components[])
{
    size_t      ix = 0;
    char const *ptr = sv.ptr;
    char const *component_start = sv.ptr;
    while (ix < num) {
        if (ptr - sv.ptr > sv.length - sep.length) {
            components[ix++] = (StringView) { component_start, sv.ptr + sv.length - component_start };
            return ix;
        }
        if (memcmp(ptr, sep.ptr, sep.length) == 0) {
            components[ix++] = (StringView) { component_start, ptr - component_start };
            ptr += sep.length;
            component_start = ptr;
            continue;
        }
        ++ptr;
    }
    return ix;
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

#define BLOCKSIZES(S) S(32) S(64) S(128) S(256) S(512) S(1024) S(2048) S(4096) S(8192) S(16384)

#undef BLOCKSIZE
#define BLOCKSIZE(size) char *fl_##size = NULL;
BLOCKSIZES(BLOCKSIZE)
#undef BLOCKSIZE

char *sb_reallocate(char *old_buf, size_t *capacity, size_t new_len)
{
    char  *ret = NULL;
    size_t new_cap = (*capacity) ? *capacity : 32;
    while (new_cap < new_len)
        new_cap *= 2;
    if (new_cap == *capacity) {
        return old_buf;
    }
    switch (new_cap) {
#define BLOCKSIZE(size)                         \
    case size:                                  \
        if (fl_##size) {                        \
            ret = fl_##size;                    \
            fl_##size = *((char **) fl_##size); \
        }                                       \
        break;
        BLOCKSIZES(BLOCKSIZE)
#undef BLOCKSIZE
    default:
        break;
    }
    if (!ret) {
        ret = allocate(new_cap);
    }
    if (old_buf) {
        memcpy(ret, old_buf, *capacity);
        switch (*capacity) {
#undef BLOCKSIZE
#define BLOCKSIZE(size)                   \
    case size:                            \
        *((char **) old_buf) = fl_##size; \
        fl_##size = old_buf;              \
        break;
            BLOCKSIZES(BLOCKSIZE)
#undef BLOCKSIZE
        default:
            break;
        }
    }
    *capacity = new_cap;
    return ret;
}

StringBuilder sb_create()
{
    StringBuilder sb = { 0 };
    return sb;
}

StringBuilder sb_copy_chars(char const *ptr, size_t len)
{
    StringBuilder sb = { 0 };
    sb.view.ptr = sb_reallocate(NULL, &sb.capacity, len + 1);
    if (len > 0) {
        memcpy((char *) sb.view.ptr, ptr, len);
        sb.view.length = len;
    }
    return sb;
}

StringBuilder sb_copy_sv(StringView sv)
{
    return sb_copy_chars(sv.ptr, sv.length + 1);
}

StringBuilder sb_copy_cstr(char const *s)
{
    return sb_copy_chars(s, strlen(s) + 1);
}

void sb_append_chars(StringBuilder *sb, char const *ptr, size_t len)
{
    sb->view.ptr = sb_reallocate((char *) sb->view.ptr, &sb->capacity, sb->view.length + len + 1);
    memcpy((char *) sb->view.ptr + sb->view.length, ptr, len);
    sb->view.length += len;
}

void sb_append_sv(StringBuilder *sb, StringView sv)
{
    sb_append_chars(sb, sv.ptr, sv.length);
}

void sb_append_cstr(StringBuilder *sb, char const *s)
{
    sb_append_chars(sb, s, strlen(s));
}

void sb_vprintf(StringBuilder *sb, char const *fmt, va_list args)
{
    va_list args2;
    va_copy(args2, args);
    size_t len = vsnprintf(NULL, 0, fmt, args);
    sb->view.ptr = sb_reallocate((char *) sb->view.ptr, &sb->capacity, sb->view.length + len + 1);
    vsnprintf((char *) sb->view.ptr + sb->view.length, len + 1, fmt, args2);
    sb->view.length += len;
}

void sb_printf(StringBuilder *sb, char const *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    sb_vprintf(sb, fmt, args);
}

StringView sb_view(StringBuilder *sb)
{
    return sb->view;
}

#ifdef SV_TEST

void test_split(char const *s, char const *sep, size_t expected)
{
    StringView parts[1024];
    size_t num = sv_split(sv_from(s), sv_from(sep), 1024, parts);
    printf("Separate '%s' by '%s': Parts: %zu", s, sep, num);
    if (num != expected) {
        printf(" (WHICH IS NOT %zu!)", expected);
    }
    for (size_t ix = 0; ix < num; ++ix) {
        printf(", %zu: '" SV_SPEC "'", ix, SV_ARG(parts[ix]));
    }
    printf("\n");
}

int main(int argc, char **argv)
{
    test_split("foo:get_bar", ":", 2);
    test_split("get_bar", ":", 1);
    test_split("get_bar:", ":", 2);
    test_split(":get_bar", ":", 2);
    test_split(":get_bar:", ":", 3);
}

#endif
