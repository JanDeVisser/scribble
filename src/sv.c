/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <sv.h>

#define STATIC_ALLOCATOR
#include <allocate.h>

#define BLOCKSIZES(S) S(32) S(64) S(128) S(256) S(512) S(1024) S(2048) S(4096) S(8192) S(16384)

#undef BLOCKSIZE
#define BLOCKSIZE(size) char *fl_##size = NULL;
BLOCKSIZES(BLOCKSIZE)
#undef BLOCKSIZE

char *allocate_for_length(size_t length, size_t *capacity)
{
    char  *ret = NULL;
    size_t cap = (!capacity || !*capacity) ? 32 : *capacity;
    while (cap < length)
        cap *= 2;
    switch (cap) {
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
        ret = allocate(cap);
    }
    if (capacity) {
        *capacity = cap;
    }
    return ret;
}

void free_buffer(char *buffer, size_t capacity)
{
    if (capacity) {
        switch (capacity) {
#undef BLOCKSIZE
#define BLOCKSIZE(size)                  \
    case size:                           \
        *((char **) buffer) = fl_##size; \
        fl_##size = buffer;              \
        break;
            BLOCKSIZES(BLOCKSIZE)
#undef BLOCKSIZE
        default:
            break;
        }
    }
}

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
    ret.ptr = allocate_for_length(len, NULL);
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
    free_buffer((char *) sv.ptr, sv.capacity);
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
    va_list args2;
    va_copy(args2, args);
    int        len = vsnprintf(NULL, 0, fmt, args);
    StringView ret = { 0 };
    char      *str = allocate_for_length(len + 1, &ret.capacity);
    vsnprintf(str, len + 1, fmt, args2);
    va_end(args2);
    ret.ptr = str;
    ret.length = len;
    return ret;
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

bool sv_eq_cstr_n(StringView s1, char const *s2, size_t n)
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

StringView sv_chop(StringView sv, size_t num)
{
    if (num > sv.length)
        num = sv.length;
    StringView ret = { sv.ptr + num, sv.length - num };
    return ret;
}

int sv_find_char(StringView sv, char ch)
{
    for (int ix = 0; ix < sv.length; ++ix) {
        if (*(sv.ptr + ix) == ch)
            return ix;
    }
    return -1;
}

int sv_find_str(StringView sv, StringView sub)
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

size_t sv_split(StringView sv, StringView sep, size_t max_components, StringView components[])
{
    size_t      ix = 0;
    char const *ptr = sv.ptr;
    char const *component_start = sv.ptr;
    while (ix < max_components) {
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

char *sb_reallocate(char *old_buf, size_t *capacity, size_t new_len)
{
    char  *ret = NULL;
    size_t new_cap = (*capacity) ? *capacity : 32;
    while (new_cap < new_len)
        new_cap *= 2;
    if (new_cap == *capacity) {
        return old_buf;
    }
    ret = allocate_for_length(new_len, capacity);
    if (old_buf) {
        memcpy(ret, old_buf, *capacity);
        free_buffer(old_buf, *capacity);
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
    sb.view.ptr = allocate_for_length(len + 1, &sb.view.capacity);
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
    sb->view.ptr = sb_reallocate((char *) sb->view.ptr, &sb->view.capacity, sb->view.length + len + 1);
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
    sb->view.ptr = sb_reallocate((char *) sb->view.ptr, &sb->view.capacity, sb->view.length + len + 1);
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
    size_t     num = sv_split(sv_from(s), sv_from(sep), 1024, parts);
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
