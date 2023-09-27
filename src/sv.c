/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ctype.h>

#include <allocate.h>
#include <sv.h>

DECLARE_SHARED_ALLOCATOR(sv)
SHARED_ALLOCATOR_IMPL(sv)

#define BLOCKSIZES(S) S(32) S(64) S(128) S(256) S(512) S(1024) S(2048) S(4096) S(8192) S(16384)

#undef BLOCKSIZE
#define BLOCKSIZE(size) static char *fl_##size = NULL;
BLOCKSIZES(BLOCKSIZE)
#undef BLOCKSIZE

char *allocate_for_length(size_t length, size_t *capacity, Allocator *allocator)
{
    if (!allocator) {
        allocator = get_allocator();
    }
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
        ret = allocator_allocate(allocator, cap);
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
    return sv_copy_chars_with_allocator(ptr, len, NULL);
}

StringView sv_copy_cstr(char const *s)
{
    size_t len = strlen(s);
    if (!s || !len) {
        return sv_null();
    }
    return sv_copy_chars(s, len);
}

StringView sv_copy_with_allocator(StringView sv, Allocator *allocator)
{
    if (sv_empty(sv)) {
        return sv;
    }
    return sv_copy_chars_with_allocator(sv.ptr, sv.length, allocator);
}

StringView sv_copy_chars_with_allocator(char const *ptr, size_t len, Allocator *allocator)
{
    if (!ptr || !len) {
        return sv_null();
    }
    StringView ret = { 0 };
    ret.ptr = allocate_for_length(len, NULL, allocator);
    ret.length = len;
    memcpy((char *) ret.ptr, ptr, len);
    ((char *) ret.ptr)[ret.length] = 0;
    return ret;
}

StringView sv_copy_cstr_with_allocator(char const *s, Allocator *allocator)
{
    size_t len = strlen(s);
    if (!s || !len) {
        return sv_null();
    }
    return sv_copy_chars_with_allocator(s, len, allocator);
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

StringView sv_aprintf(Allocator *allocator, char const *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    StringView ret = sv_avprintf(allocator, fmt, args);
    va_end(args);
    return ret;
}

StringView sv_avprintf(Allocator *allocator, char const *fmt, va_list args)
{
    StringBuilder sb = sb_acreate(allocator);
    sb_vprintf(&sb, fmt, args);
    return sb.view;
}

StringView sv_printf(char const *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    StringView ret = sv_avprintf(get_allocator(), fmt, args);
    va_end(args);
    return ret;
}

StringView sv_vprintf(char const *fmt, va_list args)
{
    return sv_avprintf(get_allocator(), fmt, args);
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
    if (!ret && (sv.capacity > sv.length)) {
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
        char *cstr = allocate_for_length(sv.length + 1, NULL, NULL);
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
            return sv.length - ix - 1;
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

StringList sv_asplit(Allocator *allocator, StringView sv, StringView sep)
{
    StringList  ret = sl_acreate(allocator);
    size_t      ix = 0;
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

StringList sv_split(StringView sv, StringView sep)
{
    return sv_asplit(get_allocator(), sv, sep);
}

StringList sv_split_by_whitespace(StringView sv)
{
    return sv_asplit_by_whitespace(get_allocator(), sv);
}

StringList sv_asplit_by_whitespace(Allocator *allocator, StringView sv)
{
    StringList  ret = sl_acreate(allocator);
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

static void sb_reallocate(StringBuilder *sb, size_t new_len)
{
    char  *ret = NULL;
    size_t new_cap = (sb->view.capacity) ? sb->view.capacity : 32;
    while (new_cap < new_len)
        new_cap *= 2;
    if (new_cap <= sb->view.capacity) {
        return;
    }
    ret = allocate_for_length(new_len, &sb->view.capacity, sb->allocator);
    if (sb->view.ptr) {
        memcpy(ret, sb->view.ptr, sb->view.capacity);
        free_buffer((char *) sb->view.ptr, sb->view.capacity);
    }
    sb->view.capacity = new_cap;
    sb->view.ptr = ret;
}

StringBuilder sb_create()
{
    return sb_acreate(get_allocator());
}

StringBuilder sb_acreate(Allocator *allocator)
{
    StringBuilder sb = { 0 };
    sb.allocator = allocator;
    return sb;
}

StringBuilder sb_copy_chars(char const *ptr, size_t len)
{
    StringBuilder sb = sb_create();
    sb.view.ptr = allocate_for_length(len + 1, &sb.view.capacity, sb.allocator);
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
    sb_reallocate(sb, sb->view.length + len + 1);
    char *p = (char *) sb->view.ptr;
    memcpy(p + sb->view.length, ptr, len);
    sb->view.length += len;
    p[sb->view.length] = '\0';
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
    sb_reallocate(sb, sb->view.length + len + 1);
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
